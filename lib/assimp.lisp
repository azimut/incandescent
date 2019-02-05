(in-package #:incandescent)
;;--------------------------------------------------
;; Assimp - 3d object loader

;; Might be a I can just drop the bones with low weight
;; instead of max limit
(defvar *max-bones-per-vertex* 3)

;; NOTE: see how assimp-mesh structure is similar to a "g-pnt + tb-data" one
(defstruct-g assimp-mesh
  (pos       :vec3)
  (normal    :vec3)
  (uv        :vec2)
  (tangent   :vec3)
  (bitangent :vec3))

;; Might be I could have used the 1 without filling the other bone slots?
(defstruct-g assimp-with-bones
  (pos           :vec3)
  (normal        :vec3)
  (uv            :vec2)
  (tangent       :vec3)
  (bitangent     :vec3)
  (bones-ids     :ivec3)
  (bones-weights :vec3))

;;--------------------------------------------------
;; Pretty printers
;;--------------------------------------------------

(defmethod print-object ((obj ai:vertex-weight) out)
  (print-unreadable-object (obj out :type t)
    (format out "~a ~a" (ai:id obj) (ai:weight obj))))

(defmethod print-object ((obj ai:node) out)
  (print-unreadable-object (obj out :type t)
    (format out "~a" (ai:name obj))))

(defmethod print-object ((obj ai:bone) out)
  (print-unreadable-object (obj out :type t)
    (format out "~a" (ai:name obj))))

;;--------------------------------------------------
;; Loaders
;;--------------------------------------------------

(defvar *assimp-meshes* NIL
  "Set of actor type objects")

(defmethod free ((actor assimp-thing))
  (free (slot-value actor 'buf)))

(defun free-meshes ()
  (loop :for mesh :in *assimp-meshes*
     :do (free mesh))
  (setf *assimp-meshes* NIL))

(defun get-bones-per-vertex (all-bones bones n-vertices)
  "returns an array of lists of tuples/cons"
  (let ((a-bones (make-array n-vertices :initial-element NIL)))
    (loop
       :for bone :across bones
       :for bone-id := (position (ai:name bone) all-bones
                                 :test #'string=
                                 :key  #'ai:name) :do
         (loop :for weight :across (ai:weights bone) :do
              (when (< (length (aref a-bones (ai:id weight))) *max-bones-per-vertex*)
                (push (cons bone-id (ai:weight weight))
                      (aref a-bones (ai:id     weight))))))
    a-bones))

;; FIXME! two classes/methods
(defun assimp-mesh-to-stream (mesh scene file-path scale)
  "returns an assimp actor object"
  (declare (ai:mesh mesh)
           (ai:scene scene)
           (pathname file-path)
           (single-float scale))
  (with-slots ((vertices       ai:vertices)
               (faces          ai:faces)
               (normals        ai:normals)
               (texture-coords ai:texture-coords)
               (tangents       ai:tangents)
               (bitangents     ai:bitangents)
               (mat-index      ai:material-index)
               (bones          ai:bones))
      mesh
    (let* ((texture-coords (elt texture-coords 0))
           (material (aref (slot-value scene 'ai:materials) mat-index))
           (textures (gethash "$tex.file" material))
           (tex-file (and textures
                          (third (assoc :ai-texture-type-diffuse textures))))
           (norm-file (when (assoc :ai-texture-type-height textures)
                        (third (assoc :ai-texture-type-height textures))))
           (albedo  (if tex-file
                        (get-tex (merge-pathnames tex-file file-path))
                        (get-tex "static/37.Paint01-1k/paint01_albedo.jpg")))
           (normal-sam (if norm-file
                           (get-tex (merge-pathnames norm-file file-path))
                           (get-tex "static/37.Paint01-1k/paint01_normal.jpg"
                                    nil t :r8)))
           (lenv (length vertices))
           (lenf (* lenv 3))
           ;; I need context for this...hackidy hack
           (meshes     (coerce (ai:meshes scene)'list))
           (all-bones  (mappend (lambda (mesh) (coerce (ai:bones mesh) 'list)) meshes))
           (has-bones  (not (every #'null all-bones)))
           (actor-type (if has-bones 'assimp-with-bones 'assimp-mesh))
           (bones-per-vertex (get-bones-per-vertex all-bones bones lenv)))
      (assert (length= bitangents
                       tangents
                       normals
                       vertices
                       texture-coords))
      (let ((v-arr (make-gpu-array NIL :dimensions lenv :element-type actor-type))
            (i-arr (make-gpu-array NIL :dimensions lenf :element-type :ushort)))
        (with-gpu-array-as-c-array (c-arr i-arr)
          (loop
             :for indices :across faces
             :for i :from 0 :by 3
             :do (setf (aref-c c-arr i)       (aref indices 0)
                       (aref-c c-arr (+ i 1)) (aref indices 1)
                       (aref-c c-arr (+ i 2)) (aref indices 2))))
        (with-gpu-array-as-c-array (c-arr v-arr)
          (if has-bones
              (loop
                 :for v  :across vertices
                 :for n  :across normals
                 :for ta :across tangents
                 :for bt :across bitangents
                 :for tc :across texture-coords
                 :for bv :across bones-per-vertex
                 :for i  :from 0
                 :for a  := (aref-c c-arr i)
                 :do
                   (setf (assimp-with-bones-pos a) v
                         (assimp-with-bones-normal a) n
                         (assimp-with-bones-tangent a) ta
                         (assimp-with-bones-bitangent a) bt
                         (assimp-with-bones-uv a) (v! (x tc) (y tc)))
                   (setf (assimp-with-bones-bones-ids a)
                         (v!int (serapeum:pad-end (map 'vector #'car bv) 3 0)))
                   (setf (assimp-with-bones-bones-weights a)
                         (v! (serapeum:pad-end (map 'vector #'cdr bv) 3 0))))
              (loop
                 :for v  :across vertices
                 :for n  :across normals
                 :for ta :across tangents
                 :for bt :across bitangents
                 :for tc :across texture-coords
                 :for i  :from 0
                 :for a  := (aref-c c-arr i)
                 :do (setf (assimp-mesh-pos a) v
                           (assimp-mesh-normal a) n
                           (assimp-mesh-tangent a) ta
                           (assimp-mesh-bitangent a) bt
                           (assimp-mesh-uv a) (v! (x tc) (y tc))))))
        (make-instance 'assimp-thing
                       :buf (make-buffer-stream v-arr :index-array i-arr)
                       :scale scale
                       :normals normal-sam
                       :albedo albedo)))))

(defun assimp-load-meshes (file &optional (scale 1f0))
  "empties and fills the *ASSIMP-MESHES* globals with assimp-flat
   instances ready to be render"
  (declare (type single-float scale))
  (assert (probe-file file))
  (free-meshes) ;; !!!
  (let* ((scene (ai:import-into-lisp
                 file
                 ;; flip-u-vs:          needed to unwrap texture correctly
                 ;; gen-smooth-normals: needed for meshes without normals
                 ;; triangulate:        use it ALWAYS
                 ;; calc-tangent-space: generates arc/bi tan from normals
                 :processing-flags '(:ai-process-triangulate
                                     :ai-process-flip-u-vs
                                     :ai-process-gen-smooth-normals
                                     :ai-process-calc-tangent-space)))
         (meshes (coerce (slot-value scene 'ai:meshes) 'list))
         (file-path (uiop:pathname-directory-pathname file))
         (actors (mapcar (lambda (mesh) (assimp-mesh-to-stream mesh
                                                          scene
                                                          file-path
                                                          scale))
                         meshes)))
    (mapcar (lambda (actor) (push actor *assimp-meshes*))
            actors))
  T)

;;--------------------------------------------------
;; Draw
;;--------------------------------------------------
(defmethod draw ((actor assimp-thing) camera (time single-float))
  (with-slots (buf albedo normals scale) actor
    (map-g #'generic-tex-pipe-simple buf
           :scale scale
           ;; Lighting
           :model-world (model->world actor)
           :world-view (world->view camera)
           :view-clip  (projection camera)
           ;; PBR
           :albedo albedo
           :normals normals)))

;;--------------------------------------------------
;; Renders
;;--------------------------------------------------

;; 3D - g-pnt with tangent info in tb-data AND textures
(defun-g vert-with-tbdata ((vert g-pnt) (tb tb-data)
                           &uniform
                           (model-world :mat4)
                           (world-view :mat4)
                           (view-clip :mat4)
                           (scale :float)
                           ;; Parallax vars
                           (light-pos :vec3)
                           (cam-pos :vec3))
  (let* ((pos       (* scale (pos vert)))
         (norm      (norm vert))
         (uv        (treat-uvs (tex vert)))
         (norm      (* (m4:to-mat3 model-world) norm))
         (world-pos (* model-world (v! pos 1)))
         (view-pos  (* world-view  world-pos))
         (clip-pos  (* view-clip   view-pos))
         (t0 (normalize
              (s~ (* model-world (v! (tb-data-tangent tb) 0))
                  :xyz)))
         (n0 (normalize
              (s~ (* model-world (v! norm 0))
                  :xyz)))
         (t0 (normalize (- t0 (* (dot t0 n0) n0))))
         (b0 (cross n0 t0))
         (tbn (mat3 t0 b0 n0)))
    (values clip-pos
            (treat-uvs uv)
            norm
            (s~ world-pos :xyz)
            tbn
            (* tbn light-pos)
            (* tbn cam-pos)
            (* tbn (s~ world-pos :xyz)))))


;; no parallax
(defun-g frag-tex-tbn ((uv :vec2)
                       (frag-norm :vec3)
                       (frag-pos :vec3)
                       (tbn :mat3)
                       (tan-light-pos :vec3)
                       (tan-cam-pos :vec3)
                       (tan-frag-pos :vec3)
                       &uniform
                       (cam-pos :vec3)
                       (albedo :sampler-2d)
                       (normals :sampler-2d))
  (let* ((color (expt (s~ (texture albedo uv) :xyz)
                      (vec3 2.2)))
         (normal (norm-from-map normals uv)))
    (values
     (v! color 1)
     ;; (v! 1 1 1 1)
     ;;frag-pos
     ;;(normalize frag-norm)
     )))

;; parallax
(defun-g frag-tex-tbn ((uv :vec2)
                       (frag-norm :vec3)
                       (frag-pos :vec3)
                       (tbn :mat3)
                       (tan-light-pos :vec3)
                       (tan-cam-pos :vec3)
                       (tan-frag-pos :vec3)
                       &uniform
                       (cam-pos :vec3)
                       (albedo :sampler-2d)
                       (normap :sampler-2d)
                       (height-map :sampler-2d))
  (let* ((light-pos *pointlight-pos*)
         ;; Parallax
         (tan-cam-dir (- tan-cam-pos tan-frag-pos))
         (newuv (parallax-mapping uv tan-cam-dir height-map .1))
         ;; ---------
         (light-color (v! 1 1 1))
         (light-strength 1f0)
         ;;--------------------
         (vec-to-light (- light-pos frag-pos))
         (dir-to-light (normalize vec-to-light))
         ;;--------------------
         (color (expt (s~ (texture albedo newuv) :xyz)
                      (vec3 2.2)))
         (normal (norm-from-map normap newuv))
         (normal (normalize (* tbn normal))))
    (values
     (v! color 1)
     ;; (v! 1 1 1 1)
     ;;frag-pos
     ;;(normalize frag-norm)
     )))


(defpipeline-g generic-tex-pipe ()
  :vertex (vert-with-tbdata g-pnt tb-data)
  :fragment (frag-tex-tbn :vec2 :vec3 :vec3 :mat3
                          ;; Parallax
                          :vec3 :vec3 :vec3))


(defpipeline-g generic-tex-pipe-simple ()
  :vertex (vert-with-tbdata g-pnt tb-data)
  :fragment (frag-tex-tbn :vec2 :vec3 :vec3 :mat3
                          ;; Parallax
                          :vec3 :vec3 :vec3))

;; (defun load-bones ()
;;   (let* ((scene *toki*)
;;          (meshes    (coerce (ai:meshes scene) 'list))
;;          (all-bones (mappend (lambda (mesh) (coerce (ai:bones mesh) 'list)) meshes))
;;          (all-bones (remove NIL all-bones))
;;          ;;--------------------------------------------------
;;          (mesh       (nth 1 meshes))
;;          (n-vertices (length (ai:vertices mesh)))
;;          (a-bones    (make-array n-vertices :initial-element NIL))
;;          (bones      (ai:bones mesh)))
;;     (loop
;;        :for bone :across bones
;;        :for bone-id := (position (ai:name bone) all-bones
;;                                  :test #'string=
;;                                  :key  #'ai:name) :do
;;          (loop :for weight :across (ai:weights bone) :do
;;               (when (< (length (aref a-bones (ai:id weight))) *max-bones-per-vertex*)
;;                 (push (cons bone-id (ai:weight weight))
;;                       (aref a-bones (ai:id     weight))))))
;;     a-bones))


