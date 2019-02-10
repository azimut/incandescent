(in-package #:incandescent)
;;--------------------------------------------------
;; Assimp - 3d object loader

;; Might be a I can just drop the bones with low weight
;; instead of max limit
(defparameter *max-bones-per-vertex* 4)
(defvar *number-of-bones* 0)

;; flip-u-vs:          needed to unwrap texture correctly
;; gen-smooth-normals: needed for meshes without normals
;; triangulate:        use it ALWAYS
;; calc-tangent-space: generates arc/bi tan from normals
(defvar *processing-flags* '(:ai-process-triangulate
                             :ai-process-flip-u-vs
                             :ai-process-calc-tangent-space))

(defun m4-init-rotate (v)
  (declare (vec3 v))
  (m4:* (m4:rotation-x (radians (x v)))
        (m4:rotation-y (radians (y v)))
        (m4:rotation-z (radians (z v)))))

;; UBO
;;--------------------------------------------------
;; max HARDCODED bones length
;; (defstruct-g (bone-transforms :layout :std-140)
;;   (transform (:mat4 60)))

;; (defstruct-g bone-transforms
;;   (transform (:mat4 35)))

;; NOTE: see how assimp-mesh structure is similar to a "g-pnt + tb-data" one
(defstruct-g assimp-mesh
  (pos       :vec3)
  (normal    :vec3)
  (uv        :vec2)
  (tangent   :vec3)
  (bitangent :vec3))

;; Might be I could have used the 1 without filling the other bone slots?
(defstruct-g assimp-with-bones
  (pos           :vec3 :accessor pos)
  (normal        :vec3 :accessor norm)
  (uv            :vec2 :accessor tex)
  (tangent       :vec3 :accessor tangent)
  (bitangent     :vec3 :accessor bitangent)
  (bones-ids     :vec4 :accessor ids)
  (bones-weights :vec4 :accessor weights))

;; (defstruct-g assimp-bones
;;   (bones-ids :vec4)
;;   (bones-weights :vec4))

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

(defmethod free ((actor assimp-thing-with-bones))
  (free (slot-value actor 'buf)))

(defun free-meshes ()
  (mapcar #'free *assimp-meshes*)
  (setf *assimp-meshes* NIL))

(defun list-bones (scene)
  "returns a plain list with all the bones in SCENE, helper for the REPL"
  (declare (ai:scene scene))
  (let* ((meshes (coerce (ai:meshes scene) 'list))
         (bones  (mappend (lambda (m) (coerce (ai:bones m) 'list)) meshes))
         (bones  (remove NIL bones)))
    bones))

(defun list-bones-unique (scene)
  "returns a plain list with all the bones in SCENE, helper for the REPL"
  (declare (ai:scene scene))
  (let* ((bones (list-bones scene))
         (bones (remove-duplicates bones :key #'ai:name :test #'string=)))
    bones))

(defun get-bones-per-vertex (all-unique-scene-bones mesh-bones n-vertices)
  "Returns an array of lists of tuples/cons pairs.
   Run once at mesh LOAD time.
   ex: #(((1 . .9) (2 .1)) ((10 .2) (20 .8)))"
  (declare (type list   all-unique-scene-bones)
           (type array  mesh-bones)
           (type fixnum n-vertices))
  (let ((v-to-bones (make-array n-vertices
                                :initial-element NIL)))
    (loop
       :for bone :across mesh-bones
       :for bone-id := (position (ai:name bone) all-unique-scene-bones
                                 :test #'string=
                                 :key  #'ai:name)
       :do
         (loop
            :for weight :across (ai:weights bone)
            :do
              (with-slots ((v ai:id) (w ai:weight)) weight
                (when (and (>= w .1) ;; discard bones with low influence
                           (< (length (aref v-to-bones v))
                              *max-bones-per-vertex*))
                  (push (cons bone-id w)
                        (aref v-to-bones v))
                  ;; Sort weights
                  (setf (aref v-to-bones v)
                        (sort (aref v-to-bones v) #'>
                              :key #'cdr))))))
    v-to-bones))

(defgeneric get-nodes-transforms (scene node-type)
  (:documentation "returns a hash of mat4's with each node transform")
  (:method (scene (node-type (eql :static)))
    (let* ((nodes-transforms (make-hash-table :test #'equal)))
      (labels ((walk-node (node parent-transform)
                 (with-slots ((name      ai:name)
                              (transform ai:transform)
                              (children  ai:children))
                     node
                   (let ((global (m4:* parent-transform
                                       (m4:transpose transform))))
                     (setf (gethash name nodes-transforms) global)
                     (map 'vector
                          (lambda (c) (walk-node c global))
                          children)))))
        (walk-node (ai:root-node scene)
                   (m4:identity)))
      nodes-transforms))
  (:method (scene (node-type (eql :animated)))
    (let* ((nodes-transforms (make-hash-table :test #'equal))
           (animation (aref (ai:animations scene) 0))
           (animation-index (ai:index animation)))
      (labels ((walk-node (node parent-transform)
                 (with-slots ((name      ai:name)
                              (transform ai:transform)
                              (children  ai:children))
                     node
                   (let* ((anim (gethash name animation-index))
                          (arot (when anim
                                  (m4:*
                                   (m4:translation
                                    (ai:value
                                     (aref (ai:position-keys anim) 0)))
                                   (q:to-mat4
                                    (ai:value
                                     (aref (ai:rotation-keys anim) 0)))
                                   (m4:scale
                                    (ai:value
                                     (aref (ai:scaling-keys anim) 0))))))
                          (transform (if arot arot transform))
                          (global (m4:* parent-transform transform)))
                     (setf (gethash name nodes-transforms) global)
                     (map 'vector
                          (lambda (c) (walk-node c global))
                          children)))))
        (walk-node (ai:root-node scene)
                   (m4:identity)))
      nodes-transforms)))

(defun get-bones-tranforms (scene)
  "ANIMATIONLESS
   returns an array with the m4 matrices of each bone offset"
  (declare (ai:scene scene))
  (let* ((root-offset      (ai:transform (ai:root-node scene)))
         (root-offset      (m4:inverse (m4:transpose root-offset)))
         (unique-bones     (list-bones-unique scene))
         (node-type        (if (emptyp (ai:animations scene))
                               :animated
                               :static))
         (nodes-transforms (get-nodes-transforms scene node-type))
         (bones-transforms (make-array (length unique-bones))))
    (loop
       :for bone :in unique-bones
       :for bone-id :from 0 :do
         (with-slots ((name   ai:name)
                      (offset ai:offset-matrix))
             bone
           (let ((node-transform (gethash name nodes-transforms)))
             (setf (aref bones-transforms bone-id)
                   ;; I got a mesh that has 0 on the bones offsets...
                   ;; The mesh also didn't have animations so might be
                   ;; that was the reason...
                   (if (m4:0p offset)
                       (m4:* root-offset
                             node-transform)
                       (m4:* root-offset
                             node-transform
                             (m4:transpose offset)))))))
    bones-transforms))

;;--------------------------------------------------

(defmethod assimp-mesh-to-stream (mesh scene file-path scale (type (eql :textured)))
  "only textured assimp thing"
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
               (mat-index      ai:material-index))
      mesh
    (let* ((lenv (length vertices))
           (lenf (* lenv 3))
           (texture-coords (elt texture-coords 0))
           ;; (texture-coords (if (emptyp texture-coords) ;; zeroed meshes with empty uv
           ;;                     (coerce (serapeum:repeat-sequence `(,(v! 0 0)) lenv) 'vector)
           ;;                     (elt texture-coords 0)))
           (material   (aref (slot-value scene 'ai:materials) mat-index))
           (textures   (gethash "$tex.file" material))
           (tex-file   (and textures
                            (third (assoc :ai-texture-type-diffuse textures))))
           (norm-file  (when (assoc :ai-texture-type-height textures)
                         (third (assoc :ai-texture-type-height textures))))
           (albedo     (if tex-file
                           (get-tex (merge-pathnames tex-file file-path))
                           (get-tex "static/37.Paint01-1k/paint01_albedo.jpg")))
           (normal-sam (if norm-file
                           (get-tex (merge-pathnames norm-file file-path))
                           (get-tex "static/37.Paint01-1k/paint01_normal.jpg"
                                    nil t :r8))))
      (assert (length= bitangents
                       tangents
                       normals
                       vertices
                       texture-coords))
      (let ((v-arr (make-gpu-array NIL :dimensions lenv :element-type 'assimp-mesh))
            (i-arr (make-gpu-array NIL :dimensions lenf :element-type :ushort)))
        (with-gpu-array-as-c-array (c-arr i-arr)
          (loop
             :for indices :across faces
             :for i :from 0 :by 3
             :do (setf (aref-c c-arr i)       (aref indices 0)
                       (aref-c c-arr (+ i 1)) (aref indices 1)
                       (aref-c c-arr (+ i 2)) (aref indices 2))))
        (with-gpu-array-as-c-array (c-arr v-arr)
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
                       (assimp-mesh-uv a) (v! (x tc) (y tc)))))
        (make-instance 'assimp-thing
                       :buf (make-buffer-stream v-arr :index-array i-arr)
                       :scale scale
                       :normals normal-sam
                       :albedo albedo)))))

(defmethod assimp-mesh-to-stream (mesh scene file-path scale (type (eql :bones)))
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
    (let* ((lenv (length vertices))
           (lenf (* lenv 3))
           (texture-coords (elt texture-coords 0))
           ;; Add zero'ed uvs if missing, doesn't fix anything as bi/tan are not generated
           ;; (texture-coords (if (emptyp texture-coords)
           ;;                     (coerce (serapeum:repeat-sequence `(,(v! 0 0)) lenv) 'vector)
           ;;                     (elt texture-coords 0)))
           (material   (aref (slot-value scene 'ai:materials) mat-index))
           (textures   (gethash "$tex.file" material))
           (tex-file   (and textures
                            (third (assoc :ai-texture-type-diffuse textures))))
           (norm-file  (when (assoc :ai-texture-type-height textures)
                         (third (assoc :ai-texture-type-height textures))))
           (albedo     (if tex-file
                           (get-tex (merge-pathnames tex-file file-path))
                           (get-tex "static/37.Paint01-1k/paint01_albedo.jpg")))
           (normal-sam (if norm-file
                           (get-tex (merge-pathnames norm-file file-path))
                           (get-tex "static/37.Paint01-1k/paint01_normal.jpg"
                                    nil t :r8)))
           ;; I need context for this...hackidy hack
           (all-bones (list-bones-unique scene))
           (bones-per-vertex (get-bones-per-vertex all-bones bones lenv)))
      (assert (length= bitangents
                       tangents
                       normals
                       vertices
                       texture-coords))
      (let ((v-arr (make-gpu-array NIL :dimensions lenv :element-type 'assimp-with-bones))
            (i-arr (make-gpu-array NIL :dimensions lenf :element-type :ushort)))
        (with-gpu-array-as-c-array (c-arr i-arr)
          (loop
             :for indices :across faces
             :for i :from 0 :by 3
             :do (setf (aref-c c-arr i)       (aref indices 0)
                       (aref-c c-arr (+ i 1)) (aref indices 1)
                       (aref-c c-arr (+ i 2)) (aref indices 2))))
        (with-gpu-array-as-c-array (c-arr v-arr)
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
               (setf (assimp-mesh-pos a) v
                     (assimp-mesh-normal a) n
                     (assimp-mesh-tangent a) ta
                     (assimp-mesh-bitangent a) bt
                     (assimp-mesh-uv a) (v! (x tc) (y tc)))
               (setf (ids a)
                     (print
                      (v! (serapeum:pad-end
                           (map 'vector #'car bv) *max-bones-per-vertex* 0))))
               (setf (weights a)
                     (v! (serapeum:pad-end
                          (map 'vector #'cdr bv) *max-bones-per-vertex*  0)))))
        (make-instance 'assimp-thing-with-bones
                       :buf (make-buffer-stream v-arr :index-array i-arr)
                       :scale scale
                       :normals normal-sam
                       :albedo albedo)))))

(defun assimp-load-meshes (file &key (scale 1f0) (pos (v! 0 0 0)))
  "empties and fills the *ASSIMP-MESHES* globals with assimp-flat
   instances ready to be render"
  (declare (type single-float scale))
  (assert (probe-file file))
  (free-meshes) ;; !!!
  (let* ((scene (ai:import-into-lisp file))
         ;; add normals if missing
         (processing-flags
          (if (emptyp (ai:normals (aref (ai:meshes scene) 0)))
              (cons :ai-process-gen-smooth-normals *processing-flags*)
              *processing-flags*))
         (scene  (ai:import-into-lisp file :processing-flags processing-flags))
         (meshes (coerce (slot-value scene 'ai:meshes) 'list))
         (bones  (list-bones scene))
         (type   (if (emptyp bones) :textured :bones))
         (file-path (uiop:pathname-directory-pathname file)))
    ;; add things upto we find an error, if any
    (mapcar (lambda (mesh)
              (push (assimp-mesh-to-stream mesh
                                           scene
                                           file-path
                                           scale
                                           type)
                    *assimp-meshes*))
            meshes))
  T)

;;--------------------------------------------------
;; Draw
;;--------------------------------------------------
(defmethod draw ((actor assimp-thing)
                 camera
                 (time single-float))
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

(defmethod draw ((actor assimp-thing-with-bones)
                 camera
                 (time single-float))
  (with-slots (buf albedo normals scale) actor
    (map-g #'generic-tex-pipe-bones buf
           :scale scale
           ;; Lighting
           :model-world (model->world actor)
           :world-view (world->view camera)
           :view-clip  (projection camera)
           :offsets *chuesos*
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

;; https://github.com/cbaggers/cepl/issues/288
(defun-g vert-with-tbdata-bones ((vert assimp-with-bones)
                                 ;;(vert g-pnt) (tb tb-data) (bones assimp-bones)
                                 &uniform
                                 (model-world :mat4)
                                 (world-view :mat4)
                                 (view-clip :mat4)
                                 (scale :float)
                                 ;;
                                 (offsets (:mat4 32))
                                 ;; Parallax vars
                                 (light-pos :vec3)
                                 (cam-pos :vec3))
  (let* ((pos       (* scale (pos vert)))
         (norm      (norm vert))
         (uv        (treat-uvs (tex vert)))
         (norm      (* (m4:to-mat3 model-world) norm))
         (world-pos (* (+ (* (aref (weights vert) 0)
                             (aref offsets (int (aref (ids vert) 0))))
                          (* (aref (weights vert) 1)
                             (aref offsets (int (aref (ids vert) 1))))
                          ;; (* (aref (weights vert) 2) 0
                          ;;    (aref offsets (int (aref (ids vert) 2))))
                          ;; (* (aref (weights vert) 3) 0
                          ;;    (aref offsets (int (aref (ids vert) 3))))
                          )
                       (v! pos 1)))
         (world-pos (* model-world world-pos))
         ;;(world-pos (* model-world (v! pos 1)))
         (view-pos  (* world-view  world-pos))
         (clip-pos  (* view-clip   view-pos))
         (t0 (normalize
              (s~ (* model-world (v! (tangent vert) 0))
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
     ;;(v! 1 .2 1 0)
     ;;frag-pos
     ;;(normalize frag-norm)
     )))

;; parallax
;; (defun-g frag-tex-tbn ((uv :vec2)
;;                        (frag-norm :vec3)
;;                        (frag-pos :vec3)
;;                        (tbn :mat3)
;;                        (tan-light-pos :vec3)
;;                        (tan-cam-pos :vec3)
;;                        (tan-frag-pos :vec3)
;;                        &uniform
;;                        (cam-pos :vec3)
;;                        (albedo :sampler-2d)
;;                        (normap :sampler-2d)
;;                        (height-map :sampler-2d))
;;   (let* ((light-pos *pointlight-pos*)
;;          ;; Parallax
;;          (tan-cam-dir (- tan-cam-pos tan-frag-pos))
;;          (newuv (parallax-mapping uv tan-cam-dir height-map .1))
;;          ;; ---------
;;          (light-color (v! 1 1 1))
;;          (light-strength 1f0)
;;          ;;--------------------
;;          (vec-to-light (- light-pos frag-pos))
;;          (dir-to-light (normalize vec-to-light))
;;          ;;--------------------
;;          (color (expt (s~ (texture albedo newuv) :xyz)
;;                       (vec3 2.2)))
;;          (normal (norm-from-map normap newuv))
;;          (normal (normalize (* tbn normal))))
;;     (values
;;      (v! color 1)
;;      ;; (v! 1 1 1 1)
;;      ;;frag-pos
;;      ;;(normalize frag-norm)
;;      )))

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

(defpipeline-g generic-tex-pipe-bones ()
  :vertex (vert-with-tbdata-bones
           assimp-with-bones
           ;;g-pnt tb-data assimp-bones
           )
  :fragment (frag-tex-tbn :vec2 :vec3 :vec3 :mat3
                          ;; Parallax
                          :vec3 :vec3 :vec3))
