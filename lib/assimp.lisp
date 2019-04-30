(in-package #:incandescent)
;;--------------------------------------------------
;; Assimp - 3d object loader
;;
;; TODO: support objets with no UVs, like low poly things
;; TODO: support more generic way to load that only returns the buffer
;; TODO: c-array for bones leaking memory on restart
;; TODO: fix the animation loop so it does it automatically...might be with a helper, is really manual now

(defvar *default-animation* 0 "default animation index")
(defvar *default-albedo* "static/37.Paint01-1k/paint01_albedo.jpg"
  "override this locally to change the albedo")
(defvar *default-normal* "static/37.Paint01-1k/paint01_normal.jpg"
  "override this locally to change the normal map")
(defvar *default-specular* "static/37.Paint01-1k/paint01_height.jpg"
  "override this locally to change the normal map")
(defvar *max-bones-per-vertex* 4)
(defvar *assimp-buffers* (make-hash-table :test #'equal))

;; flip-u-vs:          needed to unwrap texture correctly
;; gen-smooth-normals: ONLY needed for meshes without normals
;; triangulate:        use it ALWAYS
;; calc-tangent-space: generates arc/bi tan from normals
(defvar *processing-flags* '(:ai-process-triangulate
                             :ai-process-flip-u-vs
                             :ai-process-calc-tangent-space))

(defclass assimp-thing (actor)
  ((albedo   :initarg :albedo)
   (normals  :initarg :normals)
   (specular :initarg :specular)
   (scene    :initarg :scene)))

(defclass assimp-thing-with-bones (actor)
  ((albedo   :initarg :albedo)
   (normals  :initarg :normals)
   (specular :initarg :specular)
   (scene    :initarg :scene)
   (bones    :initarg :bones
             :documentation "c-array of mat4s, of transforms for each bone in the whole scene")
   (duration :initform 0f0 :initarg :duration)))

(defmethod update ((actor assimp-thing) dt))
(defmethod update ((actor assimp-thing-with-bones) dt))

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

(defstruct-g assimp-bones
  (ids     :vec4)
  (weights :vec4))

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

(defmethod print-object ((obj ai::vector-key) out)
  (print-unreadable-object (obj out :type t)
    (format out "~a" (slot-value obj 'ai::time))))

;;--------------------------------------------------
;; Loaders
;;--------------------------------------------------

(defmethod free ((actor assimp-thing))
  (with-slots (buf) actor
    (free buf)
    (free (alexandria:lastcar (buffer-stream-gpu-arrays buf)))
    (free (car (car (buffer-stream-gpu-arrays buf))))))

(defmethod free ((actor assimp-thing-with-bones))
  (with-slots (buf) actor
    (free buf)
    (free (alexandria:lastcar (buffer-stream-gpu-arrays buf)))
    (free (car (car (buffer-stream-gpu-arrays buf))))))

(defun free-assimp-buffers ()
  (alexandria:maphash-values
   (lambda (buffer)
     (free buffer)
     (free (alexandria:lastcar (buffer-stream-gpu-arrays buffer)))
     (free (car (car (buffer-stream-gpu-arrays buffer)))))
   *assimp-buffers*)
  (clrhash *assimp-buffers*))

;;--------------------------------------------------
;; Bones loader

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

(defun get-bones-per-vertex (scene mesh-bones n-vertices)
  "Returns an array of lists of tuples/cons pairs.
   Run once at mesh LOAD time.
   ex: #(((1 . .9) (2 . .1)) ((10 . .2) (20 . .8)))"
  (declare (type ai:scene scene)
           (type vector mesh-bones)
           (type positive-fixnum n-vertices))
  (let ((unique-scene-bones (list-bones-unique scene))
        (v-to-bones (make-array n-vertices :initial-element NIL)))
    (loop
       :for bone :across mesh-bones
       :for bone-id := (position (ai:name bone) unique-scene-bones
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
                  ;; Sort descending by weights
                  (setf (aref v-to-bones v)
                        (sort (aref v-to-bones v) #'>
                              :key #'cdr))))))
    v-to-bones))

;;--------------------------------------------------
;; Bone animation helpers

(defun find-index (etime positions)
  "returns the index position matching the current ETIME"
  (declare (type vector positions)
           (type number etime))
  (let ((pos (position-if (lambda (p) (< etime (slot-value p 'time)))
                          positions)))
    (if pos
        (max 0 (1- pos))
        0)))

(defun calc-interpolated-position (etime positions)
  "returns a vec3"
  (declare (type vector positions))
  (if (or (< etime (slot-value (aref positions 0) 'time))
          (length= 1 positions))
      (ai:value (aref positions 0))
      (let* ((index       (find-index etime positions))
             (next-index  (1+ index))
             (current-pos (aref positions index))
             (next-pos    (aref positions next-index)))
        (with-slots ((ctime time) (start ai:value)) current-pos
          (with-slots ((ntime time) (end ai:value)) next-pos
            (let* ((dt     (- ntime ctime))
                   (factor (/ (- etime ctime) dt))
                   (delta  (v3:- end start)))
              (v3:+ start
                    (v3:*s delta (coerce factor 'single-float)))))))))

(defun calc-interpolated-rotation (etime rotations)
  "returns a quaternion"
  (declare (type vector rotations))
  (if (or (< etime (slot-value (aref rotations 0) 'time))
          (length= 1 rotations))
      (ai:value (aref rotations 0))
      (let* ((index       (find-index etime rotations))
             (next-index  (1+ index))
             (current-rot (aref rotations index))
             (next-rot    (aref rotations next-index)))
        (with-slots ((ctime time) (start ai:value)) current-rot
          (with-slots ((ntime time) (end ai:value)) next-rot
            (let* ((dt      (- ntime ctime))
                   (factor  (/ (- etime ctime) dt))
                   (qinterp (q:lerp start end (coerce factor 'single-float))))
              (q:normalize qinterp)))))))

(defun get-frame-transform (node-animation frame)
  "returns a matrix"
  (declare (type ai::node-animation node-animation))
  (with-slots ((pos-keys ai::position-keys)
               (rot-keys ai::rotation-keys)
               (sca-keys ai::scaling-keys))
      node-animation
    ;; reset frame position
    (setf frame (mod frame (length rot-keys)))
    ;; Calculate tranform matrix based on time
    (m4-n:*
     (m4:translation (ai:value (aref pos-keys frame)))
     (q:to-mat4      (ai:value (aref rot-keys frame)))
     ;;(m4:scale (v3! 1.6))
     ;;(m4:scale (ai:value (aref sca-keys 0)))
     )))

(defun get-time-transform (node-animation time)
  "calculate the tansform matrix based on time
returns a matrix"
  (declare (type ai::node-animation node-animation))
  (with-slots ((pos-keys ai::position-keys)
               (rot-keys ai::rotation-keys)
               (sca-keys ai::scaling-keys))
      node-animation
    (let ((ipos (calc-interpolated-position time pos-keys))
          (irot (calc-interpolated-rotation time rot-keys)))
      (m4-n:*
       (m4:translation ipos)
       (q:to-mat4 irot)
       ;;(m4:scale (v3! .9f0))
       ;;(m4:scale (ai:value (aref sca-keys 0)))
       ))))

(defgeneric get-nodes-transforms (scene node-type &key frame time)
  (:documentation "returns a hash of mat4's with each node transform
for value and node name for the key")
  (:method ((scene ai:scene) (node-type (eql :static)) &key frame time)
    (let ((nodes-transforms (make-hash-table :test #'equal)))
      (labels ((walk-node (node parent-transform)
                 (declare (type ai:node node)
                          (type vector parent-transform))
                 (with-slots ((name      ai:name)
                              (transform ai:transform)
                              (children  ai:children))
                     node
                   (let ((global
                          (m4:* parent-transform (m4:transpose transform))))
                     (setf (gethash name nodes-transforms) global)
                     (map 'vector
                          (lambda (c) (walk-node c global))
                          children)))))
        (walk-node (ai:root-node scene) (m4:identity)))
      nodes-transforms))
  (:method ((scene ai:scene) (node-type (eql :animated)) &key frame time)
    (let* ((animation        (aref (ai:animations scene) *default-animation*))
           (duration         (ai:duration animation))
           ;; NOTE: animation-index is a hash lookup table for BONE>NODE-ANIMATION
           (animation-index  (ai:index animation))
           ;; NOTE: temporal lookup table for bones *thinking emoji*
           (nodes-transforms (make-hash-table :test #'equal)))
      (declare (type hash-table animation-index))
      (labels
          ((walk-node (node parent-transform)
             (declare (type ai:node node) (type vector parent-transform))
             (with-slots ((name      ai:name)
                          (transform ai:transform)
                          (children  ai:children))
                 node
               ;; FIXME: see below mess
               (let* ((node-anim (gethash name animation-index))
                      (time-transform
                       (when node-anim
                         (if frame
                             (get-frame-transform node-anim frame)
                             (get-time-transform  node-anim (mod time duration)
                                                  ))))
                      (final-transform (if time-transform
                                           time-transform
                                           (m4:transpose transform)))
                      (global (m4:* parent-transform
                                    final-transform)))
                 (setf (gethash name nodes-transforms) global)
                 ;; WALK!
                 (map 'vector
                      (lambda (c) (walk-node c global))
                      children)))))
        (walk-node (ai:root-node scene)
                   (m4:identity)))
      nodes-transforms)))

(defun get-bones-tranforms (scene &key (frame 0 frame-p) (time 0 time-p))
  "ANIMATIONLESS
   returns an array with the m4 matrices of each bone offset"
  (declare (ai:scene scene))
  (let* ((root-offset      (m4:inverse
                            (m4:transpose
                             (ai:transform (ai:root-node scene)))))
         (unique-bones     (list-bones-unique scene))
         (bones-transforms (make-array (length unique-bones)))
         ;; Note: I might have bones but NO animation
         (node-type        (if (emptyp (ai:animations scene))
                               :static
                               :animated))
         (valid            (assert (or (eq :static node-type)
                                       (or frame-p time-p))))
         ;;(nodes-transforms (get-nodes-transforms scene :static))
         (nodes-transforms (if frame-p
                               (get-nodes-transforms scene node-type
                                                     :frame frame)
                               (get-nodes-transforms scene node-type
                                                     :time time))))
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
                   ;;node-transform
                   (if (m4:0p offset)
                       (m4:* root-offset
                             node-transform)
                       (m4:* root-offset
                             node-transform
                             (m4:transpose offset)))))))
    bones-transforms))

;;--------------------------------------------------

(defun make-gpu-index-array (faces n-vertex)
  "returns a CEPL:GPU-ARRAY, intended to be the index of a buffer-stream of mesh of
   FACES with N-VERTEX"
  (let* ((n-faces (* n-vertex 3))
         (i-array (make-gpu-array NIL :dimensions n-faces :element-type :ushort)))
    (with-gpu-array-as-c-array (c-arr i-array)
      (loop
         :for indices :across faces
         :for i :from 0 :by 3
         :do (setf (aref-c c-arr i)       (aref indices 0)
                   (aref-c c-arr (+ i 1)) (aref indices 1)
                   (aref-c c-arr (+ i 2)) (aref indices 2)))
      i-array)))

(defun make-gpu-vertex-array (vertices normals tangents bitangents uvs &optional bones)
  "returns a CEPL:GPU-ARRAY, built from the provided mesh information"
  (if bones
      (let* ((n-vertex (length vertices))
             (v-arr    (make-gpu-array NIL :dimensions n-vertex :element-type 'assimp-with-bones)))
        (with-gpu-array-as-c-array (c-arr v-arr)
          (loop
             :for v  :across vertices
             :for n  :across normals
             :for ta :across tangents
             :for bt :across bitangents
             :for tc :across uvs
             :for bv :across bones ;; per vertex
             :for i  :from 0
             :for a  := (aref-c c-arr i)
             :do
               (setf (assimp-mesh-pos a) v
                     (assimp-mesh-normal a) n
                     (assimp-mesh-tangent a) ta
                     (assimp-mesh-bitangent a) bt
                     (assimp-mesh-uv a) (v! (x tc) (y tc)))

               (setf (ids a)
                     (v! (serapeum:pad-end
                          (map 'vector #'car bv) *max-bones-per-vertex* 0)))
               (setf (weights a)
                     (v! (serapeum:pad-end
                          (map 'vector #'cdr bv) *max-bones-per-vertex*  0))))
          v-arr))
      (let* ((n-vertex (length vertices))
             (v-arr    (make-gpu-array NIL :dimensions n-vertex :element-type 'assimp-mesh)))
        (with-gpu-array-as-c-array (c-arr v-arr)
          (loop
             :for v  :across vertices
             :for n  :across normals
             :for ta :across tangents
             :for bt :across bitangents
             :for tc :across uvs
             :for i  :from 0
             :for a  := (aref-c c-arr i)
             :do (setf (assimp-mesh-pos a) v
                       (assimp-mesh-normal a) n
                       (assimp-mesh-tangent a) ta
                       (assimp-mesh-bitangent a) bt
                       (assimp-mesh-uv a) (v! (x tc) (y tc))))
          v-arr))))

(defun make-buffer-stream-cached (file mesh-index vertices faces normals tangents bitangents uvs &optional bones)
  "returns a CEPL:BUFFER-STREAM"
  (declare (type fixnum mesh-index))
  (let ((key (cons file mesh-index)))
    (or (gethash key *assimp-buffers*)
        (let* ((v-arr  (make-gpu-vertex-array vertices normals tangents bitangents uvs bones))
               (i-arr  (make-gpu-index-array faces (length vertices)))
               (buffer (make-buffer-stream v-arr :index-array i-arr)))
          (setf (gethash key *assimp-buffers*)
                buffer)))))

(defun get-texture-path (material key)
  "returns a string with the path to the image file for KEY in MATERIAL hash"
  (declare (type hash-table material) (type symbol key))
  (when-let* ((textures (gethash "$tex.file" material))
              (filepath (third (assoc key textures))))
    ;; Lastly try to remove windows paths
    (if (cl-ppcre:scan "\\" filepath)
        (alexandria:lastcar
         (cl-ppcre:split "\\" filepath))
        filepath)))

;;--------------------------------------------------

(defgeneric assimp-mesh-to-stream (mesh scene file type))
(defmethod assimp-mesh-to-stream (mesh scene file (type (eql :textured)))
  "only textured assimp thing"
  (declare (ai:mesh mesh)
           (ai:scene scene))
  (with-slots ((vertices       ai:vertices)
               (faces          ai:faces)
               (normals        ai:normals)
               (texture-coords ai:texture-coords)
               (tangents       ai:tangents)
               (bitangents     ai:bitangents)
               (mat-index      ai:material-index))
      mesh
    (let* ((mesh-index (position mesh (ai:meshes scene)))
           (uvs        (elt texture-coords 0))
           (material   (aref (slot-value scene 'ai:materials) mat-index))
           (tex-file   (get-texture-path material :ai-texture-type-diffuse))
           (norm-file  (get-texture-path material :ai-texture-type-height))
           (spec-file  (get-texture-path material :ai-texture-type-specular))
           (file-path  (uiop:pathname-directory-pathname file))
           (albedo     (if tex-file
                           (get-tex (merge-pathnames tex-file file-path))
                           (get-tex *default-albedo*)))
           (normal-map (if norm-file
                           (get-tex (merge-pathnames norm-file file-path))
                           (get-tex *default-normal* nil t)))
           (specular   (if spec-file
                           (get-tex (merge-pathnames spec-file file-path))
                           (get-tex *default-specular* nil t :r8))))
      (assert (length= bitangents
                       tangents
                       normals
                       vertices
                       uvs))
      (values (make-buffer-stream-cached file mesh-index
                                         vertices faces
                                         normals tangents bitangents uvs)
              albedo
              normal-map
              specular))))

(defmethod assimp-mesh-to-stream (mesh scene file (type (eql :bones)))
  "returns an assimp actor object"
  (declare (ai:mesh mesh)
           (ai:scene scene))
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
           (mesh-index (position mesh (ai:meshes scene)))
           (texture-coords (elt texture-coords 0))
           (material   (aref (slot-value scene 'ai:materials) mat-index))
           (tex-file   (get-texture-path material :ai-texture-type-diffuse))
           (norm-file  (get-texture-path material :ai-texture-type-height))
           (spec-file  (get-texture-path material :ai-texture-type-specular))
           (file-path  (uiop:pathname-directory-pathname file))
           (albedo     (if tex-file
                           (get-tex (merge-pathnames tex-file file-path))
                           (get-tex *default-albedo*)))
           (normal-map (if norm-file
                           (get-tex (merge-pathnames norm-file file-path))
                           (get-tex *default-normal* nil t)))
           (specular   (if spec-file
                           (get-tex (merge-pathnames spec-file file-path))
                           (get-tex *default-specular* nil t :r8)))
           ;; I need context for this...hackidy hack
           (bones-per-vertex (get-bones-per-vertex scene bones lenv)))
      (assert (length= bitangents
                       tangents
                       normals
                       vertices
                       texture-coords))
      (let ((buffer (make-buffer-stream-cached file mesh-index
                                               vertices faces normals
                                               tangents bitangents texture-coords
                                               bones-per-vertex)))
        (values buffer
                albedo
                normal-map
                specular)))))

(defun assimp-safe-import-into-lisp (file)
  "wrapper around ai:import-into-lisp that ensures the proper thing is loaded"
  (let* ((scene (or (ai:import-into-lisp file)
                    (error "cannot simple load the file")))
         ;; add normals if missing
         (processing-flags
          (if (emptyp (ai:normals (aref (ai:meshes scene) 0)))
              (cons :ai-process-gen-smooth-normals *processing-flags*)
              *processing-flags*))
         (scene (ai:import-into-lisp file :processing-flags processing-flags)))
    ;; Error if all texture coords are missing :(
    (assert (notevery #'zerop
                      (map 'vector (lambda (mesh) (length (ai:texture-coords mesh)))
                           (ai:meshes scene))))
    scene))

(defgeneric assimp-get-type (obj))
(defmethod assimp-get-type ((obj ai:scene))
  (let ((bones (list-bones obj)))
    (if (emptyp bones) :textured :bones)))
(defmethod assimp-get-type ((obj ai:mesh))
  (let ((bones (ai:bones obj)))
    (if (emptyp bones) :textured :bones)))

;;--------------------------------------------------
;; FIXME: see below mess
(defun assimp-load-meshes (file &key (scale 1f0) (pos (v! 0 0 0)) (rot (q:identity)) (instantiate-p t))
  "returns a list of actor classes, instances ready to be render"
  (declare (type single-float scale))
  (let* ((path   (resolve-path file))
         (scene  (assimp-safe-import-into-lisp path))
         (meshes (slot-value scene 'ai:meshes)))
    (loop
       :for mesh :across meshes
       ;; NOTE: Drop meshes with not UVs, afaik they are placeholders
       ;;       and can ruin the load or rendering
       :when (not (emptyp (ai:texture-coords mesh)))
       :collect
       ;; NOTE: We delay the type check because there could be meshes
       ;; without bones and meshes with on the same scene.
         (let ((type (assimp-get-type mesh)))
           (multiple-value-bind (buf albedo normals specular)
               (assimp-mesh-to-stream mesh scene path type)
             (if instantiate-p
                 (ecase type
                   (:textured (make-instance 'assimp-thing
                                             :scene scene
                                             :buf buf :albedo albedo :normals normals
                                             :specular specular
                                             :scale scale
                                             :pos pos :rot rot))
                   (:bones
                    (make-instance 'assimp-thing-with-bones
                                   :duration (if (not (emptyp (ai:animations scene)))
                                                 (coerce
                                                  (ai:duration
                                                   (aref (ai:animations scene) 0))
                                                  'single-float)
                                                 0f0)
                                   :bones (make-c-array
                                           (coerce
                                            ;; NOTE: init using the first transform in the animation, for those that only have 1
                                            ;; frame of "animation"
                                            (get-bones-tranforms scene :frame 0)
                                            'list) :element-type :mat4)
                                   :scene scene
                                   :buf buf :albedo albedo :normals normals
                                   :specular specular
                                   :scale scale
                                   :pos pos :rot rot)))
                 (list buf
                       albedo
                       normals
                       specular
                       scene)))))))

;;--------------------------------------------------
;; Draw
;;--------------------------------------------------
(defmethod draw ((actor assimp-thing)
                 camera
                 (time single-float))
  (with-slots (buf albedo normals scale specular) actor
    (map-g #'assimp-tex-pipe-simple buf
           :scale scale
           ;; Lighting
           :model-world (model->world actor)
           :world-view (world->view camera)
           :view-clip  (projection camera)
           ;; PBR
           :cam-pos (pos camera)
           :albedo albedo
           :time time
           :specular specular
           :normals normals)))

(defmethod draw ((actor assimp-thing-with-bones)
                 camera
                 (time single-float))
  (with-slots (buf albedo normals scale bones) actor
    (map-g #'assimp-tex-pipe-bones buf
           :scale scale
           ;; Lighting
           :model-world (model->world actor)
           :world-view (world->view camera)
           :view-clip  (projection camera)
           :offsets bones
           ;; PBR
           :albedo albedo
           :normals normals)))

;;--------------------------------------------------
;; Renders
;;--------------------------------------------------

;; 3D - g-pnt with tangent info in tb-data AND textures
(defun-g vert-with-tbdata ((vert g-pnt)
                           (tb tb-data)
                           (bones assimp-bones)
                           &uniform
                           (model-world :mat4)
                           (world-view :mat4)
                           (view-clip :mat4)
                           (scale :float)
                           (offsets (:mat4 36))
                           ;; Parallax vars
                           (light-pos :vec3)
                           (cam-pos :vec3))
  (let* ((pos       (* scale (pos vert)))
         (norm      (norm vert))
         (uv        (treat-uvs (tex vert)))
         (norm      (* (m4:to-mat3 model-world) norm))
         ;;(world-pos (* model-world world-pos))
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
(defun-g vert-with-tbdata-bones ((vert g-pnt)
                                 (tb tb-data)
                                 (bones assimp-bones)
                                 ;;(vert g-pnt) (tb tb-data) (bones assimp-bones)
                                 &uniform
                                 (model-world :mat4)
                                 (world-view :mat4)
                                 (view-clip :mat4)
                                 (scale :float)
                                 ;;
                                 (offsets (:mat4 100)) ;; FIXME
                                 ;; Parallax vars
                                 (light-pos :vec3)
                                 (cam-pos :vec3))
  (let* ((pos       (pos vert))
         (norm      (norm vert))
         (uv        (treat-uvs (tex vert)))
         (norm      (* (m4:to-mat3 model-world) norm))
         (world-pos (* (m4:scale (v3! scale)) ;; FIXME
                       model-world
                       (* (aref (assimp-bones-weights bones) 0)
                          (aref offsets (int (aref (assimp-bones-ids bones) 0))))
                       ;; (* (aref (assimp-bones-weights bones) 1)
                       ;;    (aref offsets (int (aref (assimp-bones-ids bones) 1))))
                       ;; (* (aref (assimp-bones-weights bones) 2)
                       ;;    (aref offsets (int (aref (assimp-bones-ids bones) 2))))
                       ;; (* (aref (assimp-bones-weights bones) 3)
                       ;;    (aref offsets (int (aref (assimp-bones-ids bones) 3))))
                       (v! pos 1)))
         ;;(world-pos (* model-world world-pos))
         ;;(world-pos (* model-world (v! pos 1)))
         (view-pos  (* world-view world-pos))
         (clip-pos  (* view-clip  view-pos))
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
                       (time :float)
                       (albedo :sampler-2d)
                       (normals :sampler-2d)
                       (specular :sampler-2d))
  (let* ((color (expt (s~ (texture albedo uv) :xyz)
                      (vec3 2.2)))
         (normal (norm-from-map normals uv))
         (normal (normalize (* tbn normal)))
         (color (dir-light-apply color
                                 (v! 1 1 1)
                                 (v! 100 1000 100)
                                 frag-pos
                                 normal)))
    (values
     (v! color 1)
     ;;(v! 1 .2 1 0)
     ;;frag-pos
     (normalize frag-norm))))

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

(defpipeline-g assimp-tex-pipe ()
  :vertex (vert-with-tbdata g-pnt tb-data assimp-bones)
  :fragment (frag-tex-tbn :vec2 :vec3 :vec3 :mat3
                          ;; Parallax
                          :vec3 :vec3 :vec3))

(defpipeline-g assimp-tex-pipe-simple ()
  :vertex (vert-with-tbdata g-pnt tb-data assimp-bones)
  :fragment (frag-tex-tbn :vec2 :vec3 :vec3 :mat3
                          ;; Parallax
                          :vec3 :vec3 :vec3))

(defpipeline-g assimp-tex-pipe-bones ()
  :vertex (vert-with-tbdata-bones g-pnt tb-data assimp-bones)
  :fragment (frag-tex-tbn :vec2 :vec3 :vec3 :mat3
                          ;; Parallax
                          :vec3 :vec3 :vec3))

;;--------------------------------------------------

