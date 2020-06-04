(in-package #:incandescent)
;;--------------------------------------------------
;; Assimp - 3d object loader
;;
;; TODO: support more generic way to load that only returns the buffer
;; TODO: c-array for bones leaking memory on restart
;; TODO: fix the animation loop so it does it automatically...might be with a helper, is really manual now
;; TODO: scale bone transform

(defvar *default-albedo* "static/37.Paint01-1k/paint01_albedo.jpg"
  "override this locally to change the albedo")
(defvar *default-normal* "static/37.Paint01-1k/paint01_normal.jpg"
  "override this locally to change the normal map")
(defvar *default-specular* "static/37.Paint01-1k/paint01_height.jpg"
  "override this locally to change the normal map")
(defvar *assimp-buffers* (make-hash-table :test #'equal))

;; optimize-meshes:    looks like a good default
;; flip-u-vs:          needed to unwrap texture correctly
;; gen-smooth-normals: ONLY needed for meshes without normals
;; triangulate:        use it ALWAYS
;; calc-tangent-space: generates arc/bi tan from normals
;; process-preset-target-realtime-fast
;; process-preset-target-realtime-max-quality
;; process-preset-target-realtime-quality
;; x aiProcess_CalcTangentSpace
;; x aiProcess_GenSmoothNormals
;; - aiProcess_JoinIdenticalVertices
;; - aiProcess_ImproveCacheLocality
;; x aiProcess_LimitBoneWeights
;; x aiProcess_RemoveRedundantMaterials
;; - aiProcess_SplitLargeMeshes
;; - aiProcess_Triangulate
;; x aiProcess_GenUVCoords
;; - aiProcess_SortByPType
;; - aiProcess_FindDegenerates
;; - aiProcess_FindInvalidData
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
  #+nil
  (with-slots (buf) actor
    (free buf)
    (free (alexandria:lastcar (buffer-stream-gpu-arrays buf)))
    (free (car (car (buffer-stream-gpu-arrays buf))))))

(defmethod free ((actor assimp-thing-with-bones))
  (with-slots (bones) actor
    (free bones))
  #+nil
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

(defun make-gpu-index-array (faces n-vertex)
  "returns a CEPL:GPU-ARRAY, intended to be the index of a buffer-stream of mesh of
   FACES with N-VERTEX"
  (let* ((n-faces (* n-vertex 3))
         (i-array (make-gpu-array NIL :dimensions n-faces :element-type :ushort)))
    (with-gpu-array-as-c-array (c-arr i-array)
      (loop :for indices :across faces
            :for i :from 0 :by 3
            :do (setf (aref-c c-arr i)       (aref indices 0)
                      (aref-c c-arr (+ i 1)) (aref indices 1)
                      (aref-c c-arr (+ i 2)) (aref indices 2)))
      i-array)))

;; TODO: in the classimp version, it won't generate tangents if there are no uvs?
(defun make-gpu-vertex-array (vertices normals tangents bitangents &optional uvs bones)
  "returns a CEPL:GPU-ARRAY, built from the provided mesh information"
  (let* ((n-vertex  (length vertices))
         (has-bones (not (emptyp bones)))
         (has-uvs   (not (emptyp uvs)))
         (uvs       (or uvs ;; NOTE: we fill with fake uvs to respect g-pnt layout
                        (coerce (loop :repeat n-vertex
                                      :collect (v! 0 0))
                                'vector))))
    (cond (has-bones
           (let ((v-arr (make-gpu-array NIL :dimensions n-vertex
                                            :element-type 'assimp-with-bones)))
             (with-gpu-array-as-c-array (c-arr v-arr)
               (loop :for v  :across vertices
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
               v-arr)))
          (has-uvs
           (let ((v-arr (make-gpu-array NIL :dimensions n-vertex :element-type 'assimp-mesh)))
             (with-gpu-array-as-c-array (c-arr v-arr)
               (loop :for v  :across vertices
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
               v-arr)))
          (t
           (let ((v-arr (make-gpu-array NIL :dimensions n-vertex :element-type 'g-pnt)))
             (with-gpu-array-as-c-array (c-arr v-arr)
               (loop :for v  :across vertices
                     :for n  :across normals
                     :for tc :across uvs
                     :for i  :from 0
                     :for a  := (aref-c c-arr i)
                     :do (setf (assimp-mesh-pos a) v
                               (assimp-mesh-normal a) n
                               (assimp-mesh-uv a) (v! (x tc) (y tc))))
               v-arr))))))

(defun make-buffer-stream-cached (file mesh-index vertices faces normals tangents bitangents &optional uvs bones)
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

(defmethod assimp-mesh-to-stream (mesh scene file (type (eql :untextured)))
  "only textured assimp thing"
  (declare (ai:mesh mesh)
           (ai:scene scene))
  (with-slots ((vertices       ai:vertices)
               (faces          ai:faces)
               (normals        ai:normals))
      mesh
    (let ((mesh-index (position mesh (ai:meshes scene))))
      (assert (length= normals vertices))
      (list :buf (make-buffer-stream-cached file mesh-index
                                            vertices faces
                                            normals nil nil)))))

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
                           (get-tex (merge-pathnames norm-file file-path) :rgb8)
                           (get-tex *default-normal* nil t :rgb8)))
           (specular   (if spec-file
                           (get-tex (merge-pathnames spec-file file-path))
                           (get-tex *default-specular* nil t :r8))))
      (assert (length= bitangents
                       tangents
                       normals
                       vertices
                       uvs))
      (list :buf (make-buffer-stream-cached file mesh-index
                                            vertices faces
                                            normals tangents bitangents uvs)
            :albedo albedo
            :normals normal-map
            :specular specular))))

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
                           (get-tex (merge-pathnames norm-file file-path) :rgb8)
                           (get-tex *default-normal* nil t :rgb8)))
           (specular   (if spec-file
                           (get-tex (merge-pathnames spec-file file-path))
                           (get-tex *default-specular* nil t :r8)))
           ;; I need context for this...hackidy hack
           (bones-per-vertex (get-bones-per-vertex scene bones (length vertices))))
      (assert (length= bitangents
                       tangents
                       normals
                       vertices
                       uvs))
      (let ((buffer (make-buffer-stream-cached file mesh-index
                                               vertices faces normals
                                               tangents bitangents uvs
                                               bones-per-vertex)))
        (list :buf buffer
              :albedo albedo
              :normals normal-map
              :specular specular)))))

(defun assimp-safe-import-into-lisp (file)
  "wrapper around ai:import-into-lisp that ensures the proper thing is loaded"
  (let* ((scene (or (ai:import-into-lisp file)
                    (error "cannot simple load the file")))
         ;; add normals if missing
         (processing-flags
           (if (emptyp (ai:normals (aref (ai:meshes scene) 0)))
               (cons :ai-process-gen-smooth-normals *processing-flags*)
               *processing-flags*))
         (scene (ai:import-into-lisp file :processing-flags (print
                                                             processing-flags)
                                          :properties
                                          '(:pp-slm-triangle-limit 25000)
                                          ;;'(:pp-slm-vertex-limit 20000)
                                          )))
    ;; TODO: error instead if there is an untextured among textured
    ;; Error if all texture coords are missing :(
    #+nil
    (assert (notevery #'zerop
                      (map 'vector (lambda (mesh) (length (ai:texture-coords mesh)))
                           (ai:meshes scene))))
    scene))

(defgeneric assimp-get-type (obj))
(defmethod assimp-get-type ((obj ai:scene))
  (let ((bones (list-bones obj)))
    (if (emptyp bones) :textured :bones)))
;; TODO: support untextured with bones
(defmethod assimp-get-type ((obj ai:mesh))
  (let ((bones (ai:bones obj))
        (n-uvs (ai:texture-coords obj)))
    (cond ((and (not (emptyp n-uvs))
                (emptyp bones)) :textured)
          ((not (emptyp bones)) :bones)
          (t                    :untextured))))

;;--------------------------------------------------

(defun remove-nil-plist (plist)
  (loop :for (p v) :on plist :by #'cddr
        :when v
        :append (list p v)))

(defun assimp-load-meshes (file)
  "returns a list of meshes, each one being a plist. Everything should
   be cached."
  (let* ((path   (truename file))
         (scene  (assimp-safe-import-into-lisp path))
         (meshes (ai:meshes scene)))
    (loop :for mesh :across meshes
          ;; NOTE: Drop meshes with not UVs, afaik they are placeholders
          ;;       and can ruin the load or rendering
          ;;:when (not (emptyp (ai:texture-coords mesh)))
          :collect
          ;; NOTE: We delay the type check because there could be meshes
          ;; with and without bones on the same scene.
             (let ((type (assimp-get-type mesh)))
               (destructuring-bind (&key buf albedo normals specular)
                   (assimp-mesh-to-stream mesh scene path type)
                 (remove-nil-plist
                  (list
                   :type type
                   :scene scene
                   :buf buf
                   :albedo albedo
                   :normals normals
                   :specular specular
                   :bones (when (eq type :bones)
                            (make-c-array ;; TODO: leaking
                             (coerce
                              ;; NOTE: init using the first transform in the animation, for those that only have 1
                              ;; frame of "animation"
                              (get-bones-tranforms scene :frame 0)
                              'list) :element-type :mat4))
                   :duration (when (eq type :bones)
                               (if (not (emptyp (ai:animations scene)))
                                   (coerce
                                    (ai:duration
                                     (aref (ai:animations scene) 0))
                                    'single-float)
                                   0f0)))))))))
