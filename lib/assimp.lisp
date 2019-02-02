(in-package #:incandescent)
;;--------------------------------------------------
;; Assimp - 3d object loader

(defstruct-g assimp-mesh
  (pos       :vec3)
  (normal    :vec3)
  (uv        :vec2)
  (tangent   :vec3)
  (bitangent :vec3))

(defvar *assimp-meshes* NIL)
(defmethod free ((actor assimp-flat))
  (free (slot-value actor 'buf)))
(defun free-meshes ()
  (loop :for mesh :in *assimp-meshes*
     :do (free mesh)))

(defun assimp-mesh-to-stream (mesh)
  "takes a ASSIMP:MESH and returns a buffer-stream for it"
  (declare (ai:mesh mesh))
  (with-slots ((vertices       ai:vertices)
               (faces          ai:faces)
               (normals        ai:normals)
               (texture-coords ai:texture-coords)
               (tangents       ai:tangents)
               (bitangents     ai:bitangents))
      mesh
    (let* ((texture-coords (elt texture-coords 0))
           (lenv (length vertices))
           (lenf (* lenv)))
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
        (make-buffer-stream v-arr :index-array i-arr)))))

(defun assimp-load-meshes (file &optional (scale 1f0))
  "empties and fills the *ASSIMP-MESHES* globals with assimp-flat
   instances ready to be render"
  (declare (type single-float scale))
  (assert (probe-file file))
  (free-meshes)
  (let* ((scene (ai:import-into-lisp
                 file
                 :processing-flags '(:ai-process-triangulate
                                     :ai-process-calc-tangent-space)))
         (meshes  (loop :for mesh :across (slot-value scene 'ai:meshes)
                     :collect mesh))
         (buffers (mapcar #'assimp-mesh-to-stream meshes))
         (actors  (mapcar (lambda (buf) (make-instance 'assimp-flat
                                                  :buf buf
                                                  :scale scale))
                          buffers)))
    (mapcar (lambda (actor) (push actor *assimp-meshes*))
            actors)))
