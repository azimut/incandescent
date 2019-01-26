(in-package #:incandescent)
;;--------------------------------------------------
;; Assimp - 3d object loader

(defstruct-g assimp-mesh
  (pos       :vec3)
  (normal    :vec3)
  (uv        :vec2)
  (tangent   :vec3)
  (bitangent :vec3))

(defun assimp-mesh-to-stream (mesh)
  (declare (ai:mesh mesh))
  (with-slots ((vertices       ai:vertices)
               (faces          ai:faces)
               (normals        ai:normals)
               (texture-coords ai:texture-coords)
               (tangents       ai:tangents)
               (bitangents     ai:bitangents))
      mesh
    (let ((texture-coords (elt texture-coords 0)))
      (assert (= (length bitangents)
                 (length tangents)
                 (length normals)
                 (length vertices)
                 (length texture-coords)))
      (let ((v-arr (make-gpu-array nil :dimensions (length vertices)
                                       :element-type 'assimp-mesh))
            (i-arr (make-gpu-array nil :dimensions (* 3 (length faces))
                                       :element-type :ushort)))
        (with-gpu-array-as-c-array (c-arr i-arr)
          (loop
            :for indices :across faces
            :for i :from 0 :by 3
            :do (setf (aref-c c-arr i)       (aref indices 0)
                      (aref-c c-arr (+ i 1)) (aref indices 1)
                      (aref-c c-arr (+ i 2)) (aref indices 2))))
        (with-gpu-array-as-c-array (c-arr v-arr)
          (loop
            :for v :across vertices
            :for n :across normals
            :for ta :across tangents
            :for bt :across bitangents
            :for tc :across texture-coords
            :for i :from 0
            :for a := (aref-c c-arr i)
            :do (setf (assimp-mesh-pos a) v
                      (assimp-mesh-normal a) n
                      (assimp-mesh-tangent a) ta
                      (assimp-mesh-bitangent a) bt
                      (assimp-mesh-uv a) (v! (x tc) (y tc)))))
        (make-buffer-stream v-arr :index-array i-arr)))))
