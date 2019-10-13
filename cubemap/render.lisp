(in-package #:incandescent)

;;--------------------------------------------------
;; Cubemap
;; Rendering order does not matter
;; Use it with a 1x1x1 box AND-test-function #'<=
(defun-g cubemap-vert ((g-pnt g-pnt)
                       &uniform
                       (view :mat4)
                       (projection :mat4))
  (let* ((pos3  (pos g-pnt))
         (pos4  (v! pos3 1))
         (cpos4 (* projection view pos4)))
    (values (s~ cpos4 :xyww)
            pos3)))

(defun-g cubemap-frag ((tc    :vec3)
                       &uniform
                       (color :vec3)
                       (tex   :sampler-cube))
  (let ((color3 (s~ (texture tex tc) :xyz)))
    ;; NOTE: Extra values for defer emmisivity
    (values (v! (* color color3) 1)
            (vec4 0)
            (vec4 0)
            (v! 0 1f0))))

(defpipeline-g cubemap-pipe ()
  :vertex   (cubemap-vert g-pnt)
  :fragment (cubemap-frag :vec3))
