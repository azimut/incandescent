(in-package #:incandescent)

;; 2D

(defstruct-g texticle
  (position     :vec2)
  (in-tex-coord :vec2))

;;

(defun-g calc-text-uvs ((position :vec2)
                        (extent   :vec4)
                        (scale    :float))
  (vec4 (* scale (/ (+ (x position) (x extent))
                    (+ 1 (z extent))))
        (* scale (/ (- (y position) (y extent))
                    (+  1 (w extent))))
        0.0
        1.0))

;;

(defun-g tvert ((text-info texticle) &uniform
                (scale  :float)
                (extent :vec4))
  (with-slots (position in-tex-coord) text-info
    (values (calc-text-uvs position extent scale)
            in-tex-coord)))

(defun-g tfrag ((tex-coord  :vec2) &uniform
                (tex-image  :sampler-2d)
                (text-color :vec3))
  (let ((intensity (x (texture tex-image tex-coord))))
    ;; NOTE: I could use this for transparency, but i would need another
    ;; pass due defered fog...
    ;;(* (v! 5 5 5 1) (v! 1 1 1 intensity))
    (v! (* text-color (v3! intensity))
        (step .01 intensity))
    ;;(v! 1 0 0 1)
    ;;intensity
    ))

(defpipeline-g fondness ()
  :vertex   (tvert texticle)
  :fragment (tfrag :vec2))

;;--------------------------------------------------

;; http://www.geeks3d.com/20140807/billboarding-vertex-shader-glsl/
(defun-g text-billboard-vert ((pos         :vec3)
                              &uniform
                              (scale       :float)
                              (model-world :mat4)
                              (world-view  :mat4)
                              (view-clip   :mat4))
  (let* ((mv (* world-view model-world)))
    (setf (aref mv 0 0) 1f0
          (aref mv 0 1) 0f0
          (aref mv 0 2) 0f0
          ;;
          (aref mv 1 0) 0f0
          (aref mv 1 1) 1f0
          (aref mv 1 2) 0f0
          ;;
          (aref mv 2 0) 0f0
          (aref mv 2 1) 0f0
          (aref mv 2 2) 1f0)
    (values (* view-clip (* mv (v! (* scale pos) 1)))
            (+ .5 (* .5 (v! (x pos) (y pos)))))))

(defun-g text-billboard-frag ((uv    :vec2) &uniform
                              (color :vec3)
                              (sam   :sampler-2d))
  (let* ((uv (v! (+ -.2 (* 1 (x uv)))
                 (+ -3.5 (* 7 (y uv)))))
         (intensity  (x (texture sam uv))))
    (v! (* color (v3! intensity))
        intensity)))

(defpipeline-g text-billboard-pipe ()
  :vertex   (text-billboard-vert :vec3)
  :fragment (text-billboard-frag :vec2))
