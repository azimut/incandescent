(in-package #:incandescent)

;; 2D

(defstruct-g texticle
  (position     :vec2)
  (in-tex-coord :vec2))

;;

(defun-g calc-text-uvs ((position :vec2)
                        (extent   :vec4))
  (vec4 (* 2 (/ (+ (x position) (x extent))
                (+ -1 (z extent))))
        (* 2 (/ (- (y position) (y extent))
                (+  1 (w extent))))
        0.0
        1.0))

;;

(defun-g tvert ((text-info texticle) &uniform (extent :vec4))
  (with-slots (position in-tex-coord) text-info
    (values (calc-text-uvs position extent)
            in-tex-coord)))

(defun-g tfrag ((tex-coord  :vec2) &uniform
                (tex-image  :sampler-2d)
                (text-color :vec4))
  (let ((intensity (x (texture tex-image tex-coord))))
    ;; NOTE: I could use this for transparency, but i would need another
    ;; pass due defered fog...
    ;;(* (v! 5 5 5 1) (v! 1 1 1 intensity))
    (v! (v3! intensity) (step .1 intensity))
    ;;(v! 1 0 0 1)
    ))

(defpipeline-g fondness ()
  :vertex   (tvert texticle)
  :fragment (tfrag :vec2))
