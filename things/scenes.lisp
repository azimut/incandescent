(in-package :incandescent)

(defun make-scenes ()
  (free-actors)
  (free-scenes)
  (in-scene 0
    (make-clouds)
    (make-piso :rot (q:from-axis-angle (v! 0 0 1) (radians 180))
               :pos (v! 0 20 0))
    (make-box :pos (v! 0 5 0))
    (make-piso))
  (in-scene 1
    (make-clouds)
    (make-pbr-simple :pos (v! 0 5 0))
    (make-piso)))

;; (make-scenes)
;; (setf *scene-index* (alexandria:random-elt #(0 1)))
