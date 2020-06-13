(in-package #:incandescent)

(defun render-all-the-things (actor camera time)
  (declare (single-float time))
  (update actor time)
  (draw actor camera time))

(defgeneric draw (actor camera time))
(defmethod draw :around (actor camera time)
  (when (slot-value actor 'draw-p)
    (call-next-method)))
(defmethod draw (actor camera time))
