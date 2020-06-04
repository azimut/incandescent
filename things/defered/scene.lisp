(in-package #:incandescent)

(defvar *blend-defer* (make-blending-params :source-rgb :one
                                            :destination-rgb :one))


(defmethod update ((camera pers) dt)
  #+nil
  (with-slots (pos rot) camera
    (setf rot (q:point-at (v! 0 1 0) pos (v! 10 1 0)))
    (if (< (z pos) 0f0)
        (setf (z pos) 40f0)
        (decf (z pos) .05f0))))

;;(make-scene 1)
(make-instance-light-volume :instances 1)
(defun make-scene (&optional (instances 10))
  (free-actors)
  (make-instance-light-volume); needs to happen 1st!
  (make-piso :pos (v! 0 .1 0))
  (make-piso :pos (v! 0 10 0) :rot (q:from-axis-angle (v! 0 0 1) (radians 180)))
  (make-roman-column)
  #+nil
  (loop :for d :from -25 :to 35 :by 5
        :do (make-roman-column :pos (v! 5 0 d))
            (make-roman-column :pos (v! -5 0 d)))
  ;;(reset-camera :pos (v! -3 3 40))
  #+nil
  (progn
    (setf *light-volumes* nil)
    (loop :repeat 50
          :for x := (serapeum:random-in-range -40f0 40f0)
          :for z := (serapeum:random-in-range -40f0 40f0)
          :do (make-light-volume :pos (v! x 0 z))))
  ;; (make-light-volume :pos (v! -2 1 0))
  ;; (make-light-volume :pos (v! 4 0 4))
  ;; (make-light-volume :pos (v! 5 0 -5))
  )

#+nil
(with-setf (cull-face) nil
  (gl:draw-buffer :none)
  (gl:clear :stencil-buffer-bit)
  (gl:stencil-func :always 0 0)
  (gl:stencil-op-separate :back :keep :incr-wrap :keep)
  (gl:stencil-op-separate :front :keep :decr-wrap :keep))

#+nil
(with-blending *blend-defer*
  (with-setf* ((depth-test-function) nil
               (cull-face) :front)
    (gl:draw-buffer :color-attachment4)
    (gl:stencil-func :not-equal 0 #xff)))
