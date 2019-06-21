(in-package #:incandescent)

(defclass env-map (actor)
  ((cubetex :initarg :cubetex)
   (cubesam :initarg :cubesam)
   (buf     :initform (box))))

(defun make-env-map (tex sam)
  (declare (type cepl:texture tex)
           (type cepl:sampler sam))
  (let ((obj (make-instance 'env-map :cubetex tex :cubesam sam)))
    (push obj *actors*)
    obj))

(defmethod draw ((actor env-map) camera time)
  (with-slots (buf cubesam) actor
    (with-setf* ((cull-face) :front
                 (depth-test-function) #'<=
                 (depth-mask) nil)
      (map-g #'cubemap-pipe buf
             :tex cubesam
             ;; Rotation without translation
             :view (q:to-mat4
                    (q:inverse (rot camera)))
             :projection (projection camera)))))
