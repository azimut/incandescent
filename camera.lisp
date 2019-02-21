(in-package #:incandescent)

(defclass camera ()
  ((name :initarg :name :accessor camera-name)
   (pos  :initarg :pos  :accessor pos)
   (rot  :initarg :rot  :accessor rot)
   (near :initarg :near :accessor near)
   (far  :initarg :far  :accessor far)
   (frame-size :initarg :frame-size
               :accessor frame-size))
  (:default-initargs
   :name (gensym)
   :pos (v! 0 0 0)
   :rot (q:identity)
   :near .1
   :far 400f0
   :frame-size nil))

(defclass orth (camera) ())
(defclass pers (camera)
  ((fov :initarg :fov :accessor fov))
  (:default-initargs
   :fov 60f0))

(defparameter *camera* (make-instance 'pers :far 1000f0))

(defparameter *camera1* (make-instance 'orth))
(defparameter *cameras* (list *camera* *camera1*))
(defparameter *currentcamera* *camera*)

(defun reset-camera (&optional (camera *currentcamera*))
  (setf (pos camera) (v! 0 0 10))
  (setf (rot camera) (q:identity)))

(defun next-camera ()
  "rotates current value on *CURRRENTCAMERA*
   for each on *CAMERAS*"
  (setf *cameras* (alexandria:rotate *cameras*))
  (setf *currentcamera* (first-elt *cameras*))
  (values))

(defun world->view (camera)
  (m4:* (q:to-mat4      (q:inverse (rot camera)))
        (m4:translation (v3:negate (pos camera)))))

(defgeneric projection (camera)
  (:documentation "view to clip")
  (:method ((camera pers))
    (let ((fs (or (frame-size camera)
                  (viewport-resolution (current-viewport)))))
      (rtg-math.projection:perspective
       (x fs)
       (y fs)
       (near camera)
       (far camera)
       (fov camera))))
  (:method ((camera orth))
    (let ((fs (or (frame-size camera)
                  (viewport-resolution (current-viewport)))))
      (rtg-math.projection:orthographic
       (x fs)
       (y fs)
       (near camera)
       (far camera)))))

(defun world->clip (camera)
  (m4:* (projection camera)
        (world->view camera)))

;; https://github.com/Flafla2/Generic-Raymarch-Unity/blob/master/Assets/RaymarchGeneric.cs
(defmethod get-frustum-corners ((camera pers))
  "Stores the normalized rays representing the camera frustum in a 4x4 matrix.
  Each row is a vector.
  The following rays are stored in each row (in eyespace, not worldspace):
  Top Left corner:     row=0
  Top Right corner:    row=1
  Bottom Right corner: row=2
  Bottom Left corner:  row=3"
  (let* ((fov          (fov camera))
         ;; (aspect       (/ (first *dimensions*)
         ;;                  (second *dimensions*)))
         (aspect 1f0)
         (fov-half     (* fov .5))
         (tan-fov      (tan (radians fov-half)))
         (to-right     (v3:*s (v! 1 0 0) (* tan-fov aspect)))
         (to-top       (v3:*s (v! 0 1 0) tan-fov))
         (top-left     (v3:+ (v3:- (v! 0 0 -1) to-right) to-top))
         (top-right    (v3:+ (v3:+ (v! 0 0 -1) to-right) to-top))
         (bottom-right (v3:- (v3:+ (v! 0 0 -1) to-right) to-top))
         (bottom-left  (v3:- (v3:- (v! 0 0 -1) to-right) to-top)))
    (make-array 4 :initial-contents
                (list (v! top-left)
                      (v! top-right)
                      (v! bottom-right)
                      (v! bottom-left)))))

;;--------------------------------------------------
;; UPDATE
;;--------------------------------------------------
(defgeneric update (camera dt))
(defmethod update ((camera orth) dt)
  (setf (rot camera) (q:look-at (v! 0 0 1) (v! 0 0 0) (v! 0 1 0))
        )
  (setf (frame-size camera) (v2! 10))
  (setf (pos camera) (v! 0 0 10)))

(defmethod update ((camera pers) dt)
  (let ((factor 20))
    ;; running
    (when (keyboard-button (keyboard) key.lshift)
      (setf factor 30))
    ;; crouching
    (when (keyboard-button (keyboard) key.lctrl)
      (setf factor 10))
    ;; forward
    (when (keyboard-button (keyboard) key.w)
      (v3:incf (pos camera)
               (v3:*s (q:to-direction (rot camera))
                      (* factor dt))))
    ;; left
    (when (keyboard-button (keyboard) key.a)
      (v3:incf (pos camera)
               (v3:*s (q:rotate (v! -1 0 0) (rot camera))
                      (* factor dt))))
    ;; right
    (when (keyboard-button (keyboard) key.d)
      (v3:incf (pos camera)
               (v3:*s (q:rotate (v! 1 0 0) (rot camera))
                      (* factor dt))))
    ;; backwards
    (when (keyboard-button (keyboard) key.s)
      (v3:decf (pos camera)
               (v3:*s (q:to-direction (rot camera))
                      (* factor dt)))))

  (when (mouse-button (mouse) mouse.left)
    (let ((move (v2:*s (mouse-move (mouse))
                       0.03)))
      (setf (rot camera)
            (q:normalize
             (q:* (rot camera)
                  (q:normalize
                   (q:* (q:from-axis-angle (v! 1 0 0) (- (y move)))
                        (q:from-axis-angle (v! 0 1 0) (- (x move)))))))))))

(defmethod update ((camera pers) dt)
  ;;(setf (pos camera) (v! -20 90 485))
  ;;(setf (pos camera) (v! 120 30 50))
  ;;(setf (pos camera) (v! -4 -4 0))
  ;;(setf (pos camera) (v! 0 0 10))
  (setf (pos camera) (v! (* 10 (cos (mynow)))
                         (* 10 (cos (mynow)))
                         (* 10 (sin (mynow)))))
  ;;(setf (rot camera) (q:identity))
  ;;(setf (fov camera) 60f0)
  ;; (setf (rot camera)
  ;;       (q:from-axis-angle (v! 1 0 0)
  ;;                          (radians (* 200 (sin (mynow))))))
  (setf (rot camera)
        ;;(q:*)
        ;;(q:identity)
        (q:point-at (v! 0 1 0) (pos camera) (v! 0 0 0))
        ;;(q:from-axis-angle (v! 0 0 1) (radians (* 20 (sin (* .1 (mynow))))))
        )
  )
