(in-package #:incandescent)

(defclass camera ()
  ((name   :initarg :name  :accessor camera-name)
   (pos    :initarg :pos   :accessor pos)
   (rot    :initarg :rot   :accessor rot)
   (near   :initarg :near  :accessor near)
   (far    :initarg :far   :accessor far)
   (shots  :initarg :shots :accessor shots
           :documentation "vector of shots, which are vectors of frames")
   (n-shot :initarg :n-shot :documentation "number of shots")
   (atime  :initarg :atime  :documentation "current animation time")
   (aspeed :initarg :aspeed :documentation "animation speed")
   (frame-size :initarg :frame-size
               :accessor frame-size))
  (:default-initargs
   :name (gensym)
   :pos (v! 0 0 0)
   :rot (q:identity)
   :near .1
   :far 400f0
   :frame-size nil
   :n-shot nil
   :atime 0f0
   :aspeed 1f0
   :shots (vect (vect))))

(defclass orth (camera) ())
(defclass pers (camera)
  ((fov :initarg :fov :accessor fov))
  (:default-initargs
   :fov 60f0))

(defparameter *camera*
  (make-instance 'pers :far 1000f0))

(defparameter *camera1*
  (make-instance 'orth
                 :rot (q:from-axis-angle (v! 1 0 0)
                                         (radians -90))))

(defparameter *cameras* (list *camera* *camera1*))
(defparameter *currentcamera* *camera*)

(defun distance-to-camera (pos distance)
  (declare (type rtg-math.types:vec3 pos)
           (type number distance))
  (< (v3:length (v3:- pos (pos *currentcamera*)))
     distance))

(defun reset-camera (&key (camera *currentcamera*) (pos (v! 0 0 0)))
  (declare (type rtg-math.types:vec3)
           (type camera camera))
  (setf (pos camera) pos)
  (setf (rot camera) (q:identity)))

(defun next-camera ()
  "rotates current value on *CURRRENTCAMERA*
   for each on *CAMERAS*"
  (setf *cameras* (alexandria:rotate *cameras*))
  (setf *currentcamera* (first-elt *cameras*)))

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

;; Used for Raymarching. But I think it should be useful elsewhere.
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

(defmethod camera-to-camera ((src-camera camera) (dst-camera camera))
  "copies rotation and position from SRC-CAMERA to DST-CAMERA"
  (setf (pos dst-camera) (copy-seq (pos src-camera)))
  (setf (rot dst-camera) (copy-seq (rot src-camera)))
  dst-camera)

(let ((old-pos (v3! 0))
      (old-rot (v3! 0)))
  (defun switcharoo (&optional (camera *currentcamera*))
    (let ((current-pos (copy-seq (pos camera)))
          (current-rot (copy-seq (rot camera))))
      (if (v3:0p old-pos)
          (setf old-pos current-pos
                old-rot current-rot
                (pos camera) (v3! 0))
          (setf (pos camera) old-pos
                (rot camera) old-rot
                old-pos (v3! 0))))))

;;--------------------------------------------------
;; ANIMATION
;;--------------------------------------------------

(defun shot-on (&optional (camera *currentcamera*))
  (with-slots (n-shot atime) camera
    (setf atime 0f0)
    (setf n-shot 0)))

(defun shot-toggle (&optional (camera *currentcamera*))
  "enable/disable animation play"
  (with-slots (n-shot atime) camera
    (setf atime 0f0)
    (if (numberp n-shot)
        (setf n-shot NIL)
        (setf n-shot 0))))

(defun shot-status (&optional (camera *currentcamera*))
  (with-slots (shots n-shot) camera
    (format T "Shot status: ~a~%" n-shot)
    (format T "Shots: ~a~%" shots)))

(defun shot-pop ()
  (with-slots (shots) *currentcamera*
    (vector-pop (aref shots 0))))

(defun shot-add (time &optional (camera *currentcamera*) (shot 0))
  "use this to add the current POS/ROT frame into the SHOT"
  (declare (type unsigned-byte shot))
  (with-slots (pos rot shots) camera
    (let ((element (list (copy-seq pos) (copy-seq rot) time)))
      (vector-push-extend element
                          (aref shots shot)))))

(defun shot-show (frame &optional (camera *currentcamera*))
  "REPL helper to switch camera to frames in shot"
  (when-let* ((shot         (aref (shots camera) 0))
              (has-shots    (not (emptyp shot)))
              (actual-frame (mod frame (fill-pointer shot)))
              (element      (aref shot actual-frame))
              (position     (first element))
              (rotation     (second element)))
    (with-slots (pos rot) camera
      (setf pos position)
      (setf rot rotation))
    frame))

(defun shot-clear (&optional (camera *currentcamera*))
  "REMOVE all shots"
  (with-slots (shots n-shot) camera
    (setf n-shot NIL)
    (setf shots (vect (vect)))))

(defmethod animate ((camera camera))
  "actually animate the thing
   FIXME: DOMO ARIGATO"
  (when-let* ((n-shot   (slot-value camera 'n-shot))
              (shot     (aref (shots camera) n-shot))
              (n-frames (fill-pointer shot))
              (enough-p (>= n-frames 2))
              ;;
              (max-time (- (third
                            (aref shot (1- n-frames)))
                           .01))
              ;;(now      (* (slot-value camera 'aspeed) (mynow))) ;; FIXME
              ;;(time     (mod now max-time))
              (animp (< (slot-value camera 'atime) max-time))
              (time  (min max-time
                          (incf (slot-value camera 'atime)
                                (* (slot-value camera 'aspeed)
                                   .01))))
              ;;
              (ipos     (1- (position-if
                             (lambda (x) (< time (third x)))
                             shot)))
              (index    (if ipos
                            ipos
                            0))
              ;;
              (c-time   (third (aref shot index)))
              (n-time   (third (aref shot (1+ index))))
              (lerp     (/ (-   time c-time)
                           (- n-time c-time))))
    (setf (rot camera)
          (q:lerp (second (aref shot index))
                  (second (aref shot (1+ index)))
                  lerp))
    (setf (pos camera)
          (v3:lerp (first (aref shot index))
                   (first (aref shot (1+ index)))
                   lerp))))

;;--------------------------------------------------
;; UPDATE
;;--------------------------------------------------
(defgeneric update (camera dt))
(defmethod update ((camera orth) dt))
(defmethod update ((camera pers) dt)
  ;; (with-slots (pos) camera
  ;;   ;; no backwards
  ;;   (setf (z pos) (min (z pos) 50f0))
  ;;   (setf (x pos) (alexandria:clamp (x pos) -50f0 50f0)))
  ;;--------------------------------------------------
  ;;(animate camera)
  ;;--------------------------------------------------
  )

