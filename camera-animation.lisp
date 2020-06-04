(in-package #:incandescent)

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

(defun shot-add (time &key (pos (pos *currentcamera*))
                           (rot (rot *currentcamera*))
                           (camera *currentcamera*)
                           (shot 0))
  "use this to add the current POS/ROT frame into the SHOT"
  (declare (type unsigned-byte shot))
  (with-slots (shots) camera
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
