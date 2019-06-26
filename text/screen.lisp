(in-package :incandescent)

(defclass text ()
  ((pos  :initarg :pos  :accessor pos :documentation "2D screen position")
   (text :initarg :text :accessor text))
  (:default-initargs
   :pos (v! 0 0)))

;;--------------------------------------------------

(defun init-text (&key (charset *default-charset*)
                       (font    *default-font*))
  (declare (type string   charset)
           (type pathname font))
  (unless *font*
    (setf *font* (cepl.fond:make-fond-font font charset))
    (setf *text* (cepl.fond:make-fond-text *font* "...")))
  t)

;;--------------------------------------------------

(defun kill-text ()
  (alexandria:removef *actors* (find-actor-class 'text))
  t)

(defun make-text (&optional (string "hello!")
                            (pos (v! 0 0)))
  "opinionated constructor, mainly to be used for a single
   text overlay in front of all things"
  (declare (type (or null string) string)
           (type rtg-math.types:vec2 pos))
  (if (null string)
      (kill-text)
      (let ((current (find-actor-class 'text)))
        (if current
            (progn
              (setf (text current) string)
              (setf (pos current)  pos)
              (cepl.fond:update-fond-text *text* string)
              current)
            (let ((obj (make-instance 'text :text string :pos pos)))
              (cepl.fond:update-fond-text *text* string)
              (push obj *actors*)
              obj)))))

;;--------------------------------------------------

(defmethod update ((actor text) dt))

(defmethod draw ((actor text) camera time)
  (with-slots (pos) actor
    (with-blending *font-blending*
      (with-setf* (;;(depth-mask) nil
                   ;;(depth-test-function) nil
                   (cull-face) nil
                   ;;(depth-test-function) #'never
                   )
        (map-g #'fondness  (cepl.fond::fond-text-stream *text*)
               :extent     (v! pos *dimensions*)
               :tex-image  (cepl.fond::fond-font-sampler *font*)
               :text-color (v! 1 1 1 1))))))
