(in-package #:incandescent)

(defvar *font-fbo* nil)
(defparameter *font-dimensions* '(256 20)
  "Default: '(512 64)")

(defclass text-billboard (actor)
  ((tex :initarg :tex)
   (sam :initarg :sam)
   (msg :initarg :msg :initform "")))

(defun init-font-fbo ()
  (when (or (not *font-fbo*)
            (and (plusp (cepl.fbos::%fbo-attachment-count *font-fbo*))
                 (not (equal *font-dimensions*
                             (dimensions (attachment *font-fbo* 0))))))
    (free *font-fbo*)
    (setf *font-fbo* (make-fbo `(0 :dimensions ,*font-dimensions*
                                   :element-type :r8)))))

(defmethod free ((obj text-billboard))
  (with-slots (tex) obj
    (free tex))
  t)

(defun write-msg-on-tex (msg tex)
  (setf (attachment *font-fbo* 0) (texref tex))
  (cepl.fond:update-fond-text *text* msg)
  (with-fbo-bound (*font-fbo*)
    (clear *font-fbo*)
    (map-g #'fondness (cepl.fond::fond-text-stream  *text*)
           :extent (v! (- (floor (* .5 (first *font-dimensions*)))) ;; left
                       10
                       *font-dimensions*)
           :tex-image (cepl.fond::fond-font-sampler *font*))))

(defmethod initialize-instance :after ((instance text-billboard) &key)
  (init-text)
  (init-font-fbo)
  (with-slots (tex sam msg) instance
    (setf tex (make-texture nil :dimensions *font-dimensions* :element-type :r8)
          sam (sample tex :wrap :clamp-to-border))
    (write-msg-on-tex msg tex)))

(defun make-text-billboard (msg)
  (declare (type string msg))
  (let ((obj (make-instance 'text-billboard :msg msg)))
    (push obj *actors*)
    obj))

(defmethod update ((actor text-billboard) dt)
  (with-slots (tex scale color pos) actor
    ;;(setf scale .5)
    ;;(setf color (v! .1 2 .1))
    (write-msg-on-tex (format nil "(~f ~f ~f)" (x pos) (y pos) (z pos))
                      tex)))

(defmethod draw ((actor text-billboard) camera time)
  (with-slots (sam color scale) actor
    ;;(draw-tex sam)
    (with-blending *font-blending*
      (with-setf (depth-mask) nil
        (map-g #'text-billboard-pipe (get-quad-stream-v3)
               :scale 5f0
               :sam sam
               :color color
               :model-world (model->world actor)
               :world-view  (world->view  camera)
               :view-clip   (projection   camera))))))

