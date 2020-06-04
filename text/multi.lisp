(in-package #:incandescent)

;; 2D multiple texts on different textures, same global *FONT*

(defvar *font-fbo* nil
  "the same fbo is used to render text for any instance of TEXT-MULTI")

(defparameter *font-dimensions* '(256 20)
  "Adjust it in base of how long the text would be or leave it big.
   Default: '(512 64)")

(defclass text-multi (actor)
  ((tex     :initarg :tex)
   (sam     :initarg :sam)
   (msg     :initarg :msg     :initform "")
   (scale2  :initarg :scale2  :initform (v! 1 1))
   (offset2 :initarg :offset2 :initform (v! 0 0)))
  (:default-initargs
   :shadow-p nil
   :voxelize-p nil))

(defmethod free ((obj text-multi))
  (free (slot-value obj 'tex)))

(defun write-msg-on-tex (msg tex)
  "helper for initialization or update of texture, uses the global fbo"
  (setf (attachment *font-fbo* 0) (texref tex))
  (cepl.fond:update-fond-text *text* msg)
  (with-fbo-bound (*font-fbo*)
    (clear *font-fbo*)
    (map-g #'fondness (cepl.fond::fond-text-stream  *text*)
           :extent (v! (- (floor (* .5 (first *font-dimensions*)))) ;; left
                       10
                       *font-dimensions*)
           :tex-image (cepl.fond::fond-font-sampler *font*))))

(defmethod initialize-instance :after ((instance text-multi) &key)
  (with-slots (tex sam msg) instance
    (setf tex (make-texture nil :dimensions *font-dimensions*
                                :element-type :r8)
          sam (sample tex :wrap :clamp-to-border
                          :magnify-filter :nearest
                          :minify-filter :nearest))
    (write-msg-on-tex msg tex)))

(defun init-font-fbo ()
  "initialize FBO if uninitialized or dimensions changed."
  (when (or (not *font-fbo*)
            (and (plusp (cepl.fbos::%fbo-attachment-count *font-fbo*))
                 (not (equal *font-dimensions*
                             (dimensions (attachment *font-fbo* 0))))))
    (when *font-fbo* (free *font-fbo*))
    (setf *font-fbo* (make-fbo `(0 :dimensions ,*font-dimensions*
                                   :element-type :r8)))))

(defun make-text-multi (msg)
  (declare (type string msg))
  (init-text)
  (init-font-fbo)
  (let ((obj (make-instance 'text-multi :msg msg)))
    (push obj *actors*)
    obj))

(defmethod update ((actor text-multi) dt)
  ;;#+nil
  (with-slots (tex offset2 seed scale2 color pos) actor
    (setf scale2  (v! (+ seed .5) .1))
    (setf offset2 (v! (mod (+ (x offset2) .001) 1f0)
                      seed))
    (write-msg-on-tex (format nil "~d" (get-internal-real-time))
                      tex)))

(defun-g text-multi-vert ((pos :vec2)
                          &uniform
                          (scale :vec2)
                          (offset :vec2))
  (values (v!   (+ offset (* scale pos)) 0 1)
          (+ .5 (* .5 pos))))

(defun-g text-multi-frag ((uv :vec2) &uniform (sam :sampler-2d))
  (let ((intensity (x (texture sam uv))))
    (v! (v3! intensity)
        1)))

(defpipeline-g text-multi-pipe ()
  :vertex (text-multi-vert :vec2)
  :fragment (text-multi-frag :vec2))

(defmethod draw ((actor text-multi) camera time)
  (with-slots (sam color scale2 offset2) actor
    (draw-tex sam)
    #+nil
    (with-blending *font-blending*
      (with-setf (depth-mask) nil
        (map-g #'text-multi-pipe (get-quad-stream-v2)
               :scale scale2
               :offset offset2
               :sam sam
               ;;:color color
               )))))

