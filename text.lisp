(in-package :incandescent)

(defvar *font-blending* (make-blending-params :source-alpha :one-minus-src-alpha))
(defvar *font* nil)
(defvar *text* nil)
(defvar *default-font*
  #p"/home/sendai/Downloads/scpcb-master/GFX/font/courbd/Courier New.ttf")
(defvar *default-charset*
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ .!?")

(defclass text ()
  ((pos  :initarg :pos :documentation "2D screen position"
         :accessor pos)
   (text :initarg :text :accessor text))
  (:default-initargs
   :pos (v! 0 0)))

(defun make-text (&optional (string "hello!") (pos (v! 0 0)))
  (declare (type string string) (type rtg-math.types:vec2 pos))
  (let ((current (find-actor-class 'text)))
    (if current
        (progn
          (setf (text current) string)
          (setf (pos current)  pos)
          (cepl.fond:update-fond-text *text* string)
          current)
        (let ((obj (make-instance 'text :text string :pos pos)))
          (push obj *actors*)
          obj))))

(defun kill-text ()
  (setf *actors*
        (remove (find-actor-class 'text)
                *actors*))
  NIL)

;;--------------------------------------------------

(defun init-text (&key (font *default-font*) (charset *default-charset*))
  (declare (type pathname font) (type string charset))
  (unless *font*
    (setf *font* (cepl.fond:make-fond-font font charset))
    (setf *text* (cepl.fond:make-fond-text *font* "...")))
  nil)

;;--------------------------------------------------

(defmethod update ((actor text) dt)
  (setf (pos actor) (v! -50 120)))

(defmethod draw ((actor text) camera time)
  (with-blending *font-blending*
    (with-setf* (;;(depth-mask) nil
                 ;;(depth-test-function) nil
                 (cull-face) nil
                 ;;(depth-test-function) #'never
                 )
      (with-slots (pos) actor
        (map-g #'fondness (cepl.fond::fond-text-stream *text*)
               :extent (v! pos 638f0 384f0)
               :tex-image (cepl.fond::fond-font-sampler
                           (cepl.fond::fond-text-font *text*))
               :text-color (v! 1 1 1 1))))))

;;--------------------------------------------------

(defstruct-g texticle
  (position :vec2)
  (in-tex-coord :vec2))

(defun-g calc-text-uvs ((position :vec2)
                        (in-tex-coord :vec2)
                        (extent :vec4))
  (vec4 (* 2 (/ (+ (x position) (x extent))
                (- (z extent) 1.0)))
        (* 2 (/ (- (y position) (y extent))
                (+ (w extent) 1.0)))
        0.0
        1.0))

(defun-g tvert ((text-info texticle)
                &uniform
                (extent :vec4))
  (with-slots (position in-tex-coord) text-info
    (values
     (calc-text-uvs position in-tex-coord extent)
     in-tex-coord)))

(defun-g tfrag ((tex-coord :vec2)
                &uniform
                (tex-image :sampler-2d)
                (text-color :vec4))
  (let ((intensity (x (texture tex-image tex-coord))))
    ;; NOTE: I could use this for transparency, but i would need another
    ;; pass due defered fog...
    ;;(* (v! 5 5 5 1) (v! 1 1 1 intensity))
    (v! (v3! intensity) 1)
    ))

(defpipeline-g fondness ()
  (tvert texticle)
  (tfrag :vec2))
