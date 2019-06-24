(in-package :incandescent)

;; Everything on cepl.fond/cl-fond goes around updating the buffer stream
;; with every new text update.
;; See: (cepl.fond:update-fond-text *text* string)
;;
;; -Might be possible to re-use the same texture but not by default.-
;; See: (setf *text* (cepl.fond:make-fond-text *font* "a text"))

(defvar *font-blending* (make-blending-params :source-alpha :one-minus-src-alpha))

(defvar *font* nil "cepl.fond:fond-font struct
                    - font
                    - cepl sampler")
(defvar *text* nil "cepl.fond:fond-text struct:
                    - varr   (cepl gpu array)
                    - iarr   (cepl gpu array)
                    - stream (cepl buffer stream)
                    - font   (cepl.fond:fond-font - font, cepl sampler)")

(defvar *default-font*  (resolve-path "static/Courier New.ttf"))
(defvar *default-charset*
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ .!?")

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
               :extent     (v! pos 638f0 384f0)
               :tex-image  (cepl.fond::fond-font-sampler *font*)
               :text-color (v! 1 1 1 1))))))

;;--------------------------------------------------

(defstruct-g texticle
  (position     :vec2)
  (in-tex-coord :vec2))

(defun-g calc-text-uvs ((position     :vec2)
                        (in-tex-coord :vec2)
                        (extent       :vec4))
  (vec4 (* 2 (/ (+ (x position) (x extent))
                (- (z extent) 1.0)))
        (* 2 (/ (- (y position) (y extent))
                (+ (w extent) 1.0)))
        0.0
        1.0))

(defun-g tvert ((text-info texticle) &uniform (extent :vec4))
  (with-slots (position in-tex-coord) text-info
    (values (calc-text-uvs position in-tex-coord extent)
            in-tex-coord)))

(defun-g tfrag ((tex-coord :vec2) &uniform
                (tex-image  :sampler-2d)
                (text-color :vec4))
  (let ((intensity (x (texture tex-image tex-coord))))
    ;; NOTE: I could use this for transparency, but i would need another
    ;; pass due defered fog...
    ;;(* (v! 5 5 5 1) (v! 1 1 1 intensity))
    (v! (v3! intensity) 1)
    ))

(defpipeline-g fondness ()
  :vertex   (tvert texticle)
  :fragment (tfrag :vec2))
