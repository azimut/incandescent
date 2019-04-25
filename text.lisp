(in-package :incandescent)

;; @baggers code from discord
(in-package :%cepl.types)
(defun make-buffer-stream-from-id (vao-gl-object
                                   length
                                   &key
                                     index-type
                                     (start 0)
                                     (primitive :triangles))
  "NOTE: If the vao has an index buffer then length MUST be the length of
         the index, not the number of primatives."
  (assert (and (numberp length) (>= length 0)))
  (assert (and (numberp vao-gl-object) (>= vao-gl-object 0)))
  (assert (or (null index-type)
              (find index-type #(:ushort :uint))))
  (let* ((stream-obj (make-raw-buffer-stream :primitive primitive)))
    (setf (buffer-stream-start stream-obj) start
          (buffer-stream-length stream-obj) length
          (buffer-stream-managed stream-obj) nil
          (buffer-stream-vao stream-obj) vao-gl-object
          (buffer-stream-index-type stream-obj) index-type)
    stream-obj))

(in-package :incandescent)

;; @baggers code from:
;; https://gist.github.com/cbaggers/63929b2897de41c47ef687ec0fccbb81

(defvar *font-blending*
  (make-blending-params :source-alpha :one-minus-src-alpha))
(defvar *font* nil)
(defvar *font-sample* nil)
(defvar *default-font* #p"/home/sendai/Downloads/scpcb-master/GFX/font/courbd/Courier New.ttf")
(defvar *default-charset* "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ .!?")

(defclass text ()
  ((bs   :initarg :bs)
   (pos  :initarg :pos :documentation "2D screen position"
         :accessor pos)
   (text :initarg :text))
  (:default-initargs
   :pos (v! 0 0)))

(defun make-text (&optional (string "hello!") (pos (v! 0 0)))
  (declare (type string string) (type rtg-math.types:vec2 pos))
  (let ((obj (make-instance 'text
                            :text string
                            :pos pos
                            :bs (compute-text-stream string))))
    (push obj *actors*)
    obj))

;;--------------------------------------------------

(defun init-text (&key (font *default-font*) (charset *default-charset*))
  (declare (type pathname font) (type string charset))
  (unless *font*
    (setf *font*        (cl-fond:make-font font charset))
    (setf *font-sample* (sample
                         (make-texture-from-id (cl-fond:texture *font*)
                                               :base-dimensions '(? ?)
                                               :element-type :rgba8))))
  nil)

;;--------------------------------------------------
;; (defun maybe-this (text)
;;   (multiple-value-bind (vao-id count)
;;       (cl-fond:compute-text *font* "Heyo.")
;;     (make-buffer-stream-from-id vao-id count :index-type :uint)))

(defmethod update ((actor text) dt)
  (setf (pos actor) (v! -50 120)))

(defun compute-text-stream (string)
  (multiple-value-bind (vao num-of-elements)
      (cl-fond:compute-text *font* string)
    (%cepl.types::make-buffer-stream-from-id
     vao num-of-elements :index-type :uint)))

(defmethod draw ((actor text) camera time)
  (with-blending *font-blending*
    (with-setf* (;;(depth-mask) nil
                 ;;(depth-test-function) nil
                 (cull-face) nil
                 ;;(depth-test-function) #'never
                 )
      (with-slots (bs pos) actor
        (map-g #'fondness bs
               :extent (v! pos 638f0 384f0)
               :tex-image *font-sample*
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
