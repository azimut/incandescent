(in-package #:incandescent)

;; 1 Text
;;
;; NOTE: no idea how to hook it nicely into the main
;; program, as putting in *actors* would made it
;; pass trough a postprocessing tonemapping or worst.
;; So I just Add the function screen-text after map-g for post-processing is done.

(defvar *the-text* nil)

(defclass text ()
  ((pos   :initarg :pos   :accessor pos   :documentation "2D screen position")
   (text  :initarg :text  :accessor text)
   (color :initarg :color :accessor color)
   (scale :initarg :scale :accessor scale))
  (:default-initargs
   :scale 1f0
   :pos (v! 0 0)
   :color (v! 1 1 1)))

;;--------------------------------------------------

(defun make-text (string &key (pos (v! 0 0))
                              (scale 1f0)
                              (color (v! 1 1 1)))
  "opinionated constructor, mainly to be used for a single
   text overlay in front of all things"
  (declare (type (or null string) string)
           (type rtg-math.types:vec2 pos)
           (type rtg-math.types:vec3 color))
  (if *the-text*
      (setf (text  *the-text*) string
            (pos   *the-text*) pos
            (color *the-text*) color
            (scale *the-text*) scale)
      (setf *the-text*
            (make-instance 'text :text string
                                 :pos pos
                                 :scale scale
                                 :color color)))
  (cepl.fond:update-fond-text *text* string)
  t)

;;--------------------------------------------------

(defun screen-text ()
  (with-slots (pos color scale) *the-text*
    (with-blending *font-blending*
      (with-setf* ((depth-mask) nil
                   (depth-test-function) nil
                   (cull-face) nil)
        (map-g #'fondness (cepl.fond::fond-text-stream *text*)
               :extent (v! pos *dimensions*)
               :scale scale
               :tex-image (cepl.fond::fond-font-sampler *font*)
               :text-color color)))))
