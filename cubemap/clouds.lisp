(in-package #:incandescent)

;; Souce Cubemap with clouds
(defvar *t-cubemap* nil)
(defvar *s-cubemap* nil)

(defun free-clouds ()
  (when *t-cubemap*
    (free *t-cubemap*)
    (setf *t-cubemap* NIL)))

(defun init-clouds ()
  (free-clouds)
  (setf *t-cubemap*
        (make-cubemap-tex
         "static/ThickCloudsWater/left.png"
         "static/ThickCloudsWater/right.png"
         "static/ThickCloudsWater/up.png"
         "static/ThickCloudsWater/down.png"
         "static/ThickCloudsWater/front.png"
         "static/ThickCloudsWater/back.png"))
  (setf *s-cubemap*
        (sample *t-cubemap*
                :wrap :clamp-to-edge
                :magnify-filter :linear)))

(defun make-clouds ()
  (unless *t-cubemap*
    (init-clouds))
  (make-env-map *t-cubemap* *s-cubemap*))
