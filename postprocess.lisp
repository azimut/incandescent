(in-package #:incandescent)

(defvar *fbo*  nil)
(defvar *sam*  nil)
(defvar *sam1* NIL)
(defvar *samd* nil)
(defparameter *exposure* 1f0)
;;--------------------------------------------------
;; 2D - Post Processing

(defun-g frag-2d ((uv :vec2)
                  &uniform
                  (sam :sampler-2d)
                  (sam2 :sampler-2d)
                  (samd :sampler-2d))
  (let* (;; (color (defered-fog
         ;;            ;;(v! .5 .6 .7)
         ;;            (v! .18 .17 .15)
         ;;            uv
         ;;          sam
         ;;          samd))
         (final-color (+ (s~ (texture sam uv) :xyz)
                         (s~ (texture sam2 uv) :xyz)
                         ))
         ;;         (color2 (s~ (texture sam2 uv) :xyz))
         ;; (color
         ;;  (s~ (nineveh.anti-aliasing:fxaa3 uv sam (v2! (/ 1 320f0))) :xyz))
         ;;         (final-color (+ color color2))
         (ldr (tone-map-reinhard final-color *exposure*))
         (luma (rgb->luma-bt601 ldr))
         )
    ;;(v! (pow ldr (vec3 2.2)) 1)
    (v! ldr luma)
    ;;(v! (- 1 (x color)) 0 0 1)
    ;;(v! color 1)
    ;;(v! ldr 1)
    ))

(defpipeline-g generic-2d-pipe (:points)
  :fragment (frag-2d :vec2))
