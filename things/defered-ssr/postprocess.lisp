(in-package #:incandescent)

(defparameter *exposure* 2f0)

;;--------------------------------------------------
;; 2D - Post Processing
(defun-g frag-2d ((uv :vec2)
                  &uniform
                  (sam :sampler-2d)
                  ;;(sam3 :sampler-2d)
                  (sam2 :sampler-2d)
                  (samd :sampler-2d))
  (let* ((final-color
           (+ (* (nineveh.vignette:vignette uv 15f0 .15)
                 (s~ (texture sam uv) :xyz))))
         ;; ----------------
         ;; Gamma Correction
         ;;(ldr (tone-map-uncharted2 final-color *exposure* 2f0))
         (ldr (tone-map-reinhard final-color *exposure*))
         ;;(ldr (tone-map-hejl-burgess-dawson final-color *exposure*))
         ;;(ldr (tone-map-linear final-color *exposure*))
         ;;(ldr (tone-map-acesfilm final-color *exposure*))
         ;;(ldr (tone-map-filmic final-color *exposure*))
         (luma (rgb->luma-bt601 ldr)))
    (v! ldr luma)))

(defpipeline-g generic-2d-pipe (:points)
  :fragment (frag-2d :vec2))
