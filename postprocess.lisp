(in-package #:incandescent)

(defvar *fbo*  nil)
(defvar *sam*  nil)
(defvar *sam1* NIL)
(defvar *sam2* NIL)
(defvar *sam3* NIL)
(defvar *samd* nil)
(defparameter *exposure* 1f0)

;;--------------------------------------------------
;; 2D - Post Processing

(defun-g frag-2d ((uv :vec2)
                  &uniform
                  ;; (volume-scatter :sampler-3d)
                  ;; (screen-res :vec2)
                  (sam :sampler-2d)
                  ;;(sam3 :sampler-2d)
                  (sam2 :sampler-2d)
                  (samd :sampler-2d))
  (let* (#+nil
         (final-color (defered-fog
                          (v! .5 .6 .7)
                        ;;(v! 0 0 0)
                        uv sam samd .1))
         ;;(final-color (s~ (posterize (texture sam uv) 32f0) :xyz))
         ;;#+nil
         ;; (final-color (mix (s~ (psx-dither sam
         ;;                                   uv
         ;;                                   255f0
         ;;                                   sam2)
         ;;                       :xyz)
         ;;                   (v! .5 .6 .7)
         ;;                   (defered-fog uv samd .05)))
         ;; (final-color (mix (s~ (texture sam uv) :xyz)
         ;;                   (v! .5 .6 .7)
         ;;                   (defered-fog uv samd .1)))
         ;;(final-color (s~ (psx-dither sam uv 255f0 sam3) :xyz))
         ;; (final-color (* (x (texture sam2 uv))
         ;;                 (s~ (texture sam uv) :xyz)))
         ;;(final-color (posterize final-color 128))
         ;;(final-color (dither-pattern (s~ gl-frag-coord :xy) final-color sam2 .45 .45))
         ;;(final-color (dither-bayer (s~ gl-frag-coord :xy) final-color 0.3))
         ;; godrays
         ;; (final-color (+ (defered-fog-no-skybox (v! .5 .6 .7)
         ;;                   ;;(v! 0 0 0)
         ;;                   uv sam samd .02)
         ;;                 ;;(s~ (texture sam uv) :xyz)
         ;;                 (* (v! .2 .2 .6)-
         ;;                    (s~ (texture sam2 uv) :xyz)))
         ;;              )
         ;; bloom
         ;; (source (texture sam uv))
         ;; (source (v! (defered-fog (v! .02 .09 .12)
         ;;               uv sam samd .7)
         ;;             (w source)))
         ;; (bloom-intensity .1f0)
         ;; (bloom (* bloom-intensity
         ;;           (sample-box uv .5 sam2
         ;;                       #.(/ 1f0 (car *dimensions*))
         ;;                       #.(/ 1f0 (car (last *dimensions*))))))
         (final-color (+ (* (nineveh.vignette:vignette uv)
                            (s~ (texture sam uv) :xyz)
                            ;;(x (texture sam2 uv))
                            )
                         (* .2 (x (texture sam2 uv)))
                         ))
         ;; (final-color (v! (+ (s~ final-color :xyz)
         ;;                     (s~ bloom       :xyz))))
         ;;(color (texture sam uv))
         ;;(color1 (v! (x (texture sam uv)) 0 0))
         ;; (final-color (+ color color2))
         ;; (final-color
         ;;   (* (nineveh.vignette:vignette uv)
         ;;      (s~ (nineveh.anti-aliasing:fxaa3 uv
         ;;                                       sam
         ;;                                       (v2! (/ 1 768f0)))
         ;;          :xyz)))
         ;; ---------------- VOL FOG
         ;; (fog (post-fog
         ;;       ;;#+nil
         ;;       (linear-01-depth (x (texture samd uv))
         ;;                        .1f0 100f0)
         ;;       #+nil
         ;;       (linearize-depth (x (texture samd uv))
         ;;                        .1 100f0)
         ;;       uv
         ;;       volume-scatter
         ;;       screen-res))
         ;; (final-color (+ (s~ fog :xyz)
         ;;                 (* (w fog) final-color)))
         ;; ----------------
         ;;(ldr (tone-map-uncharted2 final-color *exposure* 2f0))
         ;;(ldr (tone-map-reinhard final-color *exposure*))
         (ldr (tonemap-acesfilm (* (linear-to-srgb (* *exposure* final-color)))))
         (luma (rgb->luma-bt601 ldr))
         )
    ;;(v! (pow ldr (vec3 2.2)) 1)
    ;;(v! ldr (* (w fog) luma))
    (v! ldr luma)
    ;;fog
    ;;(v! final-color 1)
    ;;final-color
    ;;(v! final-color (w source)) ;; bloom
    ;;(v! (- 1 (x color)) 0 0 1)
    ;;(v! color1 1)
    ;;(v! ldr 1)
    ;;color
    ))

(defpipeline-g generic-2d-pipe (:points)
  :fragment (frag-2d :vec2))
