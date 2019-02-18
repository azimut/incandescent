(in-package #:incandescent)

(defun-g fbm-hash ((p :vec2))
  (fract
   (* (sin (dot p (v! 41 289)))
      45758.5453)))

(defun-g combine-god-frag ((uv :vec2)
                           &uniform
                           (sam-god :sampler-2d)
                           (sam :sampler-2d))
  (let* ((color (mix (s~ (texture sam uv) :xyz)
                     (* *light-color* (s~ (texture sam-god uv) :xyz))
                     .1))
         ;; (color (+ (s~ (texture sam uv) :xyz)
         ;;           (* *light-color* (s~ (texture sam-god uv) :xyz))))
         (ldr (nineveh.tonemapping:tone-map-reinhard color *exposure*))
         (luma (rgb->luma-bt601 ldr)))
    (v! ldr luma)))

(defpipeline-g combine-god-pipe (:points)
  :fragment (combine-god-frag :vec2))


(defun-g god-rays-frag ((uv :vec2)
                        &uniform
                        (res :vec2)
                        (time :float)
                        (sun-pos :vec2)
                        (sam :sampler-2d))
  (let* (;;(uv (/ (s~ gl-frag-coord :xy) res))
         ;; (v2:+s (v2:*s (v2:/ (screen-coord (resolution (current-viewport)) (v! 0 0 -30))
         ;;                     (resolution (current-viewport)))
         ;;               2f0)
         ;;   -1f0)
         (uv uv)         
         (samples 10f0)
         (decay .974)
         (exposure .24)
         (density .93)
         (weight .36)
         (color (s~ (texture sam uv) :xyz))
         (occ (x color))
         (obj (y color))
         (dtc (* (- uv sun-pos)
                 (/ 1f0 samples)))
         (illumdecay .4f0)
         (dither (fbm-hash (+ uv (fract time)))))
    (dotimes (i samples)
      (decf uv dtc)
      (let ((s (x (texture sam (+ (* dither dtc)
                                  uv)))))
        (multf s (* illumdecay weight))
        (incf occ s)
        (multf illumdecay decay)
        ))
    (v! (+ (v! 0 0 0)
           (* occ exposure))
        1)
    ;;(vec4 occ)
    ))

;; https://stackoverflow.com/questions/3792481/how-to-get-screen-coordinates-from-a-3d-point-opengl
;; You then need to transform the [-1:1]^3 cube to window coordinates
;; by applying the Viewport transformation to it. window.x =
;; viewport.x + viewport.width * (cube.x+1)/2

(defun screen-coord (res &optional (pos (v! 0 0 -10)))
  (let* ((pos4 (v! pos 1))
         (cpos4 (m4:*v (world->view *currentcamera*)
                       pos4))
         (cpos4 (m4:*v (projection *currentcamera*)
                       cpos4))
         (w (w cpos4))
         (ndc (v4:/s cpos4 w)))
    ;; https://stackoverflow.com/questions/42751427/transformations-from-pixels-to-ndc
    (v2:abs (v2:/ (v! (+ (* (x res) .5 (x ndc))
                         (+ (* (x res) .5 ) 0))
                      (+ (* (y res) .5 (y ndc))
                         (+ (* (y res) .5))))
                  res))
    
    ;; https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/glViewport.xml
    ;; (v! (+ (* (+ 1 (x ndc)) (/ (x res) 2)))
    ;;     (+ (* (+ 1 (y ndc)) (/ (y res) 2))))

    ;; screen.x = ((view.w * 0.5) * ndc.x) +
    ;;            ((w * 0.5) + view.x)
    ;; screen.y = ((view.h * 0.5) * ndc.y) +
    ;;            ((h * 0.5) + view.y)
    ;;
    ;; (v2:abs (v2:/ (v! (+ .5 (* (x res)
    ;;                            (/ (+ 1 (x ndc)) 2)))
    ;;                   (+ .5 (* (y res)
    ;;                            (/ (- 1 (y ndc)) 2))))
    ;;               res))
    ;; (v2:* (v2:+ (v2:/s (s~ cpos4 :xy) w)
    ;;             (v! 1 1))
    ;;       (v2:*s res .5))    
    ))
