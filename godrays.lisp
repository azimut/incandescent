(in-package #:incandescent)

;; INPUT
;; - *light-pos* v3 light-position
;; - sam1 sampler with only the bright light
;; dimensions-v2
(defvar *god-fbo* NIL)
(defvar *god-sam* NIL)

(defun free-god ()
  (when *god-fbo*
    (free *god-fbo*)))

(defun init-god ()
  (free-god)
  (setf *god-fbo*
        (make-fbo `(0 :element-type :rgb16f
                      :dimensions ,*dimensions*)))
  (setf *god-sam* (sample (attachment-tex *god-fbo* 0)
                          :wrap :clamp-to-edge)))

(defun draw-god (sam1 time)
  (with-fbo-bound (*god-fbo*)
    (map-g #'god-rays-pipe *bs*
           :res *dimensions-v2*
           :time time
           :sam sam1
           :sun-pos (screen-coord *dimensions-v2* *light-pos*))))

(defun-g fbm-hash ((p :vec2))
  (fract
   (* (sin (dot p (v! 41 289)))
      "45758.5453")))

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
         (samples   10)
         (decay    .974)
         (exposure .24)
         (density  .93)
         (weight   .36)
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
        (multf illumdecay decay)))
    (v! (+ (v! 0 0 0)
           (* occ exposure))
        1)))

(defpipeline-g god-rays-pipe (:points)
  :fragment (god-rays-frag :vec2))

;; https://stackoverflow.com/questions/3792481/how-to-get-screen-coordinates-from-a-3d-point-opengl
;; You then need to transform the [-1:1]^3 cube to window coordinates
;; by applying the Viewport transformation to it. window.x =
;; viewport.x + viewport.width * (cube.x+1)/2
(defun screen-coord (res &optional (pos (v! 0 0 -10)))
  (let* ((pos4  (v! pos 1))
         (cpos4 (m4:*v (world->view *currentcamera*)
                       pos4))
         (cpos4 (m4:*v (projection *currentcamera*)
                       cpos4))
         (w     (w cpos4))
         (ndc (v4:/s cpos4 w)))
    ;; https://stackoverflow.com/questions/42751427/transformations-from-pixels-to-ndc
    (v2:abs (v2:/ (v! (+ (* (x res) .5 (x ndc))
                         (+ (* (x res) .5 ) 0))
                      (+ (* (y res) .5 (y ndc))
                         (+ (* (y res) .5))))
                  res))))
