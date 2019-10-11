(in-package #:incandescent)
;; TODO: per object location?
;;
;; INPUT
;; - *light-pos* v3 light-position
;; - sam1 rgb16f? sampler with only the bright light
;; dimensions-v2

;; NT:
;; This gets 2 effects
;; 1) an halo around the point given
;; 2) some other rayshafts around objects on screen
;;
;; * won't work with spotlight?
;; * shafts can exist without the first one (i.e. object out of screen),
;;   but can look really fake without it or when there is no object across which cast shafts
;; * TLDR this effect requires tweaks between shots to look good, like adding
;;   a multipler in the post process.
(defvar *god-fbo* NIL)
(defvar *god-sam* NIL)

(defun free-god ()
  (when *god-fbo* (free *god-fbo*)))

(defun init-god ()
  (free-god)
  (setf *god-fbo* (make-fbo `(0 :element-type :rgb16f :dimensions ,*dimensions*))
        *god-sam* (sample (attachment-tex *god-fbo* 0) :wrap :clamp-to-edge)))

(defun draw-god (sam1 time &key (weight .1)
                                (pos *light-pos*))
  (declare (type cepl:sampler sam1)
           (type single-float time))
  (with-fbo-bound (*god-fbo*)
    (with-setf (clear-color) (v! 0 0 0 0)
      (map-g #'god-rays-pipe *bs*
             :res (viewport-resolution (current-viewport))
             :time time
             :weight weight
             :sam sam1
             :sun-pos (screen-coord (viewport-resolution (current-viewport))
                                    pos)))))

(defun-g fbm-hash ((p :vec2))
  (fract
   (* (sin (dot p (v! 41 289)))
      "45758.5453")))

(defun-g god-rays-frag ((uv      :vec2)
                        &uniform
                        (weight  :float)
                        (sun-pos :vec2)
                        (time    :float)
                        (res     :vec2)
                        (sam     :sampler-2d))
  (let* (;;(uv (/ (s~ gl-frag-coord :xy) res))
         ;; (v2:+s (v2:*s (v2:/ (screen-coord (resolution (current-viewport)) (v! 0 0 -30))
         ;;                     (resolution (current-viewport)))
         ;;               2f0)
         ;;   -1f0)
         (uv         uv)
         (samples    30) ; 20
         (decay      .9) ; .9 ;; the ghost outside the halo
         (exposure   .4) ; .4 ;; the halo
         ;;(density    .2)
         ;;(weight     .8) ;.8
         (weight .1)
         (illumdecay .3) ;.1 ;; intensity
         (color      (texture sam uv))
         (occ        (y color))
         (dtc        (* (- uv sun-pos)
                        (/ 1f0 samples)))
         (dither     (fbm-hash (+ uv (fract .2;;time
                                            )))))
    (dotimes (i samples)
      (decf uv dtc)
      (let ((s (x (texture sam (+ uv (* dither dtc))))))
        (multf s (* illumdecay weight))
        (incf  occ s)
        (multf illumdecay decay)))
    (v! (vec3 (* occ exposure))
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
