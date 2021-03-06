(in-package #:incandescent)

(defclass celestial-sphere (actor)
  ((buf :initform (sphere))))

(defun make-celestial-sphere ()
  (let ((obj (make-instance 'celestial-sphere
                            :name :celestial-sphere)))
    (push obj *actors*)
    obj))

(defmethod draw ((actor celestial-sphere) camera time)
  (with-slots (buf) actor
    (with-setf* ((cull-face) :front
                 (depth-test-function) #'<=
                 (depth-mask) nil)
      (map-g #'celestial-pipe buf
             ;; Rotation without translation
             :view (q:to-mat4
                    (q:inverse (rot camera)))
             :projection (projection  camera)))))

;;--------------------------------------------------
;; glsl-atmosphere
;; https://github.com/wwwtyro/glsl-atmosphere/

;; RSI
;; ray-sphere intersection that assumes
;; the sphere is centered at the origin.
;; No intersection when result.x > result.y
(defun-g rsi ((r0 :vec3)
              (rd :vec3)
              (sr :float))
  (let* ((a (dot rd rd))
         (b (* 2 (dot rd r0)))
         (c (- (dot r0 r0) (* sr sr)))
         (d (- (* b b) (* 4 a c))))
    (if (< d 0)
        (v! 100000 -100000)
        (v! (/ (- (- b) (sqrt d))
               (* 2 a))
            (/ (+ (- b) (sqrt d))
               (* 2 a))))))

;; vec3 r normalized ray direction, typically a ray cast from the observers eye through a pixel
;; vec3 r0 ray origin in meters, typically the position of the viewer's eye
;; vec3 pSun the position of the sun
;; float iSun intensity of the sun
;; float rPlanet radius of the planet in meters
;; float rAtoms radius of the atmosphere in meters
;; vec3 kRlh Rayleigh scattering coefficient
;; vec3 kMie Mie scattering coefficient
;; float shRlh Rayleigh scale height in meters
;; float shMie Mie scale height in meters
;; float g Mie preferred scattering direction
(defun-g atmosphere ((r :vec3)
                     (r0 :vec3)
                     (p-sun :vec3)
                     (i-sun :float)
                     (r-planet :float)
                     (r-atmos :float)
                     (k-rlh :vec3)
                     (k-mie :float)
                     (sh-rlh :float)
                     (sh-mie :float)
                     (g :float)
                     (i-steps :uint)
                     (j-steps :uint))
  (let* (;;(i-steps 3) ;; 16
         ;;(j-steps 2) ;; 8
         (pi +PI+)
         ;; Normalize the sun and view directions
         (p-sun (normalize p-sun))
         (r (normalize r))
         ;; Calculate the step size of the
         ;; primary ray.
         (p (rsi r0 r r-atmos)))
    (if (> (x p) (y p))
        (vec3 0f0)
        (let* ((p (v! (x p)
                      (min (y p) (x (rsi r0 r r-planet)))))
               (i-step-size (/ (- (y p) (x p))
                               i-steps))
               ;; Initialize the primary ray time.
               (i-time 0f0)
               ;; Initialize accumulators for
               ;; Rayleigh and Mie scattering
               (total-rlh (vec3 0f0))
               (total-mie (vec3 0f0))
               ;; Initialize optical depth accum
               ;; for the primary ray.
               (i-od-rlh 0f0)
               (i-od-mie 0f0)
               ;; Calculate the Rayleigh
               ;; and Mie phases
               (mu (dot r p-sun))
               (mumu (* mu mu))
               (gg (* g g))
               (p-rlh (* (/ 3 (* 16 pi))
                         (+ 1 mumu)))
               (p-mie (/ (* (/ 3 (* 8 pi))
                            (* (- 1 gg) (+ 1 mumu)))
                         (* (pow (- (+ 1 gg) (* 2 mu g))
                                 1.5)
                            (* 2 gg))))
               (i 0))
          ;; sample the primary key
          (dotimes (i i-steps)
            (let* (;; Calculate the primary ray sample position
                   (i-pos (+ r0 (* r (+ i-time (* i-step-size .5)))))
                   ;; Calculate the height of the sample
                   (i-height (- (length i-pos) r-planet))
                   ;; Calculate the optical depth of the Rayleigh
                   ;; and Mie scattering for this step.
                   (od-step-rlh (* i-step-size (exp (/ (- i-height) sh-rlh))))
                   (od-step-mie (* i-step-size (exp (/ (- i-height) sh-mie)))))
              ;; Accumulate optical depth.
              (incf i-od-rlh od-step-rlh)
              (incf i-od-mie od-step-mie)
              (let (;; Calculate the step size of the secondary ray
                    (j-step-size (/ (y (rsi i-pos p-sun r-atmos)) j-steps))
                    ;; Initialize the secondary ray time
                    (j-time 0f0)
                    ;; Initialize optical depth accumulators for the sec ray
                    (j-od-rlh 0f0)
                    (j-od-mie 0f0))
                ;; Sample the seconday ray
                (dotimes (j j-steps)
                  (let* (;; Calculate the secondary ray sample position
                         (j-pos (+ i-pos (* p-sun (+ j-time (* j-step-size .5)))))
                         ;; Calculate the height of the sample
                         (j-height (- (length j-pos) r-planet)))
                    ;; Accumulate the optical depth
                    (incf j-od-rlh (* j-step-size (exp (/ (- j-height) sh-rlh))))
                    (incf j-od-mie (* j-step-size (exp (/ (- j-height) sh-mie))))
                    ;; Increment the secondary ray time
                    (incf j-time j-step-size)))
                ;; Calculate attenuation
                (let ((attn (exp (- (+ (* k-mie (+ i-od-mie j-od-mie))
                                       (* k-rlh (+ i-od-rlh j-od-rlh)))))))
                  ;; Accumulate scattering
                  (incf total-rlh (* od-step-rlh attn))
                  (incf total-mie (* od-step-mie attn))
                  ;; Increment the primary ray time
                  (incf i-time i-step-size)))))
          ;; Calculate and return the final color
          (* i-sun (+ (* p-rlh k-rlh total-rlh)
                      (* p-mie k-mie total-mie)))))))

(defun-g celestial-frag ((frag-pos :vec3)
                         &uniform
                         (light-pos :vec2))
  (let* ((offset (v! 2000 1000 1000))
         (color (* (atmosphere (normalize frag-pos)
                               (v! 0 (- 6372000 (x offset)) 0)
                               *light-pos*
                               22      ;; intensity of the sun
                               (- 6371000 (y offset)) ;; radius of the planet
                               (- 6471000 (z offset)) ;; radius of the atmos
                               (v! 5.5e-6 13.0e-6 22.4e-6) ;; rayleight coefficient
                               21e-6   ;; mie coefficient
                               8000    ;; rayleigh scale height
                               1200    ;; mie scale height
                               .758    ;; mie preferred scattering direction
                               3 ;; 16 AND 8
                               2))))
    (values (v! (* *light-color* color) 1)
            (v! (* 6 color) 1))))

(defpipeline-g celestial-pipe ()
  :vertex (cubemap-vert g-pnt)
  :fragment (celestial-frag :vec3))
