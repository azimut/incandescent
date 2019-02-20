(in-package #:incandescent)

;;--------------------------------------------------
;; PBR - BRDF
;; https://learnopengl.com/PBR/Lighting

(defun-g fresnel-schlick ((cos-theta :float)
                          (f0 :vec3))
  (+ f0
     (* (- 1 f0)
        (pow (- 1 cos-theta) 5))))

(defun-g distribution-ggx ((n :vec3)
                           (h :vec3)
                           (roughness :float))
  (let* ((a  (* roughness roughness))
         (a2 (* a a))
         (n-dot-h  (max (dot n h) 0))
         (n-dot-h2 (* n-dot-h n-dot-h))
         (num a2)
         (denom (1+ (* n-dot-h2 (1- a2))))
         (denom (* +PI+ denom denom)))
    (/ num denom)))

(defun-g geometry-schlick-ggx ((n-dot-v :float)
                               (roughness :float))
  (let* ((r (1+ roughness))
         (k (/ (* r r) 8))
         (num n-dot-v)
         (denom (+ (* n-dot-v (- 1 k))
                   k)))
    (/ num denom)))

(defun-g geometry-smith ((n :vec3)
                         (v :vec3)
                         (l :vec3)
                         (roughness :float))
  (let* ((n-dot-v (max (dot n v) 0))
         (n-dot-l (max (dot n l) 0))
         (ggx2 (geometry-schlick-ggx n-dot-v roughness))
         (ggx1 (geometry-schlick-ggx n-dot-l roughness)))
    (* ggx1 ggx2)))

;;--------------------------------------------------
;; PBR helpers to apply light - WITH IBL
;;--------------------------------------------------

(defun-g pbr-ambient-map ((irradiance-map :sampler-cube)
                          (albedo :vec3)
                          (ao :float)
                          (v :vec3)
                          (n :vec3)
                          (f0 :vec3))
  (let* ((ks (fresnel-schlick (max (dot n v) 0) f0))
         (kd (- 1 ks))
         (irradiance (s~ (texture irradiance-map n)
                         :xyz))
         (diffuse (* irradiance albedo)))
    (* diffuse kd ao)))

(defun-g fresnel-schlick-roughness ((cos-theta :float)
                                    (f0 :vec3)
                                    (roughness :float))
  (+ f0
     (* (- (max (vec3 (- 1 roughness))
                f0)
           f0)
        (pow (- 1 cos-theta) 5f0))))

(defun-g pbr-ambient-map-r ((irradiance-map :sampler-cube)
                            (albedo :vec3)
                            (ao :float)
                            (v :vec3)
                            (n :vec3)
                            (f0 :vec3)
                            (roughness :float))
  (let* ((ks (fresnel-schlick-roughness (max (dot n v) 0)
                                        f0
                                        roughness))
         (kd (- 1 ks))
         (irradiance (s~ (texture irradiance-map n) :xyz))
         (diffuse (* irradiance albedo)))
    (* diffuse kd ao)))


;; AMBIENT no SPECULAR
(defun-g ambient-ibl ((v :vec3)
                      (n :vec3)
                      (f0 :vec3)
                      (prefilter-map  :sampler-cube)
                      (irradiance-map :sampler-cube)
                      (roughness :float)
                      (metallic  :float)
                      (color :vec3)
                      (ao :float))
  (let* ((r (reflect (- v) n))
         (f (fresnel-schlick-roughness (max (dot n v) 0) f0
                                       roughness))
         (ks f)
         (kd (* (- 1 ks) (- 1 metallic)))
         (irradiance (s~ (texture irradiance-map n) :xyz))
         (diffuse (* irradiance color))
         (prefiltered-color
          (s~ (texture-lod prefilter-map r (* roughness 4f0))
              :xyz))
         (ambient (* kd diffuse ao)))
    ambient))

;; AMBIENT+SPECULAR
(defun-g ambient-ibl ((v :vec3)
                      (n :vec3)
                      (f0 :vec3)
                      (brdf-lut :sampler-2d)
                      (prefilter-map  :sampler-cube)
                      (irradiance-map :sampler-cube)
                      (roughness :float)
                      (metallic  :float)
                      (color :vec3)
                      (ao :float))
  (let* ((r (reflect (- v) n))
         (f (fresnel-schlick-roughness (max (dot n v) 0)
                                       f0
                                       roughness))
         (ks f)
         (kd (* (- 1 ks) (- 1 metallic)))
         (irradiance (s~ (texture irradiance-map n) :xyz))
         (diffuse    (* irradiance color))
         (prefiltered-color
          (s~ (texture-lod prefilter-map r (* roughness 4f0))
              :xyz))
         (env-brdf
          (texture brdf-lut (v! (max (dot n v) 0) (* roughness 4f0))))
         (specular
          (* prefiltered-color (+ (* f (x env-brdf)) (y env-brdf))))
         (ambient (* (+ specular (* kd diffuse)) ao)))
    ambient))

;;--------------------------------------------------
;; PBR helpers to apply lights - NO IBL
;;--------------------------------------------------

(defun-g pbr-direct-lum ((light-pos :vec3)
                         (frag-pos :vec3)
                         (v :vec3)
                         (n :vec3)
                         (roughness :float)
                         (f0 :vec3)
                         (metallic :float)
                         (albedo :vec3))
  (let* ((l (normalize (- light-pos frag-pos)))
         (h (normalize (+ v l)))
         (distance (length (- light-pos frag-pos)))
         (radiance (v! 5 5 5))
         ;; pbr - cook-torrance brdf
         (ndf (distribution-ggx n h roughness))
         (g   (geometry-smith n v l roughness))
         (f   (fresnel-schlick (max (dot h v) 0) f0))
         ;;
         (ks f)
         (kd (- 1 ks))
         (kd (* kd (- 1 metallic)))
         ;;
         (numerator   (* ndf g f))
         (denominator (+ .001
                         (* (max (dot n v) 0)
                            (max (dot n l) 0)
                            4)))
         (specular (/ numerator denominator))
         ;; add to outgoing radiance lo
         (n-dot-l (max (dot n l) 0))
         (lo (* (+ specular (/ (* kd albedo) +PI+))
                radiance
                n-dot-l)))
    lo))

(defun-g pbr-direct-lum ((light-pos :vec3)
                         (frag-pos :vec3)
                         (v :vec3)
                         (n :vec3)
                         (roughness :float)
                         (f0 :vec3)
                         (metallic :float)
                         (albedo :vec3)
                         (specular-strength :float))
  (let* ((l (normalize (- light-pos frag-pos)))
         (h (normalize (+ v l)))
         (distance (length (- light-pos frag-pos)))
         (radiance (v! 5 5 5))
         ;; pbr - cook-torrance brdf
         (ndf (distribution-ggx n h roughness))
         (g   (geometry-smith n v l roughness))
         (f   (fresnel-schlick (max (dot h v) 0) f0))
         ;;
         (ks f)
         (kd (- 1 ks))
         (kd (* kd (- 1 metallic)))
         ;;
         (numerator   (* ndf g f))
         (denominator (+ .001
                         (* (max (dot n v) 0)
                            (max (dot n l) 0)
                            4)))
         (specular (* specular-strength
                      (/ numerator denominator)))
         ;; add to outgoing radiance lo
         (n-dot-l (max (dot n l) 0))
         (lo (* (+ specular (/ (* kd albedo) +PI+))
                radiance
                n-dot-l)))
    lo))

(defun-g pbr-point-lum ((light-pos :vec3)
                        (frag-pos :vec3)
                        (v :vec3)
                        (n :vec3)
                        (roughness :float)
                        (f0 :vec3)
                        (metallic :float)
                        (albedo :vec3))
  (let* ((l (normalize (- light-pos frag-pos)))
         (h (normalize (+ v l)))
         (distance (length (- light-pos frag-pos)))
         (constant 1f0)
         (linear .7)
         (quadratic 1.8)
         (attenuation (/ 1f0 (+ constant
                                (* linear distance)
                                (* quadratic distance distance))))
         (light-color (v! .9 .9 .9))
         (radiance light-color attenuation)
         ;; pbr - cook-torrance brdf
         (ndf (distribution-ggx n h roughness))
         (g (geometry-smith n v l roughness))
         (f (fresnel-schlick (max (dot h v) 0) f0))
         ;;
         (ks f)
         (kd (* (- (vec3 1) ks)
                (- 1 metallic)))
         ;;
         (numerator (* ndf g f))
         (denominator (* (max (dot n v) 0)
                         (max (dot n l) 0)
                         4))
         (specular (/ numerator (max denominator .001)))
         ;; add to outgoing radiance lo
         (n-dot-l (max (dot n l) 0)))
    (* (+ specular (/ (* kd albedo) +PI+))
       radiance
       n-dot-l)))
