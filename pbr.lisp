(in-package #:incandescent)

;; https://darmstadt-graphics.com/doc/cli/05-working-with-PBR-Materials/index.html
;;                   BaseColor       Metallic 	Roughness
;; Blue Metal 	     0.1, 0.1, 1.0   1.0 	0.1
;; White Half- Metal 0.9, 0.9, 0.9   0.5 	0.25
;; White Porcealain  0.9, 0.9, 0.9   0.0 	0.15
;; Blue Rubber       0.1, 0.1, 1.0   0.0 	0.7

;;--------------------------------------------------
;; PBR - BRDF
;; https://learnopengl.com/PBR/Lighting

(defun-g fresnel-schlick ((cos-theta :float)
                          (f0 :vec3))
  (+ f0
     (* (- 1f0 f0)
        (pow (- 1f0 cos-theta) 5f0))))

(defun-g distribution-ggx ((n :vec3)
                           (h :vec3)
                           (roughness :float))
  (let* ((a  (* roughness roughness))
         (a2 (* a a))
         (n-dot-h  (max (dot n h) 0f0))
         (n-dot-h2 (* n-dot-h n-dot-h))
         (num   a2)
         (denom (1+ (* n-dot-h2 (1- a2))))
         (denom (* +PI+ denom denom)))
    (/ num denom)))

(defun-g geometry-schlick-ggx ((n-dot-v :float)
                               (roughness :float))
  (let* ((r (1+ roughness))
         (k (/ (* r r) 8f0))
         (num n-dot-v)
         (denom (+ (* n-dot-v (- 1f0 k))
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

(defun-g fresnel-schlick-roughness ((cos-theta :float)
                                    (f0        :vec3)
                                    (roughness :float))
  (+ f0
     (* (- (max (vec3 (- 1 roughness))
                f0)
           f0)
        (pow (- 1 cos-theta) 5f0))))

;; AMBIENT no SPECULAR
(defun-g ambient-ibl ((v              :vec3)
                      (n              :vec3)
                      (irradiance-map :sampler-cube)
                      (roughness      :float)
                      (metallic       :float)
                      (color          :vec3)
                      (ao             :float))
  (let* ((f0 (vec3 0.04))
         (f0 (mix f0 color metallic))
         (ks (fresnel-schlick-roughness (max (dot n v) 0) f0 roughness))
         (kd (- 1 ks))
         (irradiance (s~ (texture irradiance-map n) :xyz))
         (diffuse (* irradiance color))
         (ambient (* kd diffuse ao)))
    ambient))

;; AMBIENT + SPECULAR
(defun-g ambient-ibl ((v              :vec3)
                      (n              :vec3)
                      (brdf-lut       :sampler-2d)
                      (prefilter-map  :sampler-cube)
                      (irradiance-map :sampler-cube)
                      (roughness      :float)
                      (metallic       :float)
                      (color          :vec3)
                      (ao             :float))
  (let* (;; f - f0
         (f0 (vec3 0.04))
         (f0 (mix f0 color metallic))
         (f  (fresnel-schlick-roughness (max (dot n v) 0) f0 roughness))
         ;; Diffuse
         (ks f)
         (kd (* (- 1 ks)
                (- 1 metallic)))
         (irradiance (s~ (texture irradiance-map n) :xyz))
         (diffuse    (* irradiance color))
         ;; Specular
         (r (reflect (- v) n))
         (prefiltered-color
           (s~ (texture-lod prefilter-map r (* roughness 4f0))
               :xyz))
         (env-brdf (s~ (texture brdf-lut (v! (max (dot n v) 0) roughness)) :xy))
         (specular (* prefiltered-color
                      (+ (* f (x env-brdf))
                         (y env-brdf))))
         ;; AO just diffuse...
         (ambient (+ specular (* kd diffuse ao))))
    ambient))

;;--------------------------------------------------
;; PBR helpers to apply lights - NO IBL
;;--------------------------------------------------

(defun-g pbr-direct-lum ((light-pos         :vec3)
                         (frag-pos          :vec3)
                         (v                 :vec3)
                         (n                 :vec3)
                         (roughness         :float)
                         (metallic          :float)
                         (albedo            :vec3)
                         (specular-strength :float)
                         (light-color       :vec3))
  (let* ((f0 (vec3 0.04))
         (f0 (mix f0 albedo metallic))
         (l  (normalize (- light-pos frag-pos)))
         (h  (normalize (+ v l)))
         ;;
         (distance (length (- light-pos frag-pos)))
         (radiance light-color)
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
         (specular    (* specular-strength
                         (/ numerator denominator)))
         ;; add to outgoing radiance lo
         (n-dot-l (max (dot n l) 0))
         (lo (* (+ specular (/ (* kd albedo) +PI+))
                radiance
                n-dot-l)))
    lo))

(defun-g pbr-direct-lum ((light-pos         :vec3)
                         (frag-pos          :vec3)
                         (v                 :vec3)
                         (n                 :vec3)
                         (roughness         :float)
                         (metallic          :float)
                         (albedo            :vec3)
                         (specular-strength :float))
  (pbr-direct-lum light-pos frag-pos v n roughness metallic albedo
                  specular-strength
                  (v! 5 5 5)))

(defun-g pbr-direct-lum ((light-pos :vec3)
                         (frag-pos  :vec3)
                         (v         :vec3)
                         (n         :vec3)
                         (roughness :float)
                         (metallic  :float)
                         (albedo    :vec3))
  (pbr-direct-lum light-pos frag-pos v n roughness metallic albedo
                  .1))

;;--------------------------------------------------
;; POINT
;;--------------------------------------------------

(defun-g pbr-point-lum ((light-pos         :vec3)
                        (frag-pos          :vec3)
                        (v                 :vec3)
                        (n                 :vec3)
                        (roughness         :float)
                        (metallic          :float)
                        (albedo            :vec3)
                        (specular-strength :float)
                        (linear            :float)
                        (quadratic         :float)
                        (light-color       :vec3))
  (let* ((f0 (vec3 0.04))
         (f0 (mix f0 albedo metallic))
         (l         (normalize (- light-pos frag-pos)))
         (h         (normalize (+ v l)))
         (distance  (length    (- light-pos frag-pos)))
         (constant  1f0)
         (attenuation (/ 1f0 (+ constant
                                (* linear distance)
                                (* quadratic distance))))
         (radiance (* light-color attenuation))
         ;; pbr - cook-torrance brdf
         (ndf (distribution-ggx n h roughness))
         (g   (geometry-smith n v l roughness))
         (f   (fresnel-schlick (max (dot h v) 0) f0))
         ;;
         (ks  f)
         (kd  (- 1 ks))
         (kd  (* kd (- 1 metallic)))
         ;;
         (numerator   (* ndf g f))
         (denominator (* (max (dot n v) 0)
                         (max (dot n l) 0)
                         4))
         (specular    (* specular-strength
                         (/ numerator (max denominator .001))))
         ;; add to outgoing radiance lo
         (n-dot-l (max (dot n l) 0))
         (lo      (* (+ specular (/ (* kd albedo) +PI+))
                     radiance
                     n-dot-l)))
    (* attenuation lo)))

(defun-g pbr-point-lum ((light-pos         :vec3)
                        (frag-pos          :vec3)
                        (v                 :vec3)
                        (n                 :vec3)
                        (roughness         :float)
                        (metallic          :float)
                        (albedo            :vec3)
                        (specular-strength :float)
                        (linear            :float)
                        (quadratic         :float))
  (pbr-point-lum light-pos frag-pos v n roughness metallic albedo
                 specular-strength linear quadratic
                 (v! 5 5 5)))

(defun-g pbr-point-lum ((light-pos         :vec3)
                        (frag-pos          :vec3)
                        (v                 :vec3)
                        (n                 :vec3)
                        (roughness         :float)
                        (metallic          :float)
                        (albedo            :vec3)
                        (specular-strength :float))
  (pbr-point-lum light-pos frag-pos v n roughness metallic albedo
                 specular-strength
                 .22 .20))

(defun-g pbr-point-lum ((light-pos         :vec3)
                        (frag-pos          :vec3)
                        (v                 :vec3)
                        (n                 :vec3)
                        (roughness         :float)
                        (metallic          :float)
                        (albedo            :vec3))
  (pbr-point-lum light-pos frag-pos v n roughness metallic albedo
                 .1))

;;--------------------------------------------------
;; SPOTLIGHT
;;--------------------------------------------------

(defun-g pbr-spotlight-lum ((light-pos         :vec3)
                            (frag-pos          :vec3)
                            (v                 :vec3)
                            (n                 :vec3)
                            (roughness         :float)
                            (metallic          :float)
                            (albedo            :vec3)
                            (specular-strength :float)
                            (light-color       :vec3)
                            (light-dir         :vec3)
                            (cutoff            :float)
                            (outer-cutoff      :float)
                            (linear            :float)
                            (quadratic         :float))
  (let* ((f0 (vec3 0.04))
         (f0 (mix f0 albedo metallic))
         (distance (length (- light-pos frag-pos)))
         ;;
         (constant 1f0)
         ;;#+nil
         (attenuation (/ 1f0
                         (+ constant
                            (* linear distance)
                            (* quadratic distance distance))))
         ;;(attenuation (/ 1f0 (* distance distance)))
         (light-color (* light-color attenuation)) ;? took from learnopengl pbr code
         ;;
         (cut-off       (cos (radians cutoff)))
         (outer-cut-off (cos (radians outer-cutoff)))
         ;;
         (l         (normalize (- light-pos frag-pos)))
         (theta     (dot l (normalize (- light-dir))))
         (epsilon   (- cut-off outer-cut-off))
         (intensity (clamp (/ (- theta outer-cut-off) epsilon) 0f0 1f0))
         ;;
         (radiance (* light-color intensity))
         ;; pbr - cook-torrance brdf
         (h   (normalize (+ v l)))
         (ndf (distribution-ggx n h roughness))
         (g   (geometry-smith n v l roughness))
         (f   (fresnel-schlick (max (dot h v) 0f0) f0))
         ;;
         (numerator   (* ndf g f))
         (denominator (* (max (dot n v) 0f0)
                         (max (dot n l) 0f0)
                         4f0))
         (specular    (* specular-strength
                         (/ numerator (max denominator .001))))
         ;;
         (ks f) ; is = fresnel
         ;; for energy conservation, the diffuse and specular light can't
         ;; be above 1.0 (unless the surface emits light); to preserve this
         ;; relationship the diffuse component (kD) should equal 1.0 - kS.
         (kd (- 1f0 ks))
         ;; multiply kD by the inverse metalness such that only non-metals
         ;; have diffuse lighting, or a linear blend if partly metal (pure metals
         ;; have no diffuse light).
         (kd (* kd (- 1f0 metallic)))
         ;; scale light by NdotL
         (n-dot-l (max (dot n l) 0f0))
         ;; add to outgoing radiance lo
         (lo      (* (+ specular (/ (* kd albedo) +PI+))
                     radiance
                     n-dot-l)))
    lo))

(defun-g pbr-spotlight-lum ((light-pos         :vec3)
                            (frag-pos          :vec3)
                            (v                 :vec3)
                            (n                 :vec3)
                            (roughness         :float)
                            (metallic          :float)
                            (albedo            :vec3)
                            (specular-strength :float)
                            (light-color       :vec3)
                            (light-dir         :vec3)
                            (cutoff            :float)
                            (outer-cutoff      :float))
  (pbr-spotlight-lum light-pos frag-pos v n
                     roughness metallic albedo
                     specular-strength light-color
                     light-dir
                     cutoff
                     outer-cutoff
                     .027 .0028))

(defun-g pbr-spotlight-lum ((light-pos         :vec3)
                            (frag-pos          :vec3)
                            (v                 :vec3)
                            (n                 :vec3)
                            (roughness         :float)
                            (metallic          :float)
                            (albedo            :vec3)
                            (specular-strength :float)
                            (light-color       :vec3)
                            (light-dir         :vec3))
  (pbr-spotlight-lum light-pos frag-pos v n
                     roughness metallic albedo
                     specular-strength light-color
                     light-dir
                     4.5 7.5))

(defun-g pbr-spotlight-lum ((light-pos         :vec3)
                            (frag-pos          :vec3)
                            (v                 :vec3)
                            (n                 :vec3)
                            (roughness         :float)
                            (metallic          :float)
                            (albedo            :vec3)
                            (specular-strength :float)
                            (light-color       :vec3))
  (pbr-spotlight-lum light-pos frag-pos v n
                     roughness metallic albedo
                     specular-strength light-color
                     (v! 0 -1 0)))

(defun-g pbr-spotlight-lum ((light-pos         :vec3)
                            (frag-pos          :vec3)
                            (v                 :vec3)
                            (n                 :vec3)
                            (roughness         :float)
                            (metallic          :float)
                            (albedo            :vec3)
                            (specular-strength :float))
  (pbr-spotlight-lum light-pos frag-pos v n
                     roughness metallic albedo
                     specular-strength
                     (v! 5 5 5)))

