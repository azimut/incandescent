(in-package :incandescent)

;;--------------------------------------------------
;; 2D - Passthrough Texture Pipe

(defun-g pass-frag ((uv :vec2) &uniform (sam :sampler-2d))
  (texture sam uv))

(defpipeline-g pass-pipe (:points)
  :fragment (pass-frag :vec2))

;;--------------------------------------------------
;; 3D - g-pnt mesh without tangents

(defun-g vert ((vert g-pnt)
               &uniform
               (model-world :mat4)
               (world-view :mat4)
               (view-clip :mat4)
               (scale :float)
               (time :float))
  (let* ((pos        (* scale (pos vert)))
         (norm       (norm vert))
         (tex        (tex vert))
         (world-norm (* (m4:to-mat3 model-world) norm))
         (world-pos  (* model-world (v! pos 1)))
         (view-pos   (* world-view  world-pos))
         (clip-pos   (* view-clip   view-pos)))
    (values clip-pos
            tex
            world-norm
            (s~ world-pos :xyz))))

(defun-g frag ((uv :vec2)
               (frag-norm :vec3)
               (frag-pos :vec3)
               &uniform
               (time :float)
               (color :vec3)
               (cam-pos :vec3)
               ;; Directional light (for the most part)
               (light-color :vec3)
               (light-pos   :vec3)
               (brdf-luf :sampler-2d)
               (irradiance-map :sampler-cube)
               (prefilter-map :sampler-cube))
  (let* ((roughness .7)
         (metallic .01)
         (ambient (v! .03 .03 .03))
         (f0 (v3! .04))
         (f0 (mix f0 color metallic))
         (final-color color)
         ;; (final-color (dir-light-apply final-color
         ;;                               light-color
         ;;                               light-pos
         ;;                               frag-pos
         ;;                               frag-norm
         ;;                               cam-pos 32 1
         ;;                               ))
         ;; (ambient (ambient-ibl (normalize (- cam-pos frag-pos))
         ;;                       frag-norm
         ;;                       f0
         ;;                       brdf-luf
         ;;                       prefilter-map
         ;;                       irradiance-map
         ;;                       roughness
         ;;                       metallic
         ;;                       color
         ;;                       1f0))
         (final-color (- 1f0 (nineveh.noise:cellular-noise-fast (* 20 uv))))
         ;; (final-color (* light-color
         ;;                 (pbr-direct-lum light-pos
         ;;                                 frag-pos
         ;;                                 (normalize (- cam-pos frag-pos))
         ;;                                 frag-norm
         ;;                                 roughness
         ;;                                 f0
         ;;                                 metallic
         ;;                                 final-color
         ;;                                 )))
         ;; (final-color
         ;;  (+ final-color
         ;;     (* (v! .2 .2 .9)
         ;;        (pbr-point-lum (v! (* 2 (sin (* .01 time))) 0 0)
         ;;                       frag-pos
         ;;                       (normalize (- cam-pos frag-pos))
         ;;                       frag-norm
         ;;                       roughness
         ;;                       f0
         ;;                       metallic
         ;;                       color
         ;;                       ))))
         ;; (final-color (+ final-color
         ;;                 (point-light-apply color
         ;;                                    (v! .2 .9 .2)
         ;;                                    (v! (* 2 (sin (* .01 time))) 0 0)
         ;;                                    frag-pos
         ;;                                    frag-norm
         ;;                                    1
         ;;                                    .022
         ;;                                    .0019
         ;;                                    ;;cam-pos 2 .9
         ;;                                    )))
         )
    (values (v! (+ ambient final-color) 1)
            ;; (if (> (dot final-color (v! .2126 .7152 .0722)) 1f0)
            ;;     (v! final-color 1)
            ;;     (v! 0 0 0 1))
            )))

(defpipeline-g generic-pipe ()
  :vertex (vert g-pnt)
  :fragment (frag :vec2 :vec3 :vec3))

;;--------------------------------------------------
;; 3D - g-pnt mesh without tangents WITH texture

(defun-g frag-tex ((uv :vec2)
                   (frag-norm :vec3)
                   (frag-pos :vec3)
                   &uniform
                   (light-pos :vec3)
                   (cam-pos :vec3)
                   (albedo :sampler-2d))
  (let* ((light-pos *pointlight-pos*)
         ;; ---------
         (light-color (v! 1 1 1))
         (light-strength 1f0)
         ;;--------------------
         (vec-to-light (- light-pos frag-pos))
         (dir-to-light (normalize vec-to-light))
         ;;--------------------
         (color (expt (s~ (texture albedo (* 20 uv)) :xyz)
                      (vec3 2.2)))
         ;;--------------------
         ;;(normal (normalize frag-norm))
         ;;(nfm    (norm-from-map normap uv))
         )
    (v! color 1)))

(defpipeline-g tex-pipe ()
  :vertex (vert g-pnt)
  :fragment (frag-tex :vec2 :vec3 :vec3))

;;--------------------------------------------------
;; PBR
;;--------------------------------------------------

(defun-g vert-with-tbdata ((vert g-pnt)
                           (tb tb-data)
                           &uniform
                           (model-world :mat4)
                           (world-view :mat4)
                           (view-clip :mat4)
                           (scale :float)
                           ;; Parallax vars
                           (light-pos :vec3)
                           (cam-pos :vec3))
  (let* ((pos       (* scale (pos vert)))
         (norm      (norm vert))
         (uv        (treat-uvs (tex vert)))
         (norm      (* (m4:to-mat3 model-world) norm))
         (world-pos (* model-world (v! pos 1)))
         (view-pos  (* world-view  world-pos))
         (clip-pos  (* view-clip   view-pos))
         (t0 (normalize
              (s~ (* model-world (v! (tb-data-tangent tb) 0))
                  :xyz)))
         (n0 (normalize
              (s~ (* model-world (v! norm 0))
                  :xyz)))
         (t0 (normalize (- t0 (* (dot t0 n0) n0))))
         (b0 (cross n0 t0))
         (tbn (mat3 t0 b0 n0)))
    (values clip-pos
            (treat-uvs uv)
            norm
            (s~ world-pos :xyz)
            tbn
            (* tbn light-pos)
            (* tbn cam-pos)
            (* tbn (s~ world-pos :xyz)))))

(defun-g pbr-frag ((uv :vec2)
                   (frag-norm :vec3)
                   (frag-pos :vec3)
                   (tbn :mat3)
                   (tan-light-pos :vec3)
                   (tan-cam-pos :vec3)
                   (tan-frag-pos :vec3)
                   &uniform
                   (samd :sampler-2d)
                   (uv-repeat :float)
                   (uv-speed :float)
                   (time :float)
                   (color :vec3)
                   ;; Lighting
                   (light-pos :vec3)
                   (cam-pos :vec3)
                   ;; PBR
                   (metallic :float)
                   (albedo :sampler-2d)
                   (ao-map :sampler-2d)
                   (height-map :sampler-2d)
                   (normal-map :sampler-2d)
                   (rough-map :sampler-2d)
                   ;; IBL
                   (brdf-lut :sampler-2d)
                   (prefilter-map :sampler-cube)
                   (irradiance-map :sampler-cube))
  (let* (;; First change UV, then parallax!
         (uv (+ (* uv uv-repeat)
                (v! 0 (* uv-speed time))))
         (uv (parallax-mapping-offset-flipped
              uv
              (normalize (- tan-cam-pos tan-frag-pos))
              height-map
              .03))
         (roughness (+ .5 (x (texture rough-map uv))))
         (ao        (x (texture ao-map uv)))
         (color (* color (expt (s~ (texture albedo uv) :xyz)
                               (vec3 2.2))))
         ;; Normal Mapping
         ;;(normal (normalize frag-norm))
         (normal (norm-from-map normal-map uv))
         (normal (normalize (* tbn normal)))
         ;;----------------------------------------
         ;; PBR
         ;; metallic
         (n normal)
         (v (normalize (- cam-pos frag-pos)))
         (metallic .1)
         ;;(f0 (vec3 .04))
         (f0 color)
         (f0 (mix f0 color metallic))
         (f0 (v3! 0))
         ;; pbr - reflectance equation
         (lo (vec3 0f0))
         ;; lights START
         (lo (+ lo (pbr-direct-lum light-pos
                                   frag-pos
                                   v
                                   n
                                   roughness
                                   f0
                                   metallic
                                   color)))
         ;; (lo (+ lo (pbr-point-lum light-pos
         ;;                          frag-pos
         ;;                          v
         ;;                          n
         ;;                          roughness
         ;;                          f0
         ;;                          metallic
         ;;                          color)))
         ;; ---------- END
         ;;(ambient (* color ao (vec3 .03)))
         ;; (ambient (pbr-ambient-map-r irradiance-map
         ;;                             color
         ;;                             ao n v f0
         ;;                             roughness))
         (r (reflect (- v) n))
         (f (fresnel-schlick-roughness (max (dot n v) 0)
                                       f0
                                       roughness))
         (ks f)
         (kd (* (- 1 ks) (- 1 metallic)))
         (irradiance (s~ (texture irradiance-map n) :xyz))
         (diffuse (* irradiance color))
         (prefiltered-color (s~ (texture-lod prefilter-map
                                             r
                                             (* roughness 4f0))
                                :xyz))
         (env-brdf (texture brdf-lut (v! (max (dot n v) 0) (* roughness 4f0))))
         (specular (* prefiltered-color (+ (* f (x env-brdf)) (y env-brdf))))
         (ambient (* (+ specular (* kd diffuse)) ao))
         (final-color (+ ambient lo)))
    (v! final-color 1)
    ;;(v! uv 0 1)
    ;;(v! color 1)
    ))

(defpipeline-g pbr-pipe ()
  :vertex (vert-with-tbdata g-pnt tb-data)
  :fragment (pbr-frag :vec2 :vec3 :vec3
                      :mat3 :vec3 :vec3 :vec3))

;;--------------------------------------------------

(defun-g pbr-simple-frag ((uv :vec2)
                          (frag-norm :vec3)
                          (frag-pos :vec3)
                          &uniform
                          (light-pos :vec3)
                          (time :float)
                          (roughness :float)
                          (metallic :float)
                          (color :vec3)
                          (cam-pos :vec3)
                          ;; IBL
                          (irradiance-map :sampler-cube)
                          (prefilter-map :sampler-cube)
                          (brdf-lut :sampler-2d))
  (let* (;; First change UV, then parallax!
         (uv (treat-uvs uv))
         (normal (normalize frag-norm))
         (ao 1f0)
         (color color)
         ;;----------------------------------------
         ;; PBR
         (ambient (v3! .03))
         ;; metallic
         (f0 (vec3 .04))
         ;;(f0 color)
         (f0 (mix f0 color metallic))
         ;; pbr - reflectance equation
         (n normal)
         (v  (normalize (- cam-pos frag-pos)))
         (lo (vec3 0f0))
         ;; lights START
         (lo (+ lo (pbr-direct-lum light-pos
                                   frag-pos
                                   v
                                   n
                                   roughness
                                   f0
                                   metallic
                                   color
                                   .01)))
         ;; ---------- END
         ;; (r (reflect (- v) n))
         ;; (f (fresnel-schlick-roughness (max (dot n v) 0)
         ;;                               f0
         ;;                               roughness))
         ;; (ks f)
         ;; (kd (* (- 1 ks) (- 1 metallic)))
         ;; (irradiance (s~ (texture irradiance-map n) :xyz))
         ;; (diffuse (* irradiance color))
         ;; (prefiltered-color (s~ (texture-lod prefilter-map
         ;;                                     r
         ;;                                     (* roughness 4f0))
         ;;                        :xyz))
         ;; (env-brdf (texture brdf-lut (v! (max (dot n v) 0) (* roughness 4f0))))
         ;; (specular (* prefiltered-color (+ (* f (x env-brdf)) (y env-brdf))))
         ;; (ambient (* (+ specular (* kd diffuse)) ao))
         ;; (ambient (ambient-ibl v n f0
         ;;                       brdf-lut
         ;;                       prefilter-map
         ;;                       irradiance-map
         ;;                       .9
         ;;                       .01
         ;;                       (v! .5 .5 .5)
         ;;                       ao))
         ;;(ambient (* color ao (vec3 .3)))
         (final-color (+ ambient lo))
         )
    (v! final-color 1)))

;;----------------------------------------
;; Functions to apply the Irradiance Map ONLY
(defpipeline-g pbr-simple-pipe ()
  :vertex (vert g-pnt)
  :fragment (pbr-simple-frag :vec2 :vec3 :vec3))



