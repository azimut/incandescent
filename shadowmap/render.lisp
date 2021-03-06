(in-package #:incandescent)

;;
;; Forward rendering ONLY, for defer rendering calculate the
;; light-clip-pos in the postprocess pipe
;;

;;--------------------------------------------------
;; 3d - g-pnt - Replacements for basic vert and frags

(defun-g shadow-vert ((vert g-pnt) &uniform
                      (model-world :mat4) (world-view :mat4) (view-clip :mat4)
                      (light-world :mat4) (light-clip :mat4)
                      (scale       :float))
  (let* ((pos        (* scale (pos vert)))
         (norm       (norm vert))
         (world-norm (* (m4:to-mat3 model-world) norm))
         (world-pos  (* model-world (v! pos 1)))
         (view-pos   (* world-view  world-pos))
         (clip-pos   (* view-clip   view-pos)))
    (values clip-pos
            (tex vert)
            world-norm
            (s~ world-pos :xyz)
            (* light-clip light-world world-pos))))

(defun-g shadow-frag ((uv :vec2) (frag-norm :vec3) (frag-pos :vec3)
                      (light-clip-pos :vec4) &uniform
                      ;; Shadowmap
                      (shadowmap :sampler-2d)
                      (color :vec3)
                      (cam-pos :vec3)
                      ;; Directional light (for the most part)
                      (light-color :vec3)
                      (light-pos   :vec3))
  (let* ((final-color color)
         (final-color (dir-light-apply final-color
                                       light-color
                                       light-pos
                                       frag-pos
                                       frag-norm
                                       cam-pos .9 1f0)))
    (v! (* (shadow-factor shadowmap light-clip-pos
                          ;;(- light-pos frag-pos) frag-norm
                          )
           final-color)
        1)))

(defpipeline-g generic-shadow-pipe ()
  :vertex   (shadow-vert g-pnt)
  :fragment (shadow-frag :vec2 :vec3 :vec3 :vec4))

;;--------------------------------------------------
;; 3d - g-pnt tb-data

(defun-g shadow-vert-with-tbdata ((vert g-pnt)
                                  (tb tb-data)
                                  &uniform
                                  ;;
                                  (uv-repeat :vec2)
                                  ;;
                                  (model-world :mat4)
                                  (world-view :mat4)
                                  (view-clip :mat4)
                                  (scale :float)
                                  ;; Parallax vars
                                  (light-pos :vec3)
                                  (cam-pos :vec3)
                                  (light-world :mat4)
                                  (light-clip :mat4))
  (let* ((pos       (* scale       (pos vert)))
         (world-pos (* model-world (v! pos 1)))
         (view-pos  (* world-view  world-pos))
         (clip-pos  (* view-clip   view-pos))
         ;;
         (uv        (* uv-repeat (tex vert)))
         ;;
         (normal-m3 (transpose (inverse (m4:to-mat3 model-world))))
         (norm      (norm vert))
         (norm      (* (m4:to-mat3 model-world)
                       norm))
         ;;
         (t0  (normalize
               (* normal-m3 (tb-data-tangent tb))))
         (n0  (normalize
               (* normal-m3 (norm vert))))
         (t0  (normalize (- t0 (* (dot t0 n0) n0))))
         (b0  (cross n0 t0))
         (tbn (mat3 t0 b0 n0)))
    (values clip-pos
            (treat-uvs uv)
            norm
            (s~ world-pos :xyz)
            tbn
            (* tbn light-pos)
            (* tbn cam-pos)
            (* tbn (s~ world-pos :xyz))
            (* light-clip light-world world-pos))))

;;--------------------------------------------------
;; 3d - Frag replacements
(defun-g shadow-pbr-frag ((uv :vec2)
                          (frag-norm :vec3)
                          (frag-pos :vec3)
                          (tbn :mat3)
                          (tan-light-pos :vec3)
                          (tan-cam-pos :vec3)
                          (tan-frag-pos :vec3)
                          (light-clip-pos :vec4)
                          &uniform
                          (samd :sampler-2d)
                          ;;(uv-repeat :float)
                          (uv-speed :float)
                          (time :float)
                          (color :vec3)
                          ;; Shadowmap
                          (shadowmap :sampler-2d)
                          ;; Lighting
                          (light-pos :vec3)
                          (light-col :vec3)
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
         ;; (uv (+ (* uv uv-repeat)
         ;;        (v! 0 (* uv-speed time))))
         (uv (parallax-mapping-offset-flipped
              uv
              (normalize (- tan-cam-pos tan-frag-pos))
              height-map
              .03))
         (roughness (x (texture rough-map uv)))
         (ao        (x (texture ao-map uv)))
         (color (* color (expt (s~ (texture albedo uv) :xyz)
                               (vec3 2.2))))
         ;; Normal Mapping
         ;;(normal (normalize frag-norm))
         (normal (norm-from-map normal-map uv frag-pos frag-norm))
         ;;(normal (normalize (* tbn normal)))
         ;;----------------------------------------
         ;; PBR
         ;; metallic
         (n normal)
         (v (normalize (- cam-pos frag-pos)))
         (specular (* roughness .03))
         ;; pbr - reflectance equation
         (lo (vec3 0f0))
         ;; lights START
         ;;#+nil
         (lo (+ lo (pbr-direct-lum light-pos
                                   frag-pos
                                   v
                                   n
                                   roughness
                                   metallic
                                   color
                                   specular
                                   light-col)))
         ;; ---------- END
         ;;(ambient (* color ao (vec3 .03)))
         ;;(final-color (+ ambient lo))
         (ambient (ambient-ibl v n
                               brdf-lut
                               prefilter-map
                               irradiance-map
                               roughness
                               metallic
                               color
                               ao))
         (final-color (+ ambient
                         (* (shadow-factor shadowmap light-clip-pos)
                            lo)))
         ;; (final-color (+ ambient (* (- 1 (shadow-factor shadowmap light-clip-pos))
         ;;                            lo)))
         ;;(final-color (v3! (- 1 (shadow-factor shadowmap light-clip-pos))))
         ;; Fog
         ;; (final-color
         ;;  (fog-exp2-apply final-color
         ;;                  (v! .18 .17843138 .1552941)
         ;;                  ;;(v! .2 .3 .4)
         ;;                  frag-pos
         ;;                  cam-pos .03))
         )
    ;;(v4! (shadow-factor shadowmap light-clip-pos))
    ;; (v4! (- 1 (shadow-factor shadowmap
    ;;                          light-clip-pos
    ;;                          (- light-pos frag-pos)
    ;;                          frag-norm)))
    (v! final-color 1)
    ;;(v! uv 0 1)
    ;;(v! color 1)
    ;;lo
    ))

;;--------------------------------------------------
(defpipeline-g shadow-pbr-pipe ()
  :vertex   (shadow-vert-with-tbdata g-pnt tb-data)
  :fragment (shadow-pbr-frag :vec2 :vec3 :vec3
                                   :mat3 :vec3 :vec3 :vec3
                                   :vec4))
