(in-package #:incandescent)

;;--------------------------------------------------
;; 3d - plain frag pipeline, used to render only depth
(defun-g simplest-3d-frag ((uv :vec2)
                           (frag-norm :vec3)
                           (frag-pos :vec3))
  (v! 1 1 1 0))


(defun-g vert-bones ((vert g-pnt)
                     (tb tb-data)
                     (bones assimp-bones)
                     &uniform
                     (model-world :mat4)
                     (world-view :mat4)
                     (view-clip :mat4)
                     (offsets (:mat4 36))
                     (scale :float))
  ;;
  (let* ((pos       (* scale (pos vert)))
         (uv        (treat-uvs (tex vert)))
         (norm      (norm vert))
         (norm      (* (m4:to-mat3 model-world) norm))
         (world-pos (* (+ (* (aref (assimp-bones-weights bones) 0)
                             (aref offsets (int (aref (assimp-bones-ids bones) 0))))
                          (* (aref (assimp-bones-weights bones) 1)
                             (aref offsets (int (aref (assimp-bones-ids bones) 1))))
                          (* (aref (assimp-bones-weights bones) 2)
                             (aref offsets (int (aref (assimp-bones-ids bones) 2))))
                          (* (aref (assimp-bones-weights bones) 3)
                             (aref offsets (int (aref (assimp-bones-ids bones) 3)))))
                       (v! pos 1)))
         (world-pos (* model-world world-pos))
         ;;(world-pos (* model-world (v! pos 1)))
         (view-pos  (* world-view  world-pos))
         (clip-pos  (* view-clip   view-pos)))
    (values clip-pos uv norm (s~ world-pos :xyz))))

(defpipeline-g simplest-3d-pipe ()
  :vertex (vert g-pnt)
  :fragment (simplest-3d-frag :vec2 :vec3 :vec3))

(defpipeline-g simplest-3d-pipe-bones ()
  :vertex (vert-bones g-pnt tb-data assimp-bones)
  :fragment (simplest-3d-frag :vec2 :vec3 :vec3))

;;--------------------------------------------------
;; THE helper

(defun-g shadow-factor ((light-sampler :sampler-2d)
                        (pos-in-light-space :vec4))
  (let* ((proj-coords (/ (s~ pos-in-light-space :xyz)
                         (w pos-in-light-space)))
         (proj-coords (+ (* proj-coords 0.5) (vec3 0.5)))
         (our-depth (z proj-coords))
         (shadow 0f0)
         (bias 0.005)
         (texel-size (/ (vec2 1f0)
                        (texture-size light-sampler 0)))
         (uv (s~ proj-coords :xy)))
    ;;
    (for (x -1) (<= x 1) (++ x)
         (for (y -1) (<= y 1) (++ y)
              (let* ((uv+offset (+ uv (* (v! x y) texel-size)))
                     (pcf-depth (x (texture light-sampler
                                            uv+offset))))
                (incf shadow
                      (if (> (- our-depth bias) pcf-depth)
                          0f0
                          1f0)))))
    ;;
    (/ shadow 9f0)))

;;--------------------------------------------------
;; 3d - Replacements for basic vert and frags
(defun-g shadow-vert ((vert g-pnt)
                      (point-light-clip :vec4)
                      &uniform
                      (model-world :mat4)
                      (world-view :mat4)
                      (view-clip :mat4)
                      (scale :float)
                      (time :float)
                      (light-world :mat4)
                      (light-clip :mat4))
  ;;
  (let* ((pos        (* scale (pos vert)))
         (norm       (norm vert))
         (tex        (tex vert))
         (world-norm (* (m4:to-mat3 model-world) norm))
         (world-pos  (* model-world (v! pos 1)))
         (view-pos   (* world-view  world-pos))
         (clip-pos   (* view-clip   view-pos)))
    (values clip-pos
            tex
            (+ gl-instance-id world-norm)
            (s~ world-pos :xyz)
            (* light-clip light-world world-pos))))

(defun-g shadow-vert-with-tbdata ((vert g-pnt)
                                  (tb tb-data)
                                  &uniform
                                  (model-world :mat4)
                                  (world-view :mat4)
                                  (view-clip :mat4)
                                  (scale :float)
                                  ;; Parallax vars
                                  (light-pos :vec3)
                                  (cam-pos :vec3)
                                  (light-world :mat4)
                                  (light-clip :mat4))
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
            (* tbn (s~ world-pos :xyz))
            (* light-clip light-world world-pos))))

(defun-g shadow-vert-with-bones ((vert g-pnt)
                                 (tb tb-data)
                                 (bones assimp-bones)
                                 &uniform
                                 (offsets (:mat4 36))
                                 (model-world :mat4)
                                 (world-view :mat4)
                                 (view-clip :mat4)
                                 (scale :float)
                                 ;; Parallax vars
                                 (light-pos :vec3)
                                 (cam-pos :vec3)
                                 (light-world :mat4)
                                 (light-clip :mat4))
  (let* ((pos       (* scale (pos vert)))
         (norm      (norm vert))
         (uv        (treat-uvs (tex vert)))
         (norm      (* (m4:to-mat3 model-world) norm))
         (world-pos (* (+ (* (aref (assimp-bones-weights bones) 0)
                             (aref offsets (int (aref (assimp-bones-ids bones) 0))))
                          (* (aref (assimp-bones-weights bones) 1)
                             (aref offsets (int (aref (assimp-bones-ids bones) 1))))
                          (* (aref (assimp-bones-weights bones) 2)
                             (aref offsets (int (aref (assimp-bones-ids bones) 2))))
                          (* (aref (assimp-bones-weights bones) 3)
                             (aref offsets (int (aref (assimp-bones-ids bones) 3)))))
                       (v! pos 1)))
         (world-pos (* model-world world-pos))
         ;;(world-pos (* model-world (v! pos 1)))
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
                          (uv-repeat :float)
                          (uv-speed :float)
                          (time :float)
                          (color :vec3)
                          ;; Shadowmap
                          (shadowmap :sampler-2d)
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
         ;; (uv (+ (* uv uv-repeat)
         ;;        (v! 0 (* uv-speed time))))
         ;; (uv (parallax-mapping-offset-flipped
         ;;      uv
         ;;      (normalize (- tan-cam-pos tan-frag-pos))
         ;;      height-map
         ;;      .03))
         (roughness (x (texture rough-map uv)))
         (ao        (x (texture ao-map uv)))
         (color (* color (expt (s~ (texture albedo uv) :xyz)
                               (vec3 2.2))))
         ;; Normal Mapping
         (normal (normalize frag-norm))
         ;; (normal (norm-from-map normal-map uv))
         ;; (normal (normalize (* tbn normal)))
         ;;----------------------------------------
         ;; PBR
         ;; metallic
         (n normal)
         (v (normalize (- cam-pos frag-pos)))
         (metallic .1)
         (f0 (vec3 .04))
         ;;(f0 color)
         (f0 (mix f0 color metallic))
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
         (final-color (+ ambient lo))
         (final-color (* (shadow-factor shadowmap light-clip-pos)
                         final-color))
         ;; Fog
         ;; (final-color
         ;;  (fog-exp2-apply final-color
         ;;                  (v! .18 .17843138 .1552941)
         ;;                  ;;(v! .2 .3 .4)
         ;;                  frag-pos
         ;;                  cam-pos .03))
         )
    (v! final-color 1)
    ;;(v! uv 0 1)
    ;;(v! color 1)
    ))

;;--------------------------------------------------
(defpipeline-g shadow-pbr-pipe ()
  :vertex   (shadow-vert-with-tbdata g-pnt tb-data)
  :fragment (shadow-pbr-frag :vec2 :vec3 :vec3
                             :mat3 :vec3 :vec3 :vec3
                             :vec4))

(defpipeline-g shadow-pbr-pipe-bones ()
  :vertex   (shadow-vert-with-bones g-pnt tb-data assimp-bones)
  :fragment (shadow-pbr-frag :vec2 :vec3 :vec3
                             :mat3 :vec3 :vec3 :vec3
                             :vec4))
;;--------------------------------------------------
(defmethod draw ((actor piso) camera (time single-float))
  (with-slots (buf
               color
               albedo normal height roughness
               uv-speed
               scale ao uv-repeat metallic)
      actor
    (map-g #'shadow-pbr-pipe buf
           :uv-repeat uv-repeat
           :uv-speed uv-speed
           :scale scale
           :time time
           :color color
           :samd *samd*
           ;; Lighting
           :cam-pos (pos camera)
           :light-pos *light-pos*
           ;;
           :model-world (model->world actor)
           :world-view (world->view camera)
           :view-clip  (projection camera)
           ;; Shadowmap
           :shadowmap *shadow-sam*
           :light-world (world->view *shadow-camera*)
           :light-clip (projection *shadow-camera*)
           ;; PBR
           :albedo albedo
           :ao-map ao
           :metallic metallic
           :normal-map normal
           :height-map height
           :rough-map roughness
           ;; IBL
           :brdf-lut *s-brdf*
           :prefilter-map *s-cubemap-prefilter*
           :irradiance-map *s-cubemap-live*)))
