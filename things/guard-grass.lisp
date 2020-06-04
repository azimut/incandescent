(in-package :incandescent)

(defparameter *shadow-camera*
  (let* ((lpos (v3:*s  *light-pos* 1f0))
         (cam  (make-instance 'orth
                              :name :shadow-camera
                              :frame-size (v2! 300) ;; zoom
                              :far 210f0
                              :near 10f0
                              :rot (q:point-at (v! 0 1 0) lpos
                                               (v! 0 0 0))
                              :pos lpos)))
    ;;(setf *light-pos* lpos)
    cam))

(defclass grass-floor (actor)
  ((albedo    :initarg :albedo)
   (ao        :initarg :ao)
   (height    :initarg :height)
   (normal    :initarg :normal)
   (roughness :initarg :roughness)
   ;;
   (uv-repeat :initarg :uv-repeat)
   (uv-speed  :initarg :uv-speed)
   ;;
   (metallic  :initarg :metallic)
   (parallax  :initarg :parallax)
   (specular  :initarg :specular))
  (:default-initargs
   :uv-repeat (v! 2 2)
   :uv-speed  .1
   :metallic  .1
   :parallax  .03
   :specular  (get-tex "static/_grass2/grass_path_2_spec_2k.png"  NIL T :r8)
   :albedo    (get-tex "static/_grass2/grass_path_2_diff_2k.png"  NIL T :rgb8)
   :ao        (get-tex "static/_grass2/grass_path_2_AO_2k.png"    NIL T :r8)
   :height    (get-tex "static/_grass2/disp8.png"  NIL T :r8)
   :normal    (get-tex "static/_grass2/grass_path_2_nor_2k.png"   NIL T :rgb8)
   :roughness (get-tex "static/_grass2/grass_path_2_rough_2k.png" NIL T :r8)))

(defun make-grass-floor (&key (pos (v! 0 0 0))
                              (rot (q:identity))
                              (uv-repeat (v! 1 1)))
  (declare (type rtg-math.types:vec2 uv-repeat)
           (type rtg-math.types:quaternion rot)
           (type rtg-math.types:vec3 pos))
  (let ((obj (make-instance 'grass-floor :pos pos :rot rot
                                         :uv-repeat uv-repeat
                                         :buf (lattice 800 800 4 4 t))))
    (push obj *actors*)
    obj))

;;--------------------------------------------------

(defmethod draw ((actor grass-floor) camera (time single-float))
  (with-slots (buf
               color
               parallax specular
               albedo normal height roughness
               uv-speed
               scale ao uv-repeat metallic)
      actor
    (map-g #'grass-floor-pipe buf
           :uv-repeat uv-repeat
           :uv-speed  uv-speed
           :scale scale
           :time time
           :color color
           ;;
           :shadowmap *shadow-sam*
           :light-world (world->view *shadow-camera*)
           :light-clip (projection *shadow-camera*)
           ;; Lighting
           :cam-pos (pos camera)
           :cam-dir (q:to-direction (rot camera)) ;; flashlight
           :light-pos *light-pos*
           ;;
           :model-world (model->world actor)
           :world-view  (world->view camera)
           :view-clip   (projection camera)
           ;;
           :specular   specular
           :parallax   parallax
           ;; PBR
           :albedo     albedo
           :ao-map     ao
           :metallic   metallic
           :normal-map normal
           :height-map height
           :rough-map  roughness
           ;; IBL
           :brdf-lut       *s-brdf*
           :prefilter-map  *s-cubemap-prefilter*
           :irradiance-map *s-cubemap-live*)))

(defun-g grass-floor-frag ((uv            :vec2)
                           (frag-norm     :vec3)
                           (frag-pos      :vec3)
                           (tbn           :mat3)
                           (tan-light-pos :vec3)
                           (tan-cam-pos   :vec3)
                           (tan-frag-pos  :vec3)
                           (light-clip-pos :vec4)
                           &uniform
                           (shadowmap  :sampler-2d)
                           ;;
                           (uv-speed   :float)
                           (time       :float)
                           (color      :vec3)
                           ;; Lighting
                           (light-pos  :vec3)
                           (cam-pos    :vec3)
                           (cam-dir    :vec3) ;; flashlight
                           (shape      :sampler-2d) ;; flashlight
                           ;;
                           (parallax   :float)
                           (specular   :sampler-2d)
                           ;; PBR
                           (metallic   :float)
                           (albedo     :sampler-2d)
                           (ao-map     :sampler-2d)
                           (height-map :sampler-2d)
                           (normal-map :sampler-2d)
                           (rough-map  :sampler-2d)
                           ;; IBL
                           (brdf-lut       :sampler-2d)
                           (prefilter-map  :sampler-cube)
                           (irradiance-map :sampler-cube))
  (let* (;; First change UV, then parallax!
         ;; (uv (+ (* uv uv-repeat)
         ;;        (v! 0 (* uv-speed time))))
         ;; (uv (v! (* (x uv-repeat) (x uv))
         ;;         (* (y uv-repeat) (y uv))))
         ;;(uv (treat-uvs uv))
         #+nil
         (uv (parallax-mapping-offset
              uv
              (normalize (- tan-cam-pos tan-frag-pos))
              height-map
              parallax))
         ;; (frag-pos  tan-frag-pos)
         ;; (cam-pos   tan-cam-pos)
         ;; (light-pos tan-light-pos)
         ;;
         ;;#+nil
         (uv (+ uv (* *time* .05 time)))
         (roughness (+ .5 (x (texture rough-map uv))))
         (ao        (x (texture ao-map uv)))
         (color     (expt (s~ (texture albedo uv) :xyz)
                          (vec3 2.2)))
         ;; Normal Mapping
         ;;(normal (normalize frag-norm))
         (normal (normalize (norm-from-map normal-map uv)))
         (normal (normalize (* tbn normal)))
         ;;(normal (norm-from-map normal-map uv frag-pos frag-norm))
         ;;----------------------------------------
         ;; PBR
         ;; metallic
         (n normal)
         (v (normalize (- cam-pos frag-pos)))
         (metallic .1)
         (roughness .9)
         (f0 (vec3 .04))
         ;;(f0 color)*
         (f0 (mix f0 color metallic))
         ;;(f0 (v3! 0))
         ;; pbr - reflectance equation
         (lo (vec3 0f0))
         ;; lights START
         (lo (+ lo
                (pbr-direct-lum light-pos
                                frag-pos
                                v
                                n
                                roughness
                                f0
                                metallic
                                color
                                (x (texture specular uv)))
                ))
         (ambient (ambient-ibl v n f0
                               ;;brdf-lut
                               prefilter-map
                               irradiance-map
                               roughness
                               metallic
                               color
                               ao))
         (final-color (+ ambient lo)))
    ;;(v! final-color 1)
    ;;(v3! (x (texture height-map uv)))
    ;;ambient
    ;;normal
    ;;lo
    ;;color
    (* (shadow-factor shadowmap light-clip-pos) final-color)
    ;;(* (shadow-factor shadowmap light-clip-pos) lo)
    ;;(* (shadow-factor shadowmap light-clip-pos) color)
    ;;tan-cam-pos
    ;;(v3! (x (texture specular uv)))
    ;;(v3! roughness)
    ;;(v3! ao)
    ;;(v! uv 0 1)
    ))


#+nil
(defpipeline-g grass-floor-pipe ()
  :vertex   (shadow-vert-with-tbdata g-pnt
                                     tb-data)
  :fragment (grass-floor-frag :vec2
                              :vec3 :vec3
                              :mat3 :vec3 :vec3 :vec3
                              :vec4))
(defpipeline-g grass-floor-pipe ()
  :vertex   (vert-with-tbdata g-pnt
                              tb-data)
  :fragment (grass-floor-frag :vec2
                              :vec3 :vec3
                              :mat3 :vec3 :vec3 :vec3))
