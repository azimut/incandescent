(in-package #:incandescent)

(defclass pbr (actor)
  ((albedo    :initarg :albedo)
   (ao        :initarg :ao)
   (height    :initarg :height)
   (normal    :initarg :normal)
   (roughness :initarg :roughness)
   (uv-repeat :initarg :uv-repeat)
   (uv-speed  :initarg :uv-speed)
   (metallic  :initarg :metallic)
   (parallax  :initarg :parallax)
   (specular  :initarg :specular))
  (:default-initargs
   :uv-repeat (v! 1 1)
   :uv-speed  .1
   :metallic  .1
   :parallax  .03
   :specular  .1
   :albedo    (get-tex "static/32.Rock01-1k/rock01_albedo.jpg"    NIL T :rgb8)
   :ao        (get-tex "static/32.Rock01-1k/rock01_ao.jpg"        NIL T :r8)
   :height    (get-tex "static/32.Rock01-1k/rock01_height.jpg"    NIL T :r8)
   :normal    (get-tex "static/32.Rock01-1k/rock01_normal.jpg"    NIL T :rgb8)
   :roughness (get-tex "static/32.Rock01-1k/rock01_roughness.jpg" NIL T :r8)))

(defun make-pbr (&key (pos (v! 0 0 0)))
  (let ((obj (make-instance
              'pbr
              :buf (box 1 1 1 t)
              :pos pos)))
    (push obj *actors*)
    obj))

(defmethod update ((actor pbr) dt)
  #+nil
  (with-slots (metallic parallax specular) actor
    (setf metallic .1)
    (setf specular  .1f0)
    (setf parallax .05)))

(defmethod draw ((actor pbr) camera (time single-float))
  (with-slots (buf
               color
               parallax specular
               albedo normal height roughness
               uv-speed
               scale ao uv-repeat metallic)
      actor
    (map-g #'pbr-pipe buf
           :uv-repeat uv-repeat
           :uv-speed  uv-speed
           :scale scale
           :time time
           :color color
           ;; Lighting
           :cam-pos (pos camera)
           :cam-dir (q:to-direction (rot camera)) ;; flashlight
           :light-pos *light-pos*
           :light-col *light-color*
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

(defun-g pbr-frag ((uv            :vec2)
                   (frag-norm     :vec3)
                   (frag-pos      :vec3)
                   (tbn           :mat3)
                   (tan-light-pos :vec3)
                   (tan-cam-pos   :vec3)
                   (tan-frag-pos  :vec3)
                   &uniform
                   (uv-repeat  :vec2)
                   (uv-speed   :float)
                   (time       :float)
                   (color      :vec3)
                   ;; Lighting
                   (light-pos  :vec3)
                   (light-col  :vec3)
                   (cam-pos    :vec3)
                   (cam-dir    :vec3) ;; flashlight
                   (shape      :sampler-2d) ;; flashlight
                   ;;
                   (specular   :float)
                   (parallax   :float)
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
         (uv (parallax-mapping-offset
              uv
              (normalize (- tan-cam-pos tan-frag-pos))
              height-map
              parallax))
         ;;(frag-pos  tan-frag-pos)
         ;;(cam-pos   tan-cam-pos)
         ;;(light-pos tan-light-pos)
         ;;
         (roughness (x (texture rough-map uv)));; FIXME + .5???
         (ao        (x (texture ao-map uv)))
         (color     (* color (expt (s~ (texture albedo uv) :xyz)
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
         ;; pbr - reflectance equation
         (lo (vec3 0f0))
         ;; lights START
         (lo (+ lo (pbr-direct-lum light-pos
                                   frag-pos
                                   v
                                   n
                                   roughness
                                   metallic
                                   color
                                   specular
                                   light-col)))
         (ambient (ambient-ibl v n
                               brdf-lut
                               prefilter-map
                               irradiance-map
                               roughness
                               metallic
                               color
                               ao))
         (final-color (+ ambient lo)))
    (v! final-color 1)
    ;;ambient
    ;;normal
    ;;color
    ;;lo
    ;;(v3! roughness)
    ;;(v3! roughness)
    ;;(v3! ao)
    ;;(v! uv 0 1)
    ))

(defpipeline-g pbr-pipe ()
  :vertex   (vert-with-tbdata g-pnt
                              tb-data)
  :fragment (pbr-frag :vec2
                      :vec3 :vec3
                      :mat3 :vec3 :vec3 :vec3))
