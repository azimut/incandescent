(in-package :incandescent)

(defvar *chair* nil)
(defparameter *shadow-camera*
  (let* ((lpos (v! 0 15 0))
         (cam  (make-instance
                'orth
                :name :shadow-camera
                :frame-size (v2! 30) ;; zoom
                :far 200f0
                :near .1f0
                :rot (q:point-at (v! 0 1 0) lpos
                                 (v! 0 0 -30))
                :pos lpos)))
    (setf *light-pos* lpos)
    cam))

(defclass desk (assimp-thing) ())

(defmethod update ((actor desk) dt)
  ;;#+nil
  (when *world*
    (with-slots (pos rot) actor
      (with-slots (geom orot immovablep) *chair*
        (unless immovablep
          (setf pos (ode-geom-get-position geom))
          (setf rot (ode-geom-get-quaternion2 orot geom))))))
  #+nil
  (with-slots (pos) actor
    (setf pos (V! 0 -5 -15))))

(defun-g desk-frag ((uv             :vec2)
                    (frag-norm      :vec3)
                    (frag-pos       :vec3)
                    (tbn            :mat3)
                    (tan-light-pos  :vec3)
                    (tan-cam-pos    :vec3)
                    (tan-frag-pos   :vec3)
                    ;;(light-clip-pos :vec4)
                    &uniform
                    ;;
                    ;;(shadowmap      :sampler-2d)
                    ;;
                    (light-pos      :vec3)
                    (light-color    :vec3)
                    ;;
                    (cam-pos        :vec3)
                    (time           :float)
                    ;; IBL
                    (irradiance-map :sampler-cube)
                    (prefilter-map  :sampler-cube)
                    (brdf-lut       :sampler-2d)
                    ;;
                    (albedo         :sampler-2d)
                    (normals        :sampler-2d)
                    (specular       :sampler-2d))
  (let* ((color (expt (s~ (texture albedo uv) :xyz)
                      (vec3 2.2)))
         (normal (norm-from-map normals uv frag-pos frag-norm))
         ;; (normal (norm-from-map normals uv))
         ;; (normal (normalize (* tbn normal)))
         ;; (frag-pos  tan-frag-pos)
         ;; (light-pos tan-light-pos)
         ;; (cam-pos   tan-cam-pos)
         #+nil
         (final-color  (* 5 (point-light-apply-spec color
                                                    (v! 5 5 5)
                                                    (v! 0 20 0) ;;light-pos
                                                    frag-pos
                                                    normal
                                                    1f0 .35 .44
                                                    cam-pos
                                                    (x (texture specular uv))
                                                    1)))
         (n normal)
         (v (normalize (- cam-pos frag-pos)))
         (metallic .05)
         (roughness .9)
         (f0 (vec3 .04))
         (f0 (mix f0 color metallic))
         ;;#+nil
         (ambient (ambient-ibl v
                               n
                               f0
                               brdf-lut
                               prefilter-map
                               irradiance-map
                               roughness
                               metallic
                               color
                               1f0))
         (final-color (* 5 (pbr-point-lum (v! 0
                                              10
                                              0)
                                          frag-pos
                                          v n
                                          roughness
                                          f0
                                          metallic
                                          color
                                          (x (texture specular uv))
                                          .35 .44)))
         ;;(final-color (* final-color (shadow-factor shadowmap light-clip-pos)))
         )
    (v! (+ ambient final-color) 1)
    ;; color
    ;; (v3! (x (texture specular uv)))
    ;; (v! uv 0 0)
    ))

#+nil
(defpipeline-g desk-pipe ()
  :vertex   (shadow-vert-with-tbdata g-pnt tb-data)
  :fragment (desk-frag :vec2 :vec3 :vec3 :mat3
                       :vec3 :vec3 :vec3 :vec4))

(defpipeline-g desk-pipe ()
  :vertex   (vert-with-tbdata g-pnt tb-data)
  :fragment (desk-frag :vec2 :vec3 :vec3 :mat3
                       :vec3 :vec3 :vec3))

(defmethod draw ((actor desk) camera (time single-float))
  (with-slots (buf albedo normals scale specular) actor
    (map-g #'desk-pipe buf
           :scale scale
           :uv-repeat (v! 1 1)
           ;; shadow
           ;;:shadowmap *shadow-sam*
           ;;:light-world (world->view *shadow-camera*)
           ;;:light-clip (projection *shadow-camera*)
           :brdf-lut       *s-brdf*
           :prefilter-map  *s-cubemap-prefilter*
           :irradiance-map *s-cubemap-live*
           ;; Lighting
           :light-pos   *light-pos*
           :light-color *light-color*
           ;;
           :model-world (model->world actor)
           :world-view  (world->view camera)
           :view-clip   (projection camera)
           ;; PBR
           :cam-pos (pos camera)
           :albedo albedo
           :time time
           :specular specular
           :normals normals)))


(defun make-chair (&key (pos (v! 0 0 0))
                        (rot (q:identity)))
  (mapcar
   (lambda (obj)
     (destructuring-bind (&key buf albedo normals specular scene &allow-other-keys) obj
       (push (make-instance 'desk
                            :scene scene
                            :buf buf
                            :pos pos :rot rot
                            :albedo albedo
                            :normals normals
                            :specular specular)
             *actors*)))
   (let ((*default-specular* "/home/sendai/Downloads/chair/classic_hotel_chair_s.png")
         (*processing-flags*
           '(:ai-process-triangulate
             ;;:ai-process-flip-u-vs
             ;;:ai-process-preset-target-realtime-max-quality
             :ai-process-preset-target-realtime-quality
             :ai-process-calc-tangent-space)))
     ;;(assimp-load-meshes "/home/sendai/Downloads/chair/newchair2.obj")
     (list (car (assimp-load-meshes "/home/sendai/Downloads/chair/chair1blend.s.t.obj")))))
  t)

(defclass physic-chair (physic)
  ((data         :initarg :data)
   (ode-indices  :initarg :ode-indices)
   (ode-vertices :initarg :ode-vertices))
  (:default-initargs
   :data nil
   :ode-indices nil
   :ode-vertices nil))

(defmethod free ((object physic-chair))
  (with-slots (body geom orot data ode-vertices ode-indices) object
    (when body (%ode:body-destroy body))
    (when geom (%ode:geom-destroy geom))
    (claw:free orot)
    (claw:free data)
    (claw:free ode-indices)
    (claw:free ode-vertices)))

(defmethod initialize-instance :after ((obj physic-chair) &key)
  (with-slots (pos rot) obj
    (physic-to-ode  obj)
    (ode-update-pos obj pos)
    (ode-update-rot obj rot)))

(defmethod update ((actor physic-chair) dt)
  (when *world*
    (with-slots (body pos rot orot geom immovablep) actor
      (unless immovablep
        (setf pos (ode-geom-get-position geom))
        (setf rot (ode-geom-get-quaternion2 orot geom))))))

(defmethod draw ((actor physic-chair) camera time)
  #+nil
  (with-slots (buf scale) actor
    (map-g #'desk-pipe buf
           :scale scale
           :uv-repeat (v! 1 1)
           ;; shadow
           ;;:shadowmap *shadow-sam*
           ;;:light-world (world->view *shadow-camera*)
           ;;:light-clip (projection *shadow-camera*)
           ;; Lighting
           :light-pos   *light-pos*
           :light-color *light-color*
           ;;
           :model-world (model->world actor)
           :world-view  (world->view camera)
           :view-clip   (projection camera)
           ;; PBR
           :cam-pos (pos camera)
           :time time)))

(defun make-physic-chair (&key (pos (v! 0 0 0)) (rot (q:identity)))
  (let ((*default-specular* "/home/sendai/Downloads/chair/classic_hotel_chair_s.png")
        (*processing-flags*
          '(:ai-process-triangulate
            ;;:ai-process-flip-u-vs
            ;;:ai-process-calc-tangent-space
            ;;:ai-process-preset-target-realtime-max-quality
            ;;:ai-process-preset-target-realtime-quality
            :ai-process-calc-tangent-space
            )))
    (destructuring-bind (&key buf &allow-other-keys)
        (car (assimp-load-meshes "/home/sendai/Downloads/chair/chair11.simplify9.obj"))
      (setf *chair* (make-instance 'physic-chair
                                   :rot rot
                                   :pos pos
                                   :buf buf))
      (push *chair* *actors*))))
