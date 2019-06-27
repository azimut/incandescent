(in-package :incandescent)

(defparameter *shadow-camera*
  (let* ((lpos (v! 0 -2 10))
         (cam  (make-instance
                'orth
                :name :shadow-camera
                :frame-size (v2! 50) ;; zoom
                :far 200f0
                :near .1f0
                :rot (q:point-at (v! 0 1 0) lpos
                                 (v! 0 -20 -30))
                :pos lpos)))
    (setf *light-pos* lpos)
    cam))

(defclass desk (assimp-thing) ())

(defmethod update ((actor desk) dt)
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
                    (light-clip-pos :vec4)
                    &uniform
                    ;;
                    (shadowmap      :sampler-2d)
                    ;;
                    (light-pos      :vec3)
                    (light-color    :vec3)
                    ;;
                    (cam-pos        :vec3)
                    (time           :float)
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
                                                    (v! 0 -2 0) ;;light-pos
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
         (final-color (* 5 (pbr-point-lum (v! 0
                                              -2
                                              0)
                                          frag-pos
                                          v n
                                          roughness
                                          f0
                                          metallic
                                          color
                                          (x (texture specular uv))
                                          .35 .44)))
         (final-color (* final-color (shadow-factor shadowmap light-clip-pos)))
         )
    (v! final-color 1)
    ;; color
    ;; (v3! (x (texture specular uv)))
    ;; (v! uv 0 0)
    ))

(defpipeline-g desk-pipe ()
  :vertex   (shadow-vert-with-tbdata g-pnt tb-data)
  :fragment (desk-frag :vec2 :vec3 :vec3 :mat3
                       :vec3 :vec3 :vec3 :vec4))

(defmethod draw ((actor desk) camera (time single-float))
  (with-slots (buf albedo normals scale specular) actor
    (map-g #'desk-pipe buf
           :scale scale
           :uv-repeat (v! 1 1)
           ;; shadow
           :shadowmap *shadow-sam*
           :light-world (world->view *shadow-camera*)
           :light-clip (projection *shadow-camera*)
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
     (with-slots (buf albedo normals specular scene) obj
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
             :ai-process-preset-target-realtime-max-quality
             ;;:ai-process-preset-target-realtime-quality
             :ai-process-calc-tangent-space)))
     (assimp-load-meshes "/home/sendai/Downloads/chair/newchair2.obj"))))
