(in-package #:incandescent)

;; Static thing without physics.

(defclass route (actor) ;;(physic-box)
  ((ao        :initarg :ao)
   (albedo    :initarg :albedo)
   (normal    :initarg :normal)
   (roughness :initarg :roughness)
   (specular  :initarg :specular)
   (disp      :initarg :disp)
   ;;
   (properties :initform (v! 0 .7 .9 .2)
               :initarg :prop
               :documentation "emissive, spec, rough, metallic")
   (dim :initarg :dim))
  (:default-initargs
   :ao        (get-tex "static/2k_wall/8/white_plaster_02_ao_2k.png"    NIL T :r8)
   :albedo    (get-tex "static/2k_wall/8/white_plaster_02_diff_2k.png"  NIL T :rgb8)
   :disp      (get-tex "static/2k_wall/8/white_plaster_02_disp_2k.png"  NIL T :r8)
   :normal    (get-tex "static/2k_wall/8/white_plaster_02_nor_2k.png"   NIL T :rgb8)
   :roughness (get-tex "static/2k_wall/8/white_plaster_02_rough_2k.png" NIL T :r8)
   :specular  (get-tex "static/2k_wall/8/white_plaster_02_spec_2k.png"  NIL T :r8)))

;; self pos is a mult of lenght (100)
;; advances in negative z
;; starts at 0

(defun make-route (&key (pos   (v! 0 .9 0))
                        (color (v! 1 .3 .9))
                        (dim   (v! 1 1 1))
                        (rot   (q:identity))
                        (name  (gensym))
                        (prop  (v! 0 .7 .7 0))
                        (scale 1f0))
  (let ((obj (make-instance 'route
                            :name name
                            :dim dim
                            :prop prop
                            :scale scale :color color
                            :pos pos :rot rot
                            :buf (box (x dim) (y dim) (z dim) t))))
    (push obj *actors*)
    obj))

(defun-g route-frag ((uv          :vec2)
                     (frag-normal :vec3)
                     (frag-pos    :vec3)
                     (tbn         :mat3)
                     (tlight-pos  :vec3)
                     (tcam-pos    :vec3)
                     (tfrag-pos   :vec3)
                     &uniform
                     (ao          :sampler-2d)
                     (albedo      :sampler-2d)
                     (disp        :sampler-2d)
                     (normals     :sampler-2d)
                     (roughness   :sampler-2d);
                     (specular    :sampler-2d);

                     (color       :vec3)
                     (properties  :vec4))
  (let ((emissive (x properties))
        (spec     (x (texture specular  uv)))
        (rough    (x (texture roughness uv)))
        (metallic (w properties))
        (ao       (x (texture ao uv)))
        (normal   (norm-from-map normals uv tbn))
        (final-color (vec3 (x (* 5 (pow (s~ (texture albedo uv) :xyz) (vec3 2.2)))))))
    (values (v! final-color rough)
            (v! frag-pos    ao)
            (v! normal      spec)
            (v! metallic    emissive))))

(defpipeline-g route-pipe ()
  :vertex   (vert-with-tbdata g-pnt tb-data)
  :fragment (route-frag :vec2 :vec3 :vec3
                              :mat3 :vec3 :vec3 :vec3))

(defmethod draw ((actor route) camera time)
  (with-slots (buf scale color properties
               albedo normal roughness ao specular disp)
      actor
    (map-g #'route-pipe buf
           ;;
           :ao ao
           :albedo albedo
           :disp disp
           :normals normal
           :roughness roughness
           :specular specular
           ;;
           :uv-repeat (v2:*s (v! 10 200) .1)
           ;;
           :color color
           :scale scale
           :properties  properties
           :model-world (model->world actor)
           :world-view  (world->view camera)
           :view-clip   (projection  camera))))

;; Is visible?
(defun behind-drifter-p (self)
  (let ((drifter-pos  (z (pos (state-drifter *game-state*))))
        (self-pos     (z (pos self)))
        (offset       4f0));; offset to hide it from camera
    (when (> (- self-pos *route-half* offset)
             drifter-pos)
      t)))

(defmethod update ((obj route) dt)
  (when (behind-drifter-p obj)
    (decf (z (pos obj)) (* 2 *route-length*))))
