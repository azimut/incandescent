(in-package #:incandescent)

(defclass route (actor) ;;(physic-box)
  ((properties :initform (v! 0 .7 .9 .2)
               :initarg :prop
               :documentation "emissive, spec, rough, metallic"))
  (:default-initargs
   :pos (v! 0 .9 0)))

(defun make-route (&key (pos   (v! 0 .9 0))
                        (color (v! 1 .3 .9))
                        (dim   (v! 1 1 1))
                        (rot   (q:identity))
                        (prop  (v! 0 .7 .7 0))
                        (scale 1f0))
  (let ((obj (make-instance 'route
                            :prop prop
                            :scale scale :color color
                            :pos pos :rot rot
                            ;; ODE
                            ;; :x (coerce (x dim) 'double-float)
                            ;; :y (coerce (y dim) 'double-float)
                            ;; :z (coerce (z dim) 'double-float)
                            ;; ---
                            :buf (box (x dim) (y dim) (z dim)))))
    (push obj *actors*)
    obj))

(defun-g route-frag ((uv          :vec2)
                     (frag-normal :vec3)
                     (frag-pos    :vec3)
                     &uniform
                     (color       :vec3)
                     (properties  :vec4))
  (let ((emissive (x properties))
        (spec     (y properties))
        (rough    (z properties))
        (metallic (w properties))
        (ao       1f0)
        (albedo   color))
    (values (v! albedo      rough)
            (v! frag-pos    ao)
            (v! frag-normal spec)
            (v! metallic    emissive))))

(defpipeline-g route-pipe ()
  :vertex   (vert g-pnt)
  :fragment (route-frag :vec2 :vec3 :vec3))

(defmethod draw ((actor route) camera time)
  (with-slots (buf scale color properties) actor
    (map-g #'route-pipe buf
           :color color
           :scale scale
           :properties  properties
           :model-world (model->world actor)
           :world-view  (world->view camera)
           :view-clip   (projection  camera))))
