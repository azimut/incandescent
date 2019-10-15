(in-package #:incandescent)

;; Invisible walls

(defclass stopper (physic-box)
  ((properties :initform (v! 0 .7 .9 .2)
               :initarg :prop
               :documentation "emissive, spec, rough, metallic"))
  (:default-initargs
   :pos (v! 0 .9 0)
   :draw-p nil
   :shadow-p nil
   :immovablep t))

(defun make-stopper (&key (pos   (v! 0 .9 0))
                          (color (v! .3 1 .9))
                          (dim   (v! 1 1 1))
                          shadow-p
                          draw-p
                          (rot   (q:identity))
                          (prop  (v! 0 .7 .7 0))
                          (scale 1f0))
  (let ((obj (make-instance 'stopper
                            :prop prop
                            :shadow-p shadow-p
                            :draw-p draw-p
                            :scale scale
                            :color color
                            :pos pos :rot rot
                            :x (coerce (x dim) 'double-float)
                            :y (coerce (y dim) 'double-float)
                            :z (coerce (z dim) 'double-float)
                            :buf (box (x dim)
                                      (y dim)
                                      (z dim)))))
    (push obj *actors*)
    obj))

(defun-g stopper-frag ((uv          :vec2)
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

(defpipeline-g stopper-pipe ()
  :vertex   (vert g-pnt)
  :fragment (stopper-frag :vec2 :vec3 :vec3))

(defmethod draw ((actor stopper) camera time)
  (with-slots (buf scale color properties draw-p) actor
    (when draw-p
      (map-g #'stopper-pipe buf
             :color color
             :scale scale
             :properties  properties
             :model-world (model->world actor)
             :world-view  (world->view camera)
             :view-clip   (projection  camera)))))

(defmethod update ((obj stopper) dt)
  (when (behind-drifter-p obj)
    (with-slots (pos) obj
      (let ((new-pos (v! (x pos)
                         (y pos)
                         (- (z (pos obj)) (* 2 *route-length*)))))
        (setf pos new-pos)
        (ode-update-pos obj new-pos)))))
