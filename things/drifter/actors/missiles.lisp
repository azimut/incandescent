(in-package #:incandescent)

(defclass missile (physic-box)
  ((properties :initform (v! 0 .7 .9 .2)
               :initarg :prop
               :documentation "emissive, spec, rough, metallic")))

(defun make-missile (&key (pos   (v! 0 .9 0))
                          (color (v! .9 .5 .4))
                          (dim   (v! .5 2 .5))
                          (rot   (q:identity))
                          (prop  (v! 0 .7 .7 0))
                          (scale 1f0))
  (let ((obj (make-instance 'missile
                            :prop prop
                            :scale scale
                            :color color
                            :pos pos :rot rot
                            :x (coerce (x dim) 'double-float)
                            :y (coerce (y dim) 'double-float)
                            :z (coerce (z dim) 'double-float)
                            :buf (box (coerce (x dim) 'single-float)
                                      (coerce (y dim) 'single-float)
                                      (coerce (z dim) 'single-float)))))
    (push obj *actors*)
    obj))

(defun-g missile-frag ((uv          :vec2)
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

(defpipeline-g missile-pipe ()
  :vertex   (vert g-pnt)
  :fragment (missile-frag :vec2 :vec3 :vec3))

(defmethod draw ((actor missile) camera time)
  (with-slots (buf scale color properties) actor
    (map-g #'missile-pipe buf
           :color color
           :scale scale
           :properties  properties
           :model-world (model->world actor)
           :world-view  (world->view camera)
           :view-clip   (projection  camera))))

(let ((zpos (cm:new cm:cycle :of (alexandria:iota 5 :start -4d0 :step 2d0))))
  (defmethod reinitialize-instance :after ((obj missile) &key)
    (%ode:body-set-linear-vel (body obj) 0d0 0d0 0d0)
    (%ode:body-set-angular-vel (body obj) 0d0 0d0 0d0)
    (ode-update-rot obj (q:identity))
    (%ode:geom-set-position (geom obj) (cm:next zpos) 1d0
                            (coerce (- (z (pos (state-drifter *game-state*)))
                                       50)
                                    'double-float))
    (%ode:body-enable (body obj))))

(defmethod update ((obj missile) dt)
  (%ode:body-add-force (body obj) 0d0 0d0 10d0)
  (when (< (z (pos (state-drifter *game-state*)))
           (+ (z (pos obj)) -3))
    (reinitialize-instance obj :color (v! 2 (random 1f0) (random 1f0)))))

(defmethod collide ((self missile) (obj drifter))
  (incf (health obj) 1))
(defmethod collide ((obj drifter) (self missile))
  (incf (health obj) 1))
