(in-package #:incandescent)

;; Moving things that hit the player.

(defclass obstacle (physic-sphere)
  ((properties :initform (v! 0 .7 .9 .2)
               :initarg :prop
               :documentation "emissive, spec, rough, metallic")))

(defmethod initialize-instance :after ((obj obstacle) &key)
  (push (slot-value obj 'body) *obstacles-pointers*))

(defun make-obstacle (&key (pos      (v! 0 .9 0))
                           (color    (v! .3 1 .9))
                           (radius   1f0)
                           (shadow-p t)
                           (rot      (q:identity))
                           (prop     (v! 0 .7 .7 0))
                           (scale    1f0))
  (let ((obj (make-instance 'obstacle
                            :prop prop
                            :shadow-p shadow-p
                            :scale scale
                            :color color
                            :pos pos :rot rot
                            :radius
                            (coerce radius 'double-float)
                            :buf
                            (sphere (coerce radius 'single-float)))))
    (push obj *actors*)
    obj))

(defun-g obstacle-frag ((uv          :vec2)
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

(defpipeline-g obstacle-pipe ()
  :vertex   (vert g-pnt)
  :fragment (obstacle-frag :vec2 :vec3 :vec3))

(defmethod draw ((actor obstacle) camera time)
  (with-slots (buf scale color properties) actor
    (map-g #'obstacle-pipe buf
           :color color
           :scale scale
           :properties  properties
           :model-world (model->world actor)
           :world-view  (world->view camera)
           :view-clip   (projection  camera))))

;; fake it
(let ((pos (cm:new cm:cycle :of '(-2 0 2))))
  (defun reset-obstacle (obj)
    (with-slots (body) obj
      (let ((newpos
              (v! (cm:next pos)
                  .5
                  (random-in-range -15 -10))))
        (ode-update-pos obj newpos)
        (%ode:body-set-force body 0d0 0d0 0d0)
        (%ode:body-set-torque body 0d0 0d0 0d0)
        (%ode:body-set-linear-vel body 0d0 0d0 0d0)
        (%ode:body-enable body)))))

(defmethod update ((obj obstacle) dt)
  (with-slots (body mass pos) obj
    (when (or (> (z pos) 20)
              (< (x pos) -6)
              (> (x pos) 6))
      (reset-obstacle obj))
    (%ode:body-add-force body 0d0 0d0 4d0)))
