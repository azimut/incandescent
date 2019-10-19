(in-package #:incandescent)

(defclass collectable (physic-sphere)
  ((life :initform 10f0)
   (properties :initform (v! 0 .7 .9 .2)
               :initarg :prop
               :documentation "emissive, spec, rough, metallic")))

(defun make-collectable (&key (pos      (v! 0 .9 0))
                              (color    (v! .9 1 .3))
                              (radius   1f0)
                              (shadow-p t)
                              (rot      (q:identity))
                              (prop     (v! 0 .7 .7 0))
                              (scale    1f0))
  (let ((obj (make-instance 'collectable
                            :prop prop
                            :shadow-p shadow-p
                            :scale scale
                            :color color
                            :pos pos :rot rot
                            :radius (coerce radius 'double-float)
                            :buf (sphere (coerce radius 'single-float)))))
    (push obj *actors*)
    obj))

(defun-g collectable-frag ((uv          :vec2)
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

(defpipeline-g collectable-pipe ()
  :vertex   (vert g-pnt)
  :fragment (collectable-frag :vec2 :vec3 :vec3))

(defmethod draw ((actor collectable) camera time)
  (with-slots (buf scale color properties) actor
    (map-g #'collectable-pipe buf
           :color color
           :scale scale
           :properties  properties
           :model-world (model->world actor)
           :world-view  (world->view camera)
           :view-clip   (projection  camera))))

(let ((pos (cm:new cm:cycle :of '(-2 0 2))))
  (defun reset-collectable (obj)
    (with-slots (body life) obj
      (let ((newpos
              (v! (cm:next pos)
                  .5
                  (+ (z (pos (state-drifter *game-state*)))
                     (random-in-range -35 -40)))))
        (setf life (random-in-range 10f0 15f0))
        (ode-update-pos obj newpos)
        (%ode:body-set-linear-vel body 0d0 0d0 0d0)
        (%ode:body-enable body)))))

(defmethod update ((obj collectable) dt)
  (when (behind-drifter-p obj)
    (reset-collectable obj))
  (%ode:body-add-force (body obj) 0d0 0d0 4d0))

(defmethod collide ((o1 collectable) (o2 drifter)) (collected o1))
(defmethod collide ((o1 drifter) (o2 collectable)) (collected o2))

(defun collected (obj)
  (reset-collectable obj)
  (incf (state-score *game-state*))
  (make-text (format nil "score: ~d" (state-score *game-state*))
             :scale 1.5f0))
