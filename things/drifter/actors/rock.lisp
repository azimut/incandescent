(in-package #:incandescent)

(defclass rock (physic-box)
  ((life :initarg :life :accessor life)
   (properties :initform (v! 0 .7 .9 .2)
               :initarg :prop
               :documentation "emissive, spec, rough, metallic"))
  (:default-initargs
   :life (random-in-range 10f0 20f0)))

(defun make-rock (&key (pos   (v! 0 .9 0))
                       (color (v! .3 1 .9))
                       (dim   (v! 1 1 1))
                       (rot   (q:identity))
                       (prop  (v! 0 .7 .7 0))
                       (scale 1f0))
  (let ((obj (make-instance 'rock
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

(defun-g rock-frag ((uv          :vec2)
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

(defpipeline-g rock-pipe ()
  :vertex   (vert g-pnt)
  :fragment (rock-frag :vec2 :vec3 :vec3))

(defmethod draw ((actor rock) camera time)
  (with-slots (buf scale color properties) actor
    (map-g #'rock-pipe buf
           :color color
           :scale scale
           :properties  properties
           :model-world (model->world actor)
           :world-view  (world->view camera)
           :view-clip   (projection  camera))))

;; fake it
(let ((xpos (cm:new cm:heap :of '(-2 0 2)))
      (zpos (cm:new cm:heap :of (alexandria:iota 8 :start -40 :step -7))))
  (defun reset-rock (obj)
    (let ((newpos
            (v! (cm:next xpos)
                (cm:pick (* .5 (slot-value obj 'y))
                         (* 2 (slot-value obj 'y)))
                (+ (z (pos (state-drifter *game-state*)))
                   (cm:next zpos))))
          (newrot (cm:pick (q:identity)
                           (q:from-axis-angle (v! 0 1 0) (radians (random 360f0)))
                           (q:from-axis-angle (v! (random 1f0) (random 1f0) (random 1f0))
                                              (radians (random 360f0))))))
      ;;(setf (life obj) (random-in-range 10f0 20f0))
      (and (cm:odds .5)
           (%ode:body-add-force (body obj)
                                (cm:pick 100d0 -100d0)
                                0d0
                                0d0))
      (ode-update-rot obj newrot)
      (ode-update-pos obj newpos)
      (%ode:body-enable (body obj)))))

(defmethod update ((obj rock) dt)
  (if (and ;;(< (life obj) 0)
       (behind-drifter-p obj))
      (reset-rock obj)
      ;;(decf (life obj) dt)
      ))
