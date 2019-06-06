(in-package #:incandescent)

(defclass physic (actor)
  ((body :initarg :body :documentation "body pointer")
   (mass :initarg :mass :documentation "mass pointer")
   (geom :initarg :geom :documentation "geometry pointer"))
  (:default-initargs
   :body (%ode:body-create *world*)
   :mass (claw:alloc '%ode:mass)))

;; FIXME: mass is leaking
(defmethod free ((object physic))
  (with-slots (body geom) object
    (%ode:body-destroy body)
    (%ode:geom-destroy geom)))

(defmethod (setf pos) (value (object physic))
  (%ode:body-set-position (slot-value object 'body)
                          (coerce (x value) 'double-float)
                          (coerce (y value) 'double-float)
                          (coerce (z value) 'double-float))
  (setf (slot-value object 'pos) value))


(claw:defcallback near-callback :void ((data :pointer)
                                       (o1 %ode:geom-id)
                                       (o2 %ode:geom-id))
  (declare (ignore data))
  (let ((b1 (%ode:geom-get-body o1))
        (b2 (%ode:geom-get-body o2)))
    (claw:c-with ((contact %ode:contact :calloc t))
      (setf (contact :surface :mode) (logior %ode:+contact-bounce+
                                             %ode:+contact-soft-cfm+)
            ;; friction parameter
            (contact :surface :mu) 6.9d0
            ;; bounce is the amount of "bouncyness"
            (contact :surface :bounce) 0.9d0
            ;; bounce_vel is the minimum incoming velocity to cause a bounce
            (contact :surface :bounce-vel) 0.1d0
            ;; constraint force mixing parameter
            (contact :surface :soft-cfm) 0.001d0)
      (let ((numc (%ode:collide o1 o2 1 (contact :geom &) (claw:sizeof '%ode:contact))))
        (when (< 0 numc)
          (let ((c (%ode:joint-create-contact *world* *contactgroup* (contact &))))
            (%ode:joint-attach c b1 b2)))))))


(dotimes (i 30)
  (make-physic-sphere :pos (v! (serapeum:random-in-range -10 10)
                               (serapeum:random-in-range 3 10)
                               (serapeum:random-in-range -10 10))))
