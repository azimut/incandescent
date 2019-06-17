(in-package #:incandescent)

(defclass physic (actor)
  ((body :initarg :body :reader body :documentation "body pointer")
   (mass :initarg :mass :reader mass :documentation "mass pointer")
   (geom :initarg :geom :reader geom :documentation "geometry pointer")
   (immovablep :initarg :immovablep
               :documentation "ode immovable object, aka without body")
   (orot :initarg :orot))
  (:default-initargs
   :immovablep nil
   :body (%ode:body-create *world*)
   :mass (claw:alloc '%ode:mass)
   :orot (claw:alloc '%ode:real 4)))

;; FIXME: mass is leaking
(defmethod free ((object physic))
  (with-slots (body geom orot) object
    (%ode:body-destroy body)
    (%ode:geom-destroy geom)
    (claw:free orot)))

(defun ode-update-rot (physic q)
  "Ideally only called once at initialization"
  (declare (type rtg-math.types:quaternion q))
  (with-slots (geom orot) physic
    (claw:c-let ((quat %ode:real :ptr orot))
      (setf (quat 0) (coerce (x q) 'double-float))
      (setf (quat 1) (coerce (y q) 'double-float))
      (setf (quat 2) (coerce (z q) 'double-float))
      (setf (quat 3) (coerce (w q) 'double-float)))
    (%ode:geom-set-quaternion geom orot)))

(defun ode-update-pos (physic v)
  "Ideally only called once at initialization"
  (declare (type rtg-math.types:vec3 v))
  (%ode:geom-set-position (slot-value physic 'geom)
                          (coerce (x v) 'double-float)
                          (coerce (y v) 'double-float)
                          (coerce (z v) 'double-float)))
