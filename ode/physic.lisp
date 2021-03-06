(in-package #:incandescent)

(defvar *body-to-actor* (list)
  "associative list between body pointer and lisp object")

(defclass physic (actor)
  ((body       :initarg :body :reader body :documentation "body pointer")
   (mass       :initarg :mass :reader mass :documentation "mass pointer")
   (geom       :initarg :geom :reader geom :documentation "geometry pointer") ;; RM?
   (orot       :initarg :orot              :documentation "ODE rotation pointer")
   (density    :initarg :density)
   (immovablep :initarg :immovablep
               :documentation "ode immovable object (aka without body or mass) but that still interacts with other objects"))
  (:default-initargs
   :density 1.0f0
   :body (%ode:body-create *world*)
   :mass (claw:alloc '%ode:mass)
   :orot (claw:alloc '%ode:real 4)
   :immovablep nil))

;; FIXME: mass is leaking?
(defmethod free :around ((object physic))
  (with-slots (body geom orot) object
    (%ode:body-destroy body)
    (%ode:geom-destroy geom)
    (claw:free orot)
    (alexandria:removef *body-to-actor* body
                        :key #'car :test #'sb-sys:sap=))
  (call-next-method))

(defmethod initialize-instance :after ((obj physic) &key)
  (push (list (slot-value obj 'body) obj) *body-to-actor*))

(defun pointer-to-actor (pointer)
  (serapeum:assocadr pointer *body-to-actor* :test #'sb-sys:sap=))

(defun update-ode-rot (orot qrot)
  "set the ODE rotation to the QROT rtg-math quaternion"
  (declare (type rtg-math.types:quaternion qrot))
  (claw:c-let ((ode-rot %ode:real :ptr orot))
    (setf (ode-rot 0) (x qrot))
    (setf (ode-rot 1) (y qrot))
    (setf (ode-rot 2) (z qrot))
    (setf (ode-rot 3) (w qrot))))

(defun ode-update-rot (physic q)
  "Ideally only called once at initialization"
  (declare (type rtg-math.types:quaternion q))
  (with-slots (geom orot) physic
    (update-ode-rot orot q)
    (%ode:geom-set-quaternion geom orot)))

(defun ode-update-pos (physic v)
  "Ideally only called once at initialization"
  (declare (type rtg-math.types:vec3 v))
  (%ode:geom-set-position (slot-value physic 'geom) (x v) (y v) (z v)))

