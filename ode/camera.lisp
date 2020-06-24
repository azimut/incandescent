(in-package #:incandescent)

;; Attach physics to a camera...

(defvar *camera-solid* nil)

(defclass physic-camera (pers)
  ((body    :initarg :body :documentation "body pointer")
   (mass    :initarg :mass :documentation "mass pointer")
   (geom    :initarg :geom :documentation "geometry pointer"))
  (:default-initargs
   :body (%ode:body-create *world*)
   :mass (claw:alloc '%ode:mass)))

;; FIXME: mass is leaking
(defmethod free ((object physic-camera))
  (with-slots (body geom) object
    (%ode:body-destroy body)
    (%ode:geom-destroy geom)))

(defmethod initialize-instance :after ((obj physic-camera) &key)
  (let ((radius .5f0)
        (density 1f0))
    (with-slots (mass body geom pos) obj
      (setf geom (%ode:create-sphere *space* radius))
      (claw:c-let ((m %ode:mass :from mass))
        (%ode:mass-set-sphere (m &) density radius)
        (%ode:body-set-mass body (m &))
        (%ode:geom-set-body geom body))
      ;; trigger update on ODE side
      (setf (pos obj) pos))))

(defmethod (setf pos) (value (object physic-camera))
  (%ode:body-set-position (slot-value object 'body)
                          (x value)
                          (y value)
                          (z value))
  (setf (slot-value object 'pos) value))

(defmethod update :before ((camera physic-camera) dt)
  (with-slots (pos) camera
    (%ode:body-set-position (slot-value camera 'body)
                            (x pos)
                            (y pos)
                            (z pos))))

(defun toggle-solid-camera ()
  (if (eq 'physic-camera (serapeum:class-name-of *currentcamera*))
      (progn
        (camera-to-camera *currentcamera* *camera*)
        (setf *currentcamera* *camera*)
        (free *camera-solid*)
        (setf *camera-solid* nil))
      (progn
        (setf *camera-solid* (make-instance 'physic-camera))
        (camera-to-camera *currentcamera* *camera-solid*)
        (setf *currentcamera* *camera-solid*)))
  *currentcamera*)
