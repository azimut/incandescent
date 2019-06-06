(in-package #:incandescent)

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
  (let ((radius .5d0)
        (density 1d0))
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
                          (coerce (x value) 'double-float)
                          (coerce (y value) 'double-float)
                          (coerce (z value) 'double-float))
  (setf (slot-value object 'pos) value))

(defmethod update :before ((camera physic-camera) dt)
  (with-slots (pos) camera
    (%ode:body-set-position (slot-value camera 'body)
                            (coerce (x pos) 'double-float)
                            (coerce (y pos) 'double-float)
                            (coerce (z pos) 'double-float))))

(defun get-solid-camera ()
  (or *camera-solid*
      (setf *camera-solid*
            (make-instance 'physic-camera
                           :pos (pos *currentcamera*)
                           :rot (rot *currentcamera*)))))

(defun toggle-solid-camera ()
  (cond ((not *camera-solid*)
         (setf (pos (get-solid-camera)) (copy-seq (pos *currentcamera*))
               (rot (get-solid-camera)) (copy-seq (rot *currentcamera*))
               *currentcamera* (get-solid-camera)))
        ((equal *currentcamera* *camera-solid*)
         (setf (pos *camera*) (copy-seq (pos *camera-solid*))
               (rot *camera*) (copy-seq (rot *camera-solid*))
               *currentcamera* *camera*))
        ((not (equal *currentcamera* *camera-solid*))
         (setf (pos (get-solid-camera)) (copy-seq (pos *currentcamera*))
               (rot (get-solid-camera)) (copy-seq (rot *currentcamera*))
               *currentcamera* (get-solid-camera)))))
