(in-package :incandescent)

;;; ?? NOT WORKING mass is weird

(defclass physic-cylinder (physic)
  ((density :initarg :density)
   (radius  :initarg :radius)
   (height  :initarg :height))
  (:default-initargs
   :density 1d0
   :radius .5d0
   :height 1d0))

(defmethod initialize-instance :after ((obj physic-cylinder) &key)
  (with-slots (mass body geom density pos rot radius height) obj
    (setf geom (%ode:create-cylinder *space* radius height))
    (claw:c-let ((m %ode:mass :from mass))
      (%ode:mass-set-cylinder (m &) density 1 radius height)
      ;;(%ode:mass-set-sphere (m &) density radius)
      (%ode:body-set-mass body (m &))
      (%ode:geom-set-body geom body))
    (ode-update-pos obj pos)
    (ode-update-rot obj rot)))

(defun make-physic-cylinder (&key (pos (v! 0 0 0))
                                  (rot (q:identity))
                                  (density 1d0)
                                  (radius .5d0)
                                  (height 1d0))
  (declare (type double-float density radius height))
  (let ((obj (make-instance 'physic-cylinder
                            :pos pos
                            :rot rot
                            :radius radius
                            :height height
                            :buf (cylinder (coerce radius 'single-float)
                                           (coerce height 'single-float))
                            :density density)))
    (push obj *actors*)
    obj))
