(in-package :incandescent)

(defclass physic-sphere (physic)
  ((radius  :initarg :radius :documentation "single, for mass and geometry"))
  (:default-initargs
   :radius .5f0))

(defmethod initialize-instance :after ((obj physic-sphere) &key)
  (with-slots (mass body geom density radius pos rot immovablep) obj
    (setf geom (%ode:create-sphere *space* radius))
    (unless immovablep
      (claw:c-let ((m %ode:mass :from mass))
        (%ode:mass-set-sphere (m &) density radius)
        (%ode:body-set-mass body (m &))
        (%ode:geom-set-body geom body)))
    (ode-update-pos obj pos)
    (ode-update-rot obj rot)))

(defun make-physic-sphere (&key (pos (v! 0 0 0))
                                (rot (q:identity))
                                (color (v! 1 1 1))
                                (density 1f0)
                                (radius .5f0)
                                immovablep)
  (declare (type single-float radius density)
           (type boolean immovablep))
  (let ((obj (make-instance 'physic-sphere
                            :pos pos
                            :rot rot
                            :color color
                            :immovablep immovablep
                            :buf (sphere radius)
                            :radius radius :density density)))
    (push obj *actors*)
    obj))
