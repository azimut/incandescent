(in-package :incandescent)

(defclass physic-box (physic)
  ((x :initform 1f0 :initarg :x)
   (y :initform 1f0 :initarg :y)
   (z :initform 1f0 :initarg :z)))

(defmethod initialize-instance :after ((obj physic-box) &key)
  (with-slots (mass body geom density immovablep x y z pos rot) obj
    (setf geom (%ode:create-box *space* x y z))
    (unless immovablep
      (claw:c-let ((m %ode:mass :from mass))
        (%ode:mass-set-box (m &) density x y z)
        (%ode:body-set-mass body (m &))
        (%ode:geom-set-body geom body)))
    (ode-update-pos obj pos)
    (ode-update-rot obj rot)))

(defun make-physic-box (&key (pos (v! 0 0 0))
                             (rot (q:identity))
                             (color (v! 1 1 1))
                             (density 1f0)
                             (x 1f0) (y 1f0) (z 1f0)
                             (draw-p t)
                             immovablep)
  (declare (type single-float density x y z)
           (type boolean immovablep draw-p))
  (let ((obj (make-instance 'physic-box
                            :pos pos
                            :rot rot
                            :color color
                            :draw-p draw-p
                            :x x :y y :z z
                            :immovablep immovablep
                            :buf (box x y z)
                            :density density)))
    (push obj *actors*)
    obj))
