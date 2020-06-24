(in-package :incandescent)

(defclass physic-newcyl (physic)
  ((data         :initarg :data)
   (radius       :initarg :radius)
   (height       :initarg :height)
   (ode-indices  :initarg :ode-indices)
   (ode-vertices :initarg :ode-vertices))
  (:default-initargs
   :data nil
   :radius 0.5f0
   :height 1.0f0
   :ode-indices nil
   :ode-vertices nil))

(defmethod free ((object physic-newcyl))
  (with-slots (body data geom orot ode-vertices ode-indices) object
    (when body (%ode:body-destroy body))
    (when geom (%ode:geom-destroy geom))
    (claw:free orot)
    (claw:free ode-vertices)
    (claw:free ode-indices)
    (claw:free data)))

(defmethod initialize-instance :after ((obj physic-newcyl) &key)
  (with-slots (pos rot) obj
    (physic-to-ode  obj)
    (ode-update-pos obj pos)
    (ode-update-rot obj rot)))

(defun make-physic-newcyl (&key (pos (v! 0 0 0))
                                (rot (q:identity))
                                (color (v! 1 1 1))
                                (density 1f0)
                                (radius .5f0)
                                (height 1f0)
                                immovablep)
  (declare (type single-float density radius height))
  (let ((obj (make-instance 'physic-newcyl
                            :pos pos :rot rot
                            :color color
                            :immovablep immovablep
                            :radius radius
                            :height height
                            :buf (cylinder radius height)
                            :density density)))
    (push obj *actors*)
    obj))
