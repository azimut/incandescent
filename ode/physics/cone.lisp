(in-package :incandescent)

;; Note: trimesh cone

(defclass physic-cone (physic)
  ((data         :initarg :data)
   (radius       :initarg :radius)
   (height       :initarg :height)
   (ode-indices  :initarg :ode-indices)
   (ode-vertices :initarg :ode-vertices))
  (:default-initargs
   :data nil
   :radius 0.5d0
   :height 1.0d0
   :ode-indices nil
   :ode-vertices nil))

(defmethod free ((object physic-cone))
  (with-slots (body geom orot data ode-vertices ode-indices) object
    (when body (%ode:body-destroy body))
    (when geom (%ode:geom-destroy geom))
    (claw:free orot)
    (claw:free data)
    (claw:free ode-indices)
    (claw:free ode-vertices)))

(defmethod initialize-instance :after ((obj physic-cone) &key)
  (with-slots (pos rot) obj
    (physic-to-ode  obj)
    (ode-update-pos obj pos)
    (ode-update-rot obj rot)))

(defun make-physic-cone (&key (pos (v! 0 0 0))
                              (rot (q:identity))
                              (color (v! 1 1 1))
                              (density 1.0d0)
                              (radius  0.5d0)
                              (height  1.0d0)
                              immovablep)
  (declare (type double-float density radius height)
           (type boolean immovablep))
  (let ((obj (make-instance 'physic-cone
                            :pos pos :rot rot
                            :color color
                            :immovablep immovablep
                            :radius radius
                            :height height
                            :buf (cone (coerce radius 'single-float)
                                       (coerce height 'single-float))
                            :density density)))
    (push obj *actors*)
    obj))

