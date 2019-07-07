(in-package :incandescent)

(defclass physic-newcyl (physic)
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
                                (density 1d0)
                                (radius .5d0)
                                (height 1d0)
                                immovablep)
  (declare (type double-float density radius height))
  (let ((obj (make-instance 'physic-newcyl
                            :pos pos :rot rot
                            :color color
                            :immovablep immovablep
                            :radius radius
                            :height height
                            :buf (cylinder (coerce radius 'single-float)
                                           (coerce height 'single-float))
                            :density density)))
    (push obj *actors*)
    obj))

(defmethod update ((actor physic-newcyl) dt)
  (when *world*
    (with-slots (pos rot orot geom immovablep) actor
      (unless immovablep
        (setf pos (ode-geom-get-position geom))
        (setf rot (ode-geom-get-quaternion2 orot geom))))))

(defmethod draw ((actor physic-newcyl) camera (time single-float))
  (with-slots (buf scale color) actor
    (map-g #'generic-pipe buf
           :scale scale
           :color color
           ;; :shadowmap *shadow-sam*
           ;; :light-world (world->view *shadow-camera*)
           ;; :light-clip  (projection *shadow-camera*)
           :cam-pos (pos camera)
           :model-world (model->world actor)
           :world-view  (world->view camera)
           :view-clip   (projection  camera)
           ;; Directional light (for the most part)
           :light-color *light-color*
           :light-pos   *light-pos*)))
