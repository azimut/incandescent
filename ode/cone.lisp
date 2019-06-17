(in-package :incandescent)

;; Note: static cone

(defclass physic-cone (physic)
  ((density      :initarg :density)
   (radius       :initarg :radius)
   (height       :initarg :height)
   (data         :initarg :data)
   (ode-vertices :initarg :ode-vertices)
   (ode-indices  :initarg :ode-indices))
  (:default-initargs
   :data nil
   :geom nil
   :ode-indices nil
   :ode-vertices nil
   :density 1d0
   :radius .5d0
   :height 1d0))

(defmethod free ((object physic-cone))
  (with-slots (body data geom orot ode-vertices ode-indices) object
    (when body (%ode:body-destroy body))
    (when geom (%ode:geom-destroy geom))
    (claw:free orot)
    (claw:free ode-vertices)
    (claw:free ode-indices)
    (claw:free data)))

(defmethod initialize-instance :after ((obj physic-cone) &key)
  (with-slots (mass body geom
               data
               buf
               pos rot
               ode-vertices ode-indices
               density radius height)
      obj
    (multiple-value-bind (v i d g) (buffer-strem-to-ode buf)
      (setf ode-vertices v
            ode-indices  i
            data         d
            geom         g))
    (ode-update-pos obj pos)
    (ode-update-rot obj rot)))

(defun make-physic-cone (&key (pos (v! 0 0 0))
                              (rot (q:identity))
                              (density 1d0)
                              (radius .5d0)
                              (height 1d0))
  (declare (type double-float density radius height))
  (let ((obj (make-instance 'physic-cone
                            :pos pos
                            :rot rot
                            :radius radius
                            :height height
                            :buf (cone (coerce radius 'single-float)
                                       (coerce height 'single-float))
                            :density density)))
    (push obj *actors*)
    obj))

(defmethod update ((actor physic-cone) dt)
  #+nil
  (when *world*
    (with-slots (pos rot orot geom) actor
      (setf pos (ode-geom-get-position geom))
      (setf rot (ode-geom-get-quaternion2 orot geom)))))

(defmethod draw ((actor physic-cone) camera (time single-float))
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
