(in-package :incandescent)

(defclass physic-sphere (physic)
  ((radius  :initarg :radius  :documentation "double, for mass and geometry"))
  (:default-initargs
   :radius .5d0))

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
                                (density 1d0)
                                (radius .5d0)
                                immovablep)
  (declare (type double-float radius density)
           (type boolean immovablep))
  (let ((obj (make-instance 'physic-sphere
                            :pos pos
                            :rot rot
                            :color color
                            :immovablep immovablep
                            :buf (sphere (coerce radius 'single-float))
                            :radius radius :density density)))
    (push obj *actors*)
    obj))

(defmethod update :around ((actor physic-sphere) dt)
  "updates visual representation from ODE value"
  (when *world*
    (with-slots (pos orot rot geom body immovablep) actor
      (unless immovablep
        (setf pos (ode-geom-get-position    geom))
        (setf rot (ode-geom-get-quaternion2 orot geom)))))
  (call-next-method))

(defmethod draw ((actor physic-sphere) camera (time single-float))
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
