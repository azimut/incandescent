(in-package :incandescent)

(defclass physic-sphere (actor physic)
  ((radius  :initarg :radius  :documentation "double, for mass and geometry")
   (density :initarg :density :documentation "double, for mass"))
  (:default-initargs
   :radius .5d0
   :density 1d0))

(defmethod initialize-instance :after ((obj physic-sphere) &key)
  (with-slots (mass body geom density radius pos) obj
    (setf geom (%ode:create-sphere *space* radius))
    (claw:c-let ((m %ode:mass :from mass))
      (%ode:mass-set-sphere (m &) density radius)
      (%ode:body-set-mass body (m &))
      (%ode:geom-set-body geom body))
    ;; trigger update on ODE side
    (setf (pos obj) pos)))

(defun make-physic-sphere (&key (pos (v! 0 0 0))
                                (density 1d0)
                                (radius .5d0))
  (declare (type double-float radius density))
  (let ((obj (make-instance 'physic-sphere
                            :pos pos
                            :buf (sphere (coerce radius 'single-float))
                            :radius radius :density density)))
    (push obj *actors*)
    obj))

(defmethod update ((actor physic-sphere) dt)
  (when *world*
    (with-slots (pos rot geom) actor
      (claw:c-let ((ode-pos :double :ptr (%ode:geom-get-position geom))
                   (ode-rot :double :ptr (%ode:geom-get-rotation geom)))
        (setf pos (v! (ode-pos 0) (ode-pos 1) (ode-pos 2)))
        (setf rot (q:from-mat3
                   (m! (ode-rot 0) (ode-rot 1) (ode-rot 2)
                       (ode-rot 4) (ode-rot 5) (ode-rot 6)
                       (ode-rot 8) (ode-rot 9) (ode-rot 10))))))))

(defmethod draw ((actor physic-sphere) camera (time single-float))
  (with-slots (buf scale color) actor
    (map-g #'generic-pipe buf
           :scale scale
           :color color
           :time  time
           :cam-pos (pos camera)
           :model-world (model->world actor)
           :world-view  (world->view camera)
           :view-clip   (projection  camera)
           ;; Directional light (for the most part)
           :light-color *light-color*
           :light-pos   *light-pos*
           :brdf-luf *s-brdf*
           :irradiance-map *s-cubemap-live*
           :prefilter-map *s-cubemap-prefilter*)))
