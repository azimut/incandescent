(in-package :incandescent)

(defclass physic-box (physic)
  ((density :initform 1d0 :initarg :density)
   (x :initform 1d0 :initarg :x)
   (y :initform 1d0 :initarg :y)
   (z :initform 1d0 :initarg :z)))

(defmethod initialize-instance :after ((obj physic-box) &key)
  (with-slots (mass body geom density x y z pos) obj
    (setf geom (%ode:create-box *space* x y z))
    (claw:c-let ((m %ode:mass :from mass))
      (%ode:mass-set-box (m &) density x y z)
      (%ode:body-set-mass body (m &))
      (%ode:geom-set-body geom body))
    ;; trigger update on ODE side
    (setf (pos obj) pos)))

(defun make-physic-box (&key (pos (v! 0 0 0))
                             (density 1d0)
                             (x 1d0) (y 1d0) (z 1d0))
  (declare (type double-float density x y z))
  (let ((obj (make-instance 'physic-box
                            :pos pos
                            :x x :y y :z z
                            :buf (box (coerce x 'single-float)
                                      (coerce y 'single-float)
                                      (coerce z 'single-float))
                            :density density)))
    (push obj *actors*)
    obj))

(defmethod update ((actor physic-box) dt)
  (when *world*
    (with-slots (pos rot geom) actor
      (claw:c-let ((ode-pos :double :ptr (%ode:geom-get-position geom))
                   (ode-rot :double :ptr (%ode:geom-get-rotation geom)))
        (setf pos (v! (ode-pos 0) (ode-pos 1) (ode-pos 2)))
        (setf rot (q:from-mat3
                   (m! (ode-rot 0) (ode-rot 1) (ode-rot 2)
                       (ode-rot 4) (ode-rot 5) (ode-rot 6)
                       (ode-rot 8) (ode-rot 9) (ode-rot 10))))))))

(defmethod draw ((actor physic-box) camera (time single-float))
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
