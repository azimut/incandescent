(in-package :incandescent)

(defclass physic-box (physic)
  ((density :initform 1d0 :initarg :density)
   (x       :initform 1d0 :initarg :x)
   (y       :initform 1d0 :initarg :y)
   (z       :initform 1d0 :initarg :z)))

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
                             (density 1d0)
                             (x 1d0) (y 1d0) (z 1d0)
                             immovablep)
  (declare (type double-float density x y z)
           (type boolean immovablep))
  (let ((obj (make-instance 'physic-box
                            :pos pos
                            :rot rot
                            :x x :y y :z z
                            :immovablep immovablep
                            :buf (box (coerce x 'single-float)
                                      (coerce y 'single-float)
                                      (coerce z 'single-float))
                            :density density)))
    (push obj *actors*)
    obj))

(defmethod update ((actor physic-box) dt)
  "updates visual representation from ODE value"
  (when *world*
    (with-slots (pos rot orot geom immovablep) actor
      (unless immovablep
        (setf pos (ode-geom-get-position geom))
        (setf rot (ode-geom-get-quaternion2 orot geom))))))

(defmethod draw ((actor physic-box) camera (time single-float))
  (with-slots (buf scale color) actor
    (map-g #'generic-pipe buf
           :scale scale
           :color color
           ;;:shadowmap *shadow-sam*
           ;; :light-world (world->view *shadow-camera*)
           ;; :light-clip  (projection *shadow-camera*)
           :cam-pos (pos camera)
           :model-world (model->world actor)
           :world-view  (world->view camera)
           :view-clip   (projection  camera)
           ;; Directional light (for the most part)
           :light-color *light-color*
           :light-pos   *light-pos*)))
