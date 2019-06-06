(in-package :incandescent)

;; NO idea how jointgroup is gonna be

(defvar *world* nil)
(defvar *space* nil)
(defvar *contactgroup* nil)

(defclass physic-sphere (actor)
  ((body    :initarg :body    :documentation "body pointer")
   (mass    :initarg :mass    :documentation "mass pointer")
   (geom    :initarg :geom    :documentation "geometry pointer")
   (radius  :initarg :radius  :documentation "double, for mass and geometry")
   (density :initarg :density :documentation "double, for mass"))
  (:default-initargs
   :radius .5d0
   :density 1d0
   :body (%ode:body-create *world*)
   :mass (claw:alloc '%ode:mass)))

(defun ode-init ()
  (unless *world*
    (%ode:init-ode)
    (setf *world* (%ode:world-create))
    (setf *space* (%ode:hash-space-create nil))
    (setf *contactgroup* (%ode:joint-group-create 0))
    (%ode:create-plane *space* 0d0 1d0 0d0 0d0))
  (%ode:world-set-gravity *world* 0d0 -9.8d0 0d0)
  ;; CFM = Constraint Force Mixing
  (%ode:world-set-erp *world* .1d0)
  (%ode:world-set-cfm *world* 1d-5))

(defun ode-destroy ()
  (%ode:joint-group-destroy *contactgroup*)
  (%ode:space-destroy *space*)
  (%ode:world-destroy *world*)
  (%ode:close-ode)
  (setf *world* nil
        *space* nil
        *contactgroup* nil))

;;--------------------------------------------------
;;
;; Constructors
;;
;; FIXME: mass is leaking
(defmethod free ((object physic-sphere))
  (with-slots (body geom) object
    (%ode:body-destroy body)
    (%ode:geom-destroy geom)))

(defmethod (setf pos) (value (object physic-sphere))
  (%ode:body-set-position (slot-value object 'body)
                          (coerce (x value) 'double-float)
                          (coerce (y value) 'double-float)
                          (coerce (z value) 'double-float))
  (setf (slot-value object 'pos) value))

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

;;--------------------------------------------------

(let ((stepper (make-stepper (seconds .01) (seconds .01))))
  (defun ode-update ()
    "updates the objets within the physics engine"
    (when (and *world* (funcall stepper))
      (%ode:space-collide *space* nil (claw:callback 'near-callback))
      (%ode:world-quick-step *world* 0.01d0)
      (%ode:joint-group-empty *contactgroup*))))

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

(claw:defcallback near-callback :void ((data :pointer) (o1 %ode:geom-id) (o2 %ode:geom-id))
  (declare (ignore data))
  (let ((b1 (%ode:geom-get-body o1))
        (b2 (%ode:geom-get-body o2)))
    (claw:c-with ((contact %ode:contact :calloc t))
      (setf (contact :surface :mode) (logior %ode:+contact-bounce+
                                             %ode:+contact-soft-cfm+)
            ;; friction parameter
            (contact :surface :mu) .05d0
            ;; bounce is the amount of "bouncyness"
            (contact :surface :bounce) 0.4d0
            ;; bounce_vel is the minimum incoming velocity to cause a bounce
            (contact :surface :bounce-vel) 0.1d0
            ;; constraint force mixing parameter
            (contact :surface :soft-cfm) 0.001d0)
      (let ((numc (%ode:collide o1 o2 1 (contact :geom &) (claw:sizeof '%ode:contact))))
        (when (< 0 numc)
          (let ((c (%ode:joint-create-contact *world* *contactgroup* (contact &))))
            (%ode:joint-attach c b1 b2)))))))


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
