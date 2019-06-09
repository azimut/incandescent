(in-package :incandescent)

;; NO idea how jointgroup is gonna be

(defvar *world* nil)
(defvar *space* nil)
(defvar *contactgroup* nil)

(defun ode-init ()
  (unless *world*
    (%ode:init-ode)
    (setf *world* (%ode:world-create))
    (setf *space* (%ode:hash-space-create nil))
    (setf *contactgroup* (%ode:joint-group-create 0))
    (%ode:create-plane *space* 0d0 1d0 0d0 0d0))
  (%ode:world-set-gravity *world* 0d0 -9.8d0 0d0)
  ;; CFM = Constraint Force Mixing
  ;; changes stiffness of a joint.
  (%ode:world-set-cfm *world* 1d-5)
  ;; ERP = Error Reduction Parameter = [0,1]
  ;; corrects  joint error
  (%ode:world-set-erp *world* .1d0)
  (%ode:world-set-auto-disable-flag *world* 1)
  (%ode:world-set-quick-step-w *world* 1.3d0)
  (%ode:world-set-quick-step-num-iterations *world* 40))

(defun ode-destroy ()
  (%ode:joint-group-destroy *contactgroup*)
  (%ode:space-destroy *space*)
  (%ode:world-destroy *world*)
  (%ode:close-ode)
  (setf *world* nil
        *space* nil
        *contactgroup* nil))

(progn (claw:defcallback near-callback :void ((data :pointer)
                                              (o1 %ode:geom-id)
                                              (o2 %ode:geom-id))
         (declare (ignore data))
         (let ((b1 (%ode:geom-get-body o1))
               (b2 (%ode:geom-get-body o2)))
           (claw:c-with ((contact %ode:contact :calloc t))
             (setf (contact :surface :mode) (logior
                                             ;;%ode:+contact-bounce+
                                             %ode:+contact-approx1+
                                             %ode:+contact-soft-erp+
                                             %ode:+contact-soft-cfm+)
                   (contact :surface :soft-erp) .1d0
                   ;; friction parameter
                   (contact :surface :mu) 10d0
                   ;; bounce is the amount of "bouncyness"
                   (contact :surface :bounce) .001d0
                   ;; bounce_vel is the minimum incoming velocity to cause a bounce
                   (contact :surface :bounce-vel) 10d0
                   ;; constraint force mixing parameter
                   (contact :surface :soft-cfm) #.(coerce 1e-10 'double-float))
             (let ((numc (%ode:collide o1 o2 1 (contact :geom &) (claw:sizeof '%ode:contact))))
               (when (< 0 numc)
                 (let ((c (%ode:joint-create-contact *world* *contactgroup* (contact &))))
                   (%ode:joint-attach c b1 b2)))))))
       (let ((stepper (make-stepper (seconds .01) (seconds .01))))
         (defun ode-update ()
           "updates the objets within the physics engine"
           (when (and *world* (funcall stepper))
             (%ode:space-collide *space* nil (claw:callback 'near-callback))
             (%ode:world-quick-step *world* 0.001d0)
             (%ode:joint-group-empty *contactgroup*)))))

(defun ode-geom-get-position (geom)
  (declare (type sb-sys:system-area-pointer geom))
  (claw:c-let ((ode-pos :double :ptr (%ode:geom-get-position geom)))
    (v! (ode-pos 0) (ode-pos 1) (ode-pos 2))))

(defun ode-geom-get-quaternion (geom)
  (declare (type sb-sys:system-area-pointer geom))
  (claw:c-let ((ode-rot :double :ptr (%ode:geom-get-rotation geom)))
    (q:from-mat3
     (m! (ode-rot 0) (ode-rot 1) (ode-rot 2)
         (ode-rot 4) (ode-rot 5) (ode-rot 6)
         (ode-rot 8) (ode-rot 9) (ode-rot 10)))))
