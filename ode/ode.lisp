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
  (%ode:world-set-cfm *world* 1d-5)
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

(let ((stepper (make-stepper (seconds .01) (seconds .01))))
  (defun ode-update ()
    "updates the objets within the physics engine"
    (when (and *world* (funcall stepper))
      (%ode:space-collide *space* nil (claw:callback 'near-callback))
      (%ode:world-quick-step *world* 0.01d0)
      (%ode:joint-group-empty *contactgroup*))))
