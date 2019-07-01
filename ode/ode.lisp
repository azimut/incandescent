(in-package :incandescent)

;; NO idea how jointgroup is gonna be

(defvar *world* nil)
(defvar *space* nil)
(defvar *contactgroup* nil)

(defun ode-init ()
  (unless *world*
    (%ode:init-ode)
    (setf *world*        (%ode:world-create)
          *space*        (%ode:hash-space-create nil)
          *contactgroup* (%ode:joint-group-create 0))
    (%ode:create-plane *space* 0d0 1d0 0d0 0d0))
  (%ode:world-set-gravity *world* 0d0 -9.8d0 0d0)
  ;; CFM = Constraint Force Mixing
  ;; changes stiffness of a joint.
  (%ode:world-set-cfm                       *world* 1d-5)
  ;; ERP = Error Reduction Parameter = [0,1]
  ;; corrects  joint error
  (%ode:world-set-erp                       *world* .1d0)
  (%ode:world-set-auto-disable-flag         *world* 1)
  (%ode:world-set-quick-step-w              *world* 1.3d0)
  (%ode:world-set-quick-step-num-iterations *world* 40)
  t)

(defun ode-destroy ()
  (%ode:joint-group-destroy *contactgroup*)
  (%ode:space-destroy *space*)
  (%ode:world-destroy *world*)
  (%ode:close-ode)
  (setf *world* nil
        *space* nil
        *contactgroup* nil))

(progn
  (block nn
    (claw:defcallback near-callback :void ((data :pointer)
                                           (o1 %ode:geom-id)
                                           (o2 %ode:geom-id))
      (declare (ignore data))
      (let ((b1 (%ode:geom-get-body o1))
            (b2 (%ode:geom-get-body o2)))
        (when (and b1 b2 (plusp (%ode:are-connected-excluding b1 b2 %ode:+joint-type-contact+)))
          (return-from nn))
        (claw:c-with ((contact %ode:contact :calloc t :count 6))
          (dotimes (i 5)
            (setf (contact i :surface :mode) (logior %ode:+contact-bounce+
                                                     ;; %ode:+contact-slip1+
                                                     ;; %ode:+contact-slip2+
                                                     ;; %ode:+contact-approx1+
                                                     ;; %ode:+contact-soft-erp+
                                                     %ode:+contact-soft-cfm+)
                  ;; (contact i :surface :slip1) .7d0
                  ;; (contact i :surface :slip2) .7d0
                  ;;(contact i :surface :soft-erp) .96d0
                  ;; friction parameter
                  (contact i :surface :mu) ode:+infinity+
                  (contact i :surface :mu2) 0d0
                  ;; bounce is the amount of "bouncyness"
                  (contact i :surface :bounce) .1d0
                  ;; bounce_vel is the minimum incoming velocity to cause a bounce
                  (contact i :surface :bounce-vel) .1d0
                  ;; constraint force mixing parameter
                  (contact i :surface :soft-cfm) .01d0))
          (dotimes (i (%ode:collide o1 o2 6
                                    (contact :geom &)
                                    (claw:sizeof '%ode:contact)))
            (%ode:joint-attach (%ode:joint-create-contact *world*
                                                          *contactgroup*
                                                          (contact i))
                               b1 b2))))))
  (let ((stepper (make-stepper (seconds .01) (seconds .01))))
    (defun ode-update ()
      "updates the objets within the physics engine"
      (when (and *world* (funcall stepper))
        (%ode:space-collide *space* nil (claw:callback 'near-callback))
        (%ode:world-quick-step *world* 0.005d0)
        (%ode:joint-group-empty *contactgroup*)))))

;; FIME: leaking? c-with would free it...
(defun ode-geom-get-position (geom)
  (declare (type sb-sys:system-area-pointer geom))
  (claw:c-let ((ode-pos :double :ptr (%ode:geom-get-position geom)))
    (v! (ode-pos 0) (ode-pos 1) (ode-pos 2))))

(defun ode-geom-get-quaternion2 (orot geom)
  "returns a new rtg-math quaternion from the current rotation in the ODE geometry. Using the already C allocated OROT quaternion passed"
  (declare (type sb-sys:system-area-pointer orot geom))
  (%ode:geom-get-quaternion geom orot)
  (claw:c-let ((ode-rot :double :ptr orot))
    (q! (coerce (ode-rot 0) 'single-float)
        (coerce (ode-rot 1) 'single-float)
        (coerce (ode-rot 2) 'single-float)
        (coerce (ode-rot 3) 'single-float))))

(defun buffer-stream-to-ode (buf)
  "creates a new trimesh geometry on ODE from a cepl buffer stream"
  (destructuring-bind ((gv) gi) (buffer-stream-gpu-arrays buf)
    (let ((gvl (car (gpu-array-dimensions gv)))
          (gil (car (gpu-array-dimensions gi))))
      (claw:c-let ((vertices :float :calloc t :count (* 3 gvl))
                   (indices  :unsigned-int :calloc t :count gil)
                   (mesh-data %ode::tri-mesh-data-id
                              :ptr (%ode:geom-tri-mesh-data-create)))
        ;;
        (with-gpu-array-as-c-array (ci gi)
          (loop :for i :below gil
                :do (setf (indices i) (aref-c ci i))))
        ;;
        (with-gpu-array-as-c-array (cv gv)
          (loop :for i :below gvl
                :for ovx :by 3
                :for ovy := (+ ovx 1)
                :for ovz := (+ ovx 2)
                :do (setf (vertices ovx) (x (pos (aref-c cv i))))
                    (setf (vertices ovy) (y (pos (aref-c cv i))))
                    (setf (vertices ovz) (z (pos (aref-c cv i))))))
        ;;
        (%ode:geom-tri-mesh-data-build-single (mesh-data &)
                                              (vertices &)
                                              (* 3 (claw:sizeof :float))
                                              gvl
                                              (indices &)
                                              gil
                                              (* 3 (claw:sizeof :unsigned-int)))
        (values (vertices &)
                (indices &)
                (mesh-data &)
                (%ode:create-tri-mesh *space* (mesh-data &) 0 0 0))))))

(defun physic-to-ode (physic)
  (with-slots (buf
               pos
               ode-vertices ode-indices data geom
               mass density immovablep
               body)
      physic
    (multiple-value-bind (v i d g) (buffer-stream-to-ode buf)
      (setf ode-vertices v
            ode-indices  i
            data         d
            geom         g))
    (unless immovablep
      (claw:c-let ((m %ode:mass :ptr mass))
        (%ode:geom-set-data     geom data)
        (%ode:mass-set-trimesh  mass density geom)
        (%ode:geom-set-position geom (- (m :c 0)) (- (m :c 1)) (- (m :c 2)))
        (%ode:mass-translate    mass (- (m :c 0)) (- (m :c 1)) (- (m :c 2)))
        ;;
        (%ode:geom-set-body geom body)
        (%ode:body-set-mass body mass)))))
