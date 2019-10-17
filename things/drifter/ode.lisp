(in-package #:incandescent)

;; Override callback

(progn
  (block gg
    (claw:defcallback near-callback :void ((data :pointer)
                                           (o1 %ode:geom-id)
                                           (o2 %ode:geom-id))
      (declare (ignore data))
      (let ((b1 (%ode:geom-get-body o1))
            (b2 (%ode:geom-get-body o2)))
        (when (and b1 b2 (plusp (%ode:are-connected-excluding b1 b2 %ode:+joint-type-contact+)))
          (return-from gg))
        (claw:c-with ((contact %ode:contact :calloc t :count 5))
          (dotimes (i 5)
            (setf (contact i :surface :mode) (logior %ode:+contact-bounce+
                                                     %ode:+contact-slip1+
                                                     ;; %ode:+contact-slip2+
                                                     ;; %ode:+contact-approx1+
                                                     ;; %ode:+contact-soft-erp+
                                                     %ode:+contact-soft-cfm+)
                  (contact i :surface :slip1) .7d0
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
          (let ((numc (%ode:collide o1 o2 10
                                    (contact :geom &)
                                    (claw:sizeof '%ode:contact))))
            (when (plusp numc)
              (when (and b1 b2)
                (collide (pointer-to-actor b1) (pointer-to-actor b2)))

              (dotimes (i numc)
                (%ode:joint-attach (%ode:joint-create-contact *world*
                                                              *contactgroup*
                                                              (contact i))
                                   b1 b2))))))))
  (let ((stepper (make-stepper (seconds .01) (seconds .01))))
    (defun ode-update ()
      "updates the objets within the physics engine"
      (when (and *world* (funcall stepper))
        (%ode:space-collide *space* nil (claw:callback 'near-callback))
        (%ode:world-quick-step *world* 0.0099d0)
        (%ode:joint-group-empty *contactgroup*)))))
