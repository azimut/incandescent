(in-package #:incandescent)

;; Camera and Actors(TODO) keyboard controls

(defun cam-spin (ang)
  (with-slots (rot) *camera*
    (setf rot (q:normalize
               (q:* rot (q:from-axis-angle (v! 0 0 1) ang))))))

(defun cam-turn (ang)
  (with-slots (rot) *camera*
    (setf rot (q:normalize
               (q:* rot (q:from-axis-angle (v! 0 1 0) ang))))))

(defun cam-tilt (ang)
  (with-slots (rot) *camera*
    (setf rot (q:normalize
               (q:* rot (q:from-axis-angle (v! 1 0 0) ang))))))

(defun god-move (factor dt camera)
  "absolute movement"
  (declare (type fixnum factor)
           (type single-float dt)
           (type camera camera))
  ;; ↑ forward
  (when (keyboard-button (keyboard) key.w)
    (v3:incf (pos camera)
             (v3:*s (q:to-direction (rot camera))
                    (* factor dt))))
  ;; ← left
  (when (keyboard-button (keyboard) key.a)
    (v3:incf (pos camera)
             (v3:*s (q:rotate (v! -1 0 0) (rot camera))
                    (* factor dt))))
  ;; → right
  (when (keyboard-button (keyboard) key.d)
    (v3:incf (pos camera)
             (v3:*s (q:rotate (v! 1 0 0) (rot camera))
                    (* factor dt))))
  ;; ↓ backwards
  (when (keyboard-button (keyboard) key.s)
    (v3:decf (pos camera)
             (v3:*s (q:to-direction (rot camera))
                    (* factor dt))))
  ;; - up - jump
  (when (keyboard-button (keyboard) key.space)
    (v3:decf (pos camera)
             (v3:*s (q:rotate (v! 0 -1 0)
                              (rot camera))
                    (* factor dt))))
  ;; - down - croutch
  (when (keyboard-button (keyboard) key.c)
    (v3:decf (pos camera)
             (v3:*s (q:rotate (v! 0 1 0) (rot camera))
                    (* factor dt)))))

(defmethod control ((camera camera) dt)
  "free camera controls"
  (let ((factor 20))
    ;; MODIFIERS
    ;; - run
    (when (keyboard-button (keyboard) key.lshift)
      (setf factor 30))
    ;; - stealth
    (when (keyboard-button (keyboard) key.lctrl)
      (setf factor 5))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; MOVEMENT
    (god-move factor dt camera)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; SPIN
    (when (key-down-p key.q)
      (cam-spin (radians 1.8f0)))
    (when (key-down-p key.e)
      (cam-spin (radians -1.8f0)))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; PAN
    (when (key-down-p key.left)
      (cam-turn (radians 1.8f0)))
    (when (key-down-p key.right)
      (cam-turn (radians -1.8f0)))
    (when (key-down-p key.up)
      (cam-tilt (radians 1.8f0)))
    (when (key-down-p key.down)
      (cam-tilt (radians -1.8f0))))
  (when (mouse-button (mouse) mouse.left)
    (let ((move (v2:*s (mouse-move (mouse))
                       0.03)))
      (setf (rot camera)
            (q:normalize
             (q:* (rot camera)
                  (q:normalize
                   (q:* (q:from-axis-angle (v! 1 0 0) (- (y move)))
                        (q:from-axis-angle (v! 0 1 0) (- (x move)))))))))))
