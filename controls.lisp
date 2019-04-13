(in-package #:incandescent)

;; Camera and Actors(TODO) keyboard controls

(defvar *vec3-right*   (v!  1  0  0))
(defvar *vec3-left*    (v! -1  0  0))
(defvar *vec3-up*      (v!  0  1  0))
(defvar *vec3-down*    (v!  0 -1  0))
(defvar *vec3-forward* (v!  0  0  1))
(defvar *vec3-back*    (v!  0  0 -1))

(defun cam-spin (ang)
  (with-slots (rot) *camera*
    (setf rot (q:normalize
               (q:* rot (q:from-axis-angle *vec3-forward* ang))))))

(defun cam-turn (ang)
  (with-slots (rot) *camera*
    (setf rot (q:normalize
               (q:* rot (q:from-axis-angle *vec3-up* ang))))))

(defun cam-tilt (ang)
  (with-slots (rot) *camera*
    (setf rot (q:normalize
               (q:* rot (q:from-axis-angle *vec3-right* ang))))))

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
             (v3:*s (q:rotate *vec3-left* (rot camera))
                    (* factor dt))))
  ;; → right
  (when (keyboard-button (keyboard) key.d)
    (v3:incf (pos camera)
             (v3:*s (q:rotate *vec3-right* (rot camera))
                    (* factor dt))))
  ;; ↓ backwards
  (when (keyboard-button (keyboard) key.s)
    (v3:decf (pos camera)
             (v3:*s (q:to-direction (rot camera))
                    (* factor dt))))
  ;; - up - jump
  (when (keyboard-button (keyboard) key.space)
    (v3:decf (pos camera)
             (v3:*s (q:rotate *vec3-down* (rot camera))
                    (* factor dt))))
  ;; - down - croutch
  (when (keyboard-button (keyboard) key.c)
    (v3:decf (pos camera)
             (v3:*s (q:rotate *vec3-up* (rot camera))
                    (* factor dt)))))


(defun human-move (factor dt camera)
  "absolute movement"
  (declare (type fixnum factor)
           (type single-float dt)
           (type camera camera))
  ;; ↑ forward
  (when (keyboard-button (keyboard) key.w)
    (let* ((camdir (q:to-direction (rot camera)))
           (dt-pos (v3:*s camdir (* factor dt)))
           (dt-pos (v! (x dt-pos) 0 (z dt-pos))))
      (v3:incf (pos camera) dt-pos)))
  ;; ↓ backwards
  (when (keyboard-button (keyboard) key.s)
    (let* ((camdir (q:to-direction (rot camera)))
           (dt-pos (v3:*s camdir (* factor dt)))
           (dt-pos (v! (x dt-pos) 0 (z dt-pos))))
      (v3:decf (pos camera) dt-pos)))
  ;; ← left
  (when (keyboard-button (keyboard) key.a)
    (let* ((camdir (q:rotate *vec3-left* (rot camera)))
           (dt-pos (v3:*s camdir (* factor dt)))
           (dt-pos (v! (x dt-pos) 0 (z dt-pos))))
      (v3:incf (pos camera) dt-pos)))
  ;; → right
  (when (keyboard-button (keyboard) key.d)
    (let* ((camdir (q:rotate *vec3-right* (rot camera)))
           (dt-pos (v3:*s camdir (* factor dt)))
           (dt-pos (v! (x dt-pos) 0 (z dt-pos))))
      (v3:incf (pos camera) dt-pos)))
  ;; - up - jump
  (when (keyboard-button (keyboard) key.space)
    (v3:decf (pos camera)
             (v3:*s (q:rotate *vec3-down* (rot camera))
                    (* factor dt))))
  ;; - down - croutch
  (when (keyboard-button (keyboard) key.c)
    (v3:decf (pos camera)
             (v3:*s (q:rotate *vec3-up* (rot camera))
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
    (human-move factor dt camera)
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
                   (q:* (q:from-axis-angle *vec3-right* (- (y move)))
                        (q:from-axis-angle *vec3-up*    (- (x move)))))))))))
