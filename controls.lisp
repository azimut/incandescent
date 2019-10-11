(in-package #:incandescent)

;; Camera and Actors(TODO) keyboard controls

(defparameter *crawl* (make-stepper (seconds .8) (seconds .8)))
(defparameter *walk*  (make-stepper (seconds .7) (seconds .7)))
(defparameter *run*   (make-stepper (seconds .5) (seconds .5)))

(defun cam-spin (camera ang)
  (setf (rot camera)
        (q:normalize
         (q:* (rot camera) (q:from-axis-angle *vec3-forward* ang)))))

(defun cam-turn (camera ang)
  (setf (rot camera)
        (q:normalize
         (q:* (rot camera) (q:from-axis-angle *vec3-up* ang)))))

(defun cam-tilt (camera ang)
  (setf (rot camera)
        (q:normalize
         (q:* (rot camera) (q:from-axis-angle *vec3-right* ang)))))


(defun god-move (factor dt camera)
  "absolute movement"
  (declare (type fixnum factor)
           (type single-float dt)
           ;;(type camera camera)
           )
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
           ;;(type camera camera)
           )
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

(defgeneric control (camera dt factor))

;; MODIFIERS
(defmethod control :around (camera dt factor)
  ;; - run
  (when (keyboard-button (keyboard) key.lshift)
    (setf factor 20))
  ;; - stealth
  (when (keyboard-button (keyboard) key.lctrl)
    (setf factor 5))
  (call-next-method))

(defmethod control (camera dt factor)
  "free camera controls"
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; MOVEMENT
  (human-move factor dt camera)
  ;;(god-move factor dt camera)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; SPIN
  (when (key-down-p key.q)
    (cam-spin camera (radians 1.8f0)))
  (when (key-down-p key.e)
    (cam-spin camera (radians -1.8f0)))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; PAN
  (when (key-down-p key.left)
    (cam-turn camera (radians 1.8f0)))
  (when (key-down-p key.right)
    (cam-turn camera (radians -1.8f0)))
  (when (key-down-p key.up)
    (cam-tilt camera (radians 1.8f0)))
  (when (key-down-p key.down)
    (cam-tilt camera (radians -1.8f0)))
  ;;
  (when (mouse-button (mouse) mouse.left)
    (let ((move (v2:*s (mouse-move (mouse))
                       0.03)))
      (setf (rot camera)
            (q:normalize
             (q:* (rot camera)
                  (q:normalize
                   (q:* (q:from-axis-angle *vec3-right* (- (y move)))
                        (q:from-axis-angle *vec3-up*    (- (x move))))))))))
  (pos camera))
