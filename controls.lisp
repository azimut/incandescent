(in-package #:incandescent)

;; Camera and Actors(TODO) keyboard controls

(defparameter *crawl* (make-stepper (seconds .8) (seconds .8)))
(defparameter *walk*  (make-stepper (seconds .7) (seconds .7)))
(defparameter *run*   (make-stepper (seconds .5) (seconds .5)))

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

(defun walk-sound (factor)
  (declare (type fixnum factor))
  (when (or (key-down-p key.w)
            (key-down-p key.a)
            (key-down-p key.s)
            (key-down-p key.d))
    (let ((campos (pos *camera*))
          (s      (alexandria:random-elt
                   (slot-value
                    (gethash :footsteps *audio-sounds*)
                    'sources)))) ;; FIXME!
      ;; NOTE: negative Y on sound position workaround an audio glitch where
      ;;       sound played on 1 channel at times
      (ecase factor
        (5  (and (funcall *crawl*)
                 (setf (harmony:input-location s *sfx*)
                       (v! (x campos) -10 (z campos)))
                 (harmony-simple:resume s)))
        (10  (and (funcall *walk*)
                  (setf (harmony:input-location s *sfx*)
                        (v! (x campos) -10 (z campos)))
                  (harmony-simple:resume s)))
        (20 (and (funcall *run*)
                 (setf (harmony:input-location s *sfx*)
                       (v! (x campos) -10 (z campos)))
                 (harmony-simple:resume s)))
        ;; (20 (and (funcall *crawl*)
        ;;          (setf (harmony:input-location (play-sound :footsteps) *sfx*)
        ;;                (pos *camera*))))
        ))))

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
  (let ((factor 10))
    ;; MODIFIERS
    ;; - run
    (when (keyboard-button (keyboard) key.lshift)
      (setf factor 20))
    ;; - stealth
    (when (keyboard-button (keyboard) key.lctrl)
      (setf factor 5))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; MOVEMENT
    ;;(human-move factor dt camera)
    (human-move factor dt camera)
    (walk-sound factor)
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
