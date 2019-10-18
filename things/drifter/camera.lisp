(in-package #:incandescent)

;; Platformer:
;; - camera moves in the -Z direction
;; - camera reacts to jumps
;; - controls only jump or move forward
;; - z-offset 0
;;
;; Infinite runner:
;; - strafe
;;
;; Boss fight:
;; - no forward/backward

;; welcome menu -> infinite runner
;; From infinite runner  -> boss "shooter"
;; From infinite runner  -> platformer

;; Main camera
(defmethod update ((obj pers) dt)
  (setf (fov obj) (rocket-get "camera:fov"))
  (with-slots (pos rot) obj
    (let ((dpos (pos (state-drifter *game-state*))))
      (setf (y pos) (rocket-get "camera:y"))
      (setf (x pos) (rocket-get "camera:x"))
      (setf (z pos) (+ (rocket-get "camera:z-offset") (z dpos)))
      (setf rot (q:point-at
                 (v! 0 1 0)
                 pos
                 (v! (* .25 (x dpos))
                     (+ (* .5 (y dpos)) (rocket-get "camera:y-roffset"))
                     (+ (z pos) (rocket-get "camera:z-roffset"))))))))

;; Shadow camera
(defmethod update ((obj orth) dt)
  (with-slots (pos) obj
    (let ((dpos (pos (state-drifter *game-state*))))
      (setf (z pos) (+ 5f0 (z dpos)))
      (setf *light-pos* (copy-seq pos)))))
