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

;; welcome menu          -> infinite runner
;; From infinite runner  -> boss "shooter"
;; From infinite runner  -> platformer

;;(defclass drifter-camera (pers listener) ())

;; (defmethod initialize-instance :after ((obj drifter-camera) &key)
;;   (setf *currentcamera* obj))

;;(defvar *drifter-camera* (make-instance 'drifter-camera))

;; Main camera
(defmethod update ((obj pers) dt)
  (setf (fov obj) (rocket-get "camera:fov"))
  (let ((dpos (pos (state-drifter *game-state*))))
    (setf (pos obj)
          ;;dpos
          ;;#+nil
          (v! (rocket-get "camera:x")
              (rocket-get "camera:y")
              (+ (rocket-get "camera:z-offset") (z dpos)))
          )
    (setf (rot obj)
          (q:point-at
           (v! 0 1 0)
           (pos obj)
           (v! (* .5 (x dpos))
               (+ (* .5 (y dpos)) (rocket-get "camera:y-roffset"))
               (+ (z (pos obj)) (rocket-get "camera:z-roffset")))))))

;; Shadow camera
(defmethod update ((obj orth) dt)
  (with-slots (pos) obj
    (let ((dpos (pos (state-drifter *game-state*))))
      (setf (z pos) (+ 5f0 (z dpos)))
      (setf *light-pos* (copy-seq pos)))))
