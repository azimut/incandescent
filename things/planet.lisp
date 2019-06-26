(in-package :incandescent)

(defclass planet (pbr) ())
(defclass planet-label (text-billboard) ())

(defun make-planet (&key (pos (v! 0 0 0))
                         (rot (q:identity)))
  (let* ((seed (random 1f0))
         (obj  (make-instance 'planet :pos pos :rot rot :seed seed
                                      :buf (sphere 1 30 30 t)))
         ;; (obj2 (make-instance 'planet-label
         ;;                      :pos (v3:+ pos (v! 0 1.5 0)) :rot rot
         ;;                      :seed seed))
         )
    ;;(push obj2 *actors*)
    (push obj *actors*)
    t))

(defmethod update ((actor planet) dt))
(defmethod update ((actor planet) dt))

(defmethod update :around ((actor planet-label) dt)
  #+nil
  (with-slots (tex scale color pos) actor
    (write-msg-on-tex (format nil "(~f ~f ~f)" (x pos) (y pos) (z pos))
                      tex)))


;;--------------------------------------------------

(defclass moon (pbr-simple) ())
(defclass moon-label (text-billboard) ())

(defun make-moon (&key (pos (v! 0 0 0))
                       (rot (q:identity))
                       (color (v! 1 1 1)))
  (let* ((seed (1- (random 2f0)))
         (obj  (make-instance 'moon :color color
                                    :pos pos :rot rot :seed seed :buf (sphere)))
         (obj2 (make-instance 'moon-label
                              :pos (v3:+ pos (v! 0 1.5 0)) :rot rot
                              :seed seed)))
    (push obj2 *actors*)
    (push obj *actors*)
    t))

(defun update-moon-pos (pos seed)
  (let ((time (mynow)))
    (v! (* 50 seed  (sin (* seed time)))
        (* 90 seed (sin (* .1 time)))
        (* 50 seed  (cos (* seed time))))))

(defmethod update ((actor moon) dt)
  (with-slots (pos seed) actor
    (setf pos (update-moon-pos pos seed))))

(defmethod update :around ((actor moon-label) dt)
  (with-slots (pos seed) actor
    (setf pos (v3:+ (update-moon-pos pos seed) (v! 0 1.5 0))))
  (with-slots (tex scale color pos) actor
    (write-msg-on-tex (format nil "~$ ~$ ~$" (x pos) (y pos) (z pos))
                      tex)))

#+nil
(progn (free-actors)
       (make-planet :rot (q:from-axis-angle (v! 0 0 1) (radians 90)))
       (dotimes (i 10)
         (make-moon :color (v! (serapeum:random-in-range 0f0 2f0)
                               (serapeum:random-in-range 0f0 2f0)
                               (serapeum:random-in-range 0f0 2f0))
                    :pos (v! (serapeum:random-in-range -15f0 15f0)
                             (serapeum:random-in-range -2f0 2f0)
                             (serapeum:random-in-range -15f0 15f0))))
       (make-clouds))
