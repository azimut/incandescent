(in-package #:incandescent)

(defparameter *shadow-dimensions* '(2048 2048))
(defparameter *shadow-dimensions* '(1024 1024))

(defparameter *default-charset*
  (format nil "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ-,/:0123456789~% ."))

(defparameter *runner-score* 2
  "score needed to pass the runner phase")
(defparameter *boss-bleeding* 5
  "yikes...speed multiplier for boss phase")
(defparameter *sidescroller-distance* 1000
  "distance to roll in the sidescrolling phase")

(defparameter *route-length* 200)
(defparameter *route-half*   (/ *route-length* 2))

(defparameter *shadow-camera*
  (let* ((lpos (v! 8 10 -660))
         (ldir (q:point-at (v! 0 1 0) lpos (v! 0 0 -665)))
         (cam  (make-instance 'orth
                              :name :shadow-camera
                              :frame-size (v2! 100) ;; zoom
                              :far 30f0
                              :near 1f0
                              :rot ldir
                              :pos lpos)))
    (setf *light-pos* lpos)
    (setf *light-dir* (q:to-direction ldir))
    cam))

;; This is a mess...but it works?

(defclass game-state ()
  ((phases      :reader   state-phases  :initform '((:welcome 0) (:runner 64) (:boss 88) (:sidescroller 112)))
   (phase       :initarg  :phase        :accessor state-phase)
   (transitionp :initform nil           :accessor state-transitionp)
   (phasesc     :reader   state-phasesc)
   (score       :initform 0             :accessor state-score)
   (drifter     :accessor state-drifter)
   (distance    :initform 0 :accessor state-distance))
  (:default-initargs
   :phase    '(:welcome 0)))

(defmethod initialize-instance :after ((obj game-state) &key)
  (setf (slot-value obj 'phasesc) (cm:new cm:line :of (slot-value obj 'phases))))
(defmethod reinitialize-instance :after ((obj game-state) &key)
  (setf (slot-value obj 'phase) '(:welcome 0))
  (setf (slot-value obj 'transitionp) nil)
  (setf (slot-value obj 'score) 0)
  (setf (slot-value obj 'phasesc) (cm:new cm:line :of (slot-value obj 'phases))))

(defvar *game-state* (make-instance 'game-state))

;;--------------------------------------------------
(defclass next-phase (event) ())

(defmethod update ((obj next-phase) dt)
  (when (>= (rocket-row) (second (state-phase *game-state*)))
    (rocket-pause)
    (alexandria:removef *actors* obj)
    (setf (state-transitionp *game-state*) nil)))

(defun next-phase ()
  (rocket-play)
  (setf (state-transitionp *game-state*) t)
  (setf (state-phase *game-state*) (first (cm:next (state-phasesc *game-state*))))
  (add-event (make-instance 'next-phase))
  t)
;;--------------------------------------------------

(defmethod (setf state-phase) :around (value (obj game-state))
  (assert (member value '(:welcome :runner :boss :sidescroller)))
  (call-next-method))

(defmethod (setf state-phase) :before ((value (eql :welcome)) (obj game-state))
  (setf (state-score obj) 0)
  (make-text
   (format nil " H,K - Strife~% U,M - Roll~%   J - Jump~%~%SPACE TO START")
   :pos (v! -70 -20)
   :scale 1.5f0)
  (let ((drifter (state-drifter *game-state*)))
    (%ode:body-enable (slot-value drifter 'body))
    (ode-update-pos drifter (v! 0 2 30))
    (setf (pos drifter) (v! 0 2 30))))

(defmethod (setf state-phase) :before ((value (eql :boss)) (obj game-state))
  (delete-all-actor-class 'obstacle)
  (delete-all-actor-class 'collectable)
  (let ((drifter (state-drifter *game-state*)))
    (%ode:body-set-linear-vel (body drifter) 0d0 0d0 0d0)
    (%ode:body-add-force (body drifter) 0d0 20d0 -40d0)
    (make-boss :pos (v! 0 -23 (+ -40 (z (pos drifter)))))))

(defmethod (setf state-phase) :before ((value (eql :sidescroller)) (obj game-state))
  (delete-all-actor-class 'boss)
  (setf (state-distance obj)
        (- (z (pos (state-drifter obj))) *sidescroller-distance*))
  (let* ((drifter (state-drifter *game-state*))
         (new-pos (v! 0 (y (pos drifter)) (z (pos drifter)))))
    (%ode:body-enable (slot-value drifter 'body))
    (ode-update-pos drifter new-pos)
    (setf (pos drifter) new-pos)))

(defmethod (setf state-phase) (value (obj game-state))
  (let* ((phases (state-phases obj))
         (alist  (assoc value phases)))
    (setf (slot-value obj 'phase) alist)
    (when (not (state-transitionp obj))
      (rocket-pause)
      (rocket-set-row (second alist))
      (setf    (slot-value obj 'phasesc) (cm:new cm:line :of phases))
      (cm:next (slot-value obj 'phasesc) (1+ (position value phases :key #'car))))
    t))

;;--------------------------------------------------

(progn
  (defun init-scene ()
    ;;
    ;;
    (free-actors)
    ;;(make-boss :pos (v! 0 -23 -100) :scale 10f0 :draw-p nil :prop (v! 0 .1 .7 .2))
    (make-drifter :color (v! .1 .1 .1) :dim (v! .9 .9 .9) :pos (v! 0 3 30))
    ;; Floor
    (make-route :name :piso :pos (v! 0 -1 0)    :dim (v! 10 2 *route-length*) :color (v! .7 .7 .7))
    (make-route :name :piso :pos (v! 0 -1 (- *route-length*)) :dim (v! 10 2 *route-length*) :color (v! .7 .7 .7))
    (make-route :name :piso :pos (v! 0 -1 (- (* 2 *route-length*))) :dim (v! 10 2 *route-length*) :color (v! .7 .7 .7))
    ;; Sidebars
    ;; (make-route :name :rside :pos (v!  5.5 .1 0) :dim (v! 1 .5 *route-length*) :color (v! .7 .7 .7))
    ;; (make-route :name :lside :pos (v! -5.5 .1 0) :dim (v! 1 .5 *route-length*) :color (v! .7 .7 .7))
    ;; (make-route :name :rside :pos (v!  5.5 (- *route-length*) :dim (v! 1 .5 *route-length*) :color (v! .7 .7 .7))
    ;; (make-route :name :lside :pos (v! -5.5 (- *route-length*) :dim (v! 1 .5 *route-length*) :color (v! .7 .7 .7))
    ;; (make-route :name :rside :pos (v!  5.5 .1 (- (* 2 *route-length*) :dim (v! 1 .5 *route-length*) :color (v! .7 .7 .7))
    ;; (make-route :name :lside :pos (v! -5.5 .1 (- (* 2 *route-length*) :dim (v! 1 .5 *route-length*) :color (v! .7 .7 .7))
    ;; Invisible walls
    (make-stopper :color (v3! .8) :draw-p t :pos (v! 5.5 .1 0) :dim (v! 1 .5 *route-length*))
    (make-stopper :color (v3! .8) :draw-p t :pos (v! -5.5 .1 0) :dim (v! 1 .5 *route-length*))
    (make-stopper :color (v3! .8) :draw-p t :pos (v! 5.5 .1 (- *route-length*)) :dim (v! 1 .5 *route-length*))
    (make-stopper :color (v3! .8) :draw-p t :pos (v! -5.5 .1 (- *route-length*)) :dim (v! 1 .5 *route-length*))
    (make-stopper :color (v3! .8) :draw-p t :pos (v! 5.5 .1 (- (* 2 *route-length*))) :dim (v! 1 .5 *route-length*))
    (make-stopper :color (v3! .8) :draw-p t :pos (v! -5.5 .1 (- (* 2 *route-length*))) :dim (v! 1 .5 *route-length*))

    ;;
    ;;#+nil
    (dotimes (i 2)
      (reset-obstacle
       (make-obstacle :radius .5 :color (v! .2 .5 .9)))
      (reset-collectable
       (make-collectable :radius .5)))
    (make-clouds)
    (reinitialize-instance *game-state*)
    (setf (state-phase *game-state*) :welcome)
    )
  ;;
  ;;(init-scene)
  )
