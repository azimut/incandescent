(in-package #:incandescent)

(defparameter *shadow-dimensions* '(2048 2048))
(defparameter *shadow-dimensions* '(1024 1024))

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
  ((phases  :reader state-phases :initform '((:welcome 0) (:runner 64) (:boss 88) (:sidecroller 112)))
   (phase   :initarg :phase :accessor state-phase)
   (phasesc :reader state-phasesc)
   (score   :initform 0 :accessor state-score)
   (drifter :accessor state-drifter))
  (:default-initargs
   :phase '(:welcome 0)))

(defmethod initialize-instance :after ((obj game-state) &key)
  (setf (slot-value obj 'phasesc) (cm:new cm:line :of (slot-value obj 'phases))))
(defmethod reinitialize-instance :after ((obj game-state) &key)
  (setf (slot-value obj 'phase) '(:welcome 0))
  (setf (slot-value obj 'score) 0)
  (setf (slot-value obj 'phasesc) (cm:new cm:line :of (slot-value obj 'phases))))

(defvar *game-state* (make-instance 'game-state))

(defclass next-phase (event) ())
(defmethod update ((obj next-phase) dt)
  (when (>= (rocket-row) (second (state-phase *game-state*)))
    (rocket-pause)
    (alexandria:removef *actors* obj)))
(defun next-phase ()
  (rocket-play)
  ;; FIXME: too early for switch...
  (setf (slot-value *game-state* 'phase) (cm:next (state-phasesc *game-state*)))
  (add-event (make-instance 'next-phase))
  t)

(defmethod (setf state-phase) :around (value (obj game-state))
  (assert (member value '(:welcome :runner :boss :sidescroller)))
  (call-next-method))

(defmethod (setf state-phase) (value (obj game-state))
  (let* ((alist  (assoc value (state-phases *game-state*)))
         (nrow   (second alist))
         (phases (slot-value obj 'phases)))
    (setf (slot-value obj 'phase) alist)
    (setf (slot-value obj 'phasesc)
          (cm:new cm:line
            :of (alexandria:rotate (copy-seq phases)
                                   (position value phases :key #'car))))
    (rocket-pause)
    (rocket-set-row nrow)
    alist))

;;--------------------------------------------------

(progn
  (defun init-scene ()
    ;;
    (make-text "score")
    ;;
    (free-actors)
    ;;(make-boss :pos (v! 0 -23 -100) :scale 10f0 :draw-p nil :prop (v! 0 .1 .7 .2))
    (make-drifter :color (v! .1 .1 .1) :dim (v! .9 .9 .9) :pos (v! 0 3 10))
    ;; Floor
    (make-route :name :piso :pos (v! 0 -1 0)    :dim (v! 10 2 100) :color (v! .7 .7 .7))
    (make-route :name :piso :pos (v! 0 -1 -100) :dim (v! 10 2 100) :color (v! .7 .7 .7))
    (make-route :name :piso :pos (v! 0 -1 -200) :dim (v! 10 2 100) :color (v! .7 .7 .7))
    ;; Sidebars
    ;; (make-route :name :rside :pos (v!  5.5 .1 0) :dim (v! 1 .5 100) :color (v! .7 .7 .7))
    ;; (make-route :name :lside :pos (v! -5.5 .1 0) :dim (v! 1 .5 100) :color (v! .7 .7 .7))
    ;; (make-route :name :rside :pos (v!  5.5 .1 -100) :dim (v! 1 .5 100) :color (v! .7 .7 .7))
    ;; (make-route :name :lside :pos (v! -5.5 .1 -100) :dim (v! 1 .5 100) :color (v! .7 .7 .7))
    ;; (make-route :name :rside :pos (v!  5.5 .1 -200) :dim (v! 1 .5 100) :color (v! .7 .7 .7))
    ;; (make-route :name :lside :pos (v! -5.5 .1 -200) :dim (v! 1 .5 100) :color (v! .7 .7 .7))
    ;; Invisible walls
    (make-stopper :color (v3! .8) :draw-p t :pos (v! 5.5 .1 0) :dim (v! 1 .5 100))
    (make-stopper :color (v3! .8) :draw-p t :pos (v! -5.5 .1 0) :dim (v! 1 .5 100))
    (make-stopper :color (v3! .8) :draw-p t :pos (v! 5.5 .1 -100) :dim (v! 1 .5 100))
    (make-stopper :color (v3! .8) :draw-p t :pos (v! -5.5 .1 -100) :dim (v! 1 .5 100))
    (make-stopper :color (v3! .8) :draw-p t :pos (v! 5.5 .1 -200) :dim (v! 1 .5 100))
    (make-stopper :color (v3! .8) :draw-p t :pos (v! -5.5 .1 -200) :dim (v! 1 .5 100))

    ;;
    ;;#+nil
    (dotimes (i 2)
      (reset-obstacle
       (make-obstacle :radius .5
                      :color (v! .2 .5 .9)))
      (reset-collectable
       (make-collectable :radius .5)))
    (make-clouds)
    ;;(make-text "asd")
    (reinitialize-instance *game-state*)
    (setf (state-phase *game-state*) :runner)
    )
  ;;
  (init-scene)
  )
