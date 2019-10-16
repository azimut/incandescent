(in-package #:incandescent)

(defvar *obstacles-pointers*    (list))
(defvar *collectables-pointers* (list))
(defvar *drifter-pointer*       nil)
(defvar *drifter*               nil)
(defvar *score*                 0)
(defvar *final-fase*            nil)

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

(defun reset-state ()
  (setf *final-fase*            nil)
  (setf *score*                 0)
  (setf *drifter*               nil)
  (setf *drifter-pointer*       nil)
  (setf *obstacles-pointers*    nil)
  (setf *collectables-pointers* nil))

(progn
  (defun init-scene ()
    (reset-state)
    ;;
    ;;(make-text "score")
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
    #+nil
    (dotimes (i 2)
      (reset-obstacle
       (make-obstacle :radius .5
                      :color (v! .2 .5 .9)))
      (reset-collectable
       (make-collectable :radius .5)))
    (make-clouds)
    ;;(make-text "asd")
    )
  ;;
  (init-scene)
  )
