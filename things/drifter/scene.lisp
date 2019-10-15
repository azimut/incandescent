(in-package #:incandescent)

(defvar *obstacles-pointers*    (list))
(defvar *collectables-pointers* (list))
(defvar *drifter-pointer*       nil)
(defvar *drifter*               nil)
(defvar *score*                 0)
(defvar *final-fase*            nil)


(defparameter *shadow-camera*
  (let* ((lpos (v! 10 10 10))
         (ldir (q:point-at (v! 0 1 0) lpos (v! 0 0 0)))
         (cam  (make-instance 'orth
                              :name :shadow-camera
                              :frame-size (v2! 35) ;; zoom
                              :far 50f0
                              :near 10f0
                              :rot ldir
                              :pos lpos)))
    (setf *light-pos* lpos)
    (setf *light-dir* (q:to-direction ldir))
    cam))

(defmethod update ((obj pers) dt)
  (with-slots (pos rot) obj
    (let ((dpos (pos *drifter*)))
      (setf (y pos) 3f0)
      (setf (x pos) 0f0)
      (setf rot (q:point-at (v! 0 1 0)
                            pos
                            (v! (* .15 (x dpos))
                                (min 4f0 (+ 1 (y dpos)))
                                (z dpos))))
      (setf (z pos) (+ 6f0 (z dpos))))))

(progn
  (defun init-scene ()
    (setf *final-fase*            nil)
    (setf *score*                 0)
    (setf *drifter*               nil)
    (setf *drifter-pointer*       nil)
    (setf *obstacles-pointers*    nil)
    (setf *collectables-pointers* nil)
    ;;
    ;;(make-text "score")
    ;;
    (free-actors)
    (make-boss :pos (v! 0 -23 -100) :scale 10f0 :draw-p nil)
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
    ;;(make-env-map *cube-tex* *cube-sam*)
    (make-clouds)
    ;;(make-text "asd")
    )
  ;;
  ;;(init-scene)
  )
