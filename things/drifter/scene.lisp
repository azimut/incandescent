(in-package #:incandescent)

(defvar *obstacles-pointers*    (list))
(defvar *collectables-pointers* (list))
(defvar *drifter-pointer*       nil)
(defvar *drifter*               nil)
(defvar *score*                 0)

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
      (setf rot (q:point-at (v! 0 1 0)
                            pos
                            (v! 0
                                (min 4f0 (+ 3 (y dpos)))
                                (z dpos))))
      (setf (z pos) (+ 10f0 (z dpos))))))

(progn
  (defun init-scene ()
    (setf *score*                 0)
    (setf *drifter*               nil)
    (setf *drifter-pointer*       nil)
    (setf *obstacles-pointers*    nil)
    (setf *collectables-pointers* nil)
    ;;
    (make-text "score")
    ;;
    (free-actors)
    (make-drifter :color (v! .1 .1 .1)
                  :dim (v! .9 .9 .9)
                  :pos (v! 0 3 10))
    ;;
    (make-route :pos (v! 0 -1 0)
                :dim (v! 10 2 50)
                :color (v! .7 .7 .7))
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
  (init-scene))
