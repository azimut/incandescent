(in-package #:incandescent)

(defvar *score* 0)
(defvar *obstacles-pointers* (list))
(defvar *drifter-pointer*    nil)

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

(progn
  (defun init-scene ()
    (setf *score* 0)
    (setf *drifter-pointer* nil)
    (setf *obstacles-pointers* nil)
    ;;
    (free-actors)
    (make-drifter :color (v! .9 .1 .1) :pos (v! 0 2 0))
    ;;
    (make-route :pos (v! 0 -1 0)
                :dim (v! 10 2 50)
                :color (v! .7 .7 .7))
    ;;
    ;;#+nil
    (dotimes (i 5)
      (reset-obstacle
       (make-obstacle :radius .5
                      :color (v! .3 1 .7))))
    ;;(make-text-multi "score 10")
    )
  (init-scene)
  )
