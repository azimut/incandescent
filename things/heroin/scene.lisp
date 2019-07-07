(in-package :incandescent)

(defparameter *shadow-camera*
  (let* ((lpos (v! 50 100 -50))
         (cam  (make-instance
                'orth
                :name :shadow-camera
                :frame-size (v2! 30) ;; zoom
                :far 200f0
                :near .1f0
                :rot (q:point-at (v! 0 1 0) lpos
                                 (v! 0 -2 0))
                :pos lpos)))
    (setf *light-pos* lpos)
    cam))

(defvar *bullets* nil "bullets to shot")
(defvar *bang* nil "bullets shooted already")
(defvar *bodies* nil
  "ode:body list for each bullet, used on the callback to remove it")

(defvar *t1-obj* nil)
(defvar *t1-pos* nil)
(defvar *t1-obj-active* nil)
(defvar *t1-pos-active* nil)

(defun make-shot ()
  "initilize bullets objects, and puts them somewhere offscreen"
  (setf *bang* nil)
  (setf *bodies* nil)
  (setf *bullets*
        (loop :repeat 10
              :for bullet := (make-physic-caja :pos (v! (random 1000) 1000 (random 1000)))
              :for body   := (slot-value bullet 'body)
              :collect (progn (%ode:body-disable body)
                              (push body *bodies*)
                              bullet)))
  (setf *bodies* (coerce *bodies* 'vector)))

(defun do-shot (&optional (destino (v! 0 0 0)))
  "shoots a bullet"
  (when (not *bullets*)
    (rotatef *bullets* *bang*))
  (when-let* ((bullet  (pop *bullets*))
              (body    (slot-value bullet 'body))
              (ran-pos (v! (if (> (random 1f0) .5)
                               (random-in-range 20f0 50f0)
                               (random-in-range -20f0 -50f0))
                           5
                           (if (> (random 1f0) .5)
                               (random-in-range 20f0 50f0)
                               (random-in-range -20f0 -50f0))))
              (dir-to-orig (v3:*s
                            (v3:normalize
                             (q:to-direction
                              (q:point-at (v! 0 1 0)
                                          ran-pos
                                          destino)))
                            20000f0))
              (dir-to-orig (v! (x dir-to-orig)
                               0
                               (z dir-to-orig))))
    (push bullet *bang*)
    (when (not (position body *bodies* :test #'sb-sys:sap=))
      (setf (svref *bodies* (position-if #'cffi:null-pointer-p *bodies*))
            body))
    (ode-update-pos bullet ran-pos)
    (%ode:body-add-force body
                         (coerce (x dir-to-orig) 'double-float)
                         (coerce (y dir-to-orig) 'double-float)
                         (coerce (z dir-to-orig) 'double-float))
    (%ode:body-enable body)))

(defun make-tower (center-pos &key (upto 24))
  "initilize tower positions and objects"
  (let* ((positions
           (loop :for s :from .5 :by 2 :upto upto
                 :append (list (v3:+ center-pos (v! 0 s 0))
                               (v3:+ center-pos (v! 2 s 0))
                               (v3:+ center-pos (v! 1 (+ 1 s) -1))
                               (v3:+ center-pos (v! 1 (+ 1 s) 1)))))
         (objects
           (loop :for (p1 p2 p3 p4) :on positions :by #'cddddr
                 :for i :from .1 :by .7
                 :when (and p1 p2 p3 p4)
                 :append (list  (make-physic-caja :z 3d0 :x .5d0 :pos (v! (random 1000)
                                                                          1000
                                                                          (random 1000)))
                                (make-physic-caja :z 3d0 :x .5d0 :pos (v! (random 1000) 1000 (random 1000)))
                                (make-physic-caja :x (+ i 3d0) :z .5d0 :pos (v! (random 1000) 1000 (random 1000)))
                                (make-physic-caja :x (+ i -.35 3d0) :z .5d0 :pos (v! (random 1000) 1000 (random 1000)))))))
    (mapcar (lambda (o) (%ode:body-disable (slot-value o 'body)))
            objects)
    (values positions
            objects)))

(defun init-towers ()
  (setf *t1-pos-active* nil
        *t1-obj-active* nil
        *t1-obj* nil
        *t1-pos* nil)
  (multiple-value-bind (p o) (make-tower (v! 0 0 0))
    (setf *t1-pos* p
          *t1-obj* o)))

(let ((place t))
  (defun place-t1 ()
    "puts (or removes) 1 new cube on the tower, based on the positions
     provided at (init-towers)"
    (when (not *t1-pos*)
      (setf place nil))
    (when (not *t1-pos-active*)
      (setf place t))
    (if place
        (when-let ((obj (pop *t1-obj*))
                   (pos (pop *t1-pos*)))
          (push obj *t1-obj-active*)
          (push pos *t1-pos-active*)
          (ode-update-pos obj pos)
          ;;(ode-update-rot obj (q:identity))
          (%ode:body-disable (slot-value obj 'body))
          (%ode:body-set-force (slot-value obj 'body) 0d0 0d0 0d0)
          (%ode:body-set-torque (slot-value obj 'body) 0d0 0d0 0d0))
        (when-let ((obj (pop *t1-obj-active*))
                   (pos (pop *t1-pos-active*)))
          (push obj *t1-obj*)
          (push pos *t1-pos*)
          (ode-update-pos obj (v! (random 1000) 1000 (random 1000)))
          (%ode:body-disable (slot-value obj 'body))
          (%ode:body-set-force (slot-value obj 'body) 0d0 0d0 0d0)
          (%ode:body-set-torque (slot-value obj 'body) 0d0 0d0 0d0)))))

(defun init-scene ()
  (free-actors)
  (init-towers)
  (make-env-map *cube-tex* *cube-sam*)
  (setf *light-pos* (v! 50 100 -50))
  (make-snowblock)
  (make-shot))
