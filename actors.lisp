(in-package #:incandescent)

(defvar *actors* nil)

(defclass actor ()
  ((name  :initarg :name  :reader   actor-name)
   (pos   :initarg :pos   :accessor pos)
   (rot   :initarg :rot   :accessor rot)
   (buf   :initarg :buf)
   (color :initarg :color)
   (scale :initarg :scale))
  (:default-initargs
   :name  (gensym)
   :pos   (v! 0 0 0)
   :rot   (q:identity)
   :buf   (box)
   :color (v! 1 1 1)
   :scale 1f0))

(defmethod free (object) t)
(defun free-actors ()
  (mapcar #'free *actors*)
  (setf *actors* nil))

(defun update-all-the-things (l dt)
  (declare (list l))
  (dolist (actor l)
    (update actor dt)))

(defun model->world (actor)
  (with-slots (pos rot) actor
    (m4:* (m4:translation pos)
          (q:to-mat4 rot))))
(defun find-actor-class (class-name)
  (declare (type symbol class-name))
  (find-if (lambda (a) (typep a class-name))
           *actors*))

(defun delete-actor-name (actor-name)
  (declare (symbol actor-name))
  (setf *actors*
        (delete-if
         (lambda (x) (eq actor-name (slot-value x 'name)))
         *actors*))
  NIL)

(defun delete-actor-class (class-name)
  (declare (string class-name))
  (setf *actors*
        (delete-if
         (lambda (x) (string= class-name (class-name (class-of x))))
         *actors*))
  NIL)

(defmethod sync (x) (+ .5 (* .5 (sin x))))
(defmethod cync (x) (+ .5 (* .5 (cos x))))

;;--------------------------------------------------



;;--------------------------------------------------
;; UPDATE
;;--------------------------------------------------
(defgeneric update (actor dt))
(defmethod update (actor dt))




