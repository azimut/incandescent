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

(defclass pbr-simple (actor)
  ((roughness :initarg :roughness)
   (metallic  :initarg :metallic)
   (specular  :initarg :specular))
  (:default-initargs
   :roughness .1
   :metallic  .1
   :specular  .1))

(defun make-pbr-simple (&key (pos (v! 0 0 0)))
  (let ((obj (make-instance
              'pbr-simple
              :buf (sphere)
              :pos pos)))
    (push obj *actors*)
    obj))

;;--------------------------------------------------

(defclass pbr (actor)
  ((albedo    :initarg :albedo)
   (ao        :initarg :ao)
   (height    :initarg :height)
   (normal    :initarg :normal)
   (roughness :initarg :roughness)
   (uv-repeat :initarg :uv-repeat)
   (uv-speed  :initarg :uv-speed)
   (metallic  :initarg :metallic))
  (:default-initargs
   :uv-repeat (v! 1 1)
   :uv-speed  .1
   :metallic  .1
   :albedo    (get-tex "static/32.Rock01-1k/rock01_albedo.jpg"    NIL T :rgb8)
   :ao        (get-tex "static/32.Rock01-1k/rock01_ao.jpg"        NIL T :r8)
   :height    (get-tex "static/32.Rock01-1k/rock01_height.jpg"    NIL T :r8)
   :normal    (get-tex "static/32.Rock01-1k/rock01_normal.jpg"    NIL T :rgb8)
   :roughness (get-tex "static/32.Rock01-1k/rock01_roughness.jpg" NIL T :r8)))

(defun make-pbr (&key (pos (v! 0 0 0)))
  (let ((obj (make-instance
              'pbr
              :buf (sphere 1 30 30 t)
              :pos pos)))
    (push obj *actors*)
    obj))

;;--------------------------------------------------
;; UPDATE
;;--------------------------------------------------
(defgeneric update (actor dt))
(defmethod update (actor dt))
(defmethod update ((actor pbr) dt)
  (setf (slot-value actor 'metallic) .01))

(defmethod update ((actor pbr-simple) dt)
  (with-slots (metallic specular roughness) actor
    (setf metallic  .1
          specular  .1f0
          roughness .9)))



