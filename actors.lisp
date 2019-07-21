(in-package #:incandescent)

(defvar *actors* nil)
(defvar *scenes* (make-array 5 :initial-element nil))
(defvar *scene-index* 0)

(defclass actor ()
  ((name   :initarg :name   :reader   actor-name)
   (pos    :initarg :pos    :accessor pos)
   (rot    :initarg :rot    :accessor rot)
   (buf    :initarg :buf)
   (color  :initarg :color)
   (scale  :initarg :scale)
   (seed   :initarg :seed   :initform (random 1f0))
   (draw-p :initarg :draw-p :initform t))
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

(defun free-scene (nr-scene)
  (declare (type (integer 0 4) nr-scene))
  (when-let ((scene (aref *scenes* nr-scene)))
    (mapcar #'free scene)
    (setf (aref *scenes* nr-scene) nil)))

(defun free-scenes ()
  (dotimes (i 5)
    (free-scene i))
  (setf *scenes* (make-array 5 :initial-element nil)))

(defmacro in-scene (nr-scene &body body)
  "(in-scene 0
     (make-box)"
  (declare (type (integer 0 4) nr-scene))
  `(progn (free-scene ,nr-scene)
          (let ((*actors* nil))
            (mapcar (lambda (n) (push n (aref *scenes* ,nr-scene)))
                    (serapeum:collecting
                      ,@(loop :for actor :in body
                              :collect `(collect ,actor)))))))

(defun update-all-the-things (l dt)
  (declare (list l))
  (dolist (actor l)
    (update actor dt)))

(defun model->world (actor)
  (with-slots (pos rot) actor
    (m4:* (m4:translation pos)
          (q:to-mat4      rot))))

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
  (declare (type string class-name))
  (let ((obj (find-if (lambda (x) (string-equal class-name (serapeum:class-name-of x)))
                      *actors*)))
    (setf *actors* (delete obj *actors*))
    (when obj (free obj)))
  NIL)

(defun get-an-actor-class (class-name)
  "returns an random actor of CLASS-NAME on *ACTORS*"
  (declare (type string class-name))
  (when-let ((actors (serapeum:filter (lambda (x) (string-equal class-name (serapeum:class-name-of x)))
                                      *actors*)))
    (alexandria:random-elt actors)))

(defmethod sync (x) (+ .5 (* .5 (sin x))))
(defmethod cync (x) (+ .5 (* .5 (cos x))))

;;--------------------------------------------------
;; UPDATE
;;--------------------------------------------------
(defgeneric update (actor dt))
(defmethod update (actor dt))
