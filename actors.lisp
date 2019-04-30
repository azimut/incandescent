(in-package #:incandescent)

(defvar *actors* nil)

(defvar *scale* 1f0)
(defvar *color* (v! .3 .3 .3))
(defvar *rough* 1f0)
(defvar *pointlight-pos* (v! 0 0 0))

(defparameter *light-pos* (v! -2000 1000 0))
(defparameter *light-color* (v! 0.78823537 0.8862746 1.0))
(defparameter *parallax-scale* .01f0)

(defclass actor ()
  ((name  :initarg :name :reader actor-name)
   (pos   :initarg :pos :accessor pos)
   (rot   :initarg :rot :accessor rot)
   (buf   :initarg :buf)
   (color :initarg :color)
   (scale :initarg :scale))
  (:default-initargs
   :name (gensym)
   :pos (v! 0 0 0)
   :rot (q:identity)
   :buf (box)
   :color (v! 1 1 1)
   :scale 1f0))

(defun update-all-the-things (l dt)
  (declare (list l))
  (loop :for actor :in l :do
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
   (metallic  :initarg :metallic))
  (:default-initargs
   :roughness .1
   :metallic  .1))

(defun make-pbr-simple (&optional (pos (v! 0 0 0)))
  (let ((obj
         (make-instance
          'pbr-simple
          :buf (sphere)
          :pos pos)))
    (push obj *actors*)
    obj))

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
   :uv-speed .1
   :metallic .1
   :albedo    (get-tex "static/32.Rock01-1k/rock01_albedo.jpg" NIL T :rgb8)
   :ao        (get-tex "static/32.Rock01-1k/rock01_ao.jpg" NIL T :r8)
   :height    (get-tex "static/32.Rock01-1k/rock01_height.jpg" NIL T :r8)
   :normal    (get-tex "static/32.Rock01-1k/rock01_normal.jpg" NIL T :rgb8)
   :roughness (get-tex "static/32.Rock01-1k/rock01_roughness.jpg" NIL T :r8)))

(defclass pbr-shadow (pbr) ())

(defclass piso (pbr-simple) ())
(defun make-piso (&key (pos (v! 0 0 0)) (rot (q:identity)) (scale 1f0) (buf (lattice 100 100 2 2 t)) (uv-repeat (v! 1 1)))
  (let ((obj
         (make-instance
          'pbr
          :uv-speed 0f0
          :uv-repeat uv-repeat
          :buf buf
          :pos pos
          :scale scale
          :rot rot)))
    (push obj *actors*)
    obj))

(defclass thing (pbr) ())
(defun make-thing (&optional (pos (v! 0 0 0)) (rot (q:identity)))
  (let ((obj (make-instance
              'pbr-simple
              :buf (sphere)
              :pos pos
              :rot rot)))
    (push obj *actors*)
    obj))

(defclass box (actor)
  ((buf :initform (box 2 2 2))))
(defun make-box (&key (pos (v! 0 0 0)) (rot (q:identity))
                   (scale 1f0) (name (gensym)) (buf (box)))
  (let ((obj (make-instance 'box
                            :buf buf
                            :name name
                            :pos pos
                            :rot rot
                            :scale scale)))
    (push obj *actors*)
    obj))
(defun init-box (&optional (buf (box)) (range 8f0))
  "helper to create a bunch of boxes at random pos/rot/scale"
  (declare (type cepl:buffer-stream buf)
           (type single-float range))
  (let ((half (* .5 range)))
    (dotimes (i 20)
      (make-box :pos (v! (- (random range) half)
                         (- (random range) half)
                         (- (random range) half))
                :buf buf
                :scale (random 1f0)
                :rot (q:from-axis-angle
                      (v! (random 1f0)
                          (random 1f0)
                          (random 1f0))
                      (radians (random 360)))))))

;;--------------------------------------------------
;; UPDATE
;;--------------------------------------------------
(defgeneric update (actor dt))
(defmethod update (actor dt))
(defmethod update ((actor pbr) dt)
  (with-slots (metallic) actor
    (setf metallic .1f0)))
(defmethod update ((actor pbr-simple) dt))
(defmethod update ((actor box) dt)
  (with-slots (pos rot color name) actor
    (unless (eq :piso name)
      ;;(setf color (v! .1 .3 .9))
      ;;(setf pos (v! 0 0 0))
      (setf rot (q:* rot
                     (q:from-axis-angle
                      (v! 0 1 1)
                      (radians (mod (* 20 dt) 360)))
                     )))
    )
  )


