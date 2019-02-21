(in-package #:incandescent)

(defvar *actors* nil)

(defvar *scale* 1f0)
(defvar *color* (v! .3 .3 .3))
(defvar *rough* 1f0)
(defvar *pointlight-pos* (v! 0 0 0))

(defparameter *light-pos* (v! 0 1000 300))
(defparameter *light-color* (v! .9 .9 .9))
(defparameter *exposure* 2f0)
(defparameter *parallax-scale* .01f0)

(defun update-all-the-things (l)
  (declare (list l))
  (loop :for actor :in l :do
     (update actor)))

(defun model->world (actor)
  (with-slots (pos rot) actor
      (m4:* (m4:translation pos)
            (q:to-mat4 rot))))

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
   :uv-repeat 1f0
   :uv-speed .1
   :metallic .1
   :albedo    (get-tex "static/37.Paint01-1k/paint01_albedo.jpg" NIL T :rgb8)
   :ao        (get-tex "static/37.Paint01-1k/paint01_ao.jpg" NIL T :r8)
   :height    (get-tex "static/37.Paint01-1k/paint01_height.jpg" NIL T :r8)
   :normal    (get-tex "static/37.Paint01-1k/paint01_normal.jpg" NIL T :rgb8)
   :roughness (get-tex "static/37.Paint01-1k/paint01_roughness.jpg" NIL T :r8)))

(defclass pbr-shadow (pbr) ())

(defclass piso (pbr-simple) ())
(defun make-piso (&optional (pos (v! 0 0 0))
                    (rot (q:identity))
                    (scale 1f0))
  (let ((obj
         (make-instance
          'piso
          :buf (lattice 100 100 2 2 t)
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
(defun make-box (&optional (pos (v! 0 0 0)) (scale 1f0))
  (let ((obj (make-instance 'box
                            :pos pos
                            :scale scale)))
    (push obj *actors*)
    obj))

(defclass assimp-flat (actor)
  ())
(defclass assimp-thing (actor)
  ((albedo   :initarg :albedo)
   (normals  :initarg :normals)
   (specular :initarg :specular)))
(defclass assimp-thing-with-bones (actor)
  ((albedo   :initarg :albedo)
   (normals  :initarg :normals)
   (specular :initarg :specular)))

;;--------------------------------------------------
;; UPDATE
;;--------------------------------------------------
(defgeneric update (actor))
(defmethod update (actor))
(defmethod update ((actor pbr)))
(defmethod update ((actor pbr-simple))  )
(defmethod update ((actor box))
  (with-slots (pos rot color) actor
    (setf color (v! .1 .3 .9))
    (setf pos (v! 0 0 0))
    (setf rot (q:from-axis-angle (v! 0 (sin (mynow)) (cos (mynow)))
                                 (radians (mod (* 20 (mynow)) 360))))))
(defmethod update ((actor assimp-flat))
  ;;(setf (rot actor) (q:from-axis-angle (v! 1 0 0) (radians -90)))
  )
(defmethod update ((actor assimp-thing))
  (with-slots (scale rot pos) actor
    (setf pos (v! (mod (mynow) 20)
                  10;;(- (mod (* 2 (mynow)) 20))
                  (+ +100 (- (mod (* 10 (mynow)) 120)))))
    ;;(setf pos (v! 0 0 100))
    (setf rot (q:*
               (q:from-axis-angle
                (v! 0 0 1)
                (radians (* 20 (sin (* 1 (mynow))))))
               (q:from-axis-angle
                (v! 0 1 0)
                (radians 180))))
    ;; (setf rot (q:from-axis-angle
    ;;            (v! .9 .8 1)
    ;;            (radians (* 30 (sin (* .5 (mynow)))))))
    (setf scale 1f0))
  )

(defmethod update ((actor assimp-thing-with-bones))
  (with-slots (scale rot pos) actor
    (setf pos (v! 0 0 0))
    ;; (setf rot (q:* (q:from-axis-angle (v! 0 1 0)
    ;;                                   (radians -30))
    ;;                (q:from-axis-angle (v! 1 0 0)
    ;;                                   (radians -90))))
    ;; (setf rot (q:from-axis-angle (v! 1 0 0)
    ;;                              (radians -90)))
    ;; (setf rot (q:* (q:from-axis-angle (v! 1 0 0) (radians 270))
    ;;                (q:from-axis-angle (v! 0 1 0)
    ;;                                   (radians (mod (* .1 (get-internal-real-time))
    ;;                                                 360)))))
    (setf rot (q:identity))
    (setf scale 1f0)))
