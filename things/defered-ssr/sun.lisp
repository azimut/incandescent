(in-package #:incandescent)

(defclass sun (actor)
  ((properties :initform (v! 0 .7 .9 .9)
               :initarg :prop
               :documentation "emissive, spec, rough, metallic")))

(defmethod update ((actor sun) dt)
  #+nil
  (with-slots (color) actor
    (setf color (v! 1 1 1)))
  ;;#+nil
  (with-slots (pos rot) actor
    ;;(setf rot (q:identity))
    ;;(control actor dt 1)
    ;; (setf *light-pos* (copy-seq pos))
    ;; (setf *light-dir* (q:to-direction (q:point-at (v! 0 1 0) pos (v! .7 0 .2))))
    ;; (setf (pos *shadow-camera*) (copy-seq pos))
    ;; (setf (rot *shadow-camera*) (q:point-at (v! 0 1 0) pos (v! .7 0 .2)))
    ))

(defun make-sun (&key (pos   *light-pos*)
                      (rot   (q:identity))
                      (prop  (v! 1 .7 .9 .9))
                      (color (v! 1 1 1))
                      (radius 1f0)
                      (scale 1f0))
  (let ((obj (make-instance 'sun
                            :prop prop
                            :scale scale :color color
                            :pos pos
                            :buf (sphere radius))))
    (push obj *actors*)
    obj))

(defun-g sun-frag ((uv          :vec2)
                   (frag-normal :vec3)
                   (frag-pos    :vec3)
                   &uniform
                   (color       :vec3)
                   (properties  :vec4))
  (let ((emissive (x properties))
        (spec     (y properties))
        (rough    (z properties))
        (metallic (w properties))
        (albedo color))
    (values (v! albedo      rough)
            (v! frag-pos    emissive)
            (v! frag-normal spec)
            (v! metallic 1))))

(defpipeline-g sun-pipe ()
  :vertex   (vert g-pnt)
  :fragment (sun-frag :vec2 :vec3 :vec3))

(defmethod draw-variance-actor ((actor sun)))
(defmethod draw ((actor sun) camera time)
  (with-slots (buf scale color properties) actor
    (map-g #'sun-pipe buf
           :color color
           :scale scale
           :properties (v! properties)
           :model-world (model->world actor)
           :world-view  (world->view camera)
           :view-clip   (projection  camera))))
