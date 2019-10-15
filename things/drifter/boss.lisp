(in-package #:incandescent)

;; Static thing without physics.

(defclass boss (actor)
  ((properties :initarg :prop  :documentation "emissive, spec, rough, metallic")
   (scene      :initarg :scene :documentation "assimp scene")
   (hp         :initform 100)
   (mode       ))
  (:default-initargs
   :draw-p nil
   :prop (v! 0 .7 .9 .2)))

(defun make-boss (&key (pos   (v! 0 2 0))
                       (color (v! 1 1 1))
                       (rot   (q:identity))
                       draw-p
                       (name  (gensym))
                       (prop  (v! 0 .7 .7 0))
                       (scale 1f0))
  (destructuring-bind (&key buf scene &allow-other-keys)
      (first (assimp-load-meshes "static/bunny.obj"))
    (let ((obj (make-instance 'boss
                              :name name
                              :prop prop
                              :draw-p draw-p
                              :scale scale
                              :color color
                              :scene scene
                              :pos pos :rot rot
                              :buf buf)))
      (push obj *actors*)
      obj)))

(defun-g boss-frag ((uv          :vec2)
                    (frag-normal :vec3)
                    (frag-pos    :vec3)
                    &uniform
                    (color       :vec3)
                    (properties  :vec4))
  (let ((emissive (x properties))
        (spec     (y properties))
        (rough    (z properties))
        (metallic (w properties))
        (ao       1f0)
        (albedo   color))
    (values (v! albedo      rough)
            (v! frag-pos    ao)
            (v! frag-normal spec)
            (v! metallic    emissive))))

(defpipeline-g boss-pipe ()
  :vertex   (vert g-pnt)
  :fragment (boss-frag :vec2 :vec3 :vec3))

(defmethod draw ((actor boss) camera time)
  (with-slots (buf scale color properties) actor
    (map-g #'boss-pipe buf
           :color color
           :scale scale
           :properties  properties
           :model-world (model->world actor)
           :world-view  (world->view camera)
           :view-clip   (projection  camera))))

(let* ((initial-pos (v! 0 -23 -100))
       (final-pos   (v! 0 8.6 -100))
       (initial-y   (y initial-pos))
       (final-y     (y final-pos)))
  (defun reset-boss ()
    (when-let ((obj (find-actor-class 'boss)))
      (setf (pos obj) initial-pos)
      (setf *final-fase* nil)))
  (defmethod update ((obj boss) dt)
    (when *final-fase*
      (with-slots (pos draw-p) obj
        (when (< (y pos) final-y)
          (setf draw-p t)
          (incf (y pos) (* 2f0 dt)))))))
