(in-package #:incandescent)

;; without shadow and bones looks off

(defclass person (actor)
  ((scene  :initarg :scene)
   (albedo :initarg :albedo)))

(defmethod update ((actor person) dt)
  (with-slots (pos rot scale) actor
    (setf scale 3f0)
    (setf pos (v! 0 3 0))
    (setf rot (q:from-axis-angle (v! 0 1 0) (radians 180)))))

(defun make-person (&key (pos (v! 0 0 0)) (rot (q:identity)))
  (let ((*processing-flags*
          '(:ai-process-triangulate
            :ai-process-flip-u-vs
            :ai-process-preset-target-realtime-max-quality
            :ai-process-calc-tangent-space)))
    (destructuring-bind (&key scene buf albedo &allow-other-keys)
        (first (assimp-load-meshes "/home/sendai/Downloads/_body/GeorgeB.obj"))
      (let ((obj (make-instance 'person :pos pos :rot rot
                                        :buf buf :scene scene :albedo albedo)))
        (push obj *actors*)
        obj))))

(defmethod draw ((actor person) camera time)
  (with-slots (buf scale albedo) actor
    (map-g #'person-pipe buf
           :albedo albedo
           :scale scale
           :model-world (model->world actor)
           :world-view (world->view camera)
           :view-clip (projection  camera))))

(defun-g person-frag ((uv :vec2) (frag-normal :vec3) (frag-pos :vec3)
                      &uniform
                      (albedo :sampler-2d))
  (values (texture albedo uv)
          frag-pos
          (normalize frag-normal)))

(defpipeline-g person-pipe ()
  :vertex (vert g-pnt)
  :fragment (person-frag :vec2 :vec3 :vec3))
