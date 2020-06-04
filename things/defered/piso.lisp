(in-package #:incandescent)

(defclass piso (actor) ())

(defun make-piso (&key (pos (v! 0 0 0))
                       (rot (q:identity)))
  (let ((obj (make-instance 'piso :pos pos :rot rot
                                  :buf (lattice))))
    (push obj *actors*)
    obj))

(defmethod update ((actor piso) dt))

(defmethod draw ((actor piso) camera (time single-float))
  (with-slots (buf scale color) actor
    (map-g #'piso-pipe buf
           :scale scale
           :model-world (model->world actor)
           :world-view  (world->view camera)
           :view-clip   (projection  camera))))

(defmethod draw-depth ((actor piso) camera time)
  ;;#+nil
  (with-slots (buf scale color) actor
    (map-g #'piso-empty-pipe buf
           :scale scale
           :model-world (model->world actor)
           :world-view  (world->view camera)
           :view-clip   (projection  camera))))

(defun-g piso-frag ((uv          :vec2)
                    (frag-normal :vec3)
                    (frag-pos    :vec3))
  (values (v! .5 .5 .5)
          frag-pos
          (normalize frag-normal)))

(defun-g piso-empty-frag ((uv          :vec2)
                          (frag-normal :vec3)
                          (frag-pos    :vec3))
  (values))

(defpipeline-g piso-pipe ()
  :vertex   (vert g-pnt)
  :fragment (piso-frag :vec2 :vec3 :vec3))

(defpipeline-g piso-empty-pipe ()
  :vertex   (vert g-pnt)
  :fragment (piso-empty-frag :vec2 :vec3 :vec3))
