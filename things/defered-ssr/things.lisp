(in-package #:incandescent)

(defclass thing (actor)
  ((properties :initform (v! .2 .7 .9 .9)
               :initarg :prop
               :documentation "emissive, spec, rough, metallic")
   (instances :initform 0 :initarg :instances))
  (:default-initargs
   :voxelize-p nil))

(defmethod update ((actor thing) dt)
  (with-slots (instances scale pos properties color) actor
    (setf pos (v! .5 -.4 0))
    ;;(control actor dt 1)
    (setf scale .01f0)
    (setf color (v! 1 .3 .9))
    (setf instances  1)
    (setf properties (v! 4 .7 .1 .9))))

(defmethod draw-variance-actor ((actor thing)))
(defun make-thing (&key (pos   (v! 0 .5 0))
                        (instances 0)
                        (color (v! 1 .3 .9))
                        (dim   1f0)
                        (prop  (v! 1 .7 .9 .9))
                        (scale .1f0))
  (let ((obj (make-instance 'thing
                            :instances instances
                            :prop prop
                            :scale scale :color color
                            :pos pos
                            :buf (box dim dim dim))))
    (push obj *actors*)
    obj))

(defun-g thing-vert ((vert g-pnt) &uniform
                     (time        :float)
                     (model-world :mat4)
                     (world-view  :mat4)
                     (view-clip   :mat4)
                     (scale       :float))
  (let* ((pos        (* scale
                        (pos vert)))
         (norm       (norm vert))
         (tex        (tex vert))
         (world-norm (* (m4:to-mat3 model-world) norm))
         (world-pos  (* model-world (v! pos 1)))
         (view-pos   (* world-view  world-pos))
         (clip-pos   (* view-clip   view-pos)))
    (values clip-pos
            tex
            world-norm
            (s~ world-pos :xyz))))

(defun-g thing-frag ((uv          :vec2)
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
            (v! metallic    emissive))))

(defpipeline-g thing-pipe ()
  :vertex   (thing-vert g-pnt)
  :fragment (thing-frag :vec2 :vec3 :vec3))

(defmethod draw ((actor thing) camera time)
  (with-slots (buf scale color properties instances) actor
    (with-instances instances
      (when (> instances 0)
        (map-g #'thing-pipe buf
               ;; VERTEX
               :time time
               :model-world (model->world actor)
               :world-view  (world->view camera)
               :view-clip   (projection  camera)
               :scale scale
               ;; FRAGMENT
               :color color
               :properties (v! properties))))))
