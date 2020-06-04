(in-package :incandescent)

(defclass sun (actor) ())

(defun make-sun (&key (pos (v! 0 0 0))
                      (rot (q:identity))
                      (draw-p t)
                      (radius 1f0))
  (let ((obj (make-instance 'sun :pos pos :rot rot
                                 :draw-p draw-p
                                 :buf (sphere radius))))
    (push obj *actors*)
    obj))

(defmethod update ((actor sun) dt))

(defmethod draw ((actor sun) camera (time single-float))
  (with-slots (buf scale color) actor
    (map-g #'sun-pipe buf
           :scale scale
           :color color
           :time  time
           :cam-pos (pos camera)
           :model-world (model->world actor)
           :world-view  (world->view camera)
           :view-clip   (projection  camera)
           :light-color *light-color*
           :light-pos   *light-pos*)))

(defun-g sun-frag ((uv :vec2) (frag-pos :vec3) (frag-normal :vec3)
                   &uniform
                   (time        :float)
                   (color       :vec3)
                   (cam-pos     :vec3)
                   (light-color :vec3)
                   (light-pos   :vec3))
  (values (v! 15 15 15 1)
          (v! 15 15 15 0)))

(defpipeline-g sun-pipe ()
  :vertex   (vert g-pnt)
  :fragment (sun-frag :vec2 :vec3 :vec3))
