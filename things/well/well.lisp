(in-package :incandescent)

(defclass well (actor) ())

(defun make-well (&key (pos (v! 0 0 0))
                       (rot (q:identity))
                       (radius .5)
                       (height 1f0))
  (let ((obj (make-instance 'well :pos pos :rot rot
                                  :buf (cylinder radius height t))))
    (push obj *actors*)
    obj))

(defmethod update ((actor well) dt))

(progn (free-actors)
       ;;(reset-camera)
       (make-well :radius 5f0 :height 20f0
                  :pos (v! 0 -10 0)))

(defmethod draw ((actor well) camera (time single-float))
  (with-setf* ((cull-face) :front)
    (with-slots (buf scale color) actor
      (map-g #'well-pipe buf
             :scale scale
             :color color
             :time  time
             :cam-pos (pos camera)
             :model-world (model->world actor)
             :world-view  (world->view camera)
             :view-clip   (projection  camera)
             :light-color *light-color*
             :light-pos   *light-pos*))))

(defun-g well-frag ((uv :vec2) (frag-pos :vec3) (frag-normal :vec3)
                    &uniform
                    (time        :float)
                    (color       :vec3)
                    (cam-pos     :vec3)
                    (light-color :vec3)
                    (light-pos   :vec3))
  (v! uv 1 0))

(defpipeline-g well-pipe ()
  :vertex   (vert g-pnt)
  :fragment (well-frag :vec2 :vec3 :vec3))
