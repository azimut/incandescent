(in-package :incandescent)

(progn (setf *actors* nil)
       (make-sky)
       (make-pinkf)
       (make-desk)
       ;; (make-walls :pos (v! 6 0 0)
       ;;             :rot (q:from-axis-angle (v! 0 0 90) (radians 90))
       ;;             )
       ;; (make-walls :pos (v! -6 0 0)
       ;;             :rot (q:from-axis-angle (v! 0 0 90) (radians -90))
       ;;             )
       (reset-camera *camera* (v! 0 0 20)))

(defclass walls (actor) ())
(defmethod update ((actor walls) dt))
(defun make-walls (&key (pos (v! 0 0 0))
                        (rot (q:identity)))
  (let ((obj (make-instance
              'walls
              :buf (lattice)
              :pos pos
              :rot rot)))
    (push obj *actors*)
    obj))

(defun-g walls-frag ((uv :vec2)
                     (frag-norm :vec3)
                     (frag-pos :vec3)
                     &uniform
                     (light-pos :vec3)
                     (cam-pos :vec3)
                     (albedo :sampler-2d))
  (v! 0 1 0 1))

(defpipeline-g walls-pipe ()
  (vert g-pnt)
  (walls-frag :vec2 :vec3 :vec3))

(defmethod draw ((actor walls) camera time)
  (with-slots (buf scale) actor
    (map-g #'walls-pipe buf
           :scale scale
           :model-world (model->world actor)
           :world-view (world->view camera)
           :view-clip  (projection camera))))
