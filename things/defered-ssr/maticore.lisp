(in-package #:incandescent)

(defclass manticore (actor) ())

(defun make-manticore (&key (pos (v! 0 5 0))
                            (color (v! 1 .3 .9))
                            (rot (q:identity))
                            (scale 1f0))
  (let ((obj (make-instance 'manticore :scale scale
                                       :color color
                                       :pos pos :rot rot :buf (sphere))))
    (push obj *actors*)
    obj))

(defmethod update ((actor manticore) dt)
  ;;#+nil
  (with-slots (seed pos) actor
    (let ((time (* seed .1 (mynow))))
      (setf pos (v! (* 5f0 seed (sin time))
                    (+ 3 (* 4f0  seed (cos time)))
                    (* 5f0 seed (cos time)))))))

(defmethod draw ((actor manticore) camera time)
  (with-slots (buf scale color) actor
    (map-g #'manticore-pipe buf
           :color color
           :scale scale
           :model-world (model->world actor)
           :world-view  (world->view camera)
           :view-clip   (projection  camera))))

(defun-g manticore-frag ((uv             :vec2)
                         (frag-normal    :vec3)
                         (frag-pos       :vec3)
                         &uniform
                         (color         :vec3))
  (let* (;;#+nil
         (ao 1f0)
         (spec .7)
         (rough .1)
         (albedo color))
    (values (v! albedo rough)
            (v! frag-pos ao)
            (v! frag-normal spec)
            .9)))

(defpipeline-g manticore-pipe ()
  :vertex   (vert g-pnt)
  :fragment (manticore-frag :vec2 :vec3 :vec3))

;;--------------------------------------------------

(defun-g manticore-ssr-frag ((uv          :vec2)
                             (frag-normal :vec3)
                             (frag-pos    :vec3)
                             &uniform
                             (color       :vec3)
                             (scene-sam   :sampler-2d)
                             (ssr-sam     :sampler-2d))
  (let* ((ssr (s~ (texture ssr-sam uv) :xyz))
         (reflection (s~ (texture scene-sam (s~ ssr :xy)) :xyz))
         (ao 1f0)
         (spec .01)
         (rough .9)
         (albedo color))
    ;;(v! 0 1 0 1)
    ;;#+nil
    (v! (mix albedo
             reflection
             (* .5 (z ssr)))
        1)))

(defpipeline-g manticore-ssr-pipe ()
  :vertex   (vert g-pnt)
  :fragment (manticore-ssr-frag :vec2 :vec3 :vec3))
