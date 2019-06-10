(in-package #:incandescent)

(defclass water (actor)
  ((albedo :initarg :albedo)))

(progn (defun test ()
         (free-actors)
         ;;(reset-camera)
         (make-water))
       ;;(test)
       )
(defun make-water (&key (pos (v! 0 0 0))
                        (rot (q:identity)))
  (let ((obj (make-instance 'water
                            :pos pos :rot rot
                            :albedo (get-tex "static/uv.png")
                            :buf (get-quad-stream-v3))))
    (push obj *actors*)
    obj))

(defmethod update ((actor water) dt))

(defmethod draw ((actor water) camera (time single-float))
  (with-slots (buf scale color albedo) actor
    (map-g #'water-pipe buf
           :time  time
           :cam-pos (pos camera)
           :albedo albedo
           :model-world (model->world actor)
           :world-view  (world->view camera)
           :view-clip   (projection  camera)
           :light-color *light-color*
           :light-pos   *light-pos*)))

;;--------------------------------------------------

(defun-g water-vert ((pos         :vec3)
                     &uniform
                     (model-world :mat4)
                     (world-view  :mat4)
                     (view-clip   :mat4))
  (let* ((world-pos (* model-world (v! (* 5 pos) 1)))
         (view-pos  (* world-view world-pos))
         (clip-pos  (* view-clip  view-pos)))
    (values clip-pos
            (+ .5 (* .5 (v! (x pos) (- (y pos)))))
            (s~ world-pos :xyz)
            (v! 0 1 0))))

(defun-g flow-uv ((uv :vec2) (time :float))
  (+ uv time))

(defun-g water-frag ((uv :vec2) (frag-pos :vec3) (frag-normal :vec3)
                     &uniform
                     (time :float)
                     (color :vec3)
                     (albedo :sampler-2d)
                     (cam-pos :vec3)
                     (light-color :vec3) (light-pos :vec3))
  (let* ((color (texture albedo (* 4 (flow-uv uv (* .001 time)))))
         (color3 (pow (s~ color :xyz) (v3! 2.2))))
    (v! color3 (w color))))

(defpipeline-g water-pipe ()
  :vertex   (water-vert :vec3)
  :fragment (water-frag :vec2 :vec3 :vec3))
