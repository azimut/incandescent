(defclass lotus (actor)
  ((albedo :initarg :albedo)
   (scene  :initarg :scene)))

(defun make-lotus (&key (pos (v! 0 0 0))
                        (rot (q:identity))
                        (scale .1))
  (mapcar (op (destructuring-bind (&key buf scene &allow-other-keys)
                  (let ((*processing-flags*
                          '(:ai-process-triangulate
                            ;;:ai-process-preset-target-realtime-max-quality
                            :ai-process-calc-tangent-space)))
                    _)
                (let ((obj (make-instance
                            'lotus
                            :buf buf :scene scene
                            :albedo (get-tex "static/_lotus/lotus_petal_diffuse.jpg")
                            :pos pos :rot rot :scale scale)))
                  (push obj *actors*)
                  obj)))
          (assimp-load-meshes "static/_lotus/lotus_OBJ_low.obj")))

(progn (free-actors)
       ;;(free-assimp-buffers)
       (make-lotus :pos (v3:+ (v! -10 .5 -10)
                              (v! 0 -.5 -10))
                   :scale 1f0))

(defmethod draw ((actor lotus) camera time)
  (with-slots (buf albedo normals scale) actor
    (map-g #'pres-pipe buf
           :scale scale
           :uv-repeat (v! 1 1)
           :time time
           ;; Lighting
           :light-pos *light-pos*
           :light-color *light-color*
           :cam-pos (pos camera)
           ;;
           :model-world (model->world actor)
           :world-view (world->view  camera)
           :view-clip (projection   camera)
           :albedo albedo)))

