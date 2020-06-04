(in-package #:incandescent)

(defclass grave (actor)
  ((albedo :initarg :albedo)
   (scene  :initarg :scene)))

(defun make-grave (&key (pos (v! 0 0 0))
                        (rot (q:identity))
                        (scale .1))
  (destructuring-bind (&key buf scene &allow-other-keys)
      (let ((*processing-flags*
              '(:ai-process-triangulate
                :ai-process-preset-target-realtime-quality
                :ai-process-calc-tangent-space)))
        (nth 0 (assimp-load-meshes "static/_grave/grave1_low.obj")))
    (let ((obj (make-instance
                'grave
                :buf buf :scene scene
                :albedo (get-tex "static/_grave/128grave1_low_Diffuse.png")
                :pos pos :rot rot :scale scale)))
      (push obj *actors*)
      obj)))

(defmethod draw ((actor grave) camera time)
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
