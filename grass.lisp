(in-package :incandescent)

(defclass grass (actor)
  ((sam :initarg :sam)
   (buf :initarg :buf)))

(defun make-grass ()
  (let ((obj (make-instance
              'grass
              ;; TODO: yikes
              :buf (caar
                    (let ((*processing-flags* '(:ai-process-triangulate
                                                ;;:ai-process-flip-u-vs
                                                :ai-process-preset-target-realtime-quality
                                                :ai-process-calc-tangent-space)))
                      (assimp-load-meshes
                       "/home/sendai/Downloads/ga/Grass_Mesh.fbx"
                       :instantiate-p nil)))
              :sam (get-tex "/home/sendai/Downloads/ga/Green_Grass_AlbedoA.tga"))))
    (push obj *actors*)
    obj))

(defmethod draw ((actor grass) camera time)
  (with-slots (sam buf scale) actor
    (with-blending *blend*
      (map-g #'tex-pipe buf
             :albedo sam
             :scale .05
             :cam-pos (pos camera)
             :light-pos *light-pos*
             :model-world (model->world actor)
             :world-view  (world->view camera)
             :view-clip   (projection camera)))))
