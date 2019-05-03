(in-package #:incandescent)

;;--------------------------------------------------
;; Draw
;;--------------------------------------------------
(defmethod draw ((actor assimp-thing)
                 camera
                 (time single-float))
  (with-slots (buf albedo normals scale specular) actor
    (map-g #'assimp-tex-pipe-simple buf
           :scale scale
           ;; Lighting
           :model-world (model->world actor)
           :world-view (world->view camera)
           :view-clip  (projection camera)
           ;; PBR
           :cam-pos (pos camera)
           :albedo albedo
           :time time
           :specular specular
           :normals normals)))

(defmethod draw ((actor assimp-thing-with-bones)
                 camera
                 (time single-float))
  (with-slots (buf albedo normals scale bones) actor
    (map-g #'assimp-tex-pipe-bones buf
           :scale scale
           ;; Lighting
           :model-world (model->world actor)
           :world-view (world->view camera)
           :view-clip  (projection camera)
           :offsets bones
           ;; PBR
           :albedo albedo
           :normals normals)))
