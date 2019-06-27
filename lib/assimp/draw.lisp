(in-package #:incandescent)

;;--------------------------------------------------
;; Draw
;;--------------------------------------------------
(defmethod draw ((actor assimp-thing) camera (time single-float))
  (with-slots (buf albedo normals scale specular) actor
    (map-g #'assimp-tex-pipe-simple buf
           :scale scale
           :time time
           ;; Lighting
           :light-pos *light-pos*
           :light-color *light-color*
           :cam-pos (pos camera)
           ;;
           :model-world (model->world actor)
           :world-view (world->view  camera)
           :view-clip (projection   camera)
           :albedo albedo
           :specular specular
           :normals normals)))

(defmethod draw ((actor assimp-thing-with-bones) camera (time single-float))
  (with-slots (buf albedo normals scale bones) actor
    (map-g #'assimp-tex-pipe-bones buf
           :scale scale
           :offsets bones
           ;;
           :model-world (model->world actor)
           :world-view (world->view camera)
           :view-clip (projection camera)
           ;; PBR
           :albedo albedo
           :normals normals)))
