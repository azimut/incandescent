(in-package :incandescent)

(defvar *chuesos* nil)

(defmethod draw ((actor box) camera (time single-float))
  (with-slots (buf scale color) actor
    (map-g #'generic-shadow-pipe buf
           :scale scale
           :color color
           :cam-pos (pos camera)
           :model-world (model->world actor)
           :world-view  (world->view camera)
           :view-clip   (projection  camera)
           ;; Shadowmap
           :shadowmap *shadow-sam*
           :light-world (world->view *shadow-camera*)
           :light-clip  (projection *shadow-camera*)
           ;; Directional light (for the most part)
           :light-color *light-color*
           :light-pos   *light-pos*)))

(defmethod draw ((actor piso) camera (time single-float))
  (with-slots (buf scale color
               albedo normal height roughness
               ;;uv-speed uv-repeat
               ao metallic)
      actor
    (map-g #'shadow-pbr-pipe buf
           :scale scale
           :color color
           :uv-repeat 1f0
           :uv-speed 1f0
           :samd *samd*
           ;; Lighting
           :cam-pos (pos camera)
           :light-pos *light-pos*
           ;;
           :model-world (model->world actor)
           :world-view (world->view camera)
           :view-clip  (projection camera)
           ;; Shadowmap
           :shadowmap *shadow-sam*
           :light-world (world->view *shadow-camera*)
           :light-clip (projection *shadow-camera*)
           ;; PBR
           :albedo albedo
           :ao-map ao
           :metallic metallic
           :normal-map normal
           :height-map height
           :rough-map roughness
           ;; IBL
           :brdf-lut *s-brdf*
           :prefilter-map *s-cubemap-prefilter*
           :irradiance-map *s-cubemap-live*)))

;; NOTE: this one draws different things one the shadowmap whether has or not bones
;; needed for bone transforms happening on vertex shader.
#+nil
(defun draw-shadowmap ()
  (with-fbo-bound (*shadow-fbo* :attachment-for-size :d)
    (clear *shadow-fbo*)
    (loop :for actor :in *actors*
          :do (with-slots (buf scale) actor
                (case (class-name-of actor)
                  (assimp-thing-with-bones
                   (map-g #'simplest-3d-pipe-bones buf
                          :scale 1f0
                          :offsets *chuesos*
                          :model-world (model->world actor)
                          :world-view  (world->view *shadow-camera*)
                          :view-clip   (projection  *shadow-camera*)))
                  (piso
                   (map-g #'simplest-3d-pipe buf
                          :scale 1f0
                          :model-world (model->world actor)
                          :world-view  (world->view *shadow-camera*)
                          :view-clip   (projection  *shadow-camera*))))))))
