(in-package #:incandescent)

(defun render-all-the-things (actor camera time)
  (declare (single-float time))
  (update actor time)
  (draw actor camera time))

(defgeneric draw (actor camera time))
(defmethod draw (actor camera time))

(defmethod draw ((actor box) camera (time single-float))
  (with-slots (buf scale color) actor
    (map-g #'generic-pipe buf
           :scale scale
           :color color
           :time  time
           :cam-pos (pos camera)
           :model-world (model->world actor)
           :world-view  (world->view camera)
           :view-clip   (projection  camera)
           ;; Directional light (for the most part)
           :light-color *light-color*
           :light-pos   *light-pos*
           :brdf-luf *s-brdf*
           :irradiance-map *s-cubemap-live*
           :prefilter-map *s-cubemap-prefilter*
           )))

(defmethod draw ((actor pbr) camera (time single-float))
  (with-slots (buf
               color
               albedo normal height roughness
               uv-speed
               scale ao uv-repeat metallic)
      actor
    (map-g #'pbr-pipe buf
           :uv-repeat uv-repeat
           :uv-speed uv-speed
           :scale scale
           :time time
           :color color
           :samd *samd*
           ;; Lighting
           :cam-pos (pos camera)
           :cam-dir (q:to-direction (rot *camera*)) ;; flashlight
           :light-pos *light-pos*
           ;;
           :model-world (model->world actor)
           :world-view (world->view camera)
           :view-clip  (projection camera)
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

(defmethod draw ((actor pbr-simple) camera (time single-float))
  (with-slots (buf scale color roughness metallic) actor
    (map-g #'pbr-simple-pipe buf
           :scale scale
           :color color
           :time time
           ;; Lighting
           :cam-pos (pos camera)
           :light-pos *light-pos*
           ;;
           :model-world (model->world actor)
           :world-view (world->view camera)
           :view-clip  (projection camera)
           ;; PBR
           :roughness .1
           :metallic .9
           ;; IBL
           :brdf-lut *s-brdf*
           :prefilter-map *s-cubemap-prefilter*
           :irradiance-map *s-cubemap-live*)))

;;--------------------------------------------------

(defmethod draw ((actor piso) camera (time single-float))
  (with-slots (buf scale color roughness metallic) actor
    (map-g #'pbr-simple-pipe buf
           :scale scale
           :color color
           :time time
           ;; Lighting
           :cam-pos (pos camera)
           :light-pos *light-pos*
           ;;
           :model-world (model->world actor)
           :world-view (world->view camera)
           :view-clip  (projection camera)
           ;; PBR
           :roughness .1
           :metallic .9
           ;; IBL
           :brdf-lut *s-brdf*
           :prefilter-map *s-cubemap-prefilter*
           :irradiance-map *s-cubemap-live*)))
