(in-package #:incandescent)

(defclass piso (pbr) ())

(defun make-piso (&key (pos (v! 0 0 0)) (rot (q:identity))
                       (scale 1f0)
                       (buf (lattice 100 100 2 2 t))
                       (uv-repeat (v! 1 1)))
  (let ((obj (make-instance 'piso :buf buf
                                  :pos pos
                                  :scale scale
                                  :rot rot)))
    (push obj *actors*)
    obj))

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
           :roughness .9
           :metallic metallic
           ;; IBL
           :brdf-lut *s-brdf*
           :prefilter-map *s-cubemap-prefilter*
           :irradiance-map *s-cubemap-live*)))
