(in-package #:incandescent)

(defvar *pointlight-pos* (v! 0 0 0))

(defparameter *light-pos* (v! 7.545997 2.553596 5.4780827))
(defparameter *light-color* (v! 0.78823537 0.8862746 1.0))

(defparameter *parallax-scale* .01f0)

(defparameter *light-color* (v! 0.78823537 0.8862746 1.0))
(defparameter *light-color* (v! 1.0 0.8196079 0.69803923))
(defparameter *light-color* (v! 0.2509804 0.6117647 1.0))
(defparameter *light-color* (v! 1.0 0.9568628 0.8980393))

;; http://planetpixelemporium.com/tutorialpages/light.html
;; Candle           1900 255  147   41   (v! 1.0 0.5764706 0.16078432)
;; 40W Tungsten     2600 255  197  143   (v! 1.0 0.7725491 0.56078434)
;; 100W Tungsten    2850 255  214  170   (v! 1.0 0.83921576 0.6666667)
;; Halogen          3200 255  241  224   (v! 1.0 0.9450981 0.87843144)
;; Carbon Arc       5200 255  250  244   (v! 1.0 0.9803922 0.9568628)
;; High Noon Sun    5400 255  255  251   (v! 1.0 1.0 0.9843138)
;; Direct Sunlight  6000 255  255  255   (v! 1.0 1.0 1.0)
;; Overcast Sky     7000 201  226  255   (v! 0.78823537 0.8862746 1.0)
;; Clear Blue Sky  20000  64  156  255   (v! 0.2509804 0.6117647 1.0)
;;
;; Warm Fluorescent          255 244 229 (v! 1.0 0.9568628 0.8980393)
;; Standard Fluorescent      244 255 250 (v! 0.9568628 1.0 0.9803922)
;; Cool White Fluorescent    212 235 255 (v! 0.8313726 0.9215687 1.0)
;; Full Spectrum Fluorescent 255 244 242 (v! 1.0 0.9568628 0.9490197)
;; Grow Light Fluorescent    255 239 247 (v! 1.0 0.93725497 0.9686275)
;; Black Light Fluorescent   167 0 255   (v! 0.654902 0.0 1.0)
;; Mercury Vapor             216 247 255 (v! 0.8470589 0.9686275 1.0)
;; Sodium Vapor              255 209 178 (v! 1.0 0.8196079 0.69803923)
;; Metal Halide              242 252 255 (v! 0.9490197 0.98823535 1.0)
;; High Pressure Sodium      255 183 76  (v! 1.0 0.7176471 0.29803923)

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
