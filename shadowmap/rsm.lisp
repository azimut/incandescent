(in-package #:incandescent)

;; Reflective Shadow Map - a single bounce GI
;;
;; > This is a "disSsasaster"
;;
;; - It requires a lot of finetunning as it is right now...
;; - and still gives poor results.
;; - Screen space based (ish) so more resolution means more lookups
;; - weid on flat walls
;;
;; PARAMETERS
;; ==========
;; - Multiplier for final indirect
;; - width of sample
;; - random generated values for ubo
;;
;; USAGE
;; =====
;; - Use pcf (shadow-factor) to calculate shadow.
;; - Use *rsm-ubo* to offset texture lookups.
;;
;; Reference:
;; http://portfolio.schachtschabel.com/rsm.html
;; www.klayge.org/material/3_12/GI/rsm.pdf
;; https://github.com/diharaw/ReflectiveShadowMaps/blob/master/src/shader/indirect_light_fs.glsl
(defvar *rsm-normals* nil)
(defvar *rsm-flux*    nil)
(defvar *rsm-ubo*     nil "random v3 data")
(defparameter *shadow-dimensions* '(512 512)
  "lower for rsm...")

;;--------------------------------------------------

;; ... in varjo/cepl functions does not accept ubos as argument
(defun-g rsm-indirect ((shadowmap             :sampler-2d)
                       (rsm-normals           :sampler-2d)
                       (rsm-flux              :sampler-2d)
                       ;;(rsm-offset rsm-kernel :ubo)
                       (rsm-offset (:vec3 64))
                       (light-coord :vec2) ; in clip
                       (pos :vec3)         ; in world
                       (norm :vec3)        ; in world
                       (light-view-clip       :mat4)
                       (iterations            :int)
                       (sample-radius         :float))
  (let ((indirect (vec3 0)))
    (dotimes (i iterations)
      (let* ((loffset (aref rsm-offset i))
             (l-uv    (+ light-coord (* (s~ loffset :xy)
                                        sample-radius
                                        (/ 512f0))))
             ;;
             (l-pos (s~ (get-view-pos l-uv shadowmap light-view-clip) :xyz))
             (lnor  (normalize (s~ (texture rsm-normals l-uv) :xyz)))
             (lflux (s~ (texture rsm-flux l-uv) :xyz))
             ;;
             (result  (* lflux
                         (max 0f0 (dot lnor (normalize (- pos l-pos))))
                         (max 0f0 (dot norm (normalize (- l-pos pos))))))
             (result  (* result (z loffset) (z loffset))))
        (incf indirect result)))
    indirect))
;;--------------------------------------------------

(defstruct-g (rsm-kernel :layout :std-140)
  (random-v3 (:vec3 64)))

(defun generate-sample-kernel ()
  "runs once, goes to an UBO, 64x3"
  (loop :for i :below 64 :collect
       (let* ((x1 (random 1f0))
              (x2 (random 1f0))
              (x (* x1 (sin (* 2f0 +pi+ x2))))
              (y (* x1 (cos (* 2f0 +pi+ x2)))))
         (v! x y x1))))

;;--------------------------------------------------

(defun free-rsm ()
  (when *shadow-fbo* (free *shadow-fbo*)))

(defun init-rsm ()
  (free-rsm)
  (unless *rsm-ubo*
    (setf *rsm-ubo*
          (make-ubo (list (generate-sample-kernel))
                    'rsm-kernel)))
  ;; 0 - normals
  ;; 1 - flux (* light-color albedo-color))
  ;; NOTE: Use (get-view-pos) to get the world position back from the normals and depth
  (setf *shadow-fbo*  (make-fbo `(0 :dimensions ,*shadow-dimensions* :element-type :rgb16f)
                                `(1 :dimensions ,*shadow-dimensions* :element-type :rgb16f)
                                `(:d :dimensions ,*shadow-dimensions* :element-type :depth-component32)))
  (setf *shadow-sam*  (sample (attachment-tex *shadow-fbo* :d)
                              :wrap           :clamp-to-border
                              :minify-filter  :nearest
                              :magnify-filter :nearest))
  (setf *rsm-normals* (sample (attachment-tex *shadow-fbo* 0) :wrap :clamp-to-border))
  (setf *rsm-flux*    (sample (attachment-tex *shadow-fbo* 1) :wrap :clamp-to-border))
  (setf (cepl.samplers::border-color *shadow-sam*) (v! 1 1 1 1))
  (setf (cepl.samplers::border-color *rsm-flux*) (v! 0 0 0 1))
  (setf (cepl.samplers::border-color *rsm-normals*) (v! 0 0 0 1))
  t)

;; NOTE: Override this frag for proper colors...
(defun-g rsm-3d-frag ((uv          :vec2)
                      (frag-norm   :vec3)
                      (frag-pos    :vec3)
                      &uniform
                      (color       :vec3)
                      (light-color :vec3))
  (values frag-norm
          (* light-color color)))

(defpipeline-g rsm-3d-pipe ()
  :vertex   (vert g-pnt)
  :fragment (rsm-3d-frag :vec2 :vec3 :vec3))

;; NOTE: use *shadow-camera*
(defgeneric draw-rsm (actor camera))
(defmethod draw-rsm (actor camera)
  "draws the scene in *ACTORS* from the point of view of *SHADOW-CAMERA* into *SHADOW-FBO* using a simple shader pipe"
  (with-fbo-bound (*shadow-fbo* :attachment-for-size :d)
    ;;(clear-fbo *shadow-fbo* :d)
    ;;(with-setf (cull-face) :front)
    (with-slots (buf color scale draw-p) actor
      (when draw-p
        (map-g #'rsm-3d-pipe buf
               :color color
               :light-color *light-color*
               :scale scale
               :model-world (model->world actor)
               :world-view  (world->view camera)
               :view-clip   (projection  camera))))))

#+nil
(dotimes (i 10)
  (let* ((loffset (aref (rsm-kernel-random-v3 rsm-offset) i))
         #+nil
         (loffset (v! (nineveh.random:rand (v2! (sin (+ i (* .00000001 time)))))
                      (nineveh.random:rand (v2! (cos (+ i (* .00000001 time)))))
                      (nineveh.random:rand (v2! (* .002f0 (+ i time))))))
         (sample-radius 20f0)
         (l-uv    (+ light-coord (* (s~ loffset :xy)
                                    sample-radius
                                    (/ 512f0))))
         ;;
         (l-pos   (s~ (get-view-pos l-uv shadowmap light-clip) :xyz))
         (lnor    (normalize (s~ (texture rsm-normals l-uv) :xyz)))
         (lflux   (s~ (texture rsm-flux l-uv) :xyz))
         ;;
         (result  (* lflux
                     (max 0f0 (dot lnor (normalize (- pos l-pos))))
                     (max 0f0 (dot norm (normalize (- l-pos pos))))))
         (result  (* result (z loffset) (z loffset)))
         ;;(result  (* result (/ 1f0 20f0)))
         )
    (incf indirect result)))
