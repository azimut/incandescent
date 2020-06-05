(in-package #:incandescent)

(defclass pbr-simple (actor)
  ((roughness :initarg :roughness)
   (metallic  :initarg :metallic)
   (specular  :initarg :specular))
  (:default-initargs
   :roughness .1
   :metallic  .1
   :specular  .1))

(defun make-pbr-simple (&key (pos (v! 0 0 0))
                             (color (v! 1 1 1)))
  (declare (type rtg-math.types:vec3 pos color))
  (let ((obj (make-instance
              'pbr-simple
              :color color
              :buf (sphere)
              :pos pos)))
    (push obj *actors*)
    obj))

;;--------------------------------------------------

(defmethod update ((actor pbr-simple) dt)
  #+n
  (with-slots (metallic specular roughness) actor
    (setf metallic  .2
          specular  .0
          roughness .4))
  #+nil
  (with-slots (pos seed) actor
    (let ((time (* 2 (mynow))))
      (setf (y pos) (* 25 seed (sin (* seed .5 time))))
      (setf (x pos) (* 10 (+ seed .3) (sin (* (+ seed .03) .7 time))))
      (setf (z pos) (* 5 (+ seed .4) (cos (* (+ seed .02) .5 time)))))))

;;--------------------------------------------------

(defmethod draw ((actor pbr-simple) camera (time single-float))
  (with-slots (buf scale color roughness metallic specular) actor
    (map-g #'pbr-simple-pipe buf
           :scale scale
           :color color
           :time time
           ;; Lighting
           :cam-pos (pos camera)
           :light-pos *light-pos*
           ;;
           :model-world (model->world actor)
           :world-view  (world->view  camera)
           :view-clip   (projection   camera)
           ;; PBR
           :roughness roughness
           :metallic  metallic
           :specular  specular
           ;; IBL
           :brdf-lut       *s-brdf*
           :prefilter-map  *s-cubemap-prefilter*
           :irradiance-map *s-cubemap-live*)))

;;--------------------------------------------------

(defun-g pbr-simple-frag ((uv :vec2) (frag-norm :vec3) (frag-pos :vec3)
                          &uniform
                          (time      :float)
                          (cam-pos   :vec3)
                          (light-pos :vec3)
                          ;; Material
                          (color     :vec3)
                          (roughness :float)
                          (metallic  :float)
                          (specular  :float)
                          ;; IBL
                          (irradiance-map :sampler-cube)
                          (prefilter-map  :sampler-cube)
                          (brdf-lut       :sampler-2d))
  (let* ((normal (normalize frag-norm))
         (ao     1f0)
         (color  color)
         ;;----------------------------------------
         ;; PBR
         ;; metallic
         (f0 (vec3 .04))
         ;;(f0 color)
         (f0 (mix f0 color metallic))
         ;; pbr - reflectance equation
         (n  normal)
         (v  (normalize (- cam-pos frag-pos)))
         (lo (vec3 0f0))
         ;; lights START
         (lo (+ lo
                #+nil
                (pbr-direct-lum light-pos frag-pos
                                v
                                n
                                roughness
                                f0
                                metallic
                                color
                                specular)
                (* 5 (pbr-point-lum (v! 0
                                        18
                                        0)
                                    frag-pos
                                    v n
                                    roughness
                                    metallic
                                    color
                                    specular
                                    .35
                                    .44))))
         ;;(ambient (v3! .03))
         (ambient (ambient-ibl v
                               n
                               f0
                               brdf-lut
                               prefilter-map
                               irradiance-map
                               roughness
                               metallic
                               color
                               ao))
         ;;(ambient (* color ao (vec3 .3)))
         (final-color (+ ambient lo))
         )
    (v! final-color 1)
    ;;lo
    ))

;;----------------------------------------
;; Functions to apply the Irradiance Map ONLY
(defpipeline-g pbr-simple-pipe ()
  :vertex   (vert g-pnt)
  :fragment (pbr-simple-frag :vec2 :vec3 :vec3))
