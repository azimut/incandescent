(in-package :incandescent)

(defclass physic-ball (physic)
  ((radius  :initarg :radius  :documentation "double, for mass and geometry"))
  (:default-initargs
   :radius .5d0))

(defmethod initialize-instance :after ((obj physic-ball) &key)
  (with-slots (mass body geom density radius pos rot immovablep) obj
    (setf geom (%ode:create-sphere *space* radius))
    (unless immovablep
      (claw:c-let ((m %ode:mass :from mass))
        (%ode:mass-set-sphere (m &) density radius)
        (%ode:body-set-mass body (m &))
        (%ode:geom-set-body geom body)))
    (ode-update-pos obj pos)
    (ode-update-rot obj rot)))

(defun make-physic-ball (&key (pos (v! 0 0 0))
                              (rot (q:identity))
                              (color (v! 1 1 1))
                              (density 1d0)
                              (radius .5d0)
                              immovablep)
  (declare (type double-float radius density)
           (type boolean immovablep))
  (let ((obj (make-instance 'physic-ball
                            :pos pos
                            :rot rot
                            :color color
                            :immovablep immovablep
                            :buf (sphere (coerce radius 'single-float))
                            :radius radius :density density)))
    (push obj *actors*)
    obj))

(defmethod update ((actor physic-ball) dt)
  "updates visual representation from ODE value"
  (when *world*
    (with-slots (pos orot rot geom immovablep) actor
      (unless immovablep
        (setf pos (ode-geom-get-position    geom))
        (setf rot (ode-geom-get-quaternion2 orot geom))))))

(defmethod draw ((actor physic-ball) camera (time single-float))
  (with-slots (buf scale color) actor
    (map-g #'pbr-ball-pipe buf
           :scale scale
           :color color
           :shadowmap *shadow-sam*
           :light-world (world->view *shadow-camera*)
           :light-clip  (projection *shadow-camera*)
           :cam-pos (pos camera)
           :model-world (model->world actor)
           :world-view  (world->view camera)
           :view-clip   (projection  camera)
           ;;
           :brdf-lut       *s-brdf*
           :prefilter-map  *s-cubemap-prefilter*
           :irradiance-map *s-cubemap-live*
           ;; Directional light (for the most part)
           ;;:light-color *light-color*
           :light-pos   *light-pos*)))

;;--------------------------------------------------

(defun-g pbr-ball-frag ((uv :vec2) (frag-norm :vec3) (frag-pos :vec3) (light-clip-pos :vec4)
                        &uniform
                        (time      :float)
                        (cam-pos   :vec3)
                        (light-pos :vec3)
                        ;;
                        (shadowmap :sampler-2d)
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
                (* 3 (pbr-point-lum (v! 0
                                        10
                                        0)
                                    frag-pos
                                    v n
                                    roughness
                                    f0
                                    metallic
                                    color
                                    specular
                                    .35 .44))))
         (ambient (v3! .03))
         ;;#+nil
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
    ;;ambient
    ;;(v! (+ .001 lo) 1)
    ;;(* (shadow-factor shadowmap light-clip-pos) lo)
    ;;(* (shadow-factor shadowmap light-clip-pos) final-color)
    ))


(defpipeline-g pbr-ball-pipe ()
  :vertex   (shadow-vert g-pnt)
  :fragment (pbr-ball-frag :vec2 :vec3 :vec3 :vec4))

;;--------------------------------------------------
(progn (defun test ()
         ;;(free-actors)
         ;; (ode-destroy)
         ;; (ode-init)
         ;;(make-piso)
         (dotimes (i 50)
           (make-physic-ball
            ;;:immovablep t
            :color (v3! .5 0 0)
            :rot (q:from-axis-angle (v! (random 1f0)
                                        (random 1f0)
                                        (random 1f0))
                                    (radians (random 360f0)))
            :pos (v! (serapeum:random-in-range -5 5)
                     0
                     (serapeum:random-in-range -13 -7)))))
       (test))
