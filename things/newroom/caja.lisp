(in-package :incandescent)

(defclass physic-caja (physic)
  ((radius  :initarg :radius  :documentation "double, for mass and geometry")
   (x :initarg :x)
   (y :initarg :y)
   (z :initarg :z))
  (:default-initargs
   :radius .5d0))

(defmethod initialize-instance :after ((obj physic-caja) &key)
  (with-slots (mass body geom density immovablep x y z pos rot) obj
    (setf geom (%ode:create-box *space* x y z))
    (unless immovablep
      (claw:c-let ((m %ode:mass :from mass))
        (%ode:mass-set-box (m &) density x y z)
        (%ode:body-set-mass body (m &))
        (%ode:geom-set-body geom body)))
    (ode-update-pos obj pos)
    (ode-update-rot obj rot)))

(defun make-physic-caja (&key (pos (v! 0 0 0))
                              (rot (q:identity))
                              (color (v! 1 1 1))
                              (density 1d0)
                              (x 1d0)
                              (y 1d0)
                              (z 1d0)
                              immovablep)
  (declare (type double-float  density x y z)
           (type boolean immovablep))
  (let ((obj (make-instance 'physic-caja
                            :pos pos
                            :rot rot
                            :x x :y y :z z
                            :color color
                            :immovablep immovablep
                            :buf (box (coerce x 'single-float)
                                      (coerce y 'single-float)
                                      (coerce z 'single-float))
                            :density density)))
    (push obj *actors*)
    obj))

(defmethod update ((actor physic-caja) dt)
  "updates visual representation from ODE value"
  (when *world*
    (with-slots (pos orot rot geom immovablep) actor
      (unless immovablep
        (setf pos (ode-geom-get-position    geom))
        (setf rot (ode-geom-get-quaternion2 orot geom))))))

(defmethod draw ((actor physic-caja) camera (time single-float))
  (with-slots (buf scale color) actor
    (map-g #'pbr-caja-pipe buf
           :metallic .4
           :roughness .1
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

(defun-g pbr-caja-frag ((uv :vec2) (frag-norm :vec3) (frag-pos :vec3) (light-clip-pos :vec4)
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
                ;;#+nil
                (pbr-direct-lum light-pos frag-pos
                                v
                                n
                                roughness
                                f0
                                metallic
                                color
                                specular)
                #+nil
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
    ;;(v! final-color 1)
    ;;ambient
    ;;(v! (+ .001 lo) 1)
    ;;(* (shadow-factor shadowmap light-clip-pos) lo)
    (* (shadow-factor shadowmap light-clip-pos) final-color)
    ))


(defpipeline-g pbr-caja-pipe ()
  :vertex   (shadow-vert g-pnt)
  :fragment (pbr-caja-frag :vec2 :vec3 :vec3 :vec4))
