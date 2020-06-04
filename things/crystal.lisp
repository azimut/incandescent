(in-package #:incandescent)

;; https://www.patreon.com/posts/tutorial-making-28404194

(defparameter *blend-crystal* (make-blending-params))

(defparameter *rim-color1* (v! .1 .7 0))
(defparameter *rim-color2* (v! 0 .9 .1))
(defparameter *rim-color-blend* .4)
(defparameter *emission-color* (v! 1 1 1))

(defparameter *rim-power* 1f0
  "Default: .1  Range: 0f0   - 2f0")
(defparameter *rim-smooth* .9f0
  "Default: 1f0 Range: .5f0  - 1f0")
(defparameter *alpha* .3f0)
(defparameter *alpha-height* .1
  "Default: .35 Range: -1f0  - 1f0")
(defparameter *alpha-smooth* .9
  "Default: .9  Range: 0.1f0 - 1f0")
(defparameter *crystal-power* 1.17f0
  "Default: 1.5 Range: 0f0   - 5f0")

(defclass crystal (actor)
  ((scene :initarg :scene)
   (albedo :initarg :albedo)
   (base-color :initarg :base-color)))

(defmethod update ((actor crystal) dt)
  (with-slots (rot) actor
    ;;(setf rot (q:from-axis-angle (v! (random 1f0) 0 (random 1f0)) (+ -70 (random 140f0))))
    ))

(defun make-crystal (&key (pos (v! 0 0 0))
                          (base-color (v! 1 1 1))
                          (color (v! 1 1 1))
                          (rot (q:identity))
                          (scale 1f0))
  (destructuring-bind (&key buf scene &allow-other-keys)
      (nth 0 (assimp-load-meshes "static/_crystal/center-crystal.obj"))
    (let ((obj (make-instance 'crystal :buf buf :scene scene
                                       :base-color base-color
                                       :rot rot
                                       :albedo (get-tex "static/_crystal/Crystal_Diffuse.png" nil t :rgb8)
                                       :pos pos :scale scale
                                       :color color)))
      (push obj *actors*)
      obj)))

(defun-g crystal-frag ((uv :vec2) (frag-norm :vec3) (frag-pos :vec3)
                       &uniform
                       (albedo        :sampler-2d)
                       (base-color    :vec3)
                       (color         :vec3)
                       (cam-pos       :vec3)
                       (model-world   :mat4)
                       (light-color   :vec3)
                       (light-pos     :vec3)
                       (alpha         :float)
                       (alpha-height  :float)
                       (alpha-smooth  :float)
                       (emission-color :vec3)
                       (rim-color1     :vec3)
                       (rim-color2     :vec3)
                       (rim-color-blend :float)
                       (rim-smooth    :float)
                       (rim-power     :float)
                       (crystal-power :float))
  (let* (;;#+nil
         (view-dir       (normalize (- cam-pos frag-pos)))
         (albedo (x (pow (s~ (texture albedo uv) :xyz) (v3! 2.2))))
         (light-dir   (normalize (- light-pos frag-pos)))
         ;; (d           (pow (dot frag-norm light-dir) .3))
         ;; (half-dir    (normalize (+ light-dir view-dir)))
         ;; (gloss-power .1)
         ;; (gloss .1)
         ;; (half-dot    (pow (dot frag-norm half-dir) gloss))
         ;; (gloss-smoothness .2)
         ;; (specular 1f0)
         ;; (gloss (* specular (smoothstep .5 (max .5 gloss-smoothness) half-dot)))
         ;; (final-color (mix (v3! d) light-color (v3! gloss)))
         ;;
         (local-pos      (- frag-pos (s~ (* model-world (v! 0 0 0 1))
                                         :xyz)))
         (alpha          (max .01 alpha))
         (fade-alpha     (mix 1 alpha (+ (y local-pos) alpha-height)))
         (fade-alpha     (max alpha
                              (smoothstep alpha
                                          (max alpha alpha-smooth)
                                          fade-alpha)))
         ;; Emmisive
         (d              (- 1 (pow (dot frag-norm view-dir) rim-power)))
         (rim            (smoothstep .5 (max .5 rim-smooth) d))
         (rim-color      (mix rim-color1 rim-color2 fade-alpha))
         ;; "alpha will be no less strong than the rim."
         (fade-alpha     (mix fade-alpha 1f0 rim))
         ;;
         (mix-color      (mix color (* color base-color) fade-alpha))
         ;;
         ;;#+nil
         (final-color    (dir-light-apply-spec (v3! albedo)
                                               light-color light-pos
                                               frag-pos frag-norm
                                               cam-pos .9 .4))
         ;; does not get affected by external lighting
         (emission       (+ (* mix-color emission-color albedo)
                            ;; * albedo * emmisive-map
                            (* rim-color rim)))
         ;;
         (final-color    (* final-color
                            (pow fade-alpha (- 1 crystal-power)))))
    (values (v! final-color fade-alpha)
            (v! emission 1))
    #+nil
    (values (v! final-color fade-alpha)
            (if (> (dot final-color (v! .2126 .7152 .0722)) 1f0)
                (v! final-color fade-alpha)
                (v! 0 0 0 1)))
    #+nil
    (values (v! (v3! d) fade-alpha)
            (if (> (dot (v3! d) (v! .2126 .7152 .0722)) 1f0)
                (v! (v3! d) fade-alpha)
                (v! 0 0 0 1)))))

(defun-g brightness-p ((color :vec3))
  (> (dot color (v! .2126 .7152 .0722))
     1f0))

(defpipeline-g crystal-pipe ()
  :vertex   (vert g-pnt)
  :fragment (crystal-frag :vec2 :vec3 :vec3))

(defmethod draw ((actor crystal) camera time)
  (with-setf (cull-face) nil
    (with-blending *blend-crystal*
      (with-slots (base-color albedo color buf scale) actor
        (map-g #'crystal-pipe buf
               :albedo albedo
               :alpha-height *alpha-height*
               :alpha-smooth *alpha-smooth*
               :emission-color *emission-color*
               :crystal-power *crystal-power*
               :rim-color1 *rim-color1*
               :rim-color2 *rim-color2*
               :rim-color-blend *rim-color-blend*
               :rim-smooth  *rim-smooth*
               :rim-power *rim-power*
               :alpha *alpha*
               :scale scale
               :light-color *light-color*
               :light-pos *light-pos*
               :cam-pos (pos camera)
               :base-color base-color
               :color color
               :model-world (model->world actor)
               :world-view  (world->view  camera)
               :view-clip   (projection   camera))))))

(progn (free-actors)
       (make-crystal :color (v! .1 .3 .03)
                     :base-color (v! .3 .9 .1))
       (make-clouds)
       ;;(make-piso)
       )
