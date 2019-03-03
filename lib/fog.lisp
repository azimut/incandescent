(in-package #:incandescent)

;;--------------------------------------------------
;; https://catlikecoding.com/unity/tutorials/rendering/part-14/
;; (Euclidean) Distance (aka radial/range) based fog:
;; more realistic and expensive than using depth
;; TODO: add depth and defered fog versions

(defun-g fog-linear ((frag-pos :vec3)
                     (cam-pos :vec3)
                     (start :float)
                     (end :float))
  (let* ((view-distance (length (- frag-pos cam-pos)))
         (fog-factor
          (+ (* view-distance (/ -1 (- end start)))
             (/ end (- end start)))))
    fog-factor))

(defun-g fog-exp ((frag-pos :vec3)
                  (cam-pos :vec3)
                  (density :float))
  (let ((view-distance (length (- frag-pos cam-pos))))
    (exp2 (- (* view-distance (/ density (log 2)))))))

(defun-g fog-exp2 ((frag-pos :vec3)
                   (cam-pos :vec3)
                   (density :float))
  (let* ((view-distance (length (- frag-pos cam-pos)))
         (fog-density
          (* (/ density (sqrt (log 2))) view-distance))
         (fog-factor
          (exp2 (- (* fog-density fog-density)))))
    fog-factor))

;;--------------------------------------------------
;; Versions that apply the fog and returns the final color

(defun-g fog-linear-apply ((color :vec3)
                           (fog-color :vec3)
                           (frag-pos :vec3)
                           (cam-pos :vec3)
                           (start :float)
                           (end :float))
  (let ((fog-factor (fog-linear frag-pos cam-pos start end)))
    (mix fog-color color (saturate fog-factor))))

(defun-g fog-exp-apply ((color :vec3)
                        (fog-color :vec3)
                        (frag-pos :vec3)
                        (cam-pos :vec3)
                        (density :float))
  (let ((fog-factor (fog-exp frag-pos cam-pos density)))
    (mix fog-color color (saturate fog-factor))))

(defun-g fog-exp2-apply ((color :vec3)
                         (fog-color :vec3)
                         (frag-pos :vec3)
                         (cam-pos :vec3)
                         (density :float))
  (let ((fog-factor (fog-exp2 frag-pos cam-pos density)))
    (mix fog-color color (saturate fog-factor))))

;;--------------------------------------------------
;; http://iquilezles.org/www/articles/fog/fog.htm

;; "For example, the color of the fog can tell us about the strengh of the
;; sun. Even more, if we make the color of the fog not constant but
;; orientation dependant we can introduce an extra level of realism to
;; the image. For example, we can change the typical bluish fog color to
;; something yellowish when the view vector aligns with the sun
;; direction. This gives a very natural light dispersion effect. One
;; would argue that sucha an effect shouldn't be called fog but
;; scattering, and I agree, but in the end of the day one simply has to
;; modify a bit the fog equation to get the effect done."

(defun-g apply-fog ((color :vec3)
                    (density :float)
                    (distance :float) ;; camera to point distance
                    (ray-dir :vec3)   ;; camera to point vector
                    (sun-dir :vec3))  ;; sun light direction
  (let* ((fog-amount (- 1 (exp (* (- distance) density))))
         (sun-amount (max (dot ray-dir sun-dir) 0))
         (fog-color  (mix (v! .5 .6 .7) ;; blueish
                          (v!  1 .9 .7) ;; yellowish
                          (pow sun-amount 8))))
    (mix color fog-color fog-amount)))

;; Modified version, with more generic args (works?)
(defun-g apply-fog ((color :vec3)
                    (density :float)
                    (frag-pos :vec3)
                    (cam-pos :vec3)
                    (sun-pos :vec3))
  (let* ((distance (length (- cam-pos frag-pos)))
         (ray-dir  (normalize (- cam-pos frag-pos)))
         (sun-dir  (normalize (- sun-pos frag-pos)))
         (fog-amount (- 1 (exp (* (- distance) density))))
         (sun-amount (max (dot ray-dir sun-dir) 0))
         (fog-color  (mix (v! .5 .6 .7) ;; blueish
                          (v!  1 .9 .7) ;; yellowish
                          (pow sun-amount 8))))
    (mix color fog-color fog-amount)))

;; Height fog - IQ
(defun-g apply-fog ((color :vec3)
                    (fog-color :vec3)
                    (distance :float)
                    (cam-pos :vec3)
                    (frag-pos :vec3))
  (let* ((a .03) ;; .06 - .002
         (b .3) ;; .3   - .02
         (cam-dir (normalize (- frag-pos cam-pos)))
         (fog-amount (/ (* (/ a b)
                           (exp (* (- (y cam-pos)) b))
                           (- 1 (exp (* (- distance)
                                        (y cam-dir)
                                        b))))
                        (y cam-dir))))
    (mix color fog-color (saturate fog-amount))))

;;--------------------------------------------------
;; http://michael-david-palmer.com/fisa/UDK-2010-07/Engine/Shaders/HeightFogCommon.usf
;; https://docs.unrealengine.com/en-us/Engine/Actors/FogEffects/HeightFog
;; Calculates fogging from exponential height fog,
;; returns fog color in rgb, fog factor in a.
;; Fog Height Falloff: Height density factor, controls how the density
;;   increases as height decreases. Smaller values make the
;;   transition larger.
;; x - FogDensity *
;;     exp2(-FogHeightFalloff *
;;          (CameraWorldPosition.z - FogHeight))
;; y - FogHeightFalloff
;; z - CosTerminatorAngle
(defun-g get-exponential-height-fog ((frag-pos :vec3)
                                     (cam-pos :vec3)
                                     (fog-params :vec3)
                                     (light-pos :vec3))
  (let* ((cam-to-receiver (- frag-pos cam-pos))
         (line-integral (* (x fog-params) (length cam-to-receiver)))
         (line-integral
          (if (> (abs (z cam-to-receiver)) .0001)
              (* line-integral (/ (- 1 (exp2 (* (- (y fog-params)) (z cam-to-receiver))))
                                  (* (y fog-params) (z cam-to-receiver))))
              line-integral))
	 ;; 1 in the direction of the light vector, -1 in the opposite direction
         (cos-light-angle (dot (normalize (- light-pos frag-pos))
                               (normalize cam-to-receiver)))
         (fog-color
          (if (< cos-light-angle (z fog-params))
              (mix (v! .5 .6 .7)
                   (* .5 (+ (v! .5 .6 .7) (v! .1 .1 .1)))
                   (vec3 (saturate (/ (1+ cos-light-angle)
                                      (1+ (z fog-params))))))
              (let ((alpha (saturate (/ (- cos-light-angle (z fog-params))
                                        (- 1 (z fog-params))))))
                (mix (* .5 (+ (v! .5 .6 .7) (v! .1 .1 .1)))
                     (v! .1 .1 .1)
                     (vec3 (* alpha alpha))))))
         (fog-factor (saturate (exp2 (- line-integral)))))
    (v! (* fog-color (- 1 fog-factor)) fog-factor)))


;;--------------------------------------------------
;; Defered fog
;; https://github.com/Unity-Technologies/PostProcessing/
;; Hardcoded to work ONLY with perspective projection
(defun-g linear-01-depth ((z :float))
  (let* ((far 1000f0)
         (near .1)
         (z (* z (- 1 (/ far near)))))
    (/ (+ z (/ far near)))))
;; PostProcessing/Shaders/StdLib.hlsl
;; PostProcessing/Shaders/Builtins/DeferredFog.shader
;; PostProcessing/Shaders/API/OpenGL.hlsl
;; PostProcessing/Shaders/Builtins/Fog.hlsl
(defun-g compute-fog-distance ((depth :float))
  (let* ((far 400f0)
         (near .1)
         (dist (- (* depth far) near)))
    dist))
;;
(defun-g compute-fog-exp2 ((z :float)
                           (density :float))
  (let* ((fog 0f0)
         (fog (* density z))
         (fog (exp2 (* (- fog) fog))))
    (saturate fog)))

(defun-g defered-fog ((fog-color :vec3)
                      (uv :vec2)
                      (tex :sampler-2d)
                      (depth-tex :sampler-2d)
                      (density :float))
  (let* ((color (s~ (texture tex uv) :xyz))
         (depth (x (texture depth-tex uv)))
         (depth (linear-01-depth depth))
         (dist  (compute-fog-distance depth))
         (fog   (- 1f0 (compute-fog-exp2 dist
                                         density))))
    (mix color fog-color fog)))

;;--------------------------------------------------
;; https://github.com/SlightlyMad/VolumetricLights/blob/master/Assets/Shaders/VolumetricLight.shader
;; height-fog:
;; x:  ground level, y: height scale, z: unused, w: unused
(defun-g apply-height-fog ((wpos :vec3) (height-fog :vec4) (density :float))
  (* density (exp (* (- (+ (y wpos) (x height-fog)))
                     (y height-fog)))))
