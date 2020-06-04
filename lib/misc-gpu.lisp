(in-package #:incandescent)

(defun-g spec-phong ((light-dir :vec3) (view-dir :vec3) (normal :vec3) (shininess :float))
  (let* ((reflect-dir (reflect (- light-dir) normal))
         (spec-angle  (max (dot view-dir reflect-dir) 0))
         (spec        (pow spec-angle shininess)))
    spec))

;; https://learnopengl.com/Advanced-Lighting/Advanced-Lighting
;; https://en.wikipedia.org/wiki/Blinn%E2%80%93Phong_shading_model
(defun-g spec-blinn ((light-dir :vec3) (view-dir :vec3) (normal :vec3) (shininess :float))
  (let* ((half-dir   (normalize (+ light-dir view-dir)))
         (spec-angle (max (dot half-dir normal) 0))
         (spec       (pow spec-angle shininess)))
    spec))

;; oren-nayar
;; https://github.com/glslify/glsl-diffuse-oren-nayar
;; - is roughness a value in radians?
(defun-g diffuse-oren-nayar ((light-dir :vec3)
                             (view-dir  :vec3)
                             (normal    :vec3)
                             (roughness :float)
                             (intensity :float))
  (let* ((l-dot-v (dot light-dir view-dir))
         (n-dot-l (dot light-dir normal))
         (n-dot-v (dot normal view-dir))
         (s       (- l-dot-v (* n-dot-l n-dot-v)))
         (tt      (mix 1 (max n-dot-l n-dot-v) (step 0 s)))
         (sigma2  (* roughness roughness))
         (a       (1+ (* sigma2 (+ (/ intensity (+ sigma2 .13))
                                   (/ .5 (+ sigma2 .33))))))
         (b       (* .45 (/ sigma2 (+ sigma2 .09)))))
    (* intensity (max 0 n-dot-l) (/ (+ a (/ (* b s) tt)) +pi+))))

;;--------------------------------------------------
;; Range Constant Linear Quadratic
;; 3250  1.0      0.0014 0.000007
;; 600   1.0      0.007  0.0002
;; 325   1.0      0.014  0.0007
;; 200   1.0      0.022  0.0019
;; 160   1.0      0.027  0.0028
;; 100   1.0      0.045  0.0075
;; 65    1.0      0.07   0.017
;; 50    1.0      0.09   0.032
;; 32    1.0      0.14   0.07
;; 20    1.0      0.22   0.20
;; 13    1.0      0.35   0.44
;; 7     1.0      0.7    1.8
(defun-g point-light-apply ((color :vec3)
                            (light-color :vec3)
                            (light-pos :vec3)
                            (frag-pos :vec3)
                            (normal :vec3)
                            (constant :float)
                            (linear :float)
                            (quadratic :float))
  (let* ((light-dir   (normalize (- light-pos frag-pos)))
         (diff        (saturate (dot normal light-dir)))
         (distance    (length (- light-pos frag-pos)))
         ;; HDR distance, not squared
         (attenuation (/ 1 (+ constant
                              (* linear distance)
                              (* quadratic distance distance))))
         (ambient (* light-color .1))
         (diffuse (* light-color diff)))
    (* color attenuation (+ ambient diffuse))))

(defun-g point-light-apply ((color       :vec3)
                            (light-color :vec3)
                            (light-pos   :vec3)
                            (frag-pos    :vec3)
                            (normal      :vec3)
                            (constant    :float)
                            (linear      :float)
                            (quadratic   :float)
                            (cam-pos     :vec3)
                            (roughness   :float)
                            (intensity   :float))
  (let* ((light-dir   (normalize (- light-pos frag-pos)))
         (view-dir    (normalize (- cam-pos frag-pos)))
         (diff        (diffuse-oren-nayar light-dir
                                          view-dir
                                          normal
                                          roughness
                                          intensity))
         (distance    (length (- light-pos frag-pos)))
         ;; HDR distance, not squared
         (attenuation (/ 1 (+ constant
                              (* linear distance)
                              (* quadratic distance distance))))
         (ambient (* light-color .1))
         (diffuse (* light-color diff)))
    (* color attenuation (+ ambient diffuse))))

(defun-g point-light-apply-spec ((color :vec3)
                                 (light-color :vec3)
                                 (light-pos :vec3)
                                 (frag-pos :vec3)
                                 (normal :vec3)
                                 (constant :float)
                                 (linear :float)
                                 (quadratic :float)
                                 (cam-pos :vec3)
                                 (spec-strength :float)
                                 (shininess :int))
  (let* ((light-dir (normalize (- light-pos frag-pos)))
         (diff (saturate (dot normal light-dir)))
         ;; spec
         (view-dir (normalize (- cam-pos frag-pos)))
         (spec     (spec-blinn light-dir view-dir normal shininess))
         ;;
         (distance (length (- light-pos frag-pos)))
         ;; HDR distance, not squared
         (attenuation (/ 1 (+ constant
                              (* linear distance)
                              (* quadratic distance distance))))
         (ambient  (* light-color .1))
         (diffuse  (* light-color diff))
         (specular (* light-color spec spec-strength)))
    (* color attenuation  (+ ambient
                             diffuse
                             specular))))

;;--------------------------------------------------
;; Spotlight
;;
;; NOTE: cut-off and outer-cut-off are the result of: (cos (radians ANGLE))
(defun-g spot-light-apply ((color         :vec3)
                           (light-color   :vec3)
                           (light-pos     :vec3)
                           (light-dir     :vec3)
                           (frag-pos      :vec3)
                           (normal        :vec3)
                           ;;
                           (constant      :float)
                           (linear        :float)
                           (quadratic     :float)
                           ;;
                           (cut-off       :float)
                           (outer-cut-off :float))
  (let* ((l           (normalize (- light-pos frag-pos)))
         (diff        (saturate  (dot normal l)))
         ;;
         (distance    (length (- light-pos frag-pos)))
         (attenuation (/ 1f0 (+ constant
                                (* linear distance)
                                (* quadratic distance))))
         (light-color (* light-color attenuation))
         ;;
         (theta     (dot l (normalize (- light-dir))))
         (epsilon   (- cut-off outer-cut-off))
         ;;(intensity (/ (- theta outer-cut-off) epsilon))
         (intensity (clamp (/ (- theta outer-cut-off) epsilon) 0 1)))
    (* color intensity diff light-color)))

;;--------------------------------------------------
;; Flashlight

;; https://www.shadertoy.com/view/4slGz7
(defun-g light-pulse ((e0 :float) (e1 :float) (x :float))
  (- (step e0 x)
     (step e1 x)))
(defun-g light-flicker ((time :float))
  (- 1 (+ (* (light-pulse .0 .4 (fract (* time .2)))
             (light-pulse .4 .5 (fract (* time 6.0) ))
             .3)
          (light-pulse .1 .12 (fract (* time .12))))))

(defun cl-step (edge x)
  "For element i of the return value, 0.0 is returned if x[i] < edge[i], and 1.0 is returned otherwise."
  (declare (type number edge x))
  (if (< x edge)
      0f0
      1f0))

(defun light-pulse (e0 e1 x)
  (- (cl-step e0 x)
     (cl-step e1 x)))
(defun fract (x)
  (mod x 1f0))
(defun light-flicker (&optional (time (* .1 (get-internal-real-time))))
  (- 1 (+ (* (light-pulse .0 .4 (fract (* time .2)))
             (light-pulse .4 .5 (fract (* time 6.0) ))
             .3)
          (light-pulse .1 .12 (fract (* time .12))))))

(defun-g flash-light-apply ((color :vec3)
                            (light-color :vec3)
                            (light-pos :vec3)
                            (frag-pos :vec3)
                            (normal :vec3)
                            (cam-dir :vec3)
                            ;;
                            (constant :float)
                            (linear :float)
                            (quadratic :float)
                            ;;
                            (cut-off :float)
                            (outer-cut-off :float))
  (let* ((light-dir   (normalize (- light-pos frag-pos)))
         (diff        (saturate  (dot normal light-dir)))
         ;; HDR distance, not squared
         (distance    (length    (- light-pos frag-pos)))
         (attenuation (/ 1 (+ constant
                              (* linear distance)
                              (* quadratic distance distance))))
         ;;
         (theta     (dot light-dir (- cam-dir)))
         (epsilon   (- cut-off outer-cut-off))
         (intensity (saturate (/ (- theta outer-cut-off) epsilon)))
         ;;
         (ambient (* light-color .1))
         (diffuse (* light-color diff)))
    (* color attenuation intensity (+ ambient diffuse))))

(defun-g flash-light-apply ((color :vec3)
                            (light-color :vec3)
                            (light-pos :vec3)
                            (frag-pos :vec3)
                            (normal :vec3)
                            (cam-dir :vec3)
                            ;;
                            (constant :float)
                            (linear :float)
                            (quadratic :float)
                            ;;
                            (cut-off :float)
                            (outer-cut-off :float)
                            ;;
                            (time :float))
  (let* ((light-dir   (normalize (- light-pos frag-pos)))
         (diff        (saturate  (dot normal light-dir)))
         ;; HDR distance, not squared
         (distance    (length    (- light-pos frag-pos)))
         (attenuation (/ 1 (+ constant
                              (* linear distance)
                              (* quadratic distance distance))))
         ;;
         (theta     (dot light-dir (- cam-dir)))
         (epsilon   (- cut-off outer-cut-off))
         (intensity (saturate (/ (- theta outer-cut-off) epsilon)))
         ;;
         (ambient (* light-color .1))
         (diffuse (* light-color diff))
         ;;
         (flicker (light-flicker (* .1 time))))
    (* color attenuation intensity flicker (+ ambient diffuse))))

;;--------------------------------------------------

;; Only lambert diffuse
(defun-g dir-light-apply ((color       :vec3)
                          (light-color :vec3)
                          (light-pos   :vec3)
                          (frag-pos    :vec3)
                          (normal      :vec3))
  (let* ((light-dir (normalize (- light-pos frag-pos)))
         ;; Diffuse shading
         (diff    (saturate (dot normal light-dir)))
         ;; combine
         (ambient (* light-color .1))
         (diffuse (* light-color diff)))
    (* color (+ ambient diffuse))))

;; Only oren diffuse
(defun-g dir-light-apply ((color :vec3)
                          (light-color :vec3)
                          (light-pos :vec3)
                          (frag-pos :vec3)
                          (normal :vec3)
                          (cam-pos :vec3)
                          (roughness :float)
                          (intensity :float))
  (let* ((light-dir (normalize (- light-pos frag-pos)))
         (view-dir  (normalize (- cam-pos frag-pos)))
         ;; Diffuse shading
         (diff (diffuse-oren-nayar light-dir
                                   view-dir
                                   normal
                                   roughness
                                   intensity))
         ;; combine
         (ambient (* light-color .1))
         (diffuse (* light-color diff)))
    (* color (+ ambient diffuse))))

;; Lambert diffuse + specular
(defun-g dir-light-apply-spec ((color :vec3)
                               (light-color :vec3)
                               (light-pos :vec3)
                               (frag-pos :vec3)
                               (normal :vec3)
                               (cam-pos :vec3)
                               (shininess :float)
                               (spec-strength :float))
  (let* ((normal (normalize normal))
         (light-dir (normalize (- light-pos frag-pos)))
         ;; Diffuse shading
         (diff (saturate (dot normal light-dir)))
         ;; specular
         (view-dir (normalize (- cam-pos frag-pos)))
         (spec     (spec-blinn light-dir view-dir normal shininess))
         ;; combine
         (ambient  (* light-color .1))
         (diffuse  (* light-color diff))
         (specular (* light-color spec spec-strength)))
    (* color (+ ambient diffuse specular))))

;;--------------------------------------------------
;; https://learnopengl.com/Advanced-Lighting/Normal-Mapping
;; "Pushing pixels" code

(defun-g norm-from-map ((normal-map :sampler-2d)
                        (uv         :vec2))
  (let* ((normal (s~ (texture normal-map uv) :xyz))
         (normal (normalize (- (* 2f0 normal) 1f0))))
    normal))

(defun-g norm-from-map-flipped ((normal-map :sampler-2d)
                                (uv         :vec2))
  "Sometimes y component is wrong on the normal map."
  (let* ((normal (s~ (texture normal-map uv) :xyz))
         (normal (v! (x normal) (- (y normal)) (z normal)))
         (normal (normalize (1- (* 2 normal)))))
    normal))

;; If there is no parallax mapping, we need to convert normals to tangent space

(defun-g norm-from-map ((normal-map :sampler-2d)
                        (uv         :vec2)
                        (tbn        :mat3))
  (* tbn (norm-from-map normal-map uv)))

(defun-g norm-from-map-flipped ((normal-map :sampler-2d)
                                (uv         :vec2)
                                (tbn        :mat3))
  "Sometimes y component is wrong on the normal map."
  (* tbn (norm-from-map-flipped normal-map uv)))

;; https://github.com/JoeyDeVries/LearnOpenGL/blob/master/src/6.pbr/1.2.lighting_textured/1.2.pbr.fs
;; "Easy trick to get tangent-normals to world-space to keep PBR code simplified.
;;  Don't worry if you don't get what's going on; you generally want to do normal
;;  mapping the usual way for performance anways; I do plan make a note of this
;;  technique somewhere later in the normal mapping tutorial."
;; NT: To get the tangent normal to world-space IN the fragment shader
(defun-g norm-from-map ((normal-map :sampler-2d)
                        (uv :vec2)
                        (world-pos :vec3)
                        (normal :vec3))
  (let* ((tangent-normal (+ -1 (* 2 (s~ (texture normal-map uv) :xyz))))
         (q1  (d-fdx world-pos))
         (q2  (d-fdy world-pos))
         (st1 (d-fdx uv))
         (st2 (d-fdy uv))
         (n0  (normalize normal))
         (t0  (normalize (- (* q1 (y st2))
                            (* q1 (y st1)))))
         (b0  (- (normalize (cross n0 t0))))
         (tbn (mat3 t0 b0 n0))
         (result (normalize (* tbn tangent-normal))))
    result))

(defun-g norm-from-map-flipped ((normal-map :sampler-2d)
                                (uv :vec2)
                                (world-pos :vec3)
                                (normal :vec3))
  (let* ((tangent-normal (+ -1 (* 2 (s~ (texture normal-map uv) :xyz))))
         (q1  (d-fdx world-pos))
         (q2  (d-fdy world-pos))
         (st1 (d-fdx uv))
         (st2 (d-fdy uv))
         (n0  (normalize normal))
         (t0  (normalize (- (* q1 (y st2))
                            (* q1 (y st1)))))
         (b0  (- (normalize (cross n0 t0))))
         (tbn (mat3 t0 b0 n0))
         (result (normalize (* tbn tangent-normal))))
    (v! (x result)
        (- (y result))
        (z normal))))



;; ?
;; From "pushing pixels" don't remember why it's needed
(defun-g treat-uvs ((uv :vec2))
  (v! (x uv) (- 1.0 (y uv))))

;;--------------------------------------------------
;; https://learnopengl.com/Advanced-Lighting/Parallax-Mapping
;; vec3 viewDir   = normalize(fs_in.TangentViewPos - fs_in.TangentFragPos);
(defun-g parallax-mapping ((uv           :vec2)
                           (view-dir     :vec3)
                           (depth-map    :sampler-2d)
                           (height-scale :float))
  (let* ((height (x (texture depth-map uv)))
         (p      (* height height-scale (/ (s~ view-dir :xy)
                                           (z view-dir)))))
    (- uv p)))

(defun-g parallax-mapping-flipped ((uv           :vec2)
                                   (view-dir     :vec3)
                                   (depth-map    :sampler-2d)
                                   (height-scale :float))
  (let* ((height (- 1 (x (texture depth-map uv))))
         (p      (* height height-scale (/ (s~ view-dir :xy)
                                           (z view-dir)))))
    (- uv p)))

;; https://catlikecoding.com/unity/tutorials/rendering/part-20/
;; Limit lenght of 1
(defun-g parallax-mapping-offset ((uv           :vec2)
                                  (view-dir     :vec3)
                                  (depth-map    :sampler-2d)
                                  (height-scale :float))
  (let* ((height (x (texture depth-map uv)))
         (height (- height .5))
         (p      (* height height-scale)))
    (+ uv (* (s~ view-dir :xy) p))))

(defun-g parallax-mapping-offset-flipped ((uv           :vec2)
                                          (view-dir     :vec3)
                                          (depth-map    :sampler-2d)
                                          (height-scale :float))
  (let* ((height (- 1 (x (texture depth-map uv))))
         (height (- height .5))
         (p      (* height height-scale)))
    (+ uv (* (s~ view-dir :xy) p))))

;;--------------------------------------------------
;; https://catlikecoding.com/unity/tutorials/advanced-rendering/flat-and-wireframe-shading/

(defun-g flat-shading ((pos :vec3))
  (let* ((dpdx (d-fdx pos))
         (dpdy (d-fdy pos))
         (norm (normalize (cross dpdy dpdx))))
    norm))

;;--------------------------------------------------
;; WORKS???
;; http://www.voidcn.com/article/p-nvhpdsyj-yy.html
(defun-g linear-eye-depth ((d :float))
  (let* ((n .1)
         (f 100f0)
         (zz (/ (/ (- 1 (/ f n)) 2) f))
         (zw (/ (/ (+ 1 (/ f n)) 2) f)))
    (/ 1 (+ (* zz d) zw))))

(defun-g read-depth ((z :float))
  (let* ((pfn (+ 400 .1))
         (mfn (- 400 .1))
         (coef (* 2f0 .1)))
    (/ coef (- pfn (* z mfn)))))

;; https://learnopengl.com/Advanced-OpenGL/Depth-testing
;; Because the linearized depth values range from near to far most of
;; its values will be above 1.0 and displayed as completely white. By
;; dividing the linear depth value by far in the main function we
;; convert the linear depth value to roughly the range [0, 1]. This
;; way we can gradually see the scene become brighter the closer the
;; fragments are to the projection frustum's far plane, which is
;; better suited for demonstration purposes.
(defun-g linearize-depth ((depth :float))
  (let* ((near 0.1)
         (far 100f0)
         (z (- (* depth 2.0) 1.0)))
    (/ (* 2.0 (* near far))
       (- (+ far near) (* z (- far near))))))

(defun-g linearize-depth ((depth :float) (near :float) (far :float))
  (let* ((z (- (* depth 2.0) 1.0)))
    (/ (* 2.0 (* near far))
       (- (+ far near) (* z (- far near))))))

;; Three.js - packaging.glsl.js
(defun-g view-zto-orthographic-depth ((view-z :float)
                                      (near :float)
                                      (far :float))
  (/ (+ view-z near)
     (- near far)))

(defun-g perspective-depth-to-view-z ((inv-clip-z :float)
                                       (near :float)
                                       (far :float))
  (/ (* near far)
     (- (* (- far near) inv-clip-z) far)))

(defun-g read-depth ((depth-sampler :sampler-2d)
                     (coord :vec2)
                     (camera-near :float)
                     (camera-far :float))
  (let* ((frag-coord-z (x (texture depth-sampler coord)))
         (view-z (perspective-depth-to-view-z frag-coord-z
                                              camera-near
                                              camera-far)))
    (view-zto-orthographic-depth view-z camera-near camera-far)))

;;--------------------------------------------------

;; Code from cbaggers/daft
(defun-g calc-uv-mod ((tile-count-x :int)
                      (tile-count-y :int)
                      (anim-frame   :int))
  (let* ((uv-scale  (v! (/ 1f0 tile-count-x)
                        (/ 1f0 tile-count-y)))
         (uv-offset (v! (* (mod anim-frame tile-count-x)
                           (x uv-scale))
                        (* (floor (/ anim-frame tile-count-x))
                           (y uv-scale)))))
    (values uv-scale uv-offset)))

(defun-g calc-uv-mod ((tile-count-x :float)
                      (tile-count-y :float)
                      (anim-frame   :float))
  (let* ((uv-scale  (v! (/ 1f0 tile-count-x)
                        (/ 1f0 tile-count-y)))
         (uv-offset (v! (* (mod anim-frame tile-count-x)
                           (x uv-scale))
                        (* (floor (/ anim-frame tile-count-x))
                           (y uv-scale)))))
    (values uv-scale uv-offset)))


;;--------------------------------------------------
;; https://forum.processing.org/two/discussion/3955/textured-billboard-and-transparency-problem
;; https://developer.download.nvidia.com/whitepapers/2007/SDK10/SoftParticles_hi.pdf
;; https://discourse.threejs.org/t/soft-particles-render/504/3
(defun-g calculate-fade ((particle-depth :float)
                         (scene-depth    :float))
  (let* ((z-fade      1f0)
         (f-distance 10f0)
         (f-contrast  1f0)
         (input-depth (* (- scene-depth particle-depth) f-distance)))
    (if (and (> input-depth 0) (< input-depth 1))
        (setf z-fade (* .5 (pow (saturate (* 2f0 (if (> input-depth .5)
                                                     (- 1 input-depth)
                                                     input-depth)))
                                f-contrast))
              z-fade (if (> input-depth .5) (- 1 z-fade) z-fade))
        (setf z-fade (saturate input-depth)))
    z-fade))

;; https://developer.download.nvidia.com/cg/fmod.html
;; float2 fmod(float2 a, float2 b)
;; {
;; float2 c = frac(abs(a/b))*abs(b);
;; return (a < 0) ? -c : c;   /* if ( a < 0 ) c = 0-c */
;; }
(defun-g fmod ((a :int) (b :int))
  (let ((c (* (fract (abs (/ a b)) ) (abs b))))
    (if (< a 0) (- c) c)))
;; https://stackoverflow.com/questions/7610631/glsl-mod-vs-hlsl-fmod
;; ???
(defun-g fmod ((x :float) (y :float))
  (- x (* y (trunc (/ x y)))))
(defun-g fmod ((x :vec2) (y :vec2))
  (- x (* y (trunc (/ x y)))))
(defun-g fmod ((x :vec3) (y :vec3))
  (- x (* y (trunc (/ x y)))))

;;-------------------------------------------------
;; Used for ssao or rsm
(defun-g get-view-pos ((uv         :vec2)
                       (g-depth    :sampler-2d)
                       (world-view :mat4))
  "Returns the world position from a depth texture"
  (let* ((x (1- (* 2f0 (x uv))))
         (y (1- (* 2f0 (y uv))))
         (z (1- (* 2f0 (x (texture g-depth uv)))))
         (pos-proj (v! x y z 1))
         (pos-view (* (inverse world-view) pos-proj))
         (pos-view (/ pos-view (w pos-view))))
    pos-view))

;;--------------------------------------------------

;; Unity (it works!)
;; just send it from the vertex shader to the fragment
;; and there XY/W to get some uvs
;; and then substract Z
(defun-g compute-screen-pos ((clip-pos :vec4))
  (let ((o (* .5 clip-pos)))
    (v! (+ (v! (x o) (* (y o) 1)) ;; or -1
           (w o))
        (s~ clip-pos :zw))))


;;--------------------------------------------------

(defun-g linear-to-srgb ((c :vec3))
  (let ((igamma #.(/ 1f0 2.2f0)))
    (pow c (vec3 igamma))))

;; Usage:
;; - (tonemap-acesfilm (* (linear-to-srgb (* *exposure* final-color))))
;; https://knarkowicz.wordpress.com/2016/01/06/aces-filmic-tone-mapping-curve/
(defun-g tone-map-acesfilm ((x :vec3) (exposure :float))
  (let ((x (* exposure x))
        (a 2.51)
        (b 0.03)
        (c 2.43)
        (d 0.59)
        (e 0.14))
    (pow (clamp (/ (* x (+ b (* a x)))
                   (+ e (* x (+ d (* c x)))))
                0f0 1f0)
         (vec3 #.(/ 1f0 2.2f0)))))

;; Based on Filmic Tonemapping Operators http://filmicgames.com/archives/75
(defun-g tone-map-filmic ((color :vec3) (exposure :float))
  (let ((color (* exposure color))
        (x (max (vec3 0f0) (- color 0.004))))
    (/ (* x (+ 0.5 (* x 6.2)))
       (+ 0.06 (* x (+ 1.7 (* x 6.2)))))))

