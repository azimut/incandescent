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
                             (view-dir :vec3)
                             (normal :vec3)
                             (roughness :float)
                             (intensity :float))
  (let* ((l-dot-v (dot light-dir view-dir))
         (n-dot-l (dot light-dir normal))
         (n-dot-v (dot normal view-dir))
         (s (- l-dot-v (* n-dot-l n-dot-v)))
         (tt (mix 1 (max n-dot-l n-dot-v) (step 0 s)))
         (sigma2 (* roughness roughness))
         (a (1+ (* sigma2 (+ (/ intensity (+ sigma2 .13))
                             (/ .5 (+ sigma2 .33))))))
         (b (* .45 (/ sigma2 (+ sigma2 .09)))))
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

(defun-g point-light-apply ((color :vec3)
                            (light-color :vec3)
                            (light-pos :vec3)
                            (frag-pos :vec3)
                            (normal :vec3)
                            (constant :float)
                            (linear :float)
                            (quadratic :float)
                            (cam-pos :vec3)
                            (roughness :float)
                            (intensity :float))
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
                           (frag-pos      :vec3)
                           (normal        :vec3)
                           ;;
                           (constant      :float)
                           (linear        :float)
                           (quadratic     :float)
                           ;;
                           (cut-off       :float)
                           (outer-cut-off :float))
  (let* ((light-dir   (normalize (- light-pos frag-pos)))
         (diff        (saturate  (dot normal light-dir)))
         ;; HDR distance, not squared
         (distance    (length    (- light-pos frag-pos)))
         (attenuation (/ 1 (+ constant
                              (* linear distance)
                              (* quadratic distance distance))))
         ;;
         (theta     (dot light-dir (normalize (- (v! 0 -1 0)))))
         (epsilon   (- cut-off outer-cut-off))
         (intensity (clamp (/ (- theta outer-cut-off) epsilon) 0 1))
         ;;
         (ambient (* light-color .1))
         (diffuse (* light-color diff)))
    (* color attenuation intensity (+ ambient diffuse))))

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
(defun-g dir-light-apply ((color :vec3)
                          (light-color :vec3)
                          (light-pos :vec3)
                          (frag-pos :vec3)
                          (normal :vec3))
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
                        (uv :vec2))
  (let* ((normal (s~ (texture normal-map uv) :xyz))
         (normal (normalize (1- (* 2 normal)))))
    (v! (x normal)
        (y normal)
        (z normal))))

;; Sometimes "y" component is wrong on the normal map.
(defun-g norm-from-map-flipped ((normal-map :sampler-2d)
                                (uv :vec2))
  (let* ((normal (s~ (texture normal-map uv) :xyz))
         (normal (normalize (1- (* 2 normal)))))
    (v! (x normal)
        (- (y normal))
        (z normal))))

;; https://github.com/JoeyDeVries/LearnOpenGL/blob/master/src/6.pbr/1.2.lighting_textured/1.2.pbr.fs
;; To get the tangent normal to world-space IN the fragment shader
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
         (tbn (mat3 t0 b0 n0)))
    (normalize (* tbn tangent-normal))))

;; ?
;; From "pushing pixels" don't remember why it's needed
(defun-g treat-uvs ((uv :vec2))
  (v! (x uv) (- 1.0 (y uv))))

;;--------------------------------------------------
;; https://learnopengl.com/Advanced-Lighting/Parallax-Mapping
;; vec3 viewDir   = normalize(fs_in.TangentViewPos - fs_in.TangentFragPos);
(defun-g parallax-mapping ((uv :vec2)
                           (view-dir :vec3)
                           (depth-map :sampler-2d)
                           (height-scale :float))
  (let* ((height (x (texture depth-map uv)))
         (p      (* (* height height-scale)
                    (/ (s~ view-dir :xy)
                       (z view-dir)))))
    (- uv p)))

;; https://catlikecoding.com/unity/tutorials/rendering/part-20/
;; Limit lenght of 1
(defun-g parallax-mapping-offset ((uv :vec2)
                                  (view-dir :vec3)
                                  (depth-map :sampler-2d)
                                  (height-scale :float))
  (let* ((height (x (texture depth-map uv)))
         (height (- height .5))
         (p      (* height height-scale)))
    (+ uv (* (s~ view-dir :xy) p))))

(defun-g parallax-mapping-offset-flipped ((uv :vec2)
                                          (view-dir :vec3)
                                          (depth-map :sampler-2d)
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
         (f 1000f0)
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
         (far 1000f0)
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

