(in-package #:incandescent)

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

(defparameter *light-color* (v! 1.0 0.7176471 0.29803923))
(defvar *exposure* 1f0)

;; oren-nayar
;; https://github.com/glslify/glsl-diffuse-oren-nayar
;; - is roughness a value in radians?
(defun-g oren-nayar-diffuse ((light-dir :vec3)
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
  (let* ((light-dir (normalize (- light-pos frag-pos)))
         (diff (saturate (dot normal light-dir)))
         (distance (length (- light-pos frag-pos)))
         ;; HDR distance, not squared
         (attenuation (/ 1 (+ constant
                              (* linear distance)
                              (* quadratic distance))))
         (ambient (* light-color .1))
         (diffuse (* light-color diff)))
    (* color (+ ambient diffuse))))

(defun-g point-light-apply ((color :vec3)
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
         (view-dir    (normalize (- cam-pos frag-pos)))
         (reflect-dir (reflect (- light-dir) normal))
         (spec        (pow (max (dot view-dir reflect-dir) 0)
                           shininess))
         ;;
         (distance (length (- light-pos frag-pos)))
         ;; HDR distance, not squared
         (attenuation (/ 1 (+ constant
                              (* linear distance)
                              (* quadratic distance))))
         (ambient  (* light-color .1))
         (diffuse  (* light-color diff))
         (specular (* light-color spec spec-strength)))
    (* color (+ ambient
                diffuse
                specular))))

;;--------------------------------------------------
                                        ;
;; Only lambert diffuse
(defun-g dir-light-apply ((color :vec3)
                          (light-color :vec3)
                          (light-pos :vec3)
                          (frag-pos :vec3)
                          (normal :vec3))
  (let* ((light-dir (normalize (- light-pos frag-pos)))
         ;; Diffuse shading
         (diff (saturate (dot normal light-dir)))
         ;; combine
         (ambient (* light-color .1 color))
         (diffuse (* light-color diff color)))
    (+ ambient diffuse)))

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
         (diff (oren-nayar-diffuse light-dir
                                   view-dir
                                   normal
                                   roughness
                                   intensity))
         ;; combine
         (ambient (* light-color .1))
         (diffuse (* light-color diff)))
    (* color (+ ambient diffuse))))

;; Lambert diffuse + specular
(defun-g dir-light-apply ((color :vec3)
                          (light-color :vec3)
                          (light-pos :vec3)
                          (frag-pos :vec3)
                          (normal :vec3)
                          (cam-pos :vec3)
                          (shininess :int)
                          (spec-strength :float))
  (let* ((light-dir (normalize (- light-pos frag-pos)))
         ;; Diffuse shading
         (diff (saturate (dot normal light-dir)))
         ;; specular
         (view-dir    (normalize (- cam-pos frag-pos)))
         (reflect-dir (reflect (- light-dir) normal))
         (spec (pow (max (dot view-dir reflect-dir) 0) shininess))
         ;; combine
         (ambient  (* light-color .1))
         (diffuse  (* light-color diff))
         (specular (* light-color spec-strength spec)))
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






