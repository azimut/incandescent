(in-package #:incandescent)

;; Reference:
;; https://github.com/Unity-Technologies/VolumetricLighting/

(defun fog-params (&key (density .5) (start 1f0) (end 100f0))
  (v! (/ density (sqrt (log 2)))
      (/ density (log 2))
      (/ (- 1) (- end start))
      (/ end (- end start))))

(defvar *frustum-rays*      nil)
(defvar *t-volume-inject*   nil)
(defvar *s-volume-inject*   nil)
(defvar *z-volume-inject*   nil)
(defvar *t-volume-scatter*  nil)
(defvar *s-volume-scatter*  nil)
(defvar *z-volume-scatter*  nil)
(defvar *volume-resolution* (v! 160 90 128))
(v-def-glsl-template-fun logxor (a b) "(~a ^ ~a)" (v-int v-int) v-int :pure t)
(v-def-glsl-template-fun logand (a b) "(~a & ~a)" (:int :int) :int :pure t)
(v-def-glsl-template-fun << (x y) "(~a << ~a)" (v-int v-int) v-int :pure t)

(defun-g ihash ((n :int))
  (let ((n (logxor (<< n (int 13)) n)))
    (logand (+ (* n (+ (* n n 15731) 789221)) 1376312589)
            2147483647)))

(defun-g frand ((n :int))
  (/ (ihash n) 2147483647f0))

(defun-g cell-noise ((p :ivec2))
  (let ((i (+ (* (y p) 256) (x p))))
    (- (v! (frand i) (frand (+ i 57))) .5)))

(defun-g post-fog ((linear-depth   :float)
                   (screen-uv      :vec2)
                   (volume-scatter :sampler-3d)
                   (screen-res     :vec2))
  (let* (;;(screen-uv (treat-uvs screen-uv))
         (z (* linear-depth (/ 100f0 100f0)))
         (z (/ (- z (/ .1 100f0) (- 1 (/ .1 100f0)))))
         (uvw (vec3 0f0)))
    (when(< z 0f0)
      (return (v! 0 0 0 1)))
    (setf uvw (v! screen-uv z))
    (incf (s~ uvw :xy)
          (* (cell-noise (ivec2 (int (* 320 (x uvw)))
                                (int (* 240 (y uvw)))))
             (v! (/ 1f0 160) (/ 1f0 90))
             .8))
    (texture volume-scatter uvw)))

(defun free-volume ()
  (when *t-volume-scatter* (free *t-volume-scatter*))
  (when *t-volume-inject*  (free *t-volume-inject*)))

(defun init-volume ()
  (free-volume)
  (let ((dim (list (floor (x *volume-resolution*))
                   (floor (y *volume-resolution*))
                   (floor (z *volume-resolution*)))))
    (setf *t-volume-inject* (make-texture nil :dimensions dim :element-type :rgba8)
          *s-volume-inject* (sample *t-volume-inject*)
          *z-volume-inject* (sample *t-volume-inject*)
          (%cepl.types::%sampler-imagine *z-volume-inject*) t)
    (setf *t-volume-scatter* (make-texture nil :dimensions dim :element-type :rgba8)
          *s-volume-scatter* (sample *t-volume-scatter*)
          *z-volume-scatter* (sample *t-volume-scatter*)
          (%cepl.types::%sampler-imagine *z-volume-scatter*) t))
  (unless *frustum-rays*
    (setf *frustum-rays* (make-c-array nil :dimensions 4 :element-type :vec4)))
  t)

(defun-g hash-noise ((n :float))
  (fract (* (sin n) 753.5453123)))

(defun-g noisep ((x :vec3))
  (let* ((p (floor x))
         (f (fract x))
         (f (* f f (- 3f0 (* 2f0 f))))
         (n (+ (x p) (* (y p) 157f0) (* (z p) 113f0))))
    (mix (mix (mix (hash-noise (+ n   0f0)) (hash-noise (+ n   1f0)) (x f))
              (mix (hash-noise (+ n 157f0)) (hash-noise (+ n 158f0)) (x f))
              (y f))
         (mix (mix (hash-noise (+ n 113f0)) (hash-noise (+ n 114f0)) (x f))
              (mix (hash-noise (+ n 270f0)) (hash-noise (+ n 271f0)) (x f))
              (y f))
         (z f))))

(defun-g scroll-noise ((pos     :vec3)
                       (speed   :float)
                       (scale   :float)
                       (dir     :vec3)
                       (ammount :float)
                       (time    :float))
  (let* ((bias -.3f0)
         (mult 8f0)
         (time (* time speed))
         (noise-scale scale)
         (noise-scroll (* time dir))
         (q (* scale (- pos noise-scroll)))
         (f (* 0.5 (noisep q)))
         ;; scroll the next octave in the opposite direction to get
         ;; some morphing instead of just scrolling
         (q (+ q noise-scroll scale))
         (q (* q 2.01))
         (f (+ f (* .25 (noisep q))))
         (f (+ f bias))
         (f (* f mult))
         (f (max f 0f0)))
    (mix 1f0 f ammount)))

(defun-g density ((pos        :vec3)
                  (fog-params :vec4)
                  (time       :float))
  (let* ((fog (x fog-params))
         (fog (+ fog (max (* (exp (* (y fog-params)
                                     (+ (- (y pos)) (z fog-params))))
                             (w fog-params))
                          0f0)))
         (warp pos)
         (fog (* fog (scroll-noise warp
                                   1f0 ; - wind speed
                                   1f0 ; validate - noise scale
                                   (v! 0 0 1) ; validate - wind dir
                                   .4f0 ; validate - noise ammount
                                   time)))
         ;; TODO: fog_bomb
         ;; TODO: fog_ellipsoids
         ;; TODO: hardcoded density
         ;; m_GlobalDensityMult * 0.128f * depthCompensation
         ;; 1f0 * 0.128 * ((farClip - nearClip) * 0.01f)
         (density (* 1f0 .128 (* .01 (- 100f0 .1)))))
    (max (* fog density) 0f0)
    .1))

(defun-g frustum-ray ((uv            :vec2)
                      (frustum-rays (:vec4 4)))
  (let ((ray0 (mix (s~ (aref frustum-rays 0) :xyz)
                   (s~ (aref frustum-rays 1) :xyz)
                   (x uv)))
        (ray1 (mix (s~ (aref frustum-rays 3) :xyz)
                   (s~ (aref frustum-rays 2) :xyz)
                   (x uv))))
    (mix ray0 ray1 (y uv))))

(defun-g anisotropy ((cos-theta :float))
  (let* ((g .3f0)
         (gsq (* g g))
         (denom (1+ (- gsq (* 2f0 g cos-theta))))
         (denom (* denom denom denom))
         (denom (sqrt (max 0 denom))))
    (/ (- 1 gsq) denom)))

(defun-g directional-light ((pos :vec3)
                            (cam-pos :vec3))
  (let* ((att 1f0)
         (pos-to-camera (normalize (- cam-pos pos)))
         ;;(dir-light-dir (v! .3 .2 .2))
         (dir-light-dir (v! .7071 .5773 -.40824))
         (cos-theta (dot pos-to-camera dir-light-dir))
         (att (* att (anisotropy cos-theta))))
    (* (v! .5 .5 .5) att)))

(defun-g inject-compute (&uniform (volume-inject :image-3d)
                                  (frustum-rays (:vec4 4))
                                  (cam-pos       :vec3)
                                  (fog-params    :vec4)
                                  (time          :float))
  (declare (local-size :x 16 :y 2 :z 16))
  (let* ((color (v! 0 1 0)) ;; ambient
         (uv    (v! (/ (float (x gl-global-invocation-id)) 159f0)
                    (/ (float (y gl-global-invocation-id))  89f0)))
         (z     (/ (z gl-global-invocation-id) 127f0))
         (near  0.1f0)
         (far   100f0)
         (n-o-f (/ near far))
         (z     (+ n-o-f (* z (- 1 n-o-f))))
         (pos   (+ cam-pos (* z (frustum-ray uv frustum-rays))))
         ;; TODO: directional light
         ;;(color   (v! .1 .1 .1))
         (color   (directional-light pos cam-pos))
         (density (density pos fog-params time))
         ;; TODO: hardcoded intensity
         (intensity 1f0))
    (image-store volume-inject
                 (ivec3 (int (x gl-global-invocation-id))
                        (int (y gl-global-invocation-id))
                        (int (z gl-global-invocation-id)))
                 (v! (* intensity density color) density))))

(defpipeline-g inject-pipe ()
  :compute inject-compute)

(let ((stepper (make-stepper (seconds 1) (seconds 1)))
      (fog-params (fog-params :density .3f0 :start .1f0 :end 100f0))
      (x (floor (/ (x *volume-resolution*) 16)))
      (y (floor (/ (y *volume-resolution*)  2)))
      (z (floor (/ (z *volume-resolution*) 16))))
  (defun draw-inject ()
    (when (funcall stepper)
      (map-g #'inject-pipe (make-compute-space x y z)
             :volume-inject *z-volume-inject*
             :time (* .001 (get-internal-real-time))
             ;; TODO: to UBO? to vec3[]?
             :frustum-rays  (push-g (get-frustum-rays-v4) *frustum-rays*)
             :fog-params    fog-params
             :cam-pos       (pos *currentcamera*))
      (wait-on-gpu-fence (make-gpu-fence)))))

;;--------------------------------------------------
;; Based on 'Volumetric fog: Unified, compute shader based solution to atmospheric scattering, ACM Siggraph 2014'
;; https://bartwronski.com/publications/
(defun-g scatter-step ((accumulated-light         :vec3)
                       (accumulated-transmittance :float)
                       (slice-light               :vec3)
                       (slice-density             :float))
  (let* ((volume-depth 128f0)
         (slice-density (max slice-density 0.000001))
         (slice-transmittance (exp (/ (- slice-density) volume-depth)))
         ;; Seb Hillaire's improved transmission by calculating an integral over slice depth instead of
         ;; constant per slice value. Light still constant per slice, but that's acceptable. See slide 28 of
         ;; Physically-based & Unified Volumetric Rendering in Frostbite
         ;; http://www.frostbite.com/2015/08/physically-based-unified-volumetric-rendering-in-frostbite/
         (slice-light-integral (/ (* slice-light (- 1f0 slice-transmittance)) slice-density))
         (accumulated-light (+ accumulated-light (* slice-light-integral accumulated-transmittance)))
         (accumulated-transmittance (* accumulated-transmittance slice-transmittance)))
    (v! accumulated-light accumulated-transmittance)))

(defun-g scatter-compute (&uniform (volume-inject  :sampler-3d)
                                   (volume-scatter :image-3d))
  (declare (local-size :x 32 :y 2 :z 1))
  (let* (;; Store transmission in .a, as opposed to density in _VolumeInject
         (accum (v! 0f0 0f0 0f0 1f0))
         (pos   (ivec3 (int (x gl-global-invocation-id))
                       (int (y gl-global-invocation-id))
                       (int 0))))
    ;; NT: iterate over each voxel across Z on this XY
    (dotimes (z 128)
      (setf (z pos) z)
      (let ((slice (texture volume-inject pos)))
        (setf accum (scatter-step (s~ accum :xyz) (w accum)
                                  (s~ slice :xyz) (w slice)))
        (image-store volume-scatter pos accum)))))

(defpipeline-g scatter-pipe ()
  :compute scatter-compute)

(let ((stepper (make-stepper (seconds 1) (seconds 1)))
      (x (floor (/ (x *volume-resolution*) 32)))
      (y (floor (/ (y *volume-resolution*)  2)))
      (z 1))
  (defun draw-scatter ()
    "Solve scattering"
    ;; 32 2 1
    (when (funcall stepper)
      (map-g #'scatter-pipe (make-compute-space x y z)
             :volume-inject  *s-volume-inject*
             :volume-scatter *z-volume-scatter*)
      (wait-on-gpu-fence (make-gpu-fence)))))
