(in-package #:incandescent)

;; IBL - Needed for specular
(defvar *brdf* NIL)
(defvar *f-brdf* NIL)
(defvar *t-brdf* NIL)
(defvar *s-brdf* NIL)

;; IBL - Cubemap for the luminance
(defvar *saved* nil)
(defvar *t-cubemap-live* nil)
(defvar *s-cubemap-live* nil)

;; IBL - Prefilter cubemap - for specular
(defvar *prefilter* NIL)
(defvar *t-cubemap-prefilter* NIL)
(defvar *s-cubemap-prefilter* NIL)

(defun free-ibl ()
  (when *t-cubemap-prefilter*
    (free *t-cubemap-prefilter*)
    (setf *t-cubemap-prefilter* NIL))
  (when *t-cubemap-live*
    (free *t-cubemap-live*)
    (setf *t-cubemap-live* NIL)))

(defun init-cubemap-ibl ()
  ;; IBL - Diffuse ambient
  (unless *t-cubemap-live*
    (setf *t-cubemap-live*
          (make-texture
           NIL
           :element-type :rgb16f
           :dimensions '(32 32)
           :cubes t))
    (setf *s-cubemap-live*
          (sample *t-cubemap-live*
                  :minify-filter :linear
                  :magnify-filter :linear
                  :wrap :clamp-to-edge)))
  ;; IBL - Specular ambient
  ;; 1) prefilter
  (unless *t-cubemap-prefilter*
    (setf *t-cubemap-prefilter*
          (make-texture
           NIL
           :element-type :rgb16f
           :dimensions '(128 128)
           :mipmap 5
           :cubes t))
    (setf *s-cubemap-prefilter*
          (sample *t-cubemap-prefilter*
                  :magnify-filter :linear
                  :wrap :clamp-to-edge)))
  ;; 2) BRDF
  (unless *f-brdf*
    (setf *f-brdf* (make-fbo `(0 :element-type :rg16f :dimensions (512 512))))
    (setf *t-brdf* (attachment-tex *f-brdf* 0))
    (setf *s-brdf*
          (sample *t-brdf*
                  :wrap :clamp-to-edge
                  :magnify-filter :linear
                  :minify-filter  :linear))))

;; Is kind of garbage that I need a source cubemap to begin with
;; instead of use the scene. But, It makes it easier so i can just
;; use a separate render pipeline just for the source cubemap.
(defun update-cubemap-ibl (&optional (src-tex *t-cubemap*) (src-sam *s-cubemap*))
  (declare (type cepl:texture src-tex)
           (type cepl:sampler src-sam))
  (assert (texture-cubes-p src-tex))
  ;; Diffuse
  (unless *prefilter*
    (cubemap-render-to-prefilter-cubemap *camera-cubemap*
                                         src-tex
                                         src-sam
                                         *t-cubemap-prefilter*)
    (setf *prefilter* T))
  ;; IBL - Specular
  (unless *brdf*
    (setf (resolution (current-viewport)) (v! 512 512))
    (map-g-into *f-brdf* #'brdf-pipe *bs*)
    (setf *brdf* T))
  (unless *saved*
    (cubemap-render-to-irradiance-cubemap *camera-cubemap*
                                          src-tex
                                          src-sam
                                          *t-cubemap-live*)
    (setf *saved* T)))

;;--------------------------------------------------
;; Pipeline to create a BRDF 2d lut
;;--------------------------------------------------
;; ON INIT:
;;
;; (unless *f-brdf*
;;     (setf *f-brdf*
;;           (make-fbo (list 0 :element-type :rg16f :dimensions '(512 512))))
;;     (setf *t-brdf* (attachment-tex *f-brdf* 0))
;;     (setf *s-brdf*
;;           (cepl:sample *t-brdf* :wrap :clamp-to-edge
;;                                 :minify-filter :linear)))
;; ON DRAW LOOP:
;;
;; (unless *brdf*
;;   (setf *brdf* T)
;;   (setf (resolution (current-viewport)) (v! 512 512))
;;   (map-g-into *f-brdf* #'brdf-pipe *bs*))
(defun-g integrate-brdf ((n-dot-v :float)
                         (roughness :float))
  ;; You might've recalled from the theory tutorial that the geometry
  ;; term of the BRDF is slightly different when used alongside IBL as
  ;; its k variable has a slightly different interpretation:
  (labels ((geometry-schlick-ggx ((n-dot-v :float)
                                  (roughness :float))
             (let* ((a roughness)
                    (k (/ (* a a) 2))
                    (nom n-dot-v)
                    (denom (+ k (* n-dot-v (- 1 k)))))
               (/ nom denom)))
           (geometry-smith ((n :vec3)
                            (v :vec3)
                            (l :vec3)
                            (roughness :float))
             (let* ((n-dot-v (max (dot n v) 0))
                    (n-dot-l (max (dot n l) 0))
                    (ggx2 (geometry-schlick-ggx n-dot-v roughness))
                    (ggx1 (geometry-schlick-ggx n-dot-l roughness)))
               (* ggx1 ggx2))))
    (let* ((v (v! (sqrt (- 1 (* n-dot-v n-dot-v)))
                  0
                  n-dot-v))
           (a 0f0)
           (b 0f0)
           (n (v! 0 0 1)))
      (dotimes (i 1024)
        (let* (;; generates a sample vector that's biased towards the
               ;; preferred alignment direction (importance sampling).
               (xi (hammersley-nth-2d 1024 i))
               (h  (importance-sample-ggx xi n roughness))
               (l  (normalize (+ (- v) (* 2 (dot v h) h))))
               (n-dot-l (max (z l) 0))
               (n-dot-h (max (z h) 0))
               (v-dot-h (max (dot v h) 0)))
          (when (> n-dot-l 0)
            (let* ((g (geometry-smith n v l roughness))
                   (g-vis (/ (* g v-dot-h) (* n-dot-h n-dot-v)))
                   (fc (pow (- 1 v-dot-h) 5)))
              (incf a (* (- 1 fc) g-vis))
              (incf b (* fc g-vis))))))
      (divf a 1024f0)
      (divf b 1024f0)
      (v! a b))))

(defun-g brdf-frag ((uv :vec2))
  (integrate-brdf (x uv) (y uv)))

(defpipeline-g brdf-pipe (:points)
  :fragment (brdf-frag :vec2))

;;----------------------------------------
;; GI - IBL - Irradiance map generator
(defun-g cube-down-frag ((frag-pos :vec3)
                         &uniform
                         (tex :sampler-cube))
  (let* ((normal (normalize frag-pos))
         (irradiance (v! 0 0 0))
         (up (v! 0 1 0))
         (right (cross up normal))
         (up (cross normal right))
         (sample-delta .025)
         (nr-samples 0f0))
    (for
     (phi 0f0) (< phi (* 2 +pi+)) (setf phi (+ phi sample-delta))
     (for
      (theta 0f0) (< theta (* .5 +pi+)) (setf theta (+ theta sample-delta))
      (let* (;; spherical to cartesian (in tangent space)
             (tangent-sample (v! (* (sin theta) (cos phi))
                                 (* (sin theta) (sin phi))
                                 (cos theta)))
             ;; Tangent space to world
             (sample-vec (+ (* right (x tangent-sample))
                            (* up (y tangent-sample))
                            (* normal (z tangent-sample)))))
        (incf irradiance (* (s~ (texture tex sample-vec) :xyz)
                            (cos theta)
                            (sin theta)))
        (incf nr-samples 1f0))))
    (setf irradiance (* +pi+ irradiance (/ 1 nr-samples)))
    (v! irradiance 1)))

(defpipeline-g cube-down-pipe ()
  :vertex (cubemap-vert g-pnt)
  :fragment (cube-down-frag :vec3))

;;--------------------------------------------------
;; GI - IBL - Irradiance light
(defun cubemap-render-to-irradiance-cubemap (camera
                                             src-cubemap
                                             src-cubemap-sample
                                             dst-cubemap)
  "Adviced dimensions of DST-CUBEMAP of 32x32"
  (declare (type texture src-cubemap dst-cubemap))
  (let ((dst-dimensions (dimensions dst-cubemap))
        (src-dimensions (dimensions src-cubemap)))
    (assert (eq :RGB16F (element-type (texref dst-cubemap :cube-face 0))))
    (assert (apply #'= src-dimensions))
    (assert (apply #'= dst-dimensions))
    (setf (resolution (current-viewport)) (v! dst-dimensions))
    (setf (fov camera) 90f0)
    (with-free*
        ((fbo
          (make-fbo
           (list 0 :dimensions dst-dimensions :element-type :rgb16f)
           (list :d :dimensions dst-dimensions))))
      (loop
         :for side :in *cubemap-sides*
         :for rotation :in *cubemap-rotations*
         :for face :from 0
         :do
         ;; Rotate camera
           (destructuring-bind (up from to) rotation
             (setf (rot camera) (q:look-at up from to)))
         ;; Switch FBO texture for one of the cubemap
           (setf (attachment fbo 0)
                 (texref dst-cubemap :cube-face face))
           (with-setf* ((cull-face) :front
                        (depth-test-function) #'<=)
             (with-fbo-bound (fbo)
               (clear fbo)
               (map-g #'cube-down-pipe (box)
                      :tex src-cubemap-sample
                      :mod-clip (m4:* (projection camera)
                                      (world->view camera)))))))))

;;--------------------------------------------------
;; GI - IBL - Prefilter cubemap
;;
;; This gives us a sample vector somewhat oriented around the expected
;; microsurface's halfway vector based on some input roughness and the
;; low-discrepancy sequence value Xi. Note that Epic Games uses the
;; squared roughness for better visual results as based on Disney's
;; original PBR research.
(defun-g importance-sample-ggx ((xi :vec2)
                                (n :vec3)
                                (roughness :float))
  (let* ((a (* roughness roughness))
         (phi (* 2 +pi+ (x xi)))
         (cos-theta (sqrt (/ (- 1 (y xi))
                             (+ 1 (* (1- (* a a)) (y xi))))))
         (sin-theta (sqrt (- 1 (* cos-theta cos-theta))))
         ;; from spherical coordinates to cartesian coordinates
         (h (v! (* (cos phi) sin-theta)
                (* (sin phi) sin-theta)
                cos-theta))
         ;; from tangent-space vector to world-space sample vector
         (up (if (< (abs (z n)) .999)
                 (v! 0 0 1)
                 (v! 1 0 0)))
         (tangent (normalize (cross up n)))
         (bitangent (cross n tangent))
         (sample-vec (+ (* (x h) tangent)
                        (* (y h) bitangent)
                        (* (z h) n))))
    (normalize sample-vec)))

(defun-g prefilter-frag ((frag-pos :vec3)
                         &uniform
                         (environment-map :sampler-cube)
                         (roughness :float))
  (let* ((n (normalize frag-pos))
         ;; make the simplyfying assumption that V equals R equals the normal
         (r n)
         (v r)
         (total-weight 0f0)
         (prefiltered-color (vec3 0f0)))
    (dotimes (i 1024)
      (let* ((xi (hammersley-nth-2d 1024 i))
             (h (importance-sample-ggx xi n roughness))
             (l (normalize (- (* 2 h (dot v h)) v)))
             (n-dot-l (max (dot n l) 0f0)))
        (when (> n-dot-l 0)
          (let* ((d (distribution-ggx n h roughness))
                 (n-dot-h (max (dot n h) 0))
                 (h-dot-v (max (dot h v) 0))
                 (pdf (+ .0001 (/ (* d n-dot-h)
                                  (* 4 h-dot-v))))
                 (resolution 512)
                 (sa-texel (/ (* 4 +pi+)
                              (* 6 resolution resolution)))
                 (sa-sample (/ (+ .0001 (* pdf 1024))))
                 (mip-level (if (= 0 roughness)
                                0f0
                                (* .5 (log2 (/ sa-sample sa-texel))))))
            (incf prefiltered-color
                  (* (s~ (texture-lod environment-map l mip-level) :xyz)
                     n-dot-l))
            (incf total-weight n-dot-l)))))
    (divf prefiltered-color (vec3 total-weight))
    (v! prefiltered-color 1)))

(defpipeline-g prefilter-pipe ()
  :vertex (cubemap-vert g-pnt)
  :fragment (prefilter-frag :vec3))

;;--------------------------------------------------
;; GI - IBL - Specular prefilter cubemap
;; https://learnopengl.com/PBR/IBL/Specular-IBL
(defun cubemap-render-to-prefilter-cubemap (camera
                                            src-cubemap
                                            src-cubemap-sample
                                            dst-cubemap)
  "Adviced DST-CUBEMAP dimensions 128x128"
  (let ((dst-dimensions (dimensions dst-cubemap))
        (src-dimensions (dimensions src-cubemap)))
    (assert (= 5 (texture-mipmap-levels dst-cubemap)))
    (assert (texture-cubes-p dst-cubemap))
    (assert (texture-cubes-p src-cubemap))
    (assert (eq :RGB16F (texture-element-type dst-cubemap)))
    (assert (apply #'= dst-dimensions))
    (setf (fov camera) 90f0)
    (dotimes (mip 5)
      (let* ((mip-width  (floor (* 128 (expt .5 mip))))
             (mip-height mip-width)
             (dimensions (list mip-width mip-height))
             (roughness  (coerce (/ mip (- 5 1)) 'single-float)))
        (setf (resolution (current-viewport)) (v! dimensions))
        (with-free*
            ((fbo
              (make-fbo
               (list 0 :dimensions dimensions :element-type :rgb16f)
               (list :d :dimensions dimensions))))
          (loop
             :for side :in *cubemap-sides*
             :for rotation :in *cubemap-rotations*
             :for face :from 0
             :do
             ;; Rotate camera
               (destructuring-bind (up from to) rotation
                 (setf (rot camera) (q:look-at up from to)))
             ;; Switch FBO texture for one of the cubemap
               (setf (attachment fbo 0)
                     (texref dst-cubemap :cube-face face :mipmap-level mip))
               (with-setf* ((cull-face) :front
                            (depth-test-function) #'<=)
                 (with-fbo-bound (fbo)
                   (clear-fbo fbo)
                   (map-g #'prefilter-pipe (box)
                          :roughness roughness
                          :environment-map src-cubemap-sample
                          :mod-clip (m4:* (projection camera)
                                          (world->view camera)))))))))))

