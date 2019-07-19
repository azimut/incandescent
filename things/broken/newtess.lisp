(in-package :incandescent)

(defvar *blend-tess* (make-blending-params))

(defclass sphere-tess (actor) ())

(defun make-sphere-tess ()
  (let ((obj (make-instance 'sphere-tess :buf (sphere 1 5 5))))
    (push obj *actors*)
    obj))

#+nil
(progn (free-actors)
       (make-sphere-tess)
       ;;(make-clouds)
       )
(defvar *scale* 1f0)
(defmethod draw ((actor sphere-tess) camera time)
  (with-setf (cull-face) nil
    (with-blending *blend-tess*
      (with-slots (buf) actor
        (map-g #'draw-sphere buf
               ;; IBL
               :brdf-lut       *s-brdf*
               :prefilter-map  *s-cubemap-prefilter*
               :irradiance-map *s-cubemap-live*
               ;;
               :cam-pos (pos camera)
               :time time
               :scale *scale*
               :normal-mat
               ;;(m3:transpose)
               ;;#+nil
               (m3:transpose
                (m4:to-mat3
                 (m4:* (model->world actor)
                       (world->view camera))))
               ;;(m3:identity)
               #+nil
               (m4:to-mat3
                (m4:transpose
                 (m4:* (m4:look-at (v! 0 1 0) (pos camera) (v! 0 0 0))
                       (q:to-mat4 (rot actor)))))
               :model->world (model->world actor)
               :model->clip  (model->clip actor camera))))))

(defun-g sphere-vert ((vert g-pnt) &uniform (model->clip :mat4)
                      (scale :float))
  (values (* model->clip (v! (* scale (pos vert)) 1))
          (* scale (pos vert))))

(defparameter *inner* 1f0)
(defparameter *outer* 1f0)


(defun-g sphere-tess-con ((position (:vec3 3))
                          &uniform
                          (time :float))
  (declare (output-patch :vertices 3))
  (let ((tess-level-inner *inner*)
        (tess-level-outer *outer*))
    (when (= gl-invocation-id 0)
      (setf (aref gl-tess-level-inner 0) tess-level-inner
            ;;
            (aref gl-tess-level-outer 0) tess-level-outer
            (aref gl-tess-level-outer 1) tess-level-outer
            (aref gl-tess-level-outer 2) tess-level-outer)))
  (aref position gl-invocation-id))

(defun-g sphere-tess-eval ((position (:vec3 3))
                           &uniform
                           (model->clip :mat4))
  (declare (tessellate-to
            :primitive :triangles
            :spacing   :equal
            :order     :ccw))
  (let* ((p0  (* (x gl-tess-coord) (aref position 0)))
         (p1  (* (y gl-tess-coord) (aref position 1)))
         (p2  (* (z gl-tess-coord) (aref position 2)))
         (pos (normalize (+ p0 p1 p2))))
    (values (* model->clip (v! pos 1))
            pos)))

(defun-g sphere-geom ((position       (:vec3 3))
                      &uniform
                      (normal-mat      :mat3)
                      (model->world    :mat4))
  (declare (output-primitive :kind :triangle-strip
                             :max-vertices 3))
  (let* ((a (- (aref position 2) (aref position 0)))
         (b (- (aref position 1) (aref position 0)))
         (facet-normal (* normal-mat (normalize (cross a b)))))
    (emit ()
          (gl-position (aref gl-in 0))
          (s~ (* model->world (v! (aref position 0) 1)) :xyz)
          facet-normal)
    (emit ()
          (gl-position (aref gl-in 1))
          (s~ (* model->world (v! (aref position 1) 1)) :xyz)
          facet-normal)
    (emit ()
          (gl-position (aref gl-in 2))
          (s~ (* model->world (v! (aref position 2) 1)) :xyz)
          facet-normal)
    (end-primitive)
    (values)))

(defun-g sphere-frag ((frag-pos       :vec3)
                      (facet-normal   :vec3)
                      &uniform
                      ;; IBL
                      (irradiance-map :sampler-cube)
                      (prefilter-map  :sampler-cube)
                      (brdf-lut       :sampler-2d)
                      (model->world  :mat4)
                      (cam-pos        :vec3))
  (let* ((light-dir        (normalize (- *light-pos* frag-pos)))
         (view-dir         (normalize (- cam-pos    frag-pos)))
         (color (v! 0 .75 .75))
         (base-color (v! 0 .5 .5))
         (ambient-material (v! .04 .04 .04))
         ;;
         (alpha .3)
         (alpha-height .9)
         (alpha-smooth .9)
         (local-pos      (- frag-pos (s~ (* model->world (v! 0 0 0 1))
                                         :xyz)))
         (alpha          (max .01 alpha))
         (fade-alpha     (mix 1 alpha (+ (y local-pos) alpha-height)))
         (fade-alpha     (max alpha
                              (smoothstep alpha
                                          (max alpha alpha-smooth)
                                          fade-alpha)))
         ;;
         (n     (normalize facet-normal))
         (l     light-dir)
         (df    (saturate (dot n l)))
         ;;(final-color (+ ambient-material (* df color)))
         
         ;;
         (rim-power  1f0)
         (rim-smooth 1f0)
         (rim-color  (v! 0 .75 .75))
         (d          (pow (dot n view-dir) rim-power))
         (rim        (smoothstep .5 (max .5 rim-smooth) d))
         ;;
         (fade-alpha (mix fade-alpha 1f0 rim))
         (mix-color  (mix color (* color base-color) fade-alpha))
         ;;
         #+nil
         (final-color (dir-light-apply-spec mix-color
                                            (v! 1 1 1)
                                            *light-pos*
                                            frag-pos
                                            facet-normal
                                            cam-pos
                                            .1 .9))
         (f0 (vec3 .04))
         (metallic .1)
         (roughness .5)
         (ao 1f0)
         (f0 (mix f0 color metallic))
         #+nil
         (ambient (ambient-ibl view-dir
                               n
                               f0
                               brdf-lut
                               prefilter-map
                               irradiance-map
                               roughness
                               metallic
                               mix-color
                               ao))
         (ambient (v! 0 .04 .04))
         (final-color (+
                       ;;#+nil
                       (pbr-direct-lum *light-pos* frag-pos
                                       view-dir
                                       n
                                       roughness
                                       f0
                                       metallic
                                       mix-color
                                       2f0)
                       #+nil
                       (pbr-point-lum (v! 0 0 0)
                                      frag-pos
                                      view-dir n
                                      roughness
                                      f0
                                      metallic
                                      mix-color
                                      .2f0
                                      .35
                                      .44)
                       ;;ambient
                       ))
         ;;
         (emission-color (v! .1 .1 .1))
         (emission (+ (* mix-color emission-color)
                      (* final-color rim)
                      ))
         (crystal-power 1.8)
         #+nil
         (final-color (* final-color
                         (pow fade-alpha (- 1 crystal-power))))
         )
    (values  (v! final-color fade-alpha)
             (v! emission 1)
             )
    ;;(v! (v3! df) 1)
    ;;n
    ;;(v! 1 0 0 1)
    ))

(defpipeline-g draw-sphere ((:patch 3))
  :vertex (sphere-vert g-pnt)
  :tessellation-control (sphere-tess-con (:vec3 3))
  :tessellation-evaluation (sphere-tess-eval (:vec3 3))
  :geometry (sphere-geom (:vec3 3))
  :fragment (sphere-frag :vec3 :vec3))
