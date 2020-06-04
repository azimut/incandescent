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
       (make-clouds))

(defmethod draw ((actor sphere-tess) camera time)
  (with-setf (cull-face) nil
    (with-blending *blend-tess*
      (with-slots (buf) actor
        (map-g #'draw-sphere buf
               :cam-pos (pos camera)
               :time time
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
               :model->clip (model->clip actor camera))))))

(defun-g sphere-vert ((vert g-pnt) &uniform (model->clip :mat4))
  (values (* model->clip (v! (pos vert) 1))
          (pos vert)))

(defun-g sphere-tess-con ((position (:vec3 3))
                          &uniform
                          (time :float))
  (declare (output-patch :vertices 3))
  (let ((tess-level-inner 0f0)
        (tess-level-outer (mod (* .01 time) 5)))
    (when (= gl-invocation-id 0)
      (setf (aref gl-tess-level-inner 0) tess-level-inner
            ;;
            (aref gl-tess-level-outer 0) tess-level-outer
            (aref gl-tess-level-outer 1) tess-level-outer
            (aref gl-tess-level-outer 2) tess-level-outer)))
  (aref position gl-invocation-id))

(defun-g sphere-tess-eval ((position (:vec3 3)) &uniform (model->clip :mat4))
  (declare (tessellate-to
            :primitive :triangles
            :spacing   :equal
            :order     :ccw))
  (let* ((p0 (* (x gl-tess-coord) (aref position 0)))
         (p1 (* (y gl-tess-coord) (aref position 1)))
         (p2 (* (z gl-tess-coord) (aref position 2)))
         (pos (normalize (+ p0 p1 p2))))
    (values (* model->clip (v! pos 1))
            pos
            gl-tess-coord)))

(defun-g sphere-geom ((position       (:vec3 3))
                      (patch-distance (:vec3 3))
                      &uniform
                      (normal-mat      :mat3))
  (declare (output-primitive :kind :triangle-strip
                             :max-vertices 3))
  (let* ((a (- (aref position 2) (aref position 0)))
         (b (- (aref position 1) (aref position 0)))
         (facet-normal (* normal-mat (normalize (cross a b)))))
    (emit ()
          (gl-position (aref gl-in 0))
          (s~ (aref patch-distance 0) :xyz)
          facet-normal
          (v! 1 0 0))
    (emit ()
          (gl-position (aref gl-in 1))
          (s~ (aref patch-distance 1) :xyz)
          facet-normal
          (v! 0 1 0))
    (emit ()
          (gl-position (aref gl-in 2))
          (s~ (aref patch-distance 2) :xyz)
          facet-normal
          (v! 0 0 1))
    (end-primitive)
    (values)))

(defun-g sphere-frag ((patch-distance :vec3)
                      (facet-normal   :vec3)
                      (tri-distance   :vec3)
                      &uniform
                      (cam-pos        :vec3))
  (labels ((amplify ((d :float) (scale :float) (offset :float))
             (let* ((d (+ (* scale d) offset))
                    (d (clamp d 0 1)))
               (- 1 (exp2 (* d d -2))))))
    (let* ((light-dir        (normalize *light-pos*))
           (view-dir         (normalize (- cam-pos frag-pos)))
           (diffuse-material (v! 0 .75 .75))
           (ambient-material (v! .04 .04 .04))
           ;;
           (n     (normalize facet-normal))
           (l     light-dir)
           (df    (saturate (dot n l)))
           ;;
           (color (+ ambient-material (* df diffuse-material)))
           (d1    (min (min (x tri-distance) (y tri-distance))
                       (z tri-distance)))
           (d2    (min (min (x patch-distance) (y patch-distance))
                       (z patch-distance)))
           ;;
           (rim-power 1f0)
           (rim-smooth 1f0)
           (rim-color (v! .4 .1 .1))
           (d (- 1 (pow (dot n view-dir) rim-power)))
           (rim (smoothstep .5 (max .5 rim-smooth) d))
           (emission (* rim-color rim)))
      (values (v! color .8)
              (v! emission 1))
      ;;(v! patch-distance 1)
      ;;(v! (v3! df) 1)
      ;;n
      ;;(v! 1 0 0 1)
      )))

(defpipeline-g draw-sphere ((:patch 3))
  :vertex (sphere-vert g-pnt)
  :tessellation-control (sphere-tess-con (:vec3 3))
  :tessellation-evaluation (sphere-tess-eval (:vec3 3))
  :geometry (sphere-geom (:vec3 3) (:vec3 3))
  :fragment (sphere-frag :vec3 :vec3 :vec3))
