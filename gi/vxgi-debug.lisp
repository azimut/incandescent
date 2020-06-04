(in-package #:incandescent)

(defun-g debug-voxel-vert ((position :vec3)
                           &uniform
                           (voxel-image  :sampler-3d)
                           (level        :float)
                           (voxel-dim    :float)
                           (voxel-min    :vec3)
                           (voxel-max    :vec3)
                           (voxel-center :vec3))
  (let* ((instance (float gl-instance-id))
         (x (fract (/ instance voxel-dim)))
         (instance (floor (/ instance voxel-dim)))
         (y (fract (/ instance voxel-dim)))
         (instance (floor (/ instance voxel-dim)))
         (z (/ instance voxel-dim))
         (voxel-position (+ (v! x y z) (/ .5 voxel-dim)))
         (tc voxel-position)
         (wpos (+ (mix voxel-min voxel-max voxel-position)
                  position
                  voxel-center)))
    (values (v! wpos 1)
            wpos
            (texture-lod voxel-image tc level))))

(defun-g voxel-world-size ((voxel-min :vec3)
                           (voxel-max :vec3)
                           (voxel-dim :float))
  (/ (- voxel-max voxel-min) voxel-dim))

(defun-g debug-voxel-geom ((pos   (:vec3 1))
                           (color (:vec4 1))
                           &uniform
                           (mvp          :mat4)
                           (voxel-dim    :float)
                           (voxel-min    :vec3)
                           (voxel-max    :vec3)
                           (voxel-center :vec3))
  (declare (output-primitive :kind :triangle-strip
                             :max-vertices 36))
  (let ((cube-indices  (vector 5 4 1 0 0 0
                               0 3 1 2
                               5 6 4 7
                               0 3 3 3
                               2 7 6))
        (cube-vertices (vector (v! -0.5  0.5 -0.5)
                               (v!  0.5  0.5 -0.5)
                               (v!  0.5  0.5  0.5)
                               (v! -0.5  0.5  0.5)
                               ;;
                               (v! -0.5 -0.5 -0.5)
                               (v!  0.5 -0.5 -0.5)
                               (v!  0.5 -0.5  0.5)
                               (v! -0.5 -0.5  0.5))))
    (when (> (w (aref color 0)) 0)
      (dotimes (i (length cube-indices))
        (let ((position (+ (* (/ (- voxel-max voxel-min) voxel-dim)
                              (aref cube-vertices (aref cube-indices i)))
                           (aref pos 0))))
          (emit () (* mvp (v! position 1))
                (aref color 0))))
      (end-primitive)))
  (values))

(defun-g debug-voxel-frag ((color :vec4))
  color
  (v! 0 0 1 1)
  )

(defpipeline-g debug-voxel-pipe (:points)
  :vertex   (debug-voxel-vert :vec3)
  :geometry (debug-voxel-geom (:vec3 1) (:vec4 1))
  :fragment (debug-voxel-frag :vec4))

(let ((blend (make-blending-params)))
  (defun draw-debug-voxel ()
    ;;#+nil
    (with-setf* (;;(depth-test-function) #'always
                 )
      (with-blending blend
        (with-instances 262144 ; 64x64x64
          (map-g #'debug-voxel-pipe *point*
                 :voxel-image *voxel-light-zam*
                 :level 0f0
                 :mvp
                 (world->clip *currentcamera*)
                 #+nil
                 (m4:* *projection*
                       (m4:look-at (v! 0 1 0)
                                   (pos *camera*)
                                   (v3:normalize
                                    (q:to-direction (rot *camera*)))))
                 :voxel-min *min*
                 :voxel-max *max*
                 :voxel-center *center*
                 :voxel-dim (float *volume-dimension*)))))))
