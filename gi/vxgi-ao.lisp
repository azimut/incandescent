(in-package #:incandescent)

(defun-g trace-diffuse-voxel-cone ((from        :vec3)
                                   (direction   :vec3)
                                   (voxel-light :sampler-3d))
  "Traces a diffuse voxel cone."
  (let* ((voxel-size     #.(/ 64f0))
         (mipmap-hardcap 5.4)
         (sqrt2          1.414213);F 1.414213 ; A 1.73205080757
         ;;
         (direction      (normalize direction))
         (cone-spread    .655785173935); "aperture" ; F .325; A .55785173935
         (acc            (vec4 0f0))
         ;; Controls bleeding from close surfaces.
         ;; Low values look rather bad if using shadow cone tracing.
         ;; Might be a better choice to use shadow maps and lower this value.
         (dist           #.(* 9.5f0 (/ 1f0 64f0))
                         )); F .1953125 ; A 0.04 * voxelgiOffset("1"*100/100)
    ;; Trace
    (while (and (< dist sqrt2)
                (< (w acc) 1f0))
           (let* ((sample-pos (scale-and-bias (+ from (* dist direction))))
                  (l          (+ 1f0 (/ (* cone-spread dist) voxel-size)))
                  (level      (log2 l))
                  (ll         (* (+ 1f0 level) (+ 1f0 level)))
                  (voxel      (texture-lod voxel-light
                                           sample-pos
                                           (min mipmap-hardcap level)))
                  (a (- 1 (w voxel))))
             ;;(incf acc  (* 0.075 ll voxel (pow (- 1 (w voxel)) 2f0)))
             (incf (s~ acc :xyz) (* a (s~ voxel :xyz)))
             (incf (w acc) (w voxel))
             (incf dist (* ll voxel-size 2))))
    acc))

(defun-g indirect-diffuse-light ((wpos        :vec3)
                                 (normal      :vec3)
                                 (voxel-light :sampler-3d)
                                 (albedo      :vec3))
  (let* ((isqrt2                  .707106)
         (voxel-size              #.(/ 64f0))
         (diffuse-indirect-factor .52)
         ;; Angle mix (1.0f => orthogonal direction,
         ;;            0.0f => direction of normal).
         (angle-mix               .5)
         (w                       (v! 1 1 1)) ; cone weights
         ;; Find a base for the side cones with the normal as one
         ;; of its base vectors.
         (ortho                   (normalize (tangent normal)))
         (ortho2                  (normalize (cross ortho normal)))
         ;; Find base vectors for the corner cones too.
         (corner                  (* 0.5 (+ ortho ortho2)))
         (corner2                 (* 0.5 (- ortho ortho2)))
         ;; Find start position of trace (start with a bit of offset).
         (n-offset                (* normal (+ 1 (* 4 isqrt2)) voxel-size))
         (c-origin                (+ wpos n-offset))
         ;; Accumulate indirect diffuse light.
         (acc                     (v! 0 0 0 0))
         ;; We offset forward in normal direction, and backward in cone direction.
         ;; Backward in cone direction improves GI, and forward direction removes
         ;; artifacts.
         (cone-offset             -0.01))
    ;; Trace front cone
    (incf acc (* (x w) (trace-diffuse-voxel-cone (+ c-origin (* cone-offset normal))
                                                 normal
                                                 voxel-light)))
    ;; Trace 4 side cones.
    (incf acc (* (y w) (trace-diffuse-voxel-cone (+ c-origin (* cone-offset ortho))
                                                 (mix normal ortho angle-mix)
                                                 voxel-light)))
    (incf acc (* (y w) (trace-diffuse-voxel-cone (- c-origin (* cone-offset ortho))
                                                 (mix normal (- ortho) angle-mix)
                                                 voxel-light)))
    (incf acc (* (y w) (trace-diffuse-voxel-cone (+ c-origin (* cone-offset ortho2))
                                                 (mix normal ortho2 angle-mix)
                                                 voxel-light)))
    (incf acc (* (y w) (trace-diffuse-voxel-cone (- c-origin (* cone-offset ortho2))
                                                 (mix normal (- ortho2) angle-mix)
                                                 voxel-light)))
    ;; Trace 4 corner cones.
    (incf acc (* (z w) (trace-diffuse-voxel-cone (+ c-origin (* cone-offset corner))
                                                 (mix normal corner angle-mix)
                                                 voxel-light)))
    (incf acc (* (z w) (trace-diffuse-voxel-cone (- c-origin (* cone-offset corner))
                                                 (mix normal (- corner) angle-mix)
                                                 voxel-light)))
    (incf acc (* (z w) (trace-diffuse-voxel-cone (+ c-origin (* cone-offset corner2))
                                                 (mix normal corner2 angle-mix)
                                                 voxel-light)))
    (incf acc (* (z w) (trace-diffuse-voxel-cone (- c-origin (* cone-offset corner2))
                                                 (mix normal (- corner2) angle-mix)
                                                 voxel-light)))
    ;; Return result.
    (v! (* diffuse-indirect-factor
           (s~ acc :xyz)
           (+ albedo 0.001))
        (w acc))))
