(in-package #:incandescent)

;; NT: I think it needs several 3D textures one per orientation

(defun-g get-direction-weights ((direction :vec3))
  (let (;;(color (* direction direction))
        (color (abs direction))
        )
    color))

(v-def-glsl-template-fun << (x y) "(~a << ~a)" (v-int v-int) v-int :pure t)
(defun-g trace-voxel-cone ((from          :vec3)
                           (direction     :vec3)
                           ;;
                           (aperture      :float)
                           (offset        :float)
                           (max-distance  :float)
                           (voxel-image   :sampler-3d))
  (let* (;;(voxel-volume-bits (int 6)) ;; 6=64 7=128
         ;;(voxel-volume-size (<< 1 voxel-volume-bits))
         (voxel-volume-size 64f0)
         (voxel-volume-inverse-size (/ 1f0 voxel-volume-size))
         ;;
         (direction (normalize direction))
         (negative-direction (less-than direction (v! 0 0 0)))
         (double-aperture (max voxel-volume-inverse-size (* 2 aperture)))
         (dist offset)
         (direction-weights (get-direction-weights direction))
         (position (+ from (* dist direction)))
         (accumulator (vec4 0))
         (max-distance (min max-distance 1.1421356237)))
    (while
     (and (< dist max-distance)
          (< (w accumulator) 1f0))
     (let* ((diameter (max (* voxel-volume-inverse-size .5) (* double-aperture dist)))
            (mip-map-level (max 0f0 (+ 0f0 (log2 (* diameter voxel-volume-size)))))
            #+nil
            (voxel (+ (* (texture-lod voxel-image position mip-map-level) (x direction-weights))
                      (* (texture-lod voxel-image position mip-map-level) (y direction-weights))
                      (* (texture-lod voxel-image position mip-map-level) (z direction-weights))))
            (acol (texture-lod voxel-image position mip-map-level))
            #+nil
            (voxel (+ (* acol (x direction-weights))
                      (* acol (y direction-weights))
                      (* acol (z direction-weights))))
            (voxel (* acol (v! (s~ direction-weights :xyz) 1)))
            )
       (incf accumulator (* (- 1f0 (w accumulator)) voxel))
       (incf dist        (* 1f0 (max diameter voxel-volume-inverse-size)))
       (setf position    (+ from (* dist direction)))))
    (max accumulator (vec4 0f0))
    ;;accumulator
    ))

(defun-g trace-diffuse-voxel-cone ((from        :vec3)
                                   (normal      :vec3)
                                   (voxel-image :sampler-3d))
  (let* ((cone-directions (vector (v! 0.0 0.0 1.0)
                                  (v! 0.0  .707106781 .707106781)
                                  (v! 0.0 -.707106781 .707106781)
                                  (v! .707106781  0.0 .707106781)
                                  (v! -.707106781 0.0 .707106781)))
         (cone-weights (vector .28 .18 .18 .18 .18))
         (cone-apertures (vector 1f0 1f0 1f0 1f0 1f0))
         (cone-offset -0.01)
         (voxel-volume-inverse-size (/ 1f0 64f0))
         (offset (* 4f0 voxel-volume-inverse-size))
         (max-distance 1f0)
         (normal (normalize normal))
         (normal-offset (* normal
                           (+ 1f0 (* 4f0 .80810678118))
                           voxel-volume-inverse-size))
         (cone-origin (+ from normal-offset))
         (t0 (cross (v! 0 1 0) normal))
         (t1 (cross (v! 0 0 1) normal))
         (tangent (normalize (if (< (length t0) (length t1)) t1 t0)))
         (bitangent (normalize (cross tangent normal)))
         (tangent-space (m! tangent bitangent normal))
         (color (v! 0 0 0 0)))
    (dotimes (i 5)
      (let ((direction (* tangent-space (s~ (aref cone-directions i) :xyz))))
        (incf color (* (aref cone-weights i)
                       (v! (s~ (trace-voxel-cone
                                (+ cone-origin (* cone-offset direction))
                                direction
                                (aref cone-apertures i)
                                offset
                                max-distance
                                voxel-image)
                               :xyz)
                           1)))))
    ;;(/ (s~ color :xyz) (max (w color) 1e-6))
    (s~ color :xyz)
    ))
;;--------------------------------------------------
;; FIXME: whatever I need on the W component of the voxel I don't have it
;;(trace-shadow-cone norm pos light-pos voxel-light)
(defun-g trace-shadow-cone ((normal :vec3)
                            (from   :vec3)
                            (to     :vec3)
                            (voxel-light :sampler-3d))
  (let* ((voxel-size   #.(/ 1f0 64f0))
         (aperture     #.(tan (radians 5f0)))
         (doubled-aperture
           (max voxel-size (* aperture 2f0)))
         (s            0.33333)
         (from         (+ from (* normal 2f0 voxel-size)))
         (direction    (- to from))
         (max-distance (length direction))
         (dist         (* 2.5 voxel-size))
         (accumulator  0f0)
         (direction    (/ direction max-distance))
         (max-distance (min max-distance 1.41421356237))
         ;;(dist (+ dist ))
         ;;(dist (+ dist (* s voxel-size)))
         (position (+ from (* direction dist))))
    (while (and (< accumulator 1f0)
                (< dist        max-distance)
                (inside-cube-p position 0f0))
           ;;
           (let* ((diameter     (max (* voxel-size .5) (* dist doubled-aperture)))
                  (mipmap-level (max 0f0 (log2 (+ 1f0 (* diameter 64f0))))))
             (incf accumulator
                   (* (- 1 accumulator)
                      (clamp (* 1f0 (w (texture-lod voxel-light
                                                    position
                                                    mipmap-level)))
                             0f0 1f0)))
             (incf dist (* s (max diameter voxel-size)))
             (setf position (+ from (* direction dist)))))
    (clamp (- 1f0 accumulator) 0f0 1f0)))