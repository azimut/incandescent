(in-package #:incandescent)

(progn (defun init-scene ()
         (let ((c1 (v! .3 .2 .2))
               (poff (v! -.3 .5 0))
               (scale .3f0)
               (dwall (v! 2 2 .35))
               (cwall (v! .9 .9 .9)))
           (free-actors)
           ;;(free-assimp-buffers)
           ;;(make-obstacle :scale 2f0 :pos (v! 0 0 0) :shadow-p nil)
           ;;(make-lucy :pos (v3:- (v! 0 0 0) poff) :scale scale)
           ;;(make-sun :radius .2f0)
           ;;(make-obstacle :pos (v! 0 0 0) :scale 2f0)
           ;; BOX
           (make-wall :pos (v! 0 -.5 -1.1); backwall
                      ;;:color cwall
                      :metallic .04
                      :dim dwall)
           (make-wall :pos (v! 0 -.5 1.1) ; nearwall
                      ;;:color cwall
                      :metallic .1
                      :dim dwall)
           ;; (make-wall :pos (v! 0 .7 -.9); ceil far
           ;;            :dim (v! 15 5 4)
           ;;            :uv-scale (v! 2 1)
           ;;            ;;:color cwall
           ;;            :scale .2
           ;;            :rot (q:from-axis-angle (v! 1 0 0) (radians 90))
           ;;            )
           ;; (make-wall :pos (v! 0 .8 .8) ; ceil near
           ;;            :dim (v! 12 5 2)
           ;;            :uv-scale (v! 2 1)
           ;;            ;;:color cwall
           ;;            :scale .2
           ;;            :rot (q:from-axis-angle (v! 1 0 0) (radians 90)))
           (make-wall :pos (v! 1 -.5 0) ;rwall
                      ;;:color cwall
                      :dim dwall
                      :rot (q:from-axis-angle (v! 0 -1 0) (radians 90)))
           ;; (make-obstacle :pos (v! -.95 0 0);lwall
           ;;                :color cwall
           ;;                :dim dwall
           ;;                :rot (q:from-axis-angle (v! 0 -1 0) (radians 90)))
           ;; RED AND BLUE
           ;; (make-obstacle :pos (v3:- (v! -.8 .11 .55) poff)
           ;;                :scale scale
           ;;                :rot (q:from-axis-angle (v! 0 1 0) (radians 90))
           ;;                :dim (v! .3 2 .3)
           ;;                :prop (v! 0 .7 .7 0)
           ;;                :color (v! .1 .1 .9))
           (make-obstacle
            :pos (v3:- (v! -.2 -.1 -.1) poff)
            :color (v! .9 .1 .1)
            :dim (v3! .3)
            :prop (v! 0 .7 .7 0))
           ;; floor
           ;;#+nil
           (make-piso :pos (v! 0 -.9 0)
                      :dim dwall
                      :uv-scale (v! 2 2)
                      :metallic 1f0
                      ;;:color cwall
                      ;;:prop (v! 0 .7 .7 .5)
                      :rot (q:from-axis-angle (v! 1 0 0) (radians 90)))))
       ;;#+nil
       ;;(init-scene)
       )

#+nil
(progn (free-actors)
       (in-scene 0
         (make-piso :pos (v! 0 -1 0))
         (make-pared :pos (v! 3 9 -7) :color (v! .2 .4 .9)
                     :rot (q:from-axis-angle (v! 1 0 0) (radians 90)))
         (make-pared :pos (v! -7 9 3) :color (v! .9 .4 .2)
                     :rot (q:*
                           (q:from-axis-angle (v! 1 0 0)
                                              (radians 90))
                           (q:from-axis-angle (v! 0 0 1)
                                              (radians -90))))
         (make-manticore)
         (make-manticore :pos (v! (random 1f0) (random 1f0) (random 1f0)))
         (make-manticore :pos (v! (random 1f0) (random 1f0) (random 1f0)))
         (make-manticore :pos (v! (random 1f0) (random 1f0) (random 1f0)))
         (make-manticore :pos (v! (random 1f0) (random 1f0) (random 1f0)))))

#+nil
(progn (free-actors)
       (make-piso :pos (v! 0 -1 0))
       ;;(make-piso :pos (v! 0 15 0) :rot (q:from-axis-angle (v! 0 0 -1) (radians 180)))
       ;;
       (make-pared :pos (v! 3 9 -7) :color (v! .2 .4 .9)
                   :rot (q:from-axis-angle (v! 1 0 0) (radians 90)))
       (make-pared :pos (v! -7 9 3) :color (v! .9 .4 .2)
                   :rot (q:*
                         (q:from-axis-angle (v! 1 0 0)
                                            (radians 90))
                         (q:from-axis-angle (v! 0 0 1)
                                            (radians -90))))
       (make-manticore :pos (v! 0 (random .5) 0))
       (make-manticore)
       (make-manticore :pos (v! (random 2f0) 1 (random 2f0)))
       (make-manticore :pos (v! (- (random 2f0)) (random 2.5) (- (random 2f0)))))

(defun-g postprocess-defered ((uv :vec2)
                              &uniform
                              (time         :float)
                              (light-pos    :vec3)
                              (light-color  :vec3)
                              (light-dir    :vec3)
                              ;;
                              (albedo-sam   :sampler-2d)
                              (position-sam :sampler-2d)
                              (normal-sam   :sampler-2d)
                              (metallic-sam :sampler-2d)
                              ;;
                              (shadowmap    :sampler-2d)
                              (light-vp     :mat4)
                              ;;
                              (tlpos     :sampler-2d)
                              (tcpos     :sampler-2d)
                              ;;
                              (voxel-light  :sampler-3d)
                              (cam-pos       :vec3))
  (let* ((position (texture position-sam uv))
         (pos      (s~ position :xyz))
         (ao       (w position))
         ;;(ao 1f0)
         ;;(ao       (* ao (x (texture ssao uv))))
         (albedo   (texture albedo-sam uv))
         (normal   (texture normal-sam uv))
         (rough    (w albedo))
         (spec     (w normal))
         (color    (s~ albedo :xyz))
         (norm     (normalize (s~ normal :xyz)))
         (metal    (texture metallic-sam uv))
         (metallic (x metal))
         (emissive (y metal)); emmisive?
         (v        (normalize (- pos cam-pos)))
         ;;
         (light-clip-pos (* light-vp (v! pos 1)))
         ;; - VEC4
         ;; (indirect-raw (indirect-diffuse-light pos norm voxel-light color))
         ;;(indirect     (s~ indirect-raw :xyz))
         ;;(indirect-ao  (- 1 (clamp (w indirect-raw) 0 1)))
         ;; - VEC3
         ;;(indirect-specular (indirect-specular-light v norm spec voxel-light pos color metallic))
         (indirect (indirect-diffuse-light pos norm voxel-light color))
         (shadow (shadow-factor shadowmap light-clip-pos))
         ;;(indirect-ao (saturate (pow (y indirect) (/ 1f0 22f0))))
         ;;#+nil
         ;;(indirect 0f0)
         (indirect-specular 0f0)
         ;;(indirect-ao 1f0)
         ;;(shadow 1f0)
         ;;
         ;;(final-color (vec3 shadow))
         ;;(final-color indirect)
         ;;(final-color (vec3 metallic))
         ;;(final-color norm)
         ;;(final-color (vec3 ao))
         ;;(final-color norm)
         ;;#+nil
         (final-color
           (+ (* color emissive)
              (+ (* ao indirect) indirect-specular)
              #+nil
              (+ (* (* (- 1f0 (fresnel-schlick-roughness
                               (max (dot norm (normalize (- pos cam-pos))) 0f0)
                               (mix (vec3 .04) color metallic)
                               rough))
                       (- 1f0 metallic))
                    (* ao indirect))
                 indirect-specular)
              ;;(* ao .03 color)
              ;;#+nil
              (* shadow
                 ;;(max shadow .01)
                 ;;#+nil
                 (+
                  ;;(dir-light-apply color light-color light-pos pos norm)
                  #+nil
                  (pbr-spotlight-lum light-pos
                                     pos
                                     (normalize (- cam-pos pos))
                                     norm
                                     rough
                                     metallic
                                     color
                                     spec
                                     light-color
                                     light-dir
                                     *cone-inner*
                                     *cone-outer*
                                     .027 .0028)
                  ;;#+nil
                  (pbr-direct-lum light-pos
                                  pos
                                  (normalize (- cam-pos pos))
                                  norm
                                  rough
                                  metallic
                                  color
                                  spec
                                  light-color)
                  #+nil
                  (pbr-point-lum light-pos
                                 pos
                                 (normalize (- cam-pos pos))
                                 norm
                                 rough
                                 f0
                                 metallic
                                 color
                                 spec
                                 .22
                                 .20
                                 (* 10 (v! .3 .8 .5)))))))
         ;; (ldr  (tone-map-reinhard final-color 1f0))
         ;; (luma (rgb->luma-bt601 ldr))
         )
    (values final-color
            ;;(v! ldr luma)
            )))

(defpipeline-g postprocess-defer-pipe (:points)
  :fragment (postprocess-defered :vec2))



