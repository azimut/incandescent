(in-package #:incandescent)

(defun-g postprocess-defered ((uv :vec2)
                              &uniform
                              (time         :float)
                              (light-pos    :vec3)
                              (light-color  :vec3)
                              (light-dir    :vec3)
                              ;; IBL
                              (brdf-lut       :sampler-2d)
                              (prefilter-map  :sampler-cube)
                              (irradiance-map :sampler-cube)
                              ;;
                              (albedo-sam   :sampler-2d)
                              (position-sam :sampler-2d)
                              (normal-sam   :sampler-2d)
                              (metallic-sam :sampler-2d)
                              ;;
                              (shadowmap    :sampler-2d)
                              (light-vp     :mat4)
                              ;;
                              ;;(voxel-light  :sampler-3d)
                              (cam-pos       :vec3))
  (let* ((position (texture position-sam uv))
         (pos      (s~ position :xyz))
         (ao       (w position))
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
         ;; - VEC3
         ;;(indirect-specular (indirect-specular-light v norm spec voxel-light pos color metallic))
         ;;(indirect (indirect-diffuse-light pos norm voxel-light color))
         (shadow (shadow-factor shadowmap light-clip-pos))
         ;;#+nil
         (indirect 0f0)
         (indirect-specular 0f0)
         (indirect
           (ambient-ibl v norm irradiance-map rough metallic color ao))
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
