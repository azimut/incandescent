(in-package #:incandescent)

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
                              (god-rays     :sampler-2d)
                              ;;
                              (voxel-light  :sampler-3d)
                              (cam-pos       :vec3))
  (let* ((position (texture position-sam uv))
         (pos      (s~ position :xyz))
         (albedo   (texture albedo-sam uv))
         (normal   (texture normal-sam uv))
         (rough    (w albedo))
         (emissive (w position)); emmisive?
         (spec     (w normal))
         (color    (s~ albedo :xyz))
         (norm     (normalize (s~ normal :xyz)))
         (f0       (vec3 .04))
         (metal    (texture metallic-sam uv))
         (metallic (x metal))
         (f0       (mix f0 color metallic))
         (v        (normalize (- pos cam-pos)))
         ;;
         (light-clip-pos (* light-vp (v! pos 1)))
         ;; - VEC4
         ;; (indirect-raw (indirect-diffuse-light pos norm voxel-light color))
         ;; (indirect     (s~ indirect-raw :xyz))
         ;; (indirect-ao  (- 1 (clamp (w indirect-raw) 0 1)))
         ;; - VEC3
         ;;(indirect-specular (indirect-specular-light v norm spec voxel-light pos color metallic))
         (indirect (indirect-diffuse-light pos norm voxel-light color rough))
         (shadow (shadow-factor shadowmap light-clip-pos))
         ;; (indirect-ao (x indirect))
         ;;#+nil
         ;;(indirect 0f0)
         (indirect-specular 0f0)
         (indirect-ao 1f0)
         ;;(shadow 1f0)
         ;;
         (final-color
          (+
           ;;#+nil
           (* ;;indirect-ao
            (+ indirect
               indirect-specular))
           (* color emissive)
           ;;#+nil
           (* (max shadow .01)
              ;;#+nil
              (+
               ;;(vec3 indirect-ao)
               ;;(* .01 color)
               ;;#+nil
               (pbr-spotlight-lum light-pos
                                  pos
                                  (normalize (- cam-pos pos))
                                  norm
                                  rough
                                  f0
                                  metallic
                                  color
                                  spec
                                  (* *cone-mult* light-color)
                                  light-dir
                                  *cone-inner*
                                  *cone-outer*
                                  .027 .0028)
               #+nil
               (pbr-direct-lum light-pos
                               pos
                               (normalize (- cam-pos pos))
                               norm
                               rough
                               f0
                               metallic
                               color
                               spec
                               (* 15 light-color))
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
                              (* 10 (v! .3 .8 .5))))))))
    (values (v! final-color 1)
            #+nil
            (v! (if (and (> emissive .2)
                         (> (nineveh:rgb->luma-bt601 final-color) 0))
                    (* emissive final-color)
                    (vec3 0))
                1))
    ;;(v3! shadow)
    ))

(defpipeline-g postprocess-defer-pipe (:points)
  :fragment (postprocess-defered :vec2))



