(in-package :incandescent)

;;--------------------------------------------------
;; 2D - Passthrough Texture Pipe

(defun-g pass-frag ((uv :vec2) &uniform (sam :sampler-2d))
  (texture sam uv))

(defpipeline-g pass-pipe (:points)
  :fragment (pass-frag :vec2))

;;--------------------------------------------------
;; 3D - g-pnt mesh without tangents

(defun-g vert ((vert g-pnt) &uniform
               (model-world :mat4)
               (world-view  :mat4)
               (view-clip   :mat4)
               (scale       :float))
  (let* ((pos        (* scale (pos vert)))
         (norm       (norm vert))
         (tex        (tex vert))
         (world-norm (* (m4:to-mat3 model-world) norm))
         (world-pos  (* model-world (v! pos 1)))
         (view-pos   (* world-view  world-pos))
         (clip-pos   (* view-clip   view-pos)))
    (values clip-pos
            tex
            world-norm
            (s~ world-pos :xyz))))

;; 3D - g-pnt without tangents + modifiers for texture

(defun-g vert-tex ((vert g-pnt)
                   &uniform
                   (model-world :mat4)
                   (world-view  :mat4)
                   (view-clip   :mat4)
                   (uv-repeat   :vec2)
                   (scale       :float))
  (let* ((pos        (* scale (pos vert)))
         (norm       (norm vert))
         (tex        (tex vert))
         (world-norm (* (m4:to-mat3 model-world) norm))
         (world-pos  (* model-world (v! pos 1)))
         (view-pos   (* world-view  world-pos))
         (clip-pos   (* view-clip   view-pos)))
    (values clip-pos
            (* uv-repeat (treat-uvs tex))
            world-norm
            (s~ world-pos :xyz))))

(defun-g frag ((uv :vec2)
               (frag-norm :vec3)
               (frag-pos :vec3)
               &uniform
               (time :float)
               (color :vec3)
               (cam-pos :vec3)
               ;; Directional light (for the most part)
               (light-color    :vec3)
               (light-pos      :vec3)
               (brdf-luf       :sampler-2d)
               (irradiance-map :sampler-cube)
               (prefilter-map  :sampler-cube))
  (let* ((roughness .7)
         (metallic .01)
         (ambient (v! .03 .03 .03))
         (f0 (v3! .04))
         (f0 (mix f0 color metallic))
         (final-color color)
         (final-color (dir-light-apply final-color
                                       light-color
                                       light-pos
                                       frag-pos
                                       frag-norm
                                       cam-pos 32 1)))
    (v! (+ ambient final-color) 1)))

(defpipeline-g generic-pipe ()
  :vertex (vert g-pnt)
  :fragment (frag :vec2 :vec3 :vec3))

;;--------------------------------------------------
;; 3D - g-pnt mesh without tangents WITH texture

(defun-g frag-tex ((uv :vec2)
                   (frag-norm :vec3)
                   (frag-pos :vec3)
                   &uniform
                   (light-pos :vec3)
                   (cam-pos :vec3)
                   (albedo :sampler-2d))
  (let* ((color4 (texture albedo uv))
         (color3 (expt (s~ color4 :xyz)
                       (vec3 2.2))))
    (v! color3 (w color4))))

(defpipeline-g tex-pipe ()
  :vertex (vert g-pnt)
  :fragment (frag-tex :vec2 :vec3 :vec3))

;;--------------------------------------------------
;; PBR
;;--------------------------------------------------

(defun-g vert-with-tbdata ((vert  g-pnt)
                           (tb   tb-data)
                           &uniform
                           (model-world :mat4)
                           (world-view  :mat4)
                           (view-clip   :mat4)
                           (scale       :float)
                           ;;
                           (uv-repeat   :vec2)
                           ;; Parallax vars
                           (light-pos   :vec3)
                           (cam-pos     :vec3))
  (let* ((pos       (* scale       (pos vert)))
         (world-pos (* model-world (v! pos 1)))
         (view-pos  (* world-view  world-pos))
         (clip-pos  (* view-clip   view-pos))
         ;;
         (uv        (* uv-repeat (tex vert)))
         ;;
         (normal-m3 (transpose (inverse (m4:to-mat3 model-world))))
         (norm      (norm vert))
         (norm      (* (m4:to-mat3 model-world)
                       norm))
         ;;
         (t0  (normalize
               (* normal-m3 (tb-data-tangent tb))))
         (n0  (normalize
               (* normal-m3 (norm vert))))
         (t0  (normalize (- t0 (* (dot t0 n0) n0))))
         (b0  (cross n0 t0))
         (tbn (mat3 t0 b0 n0)))
    (values clip-pos
            (treat-uvs uv)
            norm
            (s~ world-pos :xyz)
            tbn
            (* tbn light-pos)
            (* tbn cam-pos)
            (* tbn (s~ world-pos :xyz)))))
