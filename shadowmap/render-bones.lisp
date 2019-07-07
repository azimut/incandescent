;;--------------------------------------------------
;; 3d BONES - g-pnt tb-data bones

(defun-g vert-bones ((vert g-pnt) (tb tb-data) (bones assimp-bones)
                     &uniform
                     (model-world :mat4) (world-view :mat4) (view-clip :mat4)
                     (offsets (:mat4 36))
                     (scale :float))
  ;;
  (let* ((pos       (* scale (pos vert)))
         (uv        (treat-uvs (tex vert)))
         (norm      (norm vert))
         (norm      (* (m4:to-mat3 model-world) norm))
         (world-pos (* (+ (* (aref (assimp-bones-weights bones) 0)
                             (aref offsets (int (aref (assimp-bones-ids bones) 0))))
                          (* (aref (assimp-bones-weights bones) 1)
                             (aref offsets (int (aref (assimp-bones-ids bones) 1))))
                          (* (aref (assimp-bones-weights bones) 2)
                             (aref offsets (int (aref (assimp-bones-ids bones) 2))))
                          (* (aref (assimp-bones-weights bones) 3)
                             (aref offsets (int (aref (assimp-bones-ids bones) 3)))))
                       (v! pos 1)))
         (world-pos (* model-world world-pos))
         ;;(world-pos (* model-world (v! pos 1)))
         (view-pos  (* world-view  world-pos))
         (clip-pos  (* view-clip   view-pos)))
    (values clip-pos uv norm (s~ world-pos :xyz))))

(defpipeline-g simplest-3d-pipe-bones ()
  :vertex (vert-bones g-pnt tb-data assimp-bones)
  :fragment (simplest-3d-frag :vec2 :vec3 :vec3))


(defun-g shadow-vert-with-bones ((vert g-pnt)
                                 (tb tb-data)
                                 (bones assimp-bones)
                                 &uniform
                                 (offsets (:mat4 36))
                                 (model-world :mat4)
                                 (world-view :mat4)
                                 (view-clip :mat4)
                                 (scale :float)
                                 ;; Parallax vars
                                 (light-pos :vec3)
                                 (cam-pos :vec3)
                                 (light-world :mat4)
                                 (light-clip :mat4))
  (let* ((pos       (* scale (pos vert)))
         (norm      (norm vert))
         (uv        (treat-uvs (tex vert)))
         (norm      (* (m4:to-mat3 model-world) norm))
         (world-pos (* (+ (* (aref (assimp-bones-weights bones) 0)
                             (aref offsets (int (aref (assimp-bones-ids bones) 0))))
                          (* (aref (assimp-bones-weights bones) 1)
                             (aref offsets (int (aref (assimp-bones-ids bones) 1))))
                          (* (aref (assimp-bones-weights bones) 2)
                             (aref offsets (int (aref (assimp-bones-ids bones) 2))))
                          (* (aref (assimp-bones-weights bones) 3)
                             (aref offsets (int (aref (assimp-bones-ids bones) 3)))))
                       (v! pos 1)))
         (world-pos (* model-world world-pos))
         ;;(world-pos (* model-world (v! pos 1)))
         (view-pos  (* world-view  world-pos))
         (clip-pos  (* view-clip   view-pos))
         (t0 (normalize
              (s~ (* model-world (v! (tb-data-tangent tb) 0))
                  :xyz)))
         (n0 (normalize
              (s~ (* model-world (v! norm 0))
                  :xyz)))
         (t0 (normalize (- t0 (* (dot t0 n0) n0))))
         (b0 (cross n0 t0))
         (tbn (mat3 t0 b0 n0)))
    (values clip-pos
            (treat-uvs uv)
            norm
            (s~ world-pos :xyz)
            tbn
            (* tbn light-pos)
            (* tbn cam-pos)
            (* tbn (s~ world-pos :xyz))
            (* light-clip light-world world-pos))))


(defpipeline-g shadow-pbr-pipe-bones ()
  :vertex   (shadow-vert-with-bones g-pnt tb-data assimp-bones)
  :fragment (shadow-pbr-frag :vec2 :vec3 :vec3
                                   :mat3 :vec3 :vec3 :vec3
                                   :vec4))
;;--------------------------------------------------


