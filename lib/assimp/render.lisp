(in-package #:incandescent)

;;--------------------------------------------------
;; Renders
;;--------------------------------------------------

;; 3D - g-pnt with tangent info in tb-data AND textures
(defun-g vert-with-tbdata ((vert g-pnt)
                           (tb tb-data)
                           (bones assimp-bones)
                           &uniform
                           (model-world :mat4)
                           (world-view  :mat4)
                           (view-clip   :mat4)
                           (scale       :float)
                           (offsets    (:mat4 32))
                           ;; Parallax vars
                           (light-pos   :vec3)
                           (cam-pos     :vec3))
  (let* ((pos       (* scale       (pos vert)))
         (world-pos (* model-world (v! pos 1)))
         (view-pos  (* world-view  world-pos))
         (clip-pos  (* view-clip   view-pos))
         ;;
         (uv        (tex vert))
         ;;
         (norm      (norm vert))
         (norm      (* (m4:to-mat3 model-world) norm));;?? FIXME: again
         ;;(world-pos (* model-world world-pos))
         (t0  (normalize
               (s~ (* model-world (v! (tb-data-tangent tb) 0))
                   :xyz)))
         (n0  (normalize
               (s~ (* model-world (v! (norm vert) 0))
                   :xyz)))
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

;; https://github.com/cbaggers/cepl/issues/288
(defun-g vert-with-tbdata-bones ((vert g-pnt)
                                 (tb tb-data)
                                 (bones assimp-bones)
                                 ;;(vert g-pnt) (tb tb-data) (bones assimp-bones)
                                 &uniform
                                 (model-world :mat4)
                                 (world-view :mat4)
                                 (view-clip :mat4)
                                 (scale :float)
                                 ;;
                                 (offsets (:mat4 41)) ;; FIXME
                                 ;; Parallax vars
                                 (light-pos :vec3)
                                 (cam-pos :vec3))
  (let* ((pos       (pos vert))
         (norm      (norm vert))
         (uv        (treat-uvs (tex vert)))
         (norm      (* (m4:to-mat3 model-world) norm))
         ;;#+nil
         (world-pos (* (m4:scale (v3! scale)) ;; FIXME
                       model-world
                       ;; (* (aref (assimp-bones-weights bones) 0)
                       ;;    (aref offsets (int (aref (assimp-bones-ids bones) 0))))
                       (+ (* (aref (assimp-bones-weights bones) 0)
                             (aref offsets (int (aref (assimp-bones-ids bones) 0))))
                          (* (aref (assimp-bones-weights bones) 1)
                             (aref offsets (int (aref (assimp-bones-ids bones) 1))))
                          (* (aref (assimp-bones-weights bones) 2)
                             (aref offsets (int (aref (assimp-bones-ids bones) 2))))
                          (* (aref (assimp-bones-weights bones) 3)
                             (aref offsets (int (aref (assimp-bones-ids bones) 3))))
                          )
                       (v! pos 1)))
         ;;(world-pos (* model-world world-pos))
         ;;(world-pos (* model-world (v! (* scale pos) 1)))
         (view-pos  (* world-view world-pos))
         (clip-pos  (* view-clip  view-pos))
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
            (* tbn (s~ world-pos :xyz)))))


;; no parallax
(defun-g frag-tex-tbn ((uv :vec2)
                       (frag-norm :vec3)
                       (frag-pos :vec3)
                       (tbn :mat3)
                       (tan-light-pos :vec3)
                       (tan-cam-pos :vec3)
                       (tan-frag-pos :vec3)
                       &uniform
                       ;;
                       (light-pos   :vec3)
                       (light-color :vec3)
                       ;;
                       (cam-pos :vec3)
                       (time :float)
                       (albedo :sampler-2d)
                       (normals :sampler-2d)
                       (specular :sampler-2d))
  (let* ((color (expt (s~ (texture albedo uv) :xyz)
                      (vec3 2.2)))
         ;;(normal (norm-from-map normals uv frag-pos frag-norm))
         (normal (norm-from-map normals uv))
         (normal (normalize (* tbn normal)))
         (frag-pos  tan-frag-pos)
         (light-pos tan-light-pos)
         (cam-pos   tan-cam-pos)
         (final-color  (dir-light-apply color
                                        light-color
                                        light-pos
                                        frag-pos
                                        frag-norm
                                        cam-pos .8 .2)))
    (v! final-color 1)
    ;;color
    ;;normal
    ;;frag-norm
    ;;(v! 1 0 0 1)
    ))

;; parallax
;; (defun-g frag-tex-tbn ((uv :vec2)
;;                        (frag-norm :vec3)
;;                        (frag-pos :vec3)
;;                        (tbn :mat3)
;;                        (tan-light-pos :vec3)
;;                        (tan-cam-pos :vec3)
;;                        (tan-frag-pos :vec3)
;;                        &uniform
;;                        (cam-pos :vec3)
;;                        (albedo :sampler-2d)
;;                        (normap :sampler-2d)
;;                        (height-map :sampler-2d))
;;   (let* ((light-pos *pointlight-pos*)
;;          ;; Parallax
;;          (tan-cam-dir (- tan-cam-pos tan-frag-pos))
;;          (newuv (parallax-mapping uv tan-cam-dir height-map .1))
;;          ;; ---------
;;          (light-color (v! 1 1 1))
;;          (light-strength 1f0)
;;          ;;--------------------
;;          (vec-to-light (- light-pos frag-pos))
;;          (dir-to-light (normalize vec-to-light))
;;          ;;--------------------
;;          (color (expt (s~ (texture albedo newuv) :xyz)
;;                       (vec3 2.2)))
;;          (normal (norm-from-map normap newuv))
;;          (normal (normalize (* tbn normal))))
;;     (values
;;      (v! color 1)
;;      ;; (v! 1 1 1 1)
;;      ;;frag-pos
;;      ;;(normalize frag-norm)
;;      )))

(defpipeline-g assimp-tex-pipe ()
  :vertex (vert-with-tbdata g-pnt tb-data assimp-bones)
  :fragment (frag-tex-tbn :vec2 :vec3 :vec3 :mat3
                          ;; Parallax
                          :vec3 :vec3 :vec3))

(defpipeline-g assimp-tex-pipe-simple ()
  :vertex (vert-with-tbdata g-pnt tb-data)
  :fragment (frag-tex-tbn :vec2 :vec3 :vec3 :mat3
                          ;; Parallax
                          :vec3 :vec3 :vec3))

(defpipeline-g assimp-tex-pipe-bones ()
  :vertex (vert-with-tbdata-bones g-pnt tb-data assimp-bones)
  :fragment (frag-tex-tbn :vec2 :vec3 :vec3 :mat3
                          ;; Parallax
                          :vec3 :vec3 :vec3))

;;--------------------------------------------------
