(in-package :incandescent)

;; NOTE: instancing might be an overkill
;; TODO: can probably integrated better with assimp default helpers

(defparameter *grass-blend*
  (make-blending-params  :source-alpha :one-minus-src-alpha))

(defclass grass (actor)
  ((sam :initarg :sam)
   (buf :initarg :buf)
   (spe :initarg :spe)
   (nor :initarg :nor)))

(defun make-grass (&key (pos (v! 0 0 0)) (scale 10f0))
  (let ((obj (make-instance
              'grass
              :scale scale
              :pos pos
              :rot (q:from-axis-angle
                    (v! 0 1 0)
                    (radians 180))
              ;; TODO: yikes
              :buf (caar
                    (let ((*processing-flags* '(:ai-process-triangulate
                                                :ai-process-flip-u-vs
                                                :ai-process-preset-target-realtime-quality
                                                :ai-process-calc-tangent-space)))
                      (assimp-load-meshes "static/grass/grass-share2.obj"
                                          :instantiate-p nil)))
              :sam (get-tex "static/grass/grass_patch_d.png")
              :nor (get-tex "static/grass/grass_patch_n.png")
              :spe (get-tex "static/grass/grass_patch_n.png" nil t :r8))))
    (push obj *actors*)
    obj))

(defun-g grass-frag ((uv :vec2)
                     (frag-norm :vec3)
                     (frag-pos :vec3)
                     &uniform
                     (light-pos :vec3)
                     (light-color :vec3)
                     (cam-pos :vec3)
                     (albedo :sampler-2d)
                     (specular :sampler-2d)
                     (nor :sampler-2d))
  (let* ((normal (norm-from-map nor uv))
         (color4 (texture albedo uv))
         (color3 (expt (s~ color4 :xyz)
                       (vec3 2.2f0)))
         (color3 (dir-light-apply color3
                                  light-color
                                  light-pos
                                  frag-pos
                                  normal
                                  cam-pos
                                  .1f0
                                  (x (texture specular uv)))))
    (v! color3 (w color4))))

(defpipeline-g grass-pipe ()
  :vertex   (vert g-pnt)
  :fragment (grass-frag :vec2 :vec3 :vec3))

;; NOTE: disable depth-mask when rendering anything transparent (?
(defmethod draw ((actor grass) camera time)
  (with-slots (sam nor buf scale spe) actor
    (with-blending *blend*
      (with-setf* ((depth-mask) nil
                   (cull-face) nil)
        (map-g #'grass-pipe buf
               :albedo sam
               :nor nor
               :specular spe
               :scale scale
               :cam-pos (pos camera)
               :light-pos *light-pos*
               :light-color *light-color*
               :model-world (model->world actor)
               :world-view  (world->view camera)
               :view-clip   (projection camera))))))
