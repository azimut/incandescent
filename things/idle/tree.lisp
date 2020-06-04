(in-package :incandescent)

(defclass tree (actor)
  ((albedo :initarg :albedo)
   (scene  :initarg :scene)))

(defclass tree-leaf (actor)
  ((albedo :initarg :albedo)
   (scene  :initarg :scene)))

(defun make-tree (&key (pos (v! 0 0 0))
                       (rot (q:identity))
                       (scale .1))
  (let ((meshes (let ((*processing-flags*
                        '(:ai-process-triangulate
                          :ai-process-flip-u-vs
                          :ai-process-preset-target-realtime-quality
                          :ai-process-calc-tangent-space)))
                  (assimp-load-meshes
                   (resolve-path "static/tree/tree_obj.obj")))))
    #+nil
    (destructuring-bind (&key buf scene albedo &allow-other-keys) (nth 1 meshes)
      (push (make-instance 'tree-leaf :pos pos :rot rot
                                      :scale scale
                                      :buf buf :scene scene :albedo albedo)
            *actors*))
    ;;#+nil
    (destructuring-bind (&key buf scene albedo &allow-other-keys) (nth 2 meshes)
      (push (make-instance 'tree-leaf :pos pos :rot rot
                                      :scale scale
                                      :buf buf :scene scene :albedo albedo)
            *actors*))
    (destructuring-bind (&key buf scene albedo &allow-other-keys) (nth 0 meshes)
      (push (make-instance 'tree :pos pos :rot rot
                                 :scale scale
                                 :buf buf :scene scene :albedo albedo)
            *actors*))))

(defmethod draw ((actor tree-leaf) camera (time single-float))
  (with-setf (depth-mask) nil)
  (with-blending *blend-billboards*
    (with-slots (buf albedo normals scale specular) actor
      (map-g #'tree-leaf-pipe buf
             :scale scale
             :uv-repeat (v! 1 1)
             :time time
             ;; Lighting
             :light-pos *light-pos*
             :light-color *light-color*
             :cam-pos (pos camera)
             ;;
             :model-world (model->world actor)
             :world-view (world->view  camera)
             :view-clip (projection   camera)
             :albedo albedo))))


(defun-g tree-leaf-frag ((uv :vec2)
                         (frag-norm :vec3)
                         (frag-pos :vec3)
                         (color :vec4)
                         (color-fog :vec4)
                         &uniform
                         ;;
                         (light-pos   :vec3)
                         (light-color :vec3)
                         ;;
                         (cam-pos :vec3)
                         (time :float)
                         (albedo :sampler-2d))
  (let* ((color4 (texture albedo (/ uv (x frag-norm))))
         (c (* (v! (expt (s~ color4 :xyz)
                         (vec3 2.2))
                   1)
               ;;color
               ))
         (color2 (* c (w color-fog)))
         (color2 (v! (+ (s~ color2 :xyz)
                        (* (s~ color-fog :xyz) (- 1 (w color-fog))))
                     (w color4))))
    color2
    ;;color4
    ;;c
    ;;(v! uv 0 0)
    ))

(defpipeline-g tree-leaf-pipe ()
  :vertex   (pres-vert g-pnt tb-data)
  :fragment (tree-leaf-frag :vec2 :vec3 :vec3
                            ;; Parallax
                            :vec4 :vec4))
