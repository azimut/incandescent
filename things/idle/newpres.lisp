(in-package :incandescent)

(defclass newpres (actor)
  ((albedo   :initarg :albedo)
   (specular :initarg :specular)
   (roughness :initarg :roughness)
   (scene    :initarg :scene))
  (:default-initargs
   :specular  .1
   :roughness .9))

(let ((rot (q:from-axis-angle (v! 0 0 1) (radians 180)))
      (scale .001f0)
      (pos (v! 10 1 0))
      (file "/home/sendai/Downloads/_presi/fakepresident.obj")
      (*processing-flags* '(:ai-process-triangulate
                            :ai-process-preset-target-realtime-quality
                            ;;:ai-process-flip-u-vs
                            :ai-process-calc-tangent-space)))
  ;; (free-actors)
  ;; (free-assimp-buffers)
  ;; (make-clouds)
  ;; (make-piso)
  (destructuring-bind (&key buf albedo scene &allow-other-keys)
      (nth 0 (assimp-load-meshes file))
    (let ((obj (make-instance 'newpres :buf buf :albedo albedo :scene scene
                                       :scale scale
                                       :pos pos :rot rot)))
      (push obj *actors*)))
  (destructuring-bind (&key buf albedo scene &allow-other-keys)
      (nth 1 (assimp-load-meshes file))
    (let ((obj (make-instance 'newpres :buf buf :albedo albedo :scene scene
                                       :scale scale
                                       :pos pos :rot rot)))
      (push obj *actors*))))

(defmethod draw ((actor newpres) camera (time single-float))
  (with-slots (buf albedo normals scale specular) actor
    (map-g #'newpres-pipe buf
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
           :albedo albedo
           :specular specular)))

(defpipeline-g newpres-pipe ()
  :vertex   (vert-with-tbdata g-pnt tb-data)
  :fragment (newpres-frag :vec2 :vec3 :vec3
                                :mat3 :vec3 :vec3 :vec3))

(defun-g newpres-frag ((uv :vec2)
                       (frag-norm :vec3)
                       (frag-pos :vec3)
                       (tbn           :mat3)
                       (tan-light-pos :vec3)
                       (tan-cam-pos   :vec3)
                       (tan-frag-pos  :vec3)
                       &uniform
                       ;;
                       (light-pos   :vec3)
                       (light-color :vec3)
                       ;;
                       (cam-pos :vec3)
                       (time :float)
                       (specular :float)
                       (roughness :float)
                       (albedo :sampler-2d))
  (let* ((color (expt (s~ (texture albedo uv) :xyz)
                      (vec3 2.2)))
         (final-color  (dir-light-apply color
                                        light-color
                                        light-pos
                                        frag-pos
                                        frag-norm
                                        cam-pos
                                        roughness
                                        specular)))
    ;;(v! final-color 1)
    color
    ;;(v! uv 0 0)
    ))


(defmethod update ((actor pres) dt)
  (with-slots (rot) actor
    (setf rot (q:* (q:from-axis-angle (v! 0 0 1) (radians 180))
                   (q:from-axis-angle (v! 0 1 0) (radians (mod (* 10 (mynow)) 360)))))))

(defmethod update ((actor newpres) dt)
  (with-slots (rot) actor
    (setf rot (q:* (q:from-axis-angle (v! 0 0 1) (radians 180))
                   (q:from-axis-angle (v! 0 1 0) (radians (mod (* 10 (mynow)) 360)))))))
(defmethod update ((actor otherpres) dt)
  (with-slots (rot) actor
    (setf rot (q:* (q:from-axis-angle (v! 0 0 1) (radians 180))
                   (q:from-axis-angle (v! 0 1 0) (radians (mod (* 10 (mynow)) 360)))))))
