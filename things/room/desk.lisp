(in-package :incandescent)

(setf *light-pos*   (v! 0 100 0))
(setf *light-color* (v3:*s (v! 1 .5 .55) 2f0))
;;(setf *light-color* (v3:*s (v! 1 1 1) 10f0))

(progn (setf *actors* nil)
       (make-desk)
       (make-pinkf)
       (make-sky)
       (make-chair (v! 10 -1 0))
       (make-chair (v! -10 -1 0))
       ;;(reset-camera *camera* (v! 0 4 20))
       )

(defclass desk (assimp-thing) ())

(defpipeline-g desk-pipe ()
  :vertex   (vert-with-tbdata g-pnt tb-data assimp-bones)
  :fragment (desk-frag :vec2 :vec3 :vec3 :mat3
                       :vec3 :vec3 :vec3))

(defun-g desk-frag ((uv :vec2)
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
                    (albedo   :sampler-2d)
                    (normals  :sampler-2d)
                    (specular :sampler-2d))
  (let* ((color (expt (s~ (texture albedo uv) :xyz)
                      (vec3 2.2)))
         (normal (norm-from-map normals uv))
         (normal (normalize (* tbn normal)))
         (color  (dir-light-apply color
                                  light-color
                                  tan-light-pos
                                  tan-frag-pos
                                  normal
                                  tan-cam-pos
                                  .1
                                  (x (texture specular uv))))
         )
    (v! color 1)))

(defmethod draw ((actor desk) camera (time single-float))
  (with-slots (buf albedo normals scale specular) actor
    (map-g #'desk-pipe buf
           :scale scale
           ;; Lighting
           :light-pos   *light-pos*
           :light-color *light-color*
           ;;
           :model-world (model->world actor)
           :world-view  (world->view camera)
           :view-clip   (projection camera)
           ;; PBR
           :cam-pos (pos camera)
           :albedo albedo
           :time time
           :specular specular
           :normals normals)))

(defun make-desk ()
  (mapcar
   (lambda (obj)
     (with-slots (buf albedo normals specular scene) obj
       (push (make-instance 'desk
                            :scene scene
                            :buf buf
                            :scale .1
                            :albedo albedo
                            :normals normals
                            :specular specular)
             *actors*)))
   (let ((*default-specular* "/home/sendai/Downloads/desk/classic_hotel_desk_s.png")
         (*processing-flags*
           '(:ai-process-triangulate
             :ai-process-flip-u-vs
             :ai-process-preset-target-realtime-max-quality
             ;;:ai-process-preset-target-realtime-quality
             :ai-process-calc-tangent-space)))
     (assimp-load-meshes "/home/sendai/Downloads/desk/Hotel Desk.obj"))))


(defun make-chair (&key (pos (v! 0 0 0))
                        (rot (q:identity)))
  (mapcar
   (lambda (obj)
     (with-slots (buf albedo normals specular scene) obj
       (push (make-instance 'desk
                            :scene scene
                            :buf buf
                            :scale .1
                            :pos pos :rot rot
                            :albedo albedo
                            :normals normals
                            :specular specular)
             *actors*)))
   (let ((*default-specular* "/home/sendai/Downloads/chair/classic_hotel_chair_s.png")
         (*processing-flags*
           '(:ai-process-triangulate
             :ai-process-flip-u-vs
             :ai-process-preset-target-realtime-max-quality
             ;;:ai-process-preset-target-realtime-quality
             :ai-process-calc-tangent-space)))
     (assimp-load-meshes "/home/sendai/Downloads/chair/Hotel Chair.obj"))))
