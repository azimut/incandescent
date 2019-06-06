(in-package :incandescent)

(progn (setf *actors* nil)
       (make-pinkf)
       (reset-camera))

(defclass pinkf (actor)
  ((uv-repeat :initform 40 :initarg :uv-repeat)
   (albedo                 :initarg :albedo)))

(defun make-pinkf (&key (pos (v! 0 -4 0)) (uv-repeat 40))
  (let ((obj (make-instance
              'pinkf
              :pos pos
              :buf (lattice 30 30)
              :uv-repeat uv-repeat
              :albedo (get-tex #P"/home/sendai/Downloads/assets/room/Floor.png"))))
    (push obj *actors*)
    obj))

(defun-g pinkf-vert ((vert g-pnt) &uniform
                     (model-world :mat4)
                     (world-view  :mat4)
                     (view-clip   :mat4)
                     (scale       :float)
                     (uv-repeat   :int))
  (let* ((pos        (* scale (pos vert)))
         (norm       (norm vert))
         (tex        (tex vert))
         (world-norm (* (m4:to-mat3 model-world) norm))
         (world-pos  (* model-world (v! pos 1)))
         (view-pos   (* world-view  world-pos))
         (clip-pos   (* view-clip   view-pos)))
    (values clip-pos
            (* uv-repeat tex)
            world-norm
            (s~ world-pos :xyz))))

(defun-g pinkf-frag ((uv :vec2)
                     (frag-norm :vec3)
                     (frag-pos :vec3)
                     &uniform
                     (light-pos   :vec3)
                     (light-color :vec3)
                     (cam-pos :vec3)
                     (albedo :sampler-2d))
  (let* ((color4 (texture albedo uv))
         (color3 (expt (s~ color4 :xyz)
                       (vec3 2.2)))
         (color3 (dir-light-apply color3
                                  light-color
                                  light-pos
                                  frag-pos
                                  frag-norm
                                  cam-pos
                                  .9 .1)))
    (- (v! color3 (w color4))
       (v4! (* (sin (fract (x uv)))
               (sin (fract (y uv))))))
    ;;(v! color3 (w color4))
    ))

(defpipeline-g pinkf-pipe ()
  (pinkf-vert g-pnt)
  (pinkf-frag :vec2 :vec3 :vec3))

(defmethod draw ((actor pinkf) camera time)
  (with-slots (buf albedo scale uv-repeat) actor
    (map-g #'pinkf-pipe buf
           :cam-pos (pos camera)
           :light-pos *light-pos*
           :light-color *light-color*
           :uv-repeat uv-repeat
           :scale scale
           :model-world (model->world actor)
           :world-view (world->view camera)
           :view-clip  (projection camera)
           :albedo albedo)))

;; So much to do, and too little time...




;;--------------------------------------------------

(free-assimp-buffers)
(get-tex )
(reset-camera)
(progn (setf *actors* nil)
       (make-sky)
       (mapcar (lambda (x) (push x *actors*))
               (let ((*processing-flags*
                       '(:ai-process-triangulate
                         :ai-process-flip-u-vs
                         :ai-process-preset-target-realtime-max-quality
                         ;;:ai-process-preset-target-realtime-quality
                         :ai-process-calc-tangent-space)))
                 (assimp-load-meshes
                  "/home/sendai/Downloads/desk/Hotel Desk.obj"))))
