(in-package #:incandescent)

(defclass piso (actor)
  ((diff  :initform (get-tex "static/_marble/marble_01_diff_2k.png"   nil t :rgb8))
   (nor   :initform (get-tex "static/_marble/marble_01_nor_2k.png"    nil t :rgb8))
   (rough :initform (get-tex "static/_marble/marble_01_rough2_2k.png" nil t :r8))
   (ao    :initform (get-tex "static/_marble/marble_01_AO_2k.png"     nil t :r8))
   (spec  :initform (get-tex "static/_marble/marble_01_spec_2k.png"   nil t :r8))
   (disp  :initform (get-tex "static/_marble/marble_01_disp2_2k.png"  nil t :r8))
   (uvs   :initform (v! 4 4))))

(defclass pared (piso)
  ((diff  :initform (get-tex "static/2k_wall/8/white_plaster_02_diff_2k.png"  nil t :rgb8))
   (nor   :initform (get-tex "static/2k_wall/8/white_plaster_02_nor_2k.png"   nil t :rgb8))
   (rough :initform (get-tex "static/2k_wall/8/white_plaster_02_rough_2k.png" nil t :r8))
   (ao    :initform (get-tex "static/2k_wall/8/white_plaster_02_ao_2k.png"    nil t :r8))
   (spec  :initform (get-tex "static/2k_wall/8/white_plaster_02_spec_2k.png"  nil t :r8))
   (disp  :initform (get-tex "static/2k_wall/8/white_plaster_02_disp_2k.png"  nil t :r8))
   (uvs   :initform (v! 1 1))))

(defun make-piso (&key (pos (v! 0 1 0))
                    (rot (q:identity)))
  (let ((obj (make-instance 'piso :pos pos :rot rot
                                  :buf (box 20 1 20 t)
                            ;;:buf (lattice 20 20 4 4 t)
                            )))
    (push obj *actors*)
    obj))

(defun make-pared (&key (pos (v! 0 1 0)) (rot (q:identity)) (color (v! 1 1 1)))
  (let ((obj (make-instance 'pared :pos pos :rot rot :color color
                                   :buf (box 20 1 20 t)
                                   ;;:buf (lattice 20 20 4 4 t)
                                   )))
    (push obj *actors*)
    obj))

(defmethod update ((actor piso) dt))

(defmethod draw ((actor piso) camera (time single-float))
  (with-slots (buf scale color diff rough nor ao spec disp uvs) actor
    (map-g #'piso-pipe buf
           :scale scale
           :ao-map ao
           :spec-map spec
           :diff-map diff
           :norm-map nor
           :disp-map disp
           :rough-map rough
           :uv-repeat uvs
           :color color
           :metallic .99
           ;;
           :model-world (model->world actor)
           :world-view  (world->view camera)
           :view-clip   (projection  camera))))


(defmethod draw ((actor pared) camera (time single-float))
  (with-slots (buf scale color diff rough nor ao spec disp uvs) actor
    (map-g #'piso-pipe buf
           :scale scale
           :metallic .1
           :ao-map ao
           :spec-map spec
           :diff-map diff
           :norm-map nor
           :disp-map disp
           :rough-map rough
           :uv-repeat uvs
           :color color
           ;;
           :model-world (model->world actor)
           :world-view  (world->view camera)
           :view-clip   (projection  camera))))

(defun-g piso-frag ((uv             :vec2)
                    (frag-normal    :vec3)
                    (frag-pos       :vec3)
                    (tbn            :mat3)
                    (tan-light-pos  :vec3)
                    (tan-cam-pos    :vec3)
                    (tan-frag-pos   :vec3)
                    &uniform
                    (metallic    :float)
                    (color       :vec3)
                    (disp-map    :sampler-2d)
                    (spec-map    :sampler-2d)
                    (diff-map    :sampler-2d)
                    (rough-map   :sampler-2d)
                    (norm-map    :sampler-2d)
                    (ao-map      :sampler-2d))
  (let* (;;#+nil
         (uv (parallax-mapping-offset-flipped
              uv
              (normalize (- tan-cam-pos tan-frag-pos))
              disp-map
              .01))
         (ao (x (texture ao-map uv)))
         (spec (x (texture spec-map uv)))
         (rough (x (texture rough-map uv)))
         (albedo (pow (s~ (texture diff-map uv) :xyz) (vec3 2.2))))
    (values (v! (* color albedo) rough)
            (v! frag-pos ao)
            (v! (normalize (* tbn (norm-from-map norm-map uv)))
                spec)
            metallic)))

(defpipeline-g piso-pipe ()
  :vertex   (vert-with-tbdata g-pnt tb-data)
  :fragment (piso-frag :vec2 :vec3 :vec3
                             :mat3 :vec3 :vec3 :vec3))

;;--------------------------------------------------

(defun-g rsm-piso-frag ((uv             :vec2)
                        (frag-norm      :vec3)
                        (frag-pos       :vec3)
                        (tbn            :mat3)
                        (tan-light-pos  :vec3)
                        (tan-cam-pos    :vec3)
                        (tan-frag-pos   :vec3)
                        &uniform
                        (albedo :sampler-2d)
                        (normal-map :sampler-2d)
                        (light-color :vec3))
  (let ((color (s~ (texture albedo uv) :xyz)))
    (values (normalize (* tbn (norm-from-map-flipped normal-map uv)))
            (* light-color color))))

(defpipeline-g rsm-piso-pipe ()
  :vertex   (vert-with-tbdata g-pnt tb-data)
  :fragment (rsm-piso-frag :vec2 :vec3 :vec3
                                 :mat3 :vec3 :vec3 :vec3))

(defmethod draw-rsm ((actor piso) camera)
  (with-fbo-bound (*shadow-fbo* :attachment-for-size :d)
    ;;(with-setf (cull-face) :front)
    (with-slots (buf diff nor scale uvs) actor
      (map-g #'rsm-piso-pipe buf
             :uv-repeat uvs
             :albedo diff
             :normal-map nor
             :light-color *light-color*
             :scale scale
             :model-world (model->world actor)
             :world-view  (world->view camera)
             :view-clip   (projection  camera)))))
