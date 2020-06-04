(in-package :incandescent)
;;(nth 0 (assimp-load-meshes "static/guard/boblampclean.md5mesh"))
;;:rot (q:from-axis-angle (v! 1 0 0) (radians -90))
(defclass monster (assimp-thing-with-bones) ())
(defun make-guard (&key (scale 1f0)
                        (pos (v! 0 0 0)))
  (let ((obj
          (destructuring-bind (&key scene buf
                                    albedo normals specular
                                    bones duration)
              (nth 0 (assimp-load-meshes "static/monster/forestmonster.b3d"))
            (make-instance 'monster
                           :scene scene
                           :scale scale
                           :pos pos
                           :buf buf
                           :rot (q:* (q:from-axis-angle (v! 1 0 0) (radians 90))
                                     (q:from-axis-angle (v! 0 0 1) (radians 45)))
                           :albedo albedo
                           :normals normals
                           :specular specular
                           :bones bones
                           :duration duration))))
    (push obj *actors*)
    obj))

#+nil
(progn (free-actors)
       (setf *light-pos*
             (v! 133.22603 54.215755 -97.88583))
       ;;(free-assimp-buffers)
       (make-sun :pos (v3:*s *light-pos* 3f0)
                 :radius 20f0 :draw-p nil)
       (make-env-map *cube-tex* *cube-sam*)
       ;; (make-watcher :pos (v3:*s (v! 50.86815 .05822 -121.03417) 10f0)
       ;;               :rot (q:* (q:from-axis-angle (v! 1 0 0) (radians -90))
       ;;                         (q:from-axis-angle (v! 0 0 1) (radians 340)))
       ;;               :scale .1)
       ;; (make-watcher2 :pos (v3:*s (v! 50.86815 .05822 -121.03417) 10f0)
       ;;                :rot (q:* (q:from-axis-angle (v! 1 0 0) (radians -90))
       ;;                          (q:from-axis-angle (v! 0 0 1) (radians -30)))
       ;;                :scale .1)
       (let ((pos (v! 70 20 -100))
             (rot (q:* (q:from-axis-angle (v! 0 1 0)
                                          (radians 170))
                       (q:from-axis-angle (v! 1 0 0)
                                          (radians 220)))))
         (make-ironball :pos pos
                        :rot rot
                        :scale .1
                        :nth 0)
         (make-ironball :pos pos
                        :rot rot
                        :scale .1
                        :nth 2)
         (make-ironball :pos pos
                        :rot rot
                        :scale .1
                        :nth 1))
       ;;(make-piso)
       (make-grass-floor :pos (v! 0 0  0) :uv-repeat (v! 90 90))
       (make-guard :scale .1 :pos (v! 0 0 0))
       ;; (make-box :pos (v! 100 25 (+ -20 -130)) :y 100 :x 50 :z 20)
       ;; (make-box :pos (v! 100 25 (+ -20 -140)) :y 50 :x 50 :z 20)
       ;; (make-box :pos (v! 100 25 (+ -20 -120)) :y 50 :x 40 :z 20)
       ;; (make-box :pos (v! 100 15 (+ -20 -100)) :y 40 :x 20 :z 20)
       ;;(setf *light-pos* (v! 120.20602 44.20576 -94.375824))
       ;; (setf (pos *camera*) (v! -1.475637 1.3302835 -1.7559428))
       ;; (setf (rot *camera*) (q! -0.55039555 -0.020804057 0.8331432 0.050043978))
       (setf (pos *camera*)
             (v! -0.9140594 1.1734997 -0.6212534))
       (setf (rot *camera*)
             (q! -0.8888162 0.024173126 0.45428988 -0.05515665))

       )

(defpipeline-g variance-monster-pipe ()
  :vertex   (monster-vert g-pnt tb-data assimp-bones)
  :fragment (variance-3d-frag :vec2 :vec3 :vec3))

(defmethod draw-variance-actor ((actor monster))
  "I need this one to animate the shadows on the shadow pipe, when
   everything is being render from the light POV"
  (with-slots (bones buf scale) actor
    (map-g #'variance-monster-pipe buf
           :scale scale
           :offsets bones
           :model-world (model->world actor)
           :world-view  (world->view *shadow-camera*)
           :view-clip   (projection  *shadow-camera*))))
(defvar *time* 0f0)
(in-package :shiny)
(destructuring-bind (p1 p2 p3)
    (fx-pat "<(_V)__(_vV)><v v><  o >")
  (defun f (time)
    (bbplay (next p1) :amp .1 :rate 1.2)
    (bbplay (next p2) :amp .4 :rate 1.2)
    (bbplay (next p3) :amp .4 :rate 1.2)
    (setf incandescent::*time* 0f0)
    (aat (+ time #[1 b]) #'f it)))
(aat (tempo-sync #[4 b]) #'f it)
(defun f ())
(in-package :incandescent)

;; 24.8 - right front leg touches the floor
;; 26.5 - Left front leg touches the floor
(let ((time 24.8f0)
      (init 24.8f0)
      (stepper (make-stepper (seconds .01) (seconds .01)))
      (left-sound t)
      (right-sound t)
      (chord (shiny::make-chord 60 80 4 (shiny::scale 0 'shiny::minor))))
  (defmethod update ((actor monster) dt)
    (when (funcall stepper)
      (with-slots (bones scene duration) actor
        (push-g (get-bones-tranforms scene :time time)
                bones)
        (when (and left-sound (> time 28.4))
          (setf left-sound nil)
          (shiny::clc 23 (list (nth 0 chord) (nth 1 chord)) 50 .5))
        (when (and right-sound (> time 26.5))
          (setf right-sound nil)
          (if (> (random 1f0) .2)
              (shiny::clc 23 (list (nth 2 chord)) 50 .5)
              (shiny::clc 23 (+ -12 (nth 3 chord)) 40 1)))
        (if (> time duration)
            (setf time init
                  left-sound t
                  chord (shiny::make-chord 60 80 4 (shiny::scale 0 'shiny::minor))
                  right-sound t)
            (incf time *time*))))))

(defmethod draw ((actor monster) camera (time single-float))
  (with-slots (buf albedo scale bones) actor
    (map-g #'monster-pipe buf
           :scale scale
           :offsets bones
           ;;
           :model-world (model->world actor)
           :world-view (world->view camera)
           :view-clip (projection camera)
           ;;
           :brdf-lut       *s-brdf*
           :prefilter-map  *s-cubemap-prefilter*
           :irradiance-map *s-cubemap-live*
           ;;
           :light-pos *light-pos*
           :light-color *light-color*
           ;; PBR
           :albedo albedo)))

(defun-g monster-vert ((vert g-pnt)
                       (tb tb-data)
                       (bones assimp-bones)
                       &uniform
                       (model-world :mat4)
                       (world-view  :mat4)
                       (view-clip   :mat4)
                       (scale       :float)
                       (offsets    (:mat4 41)))
  (let* ((pos       (pos vert))
         (norm      (norm vert))
         (uv        (tex vert))
         (norm      (* (m4:to-mat3 model-world) norm))
         (world-pos
           (* (m4:scale (v3! scale)) ;; FIXME
              model-world
              (+ (* (aref (assimp-bones-weights bones) 0)
                    (aref offsets (int (aref (assimp-bones-ids bones) 0))))
                 (* (aref (assimp-bones-weights bones) 1)
                    (aref offsets (int (aref (assimp-bones-ids bones) 1))))
                 (* (aref (assimp-bones-weights bones) 2)
                    (aref offsets (int (aref (assimp-bones-ids bones) 2))))
                 (* (aref (assimp-bones-weights bones) 3)
                    (aref offsets (int (aref (assimp-bones-ids bones) 3)))))
              (v! pos 1)))
         (view-pos  (* world-view world-pos))
         (clip-pos  (* view-clip  view-pos)))
    (values clip-pos
            uv
            norm
            (s~ world-pos :xyz))))

(defun-g monster-frag ((uv :vec2)
                       (frag-norm :vec3)
                       (frag-pos :vec3)
                       &uniform
                       ;;
                       (light-pos   :vec3)
                       (light-color :vec3)
                       ;;
                       (brdf-lut       :sampler-2d)
                       (prefilter-map  :sampler-cube)
                       (irradiance-map :sampler-cube)
                       ;;
                       (cam-pos :vec3)
                       (time :float)
                       (albedo :sampler-2d)
                       (normals :sampler-2d)
                       (specular :sampler-2d))
  (let* ((color (expt (s~ (texture albedo uv) :xyz)
                      (vec3 2.2)))
         (normal (normalize frag-norm))
         (metallic .05)
         (roughness .9)
         (ao 1f0)
         (n normal)
         (v (normalize (- cam-pos frag-pos)))
         (f0 (vec3 .04))
         (f0 (mix f0 color metallic))
         (ambient (ambient-ibl v n f0
                               ;;brdf-lut
                               prefilter-map
                               irradiance-map
                               roughness
                               metallic
                               color
                               ao))
         (final-color (+ ambient
                         (dir-light-apply color
                                          light-color
                                          light-pos
                                          frag-pos
                                          frag-norm
                                          cam-pos
                                          roughness
                                          .1))))
    (v! final-color 1)
    ;;ambient
    ;;color
    ;;frag-norm
    ;;(v! 1 0 0 1)
    ))

(defpipeline-g monster-pipe ()
  :vertex (monster-vert g-pnt tb-data assimp-bones)
  :fragment (monster-frag :vec2 :vec3 :vec3))
