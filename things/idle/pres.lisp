(in-package :incandescent)

(defclass pres (actor)
  ((albedo   :initarg :albedo)
   (scene    :initarg :scene)))

(let ((rot (q:from-axis-angle (v! 0 0 1) (radians 180)))
      (scale .0005f0)
      (pos (v! -10 .5 -10))
      (file "/home/sendai/Downloads/_presi/fakepresident.obj")
      (*processing-flags* '(:ai-process-triangulate
                            :ai-process-preset-target-realtime-quality
                            ;;:ai-process-flip-u-vs
                            :ai-process-calc-tangent-space)))
  (free-actors)
  ;;(free-assimp-buffers)
  (setf (pos *camera*) (v! -7.2484674 8.393046 -11.347451))
  (setf (rot *camera*) (q! -0.86901885 0.35851827 -0.27647626 0.19957888))
  ;; (setf (pos *camera*) (v! -13.175245 10.687613 -11.655856))
  ;; (setf (rot *camera*) (q! 0.8009455 -0.48497188 -0.35009125 0.026922453))
  (make-rain *asam* :n-particles 1000 :source (v! -13.175245 10.687613 -11.655856))
  (make-room-piso :uv-repeat (v! 50 50))
  (make-grave :pos (v3:+ pos (v! 0 -.5 -9))
              :rot (q:from-axis-angle (v! 0 1 0) (radians 90)))
  #+nil
  (make-lotus :pos (v3:+ pos (v! -1.4 -.05 -8))
              :scale .1)
  ;;#+nil
  (make-lotus :pos (v3:+ pos (v! 1.8 -.05 -8))
              :scale .1)
  (make-clouds)
  (make-tree :pos (v! 0 0 -30))
  (make-tree :pos (v! -10 0 -35) :scale .07)
  (make-tree :pos (v! 10 0 -35) :scale .05)
  (destructuring-bind (&key buf albedo scene &allow-other-keys)
      (nth 0 (assimp-load-meshes file))
    (let ((obj (make-instance 'pres :buf buf :albedo albedo :scene scene
                                    :scale scale
                                    :pos pos :rot rot)))
      (push obj *actors*)))
  (destructuring-bind (&key buf albedo scene &allow-other-keys)
      (nth 1 (assimp-load-meshes file))
    (let ((obj (make-instance 'pres :buf buf :albedo albedo :scene scene
                                    :scale scale
                                    :pos pos :rot rot)))
      (push obj *actors*)))
  ;;(make-text "Press F to Pay Respects" (v! -120 -80))
  )

(defmethod draw ((actor pres) camera (time single-float))
  (with-slots (buf albedo normals scale) actor
    (map-g #'pres-pipe buf
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
           :albedo albedo)))

(defun-g pres-vert ((vert  g-pnt)
                    (tb   tb-data)
                    &uniform
                    (model-world :mat4)
                    (world-view  :mat4)
                    (view-clip   :mat4)
                    (scale       :float)
                    ;;
                    (sam         :sampler-2d)
                    (uv-repeat   :vec2)
                    ;; Parallax vars
                    (light-pos   :vec3)
                    (cam-pos     :vec3))
  (let* ((pos       (* scale       (pos vert)))
         (world-pos (* model-world (v! pos 1)))
         (view-pos  (* world-view  world-pos))
         (clip-pos  (* view-clip   view-pos))
         ;;
         (uv        (treat-uvs (* uv-repeat (tex vert))))
         ;;
         (normal-m3 (transpose (inverse (m4:to-mat3 model-world))))
         (norm      (norm vert))
         (norm      (* (m4:to-mat3 model-world) norm))
         ;;
         (vertex  (v! (/ (s~ clip-pos :xyz) (w clip-pos))
                      (w clip-pos)))
         (vertex  (v! (/ (floor (* 160 (x vertex))) 160)
                      (/ (floor (* 120 (y vertex))) 120)
                      (z vertex)
                      (w vertex)))
         (vertex  (v! (* (s~ vertex :xyz) (w clip-pos))
                      (w vertex)))
         ;;
         ;; o.color = v.color*UNITY_LIGHTMODEL_AMBIENT;
         (ambient  (v! .01 .01 .01 1))
         (color    (v! .5 .5 .5 0))
         ;;
         (distance (length (* world-view model-world vertex)))
         ;;
         (psx-uv   (* uv
                      (+ distance
                         (/ (* (w vertex) (w ambient) 8) distance 2))))
         ;;
         (fog-start 1000f0)
         (fog-end  1500f0)
         (fog-color   (v! .5 .6 .7))
         (fog-density (/ (- fog-end distance)
                         (- fog-end fog-start)))
         (fog-color   (v! fog-color (clamp fog-density 0 1)))
         ;;
         (psx-normal (v! (+ distance
                            (/ (* (w vertex) (w ambient) 8) distance 2))
                         fog-density
                         1))
         ;;
         (color (v! 1 1 1 1))
         (color (v! (shader-vertex-lights-full
                     ;;vertex
                     view-pos
                     (* world-view model-world)
                     (normalize (* (m4:to-mat3 (m4:* model-world world-view)) psx-normal))
                     (* world-view (v! -12 100 -16 1))
                     (v! .5 .6 .7)
                     .0000001
                     (v! .1 .1 .1))
                    1)))
    (values ;;vertex
     ;;#+nil
     (if (> distance (+ fog-start (* (w fog-color) 255)))
         (v! (s~ vertex :xyz) 0)
         vertex)
     psx-uv ;;(treat-uvs uv)
     psx-normal;;norm
     (s~ world-pos :xyz)
     color
     fog-color)))

;; UnityCG.cginc
(defun-g shader-vertex-lights-full ((clip-pos      :vec4)
                                    (model-view    :mat4)
                                    (normal        :vec3)
                                    (light-vpos    :vec4)
                                    (light-color   :vec3)
                                    (attenuation   :float)
                                    (ambient-color :vec3))
  (let* ((view-pos  clip-pos)
         ;;(view-n    (normalize (* (m4:to-mat3 model-view) normal)))
         (view-n    normal)
         (to-light  (- (s~ light-vpos :xyz)
                       (* (s~ view-pos :xyz) (w light-vpos))))
         (length-sq (dot to-light to-light))
         (length-sq (max length-sq .000001))
         (to-light  (* to-light (inversesqrt length-sq)))
         ;;(atten     (/ 1f0 (+ 1f0 (* length-sq attenuation))))
         (diff      (max 0 (dot view-n to-light))))
    (+ ambient-color
       (* light-color diff
          ;;atten
          ))
    ;;(v3! diff)
    ))

(defun-g pres-frag ((uv :vec2)
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
  (let* ((c (* (v! (expt (s~ (texture albedo (/ uv (x frag-norm))) :xyz)
                         (vec3 2.2))
                   1)
               color
               ))
         (color2 (* c (w color-fog)))
         (color2 (v! (+ (s~ color2 :xyz)
                        (* (s~ color-fog :xyz) (- 1 (w color-fog))))
                     (w color2))))
    ;;c
    color2
    ;;color
    ;;c
    ;;(v! uv 0 0)
    ))

(defpipeline-g pres-pipe ()
  :vertex   (pres-vert g-pnt tb-data)
  :fragment (pres-frag :vec2 :vec3 :vec3
                       ;; Parallax
                       :vec4 :vec4))
