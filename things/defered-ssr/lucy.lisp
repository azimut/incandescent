(in-package #:incandescent)

(progn (defun init-scene ()
         (let ((c1 (v! .3 .2 .2))
               (poff (v! -.3 .5 0))
               (scale .3f0)
               (dwall (v! 2 2 .2))
               (cwall (v! .9 .9 .9)))
           (free-actors)
           (free-assimp-buffers)
           ;;(make-obstacle :scale 2f0 :pos (v! 0 0 0) :shadow-p nil)
           (make-lucy :pos (v3:- (v! 0 0 0) poff) :scale scale)
           (make-sun :radius 2f0)
           ;;(make-obstacle :pos (v! 0 0 0) :scale 2f0)
           ;; BOX
           (make-obstacle :pos (v! 0 0 -.95); back
                          :color cwall
                          :dim dwall)
           ;; (make-obstacle :pos (v! .4 .95 -.5) ; ceil far
           ;;                :color cwall
           ;;                :dim (v3:* dwall (v! 1 .55 1))
           ;;                :rot (q:from-axis-angle (v! 1 0 0) (radians 90)))
           #+nil
           (make-obstacle :pos (v! .4 0 .7) ; ceil near
                          :color (v! .1 .1 .1)
                          :dim (v! 12 4 .1)
                          :rot (q:from-axis-angle (v! 1 0 0) (radians 90))
                          :scale .2f0)
           ;; (make-obstacle :pos (v! .95 -.4 0) ;rwall
           ;;                :color cwall
           ;;                :dim (v! 12 4 .4)
           ;;                :rot (q:from-axis-angle (v! 0 -1 0) (radians 90))
           ;;                :scale .2f0)
           (make-obstacle :pos (v! -.95 0 0);lwall
                          :color cwall
                          :dim dwall
                          :rot (q:from-axis-angle (v! 0 -1 0) (radians 90)))
           ;; (make-obstacle :pos (v! 0 -.4 .95);nearwall
           ;;                :color cwall
           ;;                :dim (v! 12 4 .5)
           ;;                :scale .2f0)
           ;; RED AND BLUE
           (make-obstacle :pos (v3:- (v! .5 .11 .1) poff)
                          :scale scale
                          :rot (q:from-axis-angle (v! 0 1 0) (radians 90))
                          :dim (v! .3 2 .3)
                          :prop (v! 0 .7 .7 0)
                          :color (v! .1 .1 1))
           (make-obstacle :pos (v3:- (v! -.2 .1 0) poff)
                          :color (v! 1 .1 .1)
                          :prop (v! 0 .7 .7 0)
                          :scale scale)
           ;;#+nil
           (let ((m .9))
             (dotimes (i 8)
               (let ((ii (* .4 (+ i 2))))
                 (make-obstacle :pos (v3:- (v! 0 (- (* .05 ii)) 0) poff)
                                :prop (v! 0 .4 .8 m)
                                :scale scale
                                :color (v! .9 .9 .9)
                                :dim (v! (expt 2 ii) .1 (/ (expt 2 ii) 2)))
                 (setf m (max 0 (- m .3))))))))
       ;;#+nil
       ;;(init-scene)
       )

(defclass lucy (actor)
  ((properties :initform (v! 0 .3 .8 .1)
               :documentation "emissive, spec, rough, metallic")))

(defmethod update ((actor lucy) dt)
  ;;#+nil
  (with-slots (properties rot scale color pos) actor
    ;;(setf scale 10f0)
    ;;(setf color (v! .8 .8 .8))
    ;;(setf properties (v! 1 .6 .9 .09))
    ;;(setf pos (v! -.3 .1 .1))
    ))

(defun make-lucy (&key (pos   (v! 0 5 0))
                       (color (v! .6 .6 .6))
                       (rot   (q:identity))
                       (scale 1f0))
  (let* (;;#+nil
         (*processing-flags*
           '(:ai-process-triangulate
             ;; :ai-process-find-degenerates
             ;; :ai-process-find-invalid-data
             ;;:ai-process-join-identical-vertices
             ;;:ai-process-flip-u-vs
             ;;:ai-process-optimize-meshes
             :ai-process-split-large-meshes
             ;;:ai-process-calc-tangent-space
             ))
         (obj ))
    (dolist (obj
             ;;(assimp-load-meshes "/home/sendai/projects/Fast-Quadric-Mesh-Simplification-master/bin.Linux/dot1angel.obj")
             ;;(assimp-load-meshes "/home/sendai/quicklisp/local-projects/incandescent/static/Lucy100k.obj")
             ;;(assimp-load-meshes "/home/sendai/projects/ModelsOBJ-master/angel.obj")
             (assimp-load-meshes (truename "static/dot1lucy.obj"))
             )
      (push (make-instance
             'lucy
             :buf (getf obj :buf)
             :scale scale
             :color color
             :pos pos
             :rot rot)
            *actors*))
    t))

(defun-g lucy-frag ((uv          :vec2)
                    (frag-normal :vec3)
                    (frag-pos    :vec3)
                    &uniform
                    (color       :vec3)
                    (properties  :vec4))
  (let ((emissive (x properties))
        (spec     (y properties))
        (rough    (z properties))
        (metallic (w properties))
        (albedo color))
    ;; NT: can't use W for emissive as 1 is the default value for it (default-color)?
    (values (v! albedo      rough)
            (v! frag-pos    emissive)
            (v! frag-normal spec)
            (v! metallic    emissive))))

(defpipeline-g lucy-pipe ()
  :vertex   (vert g-pnt)
  :fragment (lucy-frag :vec2 :vec3 :vec3))

(defmethod draw ((actor lucy) camera time)
  (with-slots (draw-p buf scale color properties) actor
    (when draw-p
      (map-g #'lucy-pipe buf
             :color color
             :scale scale
             :properties (v! properties)
             :model-world (model->world actor)
             :world-view  (world->view camera)
             :view-clip   (projection  camera)))))
