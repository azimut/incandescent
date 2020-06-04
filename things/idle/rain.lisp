(in-package #:incandescent)

(defclass rain (billboards) ())

(defun make-rain (sam &key (n-particles 100)
                           (source (v! 0 0 0)))
  (declare (type cepl:sampler sam)
           (type alexandria:positive-fixnum n-particles)
           (type rtg-math.types:vec3 source))
  (let ((obj (make-instance 'rain :n-particles n-particles
                                  :source source
                                  :sam sam)))
    (push obj *actors*)
    obj))

;;--------------------------------------------------
;; Update

(defun-g rain-update-vert ((pdata  pdata) &uniform
                           (source :vec3)
                           (time   :float))
  (with-slots (pos dir life) pdata
    (let* ((time (* time .2 gl-vertex-id))
           (life life)
           (dir  dir)
           (pos  pos)
           (r    (rand (vec2 time))))
      (if (or (>= life 1f0)
              (< (y pos) -2f0))
          (progn ;; Reset
            (setf dir  (v! (* 360 r) ;; rot
                           (* 8 r) ;; scale
                           0))
            (setf life (* .4 r))
            (setf pos  (+ source
                          (v! (+ -20 (* 40 (rand (vec2 (* 3 time)))))
                              (+ 20 (* 2 r))
                              (+ -20 (* 40 r))))))
          (progn ;; Update
            (incf life .005)
            (incf (x pos) .3)
            (incf (y pos) -1.4f0)
            (incf (z pos) .1)))
      (values (v! 0 0 0 0)
              (:feedback pos)
              (:feedback dir)
              (:feedback life)))))

(defpipeline-g rain-update-pipe (:points)
  :vertex (rain-update-vert pdata))

(defmethod update ((actor rain) dt)
  (with-slots (tfs-dst str-src source) actor
    (with-transform-feedback (tfs-dst)
      (map-g #'rain-update-pipe str-src
             :source (v! 0 10 0)
             :time dt))))

;;--------------------------------------------------
;; Render

(defun-g rain-draw-vert ((pdata pdata)
                         &uniform
                         (world-view :mat4))
  #+nil
  (m4:* (m4:translation pos)
        (q:to-mat4      rot))
  (with-slots (pos life dir) pdata
    (values ;;(* world-view (v! pos 1))
     (v! pos 1)
     life
     dir)))

;; https://github.com/zompi2/Bilboards/blob/master/Data/shaders/billboard_gs.glsl
;; No artifact on this one...but not sure how to stop the Y rotation
(defun-g rain-draw-geom ((life (:float 1))
                         (rot  (:vec3 1))
                         &uniform
                         (camera-pos :vec3)
                         (arotation :mat4)
                         (world-view :mat4)
                         (view-clip :mat4))
  (declare (output-primitive :kind :triangle-strip :max-vertices 4))
  (let ((life (aref life 0))
        (p    (s~ (gl-position (aref gl-in 0)) :xyz)))
    (when (< life 1f0)
      (let* ((center  (* view-clip world-view (v! p 1)))
             (dir     (* (v! .5f0 .15f0) .5))
             ;;
             ;;(look  (normalize (- camera-pos p)))
             ;;(look  (v! (x look) 0 (z look)))
             ;;(up    (v! 0 1 0))
             (pp (v! 0 0 0 0))
             ;;(pp    (v! 0 0 0))
             ;;(right (cross up look))
             ;;(scale (y (aref rot 0)))
             ;;(half-s (* .5 scale))
             )
        (setf pp (v! (+ (x center) (x dir)) (+ (y center) (y dir))
                     (z center) (w center)))
        (emit () (* arotation pp) (v! 1 0) life)
        (setf pp (v! (- (x center) (x dir)) (+ (y center) (y dir))
                     (z center) (w center)))
        (emit () (* arotation pp) (v! 0 0) life)
        (setf pp (v! (+ (x center) (x dir)) (- (y center) (y dir))
                     (z center) (w center)))
        (emit () (* arotation pp) (v! 1 1) life)
        (setf pp (v! (- (x center) (x dir)) (- (y center) (y dir))
                     (z center) (w center)))
        (emit () (* arotation pp) (v! 0 1) life)
        ;;
        (end-primitive)
        (values)))))

;; Source: random Unity shader on a forum
#+nil
(defun-g rain-draw-geom ((life (:float 1))
                         (rot  (:vec3 1))
                         &uniform
                         (camera-pos :vec3)
                         (view-clip :mat4))
  (declare (output-primitive :kind :triangle-strip :max-vertices 4))
  (let ((life (aref life 0))
        (p    (s~ (gl-position (aref gl-in 0)) :xyz)))
    (when (< life 1f0)
      (let* ((look  (normalize (- camera-pos p)))
             (look  (v! (x look) 0 (z look)))
             (up    (v! 0 1 0))
             (pp    (v! 0 0 0))
             (right (cross up look))
             (scale (y (aref rot 0)))
             (half-s (* .5 scale)))
        (setf pp (- (+ p (* half-s right)) (* half-s up)))
        (emit () (* view-clip (v! pp 1)) (v! 0 0) life)
        (setf pp (+ (+ p (* half-s right)) (* half-s up)))
        (emit () (* view-clip (v! pp 1)) (v! 0 1) life)
        (setf pp (- (- p (* half-s right)) (* half-s up)))
        (emit () (* view-clip (v! pp 1)) (v! 1 0) life)
        (setf pp (+ (- p (* half-s right)) (* half-s up)))
        (emit () (* view-clip (v! pp 1)) (v! 1 1) life)
        ;;
        (end-primitive)
        (values)))))
#+nil
(defun-g rain-draw-geom ((life (:float 1))
                         (rot  (:vec3 1))
                         &uniform
                         (camera-pos :vec3)
                         (view-clip :mat4))
  (declare (output-primitive :kind :triangle-strip :max-vertices 4))
  (let ((life (aref life 0))
        (p    (s~ (gl-position (aref gl-in 0)) :xyz)))
    (when (< life 1f0)
      (let* ((to-camera (normalize (- camera-pos p)))
             ;;(to-camera (v! (x to-camera) 0 (z to-camera)))
             (up        (v! 0 1 0))
             (pp        (v! 0 0 0))
             (right     (cross to-camera up))
             (scale     (y (aref rot 0))))
        (setf pp (- p (* (+ right up) scale)))
        (emit () (* view-clip (v! pp 1)) (v! 0 0) life)
        (setf pp (- p (* (- right up) scale)))
        (emit () (* view-clip (v! pp 1)) (v! 0 1) life)
        (setf pp (+ p (* (- right up) scale)))
        (emit () (* view-clip (v! pp 1)) (v! 1 0) life)
        (setf pp (+ p (* (+ right up) scale)))
        (emit () (* view-clip (v! pp 1)) (v! 1 1) life)
        ;;
        (end-primitive)
        (values)))))

#+nil
(defun-g rain-draw-geom ((life (:float 1))
                         (rot  (:vec3 1))
                         &uniform
                         (camera-pos :vec3)
                         (view-clip :mat4))
  (declare (output-primitive :kind :triangle-strip :max-vertices 4))
  (let ((life (aref life 0))
        (p    (s~ (gl-position (aref gl-in 0)) :xyz)))
    (when (< life 1f0)
      (let* ((to-camera (normalize (- camera-pos p)))
             ;;(to-camera (v! (x to-camera) 0 (z to-camera)))
             (up        (v! 0 1 0))
             (right     (cross to-camera up))
             (scale     3f0;;(y (aref rot 0))
                        ))
        (decf p (/ right (* scale 2)))
        (emit () (* view-clip (v! p 1)) (v! 0 0) life)
        (incf (y p) scale)
        (emit () (* view-clip (v! p 1)) (v! 0 1) life)
        (decf (y p) scale) (incf p (* scale right))
        (emit () (* view-clip (v! p 1)) (v! 1 0) life)
        (incf (y p) scale)
        (emit () (* view-clip (v! p 1)) (v! 1 1) life)
        ;;
        (end-primitive)
        (values)))))

(defun-g rain-draw-frag ((uv   :vec2)
                         (life :float)
                         &uniform
                         (time :float)
                         (res  :vec2)
                         (sam  :sampler-2d)
                         (samd :sampler-2d))
  (let ((color (texture sam uv)))
    (v! (* ;;(s~ color :xyz)
         ;;(v! .18 .17843138 .1552941)
         ;;(v! 0.6392157 0.54901963 0.34509805)
         (* 1 (v! .5 .6 .7))
         ;;(v! 0 -1 0)
         )
	(* (- 1 life)
           (w color)
           ;;#+nil
           (calculate-fade
            (z gl-frag-coord)
            (x (texel-fetch samd
                            (ivec2 (int (round (x (s~ gl-frag-coord :xy))))
                                   (int (round (y (s~ gl-frag-coord :xy)))))
                            0)))))
    ;;(v!  1 1 1 1)
    ;;(pow uv (v! 3 3))
    ;;color
    ))

(defpipeline-g rain-draw-pipe (:points)
  :vertex   (rain-draw-vert pdata)
  :geometry (rain-draw-geom (:float 1) (:vec3 1))
  :fragment (rain-draw-frag :vec2 :float))

;;--------------------------------------------------

(defmethod draw ((actor rain) camera time)
  "textured particles blended into the scene"
  ;; Use a simple mask when not using soft-particles
  ;; Otherwise, setf (depth-mask) to nil AND make sure is the first element
  (with-setf (depth-mask) nil
    (with-slots (sam str-src) actor
      (with-blending *blend-billboards*
        (map-g #'rain-draw-pipe str-src
               :sam sam
               :samd *samd*
               :time time
               :res (resolution (current-viewport))
               :camera-pos (pos camera)
               :arotation (q:to-mat4 (q:from-axis-angle (v! 0 0 1) (radians 85)))
               :world-view (world->view camera)
               :view-clip  (projection camera)))))
  (swap-particles actor))
