(in-package #:incandescent)

;; Point Particles, GPU controlled by Transform Feedback

(defvar *blend* (make-blending-params))
(defvar *particle-systems* NIL
  "cache of particles creates with make- functions")

(defclass particles ()
  ((gar-src :initarg :gar-src)
   (gar-dst :initarg :gar-dst)
   (str-src :initarg :str-src)
   (str-dst :initarg :str-dst)
   (tfs-src :initarg :tfs-src)
   (tfs-dst :initarg :tfs-dst)
   (source  :initarg :source))
  (:default-initargs
   :source (v! 0 0 0))
  (:documentation "class for point based particles"))

(defclass billboards (particles)
  ((sam :initarg :sam :documentation "texture displayed in billboard"))
  (:documentation "class for point based billboards"))

(defstruct-g pdata
  (pos  :vec3)
  (dir  :vec3)
  (life :float))

;;--------------------------------------------------
;; PARTICLE LOGIC
;;--------------------------------------------------
;; Uses TTFS to delegate to the gpu all the movement logic.
;; Needs *bs* for the single stage pipeline.
;; TODO: Instacing for geometry/vertices rendending of particles.

;; (make-particles)
;; (update)
;; (draw)
;; (swap-particles)

;;--------------------------------------------------
;; Init particles

(defun-g pinit-vert ()
  (values (v! 0 0 0 0)
          (:feedback (v! 0 0 0))
          (:feedback (v! 0 0 0))
          (:feedback 0f0)))

(defpipeline-g pinit-pipe (:points)
  :vertex (pinit-vert))

(defun make-particles (n-particles)
  (declare (type alexandria:positive-fixnum n-particles))
  (let* ((gar-src (make-gpu-array nil :dimensions n-particles :element-type 'pdata))
         (gar-dst (make-gpu-array nil :dimensions n-particles :element-type 'pdata))
         (str-src (make-buffer-stream gar-src :primitive :points))
         (str-dst (make-buffer-stream gar-dst :primitive :points))
         (tfs-src (make-transform-feedback-stream gar-src))
         (tfs-dst (make-transform-feedback-stream gar-dst))
         (obj (make-instance 'particles
                             :gar-src gar-src :gar-dst gar-dst
                             :str-src str-src :str-dst str-dst
                             :tfs-src tfs-src :tfs-dst tfs-dst)))
    (reset-particles obj)
    (push obj *particle-systems*)
    (push obj *actors*)
    obj))

(defun make-billboards (sam n-particles)
  (declare (type cepl:sampler sam)
           (type alexandria:positive-fixnum n-particles))
  (let* ((gar-src (make-gpu-array nil :dimensions n-particles :element-type 'pdata))
         (gar-dst (make-gpu-array nil :dimensions n-particles :element-type 'pdata))
         (str-src (make-buffer-stream gar-src :primitive :points))
         (str-dst (make-buffer-stream gar-dst :primitive :points))
         (tfs-src (make-transform-feedback-stream gar-src))
         (tfs-dst (make-transform-feedback-stream gar-dst))
         (obj (make-instance 'billboards
                             :gar-src gar-src :gar-dst gar-dst
                             :str-src str-src :str-dst str-dst
                             :tfs-src tfs-src :tfs-dst tfs-dst
                             :sam sam)))
    (reset-particles obj)
    (push obj *particle-systems*)
    (push obj *actors*)
    obj))

;;--------------------------------------------------
;; Free & Reset

;; TFS?
(defmethod free ((obj particles))
  (with-slots (gar-src gar-dst str-src str-dst tfs-src tfs-dst) obj
    (free str-src)
    (free str-dst)
    (free gar-src)
    (free gar-dst)
    (setf tfs-src nil)
    (setf tfs-dst nil)))

(defmethod reset-particles ((actor particles))
  (with-slots (tfs-src tfs-dst) actor
    (with-transform-feedback (tfs-src)
      (map-g #'pinit-pipe *bs*))
    (with-transform-feedback (tfs-dst)
      (map-g #'pinit-pipe *bs*)))
  (values))

;;--------------------------------------------------
;; Update particles

;; - Fudge initial alpha
;; - Color overtime to fade in/out with the alpha
;; - random init ROTATION
;; - ROTATION over lifetime
;; - random init SPRITE (static per life)
;; - change SPRITE over lifetime
;; - move over lifetime

;; - Scale X
(defun-g pupdate-vert ((pdata pdata)
                       &uniform
                       (source :vec3)
                       (time :float))
  (with-slots (pos dir life) pdata
    (let* ((time (* time .2 (* 2f0 gl-vertex-id)))
           (life life)
           (new-life (+ life .01))
           (dir dir)
           (pos pos)
           (r (rand (vec2 time))))
      (if (>= new-life 1f0)
          (progn ;; Reset
            (setf dir  (v! (* 360 r) ;; rot
                           (+ 7 (* 5 r)) ;; scale
                           0))
            (setf life (* .4 r))
            (setf pos  (+ source
                          (v! (+ -9 (* 18 (rand (vec2 (* 3 time)))))
                              (+ .5 r)
                              (+ -25 (* 50 r))))))
          (progn ;; Update
            (setf life new-life)
            (incf (z pos) -.005)))
      (values (v! 0 0 0 0)
              (:feedback pos)
              (:feedback dir)
              (:feedback life)))))

(defpipeline-g pupdate-pipe (:points)
  :vertex (pupdate-vert pdata))

(defmethod update ((actor particles) dt)
  (with-slots (tfs-dst str-src source) actor
    ;; try placing things in front of camera, hacky
    (let* ((dir  (q:to-direction (rot *camera*)))
           (zdir (v! (x dir) 0 (z dir)))
           (c (pos *camera*)))
      (setf source (v3:+ (v3:*s zdir 5f0)
                         (v! (x c) -5 (z c)))))
    (with-transform-feedback (tfs-dst)
      (map-g #'pupdate-pipe str-src
             :source source
             :time dt))))

;;--------------------------------------------------
;; PARTICLE DRAW/RENDER
;;--------------------------------------------------
;; All I tried so far is either A) drawing the particles as points B)
;; drawing the particles as points and then have a geometry shader
;; convert them to billboards. It is possible draw
;; triangles(geometries) using instancing. But might be the code above
;; needs change too.

(defmethod swap-particles ((actor particles))
  (with-slots (tfs-src tfs-dst str-src str-dst gar-src gar-dst) actor
    (rotatef tfs-src tfs-dst)
    (rotatef str-src str-dst)
    (rotatef gar-src gar-dst)))

;;
;; A) POINTS
;;

(defun-g prender-points-vert ((pdata pdata)
                              &uniform
                              (world-clip :mat4))
  (with-slots (pos) pdata
    (let* ((world-pos (v! pos 1))
           (clip-pos  (* world-clip world-pos)))
      clip-pos)))

(defun-g prender-points-frag ()
  (v! 1 1 1 1))

(defpipeline-g prender-points-pipe (:points)
  :vertex   (prender-points-vert pdata)
  :fragment (prender-points-frag))

;;(gl:point-size 4)
(defmethod draw ((actor particles) camera time)
  (with-slots (str-src) actor
    (map-g #'prender-points-pipe str-src
           :world-clip (world->clip camera)))
  (swap-particles actor))

;;
;; B) POINTS -> BILLBOARDS
;;

(defun-g billboard-vert ((pdata pdata)
                         &uniform
                         (world-view :mat4))
  (with-slots (pos life dir) pdata
    (values (* world-view (v! pos 1))
            life
            dir)))

(defun-g billboard-geom ((life (:float 1))
                         (rot  (:vec3 1))
                         &uniform
                         (camera-pos :vec3)
                         (view-clip :mat4))
  (declare (output-primitive :kind :triangle-strip :max-vertices 4))
  (let ((life (aref life 0))
        (p    (s~ (gl-position (aref gl-in 0)) :xyz)))
    (when (< life 1f0)
      (let* ((to-camera (normalize (- camera-pos p)))
             (up    (v! 0 1 0))
             (right (cross to-camera up))
             (scale (y (aref rot 0))))
        ;; try discard closer to the camera billboads
        (let ((zp (v! (x p) 0 (z p)))
              (zc (v! (x camera-pos) 0 (z camera-pos))))
          (when (< (length (- zc zp)) 3)
            (setf life .999f0)))
        (decf p (/ right (* scale 2)))
        (emit ()
              (* view-clip (v! p 1))
              (v! 0 0)
              life)
        ;;
        (incf (y p) scale)
        (emit ()
              (* view-clip (v! p 1))
              (v! 0 1)
              life)
        ;;
        (decf (y p) scale)
        (incf p (* scale right))
        (emit ()
              (* view-clip (v! p 1))
              (v! 1 0)
              life)
        ;;
        (incf (y p) scale)
        (emit ()
              (* view-clip (v! p 1))
              (v! 1 1)
              life)
        (end-primitive)
        (values)))))

(defun-g billboard-frag ((uv :vec2)
                         (life :float)
                         &uniform
                         (time :float)
                         (res :vec2)
                         (scene :sampler-2d)
                         (sam :sampler-2d)
                         (samd :sampler-2d))
  (let* ((sprites 8)
         (uv (/ uv sprites))
         (color (texture sam uv)))
    (v! (* ;;(s~ color :xyz)
         ;;(v! .18 .17843138 .1552941)
         ;;(v! 0.6392157 0.54901963 0.34509805)
         (* 2 (v! .5 .6 .7))
         ;;(v! 0 -1 0)
         )
	(* (- 1 life)
           (w color)
           (calculate-fade
            (z gl-frag-coord)
            (x (texel-fetch samd
                            (ivec2 (int (round (x (s~ gl-frag-coord :xy))))
                                   (int (round (y (s~ gl-frag-coord :xy)))))
                            0)))))))

;; https://developer.download.nvidia.com/whitepapers/2007/SDK10/SoftParticles_hi.pdf
;; https://discourse.threejs.org/t/soft-particles-render/504/3
(defun-g calculate-fade ((particle-depth :float)
                         (scene-depth :float))
  (let* ((z-fade 1f0)
         (f-distance 10f0)
         (f-contrast 1f0)
         (input-depth (* (- scene-depth particle-depth) f-distance)))
    (if (and (> input-depth 0) (< input-depth 1))
        (progn
          (setf z-fade (* .5 (pow (saturate (* 2f0 (if (> input-depth .5)
                                                       (- 1 input-depth)
                                                       input-depth)))
                                  f-contrast)))
          (setf z-fade (if (> input-depth .5) (- 1 z-fade) z-fade)))
        (setf z-fade (saturate input-depth)))
    z-fade))

(defpipeline-g billboard-pipe (:points)
  :vertex   (billboard-vert pdata)
  :geometry (billboard-geom (:float 1) (:vec3 1))
  :fragment (billboard-frag :vec2 :float))

;; https://forum.processing.org/two/discussion/3955/textured-billboard-and-transparency-problem
(defmethod draw ((actor billboards) camera time)
  "textured particles blended into the scene"
  ;; Use a simple mask when not using soft-particles
  ;; Otherwise, setf (depth-mask) to nil AND make sure is the first element
  (with-setf (depth-mask) nil
    (with-slots (sam str-src) actor
      (with-blending *blend*
        (map-g #'billboard-pipe str-src
               :sam sam
               :samd *samd*
               :res (resolution (current-viewport))
               :world-view (world->view camera)
               :view-clip  (projection camera)))))
  (swap-particles actor))
