(in-package #:incandescent)

;; Point Particles, GPU controlled by Transform Feedback
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
;; (gl:point-size .2)

(defclass base-particles ()
  ((n-particles :initarg :n-particles)
   (gar-src     :initarg :gar-src)
   (gar-dst     :initarg :gar-dst)
   (str-src     :initarg :str-src)
   (str-dst     :initarg :str-dst)
   (tfs-src     :initarg :tfs-src)
   (tfs-dst     :initarg :tfs-dst))
  (:default-initargs
   :n-particles 100)
  (:documentation "template class for particles, nothing implemented on this"))

(defclass particles (base-particles)
  ((source :initform (v! 0 0 0)
           :initarg :source))
  (:documentation "effective class for point based particles"))

(defstruct-g pdata
  (pos  :vec3)
  (dir  :vec3)
  (life :float))

(defmethod free ((obj particles))
  (with-slots (gar-src gar-dst str-src str-dst tfs-src tfs-dst) obj
    (free str-src)
    (free str-dst)
    (free gar-src)
    (free gar-dst)
    (setf tfs-src nil)
    (setf tfs-dst nil)))

(defun-g pinit-vert ()
  (values (v! 0 0 0 0)
          (:feedback (v! 0 0 0))
          (:feedback (v! 0 0 0))
          (:feedback 0f0)))

(defpipeline-g pinit-pipe (:points)
  :vertex (pinit-vert))

(defun init-particles (actor)
  (with-slots (tfs-src tfs-dst) actor
    (with-transform-feedback (tfs-src)
      (map-g #'pinit-pipe *bs*))
    (with-transform-feedback (tfs-dst)
      (map-g #'pinit-pipe *bs*)))
  (values))

;;--------------------------------------------------
;; Init particles

(defmethod initialize-instance :after ((obj particles) &key)
  (with-slots (n-particles gar-src gar-dst str-src str-dst tfs-src tfs-dst) obj
    (setf gar-src (make-gpu-array nil :dimensions n-particles :element-type 'pdata)
          gar-dst (make-gpu-array nil :dimensions n-particles :element-type 'pdata)
          str-src (make-buffer-stream gar-src :primitive :points)
          str-dst (make-buffer-stream gar-dst :primitive :points)
          tfs-src (make-transform-feedback-stream gar-src)
          tfs-dst (make-transform-feedback-stream gar-dst))
    (init-particles obj)))

(defun make-particles (&key (n-particles 100)
                            (source (v! 0 0 0)))
  (declare (type alexandria:positive-fixnum n-particles)
           (type rtg-math.types:vec3 source))
  (let ((obj (make-instance 'particles :n-particles n-particles
                                       :source source)))
    (push obj *actors*)
    obj))

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
(defun-g pupdate-vert ((pdata  pdata)
                       &uniform
                       (source :vec3)
                       (time   :float))
  (with-slots (pos dir life) pdata
    (let* ((time (* time .1 (* 2f0 gl-vertex-id)))
           (life life)
           (new-life (+ life .01))
           (dir dir)
           (pos pos)
           (r (rand (vec2 time))))
      (if (>= new-life 1f0)
          (progn ;; Reset
            (setf dir  (v! (* 360 r) ;; rot
                           .4;;(* .001 (+ 7 (* 5 r))) ;; scale
                           0))
            (setf life (* .4 r))
            (setf pos
                  ;;#+nil
                  (v! -1
                      0 ;;(- (+ .1 r) .8)
                      0)
                  ;;(v! 0 0 0)
                  ))
          (progn ;; Update
            (setf life new-life)
            ;;(setf (z pos) (* .3 (sin time)))
            ;;(setf (x pos) (* .3 (cos time)))
            ))
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
           (c    (pos *camera*)))
      (setf source (v3:+ (v3:*s zdir 5f0)
                         (v! (x c) 2 (z c)))))
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

(defun-g prender-points-vert ((pdata pdata) &uniform (world-clip :mat4))
  (with-slots (pos) pdata
    (let* ((world-pos (v! pos 1))
           (clip-pos  (* world-clip world-pos)))
      clip-pos)))

(defun-g prender-points-frag ()
  (v! 1 1 1 1))

(defpipeline-g prender-points-pipe (:points)
  :vertex   (prender-points-vert pdata)
  :fragment (prender-points-frag))

(defmethod swap-particles (actor)
  (with-slots (tfs-src tfs-dst str-src str-dst gar-src gar-dst) actor
    (rotatef tfs-src tfs-dst)
    (rotatef str-src str-dst)
    (rotatef gar-src gar-dst)))

(defmethod draw-variance-actor ((actor particles)))
(defmethod draw ((actor particles) camera time)
  (with-slots (str-src) actor
    (map-g #'prender-points-pipe str-src
           :world-clip (world->clip camera)))
  (swap-particles actor))
