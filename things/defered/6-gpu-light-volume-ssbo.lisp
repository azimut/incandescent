(in-package #:incandescent)

;; Using compute shader to move the light...still not sure how they work
;; SSBO would allow it to access light pos from other render (like shadow volumes)

;; (clear-fbo *fbo*)
;; ;;#+nil
;; (with-fbo-bound (*dsfbo* :attachment-for-size :d)
;;   ;; 1 - Draw depth
;;   (gl:clear :depth-buffer-bit :stencil-buffer-bit)
;;   (loop :for actor :in *actors*
;;         :do (draw-depth actor *currentcamera* time)))
;; ;;#+nil
;; (with-fbo-bound (*dsfbo* :attachment-for-size :d)
;;   ;; 2 - Draw stencil shadows
;;   (gl:clear :stencil-buffer-bit)
;;   (gl:enable :stencil-test)
;;   (gl:stencil-func :always 0 #xff)
;;   (gl:stencil-op-separate :back  :keep :incr-wrap :keep)
;;   (gl:stencil-op-separate :front :keep :decr-wrap :keep)
;;   (with-setf* ((cull-face)  nil
;;                (depth-mask) nil
;;                ;;(depth-clamp) t
;;                ;;(depth-test-function) #'always
;;                )
;;     (loop :for actor :in *actors*
;;           :do (draw-column-silloute actor *currentcamera*)))
;;   (gl:disable :stencil-test))
;; ;;#+nil
;; (with-fbo-bound (*fbo*)
;;   ;; 3 - Draw defer scene with shadow stencil
;;   (gl:enable :stencil-test)
;;   (gl:stencil-op-separate :back :keep :keep :keep)
;;   (gl:stencil-op-separate :front :keep :keep :keep)
;;   (gl:stencil-func :equal 0 #xff)
;;   #+nil
;;   (when-let ((scene (aref *scenes* *scene-index*)))
;;     (dolist (actor scene)
;;       (update actor delta)
;;       (draw actor *currentcamera* time)))
;;   ;;#+nil
;;   (with-setf* (;;(depth-mask) nil
;;                ;;(depth-clamp) t
;;                (depth-test-function) #'=
;;                )
;;     (loop :for actor :in *actors*
;;           :do (update actor delta)
;;               (draw actor *currentcamera* time)))
;;   (gl:disable :stencil-test))
;; (with-blending *blend-defer*
;;   ;; 4 - Add lighting
;;   (loop :for light :in *light-volumes*
;;         :do (draw light *currentcamera* time)
;;             (update light delta)
;;         ))

(defvar *light-volumes* (list))
(defvar *blend-defer* (make-blending-params :source-rgb :one
                                            :destination-rgb :one))

;; Stencil light volumes from:
;; http://ogldev.atspace.co.uk/www/tutorial37/tutorial37.html

(defstruct-g (light-data :layout std-430)
  (pos    (:vec3  30))
  (color  (:vec3  30))
  (radius (:float 30)))

(defclass instance-light-volume (actor)
  ((light-ssbo :initarg :light-ssbo)
   (buf        :initarg :buf)
   (instances  :initarg :instances))
  (:default-initargs
   :buf (sphere)
   :instances 10))

(defmethod free ((obj instance-light-volume))
  (with-slots (light-ssbo) obj
    (free light-ssbo)))

(defun-g sphere-radius ((linear :float) (quadratic :float))
  (let ((constant 1f0)
        (light-max (* .6 5)))
    (/ (+ (- linear)
          (sqrt (- (* linear linear)
                   (* 4 quadratic (- constant (* (/ 256f0 5f0) light-max))))))
       (* 2 quadratic))))

(defun cpu-sphere-radius (linear quadratic)
  (let ((constant 1f0)
        (light-max (* 5 .6)))
    (/ (+ (- linear)
          (sqrt (- (* linear linear)
                   (* 4 quadratic (- constant (* (/ 256f0 5f0) light-max))))))
       (* 2 quadratic))))

(defun init-ssbo ()
  "make a new ssbo with random light data"
  (let ((positions (loop :repeat 30
                         :collect (v! (serapeum:random-in-range -40f0 40f0)
                                      0f0
                                      (serapeum:random-in-range -40f0 40f0))))
        (colors (loop :repeat 30
                      :collect (v! 1 0 0)))
        (radius (loop :repeat 30
                      :collect (cpu-sphere-radius .22 .20))))
    (make-ssbo (list positions colors radius) 'light-data)))

(defun make-instance-light-volume (&key (instances 10))
  ;; HACK!
  (when *light-volumes*
    (mapcar #'free *light-volumes*)
    (setf *light-volumes* nil))
  (let ((obj (make-instance 'instance-light-volume
                            :light-ssbo (init-ssbo)
                            :instances instances)))
    (push obj *light-volumes*)
    obj))

;;--------------------------------------------------
;; Update

(defun-g compute-light (&uniform (time :float)
                                 (light-ssbo light-data :ssbo))
  (declare (local-size :x 1 :y 1 :z 1))
  (let* ((current-pos (aref (light-data-pos light-ssbo)
                            (int (x gl-work-group-id))))
         (id          (* .1 time (x gl-work-group-id)))
         (r           (rand (v! id id)))
         (new-pos     (v! (+ (x current-pos) .5)
                          (y current-pos)
                          (- (z current-pos) .5)))
         (reset-pos   (v! (+ -9 (* 18 r))
                          (* 2 r)
                          (+ -25 (* 50 r)))))
    (if (< (z current-pos) 0f0)
        (setf (aref (light-data-pos light-ssbo) (int (x gl-work-group-id)))
              reset-pos)
        (setf (aref (light-data-pos light-ssbo) (int (x gl-work-group-id)))
              new-pos)))
  (values))

(defpipeline-g run-compute-light ()
  :compute compute-light)

(defmethod update ((actor instance-light-volume) dt)
  #+nil
  (with-slots (light-ssbo) actor
    (map-g #'run-compute-light (make-compute-space 30)
           :light-ssbo light-ssbo
           :time (* .1f0 (get-internal-real-time))))
  #+nil
  (let ((fence (make-gpu-fence)))
    (wait-on-gpu-fence fence)
    (free fence)))

;;--------------------------------------------------
;; Render

(defun-g light-volume-vert ((vert g-pnt)
                            &uniform
                            (ldata light-data :ssbo)
                            (model-clip :mat4))
  (let* ((lpos       (aref (light-data-pos    ldata) gl-instance-id))
         (lradius    (aref (light-data-radius ldata) gl-instance-id))
         (lcolor     (aref (light-data-color  ldata) gl-instance-id))
         (pos        (+ lpos (* lradius 10 (pos vert))))
         ;;(pos (* 2000 (pos vert)))
         (clip-pos   (* model-clip (v! pos 1))))
    (values clip-pos
            (v! 10 1 0)
            ;;lpos
            lcolor
            )))

(defun-g light-volume-frag ((light-pos    :vec3)
                            (light-color  :vec3)
                            &uniform
                            (albedo-sam   :sampler-2d)
                            (position-sam :sampler-2d)
                            (normal-sam   :sampler-2d)
                            (res          :vec2)
                            (cam-pos      :vec3)
                            )
  (let* ((uv       (/ (s~ gl-frag-coord :xy) res))
         (albedo   (s~ (texture albedo-sam   uv) :xyz))
         (position (s~ (texture position-sam uv) :xyz))
         (normal   (normalize
                    (s~ (texture normal-sam  uv) :xyz)))
         (final-color (point-light-apply albedo
                                         ;;(* 7 light-color)
                                         ;;#+nil
                                         (* (* 7 (x light-color))
                                            (v! .8
                                                .2
                                                .3))
                                         light-pos
                                         position
                                         normal
                                         1f0
                                         .22
                                         .20
                                         cam-pos
                                         .9
                                         .1)))
    (v! (nineveh.tonemapping:tone-map-reinhard final-color .6f0)
        (nineveh.color:rgb->luma-bt601 final-color))))

(defpipeline-g light-volume-pipe ()
  :vertex   (light-volume-vert g-pnt)
  :fragment (light-volume-frag :vec3 :vec3))

;; Stencil only pipeline
(defun-g simple-3d-frag ((light-pos :vec3) (light-color :vec3))
  (values))
(defpipeline-g simple-3d-pipe ()
  :vertex (light-volume-vert g-pnt)
  :fragment (simple-3d-frag :vec3 :vec3))

(defmethod draw ((actor instance-light-volume) camera time)
  "we render only on the stencil first to know which parts light or not,
   then we do the actual lighting using defer textures"
  (gl:enable :stencil-test)
  (with-fbo-bound (*dsfbo* :attachment-for-size :d)
    (clear-fbo *dsfbo* :s :d)
    (with-setf (cull-face) nil
      (gl:stencil-func :always 0 0)
      (gl:stencil-op-separate :back :keep :incr-wrap :keep)
      (gl:stencil-op-separate :front :keep :decr-wrap :keep)
      (with-slots (buf light-ssbo instances) actor
        (with-instances instances
          (map-g #'simple-3d-pipe buf
                 :ldata light-ssbo
                 :model-clip (m4:* (projection  camera)
                                   (world->view camera)))))))
  (with-fbo-bound (*sdfbo*)
    (clear-fbo *sdfbo* 0)
    (with-blending *blend-defer*
      (with-setf* ((cull-face) :front
                   (depth-test-function) nil)
        (gl:stencil-func :notequal 0 #xff)
        (with-slots (buf light-ssbo instances) actor
          (with-instances instances
            (map-g #'light-volume-pipe buf
                   :ldata light-ssbo
                   :res (viewport-resolution (current-viewport))
                   :cam-pos (pos camera)
                   :albedo-sam   *sam*
                   :position-sam *sam1*
                   :normal-sam   *sam2*
                   :model-clip   (world->clip camera)))))))
  (gl:disable :stencil-test))
