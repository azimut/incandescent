(in-package #:incandescent)

(defclass drifter (physic-box)
  (amotor
   (health :initform 100 :accessor health)
   (properties :initform (v! 0 .7 .9 .2)
               :initarg :prop
               :documentation "emissive, spec, rough, metallic")))

(defmethod free ((obj drifter))
  (%ode:joint-destroy (slot-value obj 'amotor)))

(defmethod initialize-instance :after ((obj drifter) &key)
  (with-slots (amotor body) obj
    (setf (state-drifter         *game-state*) obj)
    ;;
    (setf amotor (%ode:joint-create-a-motor *world* 0))
    (%ode:joint-attach amotor body 0)
    ;;(%ode:joint-set-a-motor-mode amotor %ode:+a-motor-euler+)
    ;;
    (%ode:joint-set-a-motor-num-axes amotor 2)
    ;;
    ;; 1 0 0 - front and back
    ;; 0 0 1 - left  and right
    (%ode:joint-set-a-motor-axis     amotor 0 0 0d0 0d0 1d0)
    (%ode:joint-set-a-motor-angle    amotor 0 0d0)
    (%ode:joint-set-a-motor-param    amotor %ode:+param-f-max+
                                     1000000d0)

    (%ode:joint-set-a-motor-axis     amotor 1 0 0d0 1d0 0d0)
    (%ode:joint-set-a-motor-angle    amotor 1 0d0)
    (%ode:joint-set-a-motor-param    amotor (+ %ode:+param-f-max+
                                               %ode:+param-group+)
                                     1000000d0)

    ;; (%ode:joint-set-a-motor-axis     amotor 2 0 0d0 1d0 0d0)
    ;; (%ode:joint-set-a-motor-angle    amotor 2 0d0)
    ;; (%ode:joint-set-a-motor-param    amotor (+ %ode:+param-f-max+
    ;;                                            (* 2 %ode:+param-group+))
    ;;                                  1000000d0)
    ))

(defun make-drifter (&key (pos   (v! 0 .9 0))
                          (color (v! 1 .3 .9))
                          (dim   (v! 1 1 1))
                          (shadow-p t)
                          (rot   (q:identity))
                          (prop  (v! 0 .7 .7 0))
                          (scale 1f0))
  (let ((obj (make-instance 'drifter
                            :prop prop
                            :shadow-p shadow-p
                            :scale scale :color color
                            :pos pos :rot rot
                            ;; ODE
                            :x (coerce (x dim) 'double-float)
                            :y (coerce (y dim) 'double-float)
                            :z (coerce (z dim) 'double-float)
                            ;; ---
                            :buf (box (x dim)
                                      (y dim)
                                      (z dim)))))
    (push obj *actors*)
    obj))

(defun-g drifter-frag ((uv          :vec2)
                       (frag-normal :vec3)
                       (frag-pos    :vec3)
                       &uniform
                       (color       :vec3)
                       (properties  :vec4))
  (let ((emissive (x properties))
        (spec     (y properties))
        (rough    (z properties))
        (metallic (w properties))
        (ao       1f0)
        (albedo   color))
    (values (v! albedo      rough)
            (v! frag-pos    ao)
            (v! frag-normal spec)
            (v! metallic    emissive))))

(defpipeline-g drifter-pipe ()
  :vertex   (vert g-pnt)
  :fragment (drifter-frag :vec2 :vec3 :vec3))

(defmethod draw ((actor drifter) camera time)
  (with-slots (buf scale color properties) actor
    (map-g #'drifter-pipe buf
           :color color
           :scale scale
           :properties  properties
           :model-world (model->world actor)
           :world-view  (world->view camera)
           :view-clip   (projection  camera))))

(defgeneric control-drifter (obj phase dt))

(defmethod control-drifter ((obj drifter) (phase (eql :welcome)) dt)
  (when (keyboard-button (keyboard) key.space)
    (next-phase)))

(defmethod control-drifter ((obj drifter) (phase (eql :runner)) dt)
  (when (>= (state-score *game-state*) *runner-score*)
    (next-phase))
  (make-text
   (format nil "Score: ~d/~d" (state-score *game-state*) *runner-score*)
   :pos (v! -220 -110)
   :scale 1.5f0)
  (with-slots (pos body) obj
    (let* ((groundedp (if (< (y pos) .45) t nil))
           (force     (if groundedp 40d0 10d0)))
      ;; Jump
      (when (and (keyboard-button (keyboard) key.j)
                 groundedp)
        (%ode:body-enable body)
        (%ode:body-add-force body 0d0 400d0 0d0))
      ;; Forward
      (when (keyboard-button (keyboard) key.u)
        (%ode:body-enable body)
        (%ode:body-add-force body 0d0 0d0 (- force)))
      (when (keyboard-button (keyboard) key.m)
        (%ode:body-enable body)
        (%ode:body-add-force body 0d0 0d0 force))
      ;; Strife
      (when (keyboard-button (keyboard) key.h)
        (%ode:body-enable body)
        (%ode:body-add-force body (- force) 0d0 0d0))
      (when (keyboard-button (keyboard) key.k)
        (%ode:body-enable body)
        (%ode:body-add-force body force 0d0 0d0)))))

(defmethod control-drifter ((obj drifter) (phase (eql :boss)) dt)
  (decf (health obj) (* dt *boss-bleeding*))
  (when (<= (health obj) 0f0)
    (next-phase))
  (make-text
   (format nil "Health: ~d" (health obj))
   :pos (v! 100 -110)
   :scale 1.5f0)
  (with-slots (pos body) obj
    ;; Controls
    (let* ((groundedp (if (< (y pos) .45) t nil))
           (force     (if groundedp 40d0 10d0)))
      ;; Jump
      (when (and (keyboard-button (keyboard) key.j)
                 groundedp)
        (%ode:body-enable body)
        (%ode:body-add-force body 0d0 400d0 0d0))
      ;; Strife
      (when (keyboard-button (keyboard) key.h)
        (%ode:body-enable body)
        (%ode:body-add-force body (- force) 0d0 0d0))
      (when (keyboard-button (keyboard) key.k)
        (%ode:body-enable body)
        (%ode:body-add-force body force 0d0 0d0)))))

(defmethod control-drifter ((obj drifter) (phase (eql :sidescroller)) dt)
  (make-text
   (format nil "Distance: ~f" (- (z (pos obj))
                                 (state-distance *game-state*)))
   :pos (v! -100 100)
   :scale 1.5f0)
  (with-slots (pos body) obj
    ;; Controls
    (let* ((groundedp (if (< (y pos) .45) t nil))
           (force     (if groundedp 40d0 10d0)))
      ;; Jump
      (when (and (keyboard-button (keyboard) key.j)
                 groundedp)
        (%ode:body-enable body)
        (%ode:body-add-force body 0d0 400d0 0d0))
      ;; forward
      (when (keyboard-button (keyboard) key.u)
        (%ode:body-enable body)
        (%ode:body-add-force body 0d0 0d0 (- force)))
      (when (keyboard-button (keyboard) key.m)
        (%ode:body-enable body)
        (%ode:body-add-force body 0d0 0d0 force)))))

(let ((stepper (make-stepper (seconds 2) (seconds 2))))
  (defmethod update ((obj drifter) dt)
    (control-drifter obj (first (state-phase *game-state*)) dt)))