(in-package #:incandescent)

(defclass drifter (physic-box)
  (amotor
   (properties :initform (v! 0 .7 .9 .2)
               :initarg :prop
               :documentation "emissive, spec, rough, metallic"))
  (:default-initargs
   :pos (v! 0 .9 0)))

(defmethod free ((obj drifter))
  (%ode:joint-destroy (slot-value obj 'amotor)))

(defmethod initialize-instance :after ((obj drifter) &key)
  (with-slots (amotor body) obj
    (setf *drifter* obj)
    (setf *drifter-pointer* body)
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

(let ((stepper (make-stepper (seconds 2) (seconds 2))))
  (defmethod update ((obj drifter) dt)
    (with-slots (pos body) obj
      ;; Fase
      (when (and (< (z pos) -60f0) (not *final-fase*))
        (setf *final-fase* t))
      ;; Controls
      (let* ((groundedp (if (< (y pos) .45) t nil))
             (force (if groundedp 40d0 10d0)))
        ;; Jump
        (when (keyboard-button (keyboard) key.j)
          (if groundedp
              (progn (%ode:body-enable body)
                     (%ode:body-add-force body 0d0 400d0 0d0))))
        ;; forward
        (when (keyboard-button (keyboard) key.u)
          (%ode:body-enable body)
          (%ode:body-add-force body 0d0 0d0 (- force)))
        (when (keyboard-button (keyboard) key.m)
          (%ode:body-enable body)
          (%ode:body-add-force body 0d0 0d0 force))
        ;; sides
        (when (keyboard-button (keyboard) key.h)
          (%ode:body-enable body)
          (%ode:body-add-force body (- force) 0d0 0d0))
        (when (keyboard-button (keyboard) key.k)
          (%ode:body-enable body)
          (%ode:body-add-force body force 0d0 0d0))))))
