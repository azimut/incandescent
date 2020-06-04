(in-package #:incandescent)
;;
;; 2 - creates light volumes with position/scale determined on the CPU
;;

(defvar *light-volumes* (list))
(defvar *blend-defer* (make-blending-params :source-rgb :one
                                            :destination-rgb :one))
;; on a (as-frame) in main.lisp
;; (with-blending *blend-defer*
;;   (loop :for light :in *light-volumes*
;;         :do (draw light *currentcamera* time)))

(defclass light-volume (actor)
  ((buf :initform (sphere))))

(defun make-light-volume (&key (pos (v! 0 0 0))
                               (rot (q:identity)))
  (let ((obj (make-instance 'light-volume :pos pos :rot rot)))
    (push obj *light-volumes*)
    obj))

(defun sphere-radius (linear quadratic)
  (let ((constant 1f0)
        (light-max (* 5 .6)))
    (/ (+ (- linear)
          (sqrt (- (* linear linear)
                   (* 4 quadratic (- constant (* (/ 256f0 5f0) light-max))))))
       (* 2 quadratic))))

(defmethod draw ((actor light-volume) camera time)
  (with-slots (pos buf) actor
    (map-g #'light-volume-pipe (sphere )
           :albedo-sam *sam*
           :position-sam *sam1*
           :normal-sam *sam2*
           :cam-pos (pos *currentcamera*)
           :light-pos pos
           :scale (sphere-radius .22 .20)
           :res (viewport-resolution (current-viewport))
           :model-world (model->world actor)
           :world-view  (world->view *currentcamera*)
           :view-clip   (projection  *currentcamera*))))

(defun draw-light-volume ()
  (let ((light-pos (v! (* 10 (cos (mynow)))
                       0
                       (* 10 (sin (mynow))))))
    (map-g #'light-volume-pipe (sphere )
           :albedo-sam *sam*
           :position-sam *sam1*
           :normal-sam *sam2*
           :cam-pos (pos *currentcamera*)
           :light-pos light-pos
           :scale (sphere-radius .22 .2)
           :res (viewport-resolution (current-viewport))
           :model-world (m4:translation light-pos)
           :world-view  (world->view *currentcamera*)
           :view-clip   (projection  *currentcamera*))))

(defun-g light-volume-frag ((uv           :vec2)
                            (frag-normal  :vec3)
                            (frag-pos     :vec3)
                            &uniform
                            (albedo-sam   :sampler-2d)
                            (position-sam :sampler-2d)
                            (normal-sam   :sampler-2d)
                            (res          :vec2)
                            (cam-pos      :vec3)
                            (light-color  :vec3)
                            (light-pos    :vec3))
  (let* ((uv       (/ (s~ gl-frag-coord :xy) res))
         (albedo   (s~ (texture albedo-sam   uv) :xyz))
         (position (s~ (texture position-sam uv) :xyz))
         (normal   (normalize
                    (s~ (texture normal-sam  uv) :xyz)))
         (final-color (point-light-apply albedo
                                         (* 5 (v! .6 .5 .3))
                                         light-pos
                                         position
                                         normal
                                         1f0
                                         .22
                                         .2
                                         cam-pos
                                         .9
                                         .1)))
    (v! final-color 0)))

(defpipeline-g light-volume-pipe ()
  :vertex   (vert g-pnt)
  :fragment (light-volume-frag :vec2 :vec3 :vec3))
