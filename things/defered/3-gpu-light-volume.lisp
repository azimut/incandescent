(in-package #:incandescent)

(defclass instance-light-volume (base-particles)
  ((otr-src :documentation "buffer stream used for src tfs")
   (otr-dst :documentation "buffer stream used for dst tfs")))

(defmethod free ((obj instance-light-volume))
  (with-slots (otr-src otr-dst str-src str-dst gar-src gar-dst tfs-src tfs-dst) obj
    (free str-src)
    (free str-dst)
    (free otr-src)
    (free otr-dst)
    (free gar-src)
    (free gar-dst)
    (setf tfs-src nil)
    (setf tfs-dst nil)))

(defun make-instance-light-volume (&key (n-particles 100))
  ;; HACK!
  (when *light-volumes*
    (mapcar #'free *light-volumes*)
    (setf *light-volumes* nil))
  (let ((obj (make-instance 'instance-light-volume
                            :n-particles n-particles)))
    (push obj *light-volumes*)
    obj))

(defun make-sphere-stream (gpu-data-array)
  "returns an per instance sphere stream"
  (destructuring-bind (vert index)
      (nineveh.mesh.data.primitives:sphere-gpu-arrays
       :radius 1f0)
    (make-buffer-stream (list vert (cons gpu-data-array 1))
                        :index-array index)))

(defmethod initialize-instance :after ((obj instance-light-volume) &key)
  (with-slots (n-particles
               otr-src otr-dst
               gar-src gar-dst str-src str-dst tfs-src tfs-dst) obj
    (setf gar-src (make-gpu-array nil :dimensions n-particles :element-type 'pdata)
          gar-dst (make-gpu-array nil :dimensions n-particles :element-type 'pdata)
          str-src (make-sphere-stream gar-src)
          str-dst (make-sphere-stream gar-dst)
          otr-src (make-buffer-stream gar-src :primitive :points)
          otr-dst (make-buffer-stream gar-dst :primitive :points)
          tfs-src (make-transform-feedback-stream gar-src)
          tfs-dst (make-transform-feedback-stream gar-dst))
    (init-particles obj)))

(defun-g sphere-radius ((linear :float) (quadratic :float))
  (let ((constant 1f0)
        (light-max (* .6 5)))
    (/ (+ (- linear)
          (sqrt (- (* linear linear)
                   (* 4 quadratic (- constant (* (/ 256f0 5f0) light-max))))))
       (* 2 quadratic))))

(defun-g pinit-vert (&uniform (time :float))
  (let* ((id (* .00001 time gl-vertex-id))
         (range 40f0)
         (hrange (/ range 2))
         (pos (v! (- (* range (rand (vec2 (* 3 id)))) hrange)
                  2
                  (- (* range (rand (vec2 (* 5 id)))) hrange))))
    (values (v! 0 0 0 0)
            (:feedback pos)
            (:feedback (v! 0 0 0))
            (:feedback (sphere-radius .22 .2)))))
(make-instance-light-volume :n-particles 10)
(defpipeline-g pinit-pipe (:points)
  :vertex (pinit-vert))


(defun init-particles (actor)
  (let ((time (* .01f0 (get-internal-real-time))))
    (with-slots (tfs-src tfs-dst otr-src otr-dst) actor
      (with-transform-feedback (tfs-src)
        (map-g #'pinit-pipe otr-src
               :time time))
      (with-transform-feedback (tfs-dst)
        (map-g #'pinit-pipe otr-dst
               :time time))))
  (values))

;;--------------------------------------------------
;; Update
;;(make-instance-light-volume)
;;--------------------------------------------------
;; Render

(defun-g light-volume-vert ((vert g-pnt)
                            (pdata pdata)
                            &uniform
                            (model-clip :mat4))
  (let* ((pos        (+ (pdata-pos pdata)
                        (* (pdata-life pdata) (pos vert))))
         (clip-pos   (* model-clip  (v! pos 1))))
    (values clip-pos
            (pdata-pos pdata))))

(defun-g light-volume-frag ((light-pos    :vec3)
                            &uniform
                            (albedo-sam   :sampler-2d)
                            (position-sam :sampler-2d)
                            (normal-sam   :sampler-2d)
                            (res          :vec2)
                            (cam-pos      :vec3)
                            (light-color  :vec3))
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
                                         .20
                                         cam-pos
                                         .9
                                         .1)))
    (v! final-color 1)
    ;;(v! 1 0 0 1)
    ))

(defpipeline-g light-volume-pipe ()
  :vertex   (light-volume-vert g-pnt pdata)
  :fragment (light-volume-frag :vec3))
(defun-g simple-3d-frag ((light-pos :vec3))
  (v! 1 0 0 1))
(defpipeline-g simple-3d-pipe ()
  :vertex (light-volume-vert g-pnt pdata)
  :fragment (simple-3d-frag :vec3))

(defmethod draw ((actor instance-light-volume) camera time)
  ;;#+nil
  (with-slots (str-src n-particles) actor
    #+nil
    (map-g #'light-volume-pipe nil
           :res (viewport-resolution (current-viewport))
           :cam-pos (pos *currentcamera*)
           :albedo-sam   *sam*
           :position-sam *sam1*
           :normal-sam   *sam2*
           :model-clip   (m4:* (projection  *currentcamera*)
                               (world->view *currentcamera*)))
    (with-blending *blend-defer*
      (with-instances 20
        (map-g #'light-volume-pipe str-src
               :res (viewport-resolution (current-viewport))
               :cam-pos (pos *currentcamera*)
               :albedo-sam   *sam*
               :position-sam *sam1*
               :normal-sam   *sam2*
               :model-clip   (m4:* (projection  *currentcamera*)
                                   (world->view *currentcamera*))
               ;;:scale 27f0;;(sphere-radius .22 .20)
               ;;
               ))))
  (swap-particles actor))
