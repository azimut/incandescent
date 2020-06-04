(in-package #:incandescent)

(defvar *light-volumes* (list))
(defvar *blend-defer* (make-blending-params :source-rgb :one
                                            :destination-rgb :one))

;; Stencil light volumes from:
;; http://ogldev.atspace.co.uk/www/tutorial37/tutorial37.html

(defclass instance-light-volume (base-particles)
  ((otr-src :documentation "buffer stream used for src tfs")
   (otr-dst :documentation "buffer stream used for dst tfs")
   ubo-src
   ubo-dst))

(defmethod free ((obj instance-light-volume))
  (with-slots (otr-src otr-dst
               ubo-src ubo-dst
               str-src str-dst gar-src gar-dst tfs-src tfs-dst) obj
    (free ubo-src)
    (free ubo-dst)
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
               ubo-src ubo-dst
               otr-src otr-dst
               gar-src gar-dst str-src str-dst tfs-src tfs-dst) obj
    (setf gar-src (make-gpu-array nil :dimensions n-particles :element-type 'pdata)
          gar-dst (make-gpu-array nil :dimensions n-particles :element-type 'pdata)
          str-src (make-sphere-stream gar-src)
          str-dst (make-sphere-stream gar-dst)
          otr-src (make-buffer-stream gar-src :primitive :points)
          otr-dst (make-buffer-stream gar-dst :primitive :points)
          tfs-src (make-transform-feedback-stream gar-src)
          tfs-dst (make-transform-feedback-stream gar-dst)
          ubo-src (make-ubo-from-array gar-src 0 'pdata)
          ubo-dst (make-ubo-from-array gar-dst 0 'pdata))
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
(defun-g pupdate-vert ((pdata  pdata)
                       &uniform
                       (time   :float))
  (with-slots (pos dir life) pdata
    (let* ((time (* time .2 (* 2f0 gl-vertex-id)))
           (life life)
           (new-life (+ life .01))
           (dir dir)
           (pos pos)
           (r (rand (vec2 time))))
      (if (>= (x dir) 2f0)
          (progn ;; Reset
            #+nil
            (setf dir  (v! (* 360 r) ;; rot
                           (+ 7 (* 5 r)) ;; scale
                           0))
            (setf dir (v! 0 r r))
            ;;(setf life 4f0)
            (setf pos  (v! (+ -9 (* 18 (rand (vec2 (* 3 time)))))
                           1
                           (+ -25 (* 50 r)))))
          (progn ;; Update
            (incf (x dir) .02)
            (incf (z pos) -.05)
            (incf (x pos) .05)))
      (values (v! 0 0 0 0)
              (:feedback pos)
              (:feedback dir)
              (:feedback life)))))

(defpipeline-g pupdate-pipe (:points)
  :vertex (pupdate-vert pdata))

(defmethod update ((actor instance-light-volume) dt)
  (with-slots (tfs-dst otr-src) actor
    (with-transform-feedback (tfs-dst)
      (map-g #'pupdate-pipe otr-src
             :time dt))))
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
            (pdata-pos pdata)
            (pdata-dir pdata))))

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
                                         (* (* 2 (x light-color))
                                            (v! .8 (y light-color)
                                                (z light-color)))
                                         light-pos
                                         position
                                         normal
                                         1f0
                                         .22
                                         .20
                                         cam-pos
                                         .9
                                         .1)))
    (v! final-color 1)))

(defpipeline-g light-volume-pipe ()
  :vertex   (light-volume-vert g-pnt pdata)
  :fragment (light-volume-frag :vec3 :vec3))

;; Stencil only pipeline
(defun-g simple-3d-frag ((light-pos :vec3) (light-color :vec3))
  (values))
(defpipeline-g simple-3d-pipe ()
  :vertex (light-volume-vert g-pnt pdata)
  :fragment (simple-3d-frag :vec3 :vec3))

(defmethod swap-particles ((actor instance-light-volume))
  (with-slots (otr-src otr-dst
               tfs-src tfs-dst str-src str-dst gar-src gar-dst) actor
    (rotatef otr-src otr-dst)
    (rotatef tfs-src tfs-dst)
    (rotatef str-src str-dst)
    (rotatef gar-src gar-dst)))

(defmethod draw ((actor instance-light-volume) camera time)
  (gl:enable :stencil-test)
  (with-fbo-bound (*dsfbo* :attachment-for-size :d)
    (clear-fbo *dsfbo* :s)
    (with-setf (cull-face) nil
      (gl:stencil-func :always 0 0)
      (gl:stencil-op-separate :back :keep :incr-wrap :keep)
      (gl:stencil-op-separate :front :keep :decr-wrap :keep)
      (with-slots (str-src n-particles) actor
        (with-instances n-particles
          (map-g #'simple-3d-pipe str-src
                 :model-clip (m4:* (projection  *currentcamera*)
                                   (world->view *currentcamera*)))))))
  (with-fbo-bound (*sdfbo*)
    (clear-fbo *sdfbo* 0)
    (with-blending *blend-defer*
      (with-setf* ((cull-face) :front
                   (depth-test-function) nil)
        (gl:stencil-func :notequal 0 #xff)
        (with-slots (str-src n-particles) actor
          (with-instances n-particles
            (map-g #'light-volume-pipe str-src
                   :res (viewport-resolution (current-viewport))
                   :cam-pos (pos *currentcamera*)
                   :albedo-sam   *sam*
                   :position-sam *sam1*
                   :normal-sam   *sam2*
                   :model-clip   (m4:* (projection  *currentcamera*)
                                       (world->view *currentcamera*))))))))
  (gl:disable :stencil-test)
  (swap-particles actor))
