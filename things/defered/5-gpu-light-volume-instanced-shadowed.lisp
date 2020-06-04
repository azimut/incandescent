(in-package #:incandescent)

;; Wrongly implemented...as the lights do not need to get extended, but the
;; objects to be casted out by the light do...

(defvar *light-volumes* (list))
(defvar *blend-defer* (make-blending-params :source-rgb :one
                                            :destination-rgb :one))

;; Stencil light volumes from:
;; http://ogldev.atspace.co.uk/www/tutorial37/tutorial37.html

(defclass instance-light-volume (base-particles)
  ((otr-src :documentation "buffer stream used for src tfs")
   (otr-dst :documentation "buffer stream used for dst tfs")
   sha-src
   sha-dst))

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

(defun make-shadow-sphere-stream (gpu-data-array)
  "returns an per instance sphere stream"
  (destructuring-bind (vert index)
      (nineveh.mesh.data.primitives:sphere-gpu-arrays
       :radius 1f0)
    (make-buffer-stream (list vert (cons gpu-data-array 1))
                        :index-array index
                        :primitive :triangles-adjacency)))

(defmethod initialize-instance :after ((obj instance-light-volume) &key)
  (with-slots (n-particles
               otr-src otr-dst
               sha-src sha-dst
               gar-src gar-dst str-src str-dst tfs-src tfs-dst) obj
    (setf gar-src (make-gpu-array nil :dimensions n-particles :element-type 'pdata)
          gar-dst (make-gpu-array nil :dimensions n-particles :element-type 'pdata)
          str-src (make-sphere-stream gar-src)
          str-dst (make-sphere-stream gar-dst)
          sha-src (make-shadow-sphere-stream gar-src)
          sha-dst (make-shadow-sphere-stream gar-dst)
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
                           (* 5 r)
                           (+ -25 (* 50 r)))))
          (progn ;; Update
            (incf (x dir) .01)
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

;; Light Stencil only pipeline
(defun-g simple-3d-frag ((light-pos :vec3) (light-color :vec3))
  (values))
(defpipeline-g simple-3d-pipe ()
  :vertex (light-volume-vert g-pnt pdata)
  :fragment (simple-3d-frag :vec3 :vec3))

;;----------------------------------------
;; Shadow Stencil only pipeline
(defun-g shadow-stencil-vert ((vert g-pnt)
                              (pdata pdata)
                              &uniform
                              (model-clip :mat4))
  #+nil
  (let* ((pos        (+ (pdata-pos pdata)
                        (* (pdata-life pdata) (pos vert))))
         ;;(clip-pos   (* model-clip (v! pos 1)))
         ))
  (values (v! 0 0 0 0)
          (+ (pdata-pos pdata)
             ;;(* (pdata-life pdata))
             (pos vert))
          ;;(pdata-pos pdata)
          (v! 10 10 100)
          ))

(defun-g shadow-stencil-geom ((position  (:vec3 6))
                              (light-pos (:vec3 6))
                              &uniform
                              (model-clip :mat4))
  (declare
   (output-primitive :kind :triangle-strip
                     :max-vertices 18))
  (labels ((emit-quad ((start-pos  :vec3)
                       (end-pos    :vec3)
                       (light-pos  :vec3)
                       (model-clip :mat4))
             (let* ((epsilon .01)
                    (light-dir (normalize (- start-pos (aref light-pos 0))))
                    (l (* light-dir epsilon)))
               (emit () (* model-clip (v! (+ start-pos l) 1)))
               (emit () (* model-clip (v! light-dir 0)))
               (setf light-dir (normalize (- end-pos (aref light-pos 0))))
               (setf l (* light-dir epsilon))
               (emit () (* model-clip (v! (+ end-pos l) 1)))
               (emit () (* model-clip (v! light-dir 0)))
               (end-primitive)
               (values))))
    (let* ((e1 (- (aref position 2) (aref position 0)))
           (e2 (- (aref position 4) (aref position 0)))
           (e3 (- (aref position 1) (aref position 0)))
           (e4 (- (aref position 3) (aref position 2)))
           (e5 (- (aref position 4) (aref position 2)))
           (e6 (- (aref position 5) (aref position 0)))
           (normal (cross e1 e2))
           (epsilon .01)
           (light-dir (- (aref light-pos 0) (aref position 0))))
      (when (> (dot normal light-dir) .000001)
        (let ((start-pos (vec3 0f0))
              (end-pos   (vec3 0f0))
              (normal    (cross e3 e1)))
          (when (<= (dot normal light-dir) 0)
            (setf start-pos (aref position 0))
            (setf end-pos   (aref position 2))
            (emit-quad start-pos end-pos (aref light-pos 0) model-clip))
          (setf normal    (cross e4 e5))
          (setf light-dir (- (aref light-pos 0) (aref position 2)))
          (when (<= (dot normal light-dir) 0)
            (setf start-pos (aref position 2))
            (setf end-pos   (aref position 4))
            (emit-quad start-pos end-pos (aref light-pos 0) model-clip))
          (setf normal    (cross e2 e6))
          (setf light-dir (- (aref light-pos 0) (aref position 4)))
          (when (<= (dot normal light-dir) 0)
            (setf start-pos (aref position 4))
            (setf end-pos   (aref position 0))
            (emit-quad start-pos end-pos (aref light-pos 0) model-clip))
          ;;
          (setf light-dir   (* epsilon (normalize (- (aref position 0) (aref light-pos 0)))))
          (emit () (* model-clip (v! (+ (aref position 0) light-dir) 1)))
          (setf light-dir   (* epsilon (normalize (- (aref position 2) (aref light-pos 0)))))
          (emit () (* model-clip (v! (+ (aref position 2) light-dir) 1)))
          (setf light-dir   (* epsilon (normalize (- (aref position 4) (aref light-pos 0)))))
          (emit () (* model-clip (v! (+ (aref position 4) light-dir) 1)))
          (end-primitive)
          (values))))))

(defun-g shadow-stencil-frag ()
  (v! 1 0 0 1))

(defun-g shadow-stencil-frag ((wpos :vec3) (lpos :vec3))
  (v! 1 0 0 1))

(defpipeline-g shadow-stencil-pipe (:triangles-adjacency)
  :vertex   (shadow-stencil-vert g-pnt pdata)
  :geometry (shadow-stencil-geom (:vec3 6) (:vec3 6))
  :fragment (shadow-stencil-frag))

(defmethod swap-particles ((actor instance-light-volume))
  (with-slots (sha-src sha-dst
               otr-src otr-dst
               tfs-src tfs-dst str-src str-dst gar-src gar-dst) actor
    (rotatef otr-src otr-dst)
    (rotatef sha-src sha-dst)
    (rotatef tfs-src tfs-dst)
    (rotatef str-src str-dst)
    (rotatef gar-src gar-dst)))

(defmethod draw ((actor instance-light-volume) camera time)
  ;;#+nil
  (with-slots (str-src n-particles) actor
    (with-instances n-particles
      (map-g #'shadow-stencil-pipe str-src
             :model-clip (m4:* (projection  *currentcamera*)
                               (world->view *currentcamera*)))))
  ;; (gl:enable :stencil-test)
  ;; ;; Light stencil
  ;; (with-fbo-bound (*dsfbo* :attachment-for-size :d)
  ;;   (clear-fbo *dsfbo* :s)
  ;;   (gl:stencil-func :always 0 0)
  ;;   (gl:stencil-op-separate :back :keep :incr-wrap :keep)
  ;;   (gl:stencil-op-separate :front :keep :decr-wrap :keep)
  ;;   (with-setf (cull-face) nil
  ;;     (with-slots (str-src n-particles) actor
  ;;       (with-instances n-particles
  ;;         (map-g #'simple-3d-pipe str-src
  ;;                :model-clip (m4:* (projection  *currentcamera*)
  ;;                                  (world->view *currentcamera*)))))))
  ;; ;;
  ;; (with-fbo-bound (*sdfbo*)
  ;;   (clear-fbo *sdfbo* 0)
  ;;   (with-blending *blend-defer*
  ;;     (with-setf* ((cull-face) :front
  ;;                  (depth-test-function) nil)
  ;;       ;; glStencilOpSeparate(GL_BACK, GL_KEEP, GL_KEEP, GL_KEEP);
  ;;       ;; glStencilFunc(GL_EQUAL, 0x0, 0xFF);
  ;;       ;; (gl:stencil-op-separate :back  :keep :keep :keep)
  ;;       (gl:stencil-func :notequal 0 #xff)
  ;;       (with-slots (str-src n-particles) actor
  ;;         (with-instances n-particles
  ;;           (map-g #'light-volume-pipe str-src
  ;;                  :res (viewport-resolution (current-viewport))
  ;;                  :cam-pos (pos *currentcamera*)
  ;;                  :albedo-sam   *sam*
  ;;                  :position-sam *sam1*
  ;;                  :normal-sam   *sam2*
  ;;                  :model-clip   (m4:* (projection  *currentcamera*)
  ;;                                      (world->view *currentcamera*))))))))
  ;; (gl:disable :stencil-test)
  (swap-particles actor))


(defun-g shadow-roman-column-geom ((position  (:vec3 6))
                                   (light-pos (:vec3 6))
                                   &uniform
                                   (world-clip :mat4))
  (declare
   (output-primitive :kind :triangle-strip
                     :max-vertices 18))
  (labels ((emit-quad ((start-pos  :vec3)
                       (end-pos    :vec3)
                       (light-pos3 :vec3)
                       (world-clip :mat4))
             (let* ((epsilon .8)
                    (light-dir (normalize (- start-pos light-pos3)))
                    (deviation (* light-dir epsilon)))
               (emit () (* world-clip (v! (+ start-pos deviation) 1)))
               (emit () (* world-clip (v! light-dir 0)))
               (setf light-dir (normalize (- end-pos light-pos3)))
               (setf deviation (* light-dir epsilon))
               (emit () (* world-clip (v! (+ end-pos deviation) 1)))
               (emit () (* world-clip (v! light-dir 0)))
               (end-primitive)))
           (faces-light ((a :vec3) (b :vec3) (c :vec3) (light-pos3 :vec3))
             (let* ((n  (cross (- b a) (- c a)))
                    (da (- light-pos3 a))
                    (db (- light-pos3 b))
                    (dc (- light-pos3 c)))
               (or (> (dot n da) 0)
                   (> (dot n db) 0)
                   (> (dot n dc) 0)))))
    (let* ((e1 (- (aref position 2) (aref position 0)))
           (e2 (- (aref position 4) (aref position 0)))
           (e3 (- (aref position 1) (aref position 0)))
           (e4 (- (aref position 3) (aref position 2)))
           (e5 (- (aref position 4) (aref position 2)))
           (e6 (- (aref position 5) (aref position 0)))
           (normal (cross e1 e2))
           (epsilon .8)
           (light-dir (- (aref light-pos 0) (aref position 0))))
      (when (> (dot normal light-dir) .000001)
        (let ((start-pos (vec3 0f0))
              (end-pos   (vec3 0f0))
              (normal    (cross e3 e1)))
          (when (<= (dot normal light-dir) 0)
            (setf start-pos (aref position 0))
            (setf end-pos   (aref position 2))
            (emit-quad start-pos end-pos (aref light-pos 0) world-clip))
          (setf normal    (cross e4 e5))
          (setf light-dir (- (aref light-pos 0) (aref position 2)))
          (when (<= (dot normal light-dir) 0)
            (setf start-pos (aref position 2))
            (setf end-pos   (aref position 4))
            (emit-quad start-pos end-pos (aref light-pos 0) world-clip))
          (setf normal    (cross e2 e6))
          (setf light-dir (- (aref light-pos 0) (aref position 4)))
          (when (<= (dot normal light-dir) 0)
            (setf start-pos (aref position 4))
            (setf end-pos   (aref position 0))
            (emit-quad start-pos end-pos (aref light-pos 0) world-clip))
          ;;
          (setf light-dir   (* epsilon (normalize (- (aref position 0)
                                                     (aref light-pos 0)))))
          (emit () (* world-clip (v! (+ (aref position 0) light-dir) 1)))
          (setf light-dir   (* epsilon (normalize (- (aref position 2)
                                                     (aref light-pos 0)))))
          (emit () (* world-clip (v! (+ (aref position 2) light-dir) 1)))
          (setf light-dir   (* epsilon (normalize (- (aref position 4)
                                                     (aref light-pos 0)))))
          (emit () (* world-clip (v! (+ (aref position 4) light-dir) 1)))
          (end-primitive)
          (values))))))
