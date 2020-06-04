(in-package #:incandescent)

;; SEGI

(defvar *voxel-fbo*        nil)
(defvar *voxel-light*      nil)
(defvar *voxel-light-sam*  nil)
(defvar *voxel-light-zam*  nil)

(defparameter *volume-dimension* 64)

(defparameter *voxel-mipmaps* 6)
(defparameter *voxel-dimensions* (v3! *volume-dimension*))

(defparameter *rotation-front* (q! 0.0 0.0 0.0 1.0))
(defparameter *rotation-left*  (q! 0.0 0.7 0.0 0.7))
(defparameter *rotation-top*   (q! 0.7 0.0 0.0 0.7))

(defparameter *voxel-space-origin* (v! 16 11 16) ;;(v! 10 10 10)
  )
(defparameter *voxel-space-size* 25f0)

;; AKA voxelCameraGO
(defparameter *front-view*
  (make-instance
   'orth
   :near 0f0 :far *voxel-space-size*
   :pos (v3:- *voxel-space-origin*
              (v3:*s (v3:*s *vec3-forward* *voxel-space-size*)
                     .5f0))
   :rot *rotation-front*
   :frame-size (v2! (* *voxel-space-size* .11f0))))

;; don't need this much stuff as i only care for the world->model matrix...but
;; gives a space to store rot and pos
;;#+nil
(defparameter *left-view*
  (make-instance
   'orth
   :pos (v3:+ *voxel-space-origin*
              (v3:*s (v3:*s *vec3-left* *voxel-space-size*) .5f0))
   :rot *rotation-left*))
;;#+nil
(defparameter *top-view*
  (make-instance
   'orth
   :pos (v3:+ *voxel-space-origin*
              (v3:*s (v3:*s *vec3-up* *voxel-space-size*) .5f0))
   :rot *rotation-top*))

(defun clear-voxel ()
  (%gl:clear-tex-image (texture-id *voxel-light*)
                       0
                       :rgba
                       :float
                       (cffi:null-pointer)))

(defun free-voxel ()
  (when *voxel-fbo*   (free *voxel-fbo*))
  (when *voxel-light* (free *voxel-light*)))

(defun init-voxel ()
  (free-voxel)
  (setf *voxel-fbo*
        (make-fbo `(:d :dimensions (,*volume-dimension* ,*volume-dimension*))))
  (setf *voxel-light*
        (make-texture nil :dimensions `(,*volume-dimension*
                                        ,*volume-dimension*
                                        ,*volume-dimension*)
                          :mipmap *voxel-mipmaps*
                          :element-type :rgba8))
  (setf *voxel-light-zam* (sample *voxel-light* :magnify-filter :linear
                                                :minify-filter :linear
                                                :wrap :clamp-to-border
                                                ))
  (setf *voxel-light-sam* (sample *voxel-light* :magnify-filter :linear
                                                :minify-filter :linear
                                                :wrap :clamp-to-border
                                                ))
  (setf (cepl.samplers::border-color *voxel-light-sam*) (v! 1 1 1 1))
  (setf (cepl.samplers::border-color *voxel-light-zam*) (v! 1 1 1 1))
  (setf (%cepl.types::%sampler-imagine *voxel-light-sam*) t)
  (clear-voxel))

;;#+nil
(let ((stepper (make-stepper (seconds 1.5)
                             (seconds 1.5))))
  (defun draw-voxel ()
    (when (funcall stepper)
      (clear-voxel)
      (with-fbo-bound (*voxel-fbo* :attachment-for-size :d)
        (with-setf* ((depth-mask) nil
                     (depth-test-function) nil
                     (cull-face)  nil
                     (clear-color) (v! 0 0 0 1))
          (dolist (actor *actors*)
            (with-slots (buf scale) actor
              (map-g #'voxelize-pipe buf
                     :light-pos *light-pos*
                     :scale scale
                     :model-world (model->world actor)
                     ;;
                     :view-clip        (projection *front-view*)
                     :voxel-resolution *volume-dimension*
                     :voxel-view-front (m4:inverse (model->world *front-view*))
                     :voxel-view-left  (m4:inverse (model->world *left-view*))
                     :voxel-view-top   (m4:inverse (model->world *top-view*))
                     ;;
                     :ithing *voxel-light-sam*
                     :cam-pos     (pos *currentcamera*))))))
      (generate-mipmaps *voxel-light*))))

;;:voxel-view-front (m4:inverse (model->world *front-view*))
;;:voxel-view-left (m4:inverse (model->world *left-view*))
;;:voxel-view-top (m4:inverse (model->world *top-view*)

;;--------------------------------------------------

(defun-g voxelize-vert ((vert g-pnt)
                        &uniform
                        (scale       :float)
                        (model-world :mat4))
  (let ((pos (* scale (pos vert)))
        (norm (norm vert)))
    (values (* model-world (v! pos 1))
            (normalize (* (m4:to-mat3 model-world) norm)))))

(defun-g voxelize-geom ((nor (:vec3 3))
                        &uniform
                        (view-clip        :mat4)
                        (voxel-view-front :mat4)
                        (voxel-view-left  :mat4)
                        (voxel-view-top   :mat4))
  (declare (output-primitive :kind :triangle-strip
                             :max-vertices 3))
  (let* ((pos (vector (s~ (gl-position (aref gl-in 0)) :xyz)
                      (s~ (gl-position (aref gl-in 1)) :xyz)
                      (s~ (gl-position (aref gl-in 2)) :xyz)))
         (v   (- (aref pos 1) (aref pos 0)))
         (w   (- (aref pos 2) (aref pos 0)))
         (real-normal (v! (- (* (y v) (z w)) (* (z v) (y w)))
                          (- (* (z v) (x w)) (* (x v) (z w)))
                          (- (* (x v) (y w)) (* (y v) (x w)))))
         (abs-normal  (abs real-normal))
         (angle       (int 0)))
    (cond ((and (> (x abs-normal) (y abs-normal))
                (> (x abs-normal) (z abs-normal)))
           (setf angle 1))
          ((and (> (y abs-normal) (x abs-normal))
                (> (y abs-normal) (z abs-normal)))
           (setf angle 2)))
    (dotimes (i 3)
      (let ((wp (v! (aref pos i) 1)))
        (cond ((= angle 0)
               (setf wp (* voxel-view-front wp)))
              ((= angle 1)
               (setf wp (* voxel-view-left  wp)))
              (t
               (setf wp (* voxel-view-top   wp))))
        ;; !
        (setf wp (* view-clip wp))
        (setf (z wp) (* (z wp) -1f0))
        (emit () wp;;gl-position
              wp
              (aref nor i)
              (:flat angle))))
    (end-primitive)
    (values)))

;;--------------------------------------------------
;; From sfreed141/vct
;; From OpenGL Insights:
;; https://rauwendaal+.net/2013/02/07/glslrunningaverage/
(defun-g image-atomic-rgba8-avg ((voxel-light :uimage-3d)
                                 (coord       :ivec3)
                                 (val         :vec4))
  (let ((val (v! (s~ val :xyz) (/ 255f0)))
        (new-val (pack-unorm4x8 val))
        (prev-stored-val (uint 0))
        (cur-stored-val  (uint 0)))
    ;; Spin(lock) wait while other threads modify
    (while (not (= (setf cur-stored-val
                         (image-atomic-comp-swap voxel-light
                                                 coord
                                                 prev-stored-val
                                                 new-val))
                   prev-stored-val))
           (setf prev-stored-val cur-stored-val)
           ;; Extract the moving average
           ;; (current average in rgb, normalized count in w)
           (let* ((avg (unpack-unorm4x8 cur-stored-val))
                  (avg (v! (/ (+ (* (s~ avg :xyz) (w avg))
                                 (* (s~ val :xyz) (w val))))
                           (+ (w avg) (w val)))))
             (setf new-val (pack-unorm4x8 avg))))))

;; SEGI?
(defun-g voxelize-frag ((pos   :vec4)
                        (nor   :vec3)
                        (angle :int)
                        &uniform
                        (voxel-resolution :int)
                        (ithing           :uimage-3d)
                        (light-pos        :vec3)
                        (cam-pos          :vec3))
  (let* ((coord (ivec3 (int (x pos))
                       (int (y pos))
                       (int (* voxel-resolution (z pos)))))
         (abs-normal (abs nor))
         #+nil
         (color (dir-light-apply light-pos
                                 (v! .3 .1 .1)
                                 light-pos
                                 (s~ pos :xyz)
                                 (normalize (s~ nor :xyz))
                                 cam-pos
                                 .3
                                 .1))
         (color (dir-light-apply (v! .5 .5 .5)
                                 (* 2 *light-color*)
                                 (v! 50 50 50)
                                 (s~ pos :xyz)
                                 nor)))
    (when (= angle 1)
      (setf coord (s~ coord :zyx))
      (setf (z coord) (- voxel-resolution (z coord) 1)))
    (when (= angle 2)
      (setf coord (s~ coord :xzy))
      (setf (y coord) (- voxel-resolution (y coord) 1)))
    (image-atomic-rgba8-avg ithing
                            coord
                            (v! color 1))))

(defpipeline-g voxelize-pipe ()
  :vertex   (voxelize-vert g-pnt)
  :geometry (voxelize-geom (:vec3 3))
  :fragment (voxelize-frag :vec4 :vec3 :int))

;;--------------------------------------------------

(defun-g segi-diffuse-trace ((light-view-pos   :vec4)
                             (view-world       :mat4)
                             (voxel-world-view :mat4)
                             (voxel-view-clip  :mat4)
                             (normal           :vec3)
                             (voxel-light      :sampler-3d))
  (let* ((view-vector (normalize (s~ light-view-pos :xyz)))
         (voxel-scale-factor   (/ 64f0 256f0))
         (num-samples (int 6))
         (trace-steps (int 14))
         (trace-result (v! 0 0 0 0))
         (gi           (v! 0 0 0))
         (voxel-space-position (* view-world light-view-pos))
         (voxel-space-position (* voxel-world-view voxel-space-position))
         (voxel-space-position (v! (+ .5 (* .5 (s~ voxel-space-position :xyz)))
                                   (w voxel-space-position)))
         (voxel-origin         (+ (s~ voxel-space-position :xyz)
                                  (/ (* normal .003 1.25) voxel-scale-factor))))
    (dotimes (i num-samples)
      (incf trace-result (cone-trace voxel-origin
                                     (v! 1 1 1)
                                     normal
                                     voxel-light
                                     trace-steps
                                     5.5 1f0 1f0))
      (divf trace-result (vec4 num-samples)))
    (* 20 trace-result)))

(defun-g cone-trace ((voxel-origin :vec3)
                     (kernel       :vec3)
                     (world-normal :vec3)
                     ;;
                     (voxel-light  :sampler-3d)
                     ;; uv
                     ;; noise
                     (steps        :int)
                     (width        :float)
                     (length-mult  :float)
                     (sky-mult     :float))
  (let* ((voxel-scale-factor (/ 64f0 256f0))
         (num-steps (int
                     (* steps (mix voxel-scale-factor 1f0 .5))))
         (adjusted-kernel (normalize kernel))
         (gi (v! 0 0 0))
         (trace-length 3f0)
         (near-light-gain  1f0))
    (dotimes (i num-steps)
      (let* ((fi (/ (float i) num-steps))
             (fi (mix fi 1f0 .01))
             (cone-distance (/ (- (exp2 (* fi 4f0)) .9)
                               8f0))
             (cone-size (* fi width (mix voxel-scale-factor 1f0 .5)))
             (voxel-check-coord (+ voxel-origin
                                   (* adjusted-kernel
                                      (+ .001
                                         (* cone-distance
                                            .12
                                            trace-length
                                            length-mult)))))
             (color (v! 0 0 0 0))
             (mipmap-level (floor cone-size))
             (color (texture-lod voxel-light voxel-check-coord mipmap-level))
             (falloff-fix (+ near-light-gain (* 4f0 (pow fi 1f0)))))
        (incf gi (* (s~ color :xyz)
                    (+ 1f0 (* cone-size 1f0))
                    ;;occlusion
                    falloff-fix))))
    (* (v! (* .8 gi) 0)
       (pow (saturate (* 1f0 (dot world-normal kernel))) .5))))

#+nil
(s~ (segi-diffuse-trace (* light-view (v! pos 1))
                        view-world
                        voxel-world-view
                        voxel-view-clip
                        norm
                        voxel-light)
    :xyz)
