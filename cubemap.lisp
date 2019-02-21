(in-package #:incandescent)

;;--------------------------------------------------
;; Cubemap
;;
;; TODO:
;; - Cache cubemaps?

(defclass env-map (actor)
  ((cubetex :initarg :cubetex)
   (cubesam :initarg :cubesam)
   (buf     :initform (box))))

(defun make-env-map (tex sam)
  (declare (type cepl:texture tex)
           (type cepl:sampler sam))
  (let ((obj (make-instance 'env-map :cubetex tex :cubesam sam)))
    (push obj *actors*)
    obj))

(defmethod update ((actor env-map) dt)
  (setf (pos actor) (pos *currentcamera*))
  ;;(setf (rot actor) (rot *currentcamera*))
  )

(defmethod draw ((actor env-map) camera time)
  (with-slots (buf cubesam) actor
    (with-setf* ((cull-face) :front
                 (depth-test-function) #'<=
                 (depth-mask) nil)
      (map-g #'cubemap-pipe buf
             :tex cubesam
             ;; Rotation without translation
             :view (q:to-mat4
                    (q:inverse (rot camera)))
             :projection (projection camera)))))

(defun make-cubemap-tex (&rest paths)
  "Returns a gpu texture from the provided images"
  (assert (= 6 (length paths)))
  (with-c-arrays-freed
      (ca (mapcar (lambda (p)
                    (dirt:load-image-to-c-array
                     (asdf:system-relative-pathname :incandescent p)))
                  paths))
    (make-texture ca :element-type :rgb8 :cubes t)))

;; Souce Cubemap with clouds
(defvar *t-cubemap* nil)
(defvar *s-cubemap* nil)

(defun free-cubes ()
  (when *t-cubemap*
    (free *t-cubemap*)
    (setf *t-cubemap* NIL)))

(defun init-cubemap ()
  (unless *t-cubemap*
    (setf *t-cubemap*
          (make-cubemap-tex
           "static/ThickCloudsWater/left.png"
           "static/ThickCloudsWater/right.png"
           "static/ThickCloudsWater/up.png"
           "static/ThickCloudsWater/down.png"
           "static/ThickCloudsWater/front.png"
           "static/ThickCloudsWater/back.png"))
    (setf *s-cubemap*
          (sample *t-cubemap*
                  :wrap :clamp-to-edge
                  :magnify-filter :linear))))

;;--------------------------------------------------

;; Reference: Cubemap generation mostly from
;; https://learnopengl.com/PBR/IBL/Diffuse-irradiance
(defvar *cubemap-sides* '("left" "right" "bottom" "front" "top" "back"))
(defvar *cubemap-rotations*
  (list
   (list (v! 0 -1  0) (v! 0 0 0) (v!  1  0  0))
   (list (v! 0 -1  0) (v! 0 0 0) (v! -1  0  0))
   (list (v! 0  0 -1) (v! 0 0 0) (v!  0 -1  0))
   (list (v! 0  0  1) (v! 0 0 0) (v!  0  1  0))
   (list (v! 0 -1  0) (v! 0 0 0) (v!  0  0  1))
   (list (v! 0 -1  0) (v! 0 0 0) (v!  0  0 -1))))

;;--------------------------------------------------
;; Cubemap
;; Rendering order does not matter
;; Use it with a 1x1x1 box AND
;; depth-test-function #'<=
(defun-g cubemap-vert ((g-pnt g-pnt)
                       &uniform
                       (view :mat4)
                       (projection :mat4))
  (let* ((pos3  (pos g-pnt))
         (pos4  (v! pos3 1))
         (cpos4 (* projection view pos4)))
    (values (s~ cpos4 :xyww)
            pos3)))

(defun-g cubemap-frag ((tc :vec3)
                       &uniform
                       (tex :sampler-cube))
  (let* ((color (s~ (texture tex tc) :xyz)))
    (v! color 1)))

(defpipeline-g cubemap-pipe ()
  :vertex   (cubemap-vert g-pnt)
  :fragment (cubemap-frag :vec3))

;;--------------------------------------------------
(defvar *cube-tex* NIL)
(defvar *cube-sam* NIL)

(defun make-render-cubemap (camera &optional (pos (v! 0 0 0)))
  (let ((dst-cubemap (make-texture
                      nil
                      :dimensions '(2048 2048)
                      :cubes t
                      :element-type :rgb16f)))
    (cubemap-render-to-cubemap camera dst-cubemap pos)
    dst-cubemap))
(defun init-render-cubemap ()
  (unless *cube-tex*
    (setf *cube-tex* (make-render-cubemap *camera-cubemap*))
    (setf *cube-sam* (sample *cube-tex*
                             :wrap :clamp-to-edge
                             :magnify-filter :linear))))
(defun cubemap-render-to-cubemap (camera dst-cubemap &optional (pos (v! 0 0 0)))
  "Fills the provided DST-CUBEMAP texture with render of
   current scene. Dimensions of the DST-CUBEMAP adviced as 2048x2048"
  (assert (eq :RGB16F (element-type (texref dst-cubemap))))
  (assert (texture-cubes-p dst-cubemap))
  (let ((dimensions (dimensions dst-cubemap)))
    (assert (apply #'= dimensions))
    (setf (resolution (current-viewport)) (v! dimensions))
    (setf (fov camera) 90f0)
    (setf (pos camera) pos)
    (with-free*
        ((bs (make-buffer-stream nil :primitive :points))
         (external-fbo
          (make-fbo
           (list 0 :dimensions dimensions :element-type :rgb16f)
           (list :d :dimensions dimensions)))
         (external-sample (sample (attachment-tex external-fbo 0)))
         (fbo
          (make-fbo
           (list 0 :dimensions dimensions :element-type :rgb16f)))
         (pipeline
          (pipeline-g (:points)
            :fragment
            (lambda-g ((uv :vec2) &uniform (sam :sampler-2d))
              (let ((color (s~ (texture sam uv) :xyz)))
                (v! color 1))))))
      (loop
        :for side :in *cubemap-sides*
        :for rotation :in *cubemap-rotations*
        :for face :from 0
        :do
        ;; Rotate camera
           (destructuring-bind (up from to) rotation
             (setf (rot camera)
                   (q:look-at up from to)))
           ;; Normal draw - preferably a 16bit fbo to avoid dithering
           (with-fbo-bound (external-fbo)
             (clear external-fbo)
             (loop :for actor :in *actors* :do
                      (draw actor camera 1f0)))
           ;; Switch FBO texture for one of the cubemap
           (setf (attachment fbo 0)
                 (texref dst-cubemap :cube-face face))
           ;; Final draw to LDR (? the colors
           (map-g-into fbo pipeline bs
                       :sam external-sample)))))

;;--------------------------------------------------
(defun cubemap-save-to-disk (camera &optional (dim '(250 250)) (pos (v! 0 0 0)) (image-format "bmp"))
  "Save the current scene in *ACTORS* into 6 bmp images.
   Sadly the images are saved in a 8bit LDR images.
   > (cubemap-save-to-disk *current-camera* '(500 500) (v! 0 0 0) \"bmp\")"
  (declare (type string image-format))
  (assert (apply #'= dim))
  (assert (length= dim 2))
  (assert (length= pos 3))
  (setf (resolution (current-viewport)) (v! dim))
  (setf (pos camera) pos)
  (setf (fov camera) 90f0)
  (with-free*
      ((bs  (make-buffer-stream nil :primitive :points))
       (external-fbo
        (make-fbo
         (list 0 :dimensions dim :element-type :rgb16f)
         (list :d :dimensions dim)))
       (external-sample
        (sample (attachment-tex external-fbo 0)))
       (fbo (make-fbo (list 0 :dimensions dim)))
       (pipeline
        (pipeline-g (:points)
          :fragment
          (lambda-g ((uv :vec2) &uniform (sam :sampler-2d))
            (v! (pow (s~ (texture sam uv) :xyz)
                     (vec3 (/ 1f0 2.2)))
                1)))))
    (loop
       :for side :in *cubemap-sides*
       :for rotation :in *cubemap-rotations*
       :do
       ;; Rotate camera
         (destructuring-bind (up from to) rotation
           (setf (rot camera)
                 (q:look-at up from to)))
       ;; Normal draw - preferably a 16bit fbo to avoid dithering
         (with-fbo-bound (external-fbo)
           (clear external-fbo)
           (loop :for actor :in *actors* :do
                (draw actor camera 1f0)))
       ;; Final draw to LDR (? the colors
         (map-g-into fbo pipeline bs
                     :sam external-sample)
       ;; Read from our created fbo texture
         (dirt:save-as-image
          (attachment-tex fbo 0)
          (asdf:system-relative-pathname
           :incandescent
           (concatenate 'string
                        "static/cubemap_" side "." image-format))))))

(defun fbo-to-png ()
  "BROKEN...real range for rgb16f is wonky, max is 65535 but has some
   around .1 .2 1.2"
  (let* ((texture    (attachment-tex *fbo* 0))
         (dimensions (dimensions texture))
         (width      (first dimensions))
         (height     (second dimensions))
         (array      (texref texture))
         ;; INPUT 400x500
         (fbo-data   (pull-g array))
         ;; OUTPUT
         (png-image  (png:make-image height width 3 16)))
    (loop
       :for h :in fbo-data
       :for i :from 0 :do
         (loop
            :for w :in h
            :for j :from 0 :do
              (setf (aref png-image i j 0) (round (x w)))
              (setf (aref png-image i j 1) (round (print (y w))))
              (setf (aref png-image i j 2) (round (z w)))))
    (with-open-file (stream "/home/sendai/write.png"
                            :element-type '(unsigned-byte 16)
                            :direction :output
                            :if-exists :supersede)
      (png:encode png-image stream))))
