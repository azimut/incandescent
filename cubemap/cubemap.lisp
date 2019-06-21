(in-package #:incandescent)

;; THE CAMERA
(defvar *camera-cubemap*
  (make-instance 'pers
                 :name :cubemap-cam
                 :fov 90f0))

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
      (ca (mapcar
           (lambda (p)
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
(defparameter *cubemap-sides* '("left"   "right"
                                "bottom" "top"
                                "front"  "back"))
(defparameter *cubemap-rotations*
  (list
   (list (v! 0 -1 0) (v! 0 0 0) *vec3-right*)
   (list (v! 0 -1 0) (v! 0 0 0) *vec3-left*)
   (list (v! 0 -1 0) (v! 0 0 0) *vec3-down*)
   (list (v! 0 -1 0) (v! 0 0 0) *vec3-up*)
   (list (v! 0 -1 0) (v! 0 0 0) *vec3-forward*)
   (list (v! 0 -1 0) (v! 0 0 0) *vec3-back*)))

;;--------------------------------------------------
;; Cubemap
;; Rendering order does not matter
;; Use it with a 1x1x1 box AND-test-function #'<=
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

(defun make-render-cubemap (&key (camera *camera-cubemap*)
                                 (pos (v! 0 0 0)))
  (let ((dst-cubemap (make-texture NIL :dimensions '(2048 2048)
                                       :cubes T
                                       :element-type :rgb16f)))
    (scene-to-cubemap dst-cubemap :camera camera :pos pos)
    dst-cubemap))

(defun init-render-cubemap ()
  (when *cube-tex* (free *cube-tex*))
  (setf *cube-tex* (make-render-cubemap))
  (setf *cube-sam* (sample *cube-tex* :wrap           :clamp-to-edge
                                      :magnify-filter :linear)))

;;--------------------------------------------------

(defun 16bit-texture-p (texture)
  (let ((element-type (symbol-name (texture-element-type texture))))
    (serapeum:string*= "16" element-type)))

(defun range-function-for-texture (texture)
  "returns an one(1) arg lambda that performs the value mapping"
  (if (16bit-texture-p texture)
      (lambda (val)
        (round
         (map-range 0f0 1f0 0 65535 (cpu-1-tone-map-reinhard val *exposure*))))
      (lambda (val)
        (round
         (map-range   0 255 0 65535 val)))))

(defun filename-to-save-function (filename &key)
  "returns a 2 arg function, which takes a tex and a filename"
  (alexandria:eswitch ((filename-extension filename) :test #'string=)
    ("png" #'texture-to-png-disk)
    ("bmp" #'dirt:save-as-image)
    ("tga" #'dirt:save-as-image)
    ("dds" #'dirt:save-as-image)))

(defun texture-to-png-disk (texture filename)
  "NOTE: cl-png, builds a lisp array, so we can rely on cpu side tone/color/value mapping"
  (let* ((f          (range-function-for-texture texture))
         (dimensions (dimensions texture))
         (width      (first  dimensions))
         (height     (second dimensions))
         (png-image  (png:make-image height width 3 16)))
    (with-free carray (pull1-g (texref texture))
      (loop :for h :below height
            :for hw :from (1- height) :downto 0
            :do (loop :for w :below width
                      :for color4 := (aref-c carray w hw)
                      :do (setf (aref png-image h w 0) (funcall f (x color4))
                                (aref png-image h w 1) (funcall f (y color4))
                                (aref png-image h w 2) (funcall f (z color4))))))
    (with-open-file (stream filename :element-type '(unsigned-byte 16)
                                     :direction :output
                                     :if-exists :supersede)
      (png:encode png-image stream))))

(defun filename-extension (filename)
  "returns the extension of filename, lowecased"
  (let ((filestring (if (pathnamep filename)
                        (file-namestring filename)
                        filename)))
    (alexandria:lastcar (cl-ppcre:split "[.]" (string-downcase filestring)))))

;;--------------------------------------------------

(defun scene-to-cubemap (dst-cubemap &key (camera *camera-cubemap*)
                                          (pos (v! 0 0 0)))
  "Fills the provided DST-CUBEMAP texture with render of
   current scene. Dimensions of the DST-CUBEMAP adviced as 2048x2048"
  (assert (eq :RGB16F (element-type (texref dst-cubemap))))
  (assert (texture-cubes-p dst-cubemap))
  (let ((dimensions (dimensions dst-cubemap)))
    (assert (apply #'= dimensions))
    (setf (resolution (current-viewport)) (v! dimensions))
    (setf (fov camera) 90f0)
    (setf (pos camera) pos)
    (with-free* ((bs              (make-buffer-stream nil :primitive :points))
                 (external-fbo    (make-fbo `(0  :dimensions ,dimensions :element-type :rgb16f)
                                            `(:d :dimensions ,dimensions)))
                 (external-sample (sample (attachment-tex external-fbo 0)))
                 (fbo             (make-fbo `(0 :dimensions ,dimensions :element-type :rgb16f)))
                 (pipeline        (pipeline-g (:points)
                                    :fragment
                                    (lambda-g ((uv :vec2) &uniform (sam :sampler-2d))
                                      (let ((color (s~ (texture sam uv) :xyz)))
                                        (v! color 1))))))
      (loop :for rotation :in *cubemap-rotations*
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

(defun scene-to-cubemap-disk (filename &key (camera *camera-cubemap*)
                                            (dim '(250 250))
                                            (pos (v! 0 0 0)))
  "Save the current scene in *ACTORS* into 6 images. This will need to render the
   scene 6 times from the perspective of CAMERA at different fixed rotations.

   Supports PNG, BMP, TGA, DDS."
  (declare (type rtg-math.types:vec3 pos))
  (assert (apply #'= dim))
  (assert (length= dim 2))
  (setf (resolution (current-viewport)) (v! dim))
  (setf (pos camera) pos)
  (setf (fov camera) 90f0)
  (let* ((file-pathname (resolve-path filename nil))
         (f (filename-to-save-function file-pathname)))
    (with-free* ((bs              (make-buffer-stream nil :primitive :points))
                 (external-fbo    (make-fbo `(0  :dimensions ,dim :element-type :rgb16f)
                                            `(:d :dimensions ,dim)))
                 (external-sample (sample (attachment-tex external-fbo 0)))
                 (fbo             (make-fbo `(0 :dimensions ,dim)))
                 (pipeline        (pipeline-g (:points)
                                    :fragment
                                    (lambda-g ((uv :vec2) &uniform (sam :sampler-2d))
                                      (let* ((final-color (s~ (texture sam uv) :xyz))
                                             (ldr (tone-map-reinhard final-color *exposure*))
                                             (luma (rgb->luma-bt601 ldr)))
                                        (v! ldr luma))))))
      (loop :for side :in *cubemap-sides*
            :for rotation :in *cubemap-rotations*
            :do (destructuring-bind (up from to) rotation
                  (setf (rot camera) (q:look-at up from to)))
                ;; Normal draw - preferably a 16bit fbo to avoid dithering
                (with-fbo-bound (external-fbo)
                  (clear external-fbo)
                  (loop :for actor :in *actors*
                        :do (draw actor camera 1f0)))
                ;; Final draw to LDR (? the colors
                (map-g-into fbo pipeline bs
                            :sam external-sample)
                ;; Read from our created fbo texture
                (funcall f
                         (attachment-tex fbo 0)
                         (format nil "cubemap_~a_~a.~a"
                                 (pathname-name file-pathname)
                                 side
                                 (pathname-type file-pathname)))))))

(defun screen-to-disk (filename)
  "take what is already rendered on the main fbo and save it"
  (let ((f (filename-to-save-function filename)))
    (funcall f
             (attachment-tex *fbo* 0)
             filename)))
