(in-package #:incandescent)

;; make-cubemap-tex
;;
;; make-scene-cubemap
;; free-scene-cubemap
;; init-render-cubemap
;;
;; scene-to-cubemap
;; scene-to-cubemap-disk
;; texture-to-disk
;; screen-to-disk

;; THE CAMERA
(defvar *camera-cubemap*
  (make-instance 'pers
                 :name :cubemap-cam
                 :fov 90f0))

;;--------------------------------------------------

;; Reference: Cubemap generation mostly from
;; https://learnopengl.com/PBR/IBL/Diffuse-irradiance
(defparameter *cubemap-sides* '("right" "left"
                                "top"   "bottom"
                                "front" "back"))
(defparameter *cubemap-rotations*
  (list
   (list (v! 0 1 0) (v! 0 0 0) *vec3-right*)
   (list (v! 0 1 0) (v! 0 0 0) *vec3-left*)
   (list (v! 0 1 0) (v! 0 0 0) *vec3-down*)
   (list (v! 0 1 0) (v! 0 0 0) *vec3-up*)
   (list (v! 0 1 0) (v! 0 0 0) *vec3-forward*)
   (list (v! 0 1 0) (v! 0 0 0) *vec3-back*)))

;;--------------------------------------------------
;; Constructors

(defun make-cubemap-tex (&rest paths)
  "Returns a gpu texture FROM the provided images"
  (assert (= 6 (length paths)))
  (with-c-arrays-freed
      (ca (mapcar
           (lambda (p)
             (dirt:load-image-to-c-array
              (asdf:system-relative-pathname :incandescent p)))
           paths))
    (make-texture ca :element-type :rgb8 :cubes t)))

;;--------------------------------------------------

(defvar *cube-tex* NIL)
(defvar *cube-sam* NIL)

(defun make-scene-cubemap (&key (camera *camera-cubemap*)
                                (dimensions '(2048 2048))
                                (pos (v! 0 0 0)))
  "returns a NEW cubemap from the current scene"
  (assert (apply #'= dimensions))
  (let ((dst-cubemap (make-texture NIL :cubes T
                                       :dimensions dimensions
                                       :element-type :rgb16f)))
    (scene-to-cubemap dst-cubemap :camera camera :pos pos)
    dst-cubemap))

(defun free-scene-cubemap ()
  (when *cube-tex* (free *cube-tex*))
  (setf *cube-tex* NIL))

(defun init-render-cubemap ()
  (free-scene-cubemap)
  (setf *cube-tex* (make-render-cubemap))
  (setf *cube-sam* (sample *cube-tex* :wrap :clamp-to-edge
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
    ("png" #'texture-to-png)
    ("bmp" #'dirt:save-as-image)
    ("tga" #'dirt:save-as-image)
    ("dds" #'dirt:save-as-image)))

(defun texture-to-png (texture filename)
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

(defun texture-to-disk (filename texture)
  (declare (type cepl:texture texture))
  (let ((f (filename-to-save-function filename)))
    (funcall f texture filename)))

(defun screen-to-disk (filename)
  "take what is already rendered on the main fbo and save it"
  (texture-to-disk filename (attachment-tex *fbo* 0)))
