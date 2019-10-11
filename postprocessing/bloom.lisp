(in-package #:incandescent)

;; INPUTS:
;; - *dimensions*
;; - sampler of only the scene brigther stuff
;;
;; Reference:
;; https://learnopengl.com/Advanced-Lighting/Bloom
;; https://catlikecoding.com/unity/tutorials/advanced-rendering/bloom/
(defvar *bloom-fbo* NIL)
(defvar *bloom-blend*
  (make-blending-params :source-rgb :one
                        :destination-rgb :one))

(defstruct (bloom-fbo (:constructor %make-bloom-fbo))
  (fbos     (make-array 5))
  (samplers (make-array 5))
  (widths   (make-array 5 :element-type 'single-float))
  (heights  (make-array 5 :element-type 'single-float)))

(defmethod free-bloom ()
  (when *bloom-fbo*
    (map NIL #'free (bloom-fbo-fbos *bloom-fbo*)))
  (setf *bloom-fbo* NIL))

(defun make-bloom-fbo ()
  (flet ((f (d) (mapcar (lambda (x) (floor (/ x d))) *dimensions*)))
    (let ((obj (%make-bloom-fbo)))
      (loop :for div    :in '(1 2 4 8 16)
            :for width  := (nth 0 (f div))
            :for height := (nth 1 (f div))
            :for i :from 0
            :do (let* ((fbo (make-fbo `(0 :dimensions ,(f div)
                                          :element-type :rgba16f)))
                       (sam (sample (attachment-tex fbo 0)
                                    :wrap :clamp-to-edge)))
                  (setf (aref (bloom-fbo-widths   obj) i) (/ (coerce width  'single-float)))
                  (setf (aref (bloom-fbo-heights  obj) i) (/ (coerce height 'single-float)))
                  (setf (aref (bloom-fbo-fbos     obj) i) fbo)
                  (setf (aref (bloom-fbo-samplers obj) i) sam)))
      obj)))

(defun init-bloom ()
  (free-bloom)
  (setf *bloom-fbo* (make-bloom-fbo)))

(defun draw-bloom (sam)
  "SAM is a sampler of a fbo with ONLY the bright parts
   the final blur is at *BLOOM-FBO* index 0"
  (declare (type cepl:sampler sam))
  (let ((fbos     (bloom-fbo-fbos     *bloom-fbo*))
        (samplers (bloom-fbo-samplers *bloom-fbo*))
        (widths   (bloom-fbo-widths   *bloom-fbo*))
        (heights  (bloom-fbo-heights  *bloom-fbo*)))
    (with-setf* ((depth-test-function) #'always
                 (depth-mask)  NIL
                 (cull-face)   NIL
                 (clear-color) (v! 0 0 0 1))
      ;; downsampling
      (with-fbo-bound ((aref fbos 1))
        ;;(clear (aref fbos 1))
        (map-g #'blur-pipe *bs*
               :delta 1f0
               :sam sam
               :x (aref widths  0)
               :y (aref heights 0)))
      (dolist (src '(1 2 3))
        (declare (type fixnum src))
        (with-fbo-bound ((aref fbos (1+ src)))
          ;;(clear (aref fbos (1+ src)))
          (map-g #'blur-pipe *bs*
                 :sam (aref samplers src)
                 :x   (aref widths   src)
                 :y   (aref heights  src))))
      ;; upscaling
      (dolist (dst '(3 2 1 0))
        (declare (type fixnum dst))
        (with-blending *bloom-blend*
          (with-fbo-bound ((aref fbos dst))
            (clear-fbo (aref fbos dst))
            (map-g #'blur-pipe *bs*
                   :sam (aref samplers (+ 1 dst))
                   :x   (aref widths   (+ 1 dst))
                   :y   (aref heights  (+ 1 dst))
                   :delta .5f0)))))))

;;--------------------------------------------------
;; 2D - Blur

(defun-g sample-box ((uv    :vec2)
                     (delta :float)
                     (sam   :sampler-2d)
                     (x :float)
                     (y :float))
  (let* ((texture-size (texture-size sam 0))
         (o (* (v! x y x y)
               (v! (- delta) (- delta) delta delta)))
         (s (+ (texture sam (+ uv (s~ o :xy)))
               (texture sam (+ uv (s~ o :zy)))
               (texture sam (+ uv (s~ o :xw)))
               (texture sam (+ uv (s~ o :zw))))))
    (* s .25)))

(defun-g blur-frag ((uv :vec2)
                    &uniform
                    (delta :float)
                    (sam :sampler-2d)
                    (x :float)
                    (y :float))
  (let ((color (sample-box uv delta sam x y)))
    color))

(defpipeline-g blur-pipe (:points)
  :fragment (blur-frag :vec2))

;;----------------------------------------
;; 2D - Bloom

(defun-g bloom-frag ((uv :vec2)
                     &uniform
                     (light-sam :sampler-2d)
                     (sam :sampler-2d)
                     (delta :float)
                     (x :float)
                     (y :float))
  (let* ((c (texture light-sam uv))
         (c (v! (+ (s~ (sample-box uv delta sam x y) :xyz)
                   (s~ c :xyz))
                (w c))))
    c))

(defpipeline-g dobloom-pipe (:points)
  :fragment (bloom-frag :vec2))
