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
  (make-blending-params :source-rgb :one :destination-rgb :one))

(defstruct (bloom-fbo (:constructor %make-bloom-fbo))
  (fbos     (make-array 5))
  (samplers (make-array 5))
  (widths   (make-array 5))
  (heights  (make-array 5)))

(defmethod free-bloom ()
  (map NIL #'free (bloom-fbo-fbos *bloom-fbo*))
  (setf *bloom-fbo* NIL))

(defun make-bloom-fbo ()
  (flet ((f (d) (mapcar (lambda (x) (floor (/ x d))) *dimensions*)))
    (let ((obj (%make-bloom-fbo)))
      (loop :for div :in '(1 2 4 8 16)
            :for width := (nth 0 (f div))
            :for height := (nth 1 (f div))
            :for i :from 0
            :do (let* ((fbo (make-fbo `(0 :dimensions ,(f div)
                                          :element-type :rgba16f)))
                       (sam (sample (attachment-tex fbo 0)
                                    :wrap :clamp-to-edge)))
                  (setf (aref (bloom-fbo-widths obj) i)   (coerce width 'single-float))
                  (setf (aref (bloom-fbo-heights obj) i)  (coerce height 'single-float))
                  (setf (aref (bloom-fbo-fbos obj) i)     fbo)
                  (setf (aref (bloom-fbo-samplers obj) i) sam)))
      obj)))

(defun init-bloom ()
  (unless *bloom-fbo*
    (setf *bloom-fbo* (make-bloom-fbo))))

(defun draw-bloom (sam)
  "SAM is a sampler of a fbo with ONLY the bright parts
   the final blur is at *BLOOM-FBO* index 0"
  (declare (type cepl:sampler sam))
  (let ((fbos     (bloom-fbo-fbos     *bloom-fbo*))
        (samplers (bloom-fbo-samplers *bloom-fbo*))
        (widths   (bloom-fbo-widths   *bloom-fbo*))
        (heights  (bloom-fbo-heights  *bloom-fbo*)))
    (with-setf* ((depth-mask) NIL
                 (cull-face)  NIL
                 (clear-color) (v! 0 0 0 1)
                 (depth-test-function) #'always)
      ;;
      (with-fbo-bound ((aref fbos 1))
        (clear (aref fbos 1))
        (map-g #'blur-pipe *bs*
               :sam sam
               :x (aref widths  0)
               :y (aref heights 0)
               :delta 1f0))
      (dolist (i '(1 2 3))
        (with-fbo-bound ((aref fbos (1+ i)))
          (clear (aref fbos (1+ i)))
          (map-g #'blur-pipe *bs*
                 :sam (aref samplers i)
                 :x   (aref widths i)
                 :y   (aref heights i)
                 :delta 1f0)))
      ;;
      (dolist (i '(3 2 1 0))
        (with-blending *bloom-blend*
          (with-fbo-bound ((aref fbos i))
            (clear-fbo (aref fbos i))
            (map-g #'blur-pipe *bs*
                   :sam (aref samplers (1+ i))
                   :x   (aref widths   i)
                   :y   (aref heights  i)
                   :delta .5)))))))

;;--------------------------------------------------
;; 2D - Blur

(defun-g sample-box ((uv    :vec2)
                     (delta :float)
                     (sam   :sampler-2d))
  (let* ((texture-size (texture-size sam 0))
         (x (/ 1f0 (x texture-size)))
         (y (/ 1f0 (y texture-size)))
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
  (let ((color (sample-box uv delta sam)))
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
         (c (v! (+ (s~ (sample-box uv delta sam) :xyz)
                   (s~ c :xyz))
                (w c))))
    c))

(defpipeline-g dobloom-pipe (:points)
  :fragment (bloom-frag :vec2))
