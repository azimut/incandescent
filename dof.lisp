(in-package #:incandescent)

;; Depth of Field - 5 Passes!
;;
;; - https://catlikecoding.com/unity/tutorials/advanced-rendering/depth-of-field/
;;
;; TODO: postprocess pass to do a tent-filter on the halfed fbo
;;       looks good enough for me in the low res i work

(defparameter *bokeh-radius* 1f0
  "sets the resolution of the bokeh effect
   Default: 4f0
   Range:   1f0 - 10f0")
(defparameter *coc-distance* 1f0
  "distance between the camera and the focus plane
   where everything is perfectly sharp
   Default: 10f0
   Range: .1 - 100")
(defparameter *coc-range* 4f0
  "Default: 3f0
   Range: .1 - 10")

(defvar *dof-combine-fbo* NIL)
(defvar *dof-combine-sam* NIL)

(defvar *bokeh-fbo*       NIL)
(defvar *bokeh-sam*       NIL)
(defvar *bokeh-h-fbo*     NIL)
(defvar *bokeh-h-sam*     NIL)

(defvar *coc-fbo*         NIL)
(defvar *coc-sam*         NIL)
(defvar *coc-h-fbo*       NIL)
(defvar *coc-h-sam*       NIL)

(defvar *texel-size*      NIL "stores the 1/width and 1/height vec2")
(defvar *half-texel-size* NIL)
(defvar *dof-kernel*      NIL "C-array")

;; Holds the hardcoded values for the kernel on the frag shader
(defstruct-g sample-kernel
  (k (:vec2 16)))

;;--------------------------------------------------
;; Other kernels at:
;; https://github.com/Unity-Technologies/PostProcessing/blob/v2/PostProcessing/Shaders/Builtins/DiskKernels.hlsl
(defparameter *kernel*
  (list (v! 0 0)
        (v! 0.54545456 0)
        (v! 0.16855472 0.5187581)
        (v! -0.44128203 0.3206101)
        (v! -0.44128197 -0.3206102)
        (v! 0.1685548 -0.5187581)
        (v! 1 0)
        (v! 0.809017 0.58778524)
        (v! 0.30901697 0.95105654)
        (v! -0.30901703 0.9510565)
        (v! -0.80901706 0.5877852)
        (v! -1 0)
        (v! -0.80901694 -0.58778536)
        (v! -0.30901664 -0.9510566)
        (v! 0.30901712 -0.9510565)
        (v! 0.80901694 -0.5877853)))

(defun free-dof ()
  (when *dof-combine-fbo* (free *dof-combine-fbo*))
  (when *bokeh-h-fbo*     (free *bokeh-h-fbo*))
  (when *dof-kernel*      (free *dof-kernel*))
  (when *bokeh-fbo*       (free *bokeh-fbo*))
  (when *coc-fbo*         (free *coc-fbo*))
  (when *coc-h-fbo*       (free *coc-h-fbo*)))

(defun init-dof ()
  (free-dof)
  (let ((half-dimensions (mapcar (op (floor (/ _ 2f0))) *dimensions*)))
    ;; https://forum.unity.com/threads/_maintex_texelsize-whats-the-meaning.110278/
    (setf *texel-size*      (v! (/ (nth 0 *dimensions*))
                                (/ (nth 1 *dimensions*))))
    (setf *half-texel-size* (v2:/s *texel-size* 2f0))
    ;;
    (setf *dof-kernel*      (make-c-array *kernel* :dimensions 16 :element-type :vec2))
    ;;
    (setf *coc-fbo*         (make-fbo `(0 :element-type :r16f :dimensions ,*dimensions*)))
    (setf *coc-sam*         (sample    (attachment-tex *coc-fbo* 0) :wrap :clamp-to-edge))
    ;;
    (setf *coc-h-fbo*       (make-fbo `(0 :element-type :rgba16f :dimensions ,half-dimensions)))
    (setf *coc-h-sam*       (sample    (attachment-tex *coc-h-fbo* 0) :wrap :clamp-to-edge))
    (setf *bokeh-fbo*       (make-fbo `(0 :element-type :rgba16f :dimensions ,half-dimensions)))
    (setf *bokeh-sam*       (sample    (attachment-tex *bokeh-fbo* 0) :wrap :clamp-to-edge))
    (setf *bokeh-h-fbo*     (make-fbo `(0 :element-type :rgba16f :dimensions ,half-dimensions)))
    (setf *bokeh-h-sam*     (sample    (attachment-tex *bokeh-h-fbo* 0) :wrap :clamp-to-edge))
    ;;
    (setf *dof-combine-fbo* (make-fbo `(0 :element-type :rgba16f :dimensions ,*dimensions*)))
    (setf *dof-combine-sam* (sample    (attachment-tex *bokeh-h-fbo* 0) :wrap :clamp-to-edge))
    t))

;;--------------------------------------------------
(defun draw-dof (&optional (sam *sam*) (samd *samd*))
  (declare (type cepl:sampler sam samd))
  (with-setf* ((cull-face) nil
               (depth-mask) nil
               (depth-test-function) #'always)
    (with-fbo-bound (*coc-fbo*)
      (clear *coc-fbo*)
      (map-g #'coc-pipe *bs*
             :samd samd
             :bokeh-radius *bokeh-radius*
             :focus-distance *coc-distance*
             :focus-range *coc-range*))
    (with-fbo-bound (*coc-h-fbo*)
      (clear-fbo *coc-h-fbo*)
      (map-g #'coc-prefilter-pipe *bs*
             :sam sam
             :coc-sam *coc-sam*
             :texel-size *texel-size*))
    (with-fbo-bound (*bokeh-h-fbo*)
      (clear *bokeh-h-fbo*)
      (map-g #'bokeh-pipe *bs*
             :scene *coc-h-sam*
             :bokeh-radius *bokeh-radius*
             :dof-kernel   *dof-kernel*
             :texel-size   *half-texel-size*))
    (with-fbo-bound (*bokeh-fbo*)
      (clear *bokeh-fbo*)
      (map-g #'bokeh-postfilter-pipe *bs*
             :sam *bokeh-h-sam*
             :texel-size *half-texel-size*))
    (with-fbo-bound (*dof-combine-fbo*)
      (clear *dof-combine-fbo*)
      (map-g #'dof-combine-pipe *bs*
             :sam sam
             :coc-sam *coc-sam*
             :coc-h-sam *coc-h-sam*))))
;;--------------------------------------------------
;; Circle of confusion - CoC
;; "determines the strength of the bokeh effect per point"

(defun-g coc-frag ((uv :vec2) &uniform (samd :sampler-2d)
                   (bokeh-radius   :float)
                   (focus-distance :float)
                   (focus-range    :float))
  (let* ((depth (x (texture samd uv)))
         (depth (linear-eye-depth depth))
         (coc   (/ (- depth focus-distance)
                   focus-range))
         (coc   (* (clamp coc -1 1)
                   bokeh-radius)))
    coc))

(defpipeline-g coc-pipe (:points)
  :fragment (coc-frag :vec2))

;;--------------------------------------------------
;; Circle of confusion - CoC

(defun-g coc-weight ((c :vec3))
  (/ 1f0 (+ 1f0 (max (max (x c) (y c)) (z c)))))

(defun-g coc-prefilter-frag ((uv :vec2) &uniform
                             (texel-size :vec2)
                             (sam        :sampler-2d)
                             (coc-sam    :sampler-2d))
  (let* ((o       (* (s~ texel-size :xyxy)
                     (v! -.5 -.5 .5 .5)))
         (s0      (s~ (texture sam (+ uv (s~ o :xy))) :xyz))
         (s1      (s~ (texture sam (+ uv (s~ o :zy))) :xyz))
         (s2      (s~ (texture sam (+ uv (s~ o :xw))) :xyz))
         (s3      (s~ (texture sam (+ uv (s~ o :zw))) :xyz))
         (w0      (coc-weight s0))
         (w1      (coc-weight s1))
         (w2      (coc-weight s2))
         (w3      (coc-weight s3))
         (color   (/ (+ (* s0 w0)
                        (* s1 w1)
                        (* s2 w2)
                        (* s3 w3))
                     (max (+ w0 w1 w2 s3) .00001))) ;; s3?
         (coc0    (x (texture coc-sam (+ uv (s~ o :xy)))))
         (coc1    (x (texture coc-sam (+ uv (s~ o :zy)))))
         (coc2    (x (texture coc-sam (+ uv (s~ o :xw)))))
         (coc3    (x (texture coc-sam (+ uv (s~ o :zw)))))
         ;;
         (coc-min (min (min (min coc0 coc1) coc2) coc3))
         (coc-max (max (max (max coc0 coc1) coc2) coc3))
         (coc     (if (>= coc-max (- coc-min))
                      coc-max
                      coc-min)))
    (v! color coc)))

(defpipeline-g coc-prefilter-pipe (:points)
  :fragment (coc-prefilter-frag :vec2))

;;--------------------------------------------------
;; Bokeh - DOF
;; TODO: precompute the radius per sample on the kernel
(defun-g bokeh-weight ((coc :float) (radius :float))
  (saturate (/ (+ 2 (- coc radius)) 2)))
(defun-g bokeh-frag ((uv :vec2)
                     &uniform
                     (dof-kernel (:vec2 16))
                     (bokeh-radius :float)
                     (scene :sampler-2d)
                     (texel-size :vec2))
  (let ((coc (w (texture scene uv)))
        (bg-color (v! 0 0 0))
        (fg-color (v! 0 0 0))
        (bg-weight 0f0)
        (fg-weight 0f0))
    (dotimes (k 16)
      (let* ((o (* (aref dof-kernel k)
                   bokeh-radius))
             (radius (length o))
             (o (* o texel-size))
             (s (texture scene (+ uv o)))
             (bgw (bokeh-weight (max 0 (min (w s) coc)) radius))
             (fgw (bokeh-weight (- (w s)) radius)))
        (incf bg-color (* (s~ s :xyz) bgw))
        (incf bg-weight bgw)
        (incf fg-color (* (s~ s :xyz) fgw))
        (incf fg-weight fgw)))
    (setf bg-color (* bg-color (/ 1f0 bg-weight)))
    (setf fg-color (* fg-color (/ 1f0 fg-weight)))
    (let* ((bgfg  (min 1 (/ (* fg-weight +pi+) 16)))
           (color (mix bg-color fg-color bgfg)))
      (v! color bgfg))))

(defpipeline-g bokeh-pipe (:points)
  :fragment (bokeh-frag :vec2))

;;--------------------------------------------------

(defun-g bokeh-postfilter-frag ((uv :vec2)
                                &uniform
                                (sam :sampler-2d)
                                (texel-size :vec2))
  (let* ((o (* (s~ texel-size :xyxy)
               (v! -.5 -.5 .5 .5)))
         (s (+ (texture sam (+ uv (s~ o :xy)))
               (texture sam (+ uv (s~ o :zy)))
               (texture sam (+ uv (s~ o :xw)))
               (texture sam (+ uv (s~ o :zw))))))
    (* s .25)))

(defpipeline-g bokeh-postfilter-pipe (:points)
  :fragment (bokeh-postfilter-frag :vec2))

;;--------------------------------------------------
(defun-g dof-combine-frag ((uv :vec2)
                           &uniform
                           (sam :sampler-2d)
                           (coc-sam :sampler-2d)
                           (coc-h-sam :sampler-2d)) ;; dof-sam
  (let* ((source (texture sam uv))
         (coc    (x (texture coc-sam uv)))
         (dof    (texture coc-h-sam uv))
         (dof-strength (smoothstep .1 1 (abs coc)))
         (color (mix (s~ source :xyz)
                     (s~ dof    :xyz)
                     (- (+ dof-strength (w dof))
                        (* dof-strength (w dof))))))
    (v! color (w source))))

(defpipeline-g dof-combine-pipe (:points)
  :fragment (dof-combine-frag :vec2))
