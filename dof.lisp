(in-package :incandescent)

;; Depth of Field - 5 Passes!
;;
;; - https://catlikecoding.com/unity/tutorials/advanced-rendering/depth-of-field/
;;
;; TODO: postprocess pass to do a tent-filter on the halfed fbo
;;       looks good enough for me in the low res i work

(defvar *bokeh-fbo*   NIL)
(defvar *bokeh-sam*   NIL)
(defvar *bokeh-h-fbo* NIL)
(defvar *bokeh-h-sam* NIL)

(defparameter *bokeh-radius* 4f0
  "sets the resolution of the bokeh effect
   Default: 4f0
   Range:   1f0 - 10f0")

(defvar *coc-fbo*     NIL)
(defvar *coc-sam*     NIL)
(defvar *coc-h-fbo*   NIL)
(defvar *coc-h-sam*   NIL)

(defvar *texel-size*  NIL "stores the 1/width and 1/height vec2")
(defvar *dof-kernel*  NIL)

(defparameter *coc-distance* 10f0
  "distance between the camera and the focus plane
   where everything is perfectly sharp
   Default: 10f0
   Range: .1 - 100")
(defparameter *coc-range* 3f0
  "Default: 3f0
   Range: .1 - 10f0")

;; Holds the hardcoded values for the kernel on the frag shader
(defstruct-g sample-kernel
  (k (:vec2 16)))

(defun init-dof ()
  ;; https://forum.unity.com/threads/_maintex_texelsize-whats-the-meaning.110278/
  (setf *texel-size* (v! (/ (nth 0 *dimensions*))
                         (/ (nth 1 *dimensions*))))
  ;; Other kernels at:
  ;; https://github.com/Unity-Technologies/PostProcessing/blob/v2/PostProcessing/Shaders/Builtins/DiskKernels.hlsl
  (unless *dof-kernel*
    (setf *dof-kernel* (make-c-array (list (v! 0 0)
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
                                           (v! 0.80901694 -0.5877853))
                                     :dimensions 16 :element-type :vec2)))
  (unless *coc-fbo*
    (setf *coc-fbo* (make-fbo '(0 :element-type :r16f)))
    (setf *coc-sam* (sample (attachment-tex *coc-fbo* 0)
                            :wrap :clamp-to-edge)))
  (unless *coc-h-fbo*
    (setf *coc-h-fbo*
          (make-fbo `(0 :element-type :r16f
                        :dimensions ,(list (floor (/ (first  *dimensions*) 2f0))
                                           (floor (/ (second *dimensions*) 2f0))))))
    (setf *coc-h-sam*
          (sample (attachment-tex *coc-fbo* 0)
                  :wrap :clamp-to-edge)))
  (unless *bokeh-fbo*
    (setf *bokeh-fbo* (make-fbo '(0 :element-type :rgba16f)))
    (setf *bokeh-sam* (sample (attachment-tex *bokeh-fbo* 0)
                              :wrap :clamp-to-edge))
    (setf *bokeh-h-fbo*
          (make-fbo `(0 :element-type :rgba16f
                        :dimensions ,(list (floor (/ (first  *dimensions*) 2f0))
                                           (floor (/ (second *dimensions*) 2f0))))))
    (setf *bokeh-h-sam*
          (sample (attachment-tex *bokeh-h-fbo* 0) :wrap :clamp-to-edge))))

;;--------------------------------------------------
(defun draw-dof (sam samd)
  (declare (type cepl:sampler sam samd))
  (with-setf* ((cull-face) nil
               (depth-mask) nil
               (depth-test-function) #'always)
    (with-fbo-bound (*coc-fbo*)
      (map-g #'coc-pipe *bs*
             :samd samd
             :bokeh-radius *bokeh-radius*
             :focus-distance *coc-distance*
             :focus-range *coc-range*))
    (with-fbo-bound (*coc-h-fbo*)
      (map-g #'coc-prefilter-pipe *bs*
             :texel-size *texel-size*
             :sam sam
             :coc-sam *coc-sam*))
    (with-fbo-bound (*bokeh-h-fbo*)
      (clear *bokeh-h-fbo*)
      (map-g #'bokeh-pipe *bs*
             :scene sam
             :bokeh-radius *bokeh-radius*
             :dof-kernel *dof-kernel*
             :texel-size *texel-size*))
    (with-fbo-bound (*bokeh-fbo*)
      (clear *bokeh-fbo*)
      (map-g #'pass-pipe *bs*
             :sam *bokeh-h-sam*))))
;;--------------------------------------------------
;; Circle of confusion - CoC
;; "determines the strength of the bokeh effect per point"

(defun-g coc-frag ((uv :vec2)
                   &uniform
                   (samd :sampler-2d)
                   (bokeh-radius :float)
                   (focus-distance :float)
                   (focus-range :float))
  (let* ((depth (x (texture samd uv)))
         (depth (linear-eye-depth depth))
         (coc (/ (- depth focus-distance)
                 focus-range))
         (coc (* (clamp coc -1 1)
                 bokeh-radius)))
    coc))

(defpipeline-g coc-pipe (:points)
  :fragment (coc-frag :vec2))

;;--------------------------------------------------
;; Circle of confusion - CoC

(defun-g coc-prefilter-frag ((uv :vec2)
                             &uniform
                             (sam :sampler-2d)
                             (coc-sam :sampler-2d)
                             (texel-size :vec2))
  (let* ((o       (* (s~ texel-size :xyxy)
                     (v! -.5 -.5 .5 .5)))
         (coc0    (x (texture coc-sam (+ uv (s~ o :xy)))))
         (coc1    (x (texture coc-sam (+ uv (s~ o :zy)))))
         (coc2    (x (texture coc-sam (+ uv (s~ o :xw)))))
         (coc3    (x (texture coc-sam (+ uv (s~ o :zw)))))
         (coc-min (min (min (min coc0 coc1) coc2) coc3))
         (coc-max (max (max (max coc0 coc1) coc2) coc3))
         (coc     (if (>= coc-max (- coc-min))
                      coc-max
                      coc-min)))
    (v! (s~ (texture sam uv) :xyz) coc)))

(defpipeline-g coc-prefilter-pipe (:points)
  :fragment (coc-prefilter-frag :vec2))

;;--------------------------------------------------
;; Bokeh - DOF
(defun-g bokeh-frag ((uv :vec2)
                     &uniform
                     (dof-kernel (:vec2 16))
                     (bokeh-radius :float)
                     (scene :sampler-2d)
                     (texel-size :vec2))
  (let ((color (v! 0 0 0)))
    (dotimes (k 16)
      (let* ((o (* (aref dof-kernel k) bokeh-radius))
             (radius (length o)))
        (multf o texel-size)
        (incf color (s~ (texture scene (+ uv o)) :xyz))))
    (v! (* color (/ 1f0 16))
        1)))

(defpipeline-g bokeh-pipe (:points)
  :fragment (bokeh-frag :vec2))

(defun free-dof ()
  (when *bokeh-h-fbo*
    (free *bokeh-h-fbo*)
    (setf *bokeh-h-fbo* NIL))
  (when *dof-kernel*
    (free *dof-kernel*)
    (setf *dof-kernel* NIL))
  (when *bokeh-fbo*
    (free *bokeh-fbo*)
    (setf *bokeh-fbo* NIL))
  (when *coc-fbo*
    (free *coc-fbo*)
    (setf *coc-fbo* NIL))
  (when *coc-h-fbo*
    (free *coc-h-fbo*)
    (setf *coc-h-fbo* NIL)))
