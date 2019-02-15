(in-package #:incandescent)

;; Requirements:
;; INPUTS
;; - Sampler of depth from a fbo
;; - Sampler of normals from a fbo output
;; - *dimensions*
;; OUTPUTS
;; - An fbo to render to and be captured
;;
;; References:
;; - https://github.com/McNopper/OpenGL/blob/master/Example28/shader/ssao.frag.glsl
;; - http://ogldev.atspace.co.uk/www/tutorial46/tutorial46.html
;; - https://learnopengl.com/Advanced-Lighting/SSAO

(defvar *fbo-ssbo* NIL)
(defvar *sam-ssbo* NIL)

(defvar *noise-tex*  NIL)
(defvar *noise-sam*  NIL)
(defvar *ubo-kernel* NIL)

;; UBO
(defstruct-g (random-kernel :layout :std-140)
  (random-v3 (:vec3 64)))

(defun generate-rotation-kernel ()
  "runs once, goes to a TEXTURE, 4x4x3"
  (loop :repeat 4 :collect
       (loop :repeat 4 :collect
            (v! (1- (* 2 (random 1f0)))
                (1- (* 2 (random 1f0)))
                0f0))))

(defun generate-sample-kernel ()
  "runs once, goes to an UBO, 64x3"
  (loop :for i :below 64 :collect
       (let* ((sample (v! (1- (* 2 (random 1f0)))
                          (1- (* 2 (random 1f0)))
                          (random 1f0)))
              (sample (v3:normalize sample))
              (sample (v3:*s sample (random 1f0)))
              (scale (/ i 64f0))
              (scale (lerp .1 1f0 (* scale scale))))
         (v3:*s sample scale))))

;;--------------------------------------------------

(defun init-ssao ()
  ;; SSAO - Random kernel rotations - generate noise texture
  (unless *noise-tex*
    (setf *noise-tex*
          (make-texture (generate-rotation-kernel)
                        :element-type :rgb32f))
    (setf *noise-sam*
          (sample *noise-tex*
                  :minify-filter  :nearest
                  :magnify-filter :nearest)))
  ;; SSAO - samples
  (unless *ubo-kernel*
    (setf *ubo-kernel*
          (make-ubo (list (generate-sample-kernel))
                    'random-kernel)))
  ;; OUTPUTS
  (when *fbo-ssbo* (free *fbo-ssbo*))
  (setf *fbo-ssbo* (make-fbo (list 0 :dimensions *dimensions*))
        *sam-ssbo* (sample (attachment-tex *fbo-ssbo* 0)
                           :wrap :clamp-to-edge))
  NIL)

;;--------------------------------------------------

(defun-g get-view-pos ((uv :vec2)
                       (g-depth :sampler-2d)
                       (world-view :mat4))
  (let* ((x (1- (* 2f0 (x uv))))
         (y (1- (* 2f0 (y uv))))
         (z (1- (* 2f0 (x (texture g-depth uv)))))
         (pos-proj (v! x y z 1))
         (pos-view (* (inverse world-view) pos-proj))
         (pos-view (/ pos-view (w pos-view))))
    pos-view))

(defun-g ssao-frag ((uv :vec2)
                    &uniform
                    (kernel :int)
                    (radius :float)
                    (kernel-effect :float)
                    ;;
                    (tex-noise :sampler-2d)
                    (random-kernel random-kernel :ubo)
                    ;;
                    (g-normal :sampler-2d)
                    (g-depth  :sampler-2d)
                    ;;
                    (res :vec2)
                    (view-clip :mat4))
  (let* ((pos-view (get-view-pos uv g-depth view-clip))
         (normal-view
          (normalize
           (1- (* 2 (s~ (texture g-normal uv) :xyz)))))
         (random-vector
          (normalize
           (1- (* 2 (s~ (texture tex-noise (* (/ res 4) uv)) :xyz)))))
         (tangent-view
          (normalize
           (- random-vector (* (dot random-vector normal-view)
                               normal-view))))
         (bitangent-view (cross normal-view tangent-view))
         (kernel-matrix  (mat3 tangent-view bitangent-view normal-view))
         (oclussion 0f0))
    (with-slots (random-v3) random-kernel
      (dotimes (i kernel)
        (let* ((sample-vector-view (* kernel-matrix (aref random-v3 (int i))))
               (sample-point-view  (+ pos-view (* radius (v! sample-vector-view 0))))
               (sample-point-ndc   (* view-clip sample-point-view))
               (sample-point-ndc   (/ sample-point-ndc
                                      (w sample-point-ndc)))
               (sample-point-uv    (+ .5 (* .5 (s~ sample-point-ndc :xy))))
               (z-scene-ndc        (1- (* 2 (x (texture g-depth sample-point-uv)))))
               (delta (- (z sample-point-ndc) z-scene-ndc)))
          (if (and (> delta .0001)
                   (< delta .005))
              (incf oclussion 1f0)))))
    (v! (vec3 (- 1 (/ oclussion (1- (* kernel-effect kernel)))))
        1)))

(defpipeline-g ssao-pipe (:points)
  :fragment (ssao-frag :vec2))

;;--------------------------------------------------

(defun draw-ssao (&key (n-kernels 10) (radius .1) (kernel-effect 1f0))
  (declare (type single-float radius kernel-effect)
           (type positive-fixnum n-kernels))
  (with-setf* ((depth-mask) nil
               (cull-face) nil
               (clear-color) (v! 0 0 0 1)
               (depth-test-function) #'always)
    (map-g #'ssao-pipe *bs*
           :g-normal *sam1*
           :g-depth  *samd*
           :kernel n-kernels
           :kernel-effect kernel-effect
           :radius radius
           :tex-noise *noise-sam*
           :random-kernel *ubo-kernel*
           :res (v! *dimensions*)
           :view-clip (projection *currentcamera*))))
