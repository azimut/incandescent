(in-package :incandescent)

;; Depth of Field
;;
;; - https://catlikecoding.com/unity/tutorials/advanced-rendering/depth-of-field/

(defvar *dof-fbo* NIL)
(defvar *dof-sam* NIL)
(defvar *bokeh-fbo* NIL)
(defvar *bokeh-sam* NIL)
(defvar *coc-fbo* NIL)
(defvar *coc-sam* NIL)
(defvar *texel-size* NIL)

(defparameter *coc-distance* 10f0
  "distance between the camera and the focus plane
   where everything is perfectly sharp
   Range: .1 - 100")
(defparameter *coc-range* 3f0
  "Range: .1 - 10f0")

(defun free-dof ()
  (when *dof-fbo* (free *dof-fbo*))
  (setf *dof-fbo* NIL)
  (when *bokeh-fbo* (free *bokeh-fbo*))
  (setf *bokeh-fbo* NIL)
  (when *coc-fbo* (free *coc-fbo*))
  (setf *coc-fbo* NIL))

(defun init-dof ()
  ;; https://forum.unity.com/threads/_maintex_texelsize-whats-the-meaning.110278/
  (setf *texel-size* (v! (/ (nth 0 *dimensions*))
                         (/ (nth 1 *dimensions*))))
  (unless *coc-fbo*
    (setf *coc-fbo* (make-fbo (list 0 :element-type :r16f)))
    (setf *coc-sam* (sample (attachment-tex *coc-fbo* 0)
                            :wrap :clamp-to-edge)))
  (unless *bokeh-fbo*
    (setf *bokeh-fbo* (make-fbo (list 0 :element-type :rgba16f)))
    (setf *bokeh-sam* (sample (attachment-tex *bokeh-fbo* 0)
                              :wrap :clamp-to-edge))))

;;--------------------------------------------------
(defun draw-dof (sam samd)
  (declare (type cepl:sampler sam samd))
  (with-setf* ((cull-face) nil
               (depth-mask) nil
               (depth-test-function) #'always)
    ;; (with-fbo-bound (*coc-fbo*)
    ;;   (map-g #'coc-pipe *bs*
    ;;          :samd samd
    ;;          :focus-distance *coc-distance*
    ;;          :focus-range *coc-range*))
    (with-fbo-bound (*bokeh-fbo*)
      (map-g #'bokeh-pipe *bs*
             :scene sam
             :texel-size *texel-size*))))
;;--------------------------------------------------
;; Circle of confusion - CoC
;; "determines the strength of the bokeh effect per point"

(defun-g coc-frag ((uv :vec2)
                   &uniform
                   (samd :sampler-2d)
                   (focus-distance :float)
                   (focus-range :float))
  (let* ((depth (x (texture samd uv)))
         (depth (linear-eye-depth depth))
         (coc (/ (- depth focus-distance)
                 focus-range))
         (coc (clamp coc -1 1)))
    coc))

(defpipeline-g coc-pipe (:points)
  :fragment (coc-frag :vec2))

;;--------------------------------------------------
;; Bokeh - DOF
(defun-g bokeh-frag ((uv :vec2)
                     &uniform
                     (scene :sampler-2d)
                     (texel-size :vec2))
  (let ((color (v! 0 0 0))
        (weight 0))
    (for (u -4) (<= u 4) (++ u)
         (for (v -4) (<= v 4) (++ v)
              (let ((o (* (v! u v) (* texel-size 2)))) ;; 2x
                ;; (when (<= (length o) 4)
                ;;   ;;(multf o (* texel-size 2))
                ;;   )
                (incf color (s~ (texture scene (+ uv o)) :xyz))
                ;;(incf weight 1)
                )))
    ;; (v! (* color (/ 1f0 weight))
    ;;     1)
    (v! (* color (/ 1f0 81)) 1)
    ;;(texture scene uv)
    )
  )

(defpipeline-g bokeh-pipe (:points)
  :fragment (bokeh-frag :vec2))
