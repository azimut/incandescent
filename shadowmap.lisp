(in-package #:incandescent)

;; Shadowmaps require additional information to go in/out from vertex shaders
;; so a lot of duplicate code is needed. (unless trial)
;;
;; UNIFORMS
;;
;; :shadowmap *shadow-sam*
;; :light-world (world->view *shadow-camera*)
;; :light-clip  (projection  *shadow-camera*)


(defvar *shadow-fbo* NIL)
(defvar *shadow-sam* NIL)
(defvar *shadow-dimensions* '(1024 1024))

(defparameter *shadow-camera*
  (make-instance 'orth
                 :name :shadow-camera
                 :frame-size (v2! 20) ;; zoom
                 :rot (q:point-at (v! 0 1 0) *light-pos* (v! 0 0 0))
                 :pos (v3:*s (v3:normalize  *light-pos*) 10f0)))

(defun init-shadowmap ()
  (setf *cameras* (remove :shadow-camera *cameras* :key #'camera-name))
  (push *shadow-camera* *cameras*)
  (unless *shadow-fbo*
    (setf *shadow-fbo* (make-fbo `(:d :dimensions ,*shadow-dimensions*)))
    (setf *shadow-sam* (sample (attachment-tex *shadow-fbo* :d)
                               :minify-filter  :nearest
                               :magnify-filter :nearest))))

(defun free-shadowmap ()
  (free *shadow-fbo*))

(defun draw-shadowmap ()
  (with-fbo-bound (*shadow-fbo* :attachment-for-size :d)
    (clear *shadow-fbo*)
    (loop :for actor :in *actors*
       :do (with-slots (buf scale) actor
             (map-g #'simplest-3d-pipe buf
                    :scale 1f0
                    :model-world (model->world actor)
                    :world-view  (world->view *shadow-camera*)
                    :view-clip   (projection  *shadow-camera*))))))

;;--------------------------------------------------
;; THE helper

(defun-g shadow-factor ((light-sampler :sampler-2d)
                        (pos-in-light-space :vec4))
  (let* ((proj-coords (/ (s~ pos-in-light-space :xyz)
                         (w pos-in-light-space)))
         (proj-coords (+ (* proj-coords 0.5) (vec3 0.5)))
         (our-depth (z proj-coords))
         (shadow 0f0)
         (bias 0.005)
         (texel-size (/ (vec2 1f0)
                        (texture-size light-sampler 0)))
         (uv (s~ proj-coords :xy)))
    ;;
    (for (x -1) (<= x 1) (++ x)
         (for (y -1) (<= y 1) (++ y)
              (let* ((uv+offset (+ uv (* (v! x y) texel-size)))
                     (pcf-depth (x (texture light-sampler
                                            uv+offset))))
                (incf shadow
                      (if (> (- our-depth bias) pcf-depth)
                          0f0
                          1f0)))))
    ;;
    (/ shadow 9f0)))

;;--------------------------------------------------
;; 3d - plain frag pipeline, used to render only depth

(defun-g simplest-3d-frag ((uv :vec2) (frag-norm :vec3) (frag-pos :vec3))
  (v! 1 1 1 0))

(defpipeline-g simplest-3d-pipe ()
  :vertex   (vert g-pnt)
  :fragment (simplest-3d-frag :vec2 :vec3 :vec3))
