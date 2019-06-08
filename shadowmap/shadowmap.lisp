(in-package #:incandescent)

;; Shadowmaps require additional information to go in/out from vertex shaders so a lot of duplicate code is needed. (unless trial)
;;
;; To debug try disabling culling and bias to see the effective shadowmap mask projected.
;;
;; UNIFORMS
;; - :shadowmap   *shadow-sam*
;; - :light-world (world->view *shadow-camera*)
;; - :light-clip  (projection  *shadow-camera*)
;;
;; Reference:
;; - https://learnopengl.com/Advanced-Lighting/Shadows/Shadow-Mapping
;; -

(defvar *shadow-fbo* NIL)
(defvar *shadow-sam* NIL)
(defparameter *shadow-dimensions* '(1024 1024))
;;(defparameter *shadow-dimensions* '(2048 2048))

(defparameter *shadow-camera*
  (let* ((lpos (v! 10 10 10))
         (cam  (make-instance 'orth
                              :name :shadow-camera
                              :frame-size (v2! 30) ;; zoom
                              :far 20f0
                              :near .1f0
                              :rot (q:point-at (v! 0 1 0) lpos
                                               (v! 0 3 0))
                              :pos lpos)))
    (setf *light-pos* lpos)
    (setf *cameras* (remove :shadow-camera *cameras* :key #'camera-name))
    ;;(push *shadow-camera* *cameras*)
    cam))

(defun init-shadowmap ()
  (setf *cameras* (remove :shadow-camera *cameras* :key #'camera-name))
  (push *shadow-camera* *cameras*)
  (when *shadow-fbo*
    (free *shadow-fbo*))
  (setf *shadow-fbo* (make-fbo `(:d :dimensions ,*shadow-dimensions*)))
  (setf *shadow-sam* (sample (attachment-tex *shadow-fbo* :d)
                             :wrap           :clamp-to-border
                             :minify-filter  :nearest
                             :magnify-filter :nearest))
  (setf (cepl.samplers::border-color *shadow-sam*) (v! 1 1 1 1))
  t)

(defun free-shadowmap ()
  (when *shadow-fbo* (free *shadow-fbo*)))

(defun-g simplest-3d-frag ((uv :vec2) (frag-norm :vec3) (frag-pos :vec3))
  (v! 0 0 1 0))

(defpipeline-g simplest-3d-pipe ()
  :vertex   (vert g-pnt)
  :fragment (simplest-3d-frag :vec2 :vec3 :vec3))

(defun draw-shadowmap ()
  "draws the scene in *ACTORS* from the point of view of *SHADOW-CAMERA* into *SHADOW-FBO* using a simple shader pipe"
  (with-fbo-bound (*shadow-fbo* :attachment-for-size :d)
    (clear *shadow-fbo*)
    (with-setf (cull-face) :front)
    (loop :for actor :in *actors*
          :do (with-slots (buf scale) actor
                (map-g #'simplest-3d-pipe buf
                       :scale scale
                       :model-world (model->world actor)
                       :world-view  (world->view *shadow-camera*)
                       :view-clip   (projection  *shadow-camera*))))))

;;--------------------------------------------------
;; THE helper

;; NO BIAS
(defun-g shadow-factor ((light-sampler :sampler-2d) (pos-in-light-space :vec4))
  (let* ((proj-coords   (/ (s~ pos-in-light-space :xyz)
                           (w  pos-in-light-space)))
         (proj-coords   (+ .5 (* .5 proj-coords)))
         (closest-depth (x (texture light-sampler (s~ proj-coords :xy))))
         (current-depth (z proj-coords))
         (shadow        (if (> current-depth closest-depth)
                            1f0
                            0f0)))
    (if (> (z proj-coords) 1)
        0f0
        shadow)))

;; NO BIAS - PCF
(defun-g shadow-factor ((light-sampler :sampler-2d) (pos-in-light-space :vec4))
  (let* ((proj-coords (/ (s~ pos-in-light-space :xyz)
                         (w pos-in-light-space)))
         (proj-coords  (+ (* proj-coords 0.5) (vec3 0.5)))
         (our-depth    (z proj-coords))
         (shadow        0f0)
         (texel-size    (/ (vec2 1f0)
                           (texture-size light-sampler 0)))
         (uv            (s~ proj-coords :xy))
         (num-samples   5f0
                        )
         (num-samples-start (/ (1- num-samples) 2)
                            ))
    ;;
    (if (> our-depth 1)
        (setf shadow 0f0)
        (for (x num-samples-start) (<= x num-samples) (++ x)
             (for (y num-samples-start) (<= y num-samples) (++ y)
                  (let* ((uv+offset (+ uv (* (v! x y) texel-size)))
                         (pcf-depth (x (texture light-sampler
                                                uv+offset))))
                    (incf shadow (if (> our-depth pcf-depth)
                                     1f0
                                     0f0))))))
    ;;
    (/ shadow (* num-samples num-samples)
       )))

;; BIAS static
(defun-g shadow-factor ((light-sampler :sampler-2d) (pos-in-light-space :vec4))
  (let* ((proj-coords (/ (s~ pos-in-light-space :xyz)
                         (w  pos-in-light-space)))
         (proj-coords (+ .5 (* .5 proj-coords)))
         (closest-depth (x (texture light-sampler (s~ proj-coords :xy))))
         (current-depth (z proj-coords))
         (bias .005)
         (shadow (if (> (- current-depth bias) closest-depth)
                     1f0
                     0f0)))
    (if (> current-depth 1)
        0f0
        shadow)))

;; BIAS - dynamic
(defun-g shadow-factor ((light-sampler :sampler-2d)
                        (pos-in-light-space :vec4)
                        (light-dir :vec3)
                        (normal :vec3))
  (let* ((proj-coords (/ (s~ pos-in-light-space :xyz)
                         (w  pos-in-light-space)))
         (proj-coords (+ .5 (* .5 proj-coords)))
         (closest-depth (x (texture light-sampler (s~ proj-coords :xy))))
         (current-depth (z proj-coords))
         (bias (max (* .005 (- 1 (dot normal light-dir))) .005)))
    (if (> (- current-depth bias) closest-depth)
        1f0
        0f0)))

;; BIAS static - PCF
(defun-g shadow-factor ((light-sampler :sampler-2d) (pos-in-light-space :vec4))
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
                (incf shadow (if (> (- our-depth bias) pcf-depth)
                                 1f0
                                 0f0)))))
    ;;
    (/ shadow 9f0)))

;; BIAS dynamic - PCF
(defun-g shadow-factor ((light-sampler :sampler-2d)
                        (pos-in-light-space :vec4)
                        (light-dir :vec3)
                        (normal :vec3))
  (let* ((proj-coords (/ (s~ pos-in-light-space :xyz)
                         (w pos-in-light-space)))
         (proj-coords (+ (* proj-coords 0.5) (vec3 0.5)))
         (our-depth (z proj-coords))
         (shadow 0f0)
         (bias (max (* .005 (- 1 (dot normal light-dir))) .005))
         (texel-size (/ (vec2 1f0)
                        (texture-size light-sampler 0)))
         (uv (s~ proj-coords :xy)))
    ;;
    (for (x -1) (<= x 1) (++ x)
         (for (y -1) (<= y 1) (++ y)
              (let* ((uv+offset (+ uv (* (v! x y) texel-size)))
                     (pcf-depth (x (texture light-sampler
                                            uv+offset))))
                (incf shadow (if (> (- our-depth bias) pcf-depth)
                                 1f0
                                 0f0)))))
    ;;
    (/ shadow 9f0)))

