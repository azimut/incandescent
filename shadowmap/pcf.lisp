(in-package :incandescent)

;; Uses the depth information from the POV of the directional light.

(defun free-shadowmap ()
  (when *shadow-fbo* (free *shadow-fbo*)))

(defun init-shadowmap ()
  (free-shadowmap)
  (setf *shadow-fbo* (make-fbo `(:d :dimensions ,*shadow-dimensions*)))
  (setf *shadow-sam* (sample (attachment-tex *shadow-fbo* :d)
                             :wrap           :clamp-to-border
                             :minify-filter  :nearest
                             :magnify-filter :nearest))
  (setf (cepl.samplers::border-color *shadow-sam*) (v! 1 1 1 1))
  t)

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
;; PCF type of helpers

;; NO BIAS
(defun-g shadow-factor ((light-sampler :sampler-2d) (pos-in-light-space :vec4))
  (let* ((proj-coords   (/ (s~ pos-in-light-space :xyz)
                           (w  pos-in-light-space)))
         (proj-coords   (+ .5 (* .5 proj-coords)))
         (closest-depth (x (texture light-sampler (s~ proj-coords :xy))))
         (current-depth (z proj-coords))
         (shadow        (step current-depth closest-depth)))
    (if (> current-depth 1)
        1f0
        shadow)))

;; NO BIAS - PCF
(defun-g shadow-factor ((light-sampler :sampler-2d) (pos-in-light-space :vec4))
  (let* ((proj-coords (/ (s~ pos-in-light-space :xyz)
                         (w pos-in-light-space)))
         (proj-coords (+ (* proj-coords 0.5) (vec3 0.5)))
         (our-depth   (z proj-coords))
         (shadow      0f0)
         (texel-size  (/ (vec2 1f0)
                         (texture-size light-sampler 0)))
         (uv          (s~ proj-coords :xy))
         (num-samples 3f0)
         (num-samples-start (/ (1- num-samples) 2)))
    ;;
    (if (> our-depth 1)
        (setf shadow 0f0)
        (for (x num-samples-start) (<= x num-samples) (++ x)
             (for (y num-samples-start) (<= y num-samples) (++ y)
                  (let* ((uv+offset (+ uv (* (v! x y) texel-size)))
                         (pcf-depth (x (texture light-sampler
                                                uv+offset))))
                    (incf shadow (step pcf-depth our-depth))))))
    ;;
    (- 1 (/ shadow (* num-samples num-samples 2)
            ))))

;; BIAS static
(defun-g shadow-factor ((light-sampler :sampler-2d) (pos-in-light-space :vec4))
  (let* ((proj-coords   (/ (s~ pos-in-light-space :xyz) (w  pos-in-light-space)))
         (proj-coords   (+ .5 (* .5 proj-coords)))
         (bias          .005 ;;#.(* 2f0 (/ 1024f0))
                        )
         (current-depth (z proj-coords))
         (closest-depth (x (texture light-sampler (s~ proj-coords :xy))))
         (shadow        (step (- current-depth bias) closest-depth)))
    (if (> current-depth 1)
        1f0
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
         (num-samples 3f0)
         (num-samples-start (/ (1- num-samples) 2))
         (shadow 0f0)
         (bias .005 ;;#.(* 3f0 (/ 1024f0))
               ) ;; 0.005
         (texel-size (/ (vec2 1f0)
                        (texture-size light-sampler 0)))
         (uv (s~ proj-coords :xy)))
    ;;
    (if (> our-depth 1)
        (setf shadow 0f0)
        (for (x num-samples-start) (<= x num-samples) (++ x)
             (for (y num-samples-start) (<= y num-samples) (++ y)
                  (let* ((uv+offset (+ uv (* (v! x y) texel-size)))
                         (pcf-depth (x (texture light-sampler
                                                uv+offset))))
                    (incf shadow (if (> (- our-depth bias) pcf-depth)
                                     1f0
                                     0f0))))))
    ;;
    (- 1 (/ shadow (* num-samples num-samples)))))
(defun-g shadow-factor ((light-sampler :sampler-2d) (pos-in-light-space :vec4))
  (let* ((proj-coords (/ (s~ pos-in-light-space :xyz)
                         (w pos-in-light-space)))
         (proj-coords (+ (* proj-coords 0.5) (vec3 0.5)))
         (our-depth (z proj-coords))
         (shadow 0f0)
         (bias #.(* 1f0 (/ 1024f0))
               ) ;; 0.005
         (texel-size (/ (vec2 1f0) (texture-size light-sampler 0)))
         (uv (s~ proj-coords :xy)))
    ;;
    (if (> our-depth 1)
        (setf shadow 0f0)
        (for (x -1) (<= x 1) (++ x)
             (for (y -1) (<= y 1) (++ y)
                  (let* ((uv+offset (+ uv (* (v! x y) texel-size)))
                         (pcf-depth (x (texture light-sampler
                                                uv+offset))))
                    (incf shadow (step pcf-depth (- our-depth bias)))))))
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


