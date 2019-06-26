(in-package :incandescent)

;; Uses both the depth and the depth^2 to compute the shadow. Plus some blur.
;;
;; Reference:
;; - "bennybox" tutorial https://www.youtube.com/watch?v=LGFDifcbsoQ
;; - http://codeflow.org/entries/2013/feb/15/soft-shadow-mapping/

(defvar *variance-fbo* NIL)
(defvar *variance-sam* NIL)

(defun free-variance ()
  (when *shadow-fbo*   (free *shadow-fbo*))
  (when *variance-fbo* (free *variance-fbo*)))

(defun init-variance ()
  (free-variance)
  (setf *shadow-fbo*
        (make-fbo `(0  :dimensions ,*shadow-dimensions* :element-type :rg32f)
                  `(:d :dimensions ,*shadow-dimensions*))
        *variance-fbo*
        (make-fbo `(0  :dimensions ,(mapcar (lambda (x) (round (/ x 2f0))) *shadow-dimensions*)
                       :element-type :rg32f))
        ;;(make-fbo `(0  :dimensions ,*shadow-dimensions* :element-type :rg32f))
        )
  (setf *shadow-sam*
        (sample (attachment-tex *shadow-fbo* 0)   :wrap :clamp-to-border)
        *variance-sam*
        (sample (attachment-tex *variance-fbo* 0) :wrap :clamp-to-edge))
  (setf (cepl.samplers::border-color *shadow-sam*) (v! 1 1 1 1))
  t)

;;--------------------------------------------------

(defun-g variance-3d-frag ((uv :vec2) (frag-norm :vec3) (frag-pos :vec3))
  (let ((depth (z gl-frag-coord)))
    (v! depth (* depth depth))))

(defun-g variance-3d-frag ((uv :vec2) (frag-norm :vec3) (frag-pos :vec3))
  (let* ((depth (z gl-frag-coord))
         (dx    (d-fdx depth))
         (dy    (d-fdy depth))
         (m2    (+ (* depth depth)
                   (* .25 (+ (* dx dx)
                             (* dy dy))))))
    (v! depth m2)))

(defpipeline-g variance-3d-pipe ()
  :vertex   (vert g-pnt)
  :fragment (variance-3d-frag :vec2 :vec3 :vec3))

;;

(defun-g variant-blur-frag ((uv :vec2) &uniform
                            (sam :sampler-2d)
                            (blur-scale :vec2))
  (+ (* (texture sam (+ uv (* -3 blur-scale))) #.(/ 1  64f0))
     (* (texture sam (+ uv (* -2 blur-scale))) #.(/ 6  64f0))
     (* (texture sam (+ uv (* -1 blur-scale))) #.(/ 15 64f0))
     (* (texture sam (+ uv                  )) #.(/ 20 64f0))
     (* (texture sam (+ uv (*  1 blur-scale))) #.(/ 15 64f0))
     (* (texture sam (+ uv (*  2 blur-scale))) #.(/ 6  64f0))
     (* (texture sam (+ uv (*  3 blur-scale))) #.(/ 1  64f0))))

(defpipeline-g variance-blur-pipe (:points)
  :fragment (variant-blur-frag :vec2))

;;--------------------------------------------------

(defun draw-variance (&optional (br 1f0))
  "draws the scene in *ACTORS* from the point of view of *variance-CAMERA* into *variance-FBO* using a simple shader pipe"
  (declare (type single-float br))
  (with-fbo-bound (*shadow-fbo*)
    (clear *shadow-fbo*)
    (loop :for actor :in *actors*
          :do (with-slots (buf scale) actor
                (map-g #'variance-3d-pipe buf
                       :scale scale
                       :model-world (model->world actor)
                       :world-view  (world->view *shadow-camera*)
                       :view-clip   (projection  *shadow-camera*)))))
  (with-fbo-bound (*variance-fbo*)
    (clear *variance-fbo*)
    (map-g #'variance-blur-pipe *bs*
           :sam *shadow-sam*
           :blur-scale (v! (/ 1 (* br (car *shadow-dimensions*)))
                           0)))
  (with-fbo-bound (*shadow-fbo*)
    (clear *shadow-fbo*)
    (map-g #'variance-blur-pipe *bs*
           :sam *variance-sam*
           :blur-scale (v! 0
                           (/ 1 (* br (cadr *shadow-dimensions*)))))))

;;--------------------------------------------------
#+nil
(defun-g shadow-factor ((light-sampler :sampler-2d) (pos-in-light-space :vec4))
  (let* ((proj-coords   (/ (s~ pos-in-light-space :xyz) (w  pos-in-light-space)))
         (proj-coords   (+ .5 (* .5 proj-coords)))
         (uv            (s~ proj-coords :xy))
         (moments       (s~ (texture light-sampler uv) :xy))
         (closest-depth (x (texture light-sampler uv)))
         (current-depth (z proj-coords))
         (p             (step current-depth (x moments)))
         (variance      (max .00002 (- (y moments) (* (x moments) (x moments)))))
         (d             (- current-depth (x moments)))
         (pmax          (/ variance (+ variance (* d d)))))
    p))

;; With lightbleed
(defun-g shadow-factor ((light-sampler :sampler-2d) (pos-in-light-space :vec4))
  (let* ((proj-coords   (/ (s~ pos-in-light-space :xyz) (w  pos-in-light-space)))
         (proj-coords   (+ .5 (* .5 proj-coords)))
         (uv            (s~ proj-coords :xy))
         (moments       (s~ (texture light-sampler uv) :xy))
         (closest-depth (x (texture light-sampler uv)))
         (current-depth (z proj-coords))
         (p             (step current-depth (x moments)))
         (variance      (max (- (y moments) (* (x moments) (x moments)))
                             .00002))
         (d             (- current-depth (x moments)))
         (pmax          (/ variance (+ variance (* d d)))))
    (min 1f0 (min 1f0 (max p pmax)))
    ;;(- 1 pmax)
    ))

;; "NO" lightbleed
#+nil
(defun-g lin-step ((low :float) (high :float) (v :float))
  (clamp (/ (- v low) (- high low))
         0f0
         1f0))
#+nil
(defun-g shadow-factor ((light-sampler :sampler-2d) (pos-in-light-space :vec4))
  (let* ((proj-coords   (/ (s~ pos-in-light-space :xyz) (w  pos-in-light-space)))
         (proj-coords   (+ .5 (* .5 proj-coords)))
         (uv            (s~ proj-coords :xy))
         (moments       (s~ (texture light-sampler uv) :xy))
         (closest-depth (x (texture light-sampler uv)))
         (current-depth (z proj-coords))
         (p             (smoothstep (- current-depth .02)
                                    current-depth
                                    (x moments)))
         (variance      (max (- (y moments) (* (x moments) (x moments)))
                             -.001))
         (d             (- current-depth (x moments)))
         (pmax          (/ variance (+ variance (* d d))))
         (pmax          (lin-step 0.2 1f0 pmax)))
    ;;(- 1 (min 1f0 (min 1f0 (max p pmax))))
    (clamp (max p pmax) 0f0 1f0)
    ))
