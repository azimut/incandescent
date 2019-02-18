(in-package #:incandescent)

;; Raymarching as postprocess, to integrate it with a normal
;; rasterized scene.

;; INPUTS:
;; - *dimensions*
;; - *samd*
;; - *sam*


;; VARJO (v:-) should negate the single vector
;;       up/down/right/left unity like for (v! 0 1 0) et al
;;       setf for m4:get-row // meh!
;;       for() support global vars
;;       missing MOD vec3/vec3
(defvar *quad-3d* NIL "BUFFER-STREAM that has the geometry for the quad")
(defvar *frustum* NIL "C-ARRAY that has the frustum corners???")

(defvar *ray-fbo* NIL)
(defvar *ray-sam* NIL)

(defvar *max-steps* 10f0)
(defvar *min-distance* .001)
(defvar *max-distance* 1000f0)

(defclass ray-plane (actor) ())
(defmethod update ((actor ray-plane))
  ;; (setf (pos actor) (pos *currentcamera*))
  ;; (setf (rot actor) (rot *currentcamera*))
  )

(defun init-raymarching ()
  (when *ray-fbo* (free *ray-fbo*))
  (setf *ray-fbo* (make-fbo `(0 :dimensions ,*dimensions*
                                :element-type :rgba16f))
        *ray-sam* (sample (attachment-tex *ray-fbo* 0)
                          :wrap :clamp-to-edge))
  (unless *frustum*
    (setf *frustum*
          (make-c-array (get-frustum-corners *currentcamera*)
                        :element-type :vec3 :dimensions 4)))
  (unless *quad-3d*
    ;; Half screen billboard
    ;; (setf *quad-3d*
    ;;       (let* ((verts (make-gpu-array (list (v! -.5 -.5 3)
    ;;                                           (v!  .5 -.5 2)
    ;;                                           (v! -.5  .5 1)
    ;;                                           (v!  .5  .5 0))))
    ;;              (buf (make-buffer-stream verts :primitive :triangle-strip)))
    ;;         buf))
    ;; 1 0 0 // BL
    ;; 0 1 0 // TR
    ;; 0 0 1 // BR
    ;; 0 0 0 // TL
    (setf *quad-3d*
          (let* ((verts (make-gpu-array
                         `((,(v! -1 -1 3) ,(v! 1 0 0) ,(v! 0 0))
                           (,(v!  1  1 1) ,(v! 0 1 0) ,(v! 1 1))
                           (,(v!  1 -1 2) ,(v! 0 0 1) ,(v! 1 0))
                           (,(v! -1  1 0) ,(v! 0 0 0) ,(v! 0 1)))
                         :dimensions 4 :element-type 'g-pnt))
                 (indi  (make-gpu-array '(0 1 2 1 0 3)
                                        :dimensions 6

                                        :element-type :unsigned-short))
                 (buf   (make-buffer-stream verts
                                            :index-array indi
                                            :primitive :triangle-strip)))
            buf))
    ))

(defvar *tmp2* (make-instance 'ray-plane))
(defvar *tmp3* (make-instance 'actor))

(defun draw-raymarching (time)
  (declare (type single-float time))
  (update *tmp2*)
  (when (funcall *stepper*)
    (push-g (get-frustum-corners *currentcamera*)
            *frustum*))
  (with-fbo-bound (*ray-fbo*)
    (clear *ray-fbo*)
    (map-g #'raymarch-pipe *quad-3d*
           :samd *samd*
           :sam  *sam*
           :time time
           :cam-pos (pos *currentcamera*)
           :frustum-corners *frustum*
           :world-view (m4:*
                        ;;(projection *currentcamera*)
                        ;;(world->view *currentcamera*)
                        (let ((fs (or (frame-size *currentcamera*)
                                      (viewport-resolution (current-viewport)))))
                          (rtg-math.projection:orthographic-v2
                           (v2! 1)
                           .1
                           1000f0))
                        ;;(world->view *currentcamera*)
                        ;;(m4:identity)
                        ;;(model->world *tmp3*)
                        ;;(model->world *currentcamera*)
                        )
           :view-world (m4:inverse
                        (m4:*
                         ;;(projection *currentcamera*)
                         (world->view *currentcamera*)
                         ;;(model->world *currentcamera*)
                         ;;(model->world *tmp2*)
                         )
                        ))))


;;--------------------------------------------------
;; SPHERE



(defun-g distance-estimator ((p :vec3) (c :vec3) (r :float))
  (let* ((p (- p c)))
    (- (length (- (s~ p :xz) (v! .3 .9))) .9)))

(defun-g calc-normal ((pos :vec3))
  (let* ((eps (v! .001 0))
         (center (v! 3 0 -10))
         (radius 4f0)
         (nor (v! (- (distance-estimator (+ pos (s~ eps :xyy)) center radius)
                     (distance-estimator (- pos (s~ eps :xyy)) center radius))
                  (- (distance-estimator (+ pos (s~ eps :yxy)) center radius)
                     (distance-estimator (- pos (s~ eps :yxy)) center radius))
                  (- (distance-estimator (+ pos (s~ eps :yyx)) center radius)
                     (distance-estimator (- pos (s~ eps :yyx)) center radius)))))
    (normalize nor)))

(defun-g calc-normal ((pos :vec3) (center :vec3) (radius :float))
  (let* ((eps .01)
         (nor (v! (- (distance-estimator (+ pos (v! eps 0 0)) center radius)
                     (distance-estimator (- pos (v! eps 0 0)) center radius))
                  (- (distance-estimator (+ pos (v! 0 eps 0)) center radius)
                     (distance-estimator (- pos (v! 0 eps 0)) center radius))
                  (- (distance-estimator (+ pos (v! 0 0 eps)) center radius)
                     (distance-estimator (- pos (v! 0 0 eps)) center radius)))))
    (normalize nor)))


;; (defun-g raymarch ((from :vec3) (direction :vec3) (s :float))
;;   (let ((ret (v4! 0))
;;         (max-steps 20)
;;         (draw-distance 40f0)
;;         (tt 0f0))
;;     (for (steps 0) (< steps max-steps) (++ steps)
;;          (when (or (>= tt s) (> tt draw-distance))
;;            (setf ret (v! 0 0 0 0))
;;            (break))
;;          (let* ((p (+ from (* direction tt)))
;;                 (d (distance-estimator p (v! 3 0 -10) 1f0)))
;;            (when (< d .001)
;;              (let* ((n (calc-normal p))
;;                     (light (dot (v! 0 1 0) n)))
;;                (setf ret (v4! (* light 1)))
;;                (break)))
;;            (incf tt d)))
;;     ret))

(defun-g simple-lambert ((n :vec3) (p :vec3))
  (let* ((light-dir (normalize (- p (v! 0 0 -100))))
         (light-col (v! 1 1 1))
         (n-dot-l   (max (dot n light-dir) 0)))
    (v! (*  light-col n-dot-l) 1)))

(defun-g render-surface ((p :vec3) (center :vec3) (radius :float))
  (let ((n (calc-normal p center radius)))
    (simple-lambert n p)))
;; #define dist 10
;; #define center vec3(0,0,0)
;; float SphereDistance( vec3 point ) {
;;    float newRadius = 1+2*sin(floor(point.x/dist)));
;;    point.x = mod( point.x + dist/2, dist ) - dist/2;
;;    return length( center - point ) - newRadius;
;; }

(defun-g distance-estimator ((p :vec3) (c :vec3) (r :float))
  (- (length (- p c)) r))

(defun-g distance-estimator ((p :vec3) (c :vec3) (r :float))
  (let* ((dist 4f0)
         ;;(new-radius (+ 1 (* 2 (sin (/ (x p) dist)))))
         ;;(new-radius (+ 1 (* 2 (tan (/ (x p) dist)))))
         ;;(new-radius (cos (/ (x p) dist)))
         (new-radius r)
         (p (v! (- (mod (+ (* .5 dist) (x p)) dist)
                   (* .5 dist))
                (- (mod (+ (* .5 dist) (y p)) dist)
                   (* .5 dist))
                ;; (- (mod (+ (* .5 dist) (z p)) dist)
                ;;    (* .5 dist))
                (z p)
                )))
    (- (length (- c p)) new-radius)))

(defun-g raymarch ((from :vec3) (direction :vec3))
  (let* ((p from)
         (distance 10f0)
         (center (v! 2 0 -20))
         (radius 1f0))
    (for (i 0) (< i 10) (++ i)
         (let ((distance (distance-estimator p center radius)))
           (when (< distance .01f0)
             (return (+ (v! .01 .01 .01 0)
                        (render-surface p center radius))))
           (incf p (* distance direction))))
    ;;(v! (sin (y p)) 0 0 0)
    (v! 0 0 0 0)
    ))

;;--------------------------------------------------
;; PIPELINE
;;

;; https://github.com/Flafla2/Generic-Raymarch-Unity/blob/master/Assets/RaymarchGeneric.shader
(defun-g raymarch-vert ((vert g-pnt) ;;(pos :vec3)
                        &uniform
                        (world-view :mat4)
                        (view-world :mat4)
                        (time :float)
                        (frustum-corners (:vec3 4)))
  (let* ((pos      (pos vert))
         (index    (int (z pos)))
         (pos      (v! (x pos) (y pos) .1))
         (view-pos (* world-view (v! pos 1)))
         ;; get the eyespace view ray (normalized)
         (ray (s~ (aref frustum-corners index)
                  :xyz))
         ;; Dividing by z "normalizes" it in the z axis
	 ;; Therefore multiplying the ray by some number i gives the viewspace position
	 ;; of the point on the ray with [viewspace z]=i
         (ray (/ ray (abs (z ray))))
         ;; Transform the ray from eyespace to worldspace
         (ray (s~ (* view-world (v! ray 1)) :xyz)))
    (values view-pos
            (tex vert)
            ray)))

(defun-g raymarch-frag ((uv :vec2)
                        (ray :vec3)
                        &uniform
                        (time :float)
                        (sam :sampler-2d)
                        (samd :sampler-2d)
                        (cam-pos :vec3))
  (let* ((rd    (normalize ray))
         (ro    cam-pos)
         (depth (linear-eye-depth (x (texture samd uv))))
         (depth (* depth (length ray)))
         (color (s~ (texture sam uv) :xyz))
         (add   (raymarch ro rd)))
    add
    ;;(v! 1 0 1 0)
    ))

(defpipeline-g raymarch-pipe (:triangle-strip)
  :vertex   (raymarch-vert g-pnt;;:vec3
                           )
  :fragment (raymarch-frag :vec2 :vec3))
