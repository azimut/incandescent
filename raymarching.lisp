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
  (setf (rot actor) (rot *currentcamera*))
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
           :res (resolution (current-viewport))
           :time time
           :cam-pos (pos *currentcamera*)
           :frustum-corners *frustum*
           :world-view (m4:*
                        ;;(projection *currentcamera*)
                        (let ((fs (or (frame-size *currentcamera*)
                                      (viewport-resolution (current-viewport)))))
                          (rtg-math.projection:orthographic-v2
                           (v2! 1)
                           .1
                           1000f0))
                        (world->view *tmp2*
                                     ;;*currentcamera*
                                     )
                        ;;(world->view *currentcamera*)
                                        ;,(m4:identity)
                        (model->world *tmp2*)
                        ;;(model->world *currentcamera*)
                        )
           :view-world (m4:inverse
                        (m4:*
                         ;;(projection *currentcamera*)
                         (world->view *tmp2*;;*currentcamera*
                                      )
                         ;;(model->world *currentcamera*)
                         ;;(model->world *tmp2*)
                         )
                        ))))


;;--------------------------------------------------
;; SPHERE

(defun-g distance-estimator ((p :vec3) (c :vec3) (r :float))
  (let* ((p (- p c)))
    (- (length (- (s~ p :xz) (v! .3 .9))) .9)))

;; (defun-g calc-normal ((pos :vec3))
;;   (let* ((eps (v! .001 0))
;;          (center (v! 3 0 -10))
;;          (radius 4f0)
;;          (nor (v! (- (distance-estimator (+ pos (s~ eps :xyy)) center radius)
;;                      (distance-estimator (- pos (s~ eps :xyy)) center radius))
;;                   (- (distance-estimator (+ pos (s~ eps :yxy)) center radius)
;;                      (distance-estimator (- pos (s~ eps :yxy)) center radius))
;;                   (- (distance-estimator (+ pos (s~ eps :yyx)) center radius)
;;                      (distance-estimator (- pos (s~ eps :yyx)) center radius)))))
;;     (normalize nor)))

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
  (let* ((light-pos (v! 100 100 100))
         (cam-pos   (v! 0 9 10))
         (light-dir (normalize (- p light-pos)))
         (light-col (v! 1 1 1))
         (n-dot-l   (max (dot n light-dir) 0))
         )
    ;;(v! (* light-col n-dot-l) 1)
    (v! (pbr-direct-lum light-pos
                        p
                        (normalize (- p cam-pos))
                        n
                        .9
                        (v3! .4)
                        .01
                        (v! .1 .2 1))
        0)
    ))

(defun-g render-surface ((p :vec3) (center :vec3) (radius :float))
  (let ((n (calc-normal p center radius)))
    (* (simple-lambert n p)
       (v! (v3! (clamp (ambient-occlussion p n .7 3 center radius) 0 1)) 0)
       )))

;; #define dist 10
;; #define center vec3(0,0,0)
;; float SphereDistance( vec3 point ) {
;;    float newRadius = 1+2*sin(floor(point.x/dist)));
;;    point.x = mod( point.x + dist/2, dist ) - dist/2;
;;    return length( center - point ) - newRadius;
;; }
(defun-g f-sphere ((p :vec3) (r :float))
  (- (length p) r))
(defun-g f-plane ((p :vec3) (n :vec3) (distance :float))
  "Plane with normal n (n is normalized) at some distance
   from the origin"
  (+ (dot p n) distance))
(defun-g f-box ((p :vec3) (b :vec3))
  (let ((d (- (abs p) b)))
    (+ (length (max d (v3! 0)))
       (vmax (min d (v3! 0))))))
(defun-g f-cylinder ((p :vec3) (r :float) (height :float))
  "Cylinder standing upright on the xz plane"
  (let ((d (- (length (s~ p :xz)) r)))
    (max d (- (abs (y p)) height))))
(defun-g f-linesegment ((p :vec3) (a :vec3) (b :vec3))
  "Distance to line segment between <a> and <b>, used for fCapsule() version 2below"
  (let* ((ab (- b a))
         (tt (saturate (/ (dot (- p a) ab)
                          (dot ab ab)))))
    (length (- (+ a (* ab tt)) p))))
(defun-g f-capsule ((p :vec3) (a :vec3) (b :vec3) (r :float))
  "Capsule: A Cylinder with round caps on both sides"
  (- (f-linesegment p a b) r))
;;--------------------------------------------------
;; The "Round" variant uses a quarter-circle to join the two objects smoothly:
(defun-g f-op-union-round ((a :float) (b :float) (r :float))
  (let ((u (max (v! (- r a) (- r b)) (v2! 0))))
    (- (max r (min a b)) (length u))))
(defun-g f-op-intersection-round ((a :float) (b :float) (r :float))
  (let ((u (max (v! (+ r a) (+ r b)) (v2! 0))))
    (+ (min (- r) (max a b)) (length u))))
(defun-g f-op-difference-round ((a :float) (b :float) (r :float))
  (f-op-intersection-round a (- b) r))
;;--------------------------------------------------
(defun-g f-op-union-chamfer ((a :float) (b :float) (r :float))
  "The \"Chamfer\" flavour makes a 45-degree chamfered edge
   (the diagonal of a square of size <r>):"
  (min (min a b) (* (+ (- a r) b) (sqrt 0.5))))
(defun-g f-op-intersection-chamfer ((a :float) (b :float) (r :float))
  "Difference can be built from Intersection or Union"
  (max (max a b) (* (+ a r b) (sqrt 0.5))))
(defun-g f-op-difference-chamfer ((a :float) (b :float) (r :float))
  "Intersection has to deal with what is normally the inside of
   the resulting object when using union, which we normally don't
   care about too much. Thus, intersection implementations
   sometimes differ from union implementations."
  (f-op-intersection-chamfer a (- b) r))
;;--------------------------------------------------
;; fOpUnionColumns
;; fOpDifferenceColumns
;; fOpIntersectionColumns
;;--------------------------------------------------
(defun-g fmod ((x :float) (y :float))
  (- x (* y (trunc (/ x y)))))
(defun-g f-op-union-stairs ((a :float) (b :float) (r :float) (n :float))
  "The \"Stairs\" flavour produces n-1 steps of a staircase:"
  (let ((s (/ r n))
        (u (- b r)))
    (min (min a b)
         (* .5 (+ u a (abs (- (fmod (+ (- u a) s) (* 2 s)) s)))))))
(defun-g f-op-intersection-stairs ((a :float) (b :float) (r :float) (n :float))
  "We can just call Union since stairs are symmetric."
  (f-op-union-stairs (-a) (- b) r n))
(defun-g f-op-difference-stairs ((a :float) (b :float) (r :float) (n :float))
  (f-op-union-stairs (-a) b r n))
;;--------------------------------------------------
(defun-g f-op-union-soft ((a :float) (b :float) (r :float))
  "Similar to fOpUnionRound, but more lipschitz-y at acute angles
   (and less so at 90 degrees). Useful when fudging around too much
   by MediaMolecule, from Alex Evans' siggraph slides"
  (let ((e (max (- r (abs (- a b))) 0)))
    (- (min a b) (* 2.71828 2.71828 (/ .25 r)))))
(defun-g f-op-pipe ((a :float) (b :float) (r :float))
  "produces a cylindical pipe that runs along the intersection.
   No objects remain, only the pipe. This is not a boolean operator."
  (- (length (v! a b)) r))
(defun-g f-op-engrave ((a :float) (b :float) (r :float))
  "first object gets a v-shaped engraving where it intersect the second"
  (max a (* (- (+ a r) (abs b))
            (sqrt .5))))
(defun-g f-op-groove ((a :float) (b :float) (ra :float) (rb :float))
  "first object gets a capenter-style groove cut out"
  (max a (min (+ a ra) (- rb (abs b)))))
(defun-g f-op-tounge ((a :float) (b :float) (ra :float) (rb :float))
  "first object gets a capenter-style tongue attached"
  (min a (max (- a ra) (- (abs b) rb))))
;;--------------------------------------------------
(defun-g ambient-occlussion ((p :vec3) (n :vec3) (distance :float) (samples :float)
                             (c :vec3) (r :float))
  (let ((occlusion 1f0))
    (for (i 1) (> samples 0) (decf samples 1f0)
         (decf occlusion (- (* samples distance)
                            (distance-estimator (+ p (* n samples distance))
                                                c r))))
    occlusion))

(defun-g distance-estimator ((p :vec3)
                             (c :vec3)
                             (r :float))
  (let* ((p (- p c))
         (p (* p (m3:* (m3:rotation-y (radians 20))
                       (m3:rotation-x (radians -20)))))
         )
    ;;(f-op-union-chamfer)
    ;;(f-capsule p (v! 0 5 0) (v! 0 -5 0) .3)
    (f-op-tounge (f-box p (v! .9 3 .9))
                 (f-cylinder p .9 .9)
                 .2
                 .8)
    ;;.7
    ))

(defun-g distance-estimator ((p :vec3) (c :vec3) (r :float))
  (let* ((dist 4f0)
         ;;(new-radius (+ 1 (* 2 (sin (/ (x p) dist)))))
         ;;(new-radius (+ 1 (* 2 (sin (/ (x p) dist)))))
         (new-radius (+ 1 (* r (sin (/ (x p) dist)))))
         ;;(new-radius r)
         (p (v! (- (mod (+ (* .5 dist) (x p)) dist)
                   (* .5 dist))
                (- (mod (+ (* .5 dist) (y p)) dist)
                   (* .5 dist))
                ;; (- (mod (+ (* .5 dist) (z p)) dist)
                ;;    (* .5 dist))
                (z p)
                )))
    (- (length (- c p)) new-radius)))

(defun-g raymarch ((ro :vec3)
                   (rd :vec3)
                   (s :float)
                   (time :float))
  (let* ((tt 0f0)
         (ret (v! 0 0 0 0))
         ;; Geometry
         (center (v! 0 0 0))
         (radius (+ 1 (sin (* .01 time)))))
    (dotimes (i 50)
      (when (or (>= tt s)
                (> tt 110f0))
        (setf ret (v! 0 0 0 0))
        (break))
      (let* ((p (+ ro (* rd tt)))
             (d (distance-estimator p center radius)))
        (when (< d .001)
          (setf ret (v! (+ ;;(v! .01 .01 .01)
                         (s~ (render-surface p center radius) :xyz)
                         )
                        1))
          (break))
        (incf tt d)))
    ;;(v! (sin (y p)) 0 0 0)
    ret
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
         (pos      (v!  (x pos) (y pos) .1))
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
                        (res :vec2)
                        (time :float)
                        (sam  :sampler-2d)
                        (samd :sampler-2d)
                        (cam-pos :vec3))
  (let* ((rd    (normalize ray))
         (ro    cam-pos)
         (depth (linear-eye-depth (x (texture samd uv))))
         (depth (* depth (length ray)))
         (color (s~ (texture sam uv) :xyz))
         (add   (raymarch ro rd depth time)))
    ;; return fixed4(col*(1.0 - add.w) + add.xyz * add.w,1.0);
    (v! (+ (* color (- 1 (w add)))
           (* (s~ add :xyz) (w add)))
        1)
    add
    ;;ray
    ;;(v3! depth)
    ;;(v! 1 0 1 0)
    ))

(defpipeline-g raymarch-pipe (:triangle-strip)
  :vertex   (raymarch-vert g-pnt;;:vec3
                           )
  :fragment (raymarch-frag :vec2 :vec3))
