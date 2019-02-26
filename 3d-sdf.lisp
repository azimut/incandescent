(in-package #:incandescent)

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

(defun-g render-pbr ((p :vec3) (center :vec3) (radius :float)
                     (cam-pos :vec3) (light-pos :vec3)
                     (brdf-lut :sampler-2d)
                     (irradiance-map :sampler-cube)
                     (diffuse-map :sampler-cube))
  (let* ((n (normalize (calc-normal p center radius)))
         (v (normalize (- cam-pos p)))
         (ao (ambient-occlussion p n .1 3 center radius))
         ;;(ao 1)
         (a 0)
         (f0 (v3! .04))
         (roughness .9)
         (metallic .1)
         (color (v! .3 .2 .9))
         ;; (a (ambient-ibl v
         ;;                 n
         ;;                 f0
         ;;                 brdf-lut
         ;;                 irradiance-map
         ;;                 diffuse-map
         ;;                 roughness
         ;;                 metallic
         ;;                 color
         ;;                 ao))
         ;; (a (pbr-ambient-map diffuse-map
         ;;                     color
         ;;                     ao
         ;;                     v
         ;;                     n
         ;;                     f0))
         (l (pbr-direct-lum light-pos
                            p
                            v
                            n
                            roughness
                            f0
                            metallic
                            color)))
    (v! (* ao (+ l a)) 0)))

(defun-g render-surface ((p :vec3)
                         (center :vec3)
                         (radius :float)
                         (light-pos :vec3))
  (let* ((n (calc-normal p center radius))
         ;;(ao 1)
         (ao (v! (v3! (ambient-occlussion p n .3 3 center radius)) 0))
         )
    (* (simple-lambert n p light-pos)
       ao)))

(defun-g simple-lambert ((n :vec3)
                         (p :vec3)
                         (light-pos :vec3))
  (let* ((light-dir (normalize (- p light-pos)))
         (light-col (v! .1 .1 .3))
         (n-dot-l   (max (dot n light-dir) 0)))
    (v! (* light-col n-dot-l) 1)))

(defun-g distance-estimator ((p :vec3) (c :vec3) (r :float))
  (let* ((p (- p c)))
    (- (length (- (s~ p :xz) (v! .3 .9))) .9)))

;; (defun-g distance-estimator ((p :vec3) (c :vec3) (r :float))
;;   (let* ((dist 4f0)
;;          (p (- p c))
;;          ;;(new-radius (+ 1 (* 2 (sin (/ (x p) dist)))))
;;          ;;(new-radius (+ 1 (* 2 (sin (/ (x p) dist)))))
;;          (new-radius (+ 1 (* r (sin (/ (x p) dist)))))
;;          ;;(new-radius r)
;;          (p (v! (- (mod (+ (* .5 dist) (x p)) dist)
;;                    (* .5 dist))
;;                 (- (mod (+ (* .5 dist) (y p)) dist)
;;                    (* .5 dist))
;;                 ;; (- (mod (+ (* .5 dist) (z p)) dist)
;;                 ;;    (* .5 dist))
;;                 (z p)
;;                 )))
;;     (- (length (- c p)) new-radius)))

(defun-g calc-normal ((pos :vec3) (center :vec3) (radius :float))
  (let* ((eps .0001)
         (nor (v! (- (distance-estimator (+ pos (v! eps 0 0)) center radius)
                     (distance-estimator (- pos (v! eps 0 0)) center radius))
                  (- (distance-estimator (+ pos (v! 0 eps 0)) center radius)
                     (distance-estimator (- pos (v! 0 eps 0)) center radius))
                  (- (distance-estimator (+ pos (v! 0 0 eps)) center radius)
                     (distance-estimator (- pos (v! 0 0 eps)) center radius)))))
    (normalize nor)))

(defun-g ambient-occlussion ((p :vec3) (n :vec3) (distance :float) (samples :float)
                             (c :vec3) (r :float))
  (let ((occlusion 1f0))
    (for (i 1) (> samples 0) (decf samples 1f0)
         (decf occlusion (- (* samples distance)
                            (distance-estimator (+ p (* n samples distance))
                                                c r))))
    occlusion))

;;--------------------------------------------------
;; http://mercury.sexy/hg_sdf/
;;--------------------------------------------------
(defun-g fmod ((x :float) (y :float))
  (- x (* y (trunc (/ x y)))))
(defun-g fmod ((x :vec2) (y :vec2))
  (- x (* y (trunc (/ x y)))))
(defun-g fmod ((x :vec3) (y :vec3))
  (- x (* y (trunc (/ x y)))))
(defun-g vmax ((v :vec2)) (max (x v) (y v)))
(defun-g vmax ((v :vec3)) (max (max (x v) (y v)) (z v)))
(defun-g vmax ((v :vec4)) (max (max (x v) (y v))
                               (max (z v) (w v))))
(defun-g vmin ((v :vec2)) (min (x v) (y v)))
(defun-g vmin ((v :vec3)) (min (min (x v) (y v)) (z v)))
(defun-g vmin ((v :vec4)) (min (min (x v) (y v))
                               (min (z v) (w v))))
;;--------------------------------------------------
(defun-g f-sphere ((p :vec3) (r :float))
  (- (length p) r))
(defun-g f-plane ((p :vec3) (n :vec3) (distance :float))
  "Plane with normal n (n is normalized) at some distance
   from the origin"
  (+ (dot p n) distance))
(defun-g f-box-cheap ((p :vec3) (b :vec3))
  "Cheap Box: distance to corners is overestimated"
  (vmax (- (abs p) b)))
(defun-g f-box ((p :vec3) (b :vec3))
  "Box: correct distance to corners"
  (let ((d (- (abs p) b)))
    (+ (length (max d (v3! 0)))
       (vmax (min d (v3! 0))))))
(defun-g f-box-2-cheap ((p :vec2) (b :vec2))
  "Same as above, but in two dimensions (an endless box)"
  (vmax (- (abs p) b)))
(defun-g f-box-2 ((p :vec2) (b :vec2))
  (let ((d (- (abs p) b)))
    (+ (length (max d (v2! 0)))
       (vmax (min d (v2! 0))))))
(defun-g f-corner ((p :vec2))
  "Endless \"corner\" "
  (+ (length (max p (v2! 0)))
     (vmax (min p (v2! 0)))))
;;--------------------------------------------------
;; fBlob
;;--------------------------------------------------
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
(defun-g f-torus ((p :vec3) (small-radius :float) (large-radius :float))
  "Torus in the XZ-plane"
  (- (length (v! (- (length (s~ p :xz)) large-radius)
                 (y p)))
     small-radius))
(defun-g f-circle ((p :vec3) (r :float))
  "A circle line. Can also be used to make a torus by subtracting
   the smaller radius of the torus."
  (let ((l (- (length (s~ p :xz)) r)))
    (length (v! (y p) l))))
(defun-g f-disc ((p :vec3) (r :float))
  "A circular disc with no thickness (i.e. a cylinder with no height).
   Subtract some value to make a flat disc with rounded edge."
  (let ((l (- (length (s~ p :xz)) r)))
    (if (< l 0)
        (abs (y p))
        (length (v! (y p) l)))))
(defun-g f-hexagon-circumcircle ((p :vec3) (h :vec2))
  "Hexagonal prism, circumcircle variant"
  (let ((q (abs p)))
    (max (- (y q) (y h))
         (- (max (+ (* (x q) (sqrt 3) .5) (* .5 (z q))) (z q))
            (x h)))))
(defun-g f-hexagon-incircle ((p :vec3) (h :vec2))
  "Hexagonal prism, incircle variant"
  (f-hexagon-circumcircle p (v! (* (x h) (sqrt 3) .5) (y h))))
;;--------------------------------------------------
;; fCone
;;--------------------------------------------------

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
(defun-g f-op-union-stairs ((a :float) (b :float) (r :float) (n :float))
  "The \"Stairs\" flavour produces n-1 steps of a staircase:"
  (let ((s (/ r n))
        (u (- b r)))
    (min (min a b)
         (* .5 (+ u a (abs (- (fmod (+ (- u a) s) (* 2 s)) s)))))))
(defun-g f-op-intersection-stairs ((a :float) (b :float) (r :float) (n :float))
  "We can just call Union since stairs are symmetric."
  (f-op-union-stairs (- a) (- b) r n))
(defun-g f-op-difference-stairs ((a :float) (b :float) (r :float) (n :float))
  (f-op-union-stairs (- a) b r n))
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
(defun-g p-mod-mirror1 ((p :float) (size :float))
  "Same, but mirror every second cell so they match at the boundaries"
  (let* ((halfsize (* .5 size))
         (c (floor (/ (+ p halfsize) size)))
         (p (- (fmod (+ p halfsize) size) halfsize))
         (p (* p (- (* 2 (fmod c 2)) 1))))
    (v! p c)))
(defun-g p-mod-grid2 ((p :vec2) (size :vec2))
  "Same, but mirror every second cell at the diagonal as well"
  (let* ((c (floor (/ (+ p (* .5 size)) size)))
         (p (- (fmod (+ p (* .5 size)) size) (* .5 size)))
         (p (* p (- (* 2 (fmod c (v2! 2))) (v2! 1))))
         (p (- p (/ size 2)))
         (p (if (> (x p) (y p))
                (v! (y p) (x p))
                p)))
    ;; vec4
    (v! p (floor (/ c 2)))))
;; (defun-g p-mod3 ((p :vec3) (size :vec3))
;;   (let* ((c (floor (/ (+ p (* .5 size)) size)))
;;          (p (- (fmod (+ p (* .5 size)) size) (* .5 size))))
;;     (v! p c)))
(defun-g p-mod2 ((p :vec2) (size :vec2))
  "Repeat in two dimensions"
  (let ((c (v! 1 1);; (floor (/ (+ p (* .5 size)) size))
          )
        (p (- (fmod (+ p (* .5 size)) size) (* size .5))))
    (v! p c)))
(defun-g p-mod1 ((p :float) (size :float))
  (let ((half (* size .5)))
    (- (fmod (+ p half) size) half)))
