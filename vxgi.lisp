(in-package #:incandescent)

;; Reference:
;; - https://github.com/Friduric/voxel-cone-tracing/
;;
;; two passes, runs every frame
;; 1) light voxelization
;; 2) cone tracing

(defvar *voxel-fbo*        nil)
(defvar *voxel-light*      nil)
(defvar *voxel-light-sam*  nil)
(defvar *voxel-light-zam*  nil)

(defparameter *volume-dimension* 64)
(defparameter *voxel-dimensions* (v3! *volume-dimension*))
(defparameter *voxel-mipmaps*    7)

(defun-g voxelize-vert ((vert g-pnt)
                        &uniform
                        (scale       :float)
                        (model-world :mat4))
  (let ((spos  (* scale (pos vert)))
        (norm (norm vert)))
    (values (* model-world (v! spos 1))
            (normalize (* (m4:to-mat3 model-world) norm)))))

;;--------------------------------------------------

(defun-g voxelize-geom ((nor (:vec3 3)))
  (declare (output-primitive :kind :triangle-strip
                             :max-vertices 3))
  (let* ((p1 (- (s~ (gl-position (aref gl-in 1)) :xyz)
                (s~ (gl-position (aref gl-in 0)) :xyz)))
         (p2 (- (s~ (gl-position (aref gl-in 2)) :xyz)
                (s~ (gl-position (aref gl-in 0)) :xyz)))
         (p  (abs (cross p1 p2)))
         (wp (vec3 0f0))
         (nf (vec3 0f0)))
    (dotimes (i 3)
      (setf wp (s~ (gl-position (aref gl-in i)) :xyz))
      (setf nf (aref nor i))
      (cond ((and (> (z p) (x p))
                  (> (z p) (y p)))
             (emit () (v! (x wp) (y wp) 0 1) wp nf))
            ((and (> (x p) (y p))
                  (> (x p) (z p)))
             (emit () (v! (y wp) (z wp) 0 1) wp nf))
            (t
             (emit () (v! (x wp) (z wp) 0 1) wp nf))))
    (emit-vertex)
    (end-primitive)))

;;--------------------------------------------------

(defun-g scale-and-bias ((p :vec3))
  (+ .5 (* .5 p)))

(defun-g inside-cube-p ((p :vec3) (e :float))
  (and (< (abs (x p)) (+ e 1f0))
       (< (abs (y p)) (+ e 1f0))
       (< (abs (z p)) (+ e 1f0))))

(defun-g voxelize-frag ((pos     :vec3)
                        (nor     :vec3)
                        &uniform
                        (ithing  :image-3d)
                        (props   :vec4)
                        (albedo  :vec3)
                        (cam-pos :vec3))
  (if (not (inside-cube-p pos 0f0))
      (return))
  (let* ((spec     (y props))
         (rough    (z props))
         (metallic (w props))
         (f0       (vec3 .04))
         (f0       (mix f0 albedo metallic))
         (color    (v! 0 0 0)))
    (setf color (pbr-spotlight-lum *light-pos*
                                   pos
                                   (normalize (- cam-pos pos))
                                   nor
                                   rough
                                   f0
                                   metallic
                                   albedo
                                   spec
                                   (* 3 *light-color*)
                                   *light-dir*
                                   1f0
                                   1.3
                                   .027 .0028))
    (let* ((voxel (scale-and-bias pos))
           (dim   (image-size ithing))
           (dxv   (* dim voxel))
           (alpha (pow 1f0 4f0))
           (res   (* alpha (v! color 1))))
      (image-store ithing
                   (ivec3 (int (x dxv))
                          (int (y dxv))
                          (int (z dxv)))
                   res)
      (values))))

(defpipeline-g voxelize-pipe ()
  :vertex   (voxelize-vert g-pnt)
  :geometry (voxelize-geom (:vec3 3))
  :fragment (voxelize-frag :vec3 :vec3))

(defun free-voxel ()
  (when *voxel-fbo*   (free *voxel-fbo*))
  (when *voxel-light* (free *voxel-light*)))

(defun init-voxel ()
  (free-voxel)
  (setf *voxel-fbo*
        (make-fbo `(:d :dimensions
                       ,(mapcar #'floor
                                (cdr (coerce *voxel-dimensions* 'list))))))
  (setf *voxel-light*
        (make-texture nil :dimensions (mapcar #'floor
                                              (coerce *voxel-dimensions* 'list))
                          :mipmap *voxel-mipmaps*
                          :element-type :rgba8))
  (setf *voxel-light-zam* (sample *voxel-light* ;;:minify-filter :linear
                                  :magnify-filter :nearest
                                  :wrap :clamp-to-border
                                  ))
  (setf *voxel-light-sam* (sample *voxel-light* ;;:minify-filter :linear
                                  :magnify-filter :nearest
                                  :wrap :clamp-to-border
                                  ))
  ;;(setf (cepl.samplers::border-color *voxel-light-sam*) (v! 0 0 0 1))
  ;;(setf (cepl.samplers::border-color *voxel-light-zam*) (v! 0 0 0 1))
  (setf (%cepl.types::%sampler-imagine *voxel-light-sam*) t))

(defun clear-voxel ()
  (%gl:clear-tex-image (texture-id *voxel-light*)
                       0
                       :rgba
                       :float
                       (cffi:null-pointer)))

(let ((stepper (make-stepper (seconds 1.5)
                             (seconds 1.5))))
  (defun draw-voxel ()
    (when (funcall stepper)
      (clear-voxel)
      (with-fbo-bound (*voxel-fbo* :attachment-for-size :d)
        (with-setf* ((depth-mask) nil
                     (depth-test-function) nil
                     (cull-face)  nil
                     (clear-color) (v! 0 0 0 1))
          (dolist (actor *actors*)
            (with-slots (buf scale color properties) actor
              (map-g #'voxelize-pipe buf
                     ;; - Vertex
                     :scale scale
                     :model-world (model->world actor)
                     ;; - Fragment
                     :props properties
                     :albedo color
                     :ithing *voxel-light-sam*
                     :cam-pos (pos *currentcamera*))))))
      (generate-mipmaps *voxel-light*))))

;; From Light POV


;; 2)
;; - normal vertex shader (model>world>view>clip)
;; - from visible surfaces?
;; - paths are traced thorugh the filtered direct lighting
;; - increasing lod/mipmap? each step
;; - accumulates:
;;   a) transparency (- 1 opacity)
;;   b) illumination (* emittance transparency)
;; - type of lighting/tracing
;;   a) diffuse, normal direction and 8 at 45° of it, form a hemisphere
;;   b) rough specular one direction wide cone
;;   c) fine specular one direction narrow cone
;;   d) shadow cones, determines how dense is the path (on non transparent voxels)
;; - trace every Nth pixel on the screen (member (range 4 16) n)
;; - trace ends when acumulated alpha is >1 or outside voxel
;; - traced color uses a "simple polynomial correction curve to alter the intensity"
;; #define SQRT2 1.414213
;; #define MIPMAP_HARDCAP 5.4f
;; #define VOXEL_SIZE (1/64.0)
(defun-g trace-diffuse-voxel-cone ((from        :vec3)
                                   (direction   :vec3)
                                   (voxel-light :sampler-3d))
  "Traces a diffuse voxel cone."
  (let* ((voxel-size     (/ 64f0))
         (mipmap-hardcap 5.4)
         (sqrt2          1.414213)
         ;;
         (direction      (normalize direction))
         (cone-spread    .325)
         (acc            (vec4 0f0))
         ;; Controls bleeding from close surfaces.
         ;; Low values look rather bad if using shadow cone tracing.
         ;; Might be a better choice to use shadow maps and lower this value.
         (dist           .1953125))
    ;; Trace
    (while (and (< dist sqrt2)
                (< (w acc) 1f0))
           (let* (;;(c (+ from (* dist direction)))
                  (c     (scale-and-bias (+ from (* dist direction))))
                  (l     (+ 1f0 (/ (* cone-spread dist) voxel-size)))
                  (level (log2 l))
                  (ll    (* (+ 1f0 level) (+ 1f0 level)))
                  (voxel (texture-lod voxel-light
                                      c
                                      (min mipmap-hardcap level))))
             (incf acc  (* 0.075 ll voxel (pow (- 1 (w voxel)) 2f0)))
             (incf dist (* ll voxel-size 2))))
    (pow (* (s~ acc :xyz) 2f0)
         (vec3 1.5))))

(defun-g orthogonal ((u :vec3))
  "Returns a vector that is orthogonal to u."
  (let ((u (normalize u))
        (v (v! .99146 .11664 .05832)))
    (if (> (abs (dot u v)) .99999)
        (cross u (v! 0 1 0))
        (cross u v))))

;; #define SQRT2 1.414213
;; #define ISQRT2 0.707106
;; #define DIFFUSE_INDIRECT_FACTOR 0.52f
;; #define VOXEL_SIZE (1/64.0)

;; Calculates indirect diffuse light using voxel cone tracing.
;; The current implementation uses 9 cones. I think 5 cones should be enough,
;; but it might generate more aliasing and bad blur.
(defun-g indirect-diffuse-light ((wpos        :vec3)
                                 (normal      :vec3)
                                 (voxel-light :sampler-3d)
                                 (albedo      :vec3))
  (let* ((isqrt2                  .707106)
         (voxel-size              (/ 64f0))
         (diffuse-indirect-factor .52)
         ;; Angle mix (1.0f => orthogonal direction,
         ;;            0.0f => direction of normal).
         (angle-mix               .5)
         (w                       (v! 1 1 1)) ; cone weights
         ;; Find a base for the side cones with the normal as one
         ;; of its base vectors.
         (ortho                   (normalize (orthogonal normal)))
         (ortho2                  (normalize (cross ortho normal)))
         ;; Find base vectors for the corner cones too.
         (corner                  (* .5 (+ ortho ortho2)))
         (corner2                 (* .5 (- ortho ortho2)))
         ;; Find start position of trace (start with a bit of offset).
         (n-offset                (* normal (+ 1 (* 4 isqrt2)) voxel-size))
         (c-origin                (+ wpos n-offset))
         ;; Accumulate indirect diffuse light.
         (acc                     (v! 0 0 0))
         ;; We offset forward in normal direction, and backward in cone direction.
         ;; Backward in cone direction improves GI, and forward direction removes
         ;; artifacts.
         (cone-offset             -0.01))
    ;; Trace front cone
    (incf acc (* (x w) (trace-diffuse-voxel-cone (+ c-origin (* cone-offset normal))
                                                 normal
                                                 voxel-light)))
    ;; Trace 4 side cones.
    (incf acc (* (y w) (trace-diffuse-voxel-cone (+ c-origin (* cone-offset ortho))
                                                 (mix normal ortho angle-mix)
                                                 voxel-light)))
    (incf acc (* (y w) (trace-diffuse-voxel-cone (- c-origin (* cone-offset ortho))
                                                 (mix normal (- ortho) angle-mix)
                                                 voxel-light)))
    (incf acc (* (y w) (trace-diffuse-voxel-cone (+ c-origin (* cone-offset ortho2))
                                                 (mix normal ortho2 angle-mix)
                                                 voxel-light)))
    (incf acc (* (y w) (trace-diffuse-voxel-cone (- c-origin (* cone-offset ortho2))
                                                 (mix normal (- ortho2) angle-mix)
                                                 voxel-light)))
    ;; Trace 4 corner cones.
    (incf acc (* (z w) (trace-diffuse-voxel-cone (+ c-origin (* cone-offset corner))
                                                 (mix normal corner angle-mix)
                                                 voxel-light)))
    (incf acc (* (z w) (trace-diffuse-voxel-cone (- c-origin (* cone-offset corner))
                                                 (mix normal (- corner) angle-mix)
                                                 voxel-light)))
    (incf acc (* (z w) (trace-diffuse-voxel-cone (+ c-origin (* cone-offset corner2))
                                                 (mix normal corner2 angle-mix)
                                                 voxel-light)))
    (incf acc (* (z w) (trace-diffuse-voxel-cone (- c-origin (* cone-offset corner2))
                                                 (mix normal (- corner2) angle-mix)
                                                 voxel-light)))
    ;; Return result.
    (* diffuse-indirect-factor
       acc
       (+ albedo .001))))






