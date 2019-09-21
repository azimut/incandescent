(in-package #:incandescent)

;; Inputs:
;; - *shadow-camera*

;; Reference:
;; - https://github.com/Friduric/voxel-cone-tracing/
;;
;; two passes, runs every frame
;; 1) light voxelization
;; 2) cone tracing
;;
;; Only works with things inside the -1,-1,-1 and 1,1,1 range

(defvar *voxel-fbo*        nil)
(defvar *voxel-light*      nil)
(defvar *voxel-light-sam*  nil)
(defvar *voxel-light-zam*  nil)

(defvar *voxel-stepper* nil)
(defvar *voxel-step-size* 1)

(defparameter *voxel-mipmaps*    7)
(defparameter *volume-dimension* 64)

(defun-g voxelize-vert ((vert g-pnt)
                        &uniform
                        (scale       :float)
                        (light-vp    :mat4)
                        (model-world :mat4))
  (let* ((mpos (v! (* scale (pos vert)) 1f0))
         (wpos (* model-world mpos))
         (norm (norm vert)))
    (values wpos
            (normalize (* (m4:to-mat3 model-world) norm))
            (* light-vp wpos))))

;;--------------------------------------------------

(defun-g voxelize-geom ((nor  (:vec3 3))
                        (lpos (:vec4 3)))
  (declare (output-primitive :kind :triangle-strip
                             :max-vertices 3))
  (let* ((p1 (- (s~ (gl-position (aref gl-in 1)) :xyz)
                (s~ (gl-position (aref gl-in 0)) :xyz)))
         (p2 (- (s~ (gl-position (aref gl-in 2)) :xyz)
                (s~ (gl-position (aref gl-in 0)) :xyz)))
         (p  (abs (cross p1 p2)))
         (wp (vec3 0f0)))
    (dotimes (i 3)
      (setf wp (s~ (gl-position (aref gl-in i)) :xyz))
      (cond ((and (> (z p) (x p))
                  (> (z p) (y p)))
             (emit () (v! (x wp) (y wp) 0 1)
                   wp (aref nor i) (aref lpos i)))
            ((and (> (x p) (y p))
                  (> (x p) (z p)))
             (emit () (v! (y wp) (z wp) 0 1)
                   wp (aref nor i) (aref lpos i)))
            (t
             (emit () (v! (x wp) (z wp) 0 1)
                   wp (aref nor i) (aref lpos i)))))
    (emit-vertex)
    (end-primitive)))

;;--------------------------------------------------

(defun-g scale-and-bias ((p :vec3))
  (+ .5 (* .5 p)))

(defun-g inside-cube-p ((p :vec3) (e :float))
  (and (< (abs (x p)) (+ e 1f0))
       (< (abs (y p)) (+ e 1f0))
       (< (abs (z p)) (+ e 1f0))))

(defun-g voxelize-frag ((pos         :vec3)
                        (nor         :vec3)
                        (lpos        :vec4)
                        &uniform
                        (light-pos   :vec3)
                        (light-dir   :vec3)
                        (light-color :vec3)
                        (shadowmap   :sampler-2d)
                        (ithing      :image-3d)
                        (props       :vec4)
                        (albedo      :vec3)
                        (cam-pos     :vec3))
  (if (not (inside-cube-p pos 0f0))
      (return))
  (let* ((spec     (y props))
         (rough    (z props))
         (metallic (w props))
         (f0       (vec3 .04))
         (f0       (mix f0 albedo metallic))
         (vis      (shadow-factor shadowmap lpos))
         (color    (v! 0 0 0)))
    (setf color (pbr-spotlight-lum light-pos
                                   pos
                                   (normalize (- pos cam-pos))
                                   nor
                                   rough
                                   f0
                                   metallic
                                   albedo
                                   spec
                                   (* *cone-mult* 2 light-color)
                                   light-dir
                                   *cone-inner*
                                   *cone-outer*
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
                   (v! (* vis (s~ res :xyz)) (w res)))
      (values))))

(defpipeline-g voxelize-pipe ()
  :vertex   (voxelize-vert g-pnt)
  :geometry (voxelize-geom (:vec3 3) (:vec4 3))
  :fragment (voxelize-frag :vec3 :vec3 :vec4))

(defun free-voxel ()
  (when *voxel-fbo*   (free *voxel-fbo*))
  (when *voxel-light* (free *voxel-light*)))

(defun init-voxel ()
  ;; NOTE: do not set it on a closure if you want to create an image
  (setf *voxel-stepper* (make-stepper (seconds *voxel-step-size*)
                                      (seconds *voxel-step-size*)))
  (free-voxel)
  (setf *voxel-fbo* (make-fbo `(:d :dimensions (,*volume-dimension*
                                                ,*volume-dimension*))))
  (setf *voxel-light* (make-texture nil :dimensions `(,*volume-dimension*
                                                      ,*volume-dimension*
                                                      ,*volume-dimension*)
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
  ;; (setf (cepl.samplers::border-color *voxel-light-sam*) (v! 0 0 0 1))
  ;; (setf (cepl.samplers::border-color *voxel-light-zam*) (v! 0 0 0 1))
  (setf (%cepl.types::%sampler-imagine *voxel-light-sam*) t))

(declaim (inline clear-voxel))
(defun clear-voxel ()
  (%gl:clear-tex-image (texture-id *voxel-light*)
                       0
                       :rgba
                       :float
                       (cffi:null-pointer)))

(defun draw-voxel ()
  (when (funcall *voxel-stepper*)
    (clear-voxel)
    (with-fbo-bound (*voxel-fbo* :attachment-for-size :d)
      (with-setf* ((depth-mask) nil
                   (depth-test-function) nil
                   (cull-face) nil
                   (clear-color) (v! 0 0 0 1))
        (dolist (actor *actors*)
          (with-slots (buf scale color properties) actor
            (map-g #'voxelize-pipe buf
                   ;; - Vertex
                   :scale scale
                   :light-vp (world->clip *shadow-camera*)
                   :model-world (model->world actor)
                   ;; - Fragment
                   :light-color *light-color*
                   :light-pos *light-pos*
                   :light-dir *light-dir*
                   :props properties
                   :albedo color
                   :ithing *voxel-light-sam*
                   :cam-pos (pos *currentcamera*)
                   :shadowmap *shadow-sam*)))))
    (generate-mipmaps *voxel-light*)))

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
(defun-g trace-diffuse-voxel-cone ((from        :vec3)
                                   (direction   :vec3)
                                   (voxel-light :sampler-3d))
  "Traces a diffuse voxel cone."
  (let* ((voxel-size     #.(/ 64f0))
         (mipmap-hardcap 5.4)
         (sqrt2          1.414213)
         ;;
         (direction      (normalize direction))
         (cone-spread    .325)
         (acc            (vec4 0f0))
         ;; Controls bleeding from close surfaces.
         ;; Low values look rather bad if using shadow cone tracing.
         ;; Might be a better choice to use shadow maps and lower this value.
         (dist           .0));.1953125
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

;; Calculates indirect diffuse light using voxel cone tracing.
;; The current implementation uses 9 cones. I think 5 cones should be enough,
;; but it might generate more aliasing and bad blur.
(defun-g indirect-diffuse-light ((wpos        :vec3)
                                 (normal      :vec3)
                                 (voxel-light :sampler-3d)
                                 (albedo      :vec3))
  (let* ((isqrt2                  .707106)
         (voxel-size              #.(/ 64f0))
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
       (+ albedo 0.001))))

(defun-g trace-specular-voxel-cone ((from        :vec3)
                                    (direction   :vec3)
                                    (normal      :vec3)
                                    (spec        :float)
                                    (voxel-light :sampler-3d))
  (let* ((max-distance (distance (abs from) (vec3 -1f0)))
         (mipmap-hardcap 5.4)
         ;;
         (direction (normalize direction))
         (offset (* 8 #.(/ 64f0)))
         (step  #.(/ 64f0))
         (from (+ from (* offset normal)))
         (acc (vec4 0f0))
         (dist offset))
    (while (and (< dist max-distance) (< (w acc) 1f0))
           (let ((c (+ from (* dist direction))))
             (if (not (inside-cube-p c 0f0))
                 (break))
             (setf c (scale-and-bias c))
             (let* ((level (* .1 spec (log2 (+ 1f0 (/ dist #.(/ 64f0))))))
                    (voxel (texture-lod voxel-light c (min level mipmap-hardcap)))
                    (f (- 1 (w acc))))
               (incf (s~ acc :xyz) (* .25 (+ spec) (s~ voxel :xyz) (w voxel) f))
               (incf (w acc)       (* .25 (w voxel) f))
               (incf dist          (* step (+ 1f0 (* .125 level)))))))
    (* 1f0 (pow (+ 1 spec) .8) (s~ acc :xyz))))

(defun-g indirect-specular-light ((view-direction :vec3)
                                  (normal         :vec3)
                                  (spec           :float)
                                  (voxel-light    :sampler-3d)
                                  (wpos           :vec3)
                                  (albedo         :vec3))
  (let ((reflection (normalize (reflect view-direction normal))))
    (* spec albedo (trace-specular-voxel-cone wpos
                                              reflection
                                              normal
                                              spec
                                              voxel-light))))
