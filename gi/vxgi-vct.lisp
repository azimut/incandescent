(in-package #:rtg-math.projection)


;; Like glm:ortho
(defn orthographic-v3 ((left   single-float)
                       (right  single-float)
                       (bottom single-float)
                       (top    single-float)
                       (near   single-float)
                       (far    single-float))
    mat4
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((result (m4:0!)))
    (setf (m4:melm result 0 0) (/ 2f0 (- right left))
          (m4:melm result 1 1) (/ 2f0 (- top bottom))
          (m4:melm result 0 3) (- (/ (+ right left) (- right left)))
          (m4:melm result 1 3) (- (/ (+ top bottom) (- top bottom)))
          (m4:melm result 2 2) (/ -2f0 (- far near))
          (m4:melm result 2 3) (- (/ (+ far near) (- far near)))
          (m4:melm result 3 3) 1f0)
    result))

(in-package #:incandescent)

(defparameter *volume-dimension* 64)
(defparameter *voxel-dimensions* (v3! *volume-dimension*))
(defparameter *voxel-mipmaps*    7)

(defparameter *volume-grid-size* 20f0)
(defparameter *half-size* (/ *volume-grid-size* 2f0))
(defparameter *voxel-scale*      (/ 1f0 *volume-grid-size*))
(defparameter *voxel-count*      (expt *volume-dimension* 3))
(defparameter *voxel-size*       (/ *volume-grid-size* *volume-dimension*))
(defparameter *voxel-space-origin* (v! 0 0 0))
(defparameter *voxel-space-size* 25f0)

(defparameter *geom-view-projections*
  (make-c-array
   (make-array
    3
    :element-type :mat4
    :initial-contents
    (list (m4:look-at (v! 0 1 0)  (v! *half-size* 0 0) (v! 0 0 0))
          (m4:look-at (v! 0 0 -1) (v! 0 *half-size* 0) (v! 0 0 0))
          (m4:look-at (v! 0 1 0)  (v! 0 0 *half-size*) (v! 0 0 0))))
   :element-type :mat4
   :dimensions 3))

(defparameter *geom-i-view-projections*
  (make-c-array
   (make-array
    3
    :element-type :mat4
    :initial-contents
    (list (m4:inverse (m4:look-at (v! 0 1 0)  (v! *half-size* 0 0) (v! 0 0 0)))
          (m4:inverse (m4:look-at (v! 0 0 -1) (v! 0 *half-size* 0) (v! 0 0 0)))
          (m4:inverse (m4:look-at (v! 0 1 0)  (v! 0 0 *half-size*) (v! 0 0 0)))))
   :element-type :mat4
   :dimensions 3))

;; https://github.com/sfreed141/vct

(defvar *point* nil)
(defvar *gpoint* nil)

(defvar *voxel-fbo*        nil)
(defvar *voxel-light*      nil)
(defvar *voxel-light-sam*  nil)
(defvar *voxel-light-zam*  nil)

(defparameter *volume-dimension* 64)

(defparameter *voxel-mipmaps* 6)
(defparameter *voxel-dimensions* (v3! *volume-dimension*))

(defparameter *center* (v! 0 0 0))
(defparameter *min*    (v! -20 -20 -20))
(defparameter *max*    (v! 20 20 20))

(defparameter *projection*
  (rtg-math.projection::orthographic-v3
   (x *min*) (x *max*)
   (y *min*) (y *max*)
   0f0 (- (z *max*) (z *min*))))

(defparameter *mvp_x*
  (m4:* *projection*
        (m4:look-at (v! 0 1 0)
                    (v3:+ (v! (x *max*) 0 0) *center*)
                    *center*)))
(defparameter *mvp_y*
  (m4:* *projection*
        (m4:look-at (v! 0 0 -1)
                    (v3:+ (v! 0 (y *max*) 0) *center*)
                    *center*)))
(defparameter *mvp_z*
  (m4:* *projection*
        (m4:look-at (v! 0 1 0)
                    (v3:+ (v! 0 0 (z *max*)) *center*)
                    *center*)))

;;--------------------------------------------------

(defun clear-voxel ()
  (%gl:clear-tex-image (texture-id *voxel-light*)
                       0
                       :rgba
                       :float
                       (cffi:null-pointer)))

(defun free-voxel ()
  (when *voxel-fbo*   (free *voxel-fbo*))
  (when *voxel-light* (free *voxel-light*)))

(defun init-voxel ()
  (free-voxel)
  ;; For instance rendering of debug voxels
  (unless *point*
    (setf *point* (make-buffer-stream
                   (make-gpu-array (list (v! 0 0 0)) :element-type :vec3
                                                     :dimensions 1)
                   :primitive :points)))
  (setf *voxel-fbo*
        (make-fbo `(:d :dimensions (,*volume-dimension* ,*volume-dimension*))))
  (setf *voxel-light*
        (make-texture nil :dimensions `(,*volume-dimension*
                                        ,*volume-dimension*
                                        ,*volume-dimension*)
                          :mipmap *voxel-mipmaps*
                          :element-type :rgba8))
  (setf *voxel-light-zam* (sample *voxel-light* :magnify-filter :linear
                                                :minify-filter :linear
                                                :wrap :clamp-to-edge
                                                ))
  (setf *voxel-light-sam* (sample *voxel-light* :magnify-filter :linear
                                                :minify-filter :linear
                                                :wrap :clamp-to-edge
                                                ))
  ;;(setf (cepl.samplers::border-color *voxel-light-sam*) (v! 1 1 1 1))
  ;;(setf (cepl.samplers::border-color *voxel-light-zam*) (v! 1 1 1 1))
  (setf (%cepl.types::%sampler-imagine *voxel-light-sam*) t)
  (clear-voxel))

;;--------------------------------------------------

(defun-g voxelize-vert ((vert g-pnt) &uniform
                        (scale       :float)
                        (model-world :mat4))
  (let ((pos  (* scale (pos vert)))
        (norm (norm vert)))
    (values (* model-world (v! pos 1))
            (normalize (* (m4:to-mat3 model-world) norm)))))

;;--------------------------------------------------

(defun-g voxelize-geom ((nor (:vec3 3))
                        &uniform
                        (mvp_x :mat4)
                        (mvp_y :mat4)
                        (mvp_z :mat4))
  (declare (output-primitive :kind :triangle-strip
                             :max-vertices 3))
  (let* (;; find dominant axis (using face normal)
         (face-normal (normalize (+ (aref nor 0)
                                    (aref nor 1)
                                    (aref nor 2))))
         (abs-normal  (abs face-normal))
         ;; since projecting onto std basis just find max component
         (mvp         (mat4 0))
         (axis        (int 0)))
    (cond ((and (> (x abs-normal) (y abs-normal))
                (> (x abs-normal) (z abs-normal)))
           (setf mvp mvp_x)
           (setf axis 0))
          ((and (> (y abs-normal) (x abs-normal))
                (> (y abs-normal) (z abs-normal)))
           (setf mvp mvp_y)
           (setf axis 1))
          (t
           (setf mvp mvp_z)
           (setf axis 2)))
    ;; project and emit vertices
    (dotimes (i 3)
      (emit () (* mvp (gl-position (aref gl-in i)))
            (s~ (gl-position (aref gl-in i)) :xyz)
            (aref nor i)
            (:flat axis)))
    (end-primitive)))

;;--------------------------------------------------

(defun-g image-atomic-rgba8-avg ((voxel-light :uimage-3d)
                                 (coord       :ivec3)
                                 (val         :vec4))
  (let ((val (v! (s~ val :xyz) (/ 255f0)))
        (new-val (pack-unorm4x8 val))
        (prev-stored-val (uint 0))
        (cur-stored-val  (uint 0)))
    ;; Spin(lock) wait while other threads modify
    (while (not (= (setf cur-stored-val
                         (image-atomic-comp-swap voxel-light
                                                 coord
                                                 prev-stored-val
                                                 new-val))
                   prev-stored-val))
           (setf prev-stored-val cur-stored-val)
           ;; Extract the moving average
           ;; (current average in rgb, normalized count in w)
           (let* ((avg (unpack-unorm4x8 cur-stored-val))
                  (avg (v! (/ (+ (* (s~ avg :xyz) (w avg))
                                 (* (s~ val :xyz) (w val))))
                           (+ (w avg) (w val)))))
             (setf new-val (pack-unorm4x8 avg))))))

(defun-g ndc-to-unit ((p :vec3))
  (* .5 (+ p 1f0)))

(defun-g voxel-linear-position ((wpos         :vec3)
                                (voxel-center :vec3)
                                (voxel-min    :vec3)
                                (voxel-max    :vec3))
  "Returns position of a voxel in texture coordinates. worldPosition assumed inside the voxel volume."
  (/ (- wpos voxel-center voxel-min)
     (- voxel-max voxel-min)))

(defun-g voxel-warp-fn ((x :vec3))
  (let* ((alpha .25)
         (x (+ (* alpha x)
               (* (- 3 (* 3 alpha)) x x)
               (* (- (* 2 alpha) 2) x x x))))
    (clamp x 0 1)))

(defun-g voxel-warp ((p-tc :vec3)
                     (c-tc :vec3))
  (let* ((offset (- p-tc c-tc))
         (offset (+ .5 (* .5 offset)))
         (offset (voxel-warp-fn offset))
         (offset (- (* 2 offset) 1)))
    (+ c-tc offset)))

(defun-g get-voxel-position ((size    :ivec3)
                             (pos     :vec3)
                             (axis    :int)
                             (cam-pos :vec3))
  (let* ((ndc  pos)
         (unit (ndc-to-unit ndc))
         (voxel-center *center*)
         (voxel-min    *min*)
         (voxel-max    *max*))
    ;; swizzle based on projected axis
    (cond ((= axis 0) ; looking down x axis
           (setf unit (v! (- 1 (z unit)) (y unit) (x unit))))
          ((= axis 1) ; looking down y axis
           (setf unit (v! (x unit) (- 1 (z unit)) (y unit)))))
    (setf (z unit) (- 1 (z unit)))
    (* size (voxel-warp unit (voxel-linear-position cam-pos
                                                    voxel-center
                                                    voxel-min
                                                    voxel-max)))))

(defun-g voxelize-frag ((pos  :vec3)
                        (nor  :vec3)
                        (axis :int)
                        &uniform
                        (voxel-image :uimage-3d)
                        (cam-pos     :vec3))
  (let* (;;(normal (* .5 (+ 1 (normalize nor))))
         (normal (normalize nor))
         ;;(final-color (v! .1 1 .1))
         ;;#+nil
         (final-color (dir-light-apply (v! .5 .5 .5)
                                       (* 2 *light-color*)
                                       (v! 50 50 50)
                                       pos
                                       normal))
         ;; Original vct code uses warping??
         ;;#+nil
         (voxel-position (get-voxel-position (image-size voxel-image)
                                             pos
                                             axis
                                             cam-pos))
         ;;(voxel-position (v! 0 0 0))
         (voxel-index (ivec3 (int (x voxel-position))
                             (int (y voxel-position))
                             (int (z voxel-position)))))
    (image-atomic-rgba8-avg voxel-image
                            voxel-index
                            (v! final-color 2))))

(defpipeline-g voxelize-pipe ()
  :vertex   (voxelize-vert g-pnt)
  :geometry (voxelize-geom (:vec3 3))
  :fragment (voxelize-frag :vec3 :vec3 :int))

;;--------------------------------------------------
(let ((stepper (make-stepper (seconds 1.5)
                             (seconds 1.5))))
  (defun draw-voxel ()
    (when (funcall stepper)
      (clear-voxel)
      (with-fbo-bound (*voxel-fbo* :attachment-for-size :d)
        (with-setf* (;;(depth-mask) nil
                     (depth-test-function) nil
                     (cull-face) nil
                     (clear-color) (v! 0 0 0 1))
          (dolist (actor *actors*)
            (with-slots (buf scale) actor
              (map-g #'voxelize-pipe buf
                     :scale scale
                     :model-world (model->world actor)
                     :cam-pos (pos *camera*)
                     :mvp_x *mvp_x*
                     :mvp_y *mvp_y*
                     :mvp_z *mvp_z*
                     :voxel-image *voxel-light-sam*)))))
      (generate-mipmaps *voxel-light*))))

;;--------------------------------------------------
(defun-g calculate-axis ()
  (let* ((p1 (- (s~ (gl-position (aref gl-in 1)) :xyz)
                (s~ (gl-position (aref gl-in 0)) :xyz)))
         (p2 (- (s~ (gl-position (aref gl-in 2)) :xyz)
                (s~ (gl-position (aref gl-in 0)) :xyz)))
         (face-normal (cross p1 p2))
         (n-dx (abs (x face-normal)))
         (n-dy (abs (y face-normal)))
         (n-dz (abs (z face-normal))))
    (cond ((and (> n-dx n-dy) (> n-dx n-dz))
           (int 0))
          ((and (> n-dy n-dx) (> n-dy n-dz))
           (int 1))
          (t
           (int 2)))))

;; https://github.com/jose-villegas/VCTRenderer
(defun-g voxelize-geom ((nor (:vec3 3))
                        &uniform
                        (voxel-scale        :float)
                        (volume-dimension   :float)
                        (view-projections   (:mat4 3))
                        (i-view-projections (:mat4 3)))
  (declare (output-primitive :kind :triangle-strip
                             :max-vertices 3))
  (let* ((half-pixel (vec2 (/ 1f0 volume-dimension)))
         (planes (vector (v! 0 0 0) (v! 0 0 0) (v! 0 0 0)))
         (intersection (vector (v! 0 0 0) (v! 0 0 0) (v! 0 0 0)))
         (z (v! 0 0 0))
         ;;
         (selected-index (calculate-axis))
         (view-projection (aref view-projections selected-index))
         (i-view-projection (aref i-view-projections selected-index))
         (pos (vector (* view-projection (gl-position (aref gl-in 0)))
                      (* view-projection (gl-position (aref gl-in 1)))
                      (* view-projection (gl-position (aref gl-in 2)))))
         (triangle-plane
           (v! (normalize
                (cross (- (s~ (aref pos 1) :xyz) (s~ (aref pos 0) :xyz))
                       (- (s~ (aref pos 2) :xyz) (s~ (aref pos 0) :xyz))))
               0))
         (triangle-plane (v! (s~ triangle-plane :xyz)
                             (- (dot (s~ (aref pos 0) :xyz)
                                     (s~ triangle-plane :xyz))))))
    ;; change winding, otherwise there are artifacts for the back faces.
    (when (< (dot (s~ triangle-plane :xyz) (v! 0 0 1)) 0f0)
      (let ((vertex-temp (aref pos 2)))
        (setf (aref pos 2) (aref pos 1))
        (setf (aref pos 1) vertex-temp)))
    (when (= (z triangle-plane) 0f0)
      (return))
    ;; calculate the plane through each edge of the triangle
    ;; in normal form for dilatation of the triangle
    (setf (aref planes 0) (cross (- (s~ (aref pos 0) :xyz) (s~ (aref pos 2) :xyz))
                                 (s~ (aref pos 2) :xyz)))
    (setf (aref planes 1) (cross (- (s~ (aref pos 1) :xyz) (s~ (aref pos 0) :xyz))
                                 (s~ (aref pos 0) :xyz)))
    (setf (aref planes 2) (cross (- (s~ (aref pos 2) :xyz) (s~ (aref pos 1) :xyz))
                                 (s~ (aref pos 1) :xyz)))
    (decf (z (aref planes 0)) (dot half-pixel (abs (s~ (aref planes 0) :xy))))
    (decf (z (aref planes 1)) (dot half-pixel (abs (s~ (aref planes 1) :xy))))
    (decf (z (aref planes 2)) (dot half-pixel (abs (s~ (aref planes 2) :xy))))
    ;; calculate intersection between translated planes
    (setf (aref intersection 0) (cross (aref planes 0) (aref planes 1)))
    (setf (aref intersection 1) (cross (aref planes 1) (aref planes 2)))
    (setf (aref intersection 2) (cross (aref planes 2) (aref planes 0)))
    (divf (aref intersection 0) (vec3 (z (aref intersection 0))))
    (divf (aref intersection 1) (vec3 (z (aref intersection 1))))
    (divf (aref intersection 2) (vec3 (z (aref intersection 2))))
    ;;
    (setf z (v! (/ (- (+ (* (x (aref intersection 0))
                            (x triangle-plane))
                         (* (y (aref intersection 0))
                            (y triangle-plane))
                         (w triangle-plane)))
                   (z triangle-plane))
                (/ (- (+ (* (x (aref intersection 1))
                            (x triangle-plane))
                         (* (y (aref intersection 1))
                            (y triangle-plane))
                         (w triangle-plane)))
                   (z triangle-plane))
                (/ (- (+ (* (x (aref intersection 2))
                            (x triangle-plane))
                         (* (y (aref intersection 2))
                            (y triangle-plane))
                         (w triangle-plane)))
                   (z triangle-plane))))
    (setf (s~ (aref pos 0) :xyz) (v! (s~ (aref intersection 0) :xy)
                                     (x z)))
    (setf (s~ (aref pos 1) :xyz) (v! (s~ (aref intersection 1) :xy)
                                     (y z)))
    (setf (s~ (aref pos 2) :xyz) (v! (s~ (aref intersection 2) :xy)
                                     (z z)))
    ;;
    (setf (s~ (aref pos 0) :xyz) (v! (s~ (aref intersection 0) :xy) (x z)))
    (setf (s~ (aref pos 1) :xyz) (v! (s~ (aref intersection 1) :xy) (y z)))
    (setf (s~ (aref pos 2) :xyz) (v! (s~ (aref intersection 2) :xy) (z z)))
    ;;
    (dotimes (i 3)
      (let* ((voxel-pos (* i-view-projection (aref pos i)))
             (voxel-pos (v! (/ (s~ voxel-pos :xyz) (w voxel-pos))
                            (w voxel-pos)))
             (voxel-pos (* voxel-pos voxel-scale)))
        (emit ()
              (aref pos i)
              (* (s~ voxel-pos :xyz) volume-dimension)
              (aref nor i))))
    (end-primitive)
    (values)))

(defun-g voxelize-frag ((pos     :vec3)
                        (nor     :vec3)
                        &uniform
                        (ithing  :image-3d)
                        (cam-pos :vec3))
  #+nil
  (if (not (inside-cube-p pos 0f0))
      (return))
  ;;#+nil
  (when (or (> (abs (z pos)) 10)
            (> (abs (x pos)) 10)
            (> (abs (y pos)) 10))
    (discard))
  (let ((color       (v! 0 0 0))
        (albedo      (v! .1 .1 .1))
        (light-color (v! 30 30 30))
        (light-pos   (v! 0 2 0)))
    (setf color (point-light-apply albedo
                                   light-color
                                   light-pos
                                   pos
                                   (normalize nor)
                                   1f0
                                   .22
                                   .20
                                   cam-pos
                                   .1
                                   1))
    (let* ((voxel (scale-and-bias pos))
           ;;(voxel pos)
           (dim   (image-size ithing))
           (dxv   (* dim voxel))
           ;;
           (alpha (pow 1f0 4f0))
           (res   (* alpha (v! color 1))))
      ;;#+nil
      (image-store ithing
                   (ivec3 (int (x dxv))
                          (int (y dxv))
                          (int (z dxv)))
                   ;;res
                   (v4! 1)
                   )
      (values))))


;;

;;(v-def-glsl-template-fun >> (x y) "(~a >> ~a)" (v-uint v-uint) v-uint :pure t)
;;(v-def-glsl-template-fun << (x y) "(~a << ~a)" (v-uint v-uint) v-uint :pure t)
#+nil
(defun-g conv-vec4-to-rgba8 ((val :vec4))
  (let* ((bits (logior (<< (logand (uint (w val)) (uint #x000000FF)) (uint 24))
                       (<< (logand (uint (z val)) (uint #x000000FF)) (uint 16))))
         (bits (logior bits
                       (<< (logand (uint (y val)) (uint #x000000FF)) (uint 8))))
         (bits (logior bits
                       (logand (uint (x val)) (uint #x000000FF)))))
    bits))
#+nil
(defun-g conv-rgba8-to-vec4 ((val :uint))
  (v! (float (logand val (uint #x000000FF)))
      (float (>> (logand val (uint #x0000FF00)) (uint 8)))
      (float (>> (logand val (uint #x00FF0000)) (uint 16)))
      (float (>> (logand val (uint #xFF000000)) (uint 24)))))


#+nil
(defun-g image-atomic-rgba8-avg ((voxel-light :uimage-3d)
                                 (coord :ivec3)
                                 (val   :vec4))
  (let ((val (v! (* 255 (s~ val :xyz)) (w val)))
        (new-val (conv-vec4-to-rgba8 val))
        (prev-stored-val (uint 0))
        (cur-stored-val  (uint 0)))
    (while (not (= (setf cur-stored-val (image-atomic-comp-swap voxel-light
                                                                coord
                                                                prev-stored-val
                                                                new-val))
                   prev-stored-val))
           (setf prev-stored-val cur-stored-val)
           (let* ((rval (conv-rgba8-to-vec4 cur-stored-val))
                  (rval (v! (* (w rval) (s~ rval :xyz))
                            (w rval)))
                  (cur-val-f (+ rval val))
                  (cur-val-f (v! (/ (s~ cur-val-f :xyz) (w cur-val-f))
                                 (w cur-val-f))))
             (setf new-val (conv-vec4-to-rgba8 cur-val-f))))))
