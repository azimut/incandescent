(in-package :incandescent)

;; TODO: position, rotation and might be 3d
;; TODO: real time depth test for all sprites to know the render order
(defparameter *sprite-blend*
  (make-blending-params))

(defclass sprite ()
  ((rows   :initform 1   :initarg :rows)
   (cols   :initform 1   :initarg :cols)
   (index  :initform 0   :initarg :index)
   (scale  :initform 1f0 :initarg :scale)
   (flip-x :initform 1   :initarg :flip-x)
   (flip-y :initform 1   :initarg :flip-y)
   (sam                  :initarg :sam)))

;;--------------------------------------------------

(defun make-sprite (sam &key (rows 1) (cols 1)
                             (index 0) (scale 1f0))
  (let ((obj (make-instance 'sprite-static
                            :sam sam
                            :index index
                            :scale scale
                            :rows rows :cols cols)))
    (push obj *actors*)
    obj))

;;--------------------------------------------------

(defmethod draw ((actor sprite) camera time)
  (with-setf (depth-mask) nil
    (with-blending *sprite-blend*
      (with-slots (sam rows cols index scale) actor
        (map-g #'sprite-pipe (get-quad-stream-v2)
               :sam sam
               :index index
               :scale scale
               :rows rows
               :cols cols)))))

;;--------------------------------------------------

(defun-g sprite-vert ((pos :vec2) &uniform
                      (scale :float)
                      (rows :int) (cols :int) (index :int))
  (multiple-value-bind (uv-scale uv-offset)
      (calc-uv-mod cols rows index)
    (values (v! (* scale pos) 0 1)
            (s~ (+ .5 (* .5 pos)) :xy)
            uv-scale
            uv-offset)))

(defun-g sprite-frag ((uv :vec2)
                      (uv-scale :vec2)
                      (uv-offset :vec2)
                      &uniform (sam :sampler-2d))
  (let* ((uv     (v! (x uv) (- (y uv))))
         (uv     (+ (* uv uv-scale) uv-offset))
         (color  (texture sam uv)))
    color))

;; size = (texture-size sam 0)
(defun-g pixel-uv ((gl-frag-coord-xy :vec2)
                   (size :vec2))
  (let* ((gl-fx    (x gl-frag-coord-xy))
         (gl-fy    (y gl-frag-coord-xy))
         (texcoord (/ gl-frag-coord-xy size))
         (pixel-size 3)
         (x (mod (int gl-fx) pixel-size))
         (y (mod (int gl-fy) pixel-size))
         (x (- (floor (/ pixel-size 2f0)) x))
         (y (- (floor (/ pixel-size 2f0)) y))
         (x (+ gl-fx x))
         (y (+ gl-fy y)))
    (/ (v! x y) size)))

(defpipeline-g sprite-pipe ()
  :vertex   (sprite-vert :vec2)
  :fragment (sprite-frag :vec2 :vec2 :vec2))

;;--------------------------------------------------

(defmethod update ((actor sprite) dt))
(defmethod update ((actor back1) dt))

(defclass back1 (sprite) ())
(defclass witch (sprite) ())

(defun make-back1 (sam &key (index 0))
  (let ((obj (make-instance 'back1 :sam sam :index index
                                   :cols 2 :rows 2)))
    (push obj *actors*)
    obj))

;;(test-sprite)

(defun test-sprite ()
  (free-actors)
  (make-back1
   (get-tex "/home/sendai/Downloads/Mobile - Fire Emblem Heroes - Askr Alternate Universe Forest Wall.png")
   :index 1)
  (make-back1
   (get-tex "/home/sendai/Downloads/Mobile - Fire Emblem Heroes - Askr Alternate Universe Forest Wall.png")
   :index 2))
