(in-package #:incandescent)

;; Reference: https://github.com/selfshadow/ltc_code
;; ltc-demo.zip - ltc.sh - helpers.sh

(defvar *t-ltc1* nil)
(defvar *t-ltc2* nil)
(defvar *s-ltc1* nil)
(defvar *s-ltc2* nil)

(defstruct-g rect
  (center :vec3)
  (dirx   :vec3)
  (diry   :vec3)
  (halfx  :float)
  (halfy  :float)
  (plane  :vec4))

(defstruct-g ray
  (origin :vec3)
  (dir    :vec3))

;; bool - float
(defun-g ray-plane-intersect ((ray   ray)
                              (plane :vec4))
  (let ((t0 (/ (- (dot plane (v! (ray-origin ray) 1)))
               (dot (s~ plane :xyz) (ray-dir ray)))))
    (values (> t0 0)
            t0)))

;; bool - float
(defun-g ray-rect-intersect ((ray  ray)
                             (rect rect))
  (multiple-value-bind (intersect-p t0)
      (ray-plane-intersect ray (rect-plane rect))
    (when intersect-p
      (let* ((pos (+ (ray-origin ray) (* t0 (ray-dir ray))))
             (lpos (- pos (rect-center rect)))
             (x (dot lpos (rect-dirx rect)))
             (y (dot lpos (rect-diry rect))))
        (when (or (> (abs x) (rect-halfx rect))
                  (> (abs y) (rect-halfy rect)))
          (setf intersect-p nil))))
    (values intersect-p
            t0)))

(defun-g generate-camera-ray ((resolution :vec2)
                              (view       :mat4))
  (let* ((xy  (- (/ (* 2 (s~ gl-frag-coord :xy)) resolution) 1))
         (ray (make-ray (vec3 0) (normalize (v! xy 2))))
         (focal-distance 2f0)
         (ft  (/ focal-distance (z (ray-dir ray))))
         (p-focus (* ft (ray-dir ray))))
    (setf (ray-dir ray) (normalize (- p-focus (ray-origin ray))))
    (setf (ray-origin ray) (s~ (* view (v! (ray-origin ray) 1)) :xyz))
    (setf (ray-dir ray) (s~ (* view (v! (ray-dir ray) 0)) :xyz))
    ray))

(defun-g rotation-y ((v :vec3) (a :float))
  (let ((r (v! (+ (* (x v) (cos a))
                  (* (z v) (sin a)))
               (y v)
               (+ (- (* (x v) (sin a)))
                  (* (z v) (cos a))))))
    r))

(defun-g rotation-z ((v :vec3) (a :float))
  (let ((r (v! (- (* (x v) (cos a))
                  (* (y v) (sin a)))
               (+ (* (x v) (sin a))
                  (* (y v) (cos a)))
               (z v))))
    r))

(defun-g rotation-yz ((v :vec3) (ay :float) (az :float))
  (rotation-z (rotation-y v ay) az))

(defun free-ltc ()
  (when *t-ltc1* (free *t-ltc1*))
  (when *t-ltc2* (free *t-ltc2*)))

(defun init-ltc ()
  (free-ltc)
  (setf *t-ltc1*
        (make-texture
         (let ((tmp (make-array
                     '(64 64)
                     :initial-element (v! 0 0 0 0))))
           (dotimes (i 64)
             (dotimes (j 64)
               (setf (aref tmp i j) (nth (+ i j) *ltc1*))))
           tmp)
         :dimensions '(64 64)
         :element-type :rgba32f))
  (setf *s-ltc1* (sample *t-ltc1* :minify-filter :nearest :magnify-filter :linear
                                  :wrap :clamp-to-edge))
  (setf *t-ltc2*
        (make-texture
         (let ((tmp (make-array
                     '(64 64)
                     :initial-element (v! 0 0 0 0))))
           (dotimes (i 64)
             (dotimes (j 64)
               (setf (aref tmp i j) (nth (+ i j) *ltc2*))))
           tmp)
         :dimensions '(64 64)
         :element-type :rgba32f))
  (setf *s-ltc2* (sample *t-ltc2* :minify-filter :nearest :magnify-filter :linear
                                  :wrap :clamp-to-edge))
  t)

(defun-g ltc-coords ((cos-theta :float) (roughness :float))
  (let* ((theta (acos cos-theta))
         (coords (v! roughness (/ theta (* .5 +pi+))))
         (lut-size 32f0))
    (* coords (+ (/ (- lut-size 1f0) lut-size)
                 (/ .5 lut-size)))))

(defun-g ltc-matrix ((tex-lsd-mat :sampler-2d) (coord :vec2))
  (let* ((t0 (texture tex-lsd-mat coord))
         (x0 (x t0))
         (y0 (y t0))
         (z0 (z t0))
         (w0 (w t0)))
    (m3:from-columns (v! 1  0  y0)
                     (v! 0  z0 0)
                     (v! w0 0  x0))))
