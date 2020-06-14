;;;; incandescent.lisp

(in-package #:incandescent)

(defvar *last-time* nil)
(defvar *bs* nil)
(defvar *dimensions* (list (* 16 20) (* 16 15)))

(defvar *actors* nil)
(defvar *scenes* (make-array 5 :initial-element nil))
(defvar *scene-index* 0)

(defvar *fbo*  nil)
(defvar *sam*  nil)
(defvar *sam1* NIL)
(defvar *sam2* NIL)
(defvar *sam3* NIL)
(defvar *sam4* NIL)
(defvar *sam5* NIL)
(defvar *samd* nil)

(defparameter *exposure* 2f0)

(defvar *vec3-right*   (v!  1  0  0))
(defvar *vec3-left*    (v! -1  0  0))
(defvar *vec3-up*      (v!  0  1  0))
(defvar *vec3-down*    (v!  0 -1  0))
(defvar *vec3-forward* (v!  0  0  1))
(defvar *vec3-back*    (v!  0  0 -1))

(defvar *quad-stream-v3-data*
  (list (v! -1.0   1.0 0)
        (v! -1.0  -1.0 0)
        (v!  1.0  -1.0 0)
        (v! -1.0   1.0 0)
        (v!  1.0  -1.0 0)
        (v!  1.0   1.0 0)))

(defvar *point-light-params*
  (list (v! 3250 0.0014 0.000007)
        (v!  600 0.007  0.0002)
        (v!  325 0.014  0.0007)
        (v!  200 0.022  0.0019)
        (v!  160 0.027  0.0028)
        (v!  100 0.045  0.0075)
        (v!   65 0.07   0.017)
        (v!   50 0.09   0.032)
        (v!   32 0.14   0.07)
        (v!   20 0.22   0.20)
        (v!   13 0.35   0.44)
        (v!    7 0.7    1.8))
  "Length=12
   X=Distance
   Y=Linear
   Z=Quadratic")

(defparameter *sample-colors*
  (list
   :light-candle                    (v!  1.0       0.5764706  0.16078432)
   :light-tungsten-40               (v!  1.0       0.7725491  0.56078434)
   :light-tungsten-100              (v!  1.0       0.83921576 0.6666667)
   :light-halogen                   (v!  1.0       0.9450981  0.87843144)
   :light-carbon-arc                (v!  1.0       0.9803922  0.9568628)
   :light-high-noon-sun             (v!  1.0       1.0        0.9843138)
   :light-direct-sunlight           (v!  1.0       1.0        1.0)
   :light-sky-overcast              (v!  0.7882353 0.8862746  1.0)
   :light-sky-clear-blue            (v!  0.2509804 0.6117647  1.0)
   :light-fluorescent-warm          (v!  1.0       0.9568628  0.8980393)
   :light-fluorescent-standard      (v!  0.9568628 1.0        0.9803922)
   :light-fluorescent-cool-white    (v!  0.8313726 0.9215687  1.0)
   :light-fluorescent-full-spectrum (v!  1.0       0.9568628  0.9490197)
   :light-fluorescent-grow-light    (v!  1.0       0.93725497 0.9686275)
   :light-fluorescent-black-light   (v!  0.654902  0.0        1.0)
   :light-vapor-mercury             (v!  0.8470589 0.9686275  1.0)
   :light-vapor-sodium              (v!  1.0       0.8196079  0.69803923)
   :light-metal-halide              (v!  0.9490197 0.98823535 1.0)
   :light-high-pressure-sodium      (v!  1.0       0.7176471  0.29803923))
  "http://planetpixelemporium.com/tutorialpages/light.html")

(defun get-light-color (color)
  (getf *sample-colors* color))

(defparameter *light-color* (get-light-color :light-tungsten-100))
(defparameter *light-pos*   (v! 0 0 0))

(let ((stream nil))
  (defun get-quad-stream-v3 ()
    (declare (optimize (speed 3) (safety 1) (debug 1)))
    (unless stream
      (setf stream (make-buffer-stream
                    (make-gpu-array *quad-stream-v3-data*
                                    :element-type :vec3
                                    :dimensions 6)
                    :retain-arrays t)))
    stream))

(defun fmod (x y)
  (- x (* y (floor (/ x y)))))

(defun resolve-path (path &optional (assert-p t))
  (let ((realpath (or (uiop:absolute-pathname-p path)
                      (asdf:system-relative-pathname :incandescent path))))
    (when assert-p
      (assert (probe-file realpath)))
    realpath))

(defun dimensions= (&rest rest)
  (when rest
    (serapeum:seq= (mapcar (lambda (obj) (dimensions obj))
                           rest))))

(defun map-range (a1 a2 b1 b2 s)
  (+ b1
     (/ (* (- s a1)
	   (- b2 b1))
	(- a2 a1))))

(defun cpu-1-tone-map-reinhard (color1 exposure)
  (declare (type single-float color1 exposure))
  (let* ((col (* color1 exposure))
         (r   (/ col (+ 1f0 col))))
    (expt r #.(/ 2.2))))

;; https://gamedev.stackexchange.com/questions/88285/how-does-vector3-angle-compute-the-resulting-angle
(defun v3-angle (v1 v2)
  "angle between vectors...not signed?"
  (declare (type rtg-math.types:vec3 v1 v2))
  (acos (v3:dot (v3:normalize v1) (v3:normalize v2))))

(defun set-dimensions-to-winsize (&optional (divisor 2f0))
  (setf *dimensions*
        (mapcar #'round (coerce (v2:/s (surface-resolution (current-surface))
                                       divisor)
                                'list))))

(defun reload-base-fbos ()
  (when *fbo*  (free *fbo*))
  (setf *fbo*  (make-fbo `(0 :dimensions ,*dimensions*  :element-type :rgba16f)
                         `(:d :dimensions ,*dimensions*)))
  (setf *sam*  (sample (attachment-tex *fbo*  0) :wrap :clamp-to-edge))
  (setf *samd* (sample (attachment-tex *fbo* :d) :wrap :clamp-to-edge)))

(defun random-qrotation ()
  (q:from-axis-angle
   (v! (random 1f0)
       (random 1f0)
       (random 1f0))
   (radians (random 360))))

(defun random-in-range-poked (low high poke-size)
  (assert (> high low))
  (assert (> (- high low) poke-size))
  (let ((r (random (- high (* poke-size .5)))))
    (if (> (random 1f0) .5)
        (+ low r)
        (- high r))))
