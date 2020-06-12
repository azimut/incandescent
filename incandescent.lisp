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
        (v!   7  0.7    1.8))
  "Length=12
   X=Distance
   Y=Linear
   Z=Quadratic")

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

(defun set-dimensions-to-winsize ()
  (setf *dimensions*
        (mapcar #'round (coerce (v2:/s (surface-resolution (current-surface))
                                       2f0)
                                'list))))

(defun reload-base-fbos ()
  (when *fbo*  (free *fbo*))
  (setf *fbo*  (make-fbo `(0 :dimensions ,*dimensions*  :element-type :rgba16f)
                         `(:d :dimensions ,*dimensions*)))
  (setf *sam*  (sample (attachment-tex *fbo*  0) :wrap :clamp-to-edge))
  (setf *samd* (sample (attachment-tex *fbo* :d) :wrap :clamp-to-edge)))
