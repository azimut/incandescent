;;;; incandescent.lisp

(in-package #:incandescent)

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

(defun resolve-path (path)
  (let ((realpath (or (uiop:absolute-pathname-p path)
                      (asdf:system-relative-pathname :incandescent path))))
    (assert (probe-file realpath))
    realpath))

(defun dimensions= (&rest rest)
  (when rest
    (serapeum:seq= (mapcar (lambda (obj) (dimensions obj))
                           rest))))
