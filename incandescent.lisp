;;;; incandescent.lisp

(in-package #:incandescent)

(defvar *vec3-right*   (v!  1  0  0))
(defvar *vec3-left*    (v! -1  0  0))
(defvar *vec3-up*      (v!  0  1  0))
(defvar *vec3-down*    (v!  0 -1  0))
(defvar *vec3-forward* (v!  0  0  1))
(defvar *vec3-back*    (v!  0  0 -1))

(defun fmod (x y)
  (- x (* y (floor (/ x y)))))

(defun resolve-path (path)
  (let ((realpath (or (uiop:absolute-pathname-p path)
                      (asdf:system-relative-pathname :incandescent path))))
    (assert (probe-file realpath))
    realpath))
