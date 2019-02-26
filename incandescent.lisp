;;;; incandescent.lisp

(in-package #:incandescent)

(defun fmod (x y)
  (- x (* y (floor (/ x y)))))
