;;;; package.lisp

(uiop:define-package #:incandescent
    (:use #:cl
          #:cepl
          #:rtg-math
          #:vari
          #:nineveh
          #:cepl.skitter
          #:livesupport)
  (:import-from #:alexandria
                #:first-elt
                #:mappend
                #:length=)
  (:import-from #:with-setf
                #:with-setf
                #:with-setf*))
