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
                #:emptyp
                #:when-let
                #:first-elt
                #:mappend
                #:length=)
  (:import-from #:with-setf
                #:with-setf
                #:with-setf*))
