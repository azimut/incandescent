;;;; package.lisp

(uiop:define-package #:incandescent
    (:use #:cl
          #:cepl
          #:rtg-math
          #:vari
          #:nineveh
          #:cepl.skitter
          #:livesupport)
  (:import-from #:temporal-functions
                #:make-stepper
                #:seconds)
  (:import-from #:alexandria
                #:emptyp
                #:when-let
                #:first-elt
                #:mappend
                #:length=)
  (:import-from #:serapeum
                #:class-name-of)
  (:import-from #:with-setf
                #:with-setf
                #:with-setf*))
