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
                #:maphash-values
                #:positive-fixnum
                #:emptyp
                #:when-let
                #:first-elt
                #:mappend
                #:length=)
  (:import-from #:serapeum
                #:do-each
                #:class-name-of)
  (:import-from #:with-setf
                #:with-setf
                #:with-setf*))
