;;;; package.lisp

(uiop:define-package #:incandescent
  (:use #:cl
        #:cepl
        #:rtg-math
        #:vari
        #:nineveh
        #:cepl.skitter
        #:with-setf
        #:livesupport)
  (:export #:main #:init)
  (:import-from #:temporal-functions
                #:make-stepper
                #:seconds)
  (:import-from #:arrow-macros
                #:->)
  (:import-from #:alexandria
                #:maphash-values
                #:maphash-keys
                #:hash-table-keys
                #:positive-fixnum
                #:emptyp
                #:when-let
                #:when-let*
                #:if-let
                #:first-elt
                #:mappend
                #:length=)
  (:import-from #:serapeum
                #:op
                #:do-each
                #:vect
                #:random-in-range
                #:class-name-of))
