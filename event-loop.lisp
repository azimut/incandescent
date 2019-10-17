(in-package #:incandescent)

;; seriously consider copy "trial" one

(defclass event (actor)
  ()
  (:default-initargs
   :draw-p nil
   :shadow-p nil
   :voxelize-p nil))

(defun add-event (obj)
  ;; Add only if event isn't there
  (pushnew obj *actors*
           :test (lambda (a b) (eq (serapeum:class-name-of a)
                              (serapeum:class-name-of b)))))
