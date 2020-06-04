(in-package #:incandescent)

#+nil
(progn
  (free-actors)
  (in-scene 1 (make-mandelbrot :nr-colors 8 :iterations 256
                               :dimensions '(500 300)))
  (in-scene 0 (make-variation1 :nr-colors 6 :iterations 256
                               :dimensions '(500 300))))

(let ((stepper (make-stepper (seconds .1)
                             (seconds .1))))
  (defmethod update ((actor variation1) dt)
    (when (funcall stepper)
      (with-slots (scale seed center tcolors) actor
        (setf seed   (v! (* .01 (rocket-get "variation1:seed.x"))
                         (* .01 (rocket-get "variation1:seed.y"))))
        (setf scale  (* .001 (rocket-get "variation1:scale")))
        (setf center (v! (* .01 (rocket-get "variation1:center.x"))
                         (* .01 (rocket-get "variation1:center.y"))))))))

(let ((stepper (make-stepper (seconds .1)
                             (seconds .1))))
  (defmethod update ((actor mandelbrot) dt)
    (when (funcall stepper)
      (with-slots (scale center tcolors) actor
        (setf scale  (* .1 (rocket-get "mandelbrot:scale")))
        (setf center (v! (rocket-get "mandelbrot:center.x")
                         (rocket-get "mandelbrot:center.y")))))))
