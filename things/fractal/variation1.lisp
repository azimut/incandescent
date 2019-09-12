(in-package #:incandescent)

(defclass variation1 (fractal)
  ((seed :initform (v! 1 1) :initarg :seed)))

(defun make-variation1 (&key (nr-colors 32)
                             (iterations 128)
                             (dimensions '(352 192)))
  (declare (type unsigned-byte nr-colors iterations))
  (let* ((dimension-x (floor (/ (first dimensions) 16)))
         (dimension-y (floor (/ (second dimensions) 16)))
         (dimensions  (if (and (> dimension-x 1) (> dimension-y 1))
                          (list (* 16 dimension-x)
                                (* 16 dimension-y))
                          '(352 192)))
         (obj (make-instance 'variation1 :nr-colors nr-colors
                                         :dimensions dimensions
                                         :iterations iterations)))
    (push obj *actors*)
    obj))

(let ((stepper (make-stepper (seconds .05)
                             (seconds .05))))
  (defmethod draw ((actor variation1) camera time)
    ;; Draw to texture
    (with-slots (dimensions zam scolors seed center scale iterations) actor
      (when (funcall stepper)
        (map-g #'variation1-pipe (make-compute-space (/ (first dimensions) 16)
                                                     (/ (second dimensions) 16))
               :dst zam
               :colors scolors
               :center center
               :seed seed
               :scale scale
               :max-iterations iterations)))
    ;; Draw to screen
    (with-slots (sam) actor
      (map-g #'pass-pipe *bs*
             :sam sam))))

(defun-g variation1-compute (&uniform
                             (colors         :sampler-1d)
                             (dst            :image-2d)
                             ;;
                             (center         :vec2)
                             (seed           :vec2)
                             (scale          :float)
                             (max-iterations :int))
  (declare (local-size :x 16
                       :y 16
                       :z 1))
  (let ((thread-id (ivec2 (int (x gl-global-invocation-id))
                          (int (y gl-global-invocation-id))))
        (dst-size  (image-size dst)))

    (when (any (greater-than thread-id dst-size))
      (return))

    (let* ((z (v! 0 0))
           #+nil
           (z (- (* scale (/ (v! (float (x thread-id))
                                 (float (y thread-id)))
                             (min (x dst-size)
                                  (y dst-size))))
                 center))
           ;;#+nil
           (z (- (* scale (/ (v! (* 3 (float (x thread-id)))
                                 (* 2 (float (y thread-id))))
                             (v! (float (x dst-size))
                                 (float (y dst-size)))))
                 center))
           (i (int 0)))
      (while (< (setf i (+ i 1)) max-iterations)
             (let ((x (+ (- (* (x z) (x z))
                            (* (y z) (y z)))
                         (x seed)))
                   (y (+ (+ (* (y z) (x z))
                            (* (x z) (y z)))
                         (y seed))))
               (when (> (+ (* x x) (* y y))
                        4f0)
                 (break))
               (setf (x z) x)
               (setf (y z) y)))
      (image-store dst
                   thread-id
                   (texture colors
                            (if (= i max-iterations)
                                0f0
                                (/ i
                                   ;;100f0
                                   (float max-iterations)
                                   ))))))
  (values))

(defpipeline-g variation1-pipe ()
  :compute variation1-compute)
