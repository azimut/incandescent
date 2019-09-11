(in-package #:incandescent)

;; NEEDS CEPL/VARJO hacks for currently unsupported image store

(defclass mandelbrot (actor)
  (tex
   sam
   zam
   tcolors
   scolors
   (nr-colors  :initarg :nr-colors)
   (iterations :initarg :iterations)
   (center     :initarg :center)
   (dimensions :initarg :dimensions))
  (:default-initargs
   :dimensions '(320 240)
   :nr-colors 32
   :center (v! 0 0)
   :iterations 128
   :scale .5))

(defmethod free ((actor mandelbrot))
  (with-slots (tex tcolors) actor
    (free tex)
    (free tcolors)))

(defmethod initialize-instance :after ((obj mandelbrot) &key)
  (with-slots (tex sam zam dimensions tcolors scolors nr-colors) obj
    (setf tcolors (make-texture nil :dimensions nr-colors :element-type :vec3)
          scolors (sample tcolors
                          ;;:minify-filter :nearest :magnify-filter :nearest
                          ))
    ;;(push-grays tcolors)
    (push-colors tcolors)
    (setf tex (make-texture nil :dimensions dimensions :element-type :rgba8)
          sam (sample tex :minify-filter :nearest :magnify-filter :nearest)
          zam (sample tex :minify-filter :nearest :magnify-filter :nearest))
    (setf (%cepl.types::%sampler-imagine zam) t)))

(defun make-mandelbrot (&key (nr-colors 32) (iterations 128))
  (declare (type unsigned-byte nr-colors iterations))
  (let ((obj (make-instance 'mandelbrot :nr-colors nr-colors
                                        :iterations iterations)))
    (push obj *actors*)
    obj))

(let ((stepper (make-stepper (seconds 1)
                             (seconds 1))))
  (defmethod update ((actor mandelbrot) dt)
    (with-slots (scale center tcolors) actor
      (let ((time (* .00001f0 (get-internal-real-time))))
        (when (funcall stepper)
          (push-colors tcolors)
          ;;(push-grays tcolors :color (v! .1 .3 .4) :inc (random .4))
          )
        (setf scale .000001)
        (setf center (v! .571 .5))))))

(let ((stepper (make-stepper (seconds .1)
                             (seconds .1))))
  (defmethod draw ((actor mandelbrot) camera time)
    ;; Draw to texture
    (with-slots (dimensions zam scolors center scale iterations) actor
      (when (funcall stepper)
        (map-g #'mandelbrot-pipe (make-compute-space (/ (first dimensions) 16)
                                                     (/ (second dimensions) 16))
               :dst zam
               :colors scolors
               :center center
               :scale scale
               :max-iterations iterations)))
    ;; Draw to screen
    (with-slots (sam) actor
      (map-g #'pass-pipe *bs*
             :sam sam))))

;;--------------------------------------------------

(defun push-grays (texture &key (color (v! .1 .1 .1))
                                (inc .01)
                                revert-p)
  (declare (type boolean revert-p)
           (type cepl:texture texture))
  (let* ((c color)
         (nr (first (texture-base-dimensions texture)))
         (colors (loop :repeat nr
                       :collect (progn (v3:incf c (v3! inc))
                                       (copy-seq c)))))
    (when revert-p
      (setf colors (reverse colors)))
    (push-g colors (texref texture))
    colors))

(defun push-colors (texture)
  (declare (type cepl:texture texture))
  (let* ((nr (first (texture-base-dimensions texture)))
         (colors (append (list (v! 1 1 1))
                         (loop :repeat (1- nr)
                               :collect (v! (random 1f0)
                                            (random 1f0)
                                            (random 1f0))))))
    (push-g colors (texref texture))
    colors))

;;--------------------------------------------------

(defun-g mandelbrot-compute (&uniform
                             (colors         :sampler-1d)
                             (dst            :image-2d)
                             ;;
                             (center         :vec2)
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
           (c (- (* scale (/ (v! (float (x thread-id))
                                 (float (y thread-id)))
                             (min (x dst-size)
                                  (y dst-size))))
                 center))
           (i (int 0)))
      (while (< (setf i (+ i 1)) max-iterations)
             (let ((x (+ (- (* (x z) (x z))
                            (* (y z) (y z)))
                         (x c)))
                   (y (+ (+ (* (y z) (x z))
                            (* (x z) (y z)))
                         (y c))))
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

(defpipeline-g mandelbrot-pipe ()
  :compute mandelbrot-compute)
