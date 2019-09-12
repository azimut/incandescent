(in-package #:incandescent)

;; NEEDS CEPL/VARJO hacks for currently unsupported image store

(defclass fractal (actor)
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
   :dimensions '(352 192)
   :nr-colors 32
   :center (v! 0 0)
   :iterations 128
   :scale .5))

(defmethod free ((actor fractal))
  (with-slots (tex tcolors) actor
    (free tex)
    (free tcolors)))

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
         (colors (append (list (v! 0 0 0))
                         (loop :repeat (1- nr)
                               :collect (v! (random 1f0)
                                            (random 1f0)
                                            (random 1f0))))))
    (push-g colors (texref texture))
    colors))

;;--------------------------------------------------

(defmethod initialize-instance :after ((obj fractal) &key)
  (with-slots (tex sam zam dimensions tcolors scolors nr-colors) obj
    (setf tcolors (make-texture nil :dimensions nr-colors :element-type :vec3)
          scolors (sample tcolors
                          ;;:minify-filter :nearest :magnify-filter :nearest
                          ))
    ;;(push-grays tcolors)
    (push-colors tcolors)
    (setf tex (make-texture nil :dimensions dimensions :element-type :rgba8)
          sam (sample tex ;;:minify-filter :nearest :magnify-filter :nearest
                      )
          zam (sample tex ;;:minify-filter :nearest :magnify-filter :nearest
                      ))
    (setf (%cepl.types::%sampler-imagine zam) t)))
