(in-package :incandescent)

(defclass sky (actor) ())
(defun make-sky ()
  (let ((obj (make-instance 'sky :buf (sphere))))
    (push obj *actors*)
    obj))

(defpipeline-g sky-pipe ()
  :vertex (cubemap-vert g-pnt)
  :fragment (sky-frag :vec3))

(defun-g sky-frag ((frag-pos :vec3)
                   &uniform
                   (time :float)
                   (light-pos :vec3)
                   (cam-pos :vec3))
  (+ (nineveh.noise:cellular-noise (+ (v! (* 5 (x frag-pos))
                                          (* 5 (y frag-pos))
                                          (z frag-pos))
                                      (* .005 time)))
     (v! 1 .5 .55 1)))

(defmethod draw ((actor sky) camera time)
  (with-slots (buf) actor
    (with-setf* ((cull-face) :front
                 (depth-test-function) #'<=
                 (depth-mask) nil)
      (map-g #'sky-pipe buf
             :time time
             ;; Rotation without translation
             :view (q:to-mat4
                    (q:inverse (rot camera)))
             :projection (projection camera)))))
