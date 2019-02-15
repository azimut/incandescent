(in-package #:incandescent)

(defclass celestial-sphere (actor)
  ((buf :initform (sphere))))

(defun make-celestial-sphere ()
  (let ((obj (make-instance 'celestial-sphere
                            :name :celestial-sphere)))
    (push obj *actors*)
    obj))

(defmethod draw ((actor celestial-sphere) camera time)
  (with-slots (buf color scale) actor
    (with-setf* ((cull-face) :front
                 (depth-test-function) #'<=
                 (depth-mask) nil)
      (map-g #'celestial-pipe buf
             :color color
             :cam-pos (pos camera)
             :mod-clip
             (m4:* (projection  camera)
                   (world->view camera)
                   (model->world actor))))))

(defmethod update ((actor celestial-sphere))
  (setf (pos actor) (pos *currentcamera*))
  ;;(setf (rot actor) (q:from-axis-angle (v! 0 0 1) (radians 0)))
  ;;(setf (rot actor) (q:identity))
  )

(defun-g celestial-frag ((frag-pos :vec3)
                         &uniform
                         (cam-pos :vec3)
                         (light-pos :vec2)
                         (color :vec3))
  (* (atmosphere (normalize frag-pos)
                 (v! 0 372000 0)
                 (v! 0 39 -100)
                 20f0
                 373000f0
                 471000f0
                 (v! .0000055 .000013 .0000224)
                 .000021
                 900f0 ;; rayleigh scale height
                 1100    ;; mie scale height
                 .758
                 3 ;; 16 AND 8
                 2)
     (v! .1 .1 .9)))

(defpipeline-g celestial-pipe ()
  :vertex   (cubemap-vert g-pnt)
  :fragment (celestial-frag :vec3))
