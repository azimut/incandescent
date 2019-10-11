(in-package #:incandescent)

(defclass drifter (actor) ())

(defun make-drifter (&key (pos (v! 0 0 0)) (rot (q:identity))
                          (x 2) (y 2) (z 2)
                          (scale 1f0))
  (let ((obj (make-instance 'drifter :buf (box x y z)
                                     :pos pos
                                     :rot rot
                                     :scale scale)))
    (push obj *actors*)
    obj))

(defmethod draw ((actor drifter) camera time)
  (with-slots (buf scale color) actor
    (map-g #'generic-pipe buf
           :scale scale
           :color color
           :time  time
           :cam-pos (pos camera)
           :model-world (model->world actor)
           :world-view  (world->view camera)
           :view-clip   (projection  camera)
           ;; Directional light (for the most part)
           :light-color *light-color*
           :light-pos   *light-pos*)))

(defmethod update ((actor drifter) dt))
