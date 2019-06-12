(in-package :incandescent)

(defclass land (actor) ())
(defun make-land ()
  (let ((obj (make-instance 'land :buf (lattice 200 200))))
    (push obj *actors*)
    obj))
(defmethod draw ((actor land) camera (time single-float))
  (with-slots (buf scale color) actor
    (map-g #'generic-shadow-pipe buf
           :scale scale
           :color (v! .3 .5 .9)
           :shadowmap *shadow-sam*
           :light-world (world->view *shadow-camera*)
           :light-clip  (projection *shadow-camera*)
           :cam-pos (pos camera)
           :model-world (model->world actor)
           :world-view  (world->view camera)
           :view-clip   (projection  camera)
           ;; Directional light (for the most part)
           :light-color *light-color*
           :light-pos   *light-pos*)))

