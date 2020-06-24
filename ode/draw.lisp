(in-package #:incandescent)

(defmethod update :around ((actor physic) dt)
  "updates visual representation from ODE value"
  (when *world*
    (with-slots (pos rot body orot geom immovablep) actor
      (unless immovablep
        (setf pos (ode-geom-get-position geom))
        (setf rot (ode-geom-get-quaternion2 orot geom)))))
  (call-next-method))

(defmethod draw ((actor physic) camera (time single-float))
  (with-slots (buf scale color draw-p) actor
    (when draw-p
      (map-g #'generic-pipe buf
             :scale scale
             :color color
             ;;:shadowmap *shadow-sam*
             ;; :light-world (world->view *shadow-camera*)
             ;; :light-clip  (projection *shadow-camera*)
             :cam-pos (pos camera)
             :model-world (model->world actor)
             :world-view  (world->view camera)
             :view-clip   (projection  camera)
             ;; Directional light (for the most part)
             :light-color *light-color*
             :light-pos   *light-pos*))))
