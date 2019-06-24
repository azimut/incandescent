(in-package :incandescent)

;; Basic nice thing to have - generic-pipe

(defclass box (actor) ())

(defun make-box (&key (pos (v! 0 0 0)) (rot (q:identity))
                      (x 2) (y 2) (z 2)
                      (scale 1f0)
                      (name (gensym)))
  (let ((obj (make-instance 'box :buf (box x y z)
                                 :name name
                                 :pos pos
                                 :rot rot
                                 :scale scale)))
    (push obj *actors*)
    obj))

(defun init-box (&optional (buf (box)) (range 8f0))
  "helper to create a bunch of boxes at random pos/rot/scale"
  (declare (type cepl:buffer-stream buf)
           (type single-float range))
  (let ((half (* .5 range)))
    (dotimes (i 20)
      (make-box :pos (v! (- (random range) half)
                         (- (random range) half)
                         (- (random range) half))
                :buf buf
                :scale (random 1f0)
                :rot (q:from-axis-angle (v! (random 1f0)
                                            (random 1f0)
                                            (random 1f0))
                                        (radians (random 360)))))))

(defmethod draw ((actor box) camera (time single-float))
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
           :light-pos   *light-pos*
           :brdf-luf *s-brdf*
           :irradiance-map *s-cubemap-live*
           :prefilter-map *s-cubemap-prefilter*
           )))

(defmethod update ((actor box) dt))