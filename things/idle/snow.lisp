(in-package #:incandescent)

(defclass snow (particles) ())

(defun make-snow (&key (n-particles 100)
                       (source (v! 0 0 0)))
  (declare (type alexandria:positive-fixnum n-particles)
           (type rtg-math.types:vec3 source))
  (let ((obj (make-instance 'snow :n-particles n-particles
                                  :source source)))
    (push obj *actors*)
    obj))

(defun-g snow-vert ((pdata  pdata) &uniform
                    (source :vec3)
                    (time   :float))
  (with-slots (pos dir life) pdata
    (let* ((time (* time .2 gl-vertex-id))
           (life life)
           (dir  dir)
           (pos  pos)
           (r    (rand (vec2 time))))
      (if (or (>= life 1f0)
              (< (y pos) 0f0))
          (progn ;; Reset
            (setf dir  (v! (* 360 r) ;; rot
                           (+ 7 (* 5 r)) ;; scale
                           0))
            (setf life (* .4 r))
            (setf pos  (+ source
                          (v! (+ -50 (* 100 (rand (vec2 (* 3 time)))))
                              (+ -2.5 (* 5 r))
                              (+ -50 (* 100 r))))))
          (progn ;; Update
            (incf life .0005)
            (incf (z pos) -.005)
            (incf (x pos) -.005)
            (incf (y pos) -.05)))
      (values (v! 0 0 0 0)
              (:feedback pos)
              (:feedback dir)
              (:feedback life)))))

(defpipeline-g snow-pipe (:points)
  :vertex (snow-vert pdata))

(defmethod update ((actor snow) dt)
  (with-slots (tfs-dst str-src source) actor
    ;; try placing things in front of camera, hacky
    (let* ((dir  (q:to-direction (rot *camera*)))
           (zdir (v! (x dir) 0 (z dir)))
           (c (pos *camera*)))
      (setf source (v3:+ (v3:*s zdir 5f0)
                         (v! (x c) 2 (z c)))))
    (with-transform-feedback (tfs-dst)
      (map-g #'snow-pipe str-src
             :source (v! 0 10 0)
             :time dt))))

(defmethod draw ((actor snow) camera time)
  (with-slots (str-src) actor
    (map-g #'prender-points-pipe str-src
           :world-clip (world->clip camera)))
  (swap-particles actor))
