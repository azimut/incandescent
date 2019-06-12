(in-package :incandescent)

(defvar *leafs* nil)

(defclass leafs (billboards) ())

(defun make-leafs (n-particles)
  (declare (type alexandria:positive-fixnum n-particles))
  (let* ((gar-src (make-gpu-array nil :dimensions n-particles :element-type 'pdata))
         (gar-dst (make-gpu-array nil :dimensions n-particles :element-type 'pdata))
         (str-src (make-buffer-stream gar-src :primitive :points))
         (str-dst (make-buffer-stream gar-dst :primitive :points))
         (tfs-src (make-transform-feedback-stream gar-src))
         (tfs-dst (make-transform-feedback-stream gar-dst))
         (obj (make-instance 'leafs
                             :gar-src gar-src :gar-dst gar-dst
                             :str-src str-src :str-dst str-dst
                             :tfs-src tfs-src :tfs-dst tfs-dst
                             :sam (get-tex "static/LEAF.png"))))
    (reset-particles obj)
    (push obj *particle-systems*)
    obj))


;;--------------------------------------------------
(defun-g leafs-vert ((pdata pdata)
                     &uniform
                     (source :vec3)
                     (time :float))
  (with-slots (pos dir life) pdata
    (let* ((time (* time .2 (* 2f0 gl-vertex-id)))
           (life life)
           (new-life (+ life .01))
           (dir dir)
           (pos pos)
           (r (rand (vec2 time))))
      (if (and (>= new-life 1f0) (< (y pos) 0f0))
          (progn ;; Reset
            (setf dir  (v! (* 360 (sin r)) ;; rot
                           (+ 1f0 r) ;; scale
                           0))
            (setf life (* .4 r))
            (setf pos  (+ source
                          (v! (+ -8 (* 16 (rand (vec2 (* 3 time)))))
                              (* 2 r)
                              (+ -8 (* 16 r))))))
          (progn ;; Update
            (setf life new-life)
            (decf (y pos) (+ .05 (* .01 r)))))
      (values (v! 0 0 0 0)
              (:feedback pos)
              (:feedback dir)
              (:feedback life)))))

(defpipeline-g leafs-pipe (:points)
  :vertex (leafs-vert pdata))

(defmethod update ((actor leafs) dt)
  (with-slots (tfs-dst str-src source) actor
    (let ((c (pos *camera*)))
      (setf source (v! (x c) 4 (z c))))
    (with-transform-feedback (tfs-dst)
      (map-g #'leafs-pipe str-src
             :source source
             :time dt))))

(defun-g dleafs-frag ((uv :vec2)
                      (life :float)
                      &uniform
                      (time :float)
                      (res :vec2)
                      (scene :sampler-2d)
                      (sam :sampler-2d)
                      (samd :sampler-2d))
  (let* ((sprites 8)
         ;;(uv (/ uv sprites))
         (color (texture sam uv)))
    (v! (*  (pow (s~ color :xyz) (v3! 2.2))
            ;;(v! .18 .17843138 .1552941)
            ;;(v! 0.6392157 0.54901963 0.34509805)
            ;;(* 2 (v! .5 .6 .7))
            ;;(v! 0 -1 0)
            )
	(* ;;(- 1 life)
         (w color)
         ;; (calculate-fade
         ;;  (z gl-frag-coord)
         ;;  (x (texel-fetch samd
         ;;                  (ivec2 (int (round (x (s~ gl-frag-coord :xy))))
         ;;                         (int (round (y (s~ gl-frag-coord :xy)))))
         ;;                  0)))
         ))))

(defpipeline-g dleafs-pipe (:points)
  :vertex   (billboard-vert pdata)
  :geometry (billboard-geom (:float 1) (:vec3 1))
  :fragment (dleafs-frag :vec2 :float))

(defmethod draw ((actor leafs) camera time)
  "textured particles blended into the scene"
  ;; Use a simple mask when not using soft-particles
  ;; Otherwise, setf (depth-mask) to nil AND make sure is the first element
  (with-setf (depth-mask) nil
    (with-slots (sam str-src) actor
      (with-blending *blend*
        (map-g #'dleafs-pipe str-src
               :sam sam
               :samd *samd*
               :res (resolution (current-viewport))
               :world-view (world->view camera)
               :view-clip  (projection camera)))))
  (swap-particles actor))
