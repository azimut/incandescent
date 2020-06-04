(in-package #:incandescent)

;; B) POINTS -> BILLBOARDS

(defvar *blend-billboards* (make-blending-params))

(defclass billboards (particles)
  ((sam :initarg :sam :documentation "texture displayed in billboard"))
  (:documentation "class for point based billboards"))

(defun make-billboards (sam &key (n-particles 100)
                                 (source (v! 0 0 0)))
  (declare (type alexandria:positive-fixnum n-particles)
           (type rtg-math.types:vec3        source)
           (type cepl:sampler               sam))
  (let ((obj (make-instance 'billboards :n-particles n-particles
                                        :source source
                                        :sam sam)))
    (push obj *actors*)
    obj))

;;--------------------------------------------------

;; https://developer.download.nvidia.com/whitepapers/2007/SDK10/SoftParticles_hi.pdf
;; https://discourse.threejs.org/t/soft-particles-render/504/3
(defun-g calculate-fade ((particle-depth :float)
                         (scene-depth    :float))
  (let* ((z-fade      1f0)
         (f-distance 10f0)
         (f-contrast  1f0)
         (input-depth (* (- scene-depth particle-depth) f-distance)))
    (if (and (> input-depth 0) (< input-depth 1))
        (setf z-fade (* .5 (pow (saturate (* 2f0 (if (> input-depth .5)
                                                     (- 1 input-depth)
                                                     input-depth)))
                                f-contrast))
              z-fade (if (> input-depth .5) (- 1 z-fade) z-fade))
        (setf z-fade (saturate input-depth)))
    z-fade))

(defun-g billboard-vert ((pdata pdata)
                         &uniform
                         (world-view :mat4))
  (with-slots (pos life dir) pdata
    (values (* world-view (v! pos 1))
            life
            dir)))

;; TODO: is kind of broken in the sense that changes of scale trow a monkey
;; wrench to the position which makes it a unreliable resource for a scene
(defun-g billboard-geom ((life (:float 1))
                         (rot  (:vec3  1))
                         &uniform
                         (camera-pos :vec3)
                         (view-clip  :mat4))
  (declare (output-primitive :kind :triangle-strip
                             :max-vertices 4))
  (let ((life (aref life 0))
        (p    (s~ (gl-position (aref gl-in 0)) :xyz)))
    (when (< life 1f0)
      (let* ((to-camera (normalize (- camera-pos p)))
             (up    (v! 0 1 0))
             (right (cross to-camera up))
             (scale (y (aref rot 0))))
        ;; try discard closer to the camera billboads
        (let ((zp (v! (x p) 0 (z p)))
              (zc (v! (x camera-pos) 0 (z camera-pos))))
          (when (< (length (- zc zp)) 3)
            (setf life .999f0)))
        (decf p (/ right (* scale 2)))
        (emit ()
              (* view-clip (v! p 1))
              (v! 0 0)
              life)
        ;;
        (incf (y p) scale)
        (emit ()
              (* view-clip (v! p 1))
              (v! 0 1)
              life)
        ;;
        (decf (y p) scale)
        (incf p (* scale right))
        (emit ()
              (* view-clip (v! p 1))
              (v! 1 0)
              life)
        ;;
        (incf (y p) scale)
        (emit ()
              (* view-clip (v! p 1))
              (v! 1 1)
              life)
        (end-primitive)
        (values)))))

(defun-g billboard-frag ((uv   :vec2)
                         (life :float)
                         &uniform
                         (time :float)
                         (res  :vec2)
                         (sam  :sampler-2d)
                         (samd :sampler-2d))
  (let* ((sprites 8)
         ;;(uv (/ uv sprites))
         (color (texture sam uv)))
    (v! (* (s~ color :xyz)
           ;;(v! .18 .17843138 .1552941)
           ;;(v! 0.6392157 0.54901963 0.34509805)
           ;;(* 2 (v! .5 .6 .7))
           ;;(v! 0 -1 0)
           )
	(* (- 1 life)
           (w color)
           (calculate-fade
            (z gl-frag-coord)
            (x (texel-fetch samd
                            (ivec2 (int (round (x (s~ gl-frag-coord :xy))))
                                   (int (round (y (s~ gl-frag-coord :xy)))))
                            0)))))
    ;;(v!  1 0 0 1)
    ))

(defpipeline-g billboard-pipe (:points)
  :vertex   (billboard-vert pdata)
  :geometry (billboard-geom (:float 1) (:vec3 1))
  :fragment (billboard-frag :vec2 :float))

;;--------------------------------------------------
;; https://forum.processing.org/two/discussion/3955/textured-billboard-and-transparency-problem
(defmethod draw-variance-actor ((actor billboards)))
(defmethod draw ((actor billboards) camera time)
  "textured particles blended into the scene"
  ;; Use a simple mask when not using soft-particles
  ;; Otherwise, setf (depth-mask) to nil AND make sure is the first element
  (with-setf (depth-mask) nil
    (with-slots (sam str-src) actor
      (with-blending *blend-billboards*
        (map-g #'billboard-pipe str-src
               :sam sam
               :samd *samd*
               :time time
               :res (resolution (current-viewport))
               :camera-pos (pos camera)
               :world-view (world->view camera)
               :view-clip  (projection camera)))))
  (swap-particles actor))
