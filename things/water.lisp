(in-package #:incandescent)

;; FROM
;; https://catlikecoding.com/unity/tutorials/flow/texture-distortion/
;; https://catlikecoding.com/unity/tutorials/flow/looking-through-water/
;; TODO: my shader looks like ...mud

(setf *light-pos* (v! 50 150 0))

(defclass water (actor)
  ((albedo    :initarg :albedo)
   (water     :initarg :water)
   (flowmap   :initarg :flowmap)
   (normalmap :initarg :normalmap)))

(defvar *blend-water* (make-blending-params))
(defparameter *uv-jump* (v! 0 0))

#+nil
(progn (defun test ()
         (free-actors)
         (make-water :scale 3
                     :rot (q:from-axis-angle (v! 1 0 0) (radians -90)))
         (dotimes (i 5)
           (make-box :pos (v! (serapeum:random-in-range -2f0 2f0)
                              (serapeum:random-in-range -9f0 -1f0)
                              (serapeum:random-in-range -2f0 2f0))
                     :scale (random .5)
                     :rot (q:from-axis-angle
                           (v! (random 1f0) (random 1f0) (random 1f0))
                           (radians (random 360)))))
         ;; bottom
         (make-box :pos (v! 0 -10 0)
                   :x 10 :y 1 :z 10)
         ;;
         (make-box :pos (v! 0 -4.8 2.8)
                   :rot (q:from-axis-angle (v! 1 0 0) (radians 5))
                   :x 6 :y 10 :z .5)
         (make-box :pos (v! 0 -4.8 -2.8)
                   :rot (q:from-axis-angle (v! 1 0 0) (radians -5))
                   :x 6 :y 10 :z .5)
         ;;
         (make-box :pos (v! -3.2 -4.8 0)
                   :rot (q:from-axis-angle (v! 0 1 0) (radians -90))
                   :x 6 :y 10 :z .5)
         (make-box :pos (v! 3.2 -4.8 0)
                   :rot (q:from-axis-angle (v! 0 1 0) (radians 90))
                   :x 6 :y 10 :z .5)
         ;;(reset-camera)
         )
       (test)
       )

(defun make-water (&key (pos (v! 0 0 0))
                        (rot (q:identity))
                        (scale 1f0))
  (let ((obj (make-instance
              'water
              :pos pos :rot rot :scale (coerce scale 'single-float)
              :normalmap (get-tex "static/water-normal.png" nil t :rgb8)
              :water     (get-tex "static/water.png" nil t :r8)
              :albedo    (get-tex "static/uv.png" nil t :rgb8)
              :flowmap   (get-tex "static/flowmap.rga.png")
              :buf (get-quad-stream-v3))))
    (push obj *actors*)
    obj))

(defmethod update ((actor water) dt))

(defmethod draw ((actor water) camera (time single-float))
  (with-blending *blend-water*
    ;; depth-mask nil makes it invisible on the depth texture
    (with-setf (depth-mask) nil
      (with-slots (buf scale albedo flowmap water normalmap) actor
        (map-g #'water-pipe buf
               :scale scale
               :time  (* 1f0 (get-internal-real-time))
               :cam-pos (pos camera)
               :albedo albedo
               :water water
               :flowmap flowmap
               :normalmap normalmap
               :samd *samd*
               :model-world (model->world actor)
               :world-view  (world->view camera)
               :view-clip   (projection  camera)
               :light-color *light-color*
               :light-pos   *light-pos*)))))

;;--------------------------------------------------

(defun-g water-vert ((pos :vec3)
                     &uniform
                     (scale       :float)
                     (model-world :mat4)
                     (world-view  :mat4)
                     (view-clip   :mat4))
  (let* ((world-pos (* model-world (v! (* scale pos) 1)))
         (view-pos  (* world-view world-pos))
         (clip-pos  (* view-clip  view-pos)))
    (values clip-pos
            (+ .5 (* .5 (v! (x pos) (- (y pos)))))
            (s~ world-pos :xyz)
            (v! 0 1 0)
            (compute-screen-pos clip-pos))))

(defun-g flow-uv ((uv :vec2) (time :float))
  (- uv time))
(defun-g flow-uv ((uv :vec2) (flow-vector :vec2) (time :float))
  (- uv (* flow-vector time)))
(defun-g flow-uv ((uv :vec2) (flow-vector :vec2) (time :float))
  (let ((progress (fract time)))
    (- uv (* flow-vector progress))))
(defun-g flow-uvw ((uv :vec2) (flow-vector :vec2) (time :float))
  (let ((progress (fract time)))
    (v! (- uv (* flow-vector progress))
        (- 1 (abs (- 1 (* 2 progress)))))))

(defun-g flow-uvw ((uv :vec2) (flow-vector :vec2) (time :float) (flow-b :bool))
  (let* ((phase-offset (if flow-b .5f0 0f0))
         (progress     (fract (+ time phase-offset))))
    (v! (+ (- uv (* flow-vector progress)) phase-offset)
        (- 1 (abs (- 1 (* 2 progress)))))))

(defun-g flow-uvw ((uv :vec2) (flow-vector :vec2) (time :float) (flow-b :bool)
                   (jump :vec2))
  (let* ((phase-offset (if flow-b .5f0 0f0))
         (progress     (fract (+ time phase-offset)))
         ;;(flow-vector  (v! 0 0))
         (flow-vector  (* .1 flow-vector))
         )
    (v! (+ (- uv (* flow-vector progress))
           (* (- time progress) jump)
           phase-offset)
        (- 1 (abs (- 1 (* 2 progress)))))))

(defparameter *uv-jump* (v! 0 0))
(defparameter *water-fog-color* (v! .2 .2 .8))
(defparameter *water-fog-density* .15)
(defun-g color-below-water ((screen-pos :vec4) (samd :sampler-2d))
  (let* ((uv               (/ (s~ screen-pos :xy) (s~ screen-pos :w)))
         (background-depth (linearize-depth (x (texture samd uv))))
         (surface-depth    (z screen-pos))
         (depth-difference (- background-depth surface-depth))
         (fog-factor       (exp2 (* (- *water-fog-density*)
                                    depth-difference)))
         (background       (/ depth-difference 20)))
    (mix *water-fog-color* (v3! background) (v3! fog-factor))))

(defun-g water-frag ((uv :vec2) (frag-pos :vec3) (frag-normal :vec3) (screen-pos :vec4)
                     &uniform
                     (samd :sampler-2d)
                     (time        :float)
                     (albedo      :sampler-2d)
                     (water       :sampler-2d)
                     (flowmap     :sampler-2d)
                     (normalmap   :sampler-2d)
                     (cam-pos     :vec3)
                     (light-color :vec3)
                     (light-pos   :vec3))
  (let* ((time   (* .0002 time))
         ;;(uv (* 3 uv))
         (flow   (1- (* 2 (s~ (texture flowmap uv) :xy))))
         (noise  (w (texture flowmap uv)))
         (time   (+ noise time))
         (uvwa   (flow-uvw uv flow time nil *uv-jump*))
         (uvwb   (flow-uvw uv flow time t   *uv-jump*))
         ;;
         (colora  (* (z uvwa) (v3! (x (texture water (s~ uvwa :xy))))))
         (colorb  (* (z uvwb) (v3! (x (texture water (s~ uvwb :xy))))))
         (color3  (+ (pow (s~ colora :xyz) (v3! 2.2))
                     (pow (s~ colorb :xyz) (v3! 2.2))))
         (normala (* (z uvwa) (norm-from-map normalmap (s~ uvwa :xy))))
         (normalb (* (z uvwb) (norm-from-map normalmap (s~ uvwb :xy))))
         (normals (normalize (+ normala normalb)))
         ;;
         (background  (color-below-water screen-pos samd))
         (final-color (dir-light-apply-spec (* (v! .1 .2 .8) color3)
                                            light-color
                                            light-pos
                                            frag-pos
                                            normals
                                            cam-pos
                                            .9 3.8f0)))
    (v! color3 1)
    (v! (+ background final-color) 1)
    ;;(v4! background (y final-color) 0 1)
    ))

(defpipeline-g water-pipe ()
  :vertex   (water-vert :vec3)
  :fragment (water-frag :vec2 :vec3 :vec3 :vec4))


(let ((stepper (make-stepper (seconds 1) (seconds 1))))
  (defmethod update ((actor box) dt)
    #+nil
    (when (funcall stepper)
      (setf (rot actor) (q:from-axis-angle (v! (random .2) 1 (random .2)) (radians (random 350)))))))
