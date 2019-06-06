(in-package :incandescent)

(progn (free-actors)
       ;;(make-rope)
       (make-npc (get-tex "/home/sendai/Downloads/PC Computer - Lisa The Painful RPG - Yado.png")
                 :pos (v! -2 2.3 1)
                 :chan 2
                 :rows 2 :cols 6 :index 4)
       (make-npc (get-tex "/home/sendai/Downloads/PC Computer - Lisa The Painful RPG - Rage Ironhead.png")
                 :pos (v! 2 -3 6)
                 :scale 1.5f0
                 :chan 55
                 :rows 4 :cols 6 :index 5)
       (make-npc (get-tex "/home/sendai/Downloads/PC Computer - Lisa The Painful RPG - Rage Ironhead.png")
                 :pos (v! 0 -3 4)
                 :scale 1.5f0
                 :chan 5
                 :flip-x -1
                 :rows 4 :cols 6 :index 5)
       ;;
       (make-npc (get-tex "/home/sendai/Downloads/PC Computer - Lisa The Painful RPG - Dismal Island NPCs.png")
                 :pos (v! 7 -3 4)
                 :scale 2.5f0
                 :chan 6
                 :flip-x -1
                 :rows 1.1 :cols 6 :index 4.7)
       ;;
       (make-rope :pos (v! 10 17 8)
                  :rot (q:from-axis-angle (v! 0 1 0) (radians -59)))
       (make-chair :pos (v! 10 -2 8)
                   :rot (q:* (q:from-axis-angle (v! 0 0 1) (radians 92))
                             (q:from-axis-angle (v! 1 0 0) (radians -40))))
       (make-pinkf :uv-repeat 15)
       (make-desk)
       (make-sky)
       ;;(reset-camera *camera* (v! 0 0 10))
       )
(defmethod update ((actor npc) dt)
  (with-slots (pos chan) actor
    (when (= chan 6)
      (setf (y pos) (max -5f0 (- (y pos) .1))))))
;; (make-npc (get-tex "/home/sendai/Downloads/PC Computer - Lisa The Painful RPG - Buzzos Gang.png")
;;           :pos (v! 6 -2.5 4)
;;           :scale 2f0
;;           :chan 6
;;           :rows 5 :cols 9 :index 28)
;; (make-npc (get-tex "/home/sendai/Downloads/PC Computer - Lisa The Painful RPG - Buzzos Gang.png")
;;           :pos (v! 6 -2.9 3)
;;           :scale 2f0
;;           :chan 5
;;           :rows 5 :cols 9 :index 25)

(defun find-chan (chan)
  (declare (type fixnum chan))
  (find chan *actors* :key #'chan))

(defun mute-chan (chan)
  (case chan
    (2 (when-let ((voice (find-chan 2)))
         (setf (slot-value voice 'index) 3)))
    (5 (when-let ((voice (find-chan 5)))
         (setf (slot-value voice 'index) 0)))))

(defmethod shiny::p :after (time pitch velocity duration channel &key pan)
  (case channel
    (2 (when-let ((voice (find-chan channel)))
         (setf (slot-value voice 'index) (random 3))))
    (5 (when-let ((voice (find-chan channel)))
         (setf (slot-value voice 'index) 4)
         (incudine:aat (+ time #[.25 b]) (lambda () (setf (slot-value voice 'index) 2))))
     (when-let ((voice (find-chan 55)))
       (setf (slot-value voice 'index) 4)
       (incudine:aat (+ time #[.25 b]) (lambda () (setf (slot-value voice 'index) 2)))))
    (6 (when-let ((voice (find-chan channel)))
         (incf (y (slot-value voice 'pos)) 2f0)
         (setf (slot-value voice 'index) 4.7)))))


;;--------------------------------------------------


(defclass npc (sprite actor)
  ((chan :initform 0 :initarg :chan :reader chan)))
(defmethod chan (object))
(defvar *npc-blend* (make-blending-params))
(defmethod free ((obj npc)))

;; it aint over till the fat lady sings!
(defun make-npc (sam &key (pos (v! 0 0 0))
                          (scale 1f0)
                          (chan 0)
                          (flip-x 1) (flip-y -1)
                          (rows 1) (cols 1) (index 0))
  (declare (type cepl:sampler sam))
  (let ((obj (make-instance
              'npc
              :pos pos
              :chan chan
              :scale scale
              :flip-x flip-x :flip-y flip-y
              :rows rows :cols cols :index index
              :sam sam)))
    (push obj *actors*)
    obj))

(defmethod draw ((actor npc) camera time)
  (with-blending *npc-blend*
    (with-slots (sam scale rows cols index flip-x flip-y) actor
      (with-setf (depth-mask) nil
        (map-g #'npc-pipe (get-quad-stream-v3)
               :sam sam
               :scale scale
               :flip-x flip-x :flip-y flip-y
               :rows (* 1f0 rows) :cols (* 1f0 cols) :index (* 1f0 index)
               :model-world (model->world actor)
               :world-view  (world->view  camera)
               :view-clip   (projection   camera))))))

(defpipeline-g npc-pipe ()
  :vertex   (npc-vert :vec3)
  :fragment (npc-frag :vec2 :vec2 :vec2))

;; http://www.geeks3d.com/20140807/billboarding-vertex-shader-glsl/
(defun-g npc-vert ((pos   :vec3) &uniform
                   (flip-x :int) (flip-y :int)
                   (cols  :float) (rows  :float) (index :float)
                   (scale :float)
                   ;;
                   (model-world :mat4)
                   (world-view  :mat4)
                   (view-clip   :mat4))
  (let* ((mv (* world-view model-world)))
    (setf (aref mv 0 0) 1f0
          (aref mv 0 1) 0f0
          (aref mv 0 2) 0f0
          ;;
          ;; (aref mv 1 0) 0f0
          ;; (aref mv 1 1) 1f0
          ;; (aref mv 1 2) 0f0
          ;;
          (aref mv 2 0) 0f0
          (aref mv 2 1) 0f0
          (aref mv 2 2) 1f0)
    (multiple-value-bind (uv-scale uv-offset)
        (calc-uv-mod cols rows index)
      (values (* view-clip (* mv (v! (* scale pos) 1)))
              (+ .5 (* .5 (v! (* flip-x (x pos))
                              (* flip-y (y pos)))))
              uv-scale
              uv-offset))))

(defun-g npc-frag ((uv :vec2) (uv-scale :vec2) (uv-offset :vec2)
                   &uniform
                   (sam :sampler-2d) (samd :sampler-2d))
  (let* (;;(color (v! 0 1 0))
         (color  (texture sam (+ uv-offset (* uv-scale uv))))
         (color3 (s~ color :xyz))
         )
    (v! (pow color3 (v3! 2.2))
        (step .9 (w color)))
    ;;(v! (y uv) 0 0 1)
    ))
