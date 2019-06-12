(in-package :incandescent)

(defclass voice (actor)
  ((init-time :initform (incudine:now) :initarg :init-time)
   (chan      :initform 0              :initarg :chan
              :reader chan)))
(defun make-voice (&key (pos (v! 0 0 0))
                        (init-time (incudine:now))
                        (chan 0))
  (let ((obj (make-instance 'voice :pos pos
                                   :init-time init-time
                                   :chan chan)))
    (push obj *actors*)
    obj))

;; TODO - (bps *tempo*)
(defmethod update ((actor voice) dt)
  (with-slots (rot scale init-time) actor
    (setf scale (max (- scale .04) 0f0))
    (setf rot (q:from-axis-angle (v! 0 1 0)
                                 (radians
                                  (lerp
                                   0f0
                                   360f0
                                   (* .25
                                      (coerce
                                       (mod (/ (- (incudine:now) init-time)
                                               .8d0
                                               (* incudine::*sample-rate*
                                                  (incudine:spb incudine:*tempo*)))
                                            1d0)
                                       'single-float))))))))


(defun-g sheet-frag ((uv :vec2)
                     (frag-norm :vec3)
                     (frag-pos :vec3)
                     &uniform
                     (time :float)
                     (color :vec3)
                     (cam-pos :vec3)
                     ;; Directional light (for the most part)
                     (light-color    :vec3)
                     (light-pos      :vec3))
  (let* ((roughness .7)
         (metallic .01)
         (ambient (v! .03 .03 .03))
         (f0 (v3! .04))
         (f0 (mix f0 color metallic))
         (final-color color)
         (final-color (dir-light-apply final-color
                                       light-color
                                       light-pos
                                       frag-pos
                                       frag-norm
                                       cam-pos 32 1
                                       )))
    (values (v! (+ ambient final-color) 1)
            ;; (if (> (dot final-color (v! .2126 .7152 .0722)) 1f0)
            ;;     (v! final-color 1)
            ;;     (v! 0 0 0 1))
            )))

(defpipeline-g sheet-pipe ()
  :vertex   (vert g-pnt)
  :fragment (sheet-frag :vec2 :vec3 :vec3))

(defmethod draw ((actor voice) camera (time single-float))
  (with-slots (buf scale color) actor
    (map-g #'sheet-pipe buf
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

(defmethod chan (object))
(defun find-chan (chan)
  (declare (type fixnum chan))
  (find chan *actors* :key #'chan))

(defun add-chan (time chan pos)
  (declare (type fixnum chan)
           (type double-float time)
           (type rtg-math.types:vec3 pos))
  (alexandria:if-let ((voice (find-chan chan)))
    (setf (slot-value voice 'index) (random 3))
    (make-voice :init-time time :pos pos :chan chan)))

(defmethod shiny::p :before (time pitch velocity duration channel &key pan)
  (case channel
    (1 (add-chan time 0 (v! 0 0 0)))))

(defun test-chan ()
  (free-actors)
  (let ((time (incudine:now)))
    (make-voice :pos (v! -2 0 0) :init-time time :chan 0)
    (make-voice :pos (v! 0 0 0)  :init-time time :chan 1)
    (make-voice :pos (v! 2 0 0)  :init-time time :chan 2))
  (reset-camera *camera* (v! 0 0 3)))

