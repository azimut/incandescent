(in-package #:incandescent)

;; mmm...I am not so sure about this...
(defclass positional (event)
  (;; Derived values
   (prev-pos     :accessor state-prev-pos     :initarg :prev-pos) ; ?
   (prev-ts      :accessor state-prev-ts      :initarg :prev-ts)
   (velocity     :accessor state-velocity     :initarg :velocity) ; update
   ;; Directional cone?
   (direction    :accessor state-direction    :initarg :direction)
   (cone-inner   :accessor state-cone-inner   :initarg :cone-inner) ; init
   (cone-outer   :accessor state-cone-outer   :initarg :cone-outer) ; init
   (outer-gain   :accessor state-outer-gain   :initarg :outer-gain) ; init
   ;; Distance model modifiers
   (rolloff      :accessor state-rolloff      :initarg :rolloff
                 :documentation "determines how quickly the sound decreases
   higher rolloff quickly decreases.")
   (max-distance :accessor state-max-distance :initarg :max-distance
                 :documentation "used on clamped distance models, at which
   distance the sound will stop attenuating.")
   (ref-distance :accessor state-ref-distance :initarg :ref-distance
                 :documentation "distance where gain is 1, set it to 0 to emit
   from a single point or set it to a value bigger to mark a radius where you
   will always here it at gain 1, for example music playing in a town.")
   ;;min-gain
   ;;max-gain
   )
  (:default-initargs
   :relative nil                        ; make sound positional
   :prev-pos (v! 0 0 0)
   :prev-ts (* .1f0 (get-internal-real-time))
   :velocity (v! 0 0 0)
   :direction (v! 0 0 0)
   :cone-inner 360f0
   :cone-outer 360f0
   :rolloff nil
   :ref-distance nil
   :max-distance nil
   :outer-gain 0f0)
  (:documentation "second layer of metadata to control where to play the audio"))

(defmethod initialize-instance :after ((obj positional) &key pos
                                                             ref-distance
                                                             max-distance
                                                             rolloff)
  (al:source (audio-source obj) :position pos)
  (when rolloff
    (al:source (audio-source obj) :rolloff-factor rolloff))
  (when ref-distance
    (al:source (audio-source obj) :reference-distance ref-distance))
  (when max-distance
    (al:source (audio-source obj) :max-distance max-distance)))

(defun make-positional (name paths &key (volume .5)
                                        (pos (v! 0 0 0))
                                        ref-distance
                                        max-distance
                                        rolloff)
  (make-instance 'positional :name name :paths paths
                             :max-distance max-distance
                             :ref-distance ref-distance
                             :rolloff rolloff
                             :volume volume :pos pos))

(defmethod state-direction ((obj positional))
  (setf (slot-value obj 'direction)
        (al:get-source (audio-source obj) :direction)))
(defmethod state-velocity ((obj positional))
  (setf (slot-value obj 'velocity)
        (al:get-source (audio-source obj) :velocity)))
(defmethod state-cone-inner ((obj positional))
  (setf (slot-value obj 'cone-inner)
        (al:get-source (audio-source obj) :cone-inner-angle)))
(defmethod state-cone-outer ((obj positional))
  (setf (slot-value obj 'cone-outer)
        (al:get-source (audio-source obj) :cone-outer-angle)))
(defmethod state-rolloff ((obj positional))
  (setf (slot-value obj 'rolloff)
        (al:get-source (audio-source obj) :rolloff-angle)))
(defmethod state-outer-gain ((obj positional))
  (setf (slot-value obj 'outer-gain)
        (al:get-source (audio-source obj) :cone-outer-gain)))

(defmethod (setf pos) :before (val (obj positional))
  "when position is updated keep track of the old one and calculate the velocity"
  (check-type val rtg-math.types:vec3)
  (let* ((direction (v3:- (state-prev-pos obj) val))
         (ts (* .1f0 (get-internal-real-time)))
         (dt (- ts (state-prev-ts obj))))
    (setf (state-prev-pos obj) (copy-seq (slot-value obj 'pos)))
    (setf (state-velocity obj) (v3:/s direction dt))
    (setf (state-prev-ts  obj) ts)))

(defmethod (setf state-direction) :before (val (obj positional))
  (check-type val rtg-math.types:vec3))
(defmethod (setf state-direction) :after (val (obj positional))
  (al:source (audio-source obj) :direction val))
(defmethod (setf state-velocity) :before (val (obj positional))
  (check-type val rtg-math.types:vec3))
(defmethod (setf state-velocity) :after (val (obj positional))
  (al:source (audio-source obj) :velocity val))

(defmethod (setf state-cone-inner) :before (val (obj positional))
  (check-type val (single-float 0f0 360f0)))
(defmethod (setf state-cone-inner) :after (val (obj positional))
  (al:source (audio-source obj) :cone-inner-angle val))
(defmethod (setf state-cone-outer) :before (val (obj positional))
  (check-type val (single-float 0f0 360f0)))
(defmethod (setf state-cone-outer) :after (val (obj positional))
  (al:source (audio-source obj) :cone-outer-angle val))

(defmethod (setf state-rolloff) :before (val (obj positional))
  (check-type val single-float))
(defmethod (setf state-rolloff) :after (val (obj positional))
  (al:source (audio-source obj) :rolloff-factor val))

(defmethod (setf state-outer-gain) :before (val (obj positional))
  (check-type val (single-float 0f0 1f0)))
(defmethod (setf state-outer-gain) :after (val (obj positional))
  (al:source (audio-source obj) :cone-outer-gain val))

(defmethod (setf state-max-distance) :after (value (obj positional))
  (al:source (audio-source obj) :max-distance value))
(defmethod (setf state-ref-distance) :after (value (obj positional))
  (al:source (audio-source obj) :reference-distance value))
