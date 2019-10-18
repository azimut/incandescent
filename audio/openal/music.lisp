(in-package #:incandescent)

;; TODO: fade-time, that is give a time in seconds to fade.
;; with current code, you need to try to ensure a fixed fps

(defclass music (event)
  ((fading-out-p :accessor music-fading-out-p :initarg :fading-out-p)
   (fading-in-p  :accessor music-fading-in-p  :initarg :fading-in-p)
   (fade-by      :accessor music-fade-by      :initarg :fade-by))
  (:default-initargs
   :fading-out-p nil
   :fading-in-p t
   :fade-by .01
   :loop-p t)
  (:documentation "special type of event for music"))

;; TODO: positional music with relative
(defun make-music (name paths &key (fade-by 0.01)
                                   (volume  0.5)
                                   (rate    1f0))
  "music layer, can have variations in different files..."
  (make-instance 'music :name name :paths paths
                        :rate rate :volume volume
                        :fade-by fade-by))

(defmethod play ((obj music))
  "plays cm:next buffer element in pattern"
  (with-accessors ((pattern  event-pattern)
                   (source   audio-source)
                   (fade-in  music-fading-in-p)
                   (fade-out music-fading-out-p))
      obj
    (let ((buffer (cm:next pattern)))
      (al:source source :buffer buffer)
      (al:source source :gain 0f0); let fade-in kick in on (update)
      (al:source-play source))
    (setf fade-in  t)
    (setf fade-out nil)))

(defmethod stop ((obj music))
  "do not stop the audio directly, delegate fade out to (update)"
  (with-slots (fading-out-p fading-in-p) obj
    (setf fading-out-p t
          fading-in-p  nil)))

(defmethod (setf state-gain) :around (value (obj music))
  (with-slots (volume fading-in-p fading-out-p source) obj
    (when (= 0 value)
      (setf fading-out-p nil)
      (al:source-stop source))
    (when (= value volume)
      (setf fading-in-p nil)))
  (call-next-method))

(defmethod update ((obj music) dt)
  "called by emitter, fade out/in when needed"
  (with-slots (fading-out-p fading-in-p fade-by source volume) obj
    (let ((current-gain (state-gain obj)))
      (when fading-out-p
        (let ((new-gain (max 0f0 (- current-gain (/ fade-by dt)))))
          (setf (state-gain obj) new-gain)))
      (when fading-in-p
        (let ((new-gain (min volume (+ current-gain (/ fade-by dt)))))
          (setf (state-gain obj) new-gain))))))
