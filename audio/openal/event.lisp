(in-package #:incandescent)

;; TODO: offsets: sec sample byte
(defclass event (audio)
  ((odds          :accessor event-odds          :initarg :odds)
   (pattern       :accessor event-pattern       :initarg :pattern)
   (volume        :accessor event-volume        :initarg :volume)
   (rate          :accessor event-rate          :initarg :rate)
   (volume-offset :accessor event-volume-offset :initarg :volume-offset)
   (rate-offset   :accessor event-rate-offset   :initarg :rate-offset)
   (loop-p        :accessor event-loop-p        :initarg :loop-p)
   (gain          :accessor state-gain          :initarg :gain))
  (:default-initargs
   :gain 1f0
   :odds 1f0
   :pattern nil
   :volume .5
   :rate 1f0
   :volume-offset 0f0
   :rate-offset 0f0
   :loop-p nil)
  (:documentation "first layer of metadata to control how to play an audio"))

(defmethod state-gain ((obj event))
  "query of the field is really a query on remote"
  (setf (slot-value obj 'gain)
        (al:get-source (audio-source obj) :gain)))
(defmethod (setf state-gain) :around (val (obj event))
  "update remote gain when updating slot"
  (al:source (audio-source obj) :gain val)
  (call-next-method))

;; TODO: either pattern or buffer set, ENSURE
(defmethod initialize-instance :after ((obj event) &key pattern loop-p)
  "initialize pattern if not provided, and loop status"
  (check-type loop-p boolean)
  (al:source (audio-source obj) :looping loop-p)
  (state-gain obj)
  (unless pattern
    (setf (event-pattern obj) (cm:new cm:heap :of (audio-buffers obj)))))

(defun make-event (name paths &key (volume        0.1)
                                   (odds          1f0)
                                   (pos           (v! 0 0 0))
                                   (volume-offset 0f0)
                                   (rate-offset   0f0)
                                   (rate          1f0))
  (make-instance 'event :name name
                        :paths paths :rate rate :volume volume :odds odds :pos pos
                        :rate-offset rate-offset :volume-offset volume-offset))


(declaim (inline random-offset))
(defun random-offset (value offset)
  (if (zerop offset)
      value
      (+ value (- (random offset) (/ offset 2f0)))))

(defmethod play ((obj event))
  "plays cm:next buffer element in pattern"
  (with-accessors ((pattern       event-pattern)
                   (odds          event-odds)
                   (rate          event-rate)
                   (volume        event-volume)
                   (source        audio-source)
                   (rate-offset   event-rate-offset)
                   (volume-offset event-volume-offset))
      obj
    (when (cm:odds odds)
      (let ((buffer (cm:next pattern))
            (new-volume (random-offset volume volume-offset))
            (new-rate   (random-offset rate rate-offset)))
        (al:source source :buffer buffer)
        (al:source source :gain   new-volume)
        (al:source source :pitch  new-rate)
        (al:source-play source)))))
