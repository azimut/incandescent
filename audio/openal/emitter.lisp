(in-package #:incandescent)

;; Container of events. Plays the events it holds providing positional data.
;; Inherit from this class to for your object to have sound.

(defclass dummy-actor ()
  ((pos :initform (v! 0 0 0) :initarg :pos :accessor pos)
   (rot :initform (q:identity) :initarg :rot :accessor rot)))

(defclass emitter (dummy-actor)
  ((events   :initarg :emitter  :reader   emitter-events))
  (:default-initargs
   :events   (list)))

(defun make-emitter (&rest events)
  (make-instance 'emitter :events events))

(defun play-emitter (emitter)
  (with-accessors ((velocity emitter-velocity)
                   (events   emitter-events)
                   (position pos))
      emitter
    (map nil (lambda (_) (play _ position velocity)) events)))

#+nil
(defmethod update :before ((obj emitter) dt)
  "set velocity"
  (setf (emitter-velocity obj)
        (v3:/s (v3:- (pos obj) (emitter-prev-pos obj)) dt))
  (setf (emitter-prev-pos obj)
        (copy-seq (pos obj))))

;; Ambient sfx (howl)
;;(make-emitter (make-event :odds .3 "howl.wav"))

#+nil
(list (make-sfx :foot (list "foot1.wav" "foot2.wav")
                :volume .2)
      (make-sfx :leaf (list "leaf2.wav" "leaf2.wav")
                :volume .9))

(defun update-and-play (obj pos rot)
  (setf (pos obj) pos)
  (setf (rot obj) rot)
  (play obj))
