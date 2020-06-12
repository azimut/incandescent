(in-package #:incandescent)


;; https://freesound.org/people/florianreichelt/sounds/459964/
;; (setf cloud::*server* (make-instance 'cloud::udp))
;; (cloud::connect cloud::*server*)

;; About the "kheadrot" param of hrtfearly. I found that in the code for http://csoundjournal.com/issue19/InterfacingCsoundUnity.html is the Yaw/Y-rotation in euler angles but I don't see it mentioned on the documentation.

;; Also I am not sure about the roomsize. The default rooms (small/medium/large) are suppose to have the ceiing as the last paramater? Like, when it says: "medium (1: 10*10*3)" it means a squared room with a 3 of roof? If so, is iroomZ the height? I think usually Y is height on games.

(defvar *sound* nil)
(defparameter *stepper* (make-stepper (seconds .1) (seconds .1)))
(defparameter *stepper2* (make-stepper (seconds 2) (seconds 2)))
(defparameter *stepper3* (make-stepper (seconds .01) (seconds .01)))

(defun cloud:get-listener-pos ()
  (pos *currentcamera*))
(defun cloud:get-listener-rot ()
  (rot *currentcamera*))

(defclass sound-source (room-pared cloud::roomie)
  ()
  (:default-initargs
   :buf  (box 1 2.1 1 t)
   :color (v! 0 0 0)))

(defun make-sound-source ()
  (let ((obj (make-instance 'sound-source
                            :pos (v! 0 2 20)
                            :filename "/home/sendai/whatis.wav"
                            :reverb .08
                            :order 1
                            :room-type :custom
                            :room-size (v! 2 3 50))))
    (setf *sound* obj)
    (push obj *actors*)))


(defmethod update :after ((actor sound-source) dt)
  (setf (pos actor) (v! 0 1 20))
  (cloud::upload-source actor)
  (setf (cloud::pos actor) (copy-seq (pos actor))))

(make-sound-source)
(reset-camera)

(cloud::schedule cloud::*server* (cloud::ninstr *sound*) 0 20
                 1 0 0 .5)
