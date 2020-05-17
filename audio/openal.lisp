(in-package :incandescent)

;; TODO: API LEAKS sources?

;; OpenAL seems the most api complete option. But sbcl crashes with the following if alut:init or alc:open-device was run and an exception occurrs...so...I give up
;;
;; fatal error encountered in SBCL pid 5623(tid 0x7f4c2ae14040):
;; deferrable signals partially blocked: {13}
;;
;; TODO: velocity - aka vec3 direction and speed
;; TODO: al:distance-model + distance model variables (rollof,ref dist, max dist)
;; https://www.youtube.com/watch?v=AwcYQd4cH_8
;; https://github.com/kcat/openal-soft/wiki/Guide:-Introduction
;; http://wiki.lwjgl.org/wiki/OpenAL_Tutorial_7_-_The_Doppler_Effect.html

(defvar *dev* nil)
(defvar *audio-sources* nil)
(defvar *audio-buffers* (make-hash-table :test #'equal))

(defclass audio-sound ()
  ((immovablep :initarg :immovablep)
   (abuffer    :initarg :abuffer :documentation "OPENAL audio buffer")
   (asource    :initarg :asource :documentation "OPENAL player")
   (await-p    :initarg :await-p :documentation "whether wait until audio stops playing on source"))
  (:default-initargs
   :immovablep nil
   :await-p nil
   :abuffer (error "no audio buffer provided on creation")
   :asource (al:gen-source))
  (:documentation "single BUFFER actor type"))

;;--------------------------------------------------


;;--------------------------------------------------

;;--------------------------------------------------

(make-event :nameofthething
            :volume .4
            (cm:cycle :of (list (make-audio "asdf.wav")
                                (make-audio "asdf.wav"))))

(make-event :nameofthething :volume .4 :volume-offset .1 :rate-offset .1
                            :immovable nil :pos-offset (v! 0 -.5 0)
                            :pattern #'cm:heap
            "a.wav" "c.wav")
(when movedp
  (play-event *anevent*)) ; grabs the cm:next

(defun play-event (event position velocity)
  )


(defmethod initialize-instance :after ((instance audio-sound) &key)
  (push (slot-value instance 'asource) *audio-sources*))

(defun list-abuffers ()
  (alexandria:maphash-keys #'print *audio-buffers*))
(defun clear-abuffers ()
  (alexandria:maphash-values #'al:delete-buffer *audio-buffers*))
(defun clear-sources ()
  (map NIL #'al:delete-source *audio-sources*))

(defun load-abuffer (path)
  (or (gethash path *audio-buffers*)
      (let ((buffer (alut:create-buffer-from-file path)))
        (assert (not (zerop buffer)) () "Could not load file! Use wav.")
        (setf (gethash path *audio-buffers*) buffer))))

(defun get-abuffer (abuffer)
  (etypecase abuffer
    (number      abuffer)
    (list        (alexandria:random-elt abuffer))
    (cm::pattern (cm:next abuffer))))

(defgeneric play-sound (obj &key))
(defmethod play-sound ((obj audio-sound) &key (volume 1f0 volume-p)
                                              volume-range
                                              (pitch 1f0)
                                              loop-p)
  (declare (type (or null single-float) pitch)
           (type boolean loop-p)
           (type (or null (single-float 0f0 1f0))
                 volume volume-range))
  (with-slots (asource abuffer await-p) obj
    (when (or (not await-p)
              (eq :STOPPED
                  (al:get-source asource :source-state)))
      (al:source asource :buffer (get-abuffer abuffer))
      (al:source asource :pitch   pitch)
      (al:source asource :looping loop-p)
      (when volume-p
        (al:source
         asource
         :gain
         (min 1f0
              (if volume-range
                  (+ volume (- (random volume-range)
                               (* .5 volume-range)))
                  volume))))
      (al:source-play asource))))

(defun init-audio ()
  (unless *dev*
    (alut:init)
    (setf *dev* t)))

(defmethod init :after ()
  (init-audio))

(defmethod update :after ((camera camera) dt)
  "update OpenAL listener"
  (when *dev*
    (al:listener :position (pos camera))
    (al:listener :orientation (concatenate 'vector (q:to-direction (rot camera)) *vec3-up*))))

;;--------------------------------------------------

(defclass nest (actor audio-sound) ())
(defvar *tmp1* nil)
(defun test-audio ()
  (unless *tmp1*
    (setf *tmp1*
          (make-instance
           'nest
           :abuffer
           (load-abuffer
            "/home/sendai/quicklisp/local-projects/incandescent/static/StepForest1.wav")))))
