(in-package #:incandescent)

;; TODO: this kind of limits usage...

(defvar *audio-sources* (make-hash-table :test #'equal)
  "cache for MP3-SOURCES")
(defvar *audio-sounds*  (make-hash-table)
  "lookup table of AUDIO-SOUND objects")

(deftype mixer () '(member :sfx :music))

(defclass audio-music ()
  ((name :initarg :name)
   (sources :initarg :sources)
   (volume :initarg :volume)))

(defclass audio-sound ()
  ((name    :initarg :name)
   (sources :initarg :sources :documentation "list of MP3-SOURCES")
   (volume  :initarg :volume))
  (:default-initargs
   :name (error "missing name")
   :sources '()
   :volume 1f0)
  (:documentation "pack of audio sources of the same type"))

(defclass audio-music ()
  ((name      :initarg :name)
   (sources   :initarg :sources :documentation "list of MP3-SOURCES")
   (volume    :initarg :volume)
   (fade-to   :initarg :fade-to :documentation "volume target")
   (fade-time :initarg :fade-time :documentation "time in sec to fade"))
  (:default-initargs
   :name (error "missing name")
   :sources '()
   :volume 1f0
   :fade-to NIL
   :fade-time NIL)
  (:documentation "pack of audio sources of the same type"))

(defun list-sources () (alexandria:hash-table-keys *audio-sources*))

(defun init-audio ()
  (harmony-simple:initialize :output-spec '(harmony-pulse:pulse-drain)))

(defun load-source (path &optional (mixer :sfx) (loop-p NIL))
  (declare (type string path) (type mixer mixer) (type boolean loop-p))
  (let* ((absolutep (uiop:absolute-pathname-p path))
         (path      (if absolutep
                        path
                        (asdf:system-relative-pathname :incandescent path))))
    (or (gethash path *audio-sources*)
        (setf (gethash path *audio-sources*)
              (harmony-simple:play (uiop:ensure-pathname path) mixer
                                   :paused T
                                   :loop loop-p)))))

(defun load-sfx (path) (load-source path :sfx))
(defun load-music (path) (load-source path :music T))

;; TODO: positional
(defun make-sound (name volume &rest paths)
  (declare (type symbol name)
           (type list paths)
           (type (single-float 0f0 1f0) volume))
  (assert (keywordp name))
  (setf (gethash name *audio-sounds*)
        (make-instance 'audio-sound
                       :name name
                       :volume volume
                       :sources (mapcar #'load-sfx paths))))

(defun make-music (name volume fade-to fade-time &rest paths)
  (declare (type symbol name)
           (type list paths)
           (type single-float fade-time)
           (type (single-float 0f0 1f0) volume fade-to))
  (assert (and (keywordp name)))
  (setf (gethash name *audio-sounds*)
        (make-instance 'audio-music
                       :name name
                       :volume volume
                       :fade-to fade-to
                       :fade-time fade-time
                       :sources (mapcar #'load-music paths))))

;;--------------------------------------------------
;; Runtime code

(declaim (inline source-p))
(defmethod harmony:paused-p ((server fixnum)) T)
(defun playing-p (&optional (mixer :music))
  "returns the track currently playing on the MIXER or NIL"
  (declare (type mixer mixer))
  (find-if-not #'harmony:paused-p
               (cl-mixed:sources (harmony-simple:segment mixer))))

(defun %play-sound (name)
  (declare (type symbol name))
  (assert (keywordp name))
  (with-slots (sources volume) (gethash name *audio-sounds*)
    (let* ((l (length sources))
           (r (random l))
           (s (nth r sources)))
      (setf (harmony-simple:volume s) (- volume (random .1)))
      (harmony-simple:resume s))))

(defun play-sound (&rest names)
  "Plays one or more AUDIO-SOUNDS
  > (play-sound :footsteps :cloth :chains)"
  (map NIL #'%play-sound names))

(defun play-music (name)
  (let ((sound   (gethash name *audio-sounds*))
        (sync-to (playing-p :music)))
    (with-slots (sources) sound
      (let ((source (elt sources 0)))
        ;; (when sync-to
        ;;   (harmony:seek source (harmony:sample-position sync-to)))
        (harmony-simple:resume source)))))

(let ((state nil))
  (defun test-stop-music ()
    (setf state (not state))
    (setf (harmony:looping-p (load-source "static/tarea201.mp3")) state)
    (setf (harmony:looping-p (load-source "static/tarea202.mp3")) state)
    (setf (harmony:looping-p (load-source "static/tarea203.mp3")) state)))

(defun test-music ()
  (make-music :curso201 .3 .5 2f0 "static/tarea201.mp3")
  (make-music :curso202 .3 .5 2f0 "static/tarea202.mp3")
  (make-music :curso203 .3 .5 2f0 "static/tarea203.mp3"))

(defun test-sound ()
  (make-sound
   :footsteps .4
   "static/421131__giocosound__footstep-grass-1.mp3"
   "static/421130__giocosound__footstep-grass-2.mp3"
   "static/421129__giocosound__footstep-grass-3.mp3"
   "static/421128__giocosound__footstep-grass-4.mp3"
   "static/421135__giocosound__footstep-grass-5.mp3"))
