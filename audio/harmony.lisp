(in-package #:incandescent)

;; TODO: this kind of limits usage...
;; TODO: for music it would be nice to have a weighted or boolean gated graph (? so i could see add more "generative" music sequences

(defvar *sfx* nil "sfx mixer copy")
(defvar *audio-sources* (make-hash-table :test #'equal)
  "cache for MP3-SOURCES")
(defvar *audio-sounds*  (make-hash-table)
  "lookup table of AUDIO-SOUND objects")

(deftype mixer () '(member :sfx :music))

(defclass audio-sound (actor)
  ((name     :initarg :name)
   (sources  :initarg :sources :documentation "list of MP3-SOURCES")
   (volume   :initarg :volume))
  (:default-initargs
   :name (error "missing name")
   :sources '()
   :volume 1f0)
  (:documentation "pack of audio sources of the same type"))

(defclass audio-music ()
  ((name      :initarg :name)
   (sources   :initarg :sources   :documentation "list of MP3-SOURCES")
   (volume    :initarg :volume)
   (fade-to   :initarg :fade-to   :documentation "volume target")
   (fade-time :initarg :fade-time :documentation "time in sec to fade"))
  (:default-initargs
   :name (error "missing name")
   :sources '()
   :volume 1f0
   :fade-to 1f0
   :fade-time 1f0)
  (:documentation "pack of audio sources of the same type"))

(defun list-sources () (alexandria:hash-table-keys *audio-sources*))
(defun list-sounds  () (alexandria:hash-table-keys *audio-sounds*))

(defun init-audio ()
  (prog1 (harmony-simple:initialize
          :output-spec '(harmony-pulse:pulse-drain))
    (setf *sfx* (harmony-simple:segment :sfx))))

(defun %load-source (name path mixer loop-p)
  (declare (type symbol name) (type string path) (type mixer mixer))
  (let* ((absolutep (uiop:absolute-pathname-p path))
         (path      (if absolutep
                        path
                        (asdf:system-relative-pathname :incandescent path))))
    (or (gethash path *audio-sources*)
        (setf (gethash path *audio-sources*)
              (harmony-simple:play (uiop:ensure-pathname path) mixer
                                   :name name
                                   :paused T
                                   :loop loop-p)))))

(defun %init-source (source &rest initargs)
  "sets parameters to source that cannot be set on play directly"
  (declare (type harmony-mp3:mp3-source source))
  (when (harmony-simple:paused-p source)
    (destructuring-bind (&key volume fade-to fade-time) initargs
      (when volume
        (setf (harmony-simple:volume source) volume))
      (when (and fade-to fade-time)
        (harmony-simple:fade source fade-to fade-time))))
  source)

(defun load-sfx (name path &rest initargs)
  (apply #'%init-source (%load-source name path :sfx NIL) initargs))

(defun load-music (name path &rest initargs)
  (apply #'%init-source (%load-source name path :music T) initargs))

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
                       :sources (mapcar
                                 (lambda (path)
                                   (load-sfx name path
                                             :volume volume))
                                 paths))))

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
                       :sources (mapcar
                                 (lambda (path)
                                   (load-music name path
                                               :volume volume
                                               :fade-to fade-to
                                               :fade-time fade-time))
                                 paths))))

;;--------------------------------------------------
;; Runtime code

(defmethod flow:check-connection-accepted progn (new-connection (port flow:port)))
(in-package :harmony-simple)
(defmethod make-pipeline ((server default-server))
  (let* ((*server* server)
         (pipeline (make-instance 'pipeline))
         (output (apply #'make-segment (output-spec server)))
         (master (make-segment 'basic-mixer :name :master))
         (music (make-segment 'basic-mixer :name :music :channels 1))
         (sfx (make-segment 'space-mixer :name :sfx))
         (voice (make-segment 'space-mixer :name :voice)))
    (connect pipeline master 0 output 0)
    (connect pipeline master 1 output 1)
    (connect pipeline music 0 master 0)
    (connect pipeline music 0 master 1)
    (connect pipeline sfx 0 master 2)
    (connect pipeline sfx 1 master 3)
    (connect pipeline voice 0 master 4)
    (connect pipeline voice 1 master 5)
    (setf (volume master) 0.8)
    (setf (volume sfx) 0.8)
    pipeline))

(in-package :incandescent)
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
  (declare (type symbol name))
  (let ((sound    (gethash name *audio-sounds*))
        (sync-to  (playing-p :music)))
    (declare (type (or null audio-music) sound)
             (type (or null harmony-mp3:mp3-source) sync-to))
    (with-slots (sources) sound
      (let ((source (elt sources 0)))
        (when sync-to
          (harmony:seek source (harmony:sample-position sync-to)))
        (harmony-simple:resume source)))))

(let ((stepper (make-stepper (seconds 1) (seconds 1))))
  (defun update-audio ()
    "runs on main loop to update 3D audio positions"
    (when (funcall stepper)
      ;; Camera
      (setf (harmony-simple:location  *sfx*) (pos *camera*))
      (setf (harmony-simple:direction *sfx*) (q:to-direction (rot *camera*)))
      ;; Objects position update
      (map 'vector
           (lambda (mp3)
             (when-let ((playing (not (harmony-simple:paused-p mp3)))
                        (mp3name (harmony:name mp3)))
               (setf (harmony:input-location mp3 *sfx*)
                     (pos (gethash mp3name *audio-sounds*)))))
           (cl-mixed:sources *sfx*)))
    NIL))

;;--------------------------------------------------
;; Test code

(let ((state T))
  (defun test-stop-music ()
    (setf state (not state))
    (setf (harmony:looping-p
           (load-sfx :curso201 "static/tarea201-mono.mp3"))
          state)
    (setf (harmony:looping-p
           (load-sfx :curso202 "static/tarea202-mono.mp3"))
          state)
    (setf (harmony:looping-p
           (load-sfx :curso203 "static/tarea203-mono.mp3"))
          state)))

(defun test-music ()
  (make-music :curso201 .01 .3 5f0 "static/tarea201-mono.mp3")
  (make-music :curso202 .01 .2 5f0 "static/tarea202-mono.mp3")
  (make-music :curso203 .01 .3 5f0 "static/tarea203-mono.mp3"))

(defun test-sound ()
  (make-sound
   :footsteps .2
   "static/421131__giocosound__footstep-grass-1.mp3"
   "static/421130__giocosound__footstep-grass-2.mp3"
   "static/421129__giocosound__footstep-grass-3.mp3"
   "static/421128__giocosound__footstep-grass-4.mp3"
   "static/421135__giocosound__footstep-grass-5.mp3"))
