(in-package #:incandescent)

;; TODO: this kind of limits usage...
;; TODO: for music it would be nice to have a weighted or boolean gated graph (? so i could see add more "generative" music sequences
;; TODO: heap, cycle additionally to random pick

(defvar *sfx* nil "sfx mixer copy")
(defvar *audio-sources* (make-hash-table :test #'equal)
  "cache for MP3-SOURCES")
(defvar *audio-sounds*  (make-hash-table)
  "lookup table of AUDIO-SOUND objects")
(defvar *default-max-distance* 1000f0
  "max distance we can hear something on default sfx mixer.
   NOTE: harmony default is 100K")

(deftype mixer () '(member :sfx :music))

(defclass audio-sound ()
  ((name         :initarg :name)
   (sources      :initarg :sources :documentation "list of MP3-SOURCES")
   (volume       :initarg :volume))
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
  (unless (harmony-simple:started-p harmony-simple:*server*)
    (prog1 (harmony-simple:initialize
            :output-spec '(harmony-pulse:pulse-drain))
      (setf *sfx* (harmony-simple:segment :sfx))))
  (setf (harmony:max-distance *sfx*) *default-max-distance*))

(defun %load-source (name path mixer)
  (declare (type symbol name) (type string path) (type mixer mixer))
  (let* ((absolutep (uiop:absolute-pathname-p path))
         (path      (if absolutep
                        path
                        (asdf:system-relative-pathname :incandescent path))))
    (or (gethash path *audio-sources*)
        (setf (gethash path *audio-sources*)
              (harmony-simple:play (uiop:ensure-pathname path) mixer
                                   :name name
                                   :paused T)))))

(defun %init-source (source &rest initargs)
  "sets parameters to source that cannot be set on harmony:play directly"
  (declare (type harmony-mp3:mp3-source source))
  ;;(when (harmony-simple:paused-p source))
  (destructuring-bind (&key volume fade-to fade-time) initargs
    (when volume
      (setf (harmony-simple:volume source) volume))
    (when (and fade-to fade-time)
      (harmony-simple:fade source fade-to fade-time)))
  source)

(defun load-sfx (name path &rest initargs)
  "loads and cache a mp3 file in PATH into SFX mixer"
  (apply #'%init-source (%load-source name path :sfx) initargs))

(defun load-music (name path &rest initargs)
  "laods and cache a mp3 file in PATH into MUSIC mixer"
  (apply #'%init-source (%load-source name path :music) initargs))

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

;; HACKS!
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
;; HACKS END HERE ... sorta

(in-package :incandescent)
(defmethod harmony:paused-p ((server fixnum)) T)

(defun playing-p (&optional (mixer :music))
  "returns the track currently playing on the MIXER or NIL"
  (declare (type mixer mixer))
  (find-if-not #'harmony:paused-p
               (cl-mixed:sources (harmony-simple:segment mixer))))

;;--------------------------------------------------

;; Stop a sound by passing NIL to loop-p
(defgeneric play-sound (name &key loop-p pause-p))
(defmethod play-sound ((name harmony:source) &key loop-p pause-p)
  (setf (harmony-simple:looping-p name) loop-p)
  (if pause-p
      (harmony-simple:pause name)
      (harmony-simple:resume name)))
(defmethod play-sound ((name audio-sound) &key loop-p pause-p)
  (with-slots (sources volume) name
    (let ((s (if (listp sources)
                 (alexandria:random-elt sources)
                 sources)))
      (setf (harmony-simple:looping-p s) loop-p)
      (setf (harmony-simple:volume s) (- volume (random .02)))
      (if pause-p
          (harmony-simple:pause s)
          (harmony-simple:resume s)))))
(defmethod play-sound ((name symbol) &key loop-p pause-p)
  (declare (type symbol name) (type boolean loop-p))
  (assert (keywordp name))
  (with-slots (sources volume) (gethash name *audio-sounds*)
    (let ((s (alexandria:random-elt sources)))
      (setf (harmony-simple:looping-p s) loop-p)
      (setf (harmony-simple:volume s) (- volume (random .02)))
      (if pause-p
          (harmony-simple:pause s)
          (harmony-simple:resume s)))))

;;--------------------------------------------------
(defgeneric play-music (name))
(defmethod play-music ((name symbol))
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

;;--------------------------------------------------

(let ((stepper (make-stepper (seconds .05) (seconds .05))))
  (defun update-audio ()
    "runs on main loop to update 3D audio positions"
    (when (funcall stepper)
      ;; Camera
      (setf (harmony-simple:location  *sfx*) (pos *camera*))
      (setf (harmony-simple:direction *sfx*) (q:to-direction (rot *camera*)))
      ;; Objects position update
      ;; (map 'vector
      ;;      (lambda (mp3)
      ;;        (when-let ((playing (not (harmony-simple:paused-p mp3)))
      ;;                   (mp3name (harmony:name mp3)))
      ;;          (setf (harmony:input-location mp3 *sfx*)
      ;;                (pos (gethash mp3name *audio-sounds*)))))
      ;;      (cl-mixed:sources *sfx*))
      )
    NIL))

;;--------------------------------------------------
;; Test code

(let ((state T))
  (defun test-stop-music ()
    (setf state (not state))
    (setf (harmony:looping-p
           (load-music :curso201 "static/tarea201-mono.mp3"))
          state)
    (setf (harmony:looping-p
           (load-music :curso202 "static/tarea202-mono.mp3"))
          state)
    (setf (harmony:looping-p
           (load-music :curso203 "static/tarea203-mono.mp3"))
          state)))

(defun test-music ()
  (make-music :curso201 .01 .3 5f0 "static/tarea201-mono.mp3")
  (make-music :curso202 .01 .2 5f0 "static/tarea202-mono.mp3")
  (make-music :curso203 .01 .3 5f0 "static/tarea203-mono.mp3"))

(defun test-sound ()
  (make-sound
   :generator .1
   "/home/sendai/Downloads/scpcb-master/SFX/General/GeneratorOn.ogg.mp3")
  (make-sound
   :footsteps .2
   ;; "static/StepForest1.ogg.mp3"
   ;; "static/StepForest2.ogg.mp3"
   ;; "static/StepForest3.ogg.mp3"
   "static/421131__giocosound__footstep-grass-1.mp3"
   "static/421130__giocosound__footstep-grass-2.mp3"
   "static/421129__giocosound__footstep-grass-3.mp3"
   "static/421128__giocosound__footstep-grass-4.mp3"
   ;;"static/421135__giocosound__footstep-grass-5.mp3"
   ))
