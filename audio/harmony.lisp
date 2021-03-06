(in-package #:incandescent)

;; NOTE: bugs on upstream
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
  ((name    :initarg :name)
   (sources :initarg :sources :documentation "list of MP3-SOURCES")
   (wait-p  :initarg :wait-p  :documentation "wheter wait until sound on sources stopped")
   (end-p   :initarg :end-p   :documentation "no more things to play")
   (playing :initarg :playing :documentation "MP3:SOURCE playing right now")
   (volume  :initarg :volume))
  (:default-initargs
   :sources '()
   :wait-p nil
   :end-p nil
   :playing nil
   :name (gensym)
   :volume 1f0)
  (:documentation "pack of audio sources of the same type"))

(defclass audio-music ()
  ((name      :initarg :name)
   (sources   :initarg :sources   :documentation "list of MP3-SOURCES")
   (volume    :initarg :volume)
   (fade-to   :initarg :fade-to   :documentation "volume target")
   (fade-time :initarg :fade-time :documentation "time in sec to fade"))
  (:default-initargs
   :sources '()
   :volume 1f0
   :name (gensym)
   :fade-to 1f0
   :fade-time 1f0)
  (:documentation "pack of audio sources of the same type"))

(defun list-sources () (alexandria:hash-table-keys *audio-sources*))
(defun list-sounds  () (alexandria:hash-table-keys *audio-sounds*))

(defun init-audio ()
  (unless (harmony-simple:started-p harmony-simple:*server*)
    (harmony-simple:initialize :output-spec '(harmony-pulse:pulse-drain))
    (setf *sfx* (harmony-simple:segment :sfx))
    (setf (harmony:max-distance *sfx*) *default-max-distance*))
  T)

;;--------------------------------------------------

;; Update listener to match current camera
(let ((stepper (make-stepper (seconds .05) (seconds .05))))
  (defmethod update :after ((camera pers) dt)
    (when (funcall stepper)
      (setf (harmony-simple:location  *sfx*) (pos camera))
      (setf (harmony-simple:direction *sfx*) (q:to-direction (rot camera))))))

;; Update position of sound attached to an actor in *actors*
(defmethod update :after ((actor audio-sound) dt)
  (with-slots (pos playing) actor
    (when playing
      (if (harmony:paused-p playing)
          (setf playing nil)
          (setf (harmony:input-location playing *sfx*) pos)))))

;;--------------------------------------------------

(defun %load-source (path mixer)
  (declare (type mixer mixer))
  (let ((realpath (resolve-path path)))
    (or (gethash realpath *audio-sources*)
        (setf (gethash path *audio-sources*)
              (harmony-simple:play realpath mixer
                                   :paused T)))))

(defun %init-source (source &rest initargs)
  "sets parameters to source that cannot be set on harmony:play directly"
  (declare (type harmony-mp3:mp3-source source))
  (destructuring-bind (&key volume fade-to fade-time) initargs
    (when volume
      (setf (harmony-simple:volume source) volume))
    (when (and fade-to fade-time)
      (harmony-simple:fade source fade-to fade-time)))
  source)

(defun load-sfx (path &rest initargs)
  "loads and cache a mp3 file in PATH into SFX mixer"
  (apply #'%init-source (%load-source path :sfx) initargs))

(defun load-music (path &rest initargs)
  "laods and cache a mp3 file in PATH into MUSIC mixer"
  (apply #'%init-source (%load-source path :music) initargs))

;;--------------------------------------------------
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
                                   (load-sfx path
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

;; NOTE: for some reason there are integers on sources for mixer
(defmethod harmony:paused-p ((server fixnum)) T)

(defun now-playing (&optional (mixer :music))
  "returns the track currently playing on the MIXER or NIL"
  (declare (type mixer mixer))
  (find-if-not #'harmony:paused-p
               (cl-mixed:sources (harmony-simple:segment mixer))))

;;--------------------------------------------------

(defun get-source (sources)
  (let ((s (typecase sources
             (harmony-mp3:mp3-source sources)
             (list                   (alexandria:random-elt sources))
             (cm::pattern            (cm:next sources)))))
    (if (eq s :END-OF-DATA)
        nil
        s)))

;; Stop a sound by passing NIL to loop-p
(defgeneric play-sound (name &key))
(defmethod play-sound ((name harmony:source) &key loop-p pause-p)
  (setf (harmony-simple:looping-p name) loop-p)
  (if pause-p
      (harmony-simple:pause name)
      (harmony-simple:resume name)))
(defmethod play-sound ((name audio-sound) &key loop-p pause-p seek-to)
  (declare (type boolean loop-p pause-p)
           (type (or null fixnum) seek-to))
  (with-slots (sources volume wait-p end-p playing) name
    (when (or (and wait-p playing)
              (not wait-p)
              pause-p
              (not end-p))
      (alexandria:if-let ((s (get-source sources)))
        (progn
          (when seek-to (harmony:seek-to-sample s seek-to))
          (setf (harmony-simple:looping-p s) loop-p)
          (setf (harmony-simple:volume s) (- volume (random .02)))
          (if pause-p
              (progn
                (setf playing nil)
                (harmony-simple:pause s))
              (progn
                (setf playing s) ;; NOTE: Might overwrite one already playing...it's fine
                (harmony-simple:resume s))))
        (setf end-p t)))))
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
        (sync-to  (now-playing :music)))
    (declare (type (or null audio-music) sound)
             (type (or null harmony-mp3:mp3-source) sync-to))
    (with-slots (sources) sound
      (let ((source (elt sources 0)))
        (when sync-to
          (harmony:seek source (harmony:sample-position sync-to)))
        (harmony-simple:resume source)))))
