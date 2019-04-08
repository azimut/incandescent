(in-package #:incandescent)

(defvar *audio-buffers* (make-hash-table :test #'equal))
(defvar *audio-sources* (make-hash-table :test #'equal))

(defun list-buffers () (alexandria:hash-table-keys *audio-buffers*))
(defun list-sources () (alexandria:hash-table-keys *audio-sources*))

(defun init-audio ()
  (harmony-simple:initialize :output-spec '(harmony-pulse:pulse-drain))
  ;;(harmony-simple:initialize :output-spec '(harmony-out123:out123-drain))
  ;;(harmony-simple:initialize :output-spec '(harmony-openal:openal-drain))
  ;;(harmony-simple:initialize)
  )

(defun load-buffer (path)
  (declare (type string path))
  (or (gethash path *audio-buffers*)
      (setf (gethash path *audio-buffers*)
            (harmony-simple:decode (uiop:ensure-pathname path)))))

(defun load-source (path)
  (declare (type string path))
  (or (gethash path *audio-sources*)
      (setf (gethash path *audio-sources*)
            (harmony-simple:play 'harmony:buffer-source
                                 :sfx
                                 :paused T
                                 :buffers (list (first (load-buffer path)))))))

(defun play-sound (path)
  (declare (type (or string pathname) path))
  (let ((source (load-source path)))
    (harmony-simple:resume source))
  (values))

(defun make-audio-sound (name sources)
  "(make-audio-sound :footsteps \"a.wav\" \"b.wav\")"
  )

(defun test-audio (path)
  (harmony-simple:decode path)
  ;;(harmony-simple:play #p"/home/sendai/tarea201.mp3" :music :loop T)
  )

(defvar *audio-sounds* (make-hash-table)
  "lookup table of sounds")
(defclass audio-sound ()
  ((name    :initarg :name    :initform (error "missing name"))
   (sources :initarg :sources :initform '())))
