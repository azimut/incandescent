(in-package #:incandescent)

;; Base class
;; distance models
(defclass audio ()
  ((buffers  :initarg :buffers  :reader   audio-buffers)
   (paths    :initarg :paths    :reader   audio-paths)
   (source   :initarg :source   :reader   audio-source)
   (relative :initarg :relative :reader   audio-relative)
   (name     :initarg :name     :reader   audio-name)
   (pos      :initarg :pos      :accessor pos))
  (:default-initargs
   :pos (v! 0 0 0)  ; position in local space
   :relative t      ; make basic audio in local space, not world space
   :paths (list)
   :buffers (list)
   :source nil
   :name (gensym))
  (:documentation "bare minimun data to play a file with OpenAL"))

(defgeneric play (obj))
(defgeneric stop (obj))

(defmethod pos ((obj audio))
  (setf (slot-value obj 'pos) (al:get-source (audio-source obj) :position)))
(defmethod (setf pos) :after (val (obj audio))
  "after updating locally update remote"
  (al:source (audio-source obj) :position val))

;; TODO: support pattern?
(defmethod initialize-instance :after ((obj audio) &key name paths relative pos)
  (with-slots (buffers source) obj
    (let ((resolved (mapcar (lambda (_) (load-abuffer (truename _))) paths)))
      (setf buffers resolved)
      (setf source  (init-source name))
      (al:source source :position pos)
      (al:source source :source-relative relative))))

(defun make-audio (name paths &key (pos (v! 0 0 0)))
  (make-instance 'audio :name name :paths paths :pos pos))

(defmethod play :around ((obj audio))
  "ignore order to play if source is busy"
  (let ((state (al:get-source (audio-source obj) :source-state)))
    (unless (eq :PLAYING state)
      (call-next-method))))

(defmethod play ((obj audio))
  "Simplest play, plays the first buffer in buffers"
  (with-accessors ((buffers audio-buffers)
                   (source  audio-source))
      obj
    (let ((buffer (first buffers)))
      (al:source source :buffer buffer)
      (al:source-play source))))

(defmethod stop ((obj audio))
  (al:source-stop (audio-source obj)))
