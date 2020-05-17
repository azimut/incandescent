(in-package #:incandescent)

(defclass audio ()
  ((buffers  :reader   audio-buffers)
   (paths    :reader   audio-paths    :initarg :paths)
   (source   :reader   audio-source   :initarg :source)
   (relative :reader   audio-relative :initarg :relative)
   (name     :reader   audio-name     :initarg :name)
   (pos      :accessor pos            :initarg :pos))
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
    (when (not (eq :PLAYING state))
      (call-next-method))))

(defmethod play ((obj audio))
  (let ((buffer (first (audio-buffers obj))))
    (al:source (audio-source obj) :buffer buffer)
    (al:source-play (audio-source obj))))

(defmethod stop ((obj audio))
  (al:source-stop (audio-source obj)))
