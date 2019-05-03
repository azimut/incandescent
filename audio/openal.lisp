(in-package :incandescent)

;; OpenAL seems the most api complete option. But sbcl crashes with the following if alut:init or alc:open-device was run and an exception occurrs...so...I give up
;;
;; fatal error encountered in SBCL pid 5623(tid 0x7f4c2ae14040):
;; deferrable signals partially blocked: {13}
;;

(defvar *dev* nil)
(defvar *audio-sources* nil)
(defvar *audio-buffers* (make-hash-table :test #'equal))

(defun make-cycle (elements &optional (for-elements 1 for-elements-p))
  "loops over elements in a continuous cycle"
  (when elements ;; do not make a cycle of ONLY nil (?
    (if for-elements-p
        (cm:new cm:cycle :of elements :for for-elements)
        (cm:new cm:cycle :of elements))))

(defclass audio-sound ()
  ((abuffer :initarg :abuffer
            :documentation "OPENAL audio buffer")
   (asource :initarg :asource
            :documentation "OPENAL player")
   (await-p :initarg :await-p
            :documentation "whether wait until audio stops playing on source"))
  (:default-initargs
   :await-p nil
   :abuffer (error "no audio buffer provided on creation")
   :asource (al:gen-source))
  (:documentation "single BUFFER actor type"))

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
(defmethod play-sound ((obj audio-sound) &key
                                         (volume 1f0 volume-p)
                                         volume-range
                                         (pitch 1f0)
                                         loop-p)
  (declare (type (or null single-float) pitch)
           (type boolean loop-p)
           (type (or null (single-float 0f0 1f0)) volume volume-range))
  (with-slots (asource abuffer await-p) obj
    (when (or (not await-p)
              (eq :STOPPED (al:get-source asource :source-state)))
      (al:source asource :buffer (get-abuffer abuffer))
      (al:source asource :pitch pitch)
      (al:source asource :looping loop-p)
      (when volume-p
        (al:source asource :gain (min 1f0
                                      (if volume-range
                                          (+ volume (- (random volume-range)
                                                       (* .5 volume-range)))
                                          volume))))
      (al:source-play asource))))

(defun init-audio ()
  ;;(alut:init)
  (unless *dev*
    (setf *dev* (alc:open-device))))

(defun update-audio ()
  (al:listener :position (pos *camera*))
  (al:listener :orientation (concatenate 'vector
                                         (q:to-direction *camera*)
                                         *vec3-up*)))

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
