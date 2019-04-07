(in-package #:incandescent)

;; SDL2-MIX drawbacks:
;; - no real 3d audio, no height
;; - no pitch shifting
;; - AO
;; - reverb
;;
;; NON-interactive - layers added
;;
;;  leaf cracking / crickets / owl (random or a music track)
;;  drone
;;  hearthbeat or just drum
;;  drone2
;;
;; Interactive
;;
;;  foots (walking, change on floor type) - https://freesound.org/search/?q=footstep%20grass&f=grouping_pack:%2223825_Footsteps%22%20&s=%22score%20desc%22&g=1&advanced=&a_tag=&a_filename=&a_description=&a_packname=&a_soundid=&a_username=
;;  jumpscare sound
;;  breathing (while running or stopping)
;;  flashlight
;;  pick item

;; TODO: support music
;;
;; TODO: start delay, random or fixed
;; TODO: multiple events?
;; TODO: default volume
;; TODO: dynamic volume, gain and 3d sound
;; TODO: start the play with sound paused, set the volume and then play it
;; TODO: use sdl mix groups
;; TODO: support different surface per sound???
;;
;; Reference:
;; "Lessons Learned from a Decade of Audio Programming"
;; https://www.youtube.com/watch?v=Vjm--AqG04Y

(defvar *audio-init* nil
  "is the audio initialized?")
(defvar *audio-channels* 4
  "number of channels to reserve on start")
(defvar *audio-chunks* (make-hash-table :test #'equal)
  "cache to store each audio file as a SDL2-FFI:MIX-CHUNK")
(defvar *audio-sounds* (make-hash-table :test #'equal)
  "hash to lookup AUDIO-SOUND objects")

(defclass audio-sound ()
  ((name   :initarg :name   :initform (error "missins name"))
   (chunks :initarg :chunks :initform '())
   (volume :initarg :volume :initform 128)
   (delay  :initarg :delay  :initform   0)
   (loops  :initarg :loops  :initform   0))
  (:documentation "pack of 1 or more chunks of audio"))

(defclass audio-event ()
  ((positional :initarg :positional :initform nil)
   (position   :initarg :position   :initform (v! 0 0 0))
   (channel    :initarg :channel    :initform nil)))

;; I might need
(defparameter *channels-available* '(0 1 2 3))
(defparameter *channels-playing* NIL)
;;--------------------------------------------------

(defun playing-p (channel)
  (declare (type fixnum channel))
  (not (zerop (sdl2-mixer::playing channel))))

(defun chunk-p (s) (sdl2-ffi::mix-chunk-p s))

(defmethod free ((object sdl2-ffi:mix-chunk))
  (when (sdl2-ffi::mix-chunk-validity object)
    (sdl2-mixer:free-chunk object)))

(defun list-chunks () (hash-table-keys *audio-chunks*))
(defun free-chunks ()
  (maphash-values #'free *audio-chunks*)
  (clrhash *audio-chunks*))

(defun load-chunk (path)
  (let* ((absolutep (uiop:absolute-pathname-p path))
         (path      (if absolutep
                        path
                        (asdf:system-relative-pathname :incandescent path))))
    (or (gethash path *audio-chunks*)
        (setf (gethash path *audio-chunks*)
              (sdl2-mixer:load-wav path)))))

;;--------------------------------------------------

(defun stop-audio ()
  (when *audio-init*
    (sdl2-mixer:halt-channel -1)
    (sdl2-mixer:close-audio)
    (free-chunks)
    (sdl2-mixer:quit)
    (setf *audio-init* nil)))

(defun init-audio ()
  (unless *audio-init*
    (sdl2-mixer:init :ogg)
    (sdl2-mixer:open-audio 22050 :s16sys 2 1024)
    (sdl2-mixer:allocate-channels *audio-channels*)
    (setf *audio-init* t)))

;;--------------------------------------------------

;; TODO: print-object
(defun list-sounds () (maphash-keys #'print *audio-sounds*))
(defun free-sounds () (clrhash *audio-sounds*))
(defun make-audio-sound (name chunks &optional (volume 128) (delay 0) (loops 0))
  (declare (type symbol name)
           (type list chunks))
  (when (and (not (emptyp chunks))
             (chunk-p (first chunks)))
    (setf (gethash name *audio-sounds*)
          (make-instance 'audio-sound
                         :name name
                         :chunks chunks
                         :volume volume
                         :delay delay
                         :loops loops))))

(defun angle-to-cam (position)
  (declare (type rtg-math.types:vec3 position))
  (let* ((campos   (pos *camera*))
         (campos   (v! (x campos) 0 (z campos)))
         (position (v! (x position) 0 (z position))))
    (declare (type rtg-math.types:vec3 position campos))
    (round
     (degrees (acos (v3:dot (v3:normalize campos)
                            (v3:normalize position)))))))

(defun distance-to-cam (position)
  (declare (type rtg-math.types:vec3 position))
  (let* ((campos (pos *camera*))
         (campos (v! (x campos) 0 (z campos)))
         (position (v! (x position) 0 (z position))))
    (declare (type rtg-math.types:vec3 position campos))
    (round
     (v3:length (v3:- campos position)))))

(defun get-channel ()
  (or (pop *channels-available*)
      -1))

(defun play-audio (sound position)
  "takes a SOUND and plays on a channel
   TODO: delay"
  (declare (type audio-sound sound)
           (type rtg-math.types:vec3 position))
  (with-slots (chunks volume delay loops) sound
    (let* ((n          (random (length chunks)))
           (chunk      (elt chunks n))
           (channel    (get-channel))
           (campos     (pos *camera*))
           (campos     (v! (x campos)   0 (z campos)))
           (pos        (v! (x position) 0 (z position)))
           (positional NIL))
      (declare (type fixnum channel n))
      (when (>= channel 0)
        (sdl2-mixer:play-channel channel chunk loops)
        (sdl2-mixer:volume channel (- volume (random 10)))
        (unless (v3:0p pos)
          (setf positional T)
          (sdl2-mixer::mix-set-position channel (angle-to-cam pos) (distance-to-cam pos)))
        (push (make-instance 'audio-event
                             :positional positional
                             :position pos
                             :channel channel)
              *channels-playing*))
      channel)))

(defun play-audio-event (sounds &optional (position (v! 0 0 0)))
  "SOUNDS are list of keys on *AUDIO-SOUNDS*
   TODO: position"
  (declare (type list sounds) (type rtg-math.types:vec3 position))
  (map NIL (lambda (sound) (play-audio sound position))
       (mapcar (lambda (k) (gethash k *audio-sounds*)) sounds)))

;; TODO: there is a better way...
(defun update-audio ()
  (let ((new
         (serapeum:filter-map
          (lambda (audio-event)
            (with-slots (positional position channel) audio-event
              (if (playing-p channel)
                  (progn
                    (when positional
                      (sdl2-mixer::mix-set-position channel
                                                    (angle-to-cam position)
                                                    (distance-to-cam position)))
                    audio-event)
                  (progn (pushnew channel *channels-available*) NIL))))
          *channels-playing*)))
    (setf *channels-playing* new)))

;; music fade in out, layers in out

(defun test ()
  ;; (make-audio-sound
  ;;  :footsteps
  ;;  (list (load-chunk "static/421131__giocosound__footstep-grass-1.wav")
  ;;        (load-chunk "static/421130__giocosound__footstep-grass-2.wav")
  ;;        (load-chunk "static/421129__giocosound__footstep-grass-3.wav")
  ;;        (load-chunk "static/421128__giocosound__footstep-grass-4.wav")
  ;;        (load-chunk "static/421135__giocosound__footstep-grass-5.wav"))
  ;;  20)
  (play-audio-event '(:footsteps) (v! 0 0 0)))

;; set-channel-volume  // (sdl2-mixer:volume CHANNEL VOLUME)
;;
;; (sdl2-mixer::mix-set-position channel angle distance)
;; set-3d-listener-and-orientation (position, look, up)
;;
;; stop-channel        // (sdl2-mixer::mix-halt-channel CHANNEL)
;; stop-all-channels   // (sdl2-mixer::mix-halt-channel -1)
