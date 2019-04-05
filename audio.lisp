(in-package #:incandescent)

;; NON-interactive - layers added
;;
;;  leaf cracking / crickets / owl (random or a music track)
;;  drone
;;  hearthbeat or just drum
;;  drone2
;;
;; Interactive
;;
;;  foots (walking, change on floor type)
;;  jumpscare sound
;;  breathing (while running or stopping)
;;  flashlight
;;  pick item

;; TODO: support music
;; TODO: pitch randomization (?..not to be applied on things with pitch
;; TODO: multiple sounds per event, like load several "footsteps" and pick one random
;; TODO: start delay, random or fixed
;; TODO: multiple events?
;; TODO: default volume
;; TODO: dynamic volume, gain and 3d sound
;; TODO: start the play with sound paused, set the volume and then play it
;;
;; "Lessons Learned from a Decade of Audio Programming"
;; https://www.youtube.com/watch?v=Vjm--AqG04Y

(defvar *audio-init* nil)
(defvar *audio-channels* 4)
(defvar *audio-chunks* (make-hash-table :test #'equal)
  "cache loaded chunks")

(defun free-chunk (path)
  (let ((s (gethash path *audio-chunks*)))
    (when s
      (sdl2-mixer:free-chunk s))
    (remhash path *audio-chunks*)))

(defun load-chunk (path &optional force-p)
  (when force-p
    (free-chunk path))
  (let ((absolutep (uiop:absolute-pathname-p path)))
    (or (gethash path *audio-chunks*)
        (setf (gethash path *audio-chunks*)
              (sdl2-mixer:load-wav
               (if absolutep
                   path
                   (asdf:system-relative-pathname :incandescent path)))))))

(defun free-chunks ()
  (maphash-values #'sdl2-mixer:free-chunk *audio-chunks*)
  (clrhash *audio-chunks*))

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

;;(sdl2-mixer:play-channel 0 (load-chunk ) 0)
;;
;; set-channel-3d-position (channel, position)
;; set-3d-listener-and-orientation (position, look, up)
;;
;; stop-channel        // (sdl2-mixer::mix-halt-channel CHANNEL)
;; stop-all-channels   // (sdl2-mixer::mix-halt-channel -1)
;; set-channel-volume  // (sdl2-mixer:volume CHANNEL VOLUME)
;; playing-p (channel) // (sdl2-mixer:playing CHANNEL)

;; update() - going to all channels, and removing stopped things (?
