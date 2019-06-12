(in-package :incandescent)

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

(defmethod control :after ((camera camera) dt factor)
  (declare (type fixnum factor))
  (when (or (key-down-p key.w)
            (key-down-p key.a)
            (key-down-p key.s)
            (key-down-p key.d))
    (let ((campos (pos camera))
          (s      (alexandria:random-elt
                   (slot-value
                    (gethash :footsteps *audio-sounds*)
                    'sources)))) ;; FIXME!
      ;; NOTE: negative Y on sound position workaround an audio glitch where
      ;;       sound played on 1 channel at times
      (ecase factor
        (5  (and (funcall *crawl*)
                 (setf (harmony:input-location s *sfx*)
                       (v! (x campos) -10 (z campos)))
                 (harmony-simple:resume s)))
        (10  (and (funcall *walk*)
                  (setf (harmony:input-location s *sfx*)
                        (v! (x campos) -10 (z campos)))
                  (harmony-simple:resume s)))
        (20 (and (funcall *run*)
                 (setf (harmony:input-location s *sfx*)
                       (v! (x campos) -10 (z campos)))
                 (harmony-simple:resume s)))))))
