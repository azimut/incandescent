(in-package #:cloud)

(defvar *scheduler* nil)
(defvar *playing*   nil)
(defvar *init*      nil)

(defun init-csound ()
  (setf *scheduler* (make-instance 'scheduler:scheduler))
  (scheduler:sched-run *scheduler*)
  (make-play pewter "i7" :amp 1000 :keynum 60 :bphase .2 :ephase .7 :oamp .6 :oscf 2 :oscm 3 :rev .12)
  (make-play ivory  "i1" :p4 0 :keynum 60 :amp 200 :vib .001 :glis 17.8 :drop .99)
  (make-play swirl "i99" :pan 2)
  (make-play sand  "i9"  :delay .2 :keynum 60 :amp 500 :rev .2 :ramp 6.2 :rfreq 320)
  (make-play taupe "i10" :p4 0 :keynum 60 :amp 500 :rev .8 :ramp 5 :rfreq 223)
  (make-play rust  "i11" :delay 0 :keynum 60 :amp 1200 :rev .2)
  (start-csound (get-orchestra :trapped))
  (start-thread))

(defun scale () (alexandria:random-elt #(0 4 5 7 11 12)))

(let ((shots '(23 64 80 128))
      (trigger nil)
      (started t))
  (defun kill-trigger ()
    (setf trigger nil)
    (setf started nil)
    (setf incandescent::*offset* (rtg-math:v! 0 0 0)))
  (defun voice (root)
    (let ((b (1+ (random 8))))
      (case root
        (48 (play-taupe (print (+ root (scale))) b :amp 800 :rfreq 1000 :rev 6.3))
        (60 (play-taupe (+ root (scale)) b :amp 760 :rfreq 4000)))
      (when trigger
        (setf incandescent::*offset*
              (rtg-math:v3! (- (random .25) .13)
                            (- (random .25) .13)
                            (- (random .25) .13))))
      ;;(format t "started: ~a root: ~a" started root)
      (when (and started (= root 60))
        (setf trigger t)
        (incandescent::rocket-set-row (print (alexandria:random-elt shots))))
      (when *playing*
        (at b #'voice root)))))

(defun play-noise ()
  (unless *init*
    (setf *random-state* (make-random-state t))
    (setf *init* t)
    (setf *playing* t)
    (incandescent::rocket-set-row 0)
    (rocketman::pause-it incandescent::*rocket* 0)
    (voice 48)
    (at  8 #'voice 60)
    (at 20 #'(lambda ()
               (incandescent::rocket-set-row 160)
               (kill-trigger))))
  (at 26 #'(lambda ()
             (setf *playing* nil)
             (incandescent::play :stop))))


