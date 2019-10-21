(in-package #:cloud)

(defvar *minor* '(0 2 3 5 7 8 10))
;; BASS
;; 0 0 +12 0
;; 5 3 2 0
;; LEAD
;; 0 5 9 7
;; 2 5 9 7
;; ....
;; 4 5 9 0   7
;; 2 5 9 5 4 7
;; 4 5 9 0   7
(defvar *init* nil)

;; https://github.com/ReneNyffenegger/csound-instruments
(make-orc
 :otis
 :filename "otis"
 :filepath
 ;;(truename "static/")
 (truename "/home/sendai/quicklisp/local-projects/incandescent/static/"))

;; (make-play bell    "i5"  :freq 60      :amp .032 :ifn  4     :nfreq1 70.006 :nfreq2 169.437)
;; (make-play drums   "i7"  :amp  .799    :ifn 3    :freq 203   :ratio  1.416  :idx    4.99    :pan .511)
;; (make-play drone   "i8"  :freq 60      :amp .399 :atk  9.999 :idx    3)
;; (make-play timpani "i14" :freq 48.695  :amp .046 :ifn  3     :nfreq1 5.347  :nfreq2 95.939)
;; (make-play otis15  "i15" :freq 443.713 :amp 200  :pan  .401)


(progn
  (make-play otis15  "i1" :freq 443.713 :amp 200  :pan  .401)
  (make-play drums   "i2"  :amp  .799    :ifn 3    :freq 203   :ratio  1.416  :idx    4.99    :pan .511)
  (make-play drone   "i3"  :freq 60      :amp .399 :atk  9.999 :idx    3)
  (make-play bell    "i5"  :freq 60      :amp .032 :ifn  4     :nfreq1 70.006 :nfreq2 169.437)
  (make-play timpani "i5" :freq 48.695  :amp .046 :ifn  3     :nfreq1 5.347  :nfreq2 95.939)
  )

(progn (make-play plucke "i6" :p4 0 :keynum 60)
       (make-play pluck  "i7" :p4 0 :keynum 60)
       (make-play newfm  "i8" :p4 0 :keynum 60 :llimit .2 :rlimit 2.0))


;; (play-drone   58 10 :amp .5)
;; (play-otis15  50 8  :amp 1000 :pan .4)
;; (play-drums   20 .5)
;; (play-timpani 20 4  :amp .1)
;; (play-timpani 60 4  :amp .1)
;; (play-bell-arp (cm:transpose '(60 62 65) 24) .5 .5)

(defvar *init* nil)
(defun init-audio ()
  (unless *init*
    (scheduler:sched-run *scheduler*)
    (start-csound (merge-orcs (get-orchestra :otis) (get-orchestra :xanadu)))
    (start-thread)
    (setf *init* t)))

(in-package #:incandescent)
(defun init-audio ()
  (cloud::init-audio))
