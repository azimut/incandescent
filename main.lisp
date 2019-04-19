(in-package #:incandescent)

(defvar *last-time* (get-internal-real-time))
(defvar *bs* nil)
(defparameter *stepper* (make-stepper (seconds .5) (seconds .5)))
(defvar *dimensions-v2* NIL)

;;(defparameter *dimensions* '(1366 768))
;; (defparameter *dimensions* '(683 384))
(defparameter *dimensions* '(533 400))
;;(defparameter *dimensions* '(227 365))
;;(defparameter *dimensions* '(32 32))
;;(defparameter *dimensions* '(455 256))
;;(defparameter *dimensions* '(341 192))

(defun move-thing ()
  (let ((newpos (v! (+ -5 (random 10f0)) 0 (+ -5 (random 10f0)))))
    (setf (pos (find-actor-class 'box))
          newpos)))

(defun init ()
  (setf *dimensions-v2* (v! *dimensions*))
  ;;--------------------------------------------------
  ;; Buffer stream for single stage pipelines
  (unless *bs* (setf *bs* (make-buffer-stream nil :primitive :points)))
  ;;--------------------------------------------------
  ;; HDR fbo(s)
  (when *fbo* (free *fbo*))
  (setf *fbo*
        (make-fbo
         `(0 :element-type :rgba16f
             :dimensions ,*dimensions*)
         `(1 :element-type :rgba16f
             :dimensions ,*dimensions*)
         (list :d :dimensions *dimensions*)))
  (setf *sam*  (sample (attachment-tex *fbo* 0)  :wrap :clamp-to-edge))
  (setf *sam1* (sample (attachment-tex *fbo* 1)  :wrap :clamp-to-edge))
  (setf *samd* (sample (attachment-tex *fbo* :d) :wrap :clamp-to-edge))
  ;;---------------------------------------------- ----
  (setf (clear-color) (v! 0 0 0 1))
  ;;--------------------------------------------------
  (setf *actors* nil)
  ;;(make-env-map *cube-tex* *cube-sam*)
  ;;(make-env-map *cube-tex* *s-cubemap-prefilter*)
  ;;(make-celestial-sphere)
  ;;(init-box)
  ;;(make-box)
  NIL)

(defun draw! ()
  (let* ((res   (surface-resolution (current-surface)))
         (now   (get-internal-real-time))
         (time  (* .1 now))
         (delta (* (- now *last-time*) .001))
         (delta (if (> delta .16) .00001 delta)))
    (setf *last-time* now)
    (setf (resolution (current-viewport)) res)
    ;;(setf (resolution (current-viewport)) (v! *dimensions*))
    ;;(update *shadow-camera* delta)
    (update *currentcamera* delta)
    (control *camera* delta)
    (when (and (funcall *stepper*)
               (or (key-down-p key.w)
                   (key-down-p key.a)
                   (key-down-p key.s)
                   (key-down-p key.d)))
      (play-sound :footsteps))
    ;;(setf (pos *camera1*) *light-pos*)
    ;;(update-all-the-things *actors*)
    (with-fbo-bound (*fbo*)
      (clear *fbo*)
      (loop :for actor :in *actors*
         :do
           (draw actor *currentcamera* time)
           (update actor delta)))
    (as-frame
      (with-setf* ((depth-mask) nil
                   (cull-face)  nil
                   (depth-test-function) #'always
                   ;;(clear-color) (v! 1 0 1 1)
                   )
        (map-g #'generic-2d-pipe *bs*
               :sam  *sam*
               ;;:sam2 *god-sam*
               )))
    (decay-events)))

(def-simple-main-loop play (:on-start #'init)
  (draw!))

