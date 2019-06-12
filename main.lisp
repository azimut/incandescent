(in-package #:incandescent)

(defvar *last-time* (get-internal-real-time))
(defvar *bs* nil)

;;(defparameter *dimensions* '(1366 768))
;;(defparameter *dimensions* '(800 600))
;;(defparameter *dimensions* '(533 400))
;;(defparameter *dimensions* '(341 192))
(defparameter *dimensions* '(683 384))

;;(defparameter *dimensions* '(613 726))
;;(defparameter *dimensions* '(307 360))

(defun init ()
  ;;--------------------------------------------------
  ;; Buffer stream for single stage pipelines
  (unless *bs* (setf *bs* (make-buffer-stream nil :primitive :points)))
  ;;--------------------------------------------------
  ;; HDR fbo(s)
  (when *fbo*  (free *fbo*))
  (setf *fbo*  (make-fbo `(0  :dimensions ,*dimensions* :element-type :rgba16f)
                         `(:d :dimensions ,*dimensions*)))
  (setf *sam*  (sample (attachment-tex *fbo* 0)  :wrap :clamp-to-edge))
  (setf *samd* (sample (attachment-tex *fbo* :d) :wrap :clamp-to-edge))
  ;;---------------------------------------------- ----
  (setf (clear-color) (v! 0 0 0 1))
  ;;--------------------------------------------------
  ;;(reset-camera)
  (free-actors)
  ;;--------------------------------------------------
  nil)

(defun draw! ()
  (let* ((res   (surface-resolution (current-surface)))
         (now   (get-internal-real-time))
         (time  (* .1 now))
         (delta (* (- now *last-time*) .001))
         (delta (if (> delta .16) .00001 delta)))
    (setf *last-time* now)
    (setf (resolution (current-viewport)) res)
    ;;(setf (resolution (current-viewport)) (v! *dimensions*))
    (update  *currentcamera* delta)
    (control *currentcamera* delta 10)
    ;;
    (with-fbo-bound (*fbo*)
      (clear *fbo*)
      (loop :for actor :in *actors*
            :do (update actor delta)
                (draw actor *currentcamera* time)))
    ;;
    (as-frame (with-setf* ((depth-mask) nil
                           (cull-face)  nil
                           (depth-test-function) #'always)
                (map-g #'generic-2d-pipe *bs*
                       :sam  *sam*
                       :samd *samd*)))
    (decay-events)))

(def-simple-main-loop play (:on-start #'init)
  (draw!))


