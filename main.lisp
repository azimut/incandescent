(in-package #:incandescent)

(defvar *bs* nil)
(defvar *cloud-tex* nil)
;; (defparameter *dimensions* '(400 300))
;; (defparameter *dimensions* '(800 600))
;;(defparameter *dimensions* '(532 400))
(defparameter *dimensions* '(227 128))
;;(loop for i from '(1 2 3 4) collect i)
(defun init ()
  ;; (unless *cloud-tex*
  ;;   (setf *cloud-tex*
  ;;         (get-tex "static/Cloud04_8x8.tga")))
  ;;(init-particles 100)
  ;;(init-cubemap)
  ;;(init-cubemap-pbr)
  ;;--------------------------------------------------
  ;; Buffer stream for single stage pipelines
  (unless *bs*
    (setf *bs* (make-buffer-stream nil :primitive :points)))
  ;;--------------------------------------------------
  ;; HDR fbo(s)
  (when *fbo* (free *fbo*))
  (setf *fbo*
        (make-fbo
         (list 0 :element-type :rgb16f :dimensions *dimensions*)
         (list :d :dimensions *dimensions*)))
  (setf *sam*  (sample (attachment-tex *fbo* 0)  :wrap :clamp-to-edge))
  (setf *samd* (sample (attachment-tex *fbo* :d) :wrap :clamp-to-edge))
  ;;--------------------------------------------------
  (setf (clear-color) (v! 0 0 0 1))
  ;;--------------------------------------------------
  (setf *actors* nil)
  ;;(make-pbr (v! 0 -2 0))
  ;;(make-pbr)
  ;;(make-piso (v! 0 -2 0))
  ;;(make-thing)
  (make-box)
  ;;(make-cubemap)
  ;;(make-pbr-simple (v! 0 0 -10))
  NIL)

(defun draw! ()
  (let* ((res (surface-resolution (current-surface)))
         (time (mynow)))

    ;;(setf (resolution (current-viewport)) res)
    (setf (resolution (current-viewport)) (v! *dimensions*))
    (update *currentcamera*)
    ;;(setf (pos *camera1*) *light-pos*)
    (update-all-the-things *actors*)
    (with-fbo-bound (*fbo*)
      (clear *fbo*)
      (loop :for actor :in *actors*
         :collect (draw actor *currentcamera* time)))
    (as-frame
     (with-setf* ((depth-mask) nil
                  (cull-face) nil
                  (clear-color) (v! 0 0 0 1))
       (map-g #'generic-2d-pipe *bs*
              :sam *sam*)))))

(def-simple-main-loop play (:on-start #'init)
  (draw!))
