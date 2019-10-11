(in-package #:incandescent)

;;;--------------------------------------------------

(defvar *offset* (v! 0 0 0))
(defvar *last-time* (get-internal-real-time))
(defvar *bs* nil)

(defparameter *cone-inner* 1f0)
(defparameter *cone-outer* 1.5f0)
(defparameter *cone-mult*  1f0)

(defvar *unstarted* nil)

(defparameter *dimensions* '(1366 768))
;;(defparameter *dimensions* '(800 600))
;;(defparameter *dimensions* '(533 400))
(defparameter *dimensions* '(171 81))
(defparameter *dimensions* '(341 192))
(defparameter *dimensions* '(683 384))
(defparameter *dimensions* '(320 240))

;;(defparameter *dimensions* '(200 250))
;;(defparameter *dimensions* '(613 726))
;;(defparameter *dimensions* '(307 360))

(defparameter *dimensions* (list (* 16 50) (* 16 40)))
(defparameter *dimensions* (list (* 16 20) (* 16 15)))
(defparameter *dimensions* (list (* 16 40) (* 16 30)))


;;(defparameter *dimensions* (list (* 16 20) (* 16 15)))
(defparameter *dimensions* '(1366 768))
(defparameter *dimensions* '(683 384))
(defparameter *dimensions* '(341 192))

(defvar *light-volumes* (list))
(defvar *dstex* nil)

(defvar *dsfbo* nil)
(defvar *dssam* nil)

(defvar *sdfbo* nil)
(defvar *sdsam* nil)
(defvar *sd2sam* nil)

;; 5 3 2 1 - with 0
(defmethod init ()
  (setf *random-state* (make-random-state t))
  ;;--------------------------------------------------
  ;; Buffer stream for single stage pipelines
  (unless *bs* (setf *bs* (make-buffer-stream nil :primitive :points)))
  ;;--------------------------------------------------
  ;; HDR fbo(s)
  (when *fbo*   (free *fbo*))
  (when *sdfbo* (free *sdfbo*))
  (when *dsfbo* (free *dsfbo*))
  ;;#+nil
  (setf *sdfbo* (make-fbo `(0 :dimensions ,*dimensions* :element-type :rgb16f))
        *sdsam* (sample (attachment-tex *sdfbo* 0) :wrap :clamp-to-edge))
  (setf *fbo*  (make-fbo
                `(0 :dimensions ,*dimensions*  :element-type :rgba16f)
                `(1 :dimensions ,*dimensions*  :element-type :rgba16f)
                `(2 :dimensions ,*dimensions*  :element-type :rgba16f)
                `(3 :dimensions ,*dimensions*  :element-type :rg16f)
                ;;`(:d ,*dstex*)
                ;;`(:s ,*dstex*)
                `(:d :dimensions ,*dimensions*)
                ))
  (setf *sam*  (sample (attachment-tex *fbo*  0) :wrap :clamp-to-edge))
  (setf *sam1* (sample (attachment-tex *fbo*  1) :wrap :clamp-to-edge))
  (setf *sam2* (sample (attachment-tex *fbo*  2) :wrap :clamp-to-edge))
  (setf *sam3* (sample (attachment-tex *fbo*  3) :wrap :clamp-to-edge))
  (setf *samd* (sample (attachment-tex *fbo* :d) :wrap :clamp-to-edge))
  ;;---------------------------------------------- ----
  (setf (clear-color) (v! 0 0 0 1))
  (gl:clear-stencil 0)
  ;;--------------------------------------------------
  (free-actors)
  (free-scenes)
  ;;--------------------------------------------------
  (init-scene)
  (init-shadowmap)
  nil)

(let ((stepper (make-stepper (seconds 1) (seconds 1))))
  (defmethod draw! ()
    (let* ((res   (surface-resolution (current-surface)))
           (now   (get-internal-real-time))
           (time  (* 1f0 now))
           (delta (* (- now *last-time*) .001))
           (delta (if (> delta .16) .00001 delta)))
      (setf *last-time* now)
      (setf (resolution (current-viewport)) res)
      ;;(setf (viewport-dimensions (current-viewport)) *dimensions*)
      ;;--------------------------------------------------
      (update  *currentcamera* delta)
      (control *currentcamera* delta 4)
      ;;--------------------------------------------------
      (draw-shadowmap)
      (with-fbo-bound (*fbo*)
        (clear-fbo *fbo*)
        (dolist (actor *actors*)
          (draw actor *currentcamera* time)
          (update actor delta)))
      ;;#+nil
      (with-fbo-bound (*sdfbo*) ;; defer shading
        (clear-fbo *sdfbo*)
        (with-setf* ((depth-mask) nil
                     (cull-face) nil
                     (depth-test-function) nil)
          (map-g #'postprocess-defer-pipe *bs*
                 :albedo-sam   *sam*
                 :position-sam *sam1*
                 :normal-sam   *sam2*
                 :metallic-sam *sam3*
                 :shadowmap *shadow-sam*
                 :light-vp (world->clip *shadow-camera*)
                 ;;:voxel-light  *voxel-light-zam*
                 :cam-pos (pos *currentcamera*)
                 :light-dir   *light-dir*
                 :light-color (v3:*s *light-color* *cone-mult*)
                 :light-pos   *light-pos*)))
      ;;#+nil
      (as-frame
        (with-setf* ((depth-mask) nil
                     (cull-face)  nil
                     (depth-test-function) nil)
          (map-g #'generic-2d-pipe *bs*
                 ;;:sam  *sam*
                 ;;:sam2 *god-sam*
                 ;;:sam2 *sam-ssao*
                 :sam *sdsam*
                 :samd *samd*)
          ;;(draw-tex-bl *sam-ssao*)
          ;;(draw-tex-tl *sssr*)
          ;;(draw-tex-tl *god-sam*)
          ;;
          ;; (draw-tex-tl *sam3*)
          ;;(draw-tex-tr *sam*)
          ;; (draw-tex-br *sam1*)
          ;; (draw-tex-bl *sam2*)
          ;;
          ;;(draw-tex-tr *shadow-sam*)
          ;;(draw-tex-tr *dssam*)
          ;;(draw-tex-tl *sdsam*)
          ;;(draw-tex-bl *god-sam*)
          )
        ))
    (ode-update)
    ;; Stop on ESC
    (when (keyboard-button (keyboard) key.escape)
      (play :stop))
    (decay-events)))

(def-simple-main-loop play (:on-start #'init)
  (draw!))

(defun playit ()
  ;; (cepl:initialize-cepl :gl-version nil)
  ;; (cepl.context::legacy-add-surface (cepl:cepl-context) "CEPL" 341 192 t t
  ;;                                   nil nil t nil)
  ;; (cepl:cls)
  (play :start))
