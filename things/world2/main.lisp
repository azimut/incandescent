(in-package #:incandescent)

(defvar *last-time* (get-internal-real-time))
(defvar *bs* nil)

(defparameter *cone-inner* 1f0)
(defparameter *cone-outer* 1.5f0)

(defparameter *cone-mult*  1f0)

#+nil
(defparameter *dimensions*
  (mapcar #'round (coerce (v2:/s (surface-resolution (current-surface))
                                 2f0)
                          'list)))

(defparameter *dimensions* '(1366 768))
(defparameter *dimensions* '(683 384))
(defparameter *dimensions* '(136 76))
(defparameter *dimensions* '(341 192))

(defvar *sdfbo* nil)
(defvar *sdsam* nil)

(defmethod init ()
  (setf *random-state* (make-random-state t))
  ;;--------------------------------------------------
  ;; Buffer stream for single stage pipelines
  (unless *bs* (setf *bs* (make-buffer-stream nil :primitive :points)))
  ;;--------------------------------------------------
  ;; HDR fbo(s)
  (when *fbo*   (free *fbo*))
  (when *sdfbo* (free *sdfbo*))
  (setf *sdfbo* (make-fbo `(0 :dimensions ,*dimensions* :element-type :rgb16f))
        *sdsam* (sample (attachment-tex *sdfbo* 0) :wrap :clamp-to-edge))
  (setf *fbo*  (make-fbo
                `(0 :dimensions ,*dimensions*  :element-type :rgba16f)
                `(1 :dimensions ,*dimensions*  :element-type :rgba32f)
                `(2 :dimensions ,*dimensions*  :element-type :rgba16f)
                `(3 :dimensions ,*dimensions*  :element-type :rg16f)
                `(:d :dimensions ,*dimensions*)))
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
  (init-shadowmap)
  (init-scene)
  (init-voxel)
  nil)

(defun draw! ()
  (let* ((res   (surface-resolution (current-surface)))
         (now   (get-internal-real-time))
         (time  (* 1f0 now))
         (delta (* (- now *last-time*) .001))
         (delta (if (> delta .16) .00001 delta)))
    (setf *last-time* now)
    (setf (resolution (current-viewport)) res)
    ;;(setf (viewport-dimensions (current-viewport)) *dimensions*)
    ;;--------------------------------------------------
    ;;(update  *currentcamera* delta)
    (update  *shadow-camera* delta)
    (control *currentcamera* delta 1)
    ;;--------------------------------------------------
    (draw-shadowmap)
    (with-fbo-bound (*fbo*)   ;; defer render
      (clear-fbo *fbo*)
      (dolist (actor *actors*)
        (draw actor *currentcamera* time)
        (update actor delta)))
    ;;--------------------------------------------------
    (with-fbo-bound (*sdfbo*) ;; defer shading
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
               ;; ;; IBL
               ;; :brdf-lut *s-brdf*
               ;; :prefilter-map *s-cubemap-prefilter*
               ;; :irradiance-map *s-cubemap-live*
               ;;:voxel-light  *voxel-light-zam*
               :cam-pos (pos *currentcamera*)
               :light-dir   *light-dir*
               :light-color (v3:*s *light-color* *cone-mult*)
               :light-pos   *light-pos*)))
    (as-frame
      (with-setf* ((depth-mask) nil
                   (cull-face)  nil
                   (depth-test-function) nil)
        (map-g #'generic-2d-pipe *bs*
               :sam *sdsam*
               :samd *samd*)
        ;;
        ;; (draw-tex-tl *sam3*)
        ;; (draw-tex-tr *sam*)
        ;; (draw-tex-br *sam1*)
        ;;(draw-tex-bl *sam2*)
        ;;
        ;;(draw-tex-tr *shadow-sam*)
        ;;(draw-tex-tl *sdsam*)
        )))
  ;; Stop on ESC
  (when (keyboard-button (keyboard) key.escape)
    (play-render :stop))
  (decay-events))

(def-simple-main-loop play-render (:on-start #'init)
  (draw!))

(defun playit ()
  ;; (cepl:initialize-cepl :gl-version nil)
  ;; (cepl.context::legacy-add-surface (cepl:cepl-context) "CEPL" 341 192 t t
  ;;                                   nil nil t nil)
  ;; (cepl:cls)
  (play-render :start))
