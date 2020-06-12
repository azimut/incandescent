(in-package #:incandescent)

(defun init ()
  ;; Init Lisp
  (setf *random-state* (make-random-state t))
  (setf *last-time*    (get-internal-real-time))
  (set-dimensions-to-winsize)
  (free-actors)
  (free-scenes)
  ;;--------------------------------------------------
  ;; Buffer stream for single stage pipelines
  (unless *bs* (setf *bs* (make-buffer-stream nil :primitive :points)))
  ;;--------------------------------------------------
  ;; Create HDR fbo(s) and samplers
  (reload-base-fbos)
  ;;--------------------------------------------------
  (setf (clear-color) (v! 0 0 0 1))
  ;;(gl:clear-stencil 0)
  ;;--------------------------------------------------
  #+slynk
  (slynk-mrepl::send-prompt (find (bt:current-thread) (slynk::channels)
                                  :key #'slynk::channel-thread))
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
    ;;(update  *currentcamera* delta)
    (control *currentcamera* delta 3)
    ;;--------------------------------------------------
    ;;(draw-shadowmap)
    (with-fbo-bound (*fbo*)
      (clear-fbo *fbo*)
      (dolist (actor *actors*)
        (draw actor *currentcamera* time)
        (update actor delta)))
    (as-frame
      (with-setf* ((depth-mask) nil
                   (cull-face)  nil
                   (depth-test-function) nil)
        (map-g #'generic-2d-pipe *bs*
               :sam *sam*
               :samd *samd*))))
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
