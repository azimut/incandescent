(in-package #:incandescent)

(defvar *last-time* (get-internal-real-time))
(defvar *bs* nil)

(defvar *dimensions-v2* NIL)

(defparameter *dimensions* '(1366 768))
(defparameter *dimensions* '(533 400))
(defparameter *dimensions* '(683 384))
(defvar *p3* nil)

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
  (init-audio)
  (test-sound)
  (init-text)
  ;; Wake up camera shot
  (setf (shots *camera*)
        (vect
         (vect
          (list (v! 0.0 2.0 40.0) (q! 0.8526403 -0.52249855 0.0 0.0) 0)
          (list (v! 0.0 2.0 40.0) (q! 0.99002373 -0.14090128 0.0 0.0) 0.3)
          (list (v! 0.0 10.2672105 38.46973) (q! 0.9408808 -0.33873796 0.0 0.0) 0.6)
          (list (v! 0.0 10.2672105 38.46973) (q! 0.99950665 -0.03141089 0.0 0.0) 0.8)
          (list (v! 0.0 10.2672105 38.46973) (q! 0.8758744 -0.027525585 0.48151588 -0.015132314) 1.2)
          (list (v! 0.0 10.2672105 38.46973) (q! 0.99938333 -0.03140703 0.015699469 -4.9337506e-4) 1.5)
          (list (v! 0.0 10.2672105 38.46973) (q! 0.89758444 -0.02820786 -0.43972206 0.013818893) 1.8)
          (list (v! 0.0 10.2672105 38.46973) (q! 0.93816864 -0.3458059 -0.013729579 -0.008325609) 2))))
  ;; Animate shot
  (shot-on)
  ;;--------------------------------------------------
  (setf *actors* nil)
  ;;(free-assimp-buffers)
  ;;(make-text "It is cold outside")
  (if *p3*
      (push *p3* *actors*)
      (setf *p3* (make-billboards (get-tex "static/Cloud04_8x8.tga") 100)))
  (dotimes (i 30)
    (make-grass :pos (v! (serapeum:random-in-range -60f0 60f0)
                         0
                         (serapeum:random-in-range -60f0 60f0))
                :scale (serapeum:random-in-range 10f0 15f0)))
  ;; make-piso: after grass and fog!
  (make-dirt :buf (lattice 300 700 2 2 t)
             :uv-repeat (v2:*s (v! 3 7) 4f0))
  (dotimes (i 10)
    (let ((*processing-flags*
           '(:ai-process-triangulate
             :ai-process-flip-u-vs
             :ai-process-preset-target-realtime-quality
             :ai-process-calc-tangent-space)))
      (mapcar (lambda (mesh) (push mesh *actors*))
              (assimp-load-meshes
               "static/tree/tree_obj.obj"
               :scale (serapeum:random-in-range .1 .25)
               :pos (v! (serapeum:random-in-range -100f0 100f0)
                        0
                        (serapeum:random-in-range -300f0 -40f0))
               :rot (q:from-axis-angle (v! 0 1 0)
                                       (radians (random 360)))))))
  ;; hazmat "/home/sendai/Downloads/scpcb-master/GFX/items/hazmat.b3d"
  (let ((*processing-flags*
         (cons :ai-process-preset-target-realtime-max-quality
               *processing-flags*)))
    (mapcar (lambda (mesh) (push mesh *actors*))
            (cdr
             (assimp-load-meshes
              "static/173box/173box.b3d"
              :pos (v! 0 -5 30)
              :rot (q:from-axis-angle (v! 0 1 0) (radians 270))
              :scale 1f0))))
  (setf *exposure* 0f0)
  (make-radio)
  (make-book (v! 0 0 -250))
  (reset-camera *camera* (v! 0 2 40))
  ;;
  (make-sound
   :footsteps .2
   "static/421131__giocosound__footstep-grass-1.mp3"
   "static/421130__giocosound__footstep-grass-2.mp3"
   "static/421129__giocosound__footstep-grass-3.mp3"
   "static/421128__giocosound__footstep-grass-4.mp3")
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
    (update *currentcamera* delta)
    (control *camera* delta)
    ;;
    (with-fbo-bound (*fbo*)
      (clear *fbo*)
      (loop :for actor :in *actors* :do
           (update actor delta)
           (draw actor *currentcamera* time)))
    ;;
    (as-frame
      (with-setf* ((depth-mask) nil
                   (cull-face)  nil
                   (depth-test-function) #'always)
        (map-g #'generic-2d-pipe *bs*
               :sam *sam*
               :samd *samd*)))
    (update-audio)
    (decay-events)))

(def-simple-main-loop play (:on-start #'init)
  (draw!))

