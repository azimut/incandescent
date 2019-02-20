(in-package #:incandescent)

(defvar *last-time* (get-internal-real-time))

(defvar *bs* nil)
(defvar *cloud-tex* nil)
(defparameter *stepper* (make-stepper (seconds 1) (seconds 1)))

(defparameter *dimensions* '(532 400))

(defvar *bones* NIL)
(defvar *ubo* NIL)

(defvar *chuesos* NIL)
(defvar *mann* NIL)

(defvar *cube-tex* NIL)
(defvar *cube-sam* NIL)
(defun init ()
  ;; (unless *cloud-tex*
  ;;   (setf *cloud-tex*
  ;;         (get-tex "static/Cloud04_8x8.tga")))
  ;; (when *chuesos*
  ;;   (free *chuesos*)
  ;;   (setf *chuesos* NIL)
  (unless *cube-tex*
    (setf *cube-tex* (make-render-cubemap *camera-cubemap*))
    (setf *cube-sam* (sample *cube-tex*
                             :wrap :clamp-to-edge
                             :magnify-filter :linear)))
  (unless *chuesos*
    (setf *actors* NIL)
    (let ((obj
           "/home/sendai/quicklisp/local-projects/incandescent/static/EOT_PC_VEHICLE_F35/EOT_PC_VEHICLE_F35.obj")
          (*default-normal*
           "static/EOT_PC_VEHICLE_F35/EOT_PC_VEHICLE_F35_Body_N.png")
          (*default-specular*
           "static/EOT_PC_VEHICLE_F35/EOT_PC_VEHICLE_F35_Body_S.png"))
      (assimp-load-meshes obj)
      (setf *mann* (ai:import-into-lisp obj))))

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
         (list 1 :element-type :rgb16f :dimensions *dimensions*)
         (list :d :dimensions *dimensions*)))
  (setf *sam1* (sample (attachment-tex *fbo* 1)  :wrap :clamp-to-edge))
  (setf *sam*  (sample (attachment-tex *fbo* 0)  :wrap :clamp-to-edge))
  (setf *samd* (sample (attachment-tex *fbo* :d) :wrap :clamp-to-edge))
  ;;--------------------------------------------------
  (setf (clear-color) (v! .2 .2 .2 1))
  ;;--------------------------------------------------
  (setf *actors* nil)
  ;;(make-celestial-sphere)
  ;;(make-env-map *cube-tex* *s-cubemap-live*)
  ;;(push (car *assimp-meshes*) *actors*)
  ;;(make-box)
  NIL)

(defun draw! ()
  (let* ((res   (surface-resolution (current-surface)))
         (now   (get-internal-real-time))
         (time  (* .1 now))
         ;; (delta (* (- now *last-time*) .001))
         ;; (delta (if (> delta .16) .00001 delta))
         )
    (setf *last-time* now)
    ;;(setf (resolution (current-viewport)) res)
    (setf (resolution (current-viewport)) (v! *dimensions*))
    (update *currentcamera*)
    ;;(setf (pos *camera1*) *light-pos*)
    ;;(update-all-the-things *actors*)
    ;; (with-fbo-bound (*fbo*)
    ;;   (clear *fbo*)
    ;;   (loop :for actor :in *actors*
    ;;      :do
    ;;        (draw actor *currentcamera* time)
    ;;        (update actor)))
    (draw-raymarching time)
    (as-frame
      (with-setf* ((depth-mask) nil
                   (cull-face)  nil
                   (clear-color) (v! 1 0 1 1))
        (map-g #'generic-2d-pipe *bs*
               :sam *ray-sam*)))))

(def-simple-main-loop play (:on-start #'init)
  (draw!))

