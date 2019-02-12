(in-package #:incandescent)

(defvar *last-time* (get-internal-real-time))

(defvar *bs* nil)
(defvar *cloud-tex* nil)
(defparameter *stepper*
  (make-stepper (seconds .1) (seconds .1)))
;;(defparameter *dimensions* '(400 300))
;;(defparameter *dimensions* '(800 600))
(defparameter *dimensions* '(532 400))
;;(defparameter *dimensions* '(227 128))

(defvar *shadow-fbo* NIL)
(defvar *shadow-sam* NIl)

(defvar *bones* NIL)
(defvar *ubo* NIL)

(defvar *chuesos* NIL)
(defvar *mann* NIL)

(defvar *fbo-ssbo* NIL)
(defvar *sam-ssbo* NIL)

(defun init ()
  ;; (unless *cloud-tex*
  ;;   (setf *cloud-tex*
  ;;         (get-tex "static/Cloud04_8x8.tga")))
  (unless *huesos*
    (assimp-load-meshes "/home/sendai/quicklisp/local-projects/incandescent/static/guard/boblampclean.md5mesh")
    (setf *mann* (ai:import-into-lisp "/home/sendai/quicklisp/local-projects/incandescent/static/guard/boblampclean.md5mesh"))
    (setf *chuesos* (make-c-array NIL :element-type :mat4 :dimensions 32))
    (push-g (get-bones-tranforms *mann*) *chuesos*))
  ;; (unless *bones*
  ;;   (setf *bones* (make-gpu-array NIL :dimensions 1 :element-type 'bone-transforms))
  ;;   (setf *ubo*   (make-ubo *bones*))
  ;;   ;; (let ((bones (get-bones-tranforms)))
  ;;   ;;   (with-gpu-array-as-c-array (m (ubo-data *ubo*)
  ;;   ;;                                 :access-type :write-only)
  ;;   ;;     (loop :for i :from 0 :upto 25 :do
  ;;   ;;          (loop :for j :from 0 :upto 15 :do
  ;;   ;;               (setf (cffi:mem-aref (c-array-pointer m) :float (+ i j))
  ;;   ;;                     (aref (aref bones i) j))))))
  ;;   )

  (unless *shadow-fbo*
    (setf *shadow-fbo* (make-fbo (list :d :dimensions '(1024 1024))))
    (setf *shadow-sam* (sample (attachment-tex *shadow-fbo* :d)
                               :minify-filter :nearest
                               :magnify-filter :nearest)))
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
  (when *fbo-ssbo* (free *fbo-ssbo*))
  (setf *fbo-ssbo* (make-fbo (list 0 :dimensions *dimensions*)))
  (setf *sam-ssbo* (sample (attachment-tex *fbo-ssbo* 0) :wrap :clamp-to-edge))
  ;;--------------------------------------------------
  (setf (clear-color) (v! 1 1 1 1))
  ;;--------------------------------------------------
  (setf *actors* nil)
  ;;(make-box (v! 0 2 -6))
  (make-piso (v! 0 -3 0))
  NIL)

(defun release ()
  (setf *actors* NIL)
  (free *chuesos*)
  (push-g (coerce (get-bones-tranforms *mann*) 'list) *huesos*)
  (setf *chuesos* (pull1-g *huesos*))
  (push (nth 5 *assimp-meshes*) *actors*)
  (push (nth 4 *assimp-meshes*) *actors*)
  (push (nth 3 *assimp-meshes*) *actors*))
(defvar *add* 0f0)
(defun draw! ()
  (let* ((res   (surface-resolution (current-surface)))
         (now   (get-internal-real-time))
         (time  (* .1 now))
         (delta (* (- now *last-time*) .001))
         (delta (if (> delta .16) .00001 delta)))
    (setf *last-time* now)
    (setf (resolution (current-viewport)) res)
    ;;(setf (resolution (current-viewport)) (v! *dimensions*))
    (update *currentcamera*)
    ;;(setf (pos *camera1*) *light-pos*)
    (update-all-the-things *actors*)

    ;; Shadow map
    ;; (with-fbo-bound (*shadow-fbo* :attachment-for-size :d)
    ;;   (clear *shadow-fbo*)
    ;;   (loop :for actor :in *actors*
    ;;      :do (with-slots (buf scale) actor
    ;;            (case (class-name-of actor)
    ;;              (assimp-thing-with-bones
    ;;               (map-g #'simplest-3d-pipe-bones buf
    ;;                      :scale 1f0
    ;;                      :offsets *chuesos*
    ;;                      :model-world (model->world actor)
    ;;                      :world-view  (world->view *shadow-camera*)
    ;;                      :view-clip   (projection  *shadow-camera*)))
    ;;              (piso
    ;;               (map-g #'simplest-3d-pipe buf
    ;;                      :scale 1f0
    ;;                      :model-world (model->world actor)
    ;;                      :world-view  (world->view *shadow-camera*)
    ;;                      :view-clip   (projection  *shadow-camera*)))))))

    (when (funcall *stepper*)
      (push-g (get-bones-tranforms *mann*
                                   :time (mod (incf *add* .1) 5f0))
              *chuesos*))

    (with-fbo-bound (*fbo*)
      (clear *fbo*)
      (loop :for actor :in *actors*
         :do (draw actor *currentcamera* time)))
    ;; (with-fbo-bound (*fbo-ssbo*)
    ;;   (clear *fbo-ssbo*)
    ;;   (draw-ssao :radius 1000f0
    ;;              :kernel-effect 3f0
    ;;              :n-kernels 10))
    (as-frame
      (with-setf* ((depth-mask) nil
                   (cull-face) nil
                   (clear-color) (v! 1 0 1 1))
        (map-g #'generic-2d-pipe *bs*
               :sam *sam*
               :sam2 *sam-ssbo*)))))

(def-simple-main-loop play (:on-start #'init)
  (draw!))

