(in-package #:incandescent)

(defvar *bs* nil)
(defvar *cloud-tex* nil)
;;(defparameter *dimensions* '(400 300))
;;(defparameter *dimensions* '(800 600))
(defparameter *dimensions* '(532 400))
;;(defparameter *dimensions* '(227 128))

(defvar *shadow-fbo* NIL)
(defvar *shadow-sam* NIl)

(defvar *bones* NIL)
(defvar *ubo* NIL)

(defvar *huesos* NIL)
(defvar *chuesos* NIL)
(defvar *mann* NIL)
(defun init ()
  ;; (unless *cloud-tex*
  ;;   (setf *cloud-tex*
  ;;         (get-tex "static/Cloud04_8x8.tga")))
  (unless *huesos*
    (assimp-load-meshes "/home/sendai/quicklisp/local-projects/incandescent/static/guard/boblampclean.md5mesh")
    (setf *mann* (ai:import-into-lisp "/home/sendai/quicklisp/local-projects/incandescent/static/guard/boblampclean.md5mesh"))
    (setf *huesos*  (make-gpu-array NIL :element-type :mat4 :dimensions 32))
    (push-g (coerce (get-bones-tranforms *mann*) 'list) *huesos*)
    (setf *chuesos* (pull1-g *huesos*)))
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
         (list :d :dimensions *dimensions*)))
  (setf *sam*  (sample (attachment-tex *fbo* 0)  :wrap :clamp-to-edge))
  (setf *samd* (sample (attachment-tex *fbo* :d) :wrap :clamp-to-edge))
  ;;--------------------------------------------------
  (setf (clear-color) (v! 1 1 1 1))
  ;;--------------------------------------------------
  (setf *actors* nil)
  ;;(make-box (v! 0 2 -6))
  (make-piso (v! 0 -3 0)))
NIL

(defun draw! ()
  (let* ((res (surface-resolution (current-surface)))
         (time (mynow)))

    (setf (resolution (current-viewport)) res)
    ;;(setf (resolution (current-viewport)) (v! *dimensions*))
    (update *currentcamera*)
    ;;(setf (pos *camera1*) *light-pos*)
    (update-all-the-things *actors*)

    ;; Shadow map
    (with-fbo-bound (*shadow-fbo* :attachment-for-size :d)
      (clear *shadow-fbo*)
      (loop :for actor :in *actors*
         :do (with-slots (buf scale) actor
               (map-g #'simplest-3d-pipe buf
                      :scale scale
                      :model-world (model->world actor)
                      :world-view  (world->view *shadow-camera*)
                      :view-clip   (projection *shadow-camera*)))))

    (with-fbo-bound (*fbo*)
      (clear *fbo*)
      (loop :for actor :in *actors*
         :do (draw actor *currentcamera* time)))

    ;; (let ((bones (get-bones-tranforms)))
    ;;   (with-gpu-array-as-c-array (m (ubo-data *ubo*)
    ;;                                 :access-type :write-only)
    ;;     (loop :for i :from 0 :upto 25 :do
    ;;          (loop :for j :from 0 :upto 15 :do
    ;;               (setf (cffi:mem-aref (c-array-pointer m) :float (+ i j))
    ;;                     (coerce (aref (aref bones i) j) 'single-float))))))
    (as-frame
      (with-setf* ((depth-mask) nil
                   (cull-face) nil
                   (clear-color) (v! 1 1 1 1))
        (map-g #'generic-2d-pipe *bs*
               :sam *sam*)))))

(def-simple-main-loop play (:on-start #'init)
  (draw!))
