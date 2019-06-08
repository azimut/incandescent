(in-package :incandescent)

(defvar *variance-fbo* NIL)
(defvar *variance-sam* NIL)
(defparameter *variance-dimensions* '(1024 1024))
;;(defparameter *variance-dimensions* '(2048 2048))

(defparameter *variance-camera*
  (let* ((lpos (v! 10 10 10))
         (cam  (make-instance 'orth
                              :name :shadow-camera
                              :frame-size (v2! 30) ;; zoom
                              :far 20f0
                              :near .1f0
                              :rot (q:point-at (v! 0 1 0) lpos
                                               (v! 0 3 0))
                              :pos lpos)))
    (setf *light-pos* lpos)
    cam))

(defun init-variance ()
  (when *variance-fbo*
    (free *variance-fbo*))
  (setf *variance-fbo* (make-fbo `(0  :dimensions ,*variance-dimensions*)
                                 `(:d :dimensions ,*variance-dimensions*)))
  (setf *variance-sam* (sample (attachment-tex *variance-fbo* 0)
                               :wrap           :clamp-to-border
                               :minify-filter  :nearest
                               :magnify-filter :nearest))
  (setf (cepl.samplers::border-color *variance-sam*) (v! 1 1 1 1))
  t)

(defun free-variance ()
  (when *variance-fbo* (free *variance-fbo*)))

(defun-g variance-3d-frag ((uv :vec2) (frag-norm :vec3) (frag-pos :vec3))
  (let ((depth (z frag-pos)))
    (v! depth
        (* depth depth)
        1 0)))

(defpipeline-g variance-3d-pipe ()
  :vertex   (vert g-pnt)
  :fragment (variance-3d-frag :vec2 :vec3 :vec3))

(defun draw-variance ()
  "draws the scene in *ACTORS* from the point of view of *variance-CAMERA* into *variance-FBO* using a simple shader pipe"
  (with-fbo-bound (*variance-fbo*)
    (clear *variance-fbo*)
    ;;(with-setf (cull-face) :front)
    (loop :for actor :in *actors*
          :do (with-slots (buf scale) actor
                (map-g #'variance-3d-pipe buf
                       :scale scale
                       :model-world (model->world actor)
                       :world-view  (world->view *variance-camera*)
                       :view-clip   (projection  *variance-camera*))))))
