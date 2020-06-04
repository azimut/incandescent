(in-package #:incandescent)

;; Reference:
;; - https://learnopengl.com/Advanced-Lighting/Shadows/Point-Shadows
;; - https://github.com/cbaggers/cepl/issues/320

(defvar *t-shadow-cube* nil)
(defvar *s-shadow-cube* nil)

(defun free-pointlight-pcf ()
  (when *t-shadow-cube* (free *t-shadow-cube*))
  (when *shadow-fbo*    (free *shadow-fbo*)))

(defun init-pointlight-pcf ()
  (free-pointlight-pcf)
  ;;
  (setf *shadow-fbo*    (make-fbo   `(nil :dimensions ,*shadow-dimensions*)))
  ;;
  (setf *t-shadow-cube* (make-texture nil :dimensions *shadow-dimensions*
                                          :element-type :DEPTH-COMPONENT24
                                          :cubes t))
  (setf *s-shadow-cube* (sample *t-shadow-cube* :minify-filter  :nearest
                                                :magnify-filter :nearest
                                                :wrap :clamp-to-edge))
  #+nil
  (with-fbo-bound (*shadow-fbo* :attachment-for-size :d)
    (clear-fbo *shadow-fbo*)
    (%gl:framebuffer-texture :read-framebuffer
                             (cepl.fbos::gl-enum :depth-attachment)
                             (texture-id *t-shadow-cube*)
                             0))
  #+nil
  (setf (attachment-tex *shadow-fbo* :d)
        *t-shadow-cube*)
  t)

#+nil
(progn
  (with-fbo-bound)
  (%gl:framebuffer-texture :read-framebuffer
                           (gl-enum :depth-attachment)
                           (texture-id *t-shadow-cube*)
                           0)
  (cepl.fbos::%update-fbo-state))

(defun draw-pointlight-pcf ()
  (with-fbo-bound (*shadow-fbo* :attachment-for-size :d)
    (clear *shadow-fbo*)
    ;;(with-setf (cull-face) :front)
    (loop :for actor :in *actors*
          :do (with-slots (buf scale) actor
                (map-g #'simplest-3d-pipe buf
                       :scale scale
                       :model-world (model->world actor)
                       :world-view  (world->view *shadow-camera*)
                       :view-clip   (projection  *shadow-camera*))))))
