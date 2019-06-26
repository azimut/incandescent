(in-package #:incandescent)

;; Shadowmaps require additional information to go in/out from vertex shaders so a lot of duplicate code is needed. (unless trial)
;;
;; To debug try disabling culling and bias to see the effective shadowmap mask projected.
;;
;; UNIFORMS
;; - :shadowmap   *shadow-sam*
;; - :light-world (world->view *shadow-camera*)
;; - :light-clip  (projection  *shadow-camera*)
;;
;; Reference:
;; - https://learnopengl.com/Advanced-Lighting/Shadows/Shadow-Mapping
;; -

(defvar *shadow-fbo* NIL)
(defvar *shadow-sam* NIL)

(defparameter *shadow-dimensions* '(2048 2048))
(defparameter *shadow-dimensions* '(1024 1024))

(defvar *shadow-fbo* NIL)
(defvar *shadow-sam* NIL)

(defparameter *shadow-camera*
  (let* ((lpos (v! 50 50 50))
         (cam  (make-instance 'orth
                              :name :shadow-camera
                              :frame-size (v2! 30) ;; zoom
                              :far 120f0
                              :near 70f0
                              :rot (q:point-at (v! 0 1 0) lpos
                                               (v! 0 0 0))
                              :pos lpos)))
    (setf *light-pos* lpos)
    cam))
