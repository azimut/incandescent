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

(defvar *light-dir* (q:to-direction
                     (q:point-at (v! 0 1 0) *light-pos* (v! 0 0 0))))

;; In theory perspective is for Spotlights...but I think it also needs a tweak on
;; the prober (shadow-factor) gpu function for pcf, but variance works fine
(defparameter *shadow-camera*
  (let* ((lpos (v3:*s (v! 10 20 10) 1f0))
         (ldir (q:point-at (v! 0 1 0) lpos (v! 0 3 0)))
         (cam  (make-instance 'pers
                              :name :shadow-camera
                              ;;:frame-size (v2! 10) ;; zoom
                              :far 80f0
                              :near 20f0
                              :rot ldir
                              :pos lpos)))
    (setf *light-pos* lpos)
    (setf *light-dir* (q:to-direction ldir))
    cam))

(defparameter *shadow-camera*
  (let* ((lpos (v! 5 10 5))
         (ldir (q:point-at (v! 0 1 0) lpos (v! .7 0 .2)))
         (cam  (make-instance 'orth
                              :name :shadow-camera
                              :frame-size (v2! 5) ;; zoom
                              :far 20f0
                              :near 10f0
                              :rot ldir
                              :pos lpos)))
    (setf *light-pos* lpos)
    (setf *light-dir* (q:to-direction ldir))
    cam))
