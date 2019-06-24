(in-package :incandescent)

;; Utils to load a rectangular environmentmap in HDR format
;;
;; "From Equirectangular to Cubemap"
;; https://learnopengl.com/PBR/IBL/Diffuse-irradiance

;; (reset-camera)
;; (make-hdr "asdasd.hdr")
;; (init-scene-cubemap)
;; (free-actors)
;; (make-env-map *cube-tex* *cube-sam*)
;; (free-hdr)

(defvar *t-hdr* nil)
(defvar *s-hdr* nil)

(defun free-hdr ()
  (when *t-hdr* (free *t-hdr*))
  (setf *t-hdr* nil
        *s-hdr* nil))

(defun init-hdr (file)
  (let ((filename (namestring (resolve-path file))))
    (free-hdr)
    (setf *t-hdr* (nineveh:load-hdr-2d filename))
    (setf *s-hdr* (sample *t-hdr* :wrap :clamp-to-edge
                                  :minify-filter :linear))))

(defclass hdr (actor) ())

(defun make-hdr ()
  (let ((obj (make-instance 'hdr)))
    (push obj *actors*)
    obj))

(defmethod update ((actor hdr) dt))

(defmethod draw ((actor hdr) camera (time single-float))
  (with-setf (cull-face) :front
    (with-slots (buf scale color) actor
      (map-g #'hdr-pipe buf
             :sam *s-hdr*
             :world-view  (world->view camera)
             :view-clip   (projection  camera)))))

(defun-g hdr-vert ((vert g-pnt)
                   &uniform (world-view :mat4) (view-clip :mat4))
  (let ((pos (pos vert)))
    (values (* view-clip world-view (v! pos 1))
            pos)))

(defun-g sample-spherical-map ((v :vec3))
  (let* ((uv (v! (atan (z v) (x v))
                 (asin (y v))))
         (uv (* uv (v! ".1591" ".3183")))
         (uv (+ uv .5)))
    uv))

(defun-g hdr-frag ((pos :vec3) &uniform (sam :sampler-2d))
  (let* ((uv (sample-spherical-map (normalize pos)))
         (color (s~ (texture sam uv)
                    :xyz)))
    (v! color 1)))

(defpipeline-g hdr-pipe ()
  :vertex   (hdr-vert g-pnt)
  :fragment (hdr-frag :vec3))
