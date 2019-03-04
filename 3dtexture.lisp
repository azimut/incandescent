(in-package #:incandescent)

;;--------------------------------------------------
;; Marching cubes
;;
;; https://www.gamedev.net/forums/topic/599034-raymarching-a-cube-in-glsl-am-i-missing-something/

(defvar *32cube* NIL)
(defvar *32tex*  NIL)
(defvar *32sam*  NIL)

(defvar *quad-3d* NIL "BUFFER-STREAM that has the geometry for the quad")
(defvar *frustum* NIL "C-ARRAY that has the frustum corners???")
(defvar *ray-fbo* NIL)
(defvar *ray-sam* NIL)

(defun-g get-density ((wpos :vec3)
                      (tex3 :sampler-3d)
                      (time :float))
  (let* (;; x: scale, y: intensity, z: intensity offset
         (noise-data (v! .015 1 .3))
         (noise-velocity (* (x noise-data)
                            (v! 3 3)))
         (density 1f0)
         (uv
          (fract (+ (* wpos (x noise-data))
                    (v! (* (x noise-velocity) time)
                        0
                        (* (y noise-velocity) time)))))
         (noise (x (texture tex3 uv)))
         (noise (* (saturate (- noise (z noise-data)))
                   (y noise-data)))
         (density (saturate noise)))
    density))

(defun load-into-3d (dst r g b)
  (declare (type cepl:c-array dst r g b))
  (dotimes (i 32)
    (dotimes (j 32)
      (dotimes (k 32)
        (setf (aref-c dst (* i j k))
              (v! (x (aref-c r i))
                  (x (aref-c b j))
                  (x (aref-c g k))))))))

(defun init-cube ()
  (when *32cube* (free *32cube*))
  (when *32tex*  (free *32tex*))
  (setf *32cube*
        (make-c-array NIL
                      :dimensions '(32 32 32)
                      :element-type :vec3))
  (load-into-3d
   *32cube*
   (get-c-tex "static/32x32-8-cellular-i7.bmp")
   (get-c-tex "static/32x32-8-cellular-i14.bmp")
   (get-c-tex "static/32x32-8-cellular-i21.bmp"))
  (setf *32tex* (make-texture *32cube*))
  (setf *32sam* (sample *32tex*)))
