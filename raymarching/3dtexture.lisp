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
(defvar *vol-fbo* NIL)
(defvar *vol-sam* NIL)

(defparameter *light-pos* (v! 100 100 100))
(defparameter *light-pos* (v! 100 1000 0))
(defparameter *light-pos* (v! 0 .3 -1))
(defparameter *light-pos* (v! 0 20 100))

(defclass ray-plane (actor) ())

(defun free-volumetric ()
  (when *vol-fbo* (free *vol-fbo*))
  (when *frustum* (free *frustum*))
  (when *quad-3d* (free *quad-3d*)))

(defun init-volumetric ()
  (free-volumetric)
  (setf (make-texture nil :dimensions '(32 32 32) :element-type :vec4))
  (setf *vol-fbo* (make-fbo `(0 :dimensions ,*dimensions* :element-type :rgba16f))
        *vol-sam* (sample (attachment-tex *ray-fbo* 0)
                          :wrap :clamp-to-edge))
  (setf *frustum*
        (make-c-array (get-frustum-corners *currentcamera*)
                      :element-type :vec3 :dimensions 4))
  (setf *quad-3d*
        (let* ((verts (make-gpu-array
                       `((,(v! -1 -1 3) ,(v! 0 0))
                         (,(v!  1  1 1) ,(v! 1 1))
                         (,(v!  1 -1 2) ,(v! 1 0))
                         (,(v! -1  1 0) ,(v! 0 1)))
                       :dimensions 4 :element-type 'g-pt))
               (indi  (make-gpu-array '(0 1 2 1 0 3)
                                      :dimensions 6
                                      :element-type :unsigned-short))
               (buf   (make-buffer-stream verts
                                          :index-array indi
                                          :primitive :triangle-strip)))
          buf))
  )

(defun draw-volumetric (sam samd time)
  (declare (type single-float time)
           (type cepl:sampler sam samd))
  (with-setf* ((depth-test-function) #'always
               (cull-face) NIL
               (depth-mask) NIL)
    (with-fbo-bound (*ray-fbo*)
      (clear *ray-fbo*)
      (map-g #'raymarch-pipe *quad-3d*
             :samd samd
             :sam  sam
             :time time
             :cam-pos (pos *currentcamera*)
             :frustum-corners *frustum*
             :light-pos *light-pos*
             :world-view
             (m4:*
              ;; (rtg-math.projection:orthographic-v2
              ;;  (v2! 1)
              ;;  .1f0
              ;;  10f0)
              (q:to-mat4 (q:inverse (rot *currentcamera*)))
              (q:to-mat4 (rot *currentcamera*)))
             :view-world (m4:inverse
                          (world->view *tmp2*))))))

;;--------------------------------------------------

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

;;--------------------------------------------------

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
  (assert (dimensions= r g b))
  (dotimes (x 32)
    (dotimes (y 32)
      (dotimes (z 32)
        (setf (aref-c dst x y z)
              ;; Source Textures have only 1 color
              (v! (x (aref-c r (mod x 32)))
                  (x (aref-c g (mod y 32)))
                  (x (aref-c b (mod z 32)))))))))
