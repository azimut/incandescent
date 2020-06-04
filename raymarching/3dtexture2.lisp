(in-package :incandescent)

;; Volume rendering - Texture based

;; https://www.ronja-tutorials.com/2018/10/13/baking_shaders.html
;;
;; Code below translates the tutorial to create a 3d texture from
;; layers of noise.

;; Usage:
;; (init-cube)
;; (load-into-3d)
;; (make-cubetexture)

(defvar *blend-cube* (make-blending-params))

(defvar *2dfbo* nil)
(defvar *2dsam* nil)
(defvar *3dtex* nil "32x32x32 texture, 32768 voxels")
(defvar *3dsam* nil)

(defclass noise2d (actor) ())
(defmethod draw ((actor noise2d) camera time)
  (map-g #'noise2d-pipe *bs* :height .9f0))
#+nil
(progn (free-actors)
       (push (make-instance 'noise2d) *actors*))
(defclass cubetexture (actor)
  ((cubetex :initarg :cubetex)))

(defun free-cube ()
  (when *3dtex* (free *3dtex*))
  (when *2dfbo* (free *2dfbo*)))

(defun init-cube ()
  (free-cube)
  (setf *2dfbo* (make-fbo `(0 :dimensions (32 32) :element-type :r16f)))
  (setf *2dsam* (sample (attachment-tex *2dfbo* 0)))
  (setf *3dtex* (make-texture nil :dimensions '(32 32 32) :element-type :r16f))
  (setf *3dsam* (sample *3dtex*))
  t)

;;--------------------------------------------------
;; Draw 3D texture

(defun-g noise2d-frag ((uv :vec2) &uniform (height :float))
  (v! (v3! (nineveh.noise:perlin-noise (* 10 (v! uv height))))
      1))

(defpipeline-g noise2d-pipe (:points)
  :fragment (noise2d-frag :vec2))

(defun load-into-3d ()
  (let* ((height 0f0)
         (resolution (v! 32 32 32)))
    (with-free c-3d-texture (pull1-g (texref *3dtex*))
      ;; loop through slices
      (dotimes (slice (round (z resolution)))
        (setf height (/ (+ slice .5) (z resolution)))
        (map-g-into *2dfbo* #'noise2d-pipe *bs*
                    :height height)
        (with-free c-2d-texture (pull1-g (attachment-tex *2dfbo* 0))
          (dotimes (x (round (x resolution)))
            (dotimes (y (round (y resolution)))
              ;; 1 channel texture
              (setf (aref-c c-3d-texture x y slice)
                    (aref-c c-2d-texture x y))))))
      (push-g c-3d-texture (texref *3dtex*)))))

;;--------------------------------------------------
;; Render 3D texture
(defun make-cubetexture (&key (pos (v! 0 0 0))
                              (rot (q:identity)))
  (let ((obj (make-instance 'cubetexture :pos pos :rot rot :cubetex *3dsam*)))
    (push obj *actors*)
    obj))

(defmethod update ((actor cubetexture) dt)
  (with-slots (rot) actor
    (setf rot (q:from-axis-angle (v! (sin (* .7 (mynow)))
                                     (sin (* .8 (mynow)))
                                     (cos (* .6 (mynow))) 0)
                                 (radians (* 361 (cos (* .1 (mynow)))))))))

(defmethod draw ((actor cubetexture) camera time)
  (with-blending *blend-cube*
    (with-slots (buf scale cubetex) actor
      (map-g #'cube-pipe buf
             :model-world (model->world actor)
             :world-view  (world->view camera)
             :view-clip   (projection  camera)
             :scale scale
             :time time
             :samd *samd*
             :cubetex cubetex))))

(defun-g cube-frag ((uv :vec2) (frag-pos :vec3) (frag-normal :vec3)
                    &uniform (cubetex :sampler-3d) (time :float)
                    (samd :sampler-2d))
  ;;#+nil
  (v! (v3! (x (texture cubetex (v! uv (* .001 time))))
           (cellular-noise (* uv .001 time)))
      1)
  ;;(x (texture cubetex (v! uv (* .0001 time))))
  )

(defpipeline-g cube-pipe ()
  :vertex (vert g-pnt)
  :fragment (cube-frag :vec2 :vec3 :vec3))

