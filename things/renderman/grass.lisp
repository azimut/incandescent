(in-package :incandescent)

;; NOTE: instancing might be an overkill
;; TODO: can probably integrated better with assimp default helpers
(defvar *grass-blend*
  (make-blending-params
   :source-alpha :one-minus-src-alpha))

(defclass grass (actor)
  ((sam :initarg :sam)
   (buf :initarg :buf)
   (spe :initarg :spe)
   (nor :initarg :nor)
   (instances :initarg :instances
              :initform 1)
   (offsets   :initarg :offsets
              :documentation "C array of grass-offsets, providing initial
                              position and rotation")))

(defmethod free ((object grass))
  (with-slots (offsets) object
    (free offsets)))

(defun make-offsets (instances &optional (min-x -20f0) (max-x 20f0) (min-z -20f0) (max-z 20f0))
  "Returns initial positions and rotation for instances.
   XYZ are the position
   W   is the rotation"
  (loop :repeat instances
        :collect (let ((rotation (random-in-range 0f0 360f0))
                       (position (v! (random-in-range min-x max-x)
                                     0f0
                                     (random-in-range min-z max-z))))
                   (v! position rotation))))

(defun make-grass (&key (pos (v! 0 0 0)) (scale 10f0) (instances 1))
  (declare (type (integer 1 1000) instances))
  (let ((obj (make-instance
              'grass
              :instances instances
              :offsets (make-c-array (list (make-offsets 1000))
                                     :element-type :vec4)
              :scale scale
              :pos pos
              :rot (q:from-axis-angle (v! 0 1 0) (radians 360))
              ;; TODO: yikes
              :buf (caar
                    (let ((*processing-flags* '(:ai-process-triangulate
                                                :ai-process-flip-u-vs
                                                :ai-process-preset-target-realtime-quality
                                                :ai-process-calc-tangent-space)))
                      (assimp-load-meshes "static/grass/grass-share2.obj"
                                          :instantiate-p nil)))
              :sam (get-tex "static/grass/grass_patch_d.png")
              :nor (get-tex "static/grass/grass_patch_n.png")
              :spe (get-tex "static/grass/grass_patch_n.png" nil t :r8))))
    (push obj *actors*)
    obj))


(defun-g grass-vert ((vert g-pnt)
                     &uniform
                     (offsets (:vec4 1000))
                     (model-world :mat4)
                     (world-view :mat4)
                     (view-clip :mat4)
                     (scale :float)
                     (time :float))
  (let* ((pos        (pos vert))
         (pos        (+ pos (s~ (aref offsets (int gl-vertex-id)) :xyz)))
         (pos        (* scale pos))
         (norm       (norm vert))
         (tex        (tex vert))
         (world-norm (* (m4:to-mat3 model-world) norm))
         (world-pos  (* model-world (v! pos 1)))
         (view-pos   (* world-view  world-pos))
         (clip-pos   (* view-clip   view-pos)))
    (values clip-pos
            tex
            world-norm
            (s~ world-pos :xyz))))

(defun-g grass-frag ((uv :vec2)
                     (frag-norm :vec3)
                     (frag-pos :vec3)
                     &uniform
                     (light-pos :vec3)
                     (light-color :vec3)
                     (cam-pos :vec3)
                     (albedo :sampler-2d)
                     (specular :sampler-2d)
                     (nor :sampler-2d))
  (let* ((normal (norm-from-map nor uv))
         (color4 (texture albedo uv))
         (color3 (expt (s~ color4 :xyz)
                       (vec3 2.2f0)))
         (color3 (dir-light-apply color3
                                  light-color
                                  light-pos
                                  frag-pos
                                  normal
                                  cam-pos
                                  .1f0
                                  (x (texture specular uv)))))
    (v! (v! (y frag-pos) 0 0)
        (w color4))))

(defpipeline-g grass-pipe ()
  :vertex   (grass-vert g-pnt)
  :fragment (grass-frag :vec2 :vec3 :vec3))

;; NOTE: disable depth-mask when rendering anything transparent (?
(defmethod draw ((actor grass) camera time)
  (with-slots (sam nor buf scale spe) actor
    (with-blending *grass-blend*
      (with-setf* ((depth-mask) nil
                   (cull-face)  nil)
        (map-g #'grass-pipe buf
               :albedo sam
               :nor nor
               :specular spe
               :scale scale
               :cam-pos (pos camera)
               :light-pos *light-pos*
               :light-color *light-color*
               :model-world (model->world actor)
               :world-view  (world->view camera)
               :view-clip   (projection camera))))))
