(in-package #:incandescent)

(defvar *shadow-buffers* (make-hash-table :test #'equal))
(defvar *blend-defer* (make-blending-params :source-rgb :one
                                            :destination-rgb :one))

(defclass roman-column (actor)
  ((shadow-buf :documentation "triangles-adjacency buffers-stream version")
   (height     :initform 10  :initarg :height)
   (radius     :initform .5  :initarg :radius)
   (tangentsp  :initform nil :initarg :tangentsp)))

(defmethod initialize-instance :after ((obj roman-column) &key)
  (with-slots (radius height shadow-buf tangentsp) obj
    (destructuring-bind (vert index)
        (nineveh.mesh.data.primitives:cylinder-gpu-arrays :radius radius
                                                          :height height)
      (let ((key (list :height height :radius radius :tangentsp tangentsp)))
        (or (gethash key *shadow-buffers*)
            (setf (gethash key *shadow-buffers*)
                  (if tangentsp
                      (make-buffer-stream
                       (list vert (tbdata-from-vertex-and-indices vert index))
                       :index-array index
                       :primitive :triangles-adjacency)
                      (make-buffer-stream
                       vert
                       :index-array index
                       :primitive :triangles-adjacency))))))))

(defun make-roman-column (&key (pos (v! 0 0 0))
                               (rot (q:identity))
                               (radius .5)
                               (height 10)
                               tangentsp)
  (let ((obj (make-instance 'roman-column :pos pos :rot rot
                                          :buf (cylinder radius height tangentsp))))
    (push obj *actors*)
    obj))

(defmethod update ((actor roman-column) dt)
  ;;#+nil
  (with-slots (pos) actor
    (if (> (z pos) 25f0)
        (decf (z pos) 50f0)
        (incf (z pos) .1f0))))

(defmethod draw ((actor roman-column) camera (time single-float))
  (with-slots (buf scale color) actor
    (map-g #'roman-column-pipe buf
           :scale scale
           :color color
           :time  time
           :cam-pos (pos camera)
           :model-world (model->world actor)
           :world-view  (world->view camera)
           :view-clip   (projection  camera)
           :light-color *light-color*
           :light-pos   *light-pos*)))

(defun-g flat-vert ((vert g-pnt) &uniform
                    (model-world :mat4)
                    (world-view  :mat4)
                    (view-clip   :mat4)
                    (scale       :float))
  (let* ((pos        (* scale (pos vert)))
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

(defun-g roman-column-frag ((uv :vec2)
                            (frag-normal :vec3)
                            (frag-pos :vec3)
                            &uniform
                            (time        :float)
                            (color       :vec3)
                            (cam-pos     :vec3)
                            (light-color :vec3)
                            (light-pos   :vec3))
  (values (v! .5 .5 .5)
          frag-pos
          (normalize frag-normal)))

(defpipeline-g roman-column-pipe ()
  :vertex   (flat-vert g-pnt)
  :fragment (roman-column-frag :vec2 :vec3 :vec3))
