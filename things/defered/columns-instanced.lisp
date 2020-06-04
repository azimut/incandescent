(in-package #:incandescent)

;; UBO Instanced columns with a special pipeline for shadow volumes

(defvar *shadow-buffers* (make-hash-table :test #'equal))

(defclass roman-column (actor)
  ((shadow-buf :documentation "triangles-adjacency buffers-stream version")
   (ubo        :initform nil)
   (instances  :initform 50)
   (height     :initform 10  :initarg :height)
   (radius     :initform .5  :initarg :radius)
   (tangentsp  :initform nil :initarg :tangentsp)))

(defstruct-g (roman-columns :layout :std-140)
  (pos (:vec3 50)))

(defmethod free ((obj roman-column))
  (with-slots (ubo) obj
    (free ubo)))

(defun init-position-ubo ()
  (let ((positions (loop :repeat (/ 50 2)
                         :for z :from -35 :by 5
                         :append (list (v!  5 0 z)
                                       (v! -5 0 z)))))
    (make-ubo (list positions) 'roman-columns)))

(defmethod initialize-instance :after ((obj roman-column) &key)
  (with-slots (radius height shadow-buf tangentsp ubo) obj
    (destructuring-bind (vert index)
        (nineveh.mesh.data.primitives:cylinder-gpu-arrays :radius radius
                                                          :height height)
      (let ((key (list :height height :radius radius :tangentsp tangentsp)))
        (setf shadow-buf
              (or (gethash key *shadow-buffers*)
                  (setf (gethash key *shadow-buffers*)
                        (if tangentsp
                            (make-buffer-stream
                             (list vert (tbdata-from-vertex-and-indices vert index))
                             :index-array index
                             :primitive :triangles-adjacency)
                            (make-buffer-stream
                             vert
                             :index-array (gpu-find-adjadencies vert index)
                             :primitive :triangles-adjacency)))))))
    (setf ubo (init-position-ubo))))

(defun make-roman-column (&key (pos (v! 0 0 0))
                               (rot (q:identity))
                               (radius .5)
                               (height 10)
                               tangentsp)
  (let ((obj (make-instance 'roman-column :pos pos :rot rot
                                          :buf (cylinder radius height tangentsp))))
    (push obj *actors*)
    obj))

;;--------------------------------------------------

(defun-g roman-column-vert ((vert g-pnt)
                            &uniform
                            (positions roman-columns :ubo)
                            (model-world :mat4)
                            (world-view  :mat4)
                            (view-clip   :mat4))
  (let* ((pos        (+ (aref (roman-columns-pos positions) gl-instance-id)
                        (pos vert)))
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

(defun-g roman-column-frag ((uv          :vec2)
                            (frag-normal :vec3)
                            (frag-pos    :vec3))
  (values (v! .5 .5 .5)
          frag-pos
          (normalize frag-normal)))

(defun-g roman-column-empty-frag ((uv          :vec2)
                                  (frag-normal :vec3)
                                  (frag-pos    :vec3))
  (values))

(defpipeline-g roman-column-pipe ()
  :vertex   (roman-column-vert g-pnt)
  :fragment (roman-column-frag :vec2 :vec3 :vec3))
(defpipeline-g roman-column-empty-pipe ()
  :vertex   (roman-column-vert g-pnt)
  :fragment (roman-column-empty-frag :vec2 :vec3 :vec3))

(defmethod draw ((actor roman-column) camera (time single-float))
  (with-slots (buf ubo instances) actor
    (with-instances instances
      (map-g #'roman-column-pipe buf
             :positions ubo
             :model-world (model->world actor)
             :world-view  (world->view camera)
             :view-clip   (projection  camera)))))

(defmethod draw-depth ((actor roman-column) camera time)
  (with-slots (buf ubo instances) actor
    (with-instances instances
      (map-g #'roman-column-empty-pipe buf
             :positions ubo
             :model-world (model->world actor)
             :world-view  (world->view camera)
             :view-clip   (projection  camera)))))

;;--------------------------------------------------

(defun-g shadow-roman-column-vert ((vert g-pnt)
                                   &uniform
                                   (positions roman-columns :ubo)
                                   (lights    light-data    :ssbo)
                                   (light-index :int))
  (values (v! 0 0 0 0)
          (+ (aref (roman-columns-pos positions) gl-instance-id)
             (pos vert))
          ;;(:flat (aref (light-data-pos lights) light-index))
          (:flat (v! 10 1 0))
          ))

(defun-g shadow-roman-column-geom ((position  (:vec3 6))
                                   (light-pos (:vec3 6))
                                   &uniform
                                   (world-clip :mat4))
  (declare
   (output-primitive :kind :triangle-strip
                     :max-vertices 18))
  (labels ((emit-quad ((start-pos  :vec3)
                       (end-pos    :vec3)
                       (light-pos3 :vec3)
                       (world-clip :mat4))
             (let* ((epsilon .001)
                    (light-dir (normalize (- start-pos light-pos3)))
                    (deviation (* light-dir epsilon)))
               (emit () (* world-clip (v! (+ start-pos deviation) 1)))
               (emit () (* world-clip (v! light-dir 0)))
               (setf light-dir (normalize (- end-pos light-pos3)))
               (setf deviation (* light-dir epsilon))
               (emit () (* world-clip (v! (+ end-pos deviation) 1)))
               (emit () (* world-clip (v! light-dir 0)))
               (end-primitive)))
           (faces-light ((a :vec3) (b :vec3) (c :vec3) (light-pos3 :vec3))
             (let* ((n  (cross (- b a) (- c a)))
                    (da (- light-pos3 a))
                    (db (- light-pos3 b))
                    (dc (- light-pos3 c)))
               (or (> (dot n da) 0)
                   (> (dot n db) 0)
                   (> (dot n dc) 0)))))
    (let* ((light-pos3 (aref light-pos 0))
           (p0 (aref position 0))
           (p1 (aref position 1))
           (p2 (aref position 2))
           (p3 (aref position 3))
           (p4 (aref position 4))
           (p5 (aref position 5))
           (epsilon .001)
           (light-dir (normalize (- p0 light-pos3)))
           (deviation (* light-dir epsilon)))
      (when (faces-light p0 p2 p4 light-pos3)
        (when (not (faces-light p0 p1 p2 light-pos3))
          (emit-quad p0 p2 light-pos3 world-clip))
        (when (not (faces-light p2 p3 p4 light-pos3))
          (emit-quad p2 p4 light-pos3 world-clip))
        (when (not (faces-light p4 p5 p0 light-pos3))
          (emit-quad p4 p0 light-pos3 world-clip))
        ;; Front cap
        (emit () (* world-clip (v! (+ p0 deviation) 1)))
        (setf light-dir (normalize (- p2 light-pos3)))
        (setf deviation (* light-dir epsilon))
        (emit () (* world-clip (v! (+ p2 deviation) 1)))
        (setf light-dir (normalize (- p4 light-pos3)))
        (setf deviation (* light-dir epsilon))
        (emit () (* world-clip (v! (+ p4 deviation) 1)))
        (end-primitive)
        ;; Back cap
        (setf light-dir (normalize (- p0 light-pos3)))
        (emit () (* world-clip (v! light-dir 0)))
        (setf light-dir (normalize (- p4 light-pos3)))
        (emit () (* world-clip (v! light-dir 0)))
        (setf light-dir (normalize (- p2 light-pos3)))
        (emit () (* world-clip (v! light-dir 0)))
        (end-primitive)))
    (values)))

(defun-g shadow-roman-column-frag ()
  (v! 0 1 1 1))

(defpipeline-g shadow-roman-column-pipe (:triangles-adjacency)
  :vertex   (shadow-roman-column-vert g-pnt)
  :geometry (shadow-roman-column-geom (:vec3 6) (:vec3 6))
  :fragment (shadow-roman-column-frag))

(defmethod draw-column-silloute (obj camera))
(defmethod draw-shadow-silloute ((obj piso) camera)
  (draw obj camera (mynow))
  )
(defmethod draw-column-silloute ((obj roman-column) camera)
  (with-slots (shadow-buf ubo (column-instances instances)) obj
    (when *light-volumes*
      (with-slots (light-ssbo (light-instances instances)) (first *light-volumes*)
        (dotimes (i light-instances)
          (with-instances column-instances
            (map-g #'shadow-roman-column-pipe shadow-buf
                   :world-clip (world->clip camera)
                   :positions ubo
                   :light-index i
                   :lights light-ssbo)))))))
