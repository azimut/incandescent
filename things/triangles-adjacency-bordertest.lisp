(in-package #:incandescent)

(defvar *cube-vertex* nil)
(defvar *cube-index* nil)
(defvar *cube-stream* nil)


(defun draw-cube ()
  (with-fbo-bound (*fbo*)
    (clear-fbo *fbo*)
    (with-setf (depth-test-function) #'<=
      (map-g #'va-pipe *cube-stream*
             :world-clip (world->clip *currentcamera*))
      (map-g #'va-pipe-border *cube-stream*
             :world-clip (world->clip *currentcamera*)))))
(defun init-cube ()
  (when *cube-stream* (free *cube-stream*) (setf *cube-stream* nil))
  ;;(when *cube-vertex* (free *cube-vertex*) (setf *cube-vertex* nil))
  (when *cube-index*  (free *cube-index*)  (setf *cube-index* nil))
  (setf *cube-vertex* (car (car (buffer-stream-gpu-arrays (cone)))))
  ;;#+nil
  (setf *cube-index*
        (destructuring-bind (&key new-indices &allow-other-keys)
            (destructuring-bind ((v) i) (buffer-stream-gpu-arrays (cone))
              (find-adjacencies v i))
          (make-gpu-array new-indices :element-type :ushort)))
  ;;#+nil
  (setf *cube-stream*
        (make-buffer-stream *cube-vertex*
                            :index-array *cube-index*
                            :primitive :triangles-adjacency)))
(defun-g va-vert ((vert g-pnt) &uniform (world-clip :mat4))
  (values (* world-clip (v! (pos vert) 1))
          (pos vert)))
(defun-g va-geom ((pos (:vec3 6)) &uniform (world-clip :mat4))
  (declare (output-primitive :kind :line-strip :max-vertices 6))
  (labels ((emit-line ((start :int) (end :int))
             (emit () (gl-position (aref gl-in start)))
             (emit () (gl-position (aref gl-in end)))
             (end-primitive)))
    (let* ((e1 (- (aref pos 2) (aref pos 0)))
           (e2 (- (aref pos 4) (aref pos 0)))
           (e3 (- (aref pos 1) (aref pos 0)))
           (e4 (- (aref pos 3) (aref pos 2)))
           (e5 (- (aref pos 4) (aref pos 2)))
           (e6 (- (aref pos 5) (aref pos 0)))
           (normal (cross e1 e2))
           (light-pos (v! 10 10 10))
           (light-dir (- light-pos (aref pos 0))))
      (when (> (dot normal light-dir) .00001)
        (setf normal (cross e3 e1))
        (when (<= (dot normal light-dir) 0)
          (emit-line 0 2))
        (setf normal (cross e4 e5))
        (setf light-dir (- light-pos (aref pos 2)))
        (when (<= (dot normal light-dir) 0)
          (emit-line 2 4))
        (setf normal (cross e2 e6))
        (setf light-dir (- light-pos (aref pos 4)))
        (when (<= (dot normal light-dir) 0)
          (emit-line 4 0))))))
(defun-g va-frag-border ()
  (v! 1 0 0 1))
(defun-g va-frag ((wpos :vec3))
  (v! 1 1 0 1))

(defpipeline-g va-pipe-border (:triangles-adjacency)
  :vertex   (va-vert g-pnt)
  :geometry (va-geom (:vec3 6))
  :fragment (va-frag-border))

(defpipeline-g va-pipe (:triangles-adjacency)
  :vertex   (va-vert g-pnt)
  :fragment (va-frag :vec3))
