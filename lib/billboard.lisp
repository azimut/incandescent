(in-package #:incandescent)

;;--------------------------------------------------
;; Billboarding
(defun-g billboard-vert ((pos :vec3)
                         &uniform
                         (time :float)
                         (world-view :mat4))
  (* world-view (v! 0 -6 -50 1)))

(defun-g billboard-frag ((uv :vec2)
                         &uniform
                         (tex :sampler-2d)
                         (time :float))
  (let* ((color (texture tex uv)))
    (values color
            (v! (x color) 0 0 (w color)))))

(defun-g billboard-geom (&uniform (camera-pos :vec3)
                                  (view-clip :mat4))
  (declare (output-primitive :kind :triangle-strip
                             :max-vertices 4))
  (let* ((p (s~ (gl-position (aref gl-in 0)) :xyz))
         (to-camera (normalize (- camera-pos p)))
         (up (v! 0 1 0))
         (right (cross to-camera up)))
    ;;
    (decf p (* 9 .5 right))
    (emit ()
          (* view-clip (v! p 1))
          (v! 0 0))
    ;;
    (incf (y p) 9f0)
    (emit ()
          (* view-clip (v! p 1))
          (v! 0 1))
    ;;
    (decf (y p) 9f0)
    (incf p (* right 9))
    (emit ()
          (* view-clip (v! p 1))
          (v! 1 0))
    ;;
    (incf (y p) 9f0)
    (emit ()
          (* view-clip (v! p 1))
          (v! 1 1))
    (end-primitive)
    (values)))
(defpipeline-g billboard-pipe (:points)
  :vertex (billboard-vert :vec3)
  :geometry (billboard-geom)
  :fragment (billboard-frag :vec2))
