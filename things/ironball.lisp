(in-package #:incandescent)

(defclass ironball (assimp-thing) ())


(defmethod update ((actor ironball) dt)
  (with-slots (pos) actor
    (setf (x pos) (- (x pos) .01))
    (setf (z pos) (- (z pos) .01))))
;;BODY
(defun make-ironball (&key (scale 1f0)
                           (nth 0)
                           (rot (q:from-axis-angle (v! 1 0 0) (radians -90))xbo)
                           (pos (v! 0 0 0)))
  (let ((obj
          (destructuring-bind (&key scene buf
                                    albedo normals specular)
              (nth nth (assimp-load-meshes "/home/sendai/Downloads/_ironball/IronBall/IronBall.obj"))
            (make-instance 'ironball
                           :scene scene
                           :scale scale
                           :buf buf
                           :pos pos
                           :rot rot
                           :albedo albedo
                           :normals normals
                           :specular specular))))
    (push obj *actors*)
    obj))
