(in-package :incandescent)

(defclass rope (assimp-thing) ())

(defmethod update ((actor rope) dt)
  (with-slots (rot) actor
    (setf rot (q:from-axis-angle (v! 0 1 0)
                                 (radians (+ (* 10 (sin (mynow)))
                                             -59))))))
(defun make-rope (&key (pos (v! 10 20 -10))
                       (rot (q:identity)))
  (with-slots (buf albedo normals specular scene)
      (nth 5 (assimp-load-meshes "/home/sendai/Downloads/rope/MG363_00_11.obj"))
    (push (make-instance 'rope
                         :buf buf :albedo albedo :normals normals
                         :specular specular :scene scene
                         :scale 30f0
                         :rot rot
                         :pos (v3:- pos (v! 0 9.5 0)))
          *actors*))
  (with-slots (buf albedo normals specular scene)
      (nth 0 (assimp-load-meshes "/home/sendai/Downloads/rope/MG363_00_10.obj"))
    (push (make-instance 'rope
                         :buf buf :albedo albedo :normals normals
                         :specular specular :scene scene
                         :scale 30f0
                         :rot rot
                         :pos pos)
          *actors*)))

(progn (setf *actors* nil)
       (make-desk)
       (make-pinkf)
       (make-chair :pos (v! 10 -2 -10)
                   :rot (q:* (q:from-axis-angle (v! 0 0 1) (radians 90))
                             (q:from-axis-angle (v! 1 0 0) (radians -10))))
       (make-rope)
       ;;(reset-camera *camera* (v! 0 0 10))
       )
