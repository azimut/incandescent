(in-package #:incandescent)

(defclass watcher (assimp-thing-with-bones) ())
(defclass watcher2 (assimp-thing-with-bones) ())

;;BODY
(defun make-watcher (&key (scale 1f0)
                          (rot (q:from-axis-angle (v! 1 0 0) (radians -90))xbo)
                          (pos (v! 0 0 0)))
  (let ((obj
          (destructuring-bind (&key scene buf
                                    albedo normals specular
                                    bones duration)
              (nth 0 (assimp-load-meshes "static/guard/boblampclean.md5mesh"))
            (make-instance 'watcher
                           :scene scene
                           :scale scale
                           :buf buf
                           :pos pos
                           :rot rot
                           :albedo albedo
                           :normals normals
                           :specular specular
                           :bones bones
                           :duration duration))))
    (push obj *actors*)
    obj))

;; HEAD
(defun make-watcher2 (&key (scale 1f0)
                           (rot (q:from-axis-angle (v! 1 0 0) (radians -90))xbo)
                           (pos (v! 0 0 0)))
  (let ((obj
          (destructuring-bind (&key scene buf
                                    albedo normals specular
                                    bones duration)
              (nth 1 (assimp-load-meshes "static/guard/boblampclean.md5mesh"))
            (make-instance 'watcher2
                           :scene scene
                           :scale scale
                           :buf buf
                           :pos pos
                           :rot rot
                           :albedo albedo
                           :normals normals
                           :specular specular
                           :bones bones
                           :duration duration))))
    (push obj *actors*)
    obj))

(progn (let ((time 0.8f0)
             (init 0.8f0)
             (stepper (make-stepper (seconds .01) (seconds .01))))
         (defmethod update ((actor watcher) dt)
           ;;#+nil
           (when (funcall stepper)
             (with-slots (bones scene duration) actor
               (push-g (get-bones-tranforms scene
                                            :time (+ 19.5 (* .5 (sin time))))
                       bones)
               (incf time .05)
               #+nil
               (if (> time duration)
                   (setf time init)
                   (incf time .05))))))

       (let ((time 0.8f0)
             (init 0.8f0)
             (stepper (make-stepper (seconds .01) (seconds .01))))
         (defmethod update ((actor watcher2) dt)
           #+nil
           (when (funcall stepper)
             (with-slots (bones scene duration) actor
               (push-g (get-bones-tranforms scene :time time)
                       bones)
               (if (> time duration)
                   (setf time init)
                   (incf time .04)))))))
