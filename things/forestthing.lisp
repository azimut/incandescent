(in-package :incandescent)

(defclass monster (assimp-thing-with-bones audio-sound)
  ((lerp :initform 0f0)))

(defun make-monster ()
  (mapcar
   (lambda (obj)
     (with-slots (buf albedo normals specular bones scene duration) obj
       (push (make-instance 'monster
                            :sources `(,(load-sfx "static/StepForest1.ogg.mp3")
                                       ,(load-sfx "static/StepForest2.ogg.mp3")
                                       ,(load-sfx "static/StepForest3.ogg.mp3"))
                            :scene scene
                            :buf buf
                            :scale .4
                            :duration duration
                            :rot (q:from-axis-angle (v! 1 0 0) (radians 90))
                            :bones bones
                            :albedo albedo
                            :normals normals
                            :specular specular)
             *actors*)))
   (assimp-load-meshes "static/monster/forestmonster.b3d")))

(let* ((init 24.8f0)
       (time init)
       (step (make-stepper (seconds .5) (seconds .5))))
  (defmethod update ((actor monster) dt)
    (with-slots (scene rot bones pos duration lerp) actor
      ;;
      (when (funcall step)
        (play-sound actor))
      ;;
      (if (> lerp 1)
          (setf *actors* (remove actor *actors*))
          (setf pos (v! (lerp -100f0 100f0 lerp) 0 (lerp -150f0 -400f0 lerp))))
      (incf lerp .002)
      (push-g (get-bones-tranforms scene :time time)
              bones)
      (setf rot (q:* (q:from-axis-angle (v! 1 0 0) (radians 90))
                     (q:from-axis-angle (v! 0 0 1) (radians 45))))
      ;; loop
      (if (> time duration)
          (setf time init)
          (incf time .02)))))
