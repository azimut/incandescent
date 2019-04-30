(in-package :incandescent)

(defclass monster (assimp-thing-with-bones)
  ((lerp :initform 0f0)))

(defun make-monster ()
  (make-sound :wildsteps .2
              "static/StepForest1.ogg.mp3"
              "static/StepForest2.ogg.mp3"
              "static/StepForest3.ogg.mp3")
  (mapcar
   (lambda (obj)
     (with-slots (buf albedo normals specular bones scene duration) obj
       (push (make-instance 'monster
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
    (with-slots (scene bones pos duration lerp) actor
      ;;
      (when (funcall step)
        (setf (harmony:input-location
               (play-sound (alexandria:random-elt
                            (slot-value (gethash :wildsteps *audio-sounds*) 'sources)))
               *sfx*)
              pos))
      ;;
      (if (> lerp 1)
          (setf *actors* (remove actor *actors*))
          (setf pos (v! -100 0 (lerp -150f0 -800f0 lerp))))
      (incf lerp .001)
      (push-g (get-bones-tranforms scene :time time)
              bones)
      ;; loop
      (if (> time duration)
          (setf time init)
          (incf time .02)))))
