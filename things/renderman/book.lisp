(in-package :incandescent)

;; SCP1025 - encyclopedia diseases
;; ./049/Searching4.ogg - not well sickness

(defclass book (assimp-thing audio-sound) ())

(defun make-book (&optional (pos (v! 0 0 0)))
  (declare (type rtg-math.types:vec3 pos))
  (with-slots (buf albedo normals specular scene)
      (first
       (assimp-load-meshes "static/1025/scp1025.b3d"))
    (let* ((source (load-sfx "static/1025/tarea201-mono.mp3"))
           (obj (make-instance 'book
                               :pos pos
                               :volume .5
                               :sources source
                               :buf buf :scene scene
                               :scale 1.5
                               :albedo albedo :specular specular
                               :normals normals)))
      (setf (harmony:input-location source *sfx*)
            pos)
      (push obj *actors*)
      obj)))

(let ((floatingp nil)
      (stepper (make-stepper (seconds .2) (seconds .2))))
  (defmethod update ((actor book) dt)
    (with-slots (pos rot) actor
      (when (and (key-down-p key.r)
                 (distance-to-camera pos 20))
        (when (funcall stepper) ;; controls rapid fire
          (setf floatingp (not floatingp))
          (if floatingp
              (play-sound actor :loop-p t :seek-to 0)
              (play-sound actor :loop-p nil :pause-p t))))
      (if floatingp
          (setf (y pos) (+ 4 (* .1 (sin (mynow)))))
          (setf (y pos) 0f0)))))
