(in-package :incandescent)

(defclass radio (assimp-thing audio-sound)
  ((pickup :initform nil :initarg :pickup
           :documentation "sound made to pickup object")))

(defun make-radio (&optional (pos (v! 3 -2 20)) (rot (q:from-axis-angle (v! 0 1 0) (radians 270))))
  (let ((*default-albedo* "static/radio/Textures/02 - Default_Base_Color.png"))
    (with-slots (buf albedo normals specular scene)
        (first
         (assimp-load-meshes "static/radio/Model/radio_low_poly.FBX"))
      (let ((obj (make-instance
                  'radio
                  :pos pos :rot rot
                  :scale .04
                  :buf buf
                  :volume .5
                  :pickup (load-sfx "static/PickItem1.mp3")
                  :wait-p t
                  :sources (cm:new cm:cycle
                             :of
                             (list
                              (load-sfx "static/RadioHelp1-short.mp3" :volume .5)
                              (load-sfx "static/RadioHelp2.mp3" :volume .5))
                             :repeat 1)
                  :albedo albedo :normals normals :specular specular
                  :scene scene)))
        (push obj *actors*)
        obj))))

;; FIXME: non destructive way for text...cycle?
(let ((phrase '("...ouch..."
                ""
                "where am i?"
                "PRESS R TO PICK UP"
                "")))
  (defmethod update ((actor radio) dt)
    (with-slots (rot pos sources playing scale pickup end-p) actor

      ;; No More to play
      (if end-p
          (when (key-down-p key.r)
            (setf *actors* (remove actor *actors*))
            (play-sound pickup)
            (make-monster)
            (kill-text))
          (incf *exposure* .004))

      ;; The thing playing has stopped
      ;; (when (and source (harmony:paused-p source))
      ;;   (make-text (pop phrase))
      ;;   (setf playingp nil
      ;;         source   nil))

      ;; Nothing playing and we are near
      (when (and (not playing) (distance-to-camera pos 25))
        (make-text (pop phrase))
        (play-sound actor)))))
