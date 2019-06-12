(in-package :incandescent)

(defclass grulla (assimp-thing)
  ((xfrom :initform (serapeum:random-in-range -50f0 50f0))
   (xto :initform (serapeum:random-in-range -50f0 50f0))
   (xammount :initform 0f0)
   (zfrom :initform (serapeum:random-in-range -50f0 50f0))
   (zto :initform (serapeum:random-in-range -50f0 50f0))
   (zammount :initform 0f0)))

(defun make-grulla (&optional (pos (v! 0 0 0)))
  (let ((*processing-flags*
         ;; removes an empty mesh
         (cons :ai-process-preset-target-realtime-quality
               *processing-flags*)))
    (with-slots (buf albedo normals specular scene)
        (first
         (assimp-load-meshes
          "/home/sendai/Downloads/scpcb-master/GFX/items/origami.b3d"))
      (let ((obj (make-instance 'grulla
                                :pos pos
                                :scale .1
                                :buf buf :scene scene
                                :albedo albedo :specular specular :normals normals)))
        (push obj *actors*)
        obj))))

(defmethod update ((actor grulla) dt)
  (with-slots (pos
               rot
               xfrom xto xammount
               zfrom zto zammount)
      actor
    (setf (y pos) (+ 2 (sin (mynow))))
    (setf (x pos) (lerp xfrom xto xammount))
    (setf (z pos) (lerp zfrom zto zammount))
    (incf xammount .01)
    (incf zammount .01)
    (when (and (> zammount 1f0) (> xammount 1))
      (setf xammount 0f0)
      (setf xfrom (x pos))
      (setf xto   (serapeum:random-in-range -50f0 50f0))
      (setf zammount 0f0)
      (setf zfrom (z pos))
      (setf zto   (serapeum:random-in-range -50f0 50f0))
      ;;
      (setf rot (q:look-at *vec3-up* (v! xfrom 0 zfrom) (v! xto 0 zto))))))

(defmethod draw ((actor grulla) camera time)
  (with-setf (cull-face) nil
    (with-slots (buf albedo normals scale specular) actor
      (map-g #'assimp-tex-pipe-simple buf
             :scale scale
             ;; Lighting
             :model-world (model->world actor)
             :world-view (world->view camera)
             :view-clip  (projection camera)
             ;; PBR
             :cam-pos (pos camera)
             :albedo albedo
             :time time
             :specular specular
             :normals normals))))
