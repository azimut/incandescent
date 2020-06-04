(in-package #:incandescent)

(defvar *max-bones-per-vertex* 4)
(defvar *default-animation* 0 "default animation index")

(defun list-bones (scene)
  "returns a plain list with all the bones in SCENE, helper for the REPL"
  (declare (ai:scene scene))
  (let* ((meshes (coerce (ai:meshes scene) 'list))
         (bones  (mappend (lambda (m) (coerce (ai:bones m) 'list)) meshes))
         (bones  (remove NIL bones)))
    bones))

(defun list-bones-unique (scene)
  "returns a plain list with all the bones in SCENE, helper for the REPL"
  (declare (ai:scene scene))
  (let* ((bones (list-bones scene))
         (bones (remove-duplicates bones :key #'ai:name :test #'string=)))
    bones))

;;--------------------------------------------------
;; Bones loader

(defun get-bones-per-vertex (scene mesh-bones n-vertices)
  "Returns an array OF lists OF conses. With each vertex, (BONE-ID . WEIGHT) info.
   To be used once on the buffer stream load. It uses the bones on the meshes
   to build the BONE-IDs because there could be meshes without animation but
   with bones.
   ex: #(((1 . .9) (2 . .1)) ((10 . .2) (20 . .8)))"
  (declare (type ai:scene        scene)
           (type vector          mesh-bones)
           (type positive-fixnum n-vertices))
  (let ((unique-scene-bones (list-bones-unique scene))
        (v-to-bones (make-array n-vertices :initial-element NIL)))
    (loop :for bone :across mesh-bones
          :for bone-id := (position (ai:name bone) unique-scene-bones
                                    :test #'string=
                                    :key  #'ai:name)
          :do (assert bone-id)
              (loop :for weight :across (ai:weights bone)
                    :do (with-slots ((v ai:id) (w ai:weight)) weight
                          (when (and (>= w .1) ;; discard bones with low influence
                                     (< (length (aref v-to-bones v))
                                        *max-bones-per-vertex*))
                            (push (cons bone-id w)
                                  (aref v-to-bones v))
                            ;; Sort descending by weights
                            (setf (aref v-to-bones v)
                                  (sort (aref v-to-bones v) #'>
                                        :key #'cdr))))))
    v-to-bones))

;;--------------------------------------------------
;; Bone animation helpers

(defun find-index (etime vector-keys)
  "returns the index position/rotation matching the current ETIME"
  (declare (type number etime)
           (type simple-vector vector-keys))
  (let ((pos (position-if (lambda (p) (< etime (slot-value p 'time)))
                          vector-keys)))
    (if pos
        (max 0 (1- pos))
        0)))

(defun calc-interpolated-position (etime positions)
  "returns a vec3"
  (declare (type vector positions))
  (if (or (length= 1 positions)
          (< etime (slot-value (aref positions 0) 'time)))
      (ai:value (aref positions 0))
      (let* ((index       (find-index etime positions))
             (next-index  (1+ index))
             (current-pos (aref positions index))
             (next-pos    (aref positions next-index)))
        (with-slots ((ctime time) (start ai:value)) current-pos
          (with-slots ((ntime time) (end ai:value)) next-pos
            (let* ((dt     (- ntime ctime))
                   (factor (/ (- etime ctime) dt))
                   (delta  (v3:- end start)))
              (v3:+ start
                    (v3:*s delta (coerce factor 'single-float)))))))))

(defun calc-interpolated-rotation (etime rotations)
  "returns a quaternion"
  (declare (type vector rotations))
  (if (or (length= 1 rotations)
          (< etime (slot-value (aref rotations 0) 'time)))
      (ai:value (aref rotations 0))
      (let* ((index       (find-index etime rotations))
             (next-index  (1+ index))
             (current-rot (aref rotations index))
             (next-rot    (aref rotations next-index)))
        (with-slots ((ctime time) (start ai:value)) current-rot
          (with-slots ((ntime time) (end ai:value)) next-rot
            (let* ((dt      (- ntime ctime))
                   (factor  (/ (- etime ctime) dt))
                   (qinterp (q:lerp start end (coerce factor 'single-float))))
              (q:normalize qinterp)))))))
;;
(defun get-frame-transform (node-animation frame)
  "returns a 4x4 matrix"
  (declare (type ai::node-animation node-animation))
  (with-slots ((pos-keys ai::position-keys)
               (rot-keys ai::rotation-keys)
               (sca-keys ai::scaling-keys))
      node-animation
    ;; reset frame position
    (setf frame (mod frame (length rot-keys)))
    ;; Calculate tranform matrix based on time
    (m4-n:*
     (m4:translation (ai:value (aref pos-keys frame)))
     (q:to-mat4      (ai:value (aref rot-keys frame)))
     ;;(m4:scale (ai:value (aref sca-keys 0)))
     )))

(defun get-time-transform (node-animation time)
  "calculate the tansform matrix based on time.
   returns a 4x4 matrix"
  (declare (type ai::node-animation node-animation))
  (with-slots ((pos-keys ai::position-keys)
               (rot-keys ai::rotation-keys)
               (sca-keys ai::scaling-keys))
      node-animation
    (let ((ipos (calc-interpolated-position time pos-keys))
          (irot (calc-interpolated-rotation time rot-keys)))
      (m4-n:*
       (m4:translation ipos)
       (q:to-mat4 irot)
       ;;(m4:scale (ai:value (aref sca-keys 0)))
       ))))
;;
(defgeneric get-nodes-transforms (scene node-type &key time frame)
  (:documentation "returns a hash of mat4's with each node transform
for value and node name for the key")
  (:method ((scene ai:scene) (node-type (eql :static)) &key time frame)
    (declare (ignore time frame)); ? there is a call with these params
    (let ((nodes-transforms (make-hash-table :test #'equal)))
      (labels ((walk-node (node parent-transform)
                 (declare (type ai:node node)
                          (type vector parent-transform))
                 (with-slots ((name ai:name) (transform ai:transform) (children ai:children))
                     node
                   (let ((global (m4:* parent-transform (m4:transpose transform))))
                     (setf (gethash name nodes-transforms) global)
                     (map 'vector
                          (lambda (c) (walk-node c global))
                          children)))))
        (walk-node (ai:root-node scene) (m4:identity)))
      nodes-transforms))
  (:method ((scene ai:scene) (node-type (eql :animated-time)) &key time frame)
    (declare (ignore frame))
    (let* ((animation        (aref (ai:animations scene) *default-animation*))
           (duration         (ai:duration animation))
           ;; NOTE: animation-index is a hash lookup table for BONE>NODE-ANIMATION
           (animation-index  (ai:index animation))
           ;; NOTE: temporal lookup table for bones *thinking emoji*
           (nodes-transforms (make-hash-table :test #'equal)))
      (declare (type hash-table animation-index))
      (labels
          ((walk-node (node parent-transform)
             (declare (type ai:node node) (type vector parent-transform))
             (with-slots ((name      ai:name)
                          (transform ai:transform)
                          (children  ai:children))
                 node
               (let* ((final-transform (if-let ((node-anim (gethash name animation-index)))
                                         (get-time-transform node-anim (mod time duration))
                                         (m4:transpose transform)))
                      (global          (m4:* parent-transform final-transform)))
                 (setf (gethash name nodes-transforms) global)
                 ;; WALK
                 (map 'vector
                      (lambda (c) (walk-node c global))
                      children)))))
        (walk-node (ai:root-node scene)
                   (m4:identity)))
      nodes-transforms))
  (:method ((scene ai:scene) (node-type (eql :animated-frame)) &key time frame)
    (declare (ignore time))
    (let* ((animation        (aref (ai:animations scene) *default-animation*))
           ;; NOTE: animation-index is a hash lookup table for BONE>NODE-ANIMATION
           (animation-index  (ai:index animation))
           ;; NOTE: temporal lookup table for bones *thinking emoji*
           (nodes-transforms (make-hash-table :test #'equal)))
      (declare (type hash-table animation-index))
      (labels
          ((walk-node (node parent-transform)
             (declare (type ai:node node) (type vector parent-transform))
             (with-slots ((name      ai:name)
                          (transform ai:transform)
                          (children  ai:children))
                 node
               (let* ((final-transform (if-let ((node-anim (gethash name animation-index)))
                                         (get-frame-transform node-anim frame)
                                         (m4:transpose transform)))
                      (global          (m4:* parent-transform
                                             final-transform)))
                 (setf (gethash name nodes-transforms) global)
                 ;; WALK!
                 (map 'vector
                      (lambda (c) (walk-node c global))
                      children)))))
        (walk-node (ai:root-node scene)
                   (m4:identity)))
      nodes-transforms)))

(defun get-bones-tranforms (scene &key (frame 0 frame-p) (time 0 time-p))
  "ANIMATIONLESS
   returns an array with the m4 matrices of each bone offset, ready to push-g"
  (declare (ai:scene scene)
           (type fixnum frame)
           (type number time))
  (let* ((root-offset      (m4:inverse
                            (m4:transpose
                             (ai:transform (ai:root-node scene)))))
         (unique-bones     (list-bones-unique scene))
         (bones-transforms (make-array (length unique-bones)))
         ;; Note: I might have bones but NO animation
         (node-type        (cond ((and frame-p time-p)
                                  (error "provide EITHER time or frame offset"))
                                 ((emptyp (ai:animations scene))
                                  :static)
                                 ((and time-p (not (emptyp (ai:animations scene))))
                                  :animated-time)
                                 ((and frame-p (not (emptyp (ai:animations scene))))
                                  :animated-frame)))
         (nodes-transforms (get-nodes-transforms scene node-type :time time :frame frame)))
    (loop :for bone :in unique-bones
          :for bone-id :from 0
          :do (with-slots ((name ai:name) (offset ai:offset-matrix)) bone
                (let ((node-transform (gethash name nodes-transforms)))
                  (setf (aref bones-transforms bone-id)
                        ;; I got a mesh that has 0 on the bones offsets...
                        ;; The mesh also didn't have animations so might be
                        ;; that was the reason...
                        ;;node-transform
                        (if (m4:0p offset)
                            (m4:* root-offset
                                  node-transform)
                            (m4:* root-offset
                                  node-transform
                                  (m4:transpose offset)))))))
    bones-transforms))
