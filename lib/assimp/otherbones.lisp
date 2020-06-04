(in-package :incandescent)

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

(defun get-bones-per-vertex (scene mesh-bones n-vertices)
  "Returns an array of lists of tuples/cons pairs.
   Run once at mesh LOAD time.
   ex: #(((1 . .9) (2 . .1)) ((10 . .2) (20 . .8)))"
  (declare (type ai:scene scene)
           (type vector mesh-bones)
           (type positive-fixnum n-vertices))
  (let ((unique-scene-bones (list-bones-unique scene))
        (v-to-bones (make-array n-vertices :initial-element NIL)))
    (loop
      :for bone :across mesh-bones
      :for bone-id := (position (ai:name bone) unique-scene-bones
                                :test #'string=
                                :key  #'ai:name)
      :do
         (loop
           :for weight :across (ai:weights bone)
           :do
              (with-slots ((v ai:id) (w ai:weight)) weight
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

(defun find-index (etime positions)
  "returns the index position matching the current ETIME"
  (declare (type vector positions)
           (type number etime))
  (let ((pos (position-if (lambda (p) (< etime (slot-value p 'time)))
                          positions)))
    (if pos
        (max 0 (1- pos))
        0)))

(defun calc-interpolated-position (etime positions)
  "returns a vec3"
  (declare (type vector positions))
  (if (or (< etime (slot-value (aref positions 0) 'time))
          (length= 1 positions))
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
  (if (or (< etime (slot-value (aref rotations 0) 'time))
          (length= 1 rotations))
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

(defun get-frame-transform (node-animation frame)
  "returns a matrix"
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
     ;;(m4:scale (v3! 1.6))
     ;;(m4:scale (ai:value (aref sca-keys 0)))
     )))

(defun get-time-transform (node-animation time)
  "calculate the tansform matrix based on time
returns a matrix"
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
       ;;(m4:scale (v3! .9f0))
       ;;(m4:scale (ai:value (aref sca-keys 0)))
       ))))

(defgeneric get-nodes-transforms (scene node-type &key frame time)
  (:documentation "returns a hash of mat4's with each node transform
for value and node name for the key")
  (:method ((scene ai:scene) (node-type (eql :static)) &key frame time)
    (let ((nodes-transforms (make-hash-table :test #'equal)))
      (labels ((walk-node (node parent-transform)
                 (declare (type ai:node node)
                          (type vector parent-transform))
                 (with-slots ((name      ai:name)
                              (transform ai:transform)
                              (children  ai:children))
                     node
                   (let ((global
                           (m4:* parent-transform (m4:transpose transform))))
                     (setf (gethash name nodes-transforms) global)
                     (map 'vector
                          (lambda (c) (walk-node c global))
                          children)))))
        (walk-node (ai:root-node scene) (m4:identity)))
      nodes-transforms))
  (:method ((scene ai:scene) (node-type (eql :animated)) &key frame time)
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
               ;; FIXME: see below mess
               (let* ((node-anim (gethash name animation-index))
                      (time-transform
                        (when node-anim
                          (if frame
                              (get-frame-transform node-anim frame)
                              (get-time-transform  node-anim (mod time duration)
                                                   ))))
                      (final-transform (if time-transform
                                           time-transform
                                           (m4:transpose transform)))
                      (global (m4:* parent-transform
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
   returns an array with the m4 matrices of each bone offset"
  (declare (ai:scene scene))
  (let* ((root-offset      (m4:inverse
                            (m4:transpose
                             (ai:transform (ai:root-node scene)))))
         (unique-bones     (list-bones-unique scene))
         (bones-transforms (make-array (length unique-bones)))
         ;; Note: I might have bones but NO animation
         (node-type        (if (emptyp (ai:animations scene))
                               :static
                               :animated))
         (valid            (assert (or (eq :static node-type)
                                       (or frame-p time-p))))
         ;;(nodes-transforms (get-nodes-transforms scene :static))
         (nodes-transforms (if frame-p
                               (get-nodes-transforms scene node-type
                                                     :frame frame)
                               (get-nodes-transforms scene node-type
                                                     :time time))))
    (loop
      :for bone :in unique-bones
      :for bone-id :from 0 :do
         (with-slots ((name   ai:name)
                      (offset ai:offset-matrix))
             bone
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
