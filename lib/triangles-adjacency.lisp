(in-package #:incandescent)

;; helpers to create a new index array from a given mesh
;; http://ogldev.atspace.co.uk/www/tutorial39/tutorial39.html

(defclass edge ()
  ((n1 :initform -1 :reader edge-n1)
   (n2 :initform -1 :reader edge-n2)))
(defun make-edge ()
  (make-instance 'edge))
(defmethod print-object ((obj edge) stream)
  (with-slots (n1 n2) obj
    (print-unreadable-object (obj stream :type T)
      (format stream "~d ~d" n1 n2))))
(defmethod add-neighbor ((obj edge) new)
  (with-slots (n1 n2) obj
    (cond ((minusp n1) (setf n1 new) obj)
          ((minusp n2) (setf n2 new) obj)
          (t (error "no more space, we are crowded")))))
(defmethod get-other ((obj edge) this)
  (with-slots (n1 n2) obj
    (cond ((= this n1) n2)
          ((= this n2) n1)
          (t (error "no one else around")))))

(defun half-vec3 (vec3)
  "lose some precision, nineveh primitives need it at least
   when using find-adjacencies algorithm"
  (map '(SIMPLE-ARRAY SINGLE-FLOAT (3))
       (lambda (x) (parse-float:parse-float (format nil "~5f" x)))
       vec3))

(defun find-adjacencies (gpu-vertex gpu-indices)
  ;;Step 1 - find the two triangles that share every edge
  (let* ((vertices (pull-g gpu-vertex))
         (indices  (pull-g gpu-indices))
         ;; Index: is the vec3 pos
         ;; Key: is the indice of the vec3 pos
         (pos-map  (make-hash-table :test #'equalp))
         (idx-map  (make-hash-table :test #'equalp))
         (unique-faces (list))
         (face-id 0)
         (new-indices  (list)))
    (loop :for (i0 i1 i2) :on indices :by #'cdddr
          :for v0 := (half-vec3 (first (nth i0 vertices)))
          :for v1 := (half-vec3 (first (nth i1 vertices)))
          :for v2 := (half-vec3 (first (nth i2 vertices)))
          :for e0 := nil
          :for e1 := nil
          :for e2 := nil
          :when (and (not (v3:= v0 v1))
                     (not (v3:= v1 v2))
                     (not (v3:= v2 v0)))
          ;; NOTE: drop or might be can be triangulated?
          :do (setf i0 (or (gethash v0 pos-map)
                           (setf (gethash v0 pos-map) i0))
                    i1 (or (gethash v1 pos-map)
                           (setf (gethash v1 pos-map) i1))
                    i2 (or (gethash v2 pos-map)
                           (setf (gethash v2 pos-map) i2)))
              (push (list i0 i1 i2) unique-faces)
              (setf e0 (sort (list i0 i1) #'<)
                    e1 (sort (list i1 i2) #'<)
                    e2 (sort (list i2 i0) #'<))
              (setf (gethash e0 idx-map)
                    (add-neighbor (gethash e0 idx-map (make-edge)) face-id))
              (setf (gethash e1 idx-map)
                    (add-neighbor (gethash e1 idx-map (make-edge)) face-id))
              (setf (gethash e2 idx-map)
                    (add-neighbor (gethash e2 idx-map (make-edge)) face-id))
              (incf face-id))
    (setf unique-faces (reverse unique-faces))
    ;; Step 2 - build the index buffer with the adjacency info
    ;;#+nil
    (loop :for face :in unique-faces
          :for face-id :from 0
          :when (and (not (= -1 (edge-n1 (gethash (sort (list (elt face 0) (elt face 1)) #'<) idx-map (make-edge)))))
                     (not (= -1 (edge-n2 (gethash (sort (list (elt face 0) (elt face 1)) #'<) idx-map (make-edge)))))
                     (not (= -1 (edge-n1 (gethash (sort (list (elt face 1) (elt face 2)) #'<) idx-map (make-edge)))))
                     (not (= -1 (edge-n2 (gethash (sort (list (elt face 1) (elt face 2)) #'<) idx-map (make-edge)))))
                     (not (= -1 (edge-n1 (gethash (sort (list (elt face 2) (elt face 0)) #'<) idx-map (make-edge)))))
                     (not (= -1 (edge-n2 (gethash (sort (list (elt face 2) (elt face 0)) #'<) idx-map (make-edge))))))
          :do (loop :for i :from 0 :to 2
                    :for edge      := (sort (list (elt face i)
                                                  (elt face (mod (+ i 1) 3)))
                                            #'<)
                    :for neightbor := (gethash edge idx-map)
                    :for other     := (get-other neightbor face-id)
                    :do (when (not (minusp other))
                          (push (elt face i) new-indices)
                          (push (first
                                 (set-difference (elt unique-faces other)
                                                 face))
                                new-indices))))
    (list :idx-map idx-map
          :pos-map pos-map
          :unique-faces unique-faces
          :new-indices (reverse new-indices)
          :vertices (mapcar #'first vertices)
          ;;:uvertices (remove-duplicates (mapcar #'first vertices) :test #'equalp)
          :indices indices
          )))

(defun gpu-find-adjadencies (gpu-vertex gpu-indices)
  "returns a new index gpu array from provided one"
  (destructuring-bind (&key new-indices &allow-other-keys)
      (find-adjacencies gpu-vertex gpu-indices)
    (prog1 (make-gpu-array new-indices :element-type :ushort)
      (free gpu-indices))))
