(in-package :incandescent)

;; Composite geometries are those whose body has more than 1 geometry

;; One way to position them is to give a world position to the body
;; instead of the geom and set offsets to the geoms instead.

;; TODO: I need an abstraction of some type for the whole parent children thing
;;       might be refactoring all ode?

(defclass composite (physic)
  ((geoms    :initarg :geoms
             :documentation "array with each geometry on the composite object")
   (rots     :initarg :rots)
   (poss     :initarg :poss)
   (bufs     :initarg :bufs)
   (display  :initarg :display)
   (sides    :initarg :sides)
   (ode-rots :initarg :ode-rots)
   (ode-qrots :initarg :ode-qrots)
   (massa    :initarg :massa)
   (stepper  :initarg :stepper))
  (:default-initargs
   :massa    (claw:alloc '%ode:mass)
   :display  t
   :bufs     (make-array 0 :fill-pointer 0 :adjustable t)
   :rots     (make-array 0 :fill-pointer 0 :adjustable t)
   :poss     (make-array 0 :fill-pointer 0 :adjustable t)
   :sides    (make-array 0 :fill-pointer 0 :adjustable t)
   ;;
   :stepper  (make-stepper (seconds .01) (seconds .01))
   ;;
   :geoms    (make-array 0 :fill-pointer 0 :adjustable t)
   :ode-rots (make-array 0 :fill-pointer 0 :adjustable t)
   :ode-qrots (make-array 0 :fill-pointer 0 :adjustable t)))

(defmethod free ((object composite))
  (with-slots (geoms ode-rots ode-qrots orot body) object
    (%ode:body-destroy body)
    (map 'vector #'%ode:geom-destroy geoms)
    (claw:free orot)
    (when ode-rots  (map 'vector #'claw:free ode-rots))
    (when ode-qrots (map 'vector #'claw:free ode-qrots))))

(defmethod update ((actor composite) dt)
  ;;#+nil
  (with-slots (geoms poss rots ode-rots orot stepper ode-qrots) actor
    (when (funcall stepper)
      (loop :for geom :across geoms
            :for i :from 0
            :for gpos := (ode-geom-get-position geom)
            :for grot := (ode-geom-get-quaternion2 (aref ode-qrots i) geom)
            :do (setf (aref poss i) gpos
                      (aref rots i) grot))))
  #+nil
  (with-slots (geoms orot pos rot) actor
    ;; (setf pos (ode-geom-get-position (aref geoms 0)))
    ;; (setf rot (ode-geom-get-quaternion2 orot (aref geoms 0)))
    #+nil
    (loop :for geom :across geoms
          :do (setf pos (ode-geom-get-position geom))
              (setf rot (ode-geom-get-quaternion2 orot geom)))))

(defmethod draw ((actor composite) camera time)
  ;;#+nil
  (with-slots (geoms bufs rots poss orot) actor
    (loop :for buf :across bufs
          :for rot :across rots
          :for pos :across poss
          :do (map-g #'generic-pipe buf
                     :scale 1f0
                     :color (v! 1 0 0)
                     ;;:shadowmap *shadow-sam*
                     ;; :light-world (world->view *shadow-camera*)
                     ;; :light-clip  (projection *shadow-camera*)
                     :cam-pos (pos camera)
                     :model-world (m4:* (m4:translation pos)
                                        (q:to-mat4 rot))
                     :world-view  (world->view camera)
                     :view-clip   (projection  camera)
                     ;; Directional light (for the most part)
                     :light-color *light-color*
                     :light-pos   *light-pos*))))

(defun update-ode-rot (orot qrot)
  "set the ODE rotation to the QROT rtg-math quaternion"
  (declare (type rtg-math.types:quaternion qrot))
  (claw:c-let ((ode-rot %ode:real :ptr orot))
    (setf (ode-rot 0) (coerce (x qrot) 'double-float))
    (setf (ode-rot 1) (coerce (x qrot) 'double-float))
    (setf (ode-rot 2) (coerce (x qrot) 'double-float))
    (setf (ode-rot 3) (coerce (x qrot) 'double-float))))

(defun update-ode-mrot (omrot qrot)
  "sets the ODE matrix rotation to the QROT quaternion value"
  (declare (type rtg-math.types:quaternion qrot))
  (let ((m3 (q:to-mat3 qrot)))
    (claw:c-let ((ode-mrot %ode:matrix3 :ptr omrot))
      (dotimes (i 9)
        (setf (ode-mrot i)
              (coerce (row-major-aref m3 i) 'double-float))))))

(defun make-composite (&key (pos (v! 0 5 0))
                            (rot (q:identity)))
  (let* (#+nil
         (poss  (vect (v! (+ -.15 (* .3 (random 1f0)))
                          (+ -.15 (* .3 (random 1f0)))
                          (+ -.15 (* .3 (random 1f0))))
                      (v! (+ -.15 (* .3 (random 1f0)))
                          (+ -.15 (* .3 (random 1f0)))
                          (+ -.15 (* .3 (random 1f0))))))
         (poss  (vect (v! 0 0 0) (v! 0 0 0)))
         ;;#+nil
         (rots  (vect (q:from-axis-angle
                       (v! (random 1f0) (random 1f0) (random 1f0))
                       (radians (random 360)))
                      (q:from-axis-angle
                       (v! (random 1f0) (random 1f0) (random 1f0))
                       (radians (random 360)))))
         ;;(rots  (vect (q:identity) (q:identity)))
         (sides (vect (v! (+ .1 (* .5 (random 1f0)))
                          (+ .1 (* .5 (random 1f0)))
                          (+ .1 (* .5 (random 1f0))))
                      (v! (+ .1 (* .5 (random 1f0)))
                          (+ .1 (* .5 (random 1f0)))
                          (+ .1 (* .5 (random 1f0))))))
         (bufs  (vect (box (x (aref sides 0))
                           (y (aref sides 0))
                           (z (aref sides 0)))
                      (box (x (aref sides 1))
                           (y (aref sides 1))
                           (z (aref sides 1))))))
    (assert (length= 2 poss rots bufs sides))
    (let ((obj (make-instance 'composite :pos pos :rot rot
                                         :poss poss :rots rots
                                         :bufs bufs :sides sides)))
      (push obj *actors*)
      obj)))

(defmethod initialize-instance :after ((obj composite) &key)
  (with-slots (body geoms mass massa
               poss pos
               rots rot
               ode-qrots ode-rots orot
               bufs sides)
      obj
    ;;
    (update-ode-rot orot rot)
    (%ode:body-set-position body
                            (coerce (x pos) 'double-float)
                            (coerce (y pos) 'double-float)
                            (coerce (z pos) 'double-float))
    (%ode:body-set-quaternion body orot)
    ;;
    (claw:c-let ((m  %ode:mass :ptr mass)
                 (m2 %ode:mass :ptr massa))
      (%ode:mass-set-zero (m &))
      (dotimes (k 2) ; For each geom
        ;; poss, bufs
        (vector-push-extend (%ode:create-box
                             *space*
                             (coerce (x (aref sides k)) 'double-float)
                             (coerce (y (aref sides k)) 'double-float)
                             (coerce (z (aref sides k)) 'double-float))
                            geoms)
        (%ode:mass-set-box (m2 &) 1d0
                           (coerce (x (aref sides k)) 'double-float)
                           (coerce (y (aref sides k)) 'double-float)
                           (coerce (z (aref sides k)) 'double-float))
        ;;
        (vector-push-extend (claw:alloc '%ode:matrix3) ode-rots)
        (vector-push-extend (claw:alloc '%ode:real 4)  ode-qrots)
        ;; rots
        ;;(update-ode-rot (aref ode-qrots k) (aref rots k))
        ;;(%ode:rfrom-q (aref ode-rots  k) (aref ode-qrots k))
        ;;(update-ode-mrot (aref ode-rots k) (aref rots k))
        (%ode:r-set-identity (aref ode-rots k))
        (%ode:mass-rotate (m2 &) (aref ode-rots k))
        ;;#+nil
        (%ode:mass-translate (m2 &)
                             (coerce (x (aref poss k)) 'double-float)
                             (coerce (y (aref poss k)) 'double-float)
                             (coerce (z (aref poss k)) 'double-float))
        ;;
        (%ode:mass-add (m &) (m2 &))))
    ;; For each geom...after the mass is absolute i guess
    (claw:c-let ((m %ode:mass :ptr mass))
      (dotimes (k 2)
        (%ode:geom-set-body (aref geoms k) body)
        ;;#+nil
        (%ode:geom-set-offset-position
         (aref geoms k)
         (- (coerce (x (aref poss k)) 'double-float) (m :c 0))
         (- (coerce (y (aref poss k)) 'double-float) (m :c 1))
         (- (coerce (z (aref poss k)) 'double-float) (m :c 2)))
        ;;(%ode:geom-set-offset-quaternion (aref geoms k) (aref ode-qrots k))
        (%ode:geom-set-rotation (aref geoms k) (aref ode-rots k))
        )
      ;;??
      ;;#+nil
      (%ode:mass-translate (m &) (- (m :c 0)) (- (m :c 1)) (- (m :c 2)))
      (%ode:body-set-mass body (m &)))))
