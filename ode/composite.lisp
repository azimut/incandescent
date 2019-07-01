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
   (ode-rots :initarg :ode-rots)
   (bufs     :initarg :bufs)
   (display  :initarg :display)
   (massa    :initarg :massa))
  (:default-initargs
   :massa    (claw:alloc '%ode:mass)
   :display  t
   :bufs     (make-array 0 :fill-pointer 0 :adjustable t)
   :rots     (make-array 0 :fill-pointer 0 :adjustable t)
   :poss     (make-array 0 :fill-pointer 0 :adjustable t)
   ;;
   :geoms    (make-array 0 :fill-pointer 0 :adjustable t)
   :ode-rots (make-array 0 :fill-pointer 0 :adjustable t)))

(defmethod free ((object composite))
  (with-slots (geoms ode-rots) object
    (map 'vector #'claw:free ode-rots)
    (map 'vector #'free      geoms))
  (with-slots (body orot) object
    (%ode:body-destroy body)
    (claw:free         orot)))

(let ((stepper (make-stepper (seconds .1) (seconds .1))))
  (defmethod update ((actor composite) dt)
    ;;(when (funcall stepper))
    #+nil
    (with-slots (geoms orot pos rot) actor
      (setf pos (ode-geom-get-position (aref geoms 0)))
      (setf rot (ode-geom-get-quaternion2 orot (aref geoms 0)))
      #+nil
      (loop :for geom :across geoms
            :do (setf pos (ode-geom-get-position geom))
                (setf rot (ode-geom-get-quaternion2 orot geom))))))

(defmethod draw ((actor composite) camera time)
  (with-slots (geoms bufs rots poss orot) actor
    ;; update pos
    (loop :for geom :across geoms
          :for gpos := (ode-geom-get-position geom)
          :for grot := (ode-geom-get-quaternion2 orot geom)
          :for i :from 0
          :do (setf (aref poss i) gpos
                    (aref rots i) grot))
    ;; draw
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
  (declare (type rtg-math.types:quaternion qrot))
  (claw:c-let ((ode-rot %ode:real :ptr orot))
    (setf (ode-rot 0) (coerce (x qrot) 'double-float))
    (setf (ode-rot 1) (coerce (y qrot) 'double-float))
    (setf (ode-rot 2) (coerce (z qrot) 'double-float))
    (setf (ode-rot 3) (coerce (w qrot) 'double-float))))

(defmethod initialize-instance :after ((obj composite) &key)
  (with-slots (body mass geoms poss massa rot ode-rots rots orot bufs) obj
    ;;
    (update-ode-rot orot rot)
    (%ode:body-set-position body 0d0 5d0 0d0)
    (%ode:body-set-quaternion body orot)
    ;;
    (claw:c-let ((m  %ode:mass :ptr mass)
                 (m2 %ode:mass :ptr massa))
      (%ode:mass-set-zero (m2 &))
      (dotimes (i 2) ; For each geom
        (vector-push-extend (v! (+ -.15 (* .3 (random 1f0)))
                                (+ -.15 (* .3 (random 1f0)))
                                (+ -.15 (* .3 (random 1f0))))
                            poss)
        (vector-push-extend (box)
                            bufs)
        (vector-push-extend (%ode:create-box *space* 1d0 1d0 1d0)
                            geoms)
        (%ode:mass-set-box (m2 &) 1d0 1d0 1d0 1d0)
        ;;
        (vector-push-extend (claw:alloc '%ode:real 4)
                            ode-rots)
        (let ((quat (q:from-axis-angle
                     (v! (random 1f0) (random 1f0) (random 1f0))
                     (radians (random 360)))))
          (vector-push-extend quat rots)
          (update-ode-rot (aref ode-rots i) quat))
        (%ode:mass-rotate (m2 &) (aref ode-rots i))
        ;;#+nil
        (%ode:mass-translate (m2 &)
                             (coerce (x (aref poss i)) 'double-float)
                             (coerce (y (aref poss i)) 'double-float)
                             (coerce (z (aref poss i)) 'double-float))
        ;;
        (%ode:mass-add (m &) (m2 &))))
    ;; For each geom...after the mass is absolute i guess
    (claw:c-let ((m %ode:mass :ptr mass))
      (dotimes (k 2)
        (%ode:geom-set-body (aref geoms k) body)
        ;;#+nil
        (%ode:geom-set-offset-position (aref geoms k)
                                       (- (coerce (x (aref poss k)) 'double-float) (m :c 0))
                                       (- (coerce (y (aref poss k)) 'double-float) (m :c 1))
                                       (- (coerce (z (aref poss k)) 'double-float) (m :c 2)))
        (%ode:geom-set-offset-quaternion (aref geoms k) (aref ode-rots k)))
      ;;??
      ;;#+nil
      (%ode:mass-translate (m &) (- (m :c 0)) (- (m :c 1)) (- (m :c 2)))
      (%ode:body-set-mass body (m &)))))
