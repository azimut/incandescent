(in-package :incandescent)

(defvar *p3* nil)

(defun init-renderman ()
  (init-audio)
  (init-text)
  (setf *exposure* 1f0)
  ;; Wake up camera shot
  (reset-camera *camera* (v! 0 2 40))
  ;; (setf (shots *camera*)
  ;;       (vect
  ;;        (vect
  ;;         (list (v! 0.0 2.0 40.0) (q! 0.8526403 -0.52249855 0.0 0.0) 0)
  ;;         (list (v! 0.0 2.0 40.0) (q! 0.99002373 -0.14090128 0.0 0.0) 0.3)
  ;;         (list (v! 0.0 10.2672105 38.46973) (q! 0.9408808 -0.33873796 0.0 0.0) 0.6)
  ;;         (list (v! 0.0 10.2672105 38.46973) (q! 0.99950665 -0.03141089 0.0 0.0) 0.8)
  ;;         (list (v! 0.0 10.2672105 38.46973) (q! 0.8758744 -0.027525585 0.48151588 -0.015132314) 1.2)
  ;;         (list (v! 0.0 10.2672105 38.46973) (q! 0.99938333 -0.03140703 0.015699469 -4.9337506e-4) 1.5)
  ;;         (list (v! 0.0 10.2672105 38.46973) (q! 0.89758444 -0.02820786 -0.43972206 0.013818893) 1.8)
  ;;         (list (v! 0.0 10.2672105 38.46973) (q! 0.93816864 -0.3458059 -0.013729579 -0.008325609) 2))))
  ;; ;; Animate shot
  ;; (shot-on)
  ;;--------------------------------------------------
  ;;(free-assimp-buffers)
  ;;(make-text "It is cold outside")
  (dotimes (i 1)
    (make-grass :pos (v! (random-in-range -60f0 60f0)
                         0
                         (random-in-range -60f0 60f0))
                :scale (random-in-range 10f0 15f0)))
  ;; make-piso: after grass and fog!
  #+nil
  (make-dirt :buf (lattice 300 700 2 2 t)
             :uv-repeat (v2:*s (v! 3 7) 4f0))
  #+nil
  (dotimes (i 10)
    (let ((*processing-flags*
            '(:ai-process-triangulate
              :ai-process-flip-u-vs
              :ai-process-preset-target-realtime-quality
              :ai-process-calc-tangent-space)))
      (mapcar (lambda (mesh) (push mesh *actors*))
              (assimp-load-meshes
               (resolve-path "static/tree/tree_obj.obj")
               :scale (serapeum:random-in-range .1 .25)
               :pos (v! (serapeum:random-in-range -100f0 100f0)
                        0
                        (serapeum:random-in-range -300f0 -40f0))
               :rot (q:from-axis-angle (v! 0 1 0)
                                       (radians (random 360)))))))
  ;; hazmat "/home/sendai/Downloads/scpcb-master/GFX/items/hazmat.b3d"
  ;; (let ((*processing-flags*
  ;;         (cons :ai-process-preset-target-realtime-max-quality
  ;;               *processing-flags*)))
  ;;   (mapcar (lambda (mesh) (push mesh *actors*))
  ;;           (cdr
  ;;            (assimp-load-meshes
  ;;             (resolve-path "static/173box/173box.b3d")
  ;;             :pos (v! 0 -5 30)
  ;;             :rot (q:from-axis-angle (v! 0 1 0) (radians 270))
  ;;             :scale 1f0))))
  ;;(make-radio)
  ;;(make-book (v! 0 0 -250))
  )


(defun draw-renderman ())
