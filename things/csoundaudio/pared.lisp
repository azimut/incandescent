(in-package :incandescent)

(defclass room-pared (room-piso)
  ((albedo    :initarg :albedo)
   (ao        :initarg :ao)
   (height    :initarg :height)
   (normal    :initarg :normal)
   (roughness :initarg :roughness)
   ;;
   (uv-repeat :initarg :uv-repeat)
   (uv-speed  :initarg :uv-speed)
   ;;
   (metallic  :initarg :metallic)
   (parallax  :initarg :parallax)
   (specular  :initarg :specular))
  (:default-initargs
   :uv-repeat (v! 1 1)
   :uv-speed  .1
   :metallic  .1
   :parallax  .03
   :specular  (get-tex "static/2k_wall/8/white_plaster_02_spec_2k.png"  NIL T :r8)
   :albedo    (get-tex "static/2k_wall/8/white_plaster_02_diff_2k.png"  NIL T :rgb8)
   :ao        (get-tex "static/2k_wall/8/white_plaster_02_ao_2k.png"    NIL T :r8)
   :height    (get-tex "static/2k_wall/8/white_plaster_02_disp_2k.png"  NIL T :r8)
   :normal    (get-tex "static/2k_wall/8/white_plaster_02_nor_2k.png"   NIL T :rgb8)
   :roughness (get-tex "static/2k_wall/8/white_plaster_02_rough_2k.png" NIL T :r8)))

(defun make-room-pared (&key (pos (v! 0 0 0))
                             (rot (q:identity))
                             (uv-repeat (v! 1 1)))
  (declare (type rtg-math.types:vec2 uv-repeat)
           (type rtg-math.types:quaternion rot)
           (type rtg-math.types:vec3 pos))
  (let ((obj (make-instance 'room-pared :pos pos :rot rot
                                        :uv-repeat uv-repeat
                                        :buf (lattice 100 100 4 4 t))))
    (push obj *actors*)
    obj))



;;--------------------------------------------------
;; CREATES A 100x100 room
#+nil
(let ((uv-repeat (v! 3 3))
      (room-dim  (v! 2 3 50)))
  (free-actors)
  (make-room-piso :pos (v! 0 0 0) :uv-repeat (v! 55 55))
  ;;
  (make-room-pared :rot (q:from-axis-angle (v! 0 0 1) (radians 90))
                   :pos (v! (+ (* .5 (x room-dim))) 0 0)
                   :uv-repeat uv-repeat)
  (make-room-pared :rot (q:from-axis-angle (v! 0 0 1) (radians -90))
                   :pos (v! (- (* .5 (x room-dim))) 0 0)
                   :uv-repeat uv-repeat)
  ;;
  (make-room-pared :rot (q:from-axis-angle (v! 1 0 0) (radians 90))
                   :pos (v! 0 0 (- (* .5 (z room-dim))))
                   :uv-repeat uv-repeat)
  (make-room-pared :rot (q:from-axis-angle (v! 1 0 0) (radians -90))
                   :pos (v! 0 0 (+ (* .5 (z room-dim))))
                   :uv-repeat uv-repeat)
  ;;
  (make-room-pared :rot (q:from-axis-angle (v! 1 0 0) (radians (+ 90 90)))
                   :pos (v! 0 (y room-dim) 0)
                   :uv-repeat uv-repeat)
  (make-box :pos (v! 0 2.9 0) :x .1 :y .1 :z .1 :color *light-color*)
  ;;(make-box :pos (v! 0 2.5 0) :x 1 :y 1 :z 1)
  ;;(make-pbr-simple :pos (v! 0 2.5 0))
  )
(setf *light-color* (nth 1 *sample-colors*))



