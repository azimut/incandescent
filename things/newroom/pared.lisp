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
                                        :buf (lattice 110 410 4 4 t))))
    (push obj *actors*)
    obj))

(let ((uv-repeat (v! 1 5))) (free-actors)
  ;;(make-clouds)
  (make-room-piso :pos (v! 0 -20 0)
                  :uv-repeat (v! 10 10)
                  :rot (q:from-axis-angle (v! 0 1 0) (radians 45)))
  (make-room-pared :rot (q:from-axis-angle (v! 0 0 1) (radians 90))
                   :pos (v! 20 30 0)
                   :uv-repeat uv-repeat)
  (make-room-pared :rot (q:from-axis-angle (v! 0 0 1) (radians -90))
                   :pos (v! -20 30 0)
                   :uv-repeat uv-repeat)
  (make-chair :pos (V! 0 -5 -15))
  (make-box :pos (v! 0 -2 0))
  (setf (pos *camera*) (v! -6.6046495 8.5207205 40.580578))
  (setf (rot *camera*) (v! 0.97301406 -0.12140777 -0.12544452 -0.15088868))
  #+nil
  (make-room-pared :rot (q:from-axis-angle (v! 0 0 1) (radians -180))
                   :pos (v! 0 50 0))
  )
