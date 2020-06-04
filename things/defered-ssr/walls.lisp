(in-package #:incandescent)

(defclass wall (actor)
  ((ao        :initarg :ao)
   (albedo    :initarg :albedo)
   (normal    :initarg :normal)
   (roughness :initarg :roughness)
   (specular  :initarg :specular)
   (disp      :initarg :disp)
   (metallic  :initarg :metallic)
   (uv-scale  :initarg :uv-scale))
  (:default-initargs
   :uv-scale  (v! 1 1)
   :metallic  0f0
   :ao        (get-tex "static/2k_wall/8/white_plaster_02_ao_2k.png"    NIL T :r8)
   :albedo    (get-tex "static/2k_wall/8/white_plaster_02_diff_2k.png"  NIL T :rgb8)
   :disp      (get-tex "static/2k_wall/8/white_plaster_02_disp_2k.png"  NIL T :r8)
   :normal    (get-tex "static/2k_wall/8/white_plaster_02_nor_2k.png"   NIL T :rgb8)
   :roughness (get-tex "static/2k_wall/8/white_plaster_02_rough_2k.png" NIL T :r8)
   :specular  (get-tex "static/2k_wall/8/white_plaster_02_spec_2k.png"  NIL T :r8)))

(defclass piso (wall)
  ()
  (:default-initargs
   :metallic  0f0
   :ao        (get-tex "static/_marble/marble_01_AO_2k.png"     NIL T :r8)
   :albedo    (get-tex "static/_marble/marble_01_diff_2k.png"   NIL T :rgb8)
   :disp      (get-tex "static/_marble/marble_01_disp2_2k.png"  NIL T :r8)
   :normal    (get-tex "static/_marble/marble_01_nor_2k.png"    NIL T :rgb8)
   :roughness (get-tex "static/_marble/marble_01_rough2_2k.png" NIL T :r8)
   :specular  (get-tex "static/_marble/marble_01_spec_2k.png"   NIL T :r8)))

(defmethod update ((actor wall) dt))

(defun make-wall (&key (uv-scale (v! 1 1))
                       (metallic  0f0)
                       (scale     1f0)
                       (pos       (v! 0 5 0))
                       (dim       (v! 1 1 1))
                       (rot       (q:identity)))
  (let ((obj (make-instance 'wall
                            :metallic metallic
                            :uv-scale uv-scale
                            :scale scale
                            :pos pos :rot rot
                            :buf (box (x dim) (y dim) (z dim) t))))
    (push obj *actors*)
    obj))

(defun make-piso (&key (uv-scale (v! 1 1))
                       (metallic  1f0)
                       (scale     1f0)
                       (pos       (v! 0 5 0))
                       (dim       (v! 1 1 1))
                       (rot       (q:identity)))
  (let ((obj (make-instance 'piso
                            :metallic metallic
                            :uv-scale uv-scale
                            :scale scale
                            :pos pos :rot rot
                            :buf (box (x dim) (y dim) (z dim) t))))
    (push obj *actors*)
    obj))

(defun-g wall-frag ((uv          :vec2)
                    (frag-normal :vec3)
                    (frag-pos    :vec3)
                    (tbn         :mat3)
                    (tlight-pos  :vec3)
                    (tcam-pos    :vec3)
                    (tfrag-pos   :vec3)
                    &uniform
                    (cam-pos     :vec3);disp
                    (metallic    :float)
                    (ao          :sampler-2d)
                    (albedo      :sampler-2d)
                    (disp        :sampler-2d)
                    (normals     :sampler-2d)
                    (roughness   :sampler-2d)
                    (specular    :sampler-2d))
  "defer"
  #+nil
  (let* ((uv (parallax-mapping-flipped
              uv
              (normalize (- tcam-pos tfrag-pos))
              disp
              .1)))
    ;; Parallax hack...
    #+nil
    (when (or (> (x uv) 2f0) (> (y uv) 2f0)
              (< (x uv) 0f0) (< (y uv) 0f0))
      (discard)))
  (let* ((emissive 0f0)
         (ao      (x (texture ao        uv)))
         (spec    (x (texture specular  uv)))
         (rough   (x (texture roughness uv)))
         ;;(normal   (norm-from-map normals uv frag-pos frag-normal))
         (normal  frag-normal)
         (albedo  (pow (s~ (texture albedo uv) :xyz) (vec3 2.2))))
    (values (v! albedo      rough)
            (v! frag-pos    ao)
            (v! normal      spec)
            (v! (* metallic spec) emissive))))

(defpipeline-g wall-pipe ()
  :vertex   (vert-with-tbdata g-pnt tb-data)
  :fragment (wall-frag :vec2 :vec3 :vec3
                             :mat3 :vec3 :vec3 :vec3))

(defmethod draw ((actor wall) camera time)
  "defer"
  (with-slots (buf scale disp uv-scale albedo normal roughness ao specular
               metallic)
      actor
    (map-g #'wall-pipe buf
           :scale scale
           :uv-repeat uv-scale
           :model-world (model->world actor)
           :world-view  (world->view camera)
           :view-clip   (projection  camera)
           ;;
           :metallic metallic
           :cam-pos (pos *currentcamera*)
           :ao ao
           :albedo albedo
           :disp disp
           :normals normal
           :roughness roughness
           :specular specular
           )))

;;--------------------------------------------------

(defun-g voxelize-tex-vert ((vert g-pnt)
                            &uniform
                            (scale       :float)
                            (light-vp    :mat4)
                            (model-world :mat4))
  (let ((wpos (* model-world (v! (* scale (pos vert)) 1f0)))
        (norm (norm vert)))
    (values wpos
            (normalize (* (m4:to-mat3 model-world) norm))
            (* light-vp wpos)
            (tex vert))))

;;--------------------------------------------------

(defun-g voxelize-tex-geom ((nor  (:vec3 3))
                            (lpos (:vec4 3))
                            (uvs  (:vec2 3)))
  (declare (output-primitive :kind :triangle-strip :max-vertices 3))
  (let* ((p1 (- (s~ (gl-position (aref gl-in 1)) :xyz)
                (s~ (gl-position (aref gl-in 0)) :xyz)))
         (p2 (- (s~ (gl-position (aref gl-in 2)) :xyz)
                (s~ (gl-position (aref gl-in 0)) :xyz)))
         (p  (abs (cross p1 p2)))
         (wp (vec3 0f0)))
    (dotimes (i 3)
      (setf wp (s~ (gl-position (aref gl-in i)) :xyz))
      (cond ((and (> (z p) (x p))
                  (> (z p) (y p)))
             (emit () (v! (x wp) (y wp) 0 1)
                   wp (aref nor i) (aref lpos i) (aref uvs i)))
            ((and (> (x p) (y p))
                  (> (x p) (z p)))
             (emit () (v! (y wp) (z wp) 0 1)
                   wp (aref nor i) (aref lpos i) (aref uvs i)))
            (t
             (emit () (v! (x wp) (z wp) 0 1)
                   wp (aref nor i) (aref lpos i) (aref uvs i)))))
    (emit-vertex)
    (end-primitive)))

;;--------------------------------------------------

(defun-g voxelize-tex-frag ((pos         :vec3)
                            (nor         :vec3)
                            (lpos        :vec4)
                            (uv          :vec2)
                            &uniform
                            (ithing      :image-3d)
                            (shadowmap   :sampler-2d)
                            (light-pos   :vec3)
                            (light-dir   :vec3)
                            (light-color :vec3)
                            (albedo      :sampler-2d)
                            (cone-inner  :float)
                            (cone-outer  :float))
  (if (not (inside-cube-p pos 0f0))
      (return))
  (let ((emissive 0f0)
        ;; FIXME: pow 2.2?
        (albedo   (pow (s~ (texture albedo uv) :xyz) (vec3 2.2)))
        (nor      (normalize nor))
        (vis      (shadow-factor-fast shadowmap lpos))
        (color    (v! 0 0 0)))
    ;; NT: we do not care about specular here, because that is view dependant
    (setf color (* vis (dir-light-apply albedo
                                        (* *cone-mult* light-color)
                                        light-pos
                                        pos
                                        nor)))
    (incf color (* albedo emissive))
    (let* ((voxel (scale-and-bias pos))
           (dim   (image-size ithing))
           (dxv   (* dim voxel))
           (res   (v! color 1))
           (coord (ivec3 (int (x dxv)) (int (y dxv)) (int (z dxv)))))
      (image-store ithing coord (v! (s~ res :xyz) (w res)))
      (values))))

(defpipeline-g voxelize-tex-pipe ()
  :vertex   (voxelize-tex-vert g-pnt)
  :geometry (voxelize-tex-geom (:vec3 3) (:vec4 3) (:vec2 3))
  :fragment (voxelize-tex-frag :vec3 :vec3 :vec4 :vec2))

(defmethod draw-voxel-actor ((actor wall))
  (with-slots (buf scale albedo voxelize-p draw-p) actor
    (when (and draw-p voxelize-p)
      (map-g #'voxelize-tex-pipe buf
             ;; - Vertex
             :scale scale
             :light-vp (world->clip *shadow-camera*)
             :model-world (model->world actor)
             ;; - Fragment
             :cone-inner (cos (radians *cone-inner*))
             :cone-outer (cos (radians *cone-outer*))
             :light-color *light-color*
             :light-pos *light-pos*
             :light-dir *light-dir*
             :albedo albedo
             :ithing *voxel-light-sam*
             :shadowmap *shadow-sam*))))
