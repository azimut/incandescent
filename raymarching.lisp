(in-package #:incandescent)

;; Raymarching as postprocess, to integrate it with a normal
;; rasterized scene.

;; INPUTS:
;; - *dimensions*
;; - *samd*
;; - *sam*


;; VARJO (v:-) should negate the single vector
;;       up/down/right/left unity like for (v! 0 1 0) et al
;;       setf for m4:get-row // meh!
;;       for() support global vars
;;       missing MOD vec3/vec3
;;       missing -- in (for)
;;       missing HLSL fmod()
;;       missing e definition
(defvar *quad-3d* NIL "BUFFER-STREAM that has the geometry for the quad")
(defvar *frustum* NIL "C-ARRAY that has the frustum corners???")

(defvar *ray-fbo* NIL)
(defvar *ray-sam* NIL)

(defvar *max-steps* 10f0)
(defvar *min-distance* .001)
(defvar *max-distance* 1000f0)

(defparameter *light-pos* (v! 100 100 100))
(defparameter *light-pos* (v! 100 1000 0))
(defparameter *light-pos* (v! 0 .3 -1))


(defclass ray-plane (actor) ())

(defmethod update ((actor ray-plane))
  ;;(setf (pos actor) (pos *currentcamera*))
  (setf (rot actor) (rot *currentcamera*))
  )

(defun init-raymarching ()
  (when *ray-fbo* (free *ray-fbo*))
  (setf *ray-fbo* (make-fbo `(0 :dimensions ,*dimensions*
                                :element-type :rgba16f))
        *ray-sam* (sample (attachment-tex *ray-fbo* 0)
                          :wrap :clamp-to-edge))
  (unless *frustum*
    (setf *frustum*
          (make-c-array (get-frustum-corners *currentcamera*)
                        :element-type :vec3 :dimensions 4)))
  (unless *quad-3d*
    ;; 1 0 0 // BL ;; 0 1 0 // TR ;; 0 0 1 // BR ;; 0 0 0 // TL
    (setf *quad-3d*
          (let* ((verts (make-gpu-array
                         `((,(v! -1 -1 3) ,(v! 1 0 0) ,(v! 0 0))
                           (,(v!  1  1 1) ,(v! 0 1 0) ,(v! 1 1))
                           (,(v!  1 -1 2) ,(v! 0 0 1) ,(v! 1 0))
                           (,(v! -1  1 0) ,(v! 0 0 0) ,(v! 0 1)))
                         :dimensions 4 :element-type 'g-pnt))
                 (indi  (make-gpu-array '(0 1 2 1 0 3)
                                        :dimensions 6

                                        :element-type :unsigned-short))
                 (buf   (make-buffer-stream verts
                                            :index-array indi
                                            :primitive :triangle-strip)))
            buf))))

(defvar *tmp2* (make-instance 'ray-plane))
(defvar *tmp3* (make-instance 'actor))

(defun draw-raymarching (time)
  (declare (type single-float time))
  (update *tmp2*)
  (with-fbo-bound (*ray-fbo*)
    (clear *ray-fbo*)
    (map-g #'raymarch-pipe *quad-3d*
           :samd *samd*
           :sam  *sam*
           :time time
           ;; :brdf-lut *s-brdf*
           ;; :irradiance-map *s-cubemap-prefilter*
           ;; :diffuse-map *s-cubemap-live*
           :cam-pos (pos *currentcamera*)
           :frustum-corners *frustum*
           :light-pos *light-pos*
           :world-view (m4:*
                        ;;(projection *currentcamera*)
                        (let ((fs (or (frame-size *currentcamera*)
                                      (viewport-resolution (current-viewport)))))
                          (rtg-math.projection:orthographic-v2
                           (v2! 1)
                           .1
                           1000f0))
                        (world->view *tmp2*
                                     ;;*currentcamera*
                                     )
                        ;;(world->view *currentcamera*)
                                        ;,(m4:identity)
                        (model->world *tmp2*)
                        ;;(model->world *currentcamera*)
                        )
           :view-world (m4:inverse
                        (m4:*
                         ;;(projection *currentcamera*)
                         (world->view *tmp2*;;*currentcamera*
                                      )
                         ;;(model->world *currentcamera*)
                         ;;(model->world *tmp2*)
                         )
                        ))))

;;--------------------------------------------------

(defun-g render-pbr ((p :vec3)
                     (center :vec3)
                     (radius :float)
                     (cam-pos :vec3)
                     (light-pos :vec3))
  (let* ((n  (calc-normal p center radius))
         (v  (normalize (- p cam-pos)))
         (ao 1)
         ;;(ao (ambient-occlussion p n .7 2 center radius))
         (l  (pbr-direct-lum light-pos
                             p
                             v
                             n
                             .1
                             (v3! .4)
                             .1
                             (v! .1 .1 .1))))
    (v! (* l ao) 0)))

;;--------------------------------------------------
(defun-g p-r ((p :vec2) (a :float))
  ;;cos(a)*p + sin(a)*vec2(p.y, -p.x);
  (+ (* p (cos a)) (* (sin a) (v! (y p) (- (x p))))))

(defun-g distance-estimator ((p :vec3)
                             (c :vec3)
                             (r :float))
  (let* ((p (- p c))
         (orig p)
         ;;(p (v! (x p) (/ (y p) 2) (z p)))
         (p (v! (+ 10 (x p)) (y p) (z p)))
         ;;(p (* (m3:rotation-x (radians 30)) p))
         ;;(p (v! (x p) (y p) (+ (mod (* .1 r) 10) (z p))))
         (rp (p-mod2 (v! (x p) (z p)) (v! 4 4)))
         (p (v! (x rp) (y p) (y rp)))
         )
    (let* (
           (d (f-op-engrave (f-box p (v! .4 3 .4))
                            (f-box p (v! 1  1 1))
                            .3))

           (dd (min (f-box-2-cheap (v! (x p) (y p)) (v! .1 .1)) d))
           (piso (f-plane p (v! 0 1 0) 2))
           ;;(orig (v! (x orig) (y orig) (p-mod1 (z orig) 10)))

           ;;(piso (f-sphere orig 2))
           )
      (min piso dd))))

(defun-g p-mod1 ((p :float) (size :float))
  (let ((half (* size .5)))
    (- (fmod (+ p half) size) half)))
(defun-g distance-estimator ((p :vec3) (c :vec3) (r :float))
  (let* ((dist 4f0)
         (p (- p c))
         ;;(new-radius (+ 1 (* 2 (sin (/ (x p) dist)))))
         ;;(new-radius (+ 1 (* 2 (sin (/ (x p) dist)))))
         (new-radius (+ 1 (* r (sin (/ (x p) dist)))))
         ;;(new-radius r)
         (p (v! (- (mod (+ (* .5 dist) (x p)) dist)
                   (* .5 dist))
                (- (mod (+ (* .5 dist) (y p)) dist)
                   (* .5 dist))
                ;; (- (mod (+ (* .5 dist) (z p)) dist)
                ;;    (* .5 dist))
                (z p)
                )))
    (- (length (- c p)) new-radius)))

(defun-g raymarch ((ro :vec3)
                   (rd :vec3)
                   (s :float)
                   (time :float)
                   (cam-pos :vec3)
                   (light-pos :vec3)
                   (brdf-lut :sampler-2d)
                   (irradiance-map :sampler-cube)
                   (diffuse-map :sampler-cube))
  (let* ((tt 0f0)
         (ret (v! 0 0 0 0))
         ;; Geometry
         (center (v! 0 0 -10))
         (radius (+ 1 (sin (* .01 time)))))
    (dotimes (i 100)
      (when (or (>= tt s)
                (> tt 140f0))
        (setf ret (v! 0 0 0 0))
        (break))
      (let* ((p (+ ro (* rd tt)))
             (d (distance-estimator p center time)))
        (when (< d .01)
          (setf ret
                (v! (+ ;;(v! .01 .003 .001)
                     (s~ ;;(render-surface p center radius (- light-pos))
                      (render-pbr p center time cam-pos light-pos brdf-lut irradiance-map diffuse-map)
                      :xyz)
                     ;;(let* ((n (calc-normal p center radius))))
                     )
                    1))
          (break))
        (incf tt d)))
    ;;(setf ret (v! (sin (y p)) 0 0 0))
    ret
    ))

;;--------------------------------------------------
;; PIPELINE
;;

;; https://github.com/Flafla2/Generic-Raymarch-Unity/blob/master/Assets/RaymarchGeneric.shader
(defun-g raymarch-vert ((vert g-pnt) ;;(pos :vec3)
                        &uniform
                        (world-view :mat4)
                        (view-world :mat4)
                        (time :float)
                        (frustum-corners (:vec3 4)))
  (let* ((pos      (pos vert))
         (index    (int (z pos)))
         (pos      (v!  (x pos) (y pos) .1))
         (view-pos (* world-view (v! pos 1)))
         ;; get the eyespace view ray (normalized)
         (ray (s~ (aref frustum-corners index)
                  :xyz))
         ;; Dividing by z "normalizes" it in the z axis
	 ;; Therefore multiplying the ray by some number i gives the viewspace position
	 ;; of the point on the ray with [viewspace z]=i
         (ray (/ ray (abs (z ray))))
         ;; Transform the ray from eyespace to worldspace
         (ray (s~ (* view-world (v! ray 1)) :xyz)))
    (values view-pos
            (tex vert)
            ray)))

(defun-g raymarch-frag ((uv :vec2)
                        (ray :vec3)
                        &uniform
                        (res :vec2)
                        (time :float)
                        (brdf-lut :sampler-2d)
                        (irradiance-map :sampler-cube)
                        (diffuse-map :sampler-cube)
                        (light-pos :vec3)
                        (sam  :sampler-2d)
                        (samd :sampler-2d)
                        (cam-pos :vec3))
  (let* ((rd    (normalize ray))
         (ro    cam-pos)
         (depth (linear-eye-depth (x (texture samd uv))))
         (depth (* .06 depth (length ray)))
         (color (s~ (texture sam uv) :xyz))
         (add   (raymarch ro rd depth time cam-pos light-pos
                          brdf-lut irradiance-map diffuse-map)))
    ;; return fixed4(col*(1.0 - add.w) + add.xyz * add.w,1.0);
    (v! (+ (* color         (- 1 (w add)))
           (* (s~ add :xyz) (w add)))
        1)
    ;;add
    ;;ray
    ;;(v3! depth)
    ;;(v! 1 0 1 0)
    ))

(defpipeline-g raymarch-pipe (:triangle-strip)
  :vertex   (raymarch-vert g-pnt;;:vec3
                           )
  :fragment (raymarch-frag :vec2 :vec3))
