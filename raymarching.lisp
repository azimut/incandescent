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

(defparameter *light-pos* (v! 100 100 100))
(defparameter *light-pos* (v! 100 1000 0))
(defparameter *light-pos* (v! 0 .3 -1))
(defparameter *light-pos* (v! 0 20 100))

(defclass ray-plane (actor) ())

(defun free-raymarching ()
  (when *ray-fbo* (free *ray-fbo*))
  (when *frustum* (free *frustum*))
  (when *quad-3d* (free *quad-3d*)))

(defun init-raymarching ()
  (free-raymarching)
  (setf *ray-fbo* (make-fbo `(0 :dimensions ,*dimensions*
                                :element-type :rgba16f))
        *ray-sam* (sample (attachment-tex *ray-fbo* 0)
                          :wrap :clamp-to-edge))
  (setf *frustum*
        (make-c-array (get-frustum-corners *currentcamera*)
                      :element-type :vec3 :dimensions 4))
  (setf *quad-3d*
        (let* ((verts (make-gpu-array
                       `((,(v! -1 -1 3) ,(v! 0 0))
                         (,(v!  1  1 1) ,(v! 1 1))
                         (,(v!  1 -1 2) ,(v! 1 0))
                         (,(v! -1  1 0) ,(v! 0 1)))
                       :dimensions 4 :element-type 'g-pt))
               (indi  (make-gpu-array '(0 1 2 1 0 3)
                                      :dimensions 6
                                      :element-type :unsigned-short))
               (buf   (make-buffer-stream verts
                                          :index-array indi
                                          :primitive :triangle-strip)))
          buf)))

(defun draw-raymarching (sam samd time)
  (declare (type single-float time)
           (type cepl:sampler sam samd))
  (with-setf* ((depth-test-function) #'always
               (cull-face) NIL
               (depth-mask) NIL)
    (with-fbo-bound (*ray-fbo*)
      (clear *ray-fbo*)
      (map-g #'raymarch-pipe *quad-3d*
             :samd samd
             :sam  sam
             :time time
             ;; :brdf-lut *s-brdf*
             ;; :irradiance-map *s-cubemap-prefilter*
             ;; :diffuse-map *s-cubemap-live*
             :cam-pos (pos *currentcamera*)
             :frustum-corners *frustum*
             :light-pos *light-pos*
             ;;:sam3 *32sam*
             :world-view
             (m4:*
              ;; (rtg-math.projection:orthographic-v2
              ;;  (v2! 1)
              ;;  .1f0
              ;;  10f0)
              (q:to-mat4 (q:inverse (rot *currentcamera*)))
              (q:to-mat4 (rot *currentcamera*)))
             :view-world (m4:inverse
                          (world->view *tmp2*))))))

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

           (piso (f-sphere orig 2))
           )
      ;;(min piso dd)
      dd
      )))

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
         (ret (v! 1 1 1 0))
         ;; Geometry
         (center (v! 0 0 0))
         (radius (+ 1 (sin (* .01 time)))))
    (dotimes (i 60)
      (when (or (>= tt s) (> tt 100f0))
        (setf ret (v! 0 1 1 0))
        (break))
      (let* ((p (+ ro (* rd tt)))
             (d (distance-estimator p center radius)))
        (when (< d .0001)
          (setf ret
                (v! (+ (v! .01 .01 .01)
                       (s~
                        (render-surface p center radius (- light-pos))
                        ;; (render-pbr p
                        ;;             center
                        ;;             time
                        ;;             cam-pos light-pos
                        ;;             brdf-lut irradiance-map diffuse-map)
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
(defun-g raymarch-vert ((vert g-pt) ;;(pos :vec3)
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
                        (sam3 :sampler-3d)
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
         (depth (* depth (length ray)))
         (color (s~ (texture sam uv) :xyz))
         (add   (raymarch ro rd depth time
                          cam-pos light-pos
                          brdf-lut
                          irradiance-map
                          diffuse-map
                          ;;sam3
                          )))
    ;; return fixed4(col*(1.0 - add.w) + add.xyz * add.w,1.0);
    (v! (+ (* color         (- 1 (w add)))
           (* (s~ add :xyz) (w add)))
        1)
    ;;add
    ;;ray
    ;;(v3! depth)
    ;;(v! 1 0 1 0)
    ))

;; (defun-g raymarch ((ro :vec3)
;;                    (rd :vec3)
;;                    (s :float)
;;                    (time :float)
;;                    (cam-pos :vec3)
;;                    (light-pos :vec3)
;;                    (brdf-lut :sampler-2d)
;;                    (irradiance-map :sampler-cube)
;;                    (diffuse-map :sampler-cube)
;;                    (sam3 :sampler-3d))
;;   (let* ((tt 0f0)
;;          (ret (v! 0 0 0 0))
;;          ;; Geometry
;;          (center (v! 0 0 0)))
;;     (dotimes (i 4)
;;       (when (or (>= tt s)
;;                 (> tt 40f0))
;;         (setf ret
;;               (v! 0 0 0 0)
;;               )
;;         (break))
;;       (let* ((p (+ ro (* rd tt)))
;;              (d (distance-estimator p
;;                                     center
;;                                     time
;;                                     sam3)))
;;         (when (< d .01)
;;           (setf ret
;;                 (v!
;;                  (+
;;                   (v! .1 .1 .1)
;;                   (s~
;;                    (render-surface p
;;                                    center
;;                                    1
;;                                    (- light-pos))
;;                    :xyz)
;;                   ;; (s~
;;                   ;;  (render-pbr p center time
;;                   ;;              cam-pos light-pos
;;                   ;;              brdf-lut
;;                   ;;              irradiance-map
;;                   ;;              diffuse-map)
;;                   ;;  :xyz)
;;                   ;;(v3! (get-density p sam3 time))
;;                   )
;;                  ;;(s~ (texture sam3 rd) :xyz)
;;                  1))
;;           (break))
;;         (incf tt d)))
;;     ret))

(defun-g distance-estimator ((p :vec3)
                             (c :vec3)
                             (r :float)
                             ;;(sam3 :sampler-3d)
                             )
  ;;(min)
  ;;(x (texture sam3 p))
  (f-sphere (- p c) .4)
  ;;(f-box p (v! 1 1 1))
  )

(defpipeline-g raymarch-pipe (:triangle-strip)
  :vertex   (raymarch-vert g-pt)
  :fragment (raymarch-frag :vec2 :vec3))
