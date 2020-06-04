(in-package #:incandescent)

;; So I have the precompute of rays positions and directions...
;; but nothing of the raytracing per se...I need to reread the paper
;; and/or wait for complete code samples

;; - 6x6 is in reality 4x4 with a texel around it
;; - naive grid placement is fine, but there are clever ways
;; - can use compute shaders for raytracing instead of rasterization
;; - probe memory can be an issue
;; - can be combined with voxelized for a more cheap raytracing

(defvar *adepth* nil "visibility")
(defvar *airrad* nil "HDR irradiance")

(defvar *rorigin* nil)
(defvar *rdirect* nil)
(defvar *rfbo*    nil)
(defvar *rsam0*    nil)
(defvar *rsam1*    nil)

(defparameter *irradiance-rays-per-probe* 64f0
  "Number of rays emitted each frame for each probe in the scene")
(defvar *ddgi-fbo* nil)
(defvar *ddgi-irradiance-tex* nil)
(defvar *ddgi-irradiange-tex* nil)
(defparameter *probe-counts* (v! 4 2 4))
(defparameter *probe-step* nil)
(defparameter *probe-start-position* nil)
(defun probe-counts ()
  (floor (* (x *probe-counts*)
            (y *probe-counts*)
            (z *probe-counts*))))
(defun free-ddgi ()
  (when *rorigin* (free *rorigin*))
  (when *rdirect* (free *rdirect*))
  (when *airrad*  (free *airrad*))
  (when *adepth*  (free *adepth*))
  (when *rfbo*    (free *rfbo*)))
(defun init-ddgi ()
  (free-ddgi)
  (assert (= 0 (mod (probe-counts) 2)))
  (setf *rorigin* (make-texture nil :element-type :rgba32f
                                    :dimensions (list (floor
                                                       *irradiance-rays-per-probe*)
                                                      (probe-counts))))
  (setf *rdirect* (make-texture nil :element-type :rgba32f
                                    :dimensions (list (floor
                                                       *irradiance-rays-per-probe*)
                                                      (probe-counts))))
  (setf *rfbo* (make-fbo `(0 ,*rorigin*) `(1 ,*rdirect*)))
  (setf *rsam0* (sample (attachment-tex *rfbo* 0)))
  (setf *rsam1* (sample (attachment-tex *rfbo* 1)))
  (let* ((lo (v! 0 0 0))
         (hi (v! 1 1 1))
         (probe-step (v3:/ (v3:- hi lo)
                           (v3:- *probe-counts* (v! 1 1 1)))))
    (setf *probe-step*
          (v! (max (x probe-step) 1)
              (max (y probe-step) 1)
              (max (z probe-step) 1)))
    (setf *probe-start-position*
          (v! (if (= 1 (x *probe-counts*))
                  (/ (+ (x hi) (x lo)) 2f0)
                  (x lo))
              (if (= 1 (y *probe-counts*))
                  (/ (+ (y hi) (y lo)) 2f0)
                  (y lo))
              (if (= 1 (z *probe-counts*))
                  (/ (+ (z hi) (z lo)) 2f0)
                  (z lo)))))
  #+nil
  (setf *airrad* (make-texture nil :cubes t
                                   :element-type :rgb16f
                                   :mipmap nil
                                   :dimensions '(6 6)))
  #+nil
  (setf *adepth* (make-texture nil :cubes t
                                   :element-type :rg16f
                                   :mipmap nil
                                   :dimensions '(16 16))))

(defun-g grid-coord-to-position ((c                    :vec3)
                                 (probe-start-position :vec3)
                                 (probe-step           :vec3))
  "Compute 3D worldspace position from gridcoord based on starting
  location and distance between probes."
  (+ probe-start-position (* probe-step c)))

;; TODO: use performant version without divide
(defun-g probe-index-to-grid-coord ((index :int)
                                    (probe-counts :vec3))
  "Compute the grid coordinate of the probe from the index"
  (let ((ipos (ivec3 0 0 0)))
    (setf (x ipos) (int (mod index (x probe-counts))) )
    (setf (y ipos) (int (/ (mod index (* (x probe-counts) (y probe-counts))) (x probe-counts))) )
    (setf (z ipos) (int (/ index (* (x probe-counts) (y probe-counts)))) )
    ipos))


(defun-g probe-location ((id                   :int)
                         (probe-counts         :vec3)
                         (probe-start-position :vec3)
                         (probe-step           :vec3))
  "Compute the 3D probe location in world space from the probe index"
  (grid-coord-to-position (probe-index-to-grid-coord id probe-counts)
                          probe-start-position
                          probe-step))

(defun-g spherical-fibonacci ((i :float)
                              (n :float))
  "Generate a spherical fibonacci point

     http://lgdv.cs.fau.de/publications/publication/Pub.2015.tech.IMMD.IMMD9.spheri/

     To generate a nearly uniform point distribution on the unit sphere of size N, do
     for (float i = 0.0; i < N; i += 1.0) {
       float3 point = sphericalFibonacci(i,N);
     }

     The points go from z = +1 down to z = -1 in a spiral. To generate samples on the +z hemisphere,
     just stop before i > N/2."
  (labels ((madfrac ((a :float) (b :float))
             (- (* a b) (floor (* a b)))))
    ;;
    (let* ((phi #.(+ .5 (* .5 (sqrt 5))))
           (phi (* 2f0 +pi+ (madfrac i (- phi 1f0))))
           (cos-theta (- 1 (* (+ 1 (* 2 i)) (/ n))))
           (sin-theta (sqrt (saturate (- 1f0 (* cos-theta cos-theta))))))
      (v! (* (cos phi) sin-theta)
          (* (sin phi) sin-theta)
          cos-theta))))

(defun-g generate-rays-frag ((uv :vec2)
                             &uniform
                             (probe-start-position :vec3)
                             (probe-step           :vec3)
                             (probe-counts         :vec3)
                             (rays-per-probe       :float)
                             (random-orientation   :mat3))
  (let* ((pixel-coord (v! (x gl-frag-coord)
                          (y gl-frag-coord)))
         (probe-id    (int (x pixel-coord)))
         (ray-id      (y pixel-coord))
         ;; This value should be on the order of the normal bias.
         (ray-min-distance .08))
    (values (v! (probe-location probe-id
                                probe-counts
                                probe-start-position
                                probe-step)
                ray-min-distance)
            (v! (* random-orientation
                   (spherical-fibonacci ray-id rays-per-probe))
                1))))

(defpipeline-g generate-rays-pipe (:points)
  :fragment (generate-rays-frag :vec2))

(defun draw-ddgi ()
  (with-fbo-bound (*rfbo*)
    (map-g #'generate-rays-pipe *bs*
           :probe-start-position *probe-start-position*
           :probe-step *probe-step*
           :probe-counts *probe-counts*
           :rays-per-probe *irradiance-rays-per-probe*
           :random-orientation (m3:rotation-from-axis-angle
                                (v! (random 1f0) (random 1f0) (random 1f0))
                                (radians (random 360f0))))))
