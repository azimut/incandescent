(in-package #:incandescent)

;; Reference:
;; - https://learnopengl.com/Advanced-Lighting/Shadows/Point-Shadows
;; - https://github.com/cbaggers/cepl/issues/320

(defvar *t-shadow-cube* nil)
(defvar *s-shadow-cube* nil)
(defvar *point-mats*    nil)
(defvar *far-plane*     50f0)

(defun free-pointlight ()
  (when *t-shadow-cube* (free *t-shadow-cube*))
  (when *shadow-fbo*    (free *shadow-fbo*))
  (when *point-mats*    (free *point-mats*)))

(defun init-pointlight-mat (light-pos)
  (list
   (m4:* (rtg-math.projection:perspective 1f0 1f0 .1f0 *far-plane* 90f0) (m4:look-at (v! 0 -1  0) light-pos (v3:+ light-pos (v!  1  0  0))))
   (m4:* (rtg-math.projection:perspective 1f0 1f0 .1f0 *far-plane* 90f0) (m4:look-at (v! 0 -1  0) light-pos (v3:+ light-pos (v! -1  0  0))))
   (m4:* (rtg-math.projection:perspective 1f0 1f0 .1f0 *far-plane* 90f0) (m4:look-at (v! 0  0  1) light-pos (v3:+ light-pos (v!  0  1  0))))
   (m4:* (rtg-math.projection:perspective 1f0 1f0 .1f0 *far-plane* 90f0) (m4:look-at (v! 0  0 -1) light-pos (v3:+ light-pos (v!  0 -1  0))))
   (m4:* (rtg-math.projection:perspective 1f0 1f0 .1f0 *far-plane* 90f0) (m4:look-at (v! 0 -1  0) light-pos (v3:+ light-pos (v!  0  0  1))))
   (m4:* (rtg-math.projection:perspective 1f0 1f0 .1f0 *far-plane* 90f0) (m4:look-at (v! 0 -1  0) light-pos (v3:+ light-pos (v!  0  0 -1))))))

(defun init-pointlight ()
  (free-pointlight)
  (setf *shadow-fbo*    (make-fbo `(:d :dimensions ,*shadow-dimensions*)))
  (setf *t-shadow-cube* (make-texture nil :dimensions *shadow-dimensions*
                                          :element-type :depth-component24
                                          :cubes t))
  (setf *s-shadow-cube* (sample *t-shadow-cube* :minify-filter  :nearest
                                                :magnify-filter :nearest
                                                :wrap           :clamp-to-edge))
  (setf *point-mats*    (make-c-array (init-pointlight-mat *light-pos*)
                                      :dimensions 6
                                      :element-type :mat4))
  (setf (attachment *shadow-fbo* :d) (texref *t-shadow-cube* :layer nil))
  t)

#+nil
(progn
  (with-fbo-bound (*shadow-fbo* :attachment-for-size :d))
  (clear-fbo *shadow-fbo*)
  (%gl:framebuffer-texture :read-framebuffer
                           (cepl.fbos::gl-enum :depth-attachment)
                           (texture-id *t-shadow-cube*)
                           0)
  (cepl.fbos::%update-fbo-state))

;; NOTE: needs patched cbaggers/glsl-spec to make gl-layer a "place"
(defun-g shadowmap-point-vert ((vert g-pnt) &uniform (model->world :mat4))
  (* model->world (v! (pos vert) 1)))

(defun-g shadowmap-point-geom (&uniform (shadow-matrices (:mat4 6)))
  (declare (output-primitive :kind :triangle-strip
                             :max-vertices 18))
  (dotimes (face 6)
    (setf gl-layer face)
    (dotimes (i 3)
      (let ((pos (gl-position (aref gl-in i))))
        (emit ()
          (* (aref shadow-matrices face) pos)
          pos)))
    (end-primitive))
  (values))

(defun-g shadowmap-point-frag ((frag-pos :vec4) &uniform (light-pos :vec3) (far-plane :float))
  (let* ((light-distance (length (- (s~ frag-pos :xyz) light-pos)))
         (light-distance (/ light-distance far-plane)))
    (setf gl-frag-depth light-distance)
    (values)))
(defpipeline-g shadowmap-point-pipe ()
  :vertex   (shadowmap-point-vert g-pnt)
  :geometry (shadowmap-point-geom)
  :fragment (shadowmap-point-frag :vec4))

(defun draw-pointlight ()
  (with-fbo-bound (*shadow-fbo* :attachment-for-size :d)
    (clear *shadow-fbo*)
    (with-setf (cull-face) :front)
    (loop :for actor :in *actors*
          :do (with-slots (buf scale) actor
                (map-g #'shadowmap-point-pipe buf
                       :model->world (model->world actor)
                       :light-pos *light-pos*
                       :far-plane *far-plane*
                       :shadow-matrices *point-mats*)))))

(defun-g shadow-factor ((light-sampler :sampler-cube)
                        (frag-pos      :vec3)
                        (light-pos     :vec3)
                        (far-plane     :float))
  (let* ((frag-to-light (- frag-pos light-pos))
         (closest-depth (* (x (texture light-sampler frag-to-light))
                           far-plane))
         (current-depth (length frag-to-light))
         (bias .05))
    (if (> (- current-depth bias) closest-depth)
        1f0
        0f0)
    ;;(/ closest-depth far-plane)
    ))

(reset-camera)
