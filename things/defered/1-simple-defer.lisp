(in-package #:incandescent)

;; 1 - NAIVE approach, we would still need to loop for each light

;; On (as-frame) main.lisp
;; (map-g #'postprocess-defer-pipe *bs*
;;        :albedo-sam *sam*
;;        :position-sam *sam1*
;;        :normal-sam *sam2*
;;        :cam-pos (pos *currentcamera*)
;;        :light-pos *light-pos*)

(defun-g postprocess-defered ((uv :vec2)
                              &uniform
                              (albedo-sam   :sampler-2d)
                              (position-sam :sampler-2d)
                              (normal-sam   :sampler-2d)
                              (light-pos    :vec3)
                              (cam-pos      :vec3))
  (let* ((albedo   (s~ (texture albedo-sam uv) :xyz))
         (position (s~ (texture position-sam uv) :xyz))
         (normal   (normalize
                    (s~ (texture normal-sam uv) :xyz)))
         (final-color (point-light-apply albedo
                                         (v! .6 .5 .3)
                                         (v! 0 3 0)
                                         position
                                         normal
                                         1f0
                                         .22
                                         .20
                                         cam-pos
                                         .9
                                         .1))
         (ldr  (tone-map-reinhard final-color *exposure*))
         (luma (rgb->luma-bt601 ldr)))
    (v! ldr luma)))

(defpipeline-g postprocess-defer-pipe (:points)
  :fragment (postprocess-defered :vec2))

