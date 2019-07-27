(in-package :incandescent)

;; https://github.com/jmickle66666666/PSX-Dither-Shader/

(defvar *dither1* nil)
(defvar *dither2* nil)
(defvar *dither3* nil)

(defun init-dither ()
  (setf *dither1* (sample (sampler-texture (get-tex "static/dither1.png"    t t :r8)) :minify-filter :nearest :magnify-filter :nearest))
  (setf *dither2* (sample (sampler-texture (get-tex "static/dither2.png"    t t :r8)) :minify-filter :nearest :magnify-filter :nearest))
  (setf *dither3* (sample (sampler-texture (get-tex "static/psx_dither.png" t t :r8)) :minify-filter :nearest :magnify-filter :nearest)))

(defun-g channel-error ((col :float) (col-min :float) (col-max :float))
  (let ((range (abs (- col-min col-max)))
        (arange (abs (- col col-min))))
    (/ arange range)))

(defun-g dithered-channel ((err             :float)
                           (dither-block-uv :vec2)
                           (dither-steps    :float)
                           (dither-pattern  :sampler-2d))
  (let* ((err (/ (floor (* err dither-steps)) dither-steps))
         (dither-uv (v! err 0))
         (dither-uv (v! (+ (x dither-uv) (x dither-block-uv))
                        (y dither-block-uv))))
    (x (texture dither-pattern dither-uv))))

(defun-g rgb->yuv ((rgba :vec4))
  (let* ((yuva (vec4 0 0 0 0))
         (yuva (v! (+ (* (x rgba) .2126)
                      (* (y rgba) .7152)
                      (* (z rgba) .0722))
                   0 0 0))
         (yuva (v! (x yuva)
                   (/ (- (y rgba) (x yuva)) 1.8556)
                   0 0))
         (yuva (v! (x yuva)
                   (y yuva)
                   (/ (- (x rgba) (x yuva)) 1.5748)
                   0))
         (yuva (v! (x yuva)
                   (+ (s~ yuva :yz) .5)
                   (w rgba))))
    yuva))

(defun-g yuv->rgb ((yuva :vec4))
  (let ((yuva (v! (x yuva) (- (s~ yuva :yz) .5) (w yuva))))
    (v! (+ (x yuva) (* (y yuva)  0.0)      (* (z yuva)  1.5748))
        (+ (x yuva) (* (y yuva) -0.187324) (* (z yuva) -0.468124))
        (+ (x yuva) (* (y yuva)  1.8556))
        (w yuva))))

(defun-g psx-dither ((sam            :sampler-2d)
                     (uv             :vec2)
                     (precision      :float)
                     (dither-texture :sampler-2d))
  (let* ((yuv  (rgb->yuv (texture sam uv)))
         (col1 (/ (floor (* yuv precision)) precision))
         (col2 (/ (ceil  (* yuv precision)) precision))
         ;; Vector4(1 / width,
         ;;         1 / height,
         ;;         width,
         ;;         height)
         (dither-texture-size (texture-size dither-texture 0))
         (dither-size         (y dither-texture-size))
         (dither-steps        (/ (x dither-texture-size)
                                 dither-size))
         ;;
         (main-texture-size (texture-size sam 0))
         ;;
         (dither-block-uv uv)
         (dither-block-uv (v! (mod (x dither-block-uv)
                                   (/ dither-size (x main-texture-size)))
                              (y dither-block-uv)))
         (dither-block-uv (v! (/ (x dither-block-uv)
                                 (/ dither-size (y main-texture-size)))
                              (y dither-block-uv)))
         (dither-block-uv (v! (x dither-block-uv)
                              (mod (y dither-block-uv)
                                   (/ dither-size (x main-texture-size)))))
         (dither-block-uv (v! (x dither-block-uv)
                              (/ (y dither-block-uv)
                                 (/ dither-size (y main-texture-size)))))
         (dither-block-uv (v! (/ (x dither-block-uv) dither-steps)
                              (y dither-block-uv)))
         (yuv (v! (mix (x col1) (x col2)
                       (dithered-channel (channel-error (x yuv) (x col1) (x col2))
                                         dither-block-uv
                                         dither-steps
                                         dither-texture))
                  (mix (y col1) (y col2)
                       (dithered-channel (channel-error (y yuv) (y col1) (y col2))
                                         dither-block-uv
                                         dither-steps
                                         dither-texture))
                  (mix (z col1) (z col2)
                       (dithered-channel (channel-error (z yuv) (z col1) (z col2))
                                         dither-block-uv
                                         dither-steps
                                         dither-texture))
                  (w yuv))))
    (yuv->rgb yuv)))
