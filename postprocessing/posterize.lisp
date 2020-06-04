(in-package :incandescent)

;; Postprocessing effect
;; Default "precision" is 64f0

(defvar *s-palette* nil)

(defun init-posterize ()
  (unless *s-palette*
    (setf *s-palette* (get-tex "static/palette.png" nil t :rgb8))))

(defun-g posterize ((color     :vec3)
                    (precision :float))
  (/ (floor (* color precision))
     precision))

(defun-g posterize ((color :vec3))
  (posterize color 64f0))

(defun-g posterize ((color :vec3)
                    (red   :int)
                    (green :int)
                    (blue  :int))
  (v! (/ (floor (* (x color) red))   red)
      (/ (floor (* (y color) green)) green)
      (/ (floor (* (z color) blue))  blue)))

;;-----------------------------------------------------------

(defun-g posterize ((color     :vec3)
                    (precision :float)
                    (uv        :vec2)
                    (palette   :sampler-2d))
  (let ((col (posterize color precision)))
    (texture palette (v! (/ (+ (x col) (y col) (z col)) 3)
                         (y uv)))))

