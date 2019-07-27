(in-package :incandescent)

;; https://github.com/oxysoft/RetroSuite3D/
;;
;; NOTE: width and height are hardcoded on the shader as 8x8
;; NOTE: fmod implementations changes behaviour so is floats or ints used

(defvar *s-dither1* nil)

(defun init-retro-dither ()
  (setf *s-dither1* (get-tex "static/retro-suite-dither1.png" t t :r8)))

;;--------------------------------------------------

(defun-g dither-pattern ((position   :vec2)
                         (color      :vec3)
                         (dither-sam :sampler-2d)
                         (threshold  :float)
                         (strength   :float))
  (let* ((brightness (nineveh.color:rgb->luma-bt601 color))
         (x          (mod (x position) 8f0)); width
         (y          (mod (y position) 8f0)); height
         (lim        (x (texture dither-sam (/ (v! x y) 8))))); width
    (* color (if (< brightness (* lim threshold))
                 strength
                 1f0))))

;;--------------------------------------------------

(defun-g dither-bayer ((position :vec2)
                       (color :vec3)
                       (strength :float))
  (let* ((brightness (nineveh.color:rgb->luma-bt601 color))
         (x          (int (floor (mod (x position) 4f0))))
         (y          (int (floor (mod (y position) 4f0))))
         (index      (+ x (* 4 y)))
         (lim        0f0))
    (when(< x 8)
      (when (= index  0) (setf lim 0.0625))
      (when (= index  1) (setf lim 0.5625))
      (when (= index  2) (setf lim 0.1875))
      (when (= index  3) (setf lim 0.6875))
      (when (= index  4) (setf lim 0.8125))
      (when (= index  5) (setf lim 0.3125))
      (when (= index  6) (setf lim 0.9375))
      (when (= index  7) (setf lim 0.4375))
      (when (= index  8) (setf lim 0.25))
      (when (= index  9) (setf lim 0.75))
      (when (= index 10) (setf lim 0.125))
      (when (= index 11) (setf lim 0.625))
      (when (= index 12) (setf lim 1f0))
      (when (= index 13) (setf lim 0.5))
      (when (= index 14) (setf lim 0.875))
      (when (= index 15) (setf lim 0.375)))
    (* color (if (< brightness (* lim strength))
                 0f0
                 1f0))))

;;--------------------------------------------------

#+nil
(defun-g pixel-max ((color       :vec3)
                    (color-count :int))
  (let ((dist 10000000f0))
    (dotimes (i color-count)
      ())))
