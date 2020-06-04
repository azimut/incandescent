(in-package #:incandescent)

;; NT: encode/decode of HDR would only work if I use a ssbo.
;; which I would also need for the 2nd bounce.

(v-def-glsl-template-fun << (x y) "(~a << ~a)" (v-uint v-uint) v-uint :pure t)
(v-def-glsl-template-fun >> (x y) "(~a >> ~a)" (v-uint v-uint) v-uint :pure t)

(defun-g encode-color ((color :vec4))
  "Encode HDR color to a 32 bit uint
   Alpha is 1 bit + 7 bit HDR remapping"
  (let* (;; normalize color to LDR
         (hdr     (length (s~ color :xyz)))
         (color   (v! (/ (s~ color :xyz) hdr)
                      (w color)))
         ;; encode LDR color and HDR range
         (color-xyz-m (* 255f0 (s~ color :xyz))) ;??
         (i-color (uvec3 (uint (x color-xyz-m))  ;??
                         (uint (y color-xyz-m))
                         (uint (z color-xyz-m))))
         (i-hdr   (uint (saturate (* (/ hdr 10f0) 127))))
         (color-mask (logior (<< i-hdr (uint 24))
                             (<< (x i-color) (uint 16))))
         (color-mask (logior color-mask
                             (<< (y i-color) (uint 8))))
         (color-mask (logior color-mask
                             (z i-color)))
         ;; encode alpha into highest bit
         (i-alpha    (if (> (w color) 0) (uint 1) (uint 0)))
         (color-mask (logior color-mask
                             (<< i-alpha (uint 31)))))
    color-mask))

(defun-g decode-color ((color-mask :uint))
  (let* ((range 10f0)
         (hdr   (float 0f0))
         (color (vec4 0f0)))
    ;;
    (setf hdr (float (logand color-mask (>> (uint 24) #x0000007f))))
    (setf color (v! (logand (>> color-mask (uint 16)) #x000000ff)
                    (logand (>> color-mask (uint  8)) #x000000ff)
                    (logand color-mask #x000000ff)
                    0f0))
    (divf hdr 127f0)
    (divf (s~ color :xyz) (vec3 255f0))
    (setf (s~ color :xyz) (* (s~ color :xyz) (* hdr range)))
    (setf (w color) (float (logand (>> color-mask (uint 31))
                                   #x00000001)))
    ;;
    color))
