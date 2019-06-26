(in-package :incandescent)

;; Everything on cepl.fond/cl-fond goes around updating the buffer stream
;; with every new text update.
;; See: (cepl.fond:update-fond-text *text* string)
;;
;; -Might be possible to re-use the same texture but not by default.-
;; See: (setf *text* (cepl.fond:make-fond-text *font* "a text"))

(defvar *default-font*    (resolve-path "static/Courier New.ttf"))
(defparameter *default-charset* "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ .!?()0123456789-")

;; 2D On screen text helpers

(defvar *font* nil "cepl.fond:fond-font struct
                    - font
                    - cepl sampler")
(defvar *text* nil "cepl.fond:fond-text struct:
                    - varr   (cepl gpu array)
                    - iarr   (cepl gpu array)
                    - stream (cepl buffer stream)
                    - font   (cepl.fond:fond-font - font, cepl sampler)")

(defparameter *font-blending*
  (make-blending-params :source-alpha :one-minus-src-alpha))
