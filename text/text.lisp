(in-package :incandescent)

;; Everything on cepl.fond/cl-fond goes around updating the buffer stream
;; with every new text update.
;; See: (cepl.fond:update-fond-text *text* string)
;;
;; -Might be possible to re-use the same texture but not by default.-
;; See: (setf *text* (cepl.fond:make-fond-text *font* "a text"))

(defvar *default-font*    (resolve-path "static/Courier New.ttf"))
(defvar *default-charset* "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ .!?")
