(in-package :incandescent)

(defclass dirt (pbr) ())

(defun make-dirt (&key (pos (v! 0 0 0)) (buf (lattice 100 100 2 2 t)) (uv-repeat (v! 1 1)))
  (let ((obj
         (make-instance
          'dirt
          :albedo (get-tex "static/leaf/leaf-fall1-albedo.png")
          :normal (get-tex "static/leaf/leaf-fall3-normal-unity.png")
          :ao (get-tex "static/leaf/leaf-fall1-ao.png")
          :uv-repeat uv-repeat
          :buf buf
          :pos pos)))
    (push obj *actors*)
    obj))
