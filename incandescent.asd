;;;; incandescent.asd

(asdf:defsystem #:incandescent
  :description "Describe incandescent here"
  :author "azimut <azimut.github@protonmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:cepl
               #:alexandria
               #:with-setf
               #:rtg-math.vari
               #:cepl.sdl2
               #:swank
               #:nineveh
               #:classimp
               #:livesupport
               #:cepl.skitter.sdl2
               #:dirt)
  :components ((:file "package")
               (:file "incandescent")
               (:file "lib/assets")
               (:file "lib/assimp")
               (:file "lib/misc-gpu")
               (:file "actors")
               (:file "camera")
               (:file "cubemap")
               (:file "postprocess")
               (:file "draw")
               (:file "render")
               (:file "main")))
