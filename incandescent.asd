;;;; incandescent.asd

(asdf:defsystem #:incandescent
  :description "Describe incandescent here"
  :author "azimut <azimut.github@protonmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:cepl #:rtg-math.vari #:cepl.sdl2 #:swank #:livesupport #:cepl.skitter.sdl2 #:dirt)
  :components ((:file "package")
               (:file "incandescent")))
