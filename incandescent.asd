;;;; incandescent.asd

(asdf:defsystem #:incandescent
  :description "Main package"
  :author "azimut <azimut.github@protonmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:cepl
               #:alexandria
               #:serapeum
               #:temporal-functions
               #:with-setf
               #:rtg-math.vari
               #:cepl.sdl2
               #:swank
               #:nineveh
               #:png
               #:livesupport
               #:cepl.skitter.sdl2
               #:dirt)
  :components ((:file "package")
               (:file "incandescent")
               (:file "lib/assets")   ;; lisp
               (:file "pbr")
               (:file "lib/misc-gpu")
               (:file "actors")       ;; lisp
               (:file "camera")       ;; lisp
               (:file "controls")     ;; lisp
               (:file "postprocess")  ;; gpu
               (:file "draw")         ;; lisp
               (:file "render")       ;; gpu - main pipelines
               (:file "cubemap")
               (:file "ibl")
               (:file "main")))

(asdf:defsystem #:incandescent/assimp
  :description "Describe incandescent here"
  :author "azimut <azimut.github@protonmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:incandescent
               #:classimp)
  :components ((:file "lib/assimp")))

(asdf:defsystem #:incandescent/particles
  :description "Describe incandescent here"
  :author "azimut <azimut.github@protonmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:incandescent)
  :components ((:file "particles")))

(asdf:defsystem #:incandescent/fog
  :description "Describe incandescent here"
  :author "azimut <azimut.github@protonmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:incandescent)
  :components ((:file "lib/fog")))

(asdf:defsystem #:incandescent/godrays
  :description "Describe incandescent here"
  :author "azimut <azimut.github@protonmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:incandescent)
  :components ((:file "godrays")))

(asdf:defsystem #:incandescent/ssao
  :description "Describe incandescent here"
  :author "azimut <azimut.github@protonmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:incandescent)
  :components ((:file "ssao")))

(asdf:defsystem #:incandescent/bloom
  :description "Describe incandescent here"
  :author "azimut <azimut.github@protonmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:incandescent)
  :components ((:file "bloom")))

(asdf:defsystem #:incandescent/raymarching
  :description "Describe incandescent here"
  :author "azimut <azimut.github@protonmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:incandescent)
  :components ((:file "3d-sdf")
               (:file "raymarching")))

(asdf:defsystem #:incandescent/shadowmap
  :description "Describe incandescent here"
  :author "azimut <azimut.github@protonmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:incandescent)
  :components ((:file "shadowmap")))

(asdf:defsystem #:incandescent/dof
  :description "Describe incandescent here"
  :author "azimut <azimut.github@protonmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:incandescent)
  :components ((:file "dof")))

(asdf:defsystem #:incandescent/audio
  :description "Describe incandescent here"
  :author "azimut <azimut.github@protonmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:incandescent
               #:harmony-simple
               #:harmony-pulse)
  :components ((:file "audio/harmony")))

(asdf:defsystem #:incandescent/audio-sdl2mix
  :description "Describe incandescent here"
  :author "azimut <azimut.github@protonmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:incandescent
               #:sdl2-mixer)
  :components ((:file "audio/sdl2mix")))

(asdf:defsystem #:incandescent/text
  :description "Describe incandescent here"
  :author "azimut <azimut.github@protonmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:incandescent
               #:cepl.fond)
  :components ((:file "text")))

(asdf:defsystem #:incandescent/renderman
  :description "Describe incandescent here"
  :author "azimut <azimut.github@protonmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:incandescent
               #:incandescent/audio
               #:incandescent/assimp
               #:incandescent/particles
               #:incandescent/text
               #:incandescent/fog))
