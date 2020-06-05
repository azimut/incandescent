;;;; incandescent.asd

(asdf:defsystem #:incandescent
  :description "Main package"
  :author "azimut <azimut.github@protonmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria
               #:arrow-macros
               #:cepl
               #:cepl.sdl2
               #:cepl.skitter.sdl2
               #:dirt
               #:livesupport
               #:nineveh
               #:rtg-math.vari
               #:serapeum
               #:temporal-functions
               #:with-setf)
  :components ((:file "package")
               (:file "incandescent")
               ;;(:file "lib/rocketman") ;;!!!!!!!
               (:file "lib/assets")   ;; lisp
               (:file "pbr")          ;; gpu
               (:file "lib/misc-gpu") ;; gpu
               (:file "actors")       ;; lisp
               (:file "event-loop")
               (:file "camera")       ;; lisp
               (:file "controls")     ;; lisp
               (:file "lib/fog")      ;; gpu
               (:file "postprocess")  ;; gpu
               (:file "draw")         ;; lisp
               (:file "render")       ;; gpu - main pipelines
               (:file "cubemap/render")
               (:file "cubemap/cubemap")
               (:file "cubemap/env-map")
               (:file "cubemap/clouds")
               (:file "cubemap/hdr")
               (:file "gi/ibl")
               ;; (:file "things/box")
               ;; (:file "things/piso")
               (:file "things/pbr-simple")
               (:file "things/pbr")
               ;;(:file "shadowmap/render")
               (:file "main")))

(asdf:defsystem #:incandescent/assimp
  :description "Describe incandescent here"
  :author "azimut <azimut.github@protonmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:incandescent
               #:classimp)
  :components (;;(:file "lib/assimp/bones-helpers")
               (:file "lib/assimp/main")
               (:file "lib/assimp/render")
               (:file "lib/assimp/draw")))

(asdf:defsystem #:incandescent/particles
  :description "Describe incandescent here"
  :author "azimut <azimut.github@protonmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:incandescent)
  :components ((:file "particles")))

(asdf:defsystem #:incandescent/godrays
  :description "Describe incandescent here"
  :author "azimut <azimut.github@protonmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:incandescent)
  :components ((:file "postprocessing/godrays")))

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
  :components ((:file "raymarching/3d-sdf")))

(asdf:defsystem #:incandescent/shadowmap
  :description "Describe incandescent here"
  :author "azimut <azimut.github@protonmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:incandescent)
  :components ((:file "shadowmap/shadowmap")
               (:file "shadowmap/pcf")
               (:file "shadowmap/variance")
               (:file "shadowmap/render")
               (:file "shadowmap/draw")))

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
               #:harmony-pulse
               #:cm)
  :components ((:file "audio/harmony")))

(asdf:defsystem #:incandescent/audio-openal
  :description "Describe incandescent here"
  :author "azimut <azimut.github@protonmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:incandescent
               #:cl-openal
               #:cl-alut
               #:cm)
  :pathname "audio/openal"
  :components ((:file "nepal")
               (:file "audio")
               (:file "event")
               (:file "music")
               (:file "positional")
               (:file "sfx")
               (:file "listener")))

(asdf:defsystem #:incandescent/audio-sdl2mix
  :description "Describe incandescent here"
  :author "azimut <azimut.github@protonmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:incandescent
               #:sdl2-mixer)
  :components ((:file "audio/sdl2mix")))

(asdf:defsystem #:incandescent/rocketman
  :description "Describe incandescent here"
  :author "azimut <azimut.github@protonmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:incandescent
               #:rocketman)
  :components ((:file "lib/rocketman")))

(asdf:defsystem #:incandescent/vxgi
  :description "Describe incandescent here"
  :author "azimut <azimut.github@protonmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:incandescent)
  :components ((:file "vxgi")))

(asdf:defsystem #:incandescent/text
  :description "Describe incandescent here"
  :author "azimut <azimut.github@protonmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:incandescent
               ;;#:cepl.sdl2-ttf crashes
               #:cepl.fond)
  :components ((:file "text/text")
               (:file "text/render")
               (:file "text/screen")
               (:file "text/billboard")))

(asdf:defsystem #:incandescent/ode
  :description "Describe incandescent here"
  :author "azimut <azimut.github@protonmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:incandescent
               #:bodge-ode
               #:ode-blob)
  :components ((:file "ode/ode")
               (:file "ode/physic")
               (:file "ode/camera")
               (:file "ode/cube")
               (:file "ode/sphere")
               (:file "ode/cone")
               (:file "ode/newcyl")))

(asdf:defsystem #:incandescent/renderman
  :description "Describe incandescent here"
  :author "azimut <azimut.github@protonmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:harmony-simple ;; fixme: due controls.lisp
               #:incandescent
               #:incandescent/audio
               #:incandescent/assimp
               #:incandescent/particles
               #:incandescent/text)
  :components ((:file "things/renderman/grass")
               (:file "things/renderman/radio")
               (:file "things/renderman/forestthing")
               (:file "things/renderman/dirt")
               (:file "things/renderman/book")))

(asdf:defsystem #:incandescent/damo
  :description "Describe incandescent here"
  :author "azimut <azimut.github@protonmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:incandescent
               ;;#:incandescent/shadowmap
               #:incandescent/assimp
               #:incandescent/ode
               ;;#:incandescent/rocketman
               ;;#:incandescent/particles
               )
  :components ((:file "controls")
               (:file "shadowmap/shadowmap")
               (:file "shadowmap/pcf")
               ;;(:file "shadowmap/variance")
               (:file "gi/vxgi")
               (:file "things/defered-ssr/scene")
               (:file "things/defered-ssr/obstacles")
               (:file "things/defered-ssr/lucy")
               (:file "things/defered-ssr/sun")
               ;;(:file "things/defered-ssr/sound")
               ;;(:file "postprocessing/godrays")
               )
  :defsystem-depends-on (:deploy)
  :build-operation "deploy-op"
  :build-pathname "damo-app"
  :entry-point "incandescent::playit")

(asdf:defsystem #:incandescent/next
  :description "Describe incandescent here"
  :author "azimut <azimut.github@protonmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:incandescent
               ;;#:incandescent/shadowmap
               ;;#:incandescent/assimp
               ;;#:incandescent/ode
               ;;#:incandescent/rocketman
               ;;#:incandescent/particles
               )
  :components ((:file "controls")
               (:file "shadowmap/shadowmap")
               (:file "shadowmap/pcf")
               ;;(:file "shadowmap/variance")
               (:file "gi/vxgi")
               (:file "things/defered-ssr/scene")
               (:file "things/defered-ssr/obstacles")
               ;;(:file "things/defered-ssr/lucy")
               (:file "things/defered-ssr/sun")
               (:file "things/defered-ssr/walls")
               ;;(:file "things/defered-ssr/sound")
               (:file "postprocessing/godrays")
               )
  :defsystem-depends-on (:deploy)
  :build-operation "deploy-op"
  :build-pathname "damo-next"
  :entry-point "incandescent::playit")

(asdf:defsystem #:incandescent/drifter
  :description "A game of sorts"
  :author "azimut <azimut.github@protonmail.com>"
  :license "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:incandescent
               #:cm
               #:cloud
               ;; #:deeds
               ;;#:incandescent/audio-openal
               #:incandescent/rocketman
               #:incandescent/text
               #:incandescent/assimp
               #:incandescent/ode)
  ;;
  :defsystem-depends-on (:deploy)
  :build-operation "deploy-op"
  :build-pathname "drifter"
  :entry-point "incandescent::playit"
  ;;
  :components ((:file "cubemap/render")
               (:file "cubemap/cubemap")
               (:file "cubemap/env-map")
               (:file "cubemap/clouds")
               (:file "cubemap/hdr")
               (:file "gi/ibl")
               (:file "shadowmap/shadowmap")
               (:file "shadowmap/pcf")
               ;;
               (:file "things/drifter/main")
               ;;
               (:file "things/drifter/ode")
               (:file "things/drifter/defer")
               (:file "things/drifter/camera")
               ;;
               (:file "things/drifter/actors/the")
               (:file "things/drifter/actors/route")
               (:file "things/drifter/actors/obstacle")
               (:file "things/drifter/actors/boss")
               (:file "things/drifter/actors/collectable")
               (:file "things/drifter/actors/stopper")
               (:file "things/drifter/actors/rock")
               (:file "things/drifter/actors/missiles")
               ;;
               (:file "things/drifter/sound")
               ;;
               (:file "things/drifter/scene")))

;;--------------------------------------------------

(asdf:defsystem #:incandescent/world2
  :description "Describe incandescent here"
  :author "azimut <azimut.github@protonmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:incandescent
               #:cloud)
  :components ((:file "controls")
               (:file "shadowmap/shadowmap")
               (:file "shadowmap/pcf")
               (:file "gi/vxgi")
               ;;
               (:file "things/world2/actors/obstacles")
               (:file "things/world2/actors/walls")
               ;;
               (:file "things/world2/defer")
               (:file "things/world2/postprocess")
               (:file "things/world2/scene")
               (:file "things/world2/main"))
  :defsystem-depends-on (:deploy)
  :build-operation "deploy-op"
  :build-pathname "damo-next"
  :entry-point "incandescent::playit")
