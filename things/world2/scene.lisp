(in-package #:incandescent)

(progn (defun init-scene ()
         (free-actors)
         (make-obstacle
          :pos (v! 0 0 0)
          :color (v! .9 .1 .1)
          :dim (v3! .3)
          :prop (v! 0 .7 .7 0))
         (make-piso :pos (v! 0 -1 0)
                    :dim (v! 20 1 20)
                    :uv-scale (v! 10 10)
                    :metallic .2f0))
       (init-scene))





