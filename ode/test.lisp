(in-package :incandescent)

(progn (defun test ()
         (free-actors)
         (ode-destroy)
         (ode-init)
         (make-land)
         (dotimes (i 30)
           (make-physic-box
            :rot (q:from-axis-angle (v! (random 1f0) (random 1f0) (random 1f0))
                                    (radians (random 360f0)))
            :pos (v! (serapeum:random-in-range -10 10)
                     (serapeum:random-in-range 3 10)
                     (serapeum:random-in-range -10 10)))))
       (test))

(progn (defun test ()
         (free-actors)
         (make-land)
         (loop :for i :from 1 :by 2 :to 20
               :for y :from 1 :by .7 :to 10
               :do (make-physic-box :pos (v! i (+ y .5) 10))))

       (test)
       )

(progn
  (defun test ()
    (free-actors)
    (make-land)
    (loop :for x :from 1 :by 1 :to 9
          :do (loop :for y :from 0 :to 3
                    :do (make-physic-box :pos (v! x (+ y .5) 0))))
    (loop :for z :from 0 :by 1 :to 9
          :do (loop :for y :from 0 :to 3
                    :do (make-physic-box :pos (v! 0 (+ y .5) z))))
    (loop :for y :from 0 :by 1 :to 10
          :do (if (< (random 1f0) .5)
                  (make-physic-box :pos (v! (+ 1 (random 1)) (+ y .5) 0))
                  (make-physic-sphere :pos (v! (+ 1 (random 1)) (+ y .5) 0))))
    ;; (loop :for z :from 0 :by 1 :to 10
    ;;       :do (make-physic-box :pos (v! 11 .5 z)))
    ;; (loop :for x :from 1 :by 1 :to 10
    ;;       :do (make-physic-box :pos (v! x .5 10)))
    )

  (test)
  )



(progn (defun test ()
         (free-actors)
         (make-land)
         (loop :for i :from 1 :by 1.9 :to 11
               :do (make-physic-box :pos (v! i .5 10)))
         (loop :for i :from 2 :by 1.87 :to 10
               :do (make-physic-box :pos (v! i 1.5 10)))
         ;;--------------------------------------------------
         (loop :for i :from 1 :by 1.9 :to 11
               :do (make-physic-box :pos (v! i .5 5)))
         (loop :for i :from 2 :by 1.87 :to 10
               :do (make-physic-box :pos (v! i 1.5 5)))
         )

       (test)
       )

(progn (defun test ()
         (free-actors)
         (make-land)
         (loop :for y :from 0 :by 1 :to 10
               :do (make-physic-box :pos (v! 0 (+ y .5) 1)))
         (loop :for y :from 0 :by 1 :to 10
               :do (make-physic-box :pos (v! 0 (+ y .5) 0)))
         (loop :for y :from 0 :by 1 :to 10
               :do (make-physic-box :pos (v! 1 (+ y .5) 0)))
         (make-physic-sphere :pos (v! 0 0 0))
         ;;--------------------------------------------------
         )

       (test)
       )

(progn (defun test ()
         (free-actors)
         (make-land)
         (make-physic-box :pos (v! 0 5.5 0) :x 8d0 :z 8d0)
         (loop :for y :from .5 :by 1 :to 5
               :do (make-physic-box :pos (v! 3 y 3)))
         (loop :for y :from .5 :by 1 :to 5
               :do (make-physic-box :pos (v! -3 y -3)))
         (loop :for x :from -2 :to 3
               :for z :from -2 :to 3
               :do (make-physic-sphere :pos (v! x 6.5 z)))
         ;;--------------------------------------------------
         )

       (test)
       )
