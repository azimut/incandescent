(in-package #:incandescent)

(defvar *default-amulet-sprite* "
..YYYYY..
.Y.....Y.
Y..B.B..Y
Y.......Y
Y.R...R.Y
Y..RRR..Y
.Y.....Y.
..YYYYY..")

(defvar *amulet-tex* nil)
(defvar *amulet-sam* nil)

(defparameter *amulet-colors*
  (list
   (cons (character "W") (v! 1    1    1    1))
   (cons (character "w") (v! 0.75 0.75 0.75 1))
   (cons (character "K") (v! 0    0    0    1))
   (cons (character "k") (v! 0.5  0.5  0.5  1))
   (cons (character "R") (v! 1    0    0    1))
   (cons (character "r") (v! 0.5  0    0    1))
   (cons (character "Y") (v! 1    1    0    1))
   (cons (character "y") (v! 0.5  0.5  0    1))
   (cons (character "G") (v! 0    1    0    1))
   (cons (character "g") (v! 0    0.5  0    1))
   (cons (character "C") (v! 0    1    1    1))
   (cons (character "c") (v! 0    0.5  0.5  1))
   (cons (character "B") (v! 0    0    1    1))
   (cons (character "b") (v! 0    0    0.5  1))
   (cons (character "M") (v! 1    0    1    1))
   (cons (character "m") (v! 0.5  0    0.5  1))
   (cons (character "O") (v! 1    0.5  0    1))
   (cons (character "o") (v! 0.5  0.25 0    1))))

(defun clean-sprite (string)
  "accepts only uppercase letters and dot"
  (declare (type string string))
  (cl-ppcre:regex-replace-all "[^A-Za-z.\\n]"
                              (serapeum:trim-whitespace string)
                              ""))

(defun describe-sprite (string)
  (declare (type string string))
  (let* ((rows    (cl-ppcre:split #\Newline string))
         (nr-rows (length rows))
         (nr-cols (length (nth 0 rows))))
    (list :valid-p (and (plusp nr-rows)
                        (apply #'= (mapcar #'length rows)))
          :nr-rows nr-rows
          :nr-cols nr-cols)))

(defun sprite-to-texture (string)
  (declare (type string string))
  (let ((sprite (clean-sprite string)))
    (destructuring-bind (&key valid-p nr-rows nr-cols)
        (describe-sprite sprite)
      (when valid-p
        (with-free c-array (make-c-array nil :dimensions `(,nr-cols ,nr-rows)
                                             :element-type :vec4)
          (loop :for char :across (cl-ppcre:regex-replace-all #\Newline sprite "")
                :for i :from 0
                :do (setf (row-major-aref-c c-array i)
                          (or (serapeum:cdr-safe (assoc char *amulet-colors*))
                              (v! 1 1 1 1))))
          (make-texture c-array))))))

(defun free-sprite ()
  (when *amulet-tex* (free *amulet-tex*)))

(defun draw-sprite (string)
  (free-sprite)
  (setf *amulet-tex* (sprite-to-texture string))
  (when *amulet-tex*
    (setf *amulet-sam* (sample *amulet-tex* :minify-filter :nearest
                                            :magnify-filter :nearest))))

(draw-sprite *default-amulet-sprite*)
(draw-sprite "
...M...
.C....c")
(draw-sprite "
.K.K.K.K.
.KKKKKKK.
KYYYYYYYK
KYYYRRYYK
KYYRRRRYK
KYYRWRRYK
KYYYRRYYK
KYYYYYYYK
.KKKKKKK.
.K.K.K.K.")

(draw-sprite "
......................
..kk..k...............
..k.k....k..k.k.......
..kk..k.k.k.kk........
..k...k.k.k.k.k.......
......................")

(draw-sprite "
........BBBBBBBBBB.................
.....BBBBBBBBBBBBBBBBBBB...........
...BBBBBBBBBBBBBBBBBBBBBBBBB.......
..BBBBBBBBBBBBBBBBBBBBBBBBBBB......
.BBBBBBBBBBBBBBBBBBBBBBBBBBBBB.....
.BBBBBBBBBBBBBBBBBBBBBBBBBBBBBB....
.BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB...
.BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB...
BBBBKKKKKKKKBBBBBBBBBBBBBBBBBBBB...
BBBBBKBKBKBKBBBBBBBBBBBBBBBBBBBBBBB
BBBBBBBBBBBBCBBBBBBBKKKKKKKKBBBBBBB
BBBBBBBBBBBCBBBBBBBBBKBKBKBKBBBBBBB
BBBBBBBBBBBCBBBBBBBBBBBBBBBBBBBBBBB
BBBBBBBBBBCCBBBBCBBBBBBBBBBBBBBBBBB
BBBBBBBBBBCBBBBCCCCBBBBBBBBBBBBBBBB
BBBBBBBBBCCBBBBBBBCBBBBBBBBBBBBBBBB
BBBBBBBBBCBBBBBBBCCBBBBBBBBBBBBBB..
BBBBBBBBBCCCCCCCCCBBBBBBBBBBBBBBB..
BBBBRRRRRRRRRRRRRRRRRRRRRRRRBBBBB..
BBBBRKKWKWKWWKWWWKWKWKWKWKWRBBBBB..
BBBBRKKKKKKKKKKKKKKKKKKKKKKRBBBBB..
BBBBRWKWKWKWWKWWKWWKWWWKWKWRBBBBB..
BBBBRRRRRRRRRRRRRRRRRRRRRRRRBBBBB..
..BBBBBBBBBBBBBBBBBBBBBBBBBBBBBB...
...BBBBBBBBBBBBBBBBBBBBBBBBBBBB....
......BBBBBBBBBBBBBBBBBBBBBBBB.....")


(draw-sprite "
.............................................
.............................................
.............................................
.............................................
................c............................
.............cc..............................
............c................................
............c................................
............c................................
.............c...............................
..............mc.............................
................c............................
.............................................
.............................................
.............................................
.............................................
.............................................
.............................................")
