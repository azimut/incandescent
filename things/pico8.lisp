(in-package #:incandescent)
;; ::_::
;; cls()
;; for j=0,64 do
;;   x,y=0,0
;;   for i=0,16 do
;;     a=i/36+t()+j/8
;;     b=i/24+t()+j/13
;;     n=64+2*j*(sin(a)-cos(b)/7)
;;     m=64+2*j*(cos(a)-sin(b)/7)
;;     if(x>0)
;;       line(x,y,n,m,7)
;;     x,y=n,m
;;   end
;; end
;; flip()
;; goto _


