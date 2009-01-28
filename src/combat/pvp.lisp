(in-package #:tempus)

(defun reputation-rank (ch)
  (cond
    ((zerop (reputation-of ch))
     0)
    ((>= (reputation-of ch) 1000)
     11)
    (t
     (1+ (floor (reputation-of ch) 100)))))