(in-package #:tempus)

(declaim (ftype (function (fixnum fixnum) fixnum)
                random-range dice))

(defun random-range (a b)
  "Creates a random number in interval [from,to]"
  (cond
    ((> a b)
     (+ b (random (- a b))))
    ((< a b)
     (+ a (random (- b a))))
    (t
     a)))

(defun dice (num size)
  "Simulates dice roll"
  (if (and (plusp num) (plusp size))
      (loop
         for idx from 1 upto num
         sum (random-range 1 size))
      0))

(defun rand-value (val variance min max)
  "Produces a randomized value with a given variance.  The value will not exceed the bounds of MIN and MAX."
  (if (zerop variance)
      val
      (let ((variance-min (- val variance))
            (variance-max (+ val variance)))
        (random-range (floor (if min (max min variance-min) variance-min))
                      (floor (if max (min max variance-max) variance-max))))))

(defun random-elt (seq)
  "Returns a random element of a sequence."
  (elt seq (random (length seq))))