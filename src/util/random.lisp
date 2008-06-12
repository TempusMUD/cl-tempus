(in-package #:tempus)

(defun random-range (min max)
  "creates a random number in interval [from,to]"
  (+ min (random (- max min))))

(defun dice (num size)
  "simulates dice roll"
  (if (or (minusp num) (minusp size))
      0
      (loop for idx from 1 upto num
           sum (random-range 1 size))))

(defun rand-value (val variance min max)
  (let ((variance-min (- val variance))
        (variance-max (+ val variance)))
    (random-range (if min (max min variance-min) variance-min)
                  (if max (min max variance-max) variance-max))))