(in-package #:tempus)

(defun smoke-lit (obj)
  (not (zerop (aref (value-of obj) 3))))