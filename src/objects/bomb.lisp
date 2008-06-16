(in-package #:tempus)

(defun fuse-type (object)
  (aref (value-of object) 0))
(defun fuse-state (object)
  (aref (value-of object) 1))
(defun fuse-timer (object)
  (aref (value-of object) 2))