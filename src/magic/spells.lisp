(in-package #:tempus)

(defvar *spell-info* (make-array 1000))

(defun spell-gen (spell class)
  (aref (remort-gen-of (aref *spell-info* spell)) class))

(defun spell-level (spell class)
  (aref (remort-gen-of (aref *spell-info* spell)) class))
