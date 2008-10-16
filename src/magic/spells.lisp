(in-package #:tempus)

(defvar *spell-info* (make-array 1000))

(defun spell-gen (spell class)
  (aref (min-gen-of (aref *spell-info* spell)) class))

(defun spell-level (spell class)
  (aref (min-level-of (aref *spell-info* spell)) class))

(defun load-corpse-owner (obj)
  (if (minusp (corpse-idnum obj))
      (real-mobile-proto (- (corpse-idnum obj)))
      (load-player-from-xml (corpse-idnum obj))))