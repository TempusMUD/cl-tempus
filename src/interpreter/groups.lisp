(in-package :tempus)

(defvar *access-groups* nil)

(defun security-load-groups ()
  nil)

(defun security-is-member (ch group)
  (declare (ignore group))
  (immortal-level-p ch))