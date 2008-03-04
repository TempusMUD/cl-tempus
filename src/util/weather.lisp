(in-package :tempus)

(defparameter +moon-new+ 0)
(defparameter +moon-wax-crescent+ 1)
(defparameter +moon-first-quarter+ 2)
(defparameter +moon-wax-gibbous+ 3)
(defparameter +moon-full+ 4)
(defparameter +moon-wane-gibbous+ 5)
(defparameter +moon-last-quarter+ 6)
(defparameter +moon-wane-crescent+ 7)


(defun lunar-phase (day)
  (cond
    ((or (= day 0) (= day 23))
     +moon-new+)
    ((< day 5)
     +moon-wax-crescent+)
    ((< day 7)
     +moon-first-quarter+)
    ((< day 11)
     +moon-wax-gibbous+)
    ((< day 13)
     +moon-full+)
    ((< day 17)
     +moon-wane-gibbous+)
    ((< day 19)
     +moon-last-quarter+)
    (t
     +moon-wane-crescent+)))

