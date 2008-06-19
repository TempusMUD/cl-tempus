(in-package #:tempus)

(defun parse-pc-race (arg)
  (position arg +player-races+ :test #'string-equal))