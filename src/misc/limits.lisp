(in-package #:tempus)

(defun gain-condition (ch condition value)
  (assert (<= 0 condition 2) nil "Invalid condition passed to gain-condition")
  (unless (zerop value)
    (setf (aref (conditions-of ch) condition)
          (pin (+ (aref (conditions-of ch) condition) value) 0 24))
    (when (and (zerop (get-condition ch condition))
               (link-of ch)
               (eql (state-of (link-of ch)) 'playing))
      (case condition
        (#.+full+
         (send-to-char ch
                       (case (random-range 0 2)
                         (0 "You feel quite hungry.~%")
                         (1 "You are famished.~%")
                         (2 "You are hungry.~%"))))
        (#.+thirst+
         (send-to-char ch
                       (case (random-range 0 1)
                         (0 "Your throat is parched.~%")
                         (1 "You are thirsty.~%"))))
        (#.+drunk+
         (send-to-char ch "You are now sober.~%"))))))

