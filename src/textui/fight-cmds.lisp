(in-package #:tempus)


(defcommand (ch "backstab") (:standing)
  (send-to-char ch "Backstab who?")
  (wait-state ch 4))

(defcommand (ch "backstab" name) (:standing)
  (perform-backstab ch (get-char-room-vis ch name)))