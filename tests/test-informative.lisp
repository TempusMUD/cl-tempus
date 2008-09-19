(in-package #:tempus.tests)

(in-suite* #:tempus.informative :in :tempus)

(test look-command
  (with-mock-players (alice)
    (setf (tempus::bitp (tempus::prefs-of alice) tempus::+pref-autoexit+) t)
    (tempus::interpret-command alice "l")
    (is-true (search "&cHoly Square" (char-output alice)))
    (is-true (search "   This is the main junction" (char-output alice)))
    (is-true (search "[ Exits: n e s w u ]" (char-output alice)))))

(test look-at-creature
  (with-mock-players (alice bob)
    (setf (tempus::fdesc-of bob) (format nil "This is bob.~%"))
    (tempus::interpret-command alice "l bob")
    (is (equal "This is bob.~%Bob appears to be a 100 cm tall, 100 pound male human.~%&yBob is in excellent condition.&n~%&n" (char-output alice)))
    (is (equal "Alice looks at you.~%" (char-output bob)))))