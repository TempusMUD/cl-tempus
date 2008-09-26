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

(test equipment
  (with-mock-players (alice)
    (with-mock-objects ((armor "some plate armor"))
      (setf (tempus::wear-flags-of armor) tempus::+item-wear-body+)
      (tempus::equip-char alice armor tempus::+wear-body+ :worn)
      (tempus::interpret-command alice "eq")
      (is (equal "You are using:~%&g<on body>        &nsome plate armor~%"
                 (char-output alice)))
      (clear-mock-buffers alice)
      (tempus::interpret-command alice "eq all")
      (is-true (search "<on head>        Nothing!~%"
                       (char-output alice))))))

(test money-balance
  (with-mock-players (alice)
    (setf (tempus::gold-of alice) 12345)
    (setf (tempus::cash-of alice) 54321)
    (tempus::interpret-command alice "gold")
    (is (equal "You have 12345 gold coins.~%" (char-output alice)))
    (clear-mock-buffers alice)
    (tempus::interpret-command alice "cash")
    (is (equal "You have 54321 credits.~%" (char-output alice)))))
