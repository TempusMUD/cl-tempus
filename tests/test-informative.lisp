(in-package #:tempus.tests)

(in-suite (defsuite (tempus.informative :in test)))

(deftest look-command ()
  (with-mock-players (alice)
    (setf (tempus::bitp (tempus::prefs-of alice) tempus::+pref-autoexit+) t)
    (tempus::interpret-command alice "l")
    (is (search "&cInside the Great Silver Archway" (char-output alice)))
    (is (search "   To the north, a giant silver arch" (char-output alice)))
    (is (search "[ Exits: n e s w ]" (char-output alice)))))

(deftest look-at-creature ()
  (with-mock-players (alice bob)
    (setf (tempus::fdesc-of bob) (format nil "This is bob.~%"))
    (tempus::interpret-command alice "l bob")
    (is (equal "This is bob.~%Bob appears to be a 198 cm tall, 200 pound male human.~%&yBob is in excellent condition.&n~%&n" (char-output alice)))
    (is (equal "Alice looks at you.~%" (char-output bob)))))

(deftest equipment ()
  (with-mock-players (alice)
    (with-mock-objects ((armor "some plate armor"))
      (setf (tempus::wear-flags-of armor) tempus::+item-wear-body+)
      (tempus::equip-char alice armor tempus::+wear-body+ :worn)
      (tempus::interpret-command alice "eq")
      (is (equal "You are using:~%&g<on body>        &nsome plate armor~%"
                 (char-output alice)))
      (clear-mock-buffers alice)
      (tempus::interpret-command alice "eq all")
      (is (search "<on head>        Nothing!~%"
                       (char-output alice))))))

(deftest money-balance ()
  (with-mock-players (alice)
    (setf (tempus::gold-of alice) 12345)
    (setf (tempus::cash-of alice) 54321)
    (tempus::interpret-command alice "gold")
    (is (equal "You have 12345 gold coins.~%" (char-output alice)))
    (clear-mock-buffers alice)
    (tempus::interpret-command alice "cash")
    (is (equal "You have 54321 credits.~%" (char-output alice)))))

(deftest show-obj-bits/bomb-with-unlit-fuse/no-output ()
  (with-mock-players (alice)
    (with-mock-objects ((bomb "the bomb")
                        (fuse "the fuse"))
      (setf (tempus::kind-of bomb) tempus::+item-bomb+)
      (setf (tempus::kind-of fuse) tempus::+item-fuse+)
      (tempus::obj-to-obj fuse bomb)
      (is (string= "" (with-output-to-string (ouf)
                        (tempus::show-obj-bits bomb alice ouf)))))))

(deftest show-obj-bits/bomb-with-lit-fuse/lit-flag ()
  (with-mock-players (alice)
    (with-mock-objects ((bomb "the bomb")
                        (fuse "the fuse"))
      (setf (tempus::kind-of bomb) tempus::+item-bomb+)
      (setf (tempus::kind-of fuse) tempus::+item-fuse+)
      (setf (aref (tempus::value-of fuse) 1) 1)
      (tempus::obj-to-obj fuse bomb)
      (is (string= " (lit)" (with-output-to-string (ouf)
                              (tempus::show-obj-bits bomb alice ouf)))))))

(deftest do-alignment/good/blue-message ()
  (with-mock-players (alice)
    (setf (tempus::alignment-of alice) 500)
    (tempus::interpret-command alice "alignment")
    (is (equal "&cYour alignment is 500.&n~%" (char-output alice)))))

(deftest do-alignment/evil/red-message ()
  (with-mock-players (alice)
    (setf (tempus::alignment-of alice) -500)
    (tempus::interpret-command alice "alignment")
    (is (equal "&rYour alignment is -500.&n~%" (char-output alice)))))

(deftest do-alignment/neutral/yellow-message ()
  (with-mock-players (alice)
    (setf (tempus::alignment-of alice) 100)
    (tempus::interpret-command alice "alignment")
    (is (equal "&yYour alignment is 100.&n~%" (char-output alice)))))