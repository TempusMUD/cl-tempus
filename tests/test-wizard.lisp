(in-package #:tempus.tests)

(in-suite (defsuite (tempus.wizard :in test)))

(deftest echo/normal/displays-to-room ()
  (with-mock-players (alice bob chuck)
    (setf (tempus::level-of alice) 51)
    (setf (tempus::level-of bob) 49)
    (setf (tempus::level-of chuck) 52)
    (tempus::interpret-command alice "echo testing")
    (is (equal "Testing~%" (char-output alice)))
    (is (equal "Testing~%" (char-output bob)))
    (is (equal "[Alice] testing~%" (char-output chuck)))))

(deftest send/normal/sends-to-char ()
  (with-mock-players (alice bob)
    (tempus::interpret-command alice "send .bob testing")
    (is (equal "You send 'testing' to Bob.~%" (char-output alice)))
    (is (equal "Testing~%" (char-output bob)))))

(deftest at/normal/performs-command-in-target-room ()
  (with-mock-players (alice bob)
    (tempus::char-from-room alice)
    (tempus::char-to-room alice (tempus::real-room 3001))
    (tempus::interpret-command alice "at 3013 say hi")
    (is (equal "&BYou say, &c'hi'&n~%" (char-output alice)))
    (is (equal "&BAlice says, &c'hi'&n~%" (char-output bob)))))

(deftest goto/numeric-target/changes-room ()
  (with-mock-players (alice bob)
    (tempus::char-from-room alice)
    (tempus::char-to-room alice (tempus::real-room 3001))
    (tempus::interpret-command alice "goto 3013")
    (is (= 3013 (tempus::number-of (tempus::in-room-of alice))))
    (is (equal "Alice appears with an ear-splitting bang.~%" (char-output bob)))))

(deftest goto/char-target/changes-room ()
  (with-mock-players (alice bob)
    (tempus::char-from-room alice)
    (tempus::char-to-room alice (tempus::real-room 3001))
    (tempus::interpret-command alice "goto .bob")
    (is (= 3013 (tempus::number-of (tempus::in-room-of alice))))
    (is (equal "Alice appears with an ear-splitting bang.~%" (char-output bob)))))

(deftest goto/following-imm/imm-in-same-room ()
  (with-mock-players (alice bob)
    (setf (tempus::level-of bob) 51)
    (tempus::add-follower bob alice)
    (clear-mock-buffers bob alice)
    (tempus::interpret-command alice "goto 3001")
    (is (= 3001 (tempus::number-of (tempus::in-room-of alice))))
    (is (= 3001 (tempus::number-of (tempus::in-room-of bob))))
    (is (search "Bob appears with an ear-splitting bang.~%" (char-output alice)))
    (is (search "Alice disappears in a puff of smoke.~%" (char-output bob)))))

(deftest distance/valid-rooms/returns-distance ()
  (with-mock-players (alice bob)
    (tempus::interpret-command alice "distance 24800")
    (is (equal "Room 24800 is 40 steps away.~%" (char-output alice)))))

(deftest distance/no-connection/returns-error ()
  (with-mock-players (alice bob)
    (tempus::interpret-command alice "distance 43000")
    (is (equal "There is no valid path to room 43000.~%" (char-output alice)))))

(deftest force-command ()
  (with-mock-players (alice bob)
    (function-trace-bind ((calls tempus::interpret-command))
        (tempus::interpret-command alice "force bob to inventory")
      (is (equal "You got it.~%" (char-output alice)))
      (is (= (length calls) 2))
      (is (eql (first (first calls)) bob))
      (is (equal (second (first calls)) "inventory")))))
