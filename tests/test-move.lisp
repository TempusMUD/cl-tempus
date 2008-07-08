(in-package #:tempus.tests)

(in-suite* #:tempus.move :in :tempus)

(test basic-movement
  (with-mock-players (alice bob)
    (let ((orig-room (tempus::in-room-of alice)))
      (tempus::interpret-command alice "e")
      (is (eql (tempus::to-room-of
                (aref (tempus::dir-option-of orig-room) tempus::+east+))
               (tempus::number-of (tempus::in-room-of alice))))
      (is-true (search "East Goddess Street" (char-output alice)))
      (is-true (search "The broad tree-lined avenue leads east"
                       (char-output alice)))
      (is (or (string= (char-output bob) "Alice walks east.~%")
              (string= (char-output bob) "Alice strolls east.~%")
              (string= (char-output bob) "Alice departs eastward.~%")
              (string= (char-output bob) "Alice leaves east.~%")))

      (clear-mock-buffers alice bob)

      (tempus::interpret-command alice "w")
      (is (eql (tempus::in-room-of alice) orig-room))
      (is-true (search "Holy Square" (char-output alice)))
      (is (or (string= (char-output bob) "Alice walks in from the east.~%")
              (string= (char-output bob) "Alice strolls in from the east.~%")
              (string= (char-output bob) "Alice has arrived from the east.~%")
              (char-output bob))))))

(test movement-with-brief
  (with-mock-players (alice)
    (setf (tempus::bitp (tempus::prefs-of alice) tempus::+pref-brief+) t)
    (tempus::interpret-command alice "e")
    (is-true (search "East Goddess Street" (char-output alice)))
    (is-false (search "The broad tree-lined avenue leads east"
                      (char-output alice)))))
