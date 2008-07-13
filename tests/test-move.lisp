(in-package #:tempus.tests)

(in-suite* #:tempus.move :in :tempus)

(test basic-movement
  (with-mock-players (alice bob)
    (let ((orig-room (tempus::in-room-of alice)))
      (setf (tempus::bitp (tempus::prefs-of alice) tempus::+pref-autoexit+) t)
      (tempus::interpret-command alice "e")
      (is (eql (tempus::to-room-of
                (aref (tempus::dir-option-of orig-room) tempus::+east+))
               (tempus::number-of (tempus::in-room-of alice))))
      (is-true (search "East Goddess Street" (char-output alice)))
      (is-true (search "[ Exits: n e s w u ]" (char-output alice)))
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

(test standing
  (with-mock-players (alice)
    (setf (tempus::position-of alice) tempus::+pos-sitting+)
    (tempus::interpret-command alice "stand")
    (is (= (tempus::position-of alice) tempus::+pos-standing+))
    (is (string= (char-output alice) "You clamber to your feet.~%"))

    (clear-mock-buffers alice)
    (setf (tempus::position-of alice) tempus::+pos-resting+)
    (tempus::interpret-command alice "stand")
    (is (= (tempus::position-of alice) tempus::+pos-standing+))
    (is (string= (char-output alice) "You stop resting, and clamber onto your feet.~%"))

    (clear-mock-buffers alice)
    (setf (tempus::position-of alice) tempus::+pos-sleeping+)
    (tempus::interpret-command alice "stand")
    (is (= (tempus::position-of alice) tempus::+pos-standing+))
    (is (string= (char-output alice) "You wake up, and stagger to your feet.~%"))
    (clear-mock-buffers alice)
    (setf (tempus::position-of alice) tempus::+pos-standing+)
    (tempus::interpret-command alice "stand")
    (is (= (tempus::position-of alice) tempus::+pos-standing+))
    (is (string= (char-output alice) "You are already standing.~%"))

    (clear-mock-buffers alice)
    (setf (tempus::position-of alice) tempus::+pos-fighting+)
    (tempus::interpret-command alice "stand")
    (is (= (tempus::position-of alice) tempus::+pos-fighting+))
    (is (string= (char-output alice) "You are already standing.~%"))

    (clear-mock-buffers alice)
    (setf (tempus::position-of alice) tempus::+pos-flying+)
    (tempus::interpret-command alice "stand")
    (is (= (tempus::position-of alice) tempus::+pos-standing+))
    (is (string= (char-output alice) "You settle lightly to the ground.~%"))))

(test resting
  (with-mock-players (alice)
    (setf (tempus::position-of alice) tempus::+pos-sitting+)
    (tempus::interpret-command alice "rest")
    (is (= (tempus::position-of alice) tempus::+pos-resting+))
    (is (string= (char-output alice) "You lay back and rest your tired bones.~%"))

    (clear-mock-buffers alice)
    (setf (tempus::position-of alice) tempus::+pos-resting+)
    (tempus::interpret-command alice "rest")
    (is (= (tempus::position-of alice) tempus::+pos-resting+))
    (is (string= (char-output alice) "You are already resting.~%"))

    (clear-mock-buffers alice)
    (setf (tempus::position-of alice) tempus::+pos-sleeping+)
    (tempus::interpret-command alice "rest")
    (is (= (tempus::position-of alice) tempus::+pos-sleeping+))
    (is (string= (char-output alice) "You have to wake up first.~%"))
    (clear-mock-buffers alice)
    (setf (tempus::position-of alice) tempus::+pos-standing+)
    (tempus::interpret-command alice "rest")
    (is (= (tempus::position-of alice) tempus::+pos-resting+))
    (is (string= (char-output alice) "You sit down and lay back into a relaxed position.~%"))

    (clear-mock-buffers alice)
    (setf (tempus::position-of alice) tempus::+pos-fighting+)
    (tempus::interpret-command alice "rest")
    (is (= (tempus::position-of alice) tempus::+pos-fighting+))
    (is (string= (char-output alice) "Rest while fighting?  Are you MAD?~%"))

    (clear-mock-buffers alice)
    (setf (tempus::position-of alice) tempus::+pos-flying+)
    (tempus::interpret-command alice "rest")
    (is (= (tempus::position-of alice) tempus::+pos-flying+))
    (is (string= (char-output alice) "You better not try that while flying.~%"))))
  