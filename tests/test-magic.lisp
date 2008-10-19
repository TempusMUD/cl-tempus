(in-package #:tempus.tests)

(in-suite (defsuite (tempus.magic :in test)))

(deftest mag-savingthrow/wizard/makes-save ()
  (with-mock-players (alice)
    (setf (tempus::level-of alice) (1+ tempus::+lvl-god+))
    (is (tempus::mag-savingthrow alice 50 tempus::+saving-para+))))

(deftest mag-savingthrow/level-greater-than-100/fails-save ()
  (with-mock-players (alice)
    (is (not (tempus::mag-savingthrow alice 101 tempus::+saving-para+)))))

(deftest mag-savingthrow/save-none/fails-save ()
  (with-mock-players (alice)
    (is (not (tempus::mag-savingthrow alice 100 tempus::+saving-none+)))))

(deftest mag-savingthrow/normal/returns-t-or-nil ()
  (with-mock-players (alice)
    (let ((result (tempus::mag-savingthrow alice 50 tempus::+saving-none+)))
      (is (or (eql result t) (eql result nil))))))

(deftest explode-all-sigils/normal/calls-explode-sigil ()
  (with-mock-players (alice)
    (with-mock-objects ((tunic "a leather tunic")
                        (cap "a leather cap"))
      (setf (tempus::sigil-idnum-of tunic) 1)
      (setf (tempus::sigil-idnum-of cap) 1)
      (setf (tempus::sigil-level-of tunic) 10)
      (setf (tempus::sigil-level-of cap) 10)
      (tempus::obj-to-char tunic alice)
      (tempus::obj-to-char cap alice)
      (function-trace-bind ((calls tempus::explode-sigil))
          (tempus::explode-all-sigils alice)
        (is (= 2 (length calls)))))))

(deftest explode-sigil/normal/calls-damage ()
  (with-mock-players (alice)
    (with-mock-objects ((tunic "a leather tunic"))
      (tempus::char-from-room alice)
      (tempus::char-to-room alice (tempus::real-room 7100))
      (setf (tempus::sigil-idnum-of tunic) 1)
      (setf (tempus::sigil-level-of tunic) 10)
      (tempus::obj-to-char tunic alice)
      (function-trace-bind ((calls tempus::damage))
          (tempus::explode-sigil alice tunic)
        (is (= 1 (length calls)))
        (is (equal "A leather tunic explodes when you pick it up!!~%"
                   (char-output alice)))))))

(deftest explode-sigil/peaceful-room/emits-error ()
  (with-mock-players (alice)
    (with-mock-objects ((tunic "a leather tunic"))
      (setf (tempus::sigil-idnum-of tunic) 1)
      (setf (tempus::sigil-level-of tunic) 10)
      (tempus::obj-to-char tunic alice)
      (function-trace-bind ((calls tempus::damage))
          (tempus::explode-sigil alice tunic)
        (is (zerop (length calls)))
        (is (equal "A leather tunic feels rather warm to the touch and shudders violently.~%"
                   (char-output alice)))))))