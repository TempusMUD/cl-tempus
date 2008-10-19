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