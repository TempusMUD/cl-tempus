(in-package #:tempus.tests)

(in-suite (defsuite (tempus.wizard :in test)))

(deftest force-command ()
  (with-mock-players (alice bob)
    (function-trace-bind ((calls tempus::interpret-command))
        (tempus::interpret-command alice "force bob to dance")
      (is (equal "You got it.~%" (char-output alice)))
      (is (= (length calls) 2))
      (is (eql (first (first calls)) bob))
      (is (equal (second (first calls)) "dance")))))
