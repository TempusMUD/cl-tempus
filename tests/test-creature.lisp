(in-package #:tempus.tests)

(in-suite* #:tempus.creature :in :tempus)

(test gain-condition
  (with-mock-players (alice)
    (setf (tempus::conditions-of alice) (coerce '(12 12 12) 'vector))
    (tempus::gain-condition alice tempus::+full+ 5)
    (is (= 17 (tempus::get-condition alice tempus::+full+)))
    (tempus::gain-condition alice tempus::+thirst+ 5)
    (is (= 17 (tempus::get-condition alice tempus::+thirst+)))
    (tempus::gain-condition alice tempus::+drunk+ 5)
    (is (= 17 (tempus::get-condition alice tempus::+drunk+)))
    (tempus::gain-condition alice tempus::+full+ 10)
    (is (= 24 (tempus::get-condition alice tempus::+full+)))
    (tempus::gain-condition alice tempus::+thirst+ 10)
    (is (= 24 (tempus::get-condition alice tempus::+thirst+)))
    (tempus::gain-condition alice tempus::+drunk+ 10)
    (is (= 24 (tempus::get-condition alice tempus::+drunk+)))))

(test affect-to-char
  (with-mock-players (alice)
    (let ((af (make-instance 'tempus::affected-type
                             :kind tempus::+spell-invisible+
                             :location tempus::+apply-hit+
                             :modifier 10
                             :bitvector tempus::+aff-invisible+
                             :aff-index 1)))
      (setf (tempus::max-hitp-of alice) 100)
      (setf (tempus::hitp-of alice) 100)
      (tempus::affect-to-char alice af)
      (is (= (tempus::max-hitp-of alice) 110))
      (is (equal (list af) (tempus::affected-of alice)))
      (is-true (tempus::affected-by-spell alice tempus::+spell-invisible+)))))