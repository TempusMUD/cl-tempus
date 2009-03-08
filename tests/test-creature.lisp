(in-package #:tempus.tests)

(in-suite (defsuite (tempus.creature :in test)))

(deftest gain-condition ()
  (with-fixtures ((alice mock-player))
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

(deftest affect-to-char ()
  (with-fixtures ((alice mock-player))
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
      (is (tempus::affected-by-spell alice tempus::+spell-invisible+)))))

(deftest extract-creature/with-mobile/extracted ()
  (let ((mob (tempus::read-mobile 1201)))
    (tempus::char-to-room mob (tempus::real-room 100))
    (tempus::extract-creature mob 'disconnecting)
    (is (not (member mob tempus::*characters*)))
    (is (null (tempus::in-room-of mob)))))

(deftest extract-creature/with-player/extracted ()
  (with-fixtures ((alice mock-player))
    (tempus::extract-creature alice 'tempus::disconnecting)
    (is (not (member alice tempus::*characters*)))
    (is (null (tempus::in-room-of alice)))
    (is (eql 'tempus::disconnecting (tempus::state-of (tempus::link-of alice))))))
