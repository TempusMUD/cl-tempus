(in-package #:tempus.tests)

(in-suite (defsuite (tempus.clan :in test)))

(deftest create-and-destroy-clan/normal/clan-created ()
  (let ((clan-id (+ 900 (random 100))))
    (unwind-protect
         (let ((clan (tempus::create-clan clan-id)))
           (is (not (null clan)))
           (is (= (tempus::idnum-of clan) clan-id)))
      (tempus::delete-clan clan-id))
    (is (null (tempus::real-clan clan-id)))))

(deftest add-clan-member/with-player/clan-member-added ()
  (with-mock-clan (clan)
    (with-full-mock-players (alice)
      (setf (tempus::plr-bits-of alice) tempus::+plr-clan-leader+)
      (tempus::add-clan-member alice clan)
      (is (= (tempus::clan-of alice) (tempus::idnum-of clan)))
      (is (zerop (logand (tempus::plr-bits-of alice) tempus::+plr-clan-leader+)))
      (tempus::remove-clan-member alice clan))))