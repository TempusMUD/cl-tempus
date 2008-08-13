(in-package #:tempus.tests)

(in-suite* #:tempus.obj :in :tempus)

(test affect-modify
  (with-mock-players (alice)
    (tempus::affect-modify alice tempus::+apply-wis+ 2 0 0 t)
    (is (eql (tempus::wis-of alice) 13))
    (tempus::affect-modify alice tempus::+apply-wis+ 2 0 0 nil)
    (is (eql (tempus::wis-of alice) 11))
    (tempus::affect-modify alice 0 0 tempus::+aff-glowlight+ 1 t)
    (is-true (logtest (tempus::aff-flags-of alice) tempus::+aff-glowlight+))
    (tempus::affect-modify alice 0 0 tempus::+aff-glowlight+ 1 nil)
    (is-false (logtest (tempus::aff-flags-of alice) tempus::+aff-glowlight+))))
    
(test obj-equip
  (with-mock-players (alice)
    (let ((obj (make-mock-object)))
      ;; Make the object give glowlight and wis+2
      (setf (aref (tempus::bitvector-of obj) 0) tempus::+aff-glowlight+)
      (setf (tempus::location-of (aref (tempus::affected-of obj) 0)) tempus::+apply-wis+)
      (setf (tempus::modifier-of (aref (tempus::affected-of obj) 0)) 2)
      (setf (tempus::wis-of (tempus::real-abils-of alice)) 10)
      (setf (tempus::wis-of (tempus::aff-abils-of alice)) 10)
      (tempus::equip-char alice obj tempus::+wear-body+ :worn)
      (is (eql alice (tempus::worn-by-of obj)))
      (is (eql tempus::+wear-body+ (tempus::worn-on-of obj)))
      (is (eql 12 (tempus::wis-of (tempus::aff-abils-of alice))))
      (is-true (logtest (tempus::aff-flags-of alice) tempus::+aff-glowlight+)))))

(test obj-unequip
  (with-mock-players (alice)
    (let ((obj (make-mock-object)))
      ;; Make the object give glowlight and wis+2
      (setf (aref (tempus::bitvector-of obj) 0) tempus::+aff-glowlight+)
      (setf (tempus::location-of (aref (tempus::affected-of obj) 0)) tempus::+apply-wis+)
      (setf (tempus::modifier-of (aref (tempus::affected-of obj) 0)) 2)
      (setf (tempus::wis-of (tempus::real-abils-of alice)) 10)
      (setf (tempus::wis-of (tempus::aff-abils-of alice)) 10)
      (tempus::equip-char alice obj tempus::+wear-body+ :worn)
      (tempus::unequip-char alice tempus::+wear-body+ :worn nil)
      (is (eql nil (tempus::worn-by-of obj)))
      (is (eql -1 (tempus::worn-on-of obj)))
      (is (eql 10 (tempus::wis-of (tempus::aff-abils-of alice))))
      (is-false (logtest (tempus::aff-flags-of alice) tempus::+aff-glowlight+)))))