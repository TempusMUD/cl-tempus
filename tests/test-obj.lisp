(in-package #:tempus.tests)

(in-suite* #:tempus.obj :in :tempus)

(test objs-containing-objs
  (let ((obj-a (make-mock-object))
        (obj-b (make-mock-object)))
    (tempus::obj-to-obj obj-a obj-b)
    (is (equal (list obj-a) (tempus::contains-of obj-b)))
    (is (equal (tempus::in-obj-of obj-a) obj-b))
    (tempus::obj-from-obj obj-a)
    (is-false (tempus::contains-of obj-b))
    (is-false (tempus::in-obj-of obj-a))))

(test can-carry-items
  (with-mock-players (alice)
    (setf (tempus::dex-of (tempus::aff-abils-of alice)) 11)
    (setf (tempus::level-of alice) 40)
    (is (= 12 (tempus::can-carry-items alice)))
    (tempus::affect-modify alice 0 0 tempus::+aff2-telekinesis+ 2 t)
    (is (= 22 (tempus::can-carry-items alice)))
    (setf (tempus::level-of alice) 55)
    (is (> (tempus::can-carry-items alice) 10000))))

(test can-carry-weight
  (with-mock-players (alice)
    (setf (tempus::str-of (tempus::aff-abils-of alice)) 1)
    (is (= 10 (tempus::can-carry-weight alice)))
    (setf (tempus::str-of (tempus::aff-abils-of alice)) 12)
    (is (= 140 (tempus::can-carry-weight alice)))))

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

(test obj-implant
  (with-mock-players (alice)
    (let ((obj (make-mock-object)))
      ;; Make the object give glowlight and wis+2
      (setf (tempus::kind-of obj) tempus::+item-armor+)
      (setf (tempus::wear-flags-of obj) (logior tempus::+item-wear-body+
                                                tempus::+item-wear-take+))
      (setf (tempus::extra2-flags-of obj) tempus::+item2-implant+)
      (setf (aref (tempus::bitvector-of obj) 0) tempus::+aff-glowlight+)
      (setf (tempus::location-of (aref (tempus::affected-of obj) 0)) tempus::+apply-wis+)
      (setf (tempus::modifier-of (aref (tempus::affected-of obj) 0)) 2)
      (setf (tempus::wis-of (tempus::real-abils-of alice)) 10)
      (setf (tempus::wis-of (tempus::aff-abils-of alice)) 10)
      (tempus::equip-char alice obj tempus::+wear-body+ :implant)
      (is (eql alice (tempus::worn-by-of obj)))
      (is (eql tempus::+wear-body+ (tempus::worn-on-of obj)))
      (is (eql 12 (tempus::wis-of (tempus::aff-abils-of alice))))
      (is-true (logtest (tempus::aff-flags-of alice) tempus::+aff-glowlight+)))))

(test affect-total
  (with-mock-players (alice)
    (let ((equip (make-mock-object))
          (implant (make-mock-object)))
      ;; Set up alice
      (setf (tempus::max-hitp-of alice) 100)
      (setf (tempus::max-mana-of alice) 100)
      (setf (tempus::max-move-of alice) 100)
      ;; Set up equipment
      (setf (tempus::location-of (aref (tempus::affected-of equip) 0)) tempus::+apply-hit+)
      (setf (tempus::modifier-of (aref (tempus::affected-of equip) 0)) 20)
      ;; Set up implant
      (setf (tempus::extra2-flags-of implant) tempus::+item2-implant+)
      (setf (tempus::location-of (aref (tempus::affected-of implant) 0)) tempus::+apply-mana+)
      (setf (tempus::modifier-of (aref (tempus::affected-of implant) 0)) 20)
      (tempus::equip-char alice equip tempus::+wear-body+ :worn)
      (is (eql 120 (tempus::max-hitp-of alice)))
      (tempus::equip-char alice implant tempus::+wear-body+ :implant)
      (is (eql 120 (tempus::max-mana-of alice)))
      (tempus::affect-total alice)
      (is (eql 120 (tempus::max-hitp-of alice)))
      (is (eql 120 (tempus::max-mana-of alice))))))



(defmacro object-command-test (&body body)
  `(with-mock-players (alice bob)
     (let ((armor-1 nil)
           (armor-2 nil)
           (chest nil))
       (unwind-protect
            (progn
              (setf armor-1 (make-mock-object "some plate armor"))
              (setf armor-2 (make-mock-object "some plate armor"))
              (setf chest (make-mock-object "a treasure chest"))
              (setf (tempus::wear-flags-of armor-1) tempus::+item-wear-take+)
              (setf (tempus::wear-flags-of armor-2) tempus::+item-wear-take+)
              (macrolet ((do-cmd (command-str)
                           `(tempus::interpret-command alice ,command-str))
                         (self-emit-is (emit-str)
                           `(is (equal ,emit-str (char-output alice))))
                         (other-emit-is (emit-str)
                           `(is (equal ,emit-str (char-output bob)))))
                ,@body))
         (progn
           (tempus::extract-obj armor-1)
           (tempus::extract-obj armor-2)
           (tempus::extract-obj chest))))))

(test get-command-ok
  (with-mock-players (alice bob)
    (let ((obj nil))
      (unwind-protect
           (progn
             (setf obj (make-mock-object "some plate armor"))
             (setf (tempus::wear-flags-of obj) tempus::+item-wear-take+)
             (tempus::obj-to-room obj (tempus::in-room-of alice))
             (tempus::interpret-command alice "get armor")
             (is (eql (tempus::carried-by-of obj) alice))
             (is (null (tempus::in-room-of obj)))
             (is (equal (list obj) (tempus::carrying-of alice)))
             (is (equal "You get some plate armor.~%" (char-output alice)))
             (is (equal "Alice gets some plate armor.~%" (char-output bob))))
        (tempus::extract-obj obj)))))

(test get-all-command
  (with-mock-players (alice)
    (let ((obj-a nil)
          (obj-b nil)
          (obj-c nil))
      (unwind-protect
           (progn
             (setf obj-a (make-mock-object "some plate armor"))
             (setf obj-b (make-mock-object "some plate armor"))
             (setf obj-c (make-mock-object "some plate armor"))
             (setf (tempus::wear-flags-of obj-a) tempus::+item-wear-take+)
             (setf (tempus::wear-flags-of obj-b) tempus::+item-wear-take+)
             (setf (tempus::wear-flags-of obj-c) tempus::+item-wear-take+)
             (tempus::obj-to-room obj-a (tempus::in-room-of alice))
             (tempus::obj-to-room obj-b (tempus::in-room-of alice))
             (tempus::obj-to-room obj-c (tempus::in-room-of alice))
             (tempus::interpret-command alice "get all")
             (is (equal "You get some plate armor. (x3)~%" (char-output alice)))
             (is (eql alice (tempus::carried-by-of obj-a)))
             (is (eql alice (tempus::carried-by-of obj-b)))
             (is (eql alice (tempus::carried-by-of obj-c))))
        (progn
          (tempus::extract-obj obj-a)
          (tempus::extract-obj obj-b)
          (tempus::extract-obj obj-c))))))

(test get-all-dot-command
  (with-mock-players (alice)
    (let ((obj-a nil)
          (obj-b nil)
          (obj-c nil))
      (unwind-protect
           (progn
             (setf obj-a (make-mock-object "some plate armor"))
             (setf obj-b (make-mock-object "some plate armor"))
             (setf obj-c (make-mock-object "some plate armor"))
             (setf (tempus::wear-flags-of obj-a) tempus::+item-wear-take+)
             (setf (tempus::wear-flags-of obj-b) tempus::+item-wear-take+)
             (setf (tempus::wear-flags-of obj-c) tempus::+item-wear-take+)
             (tempus::obj-to-room obj-a (tempus::in-room-of alice))
             (tempus::obj-to-room obj-b (tempus::in-room-of alice))
             (tempus::obj-to-room obj-c (tempus::in-room-of alice))
             (tempus::interpret-command alice "get all.armor")
             (is (equal "You get some plate armor. (x3)~%" (char-output alice)))
             (is (eql alice (tempus::carried-by-of obj-a)))
             (is (eql alice (tempus::carried-by-of obj-b)))
             (is (eql alice (tempus::carried-by-of obj-c))))
        (progn
          (tempus::extract-obj obj-a)
          (tempus::extract-obj obj-b)
          (tempus::extract-obj obj-c))))))

(test get-command-no-take
  (with-mock-players (alice)
    (let ((obj (make-mock-object "some plate armor")))
      (unwind-protect
           (progn
             (tempus::obj-to-room obj (tempus::in-room-of alice))
             (tempus::interpret-command alice "get armor")
             (is (eql (tempus::in-room-of obj) (tempus::in-room-of alice)))
             (is (null (tempus::carried-by-of obj)))
             (is (equal "Some plate armor: you can't take that!~%" (char-output alice))))
        (tempus::extract-obj obj)))))


(test get-command-imm-no-take
  (with-mock-players (alice)
    (let ((obj nil))
      (unwind-protect
           (progn
             (setf obj (make-mock-object "some plate armor"))
             (setf (tempus::level-of alice) 55)
             (tempus::obj-to-room obj (tempus::in-room-of alice))
             (tempus::interpret-command alice "get armor")
             (is (eql (tempus::carried-by-of obj) alice))
             (is (null (tempus::in-room-of obj)))
             (is (equal "You get some plate armor.~%" (char-output alice))))
        (tempus::extract-obj obj)))))

(test get-from-command
  (with-mock-players (alice bob)
    (let ((obj-a nil)
          (obj-b nil))
      (unwind-protect
           (progn
             (setf obj-a (make-mock-object "some plate armor"))
             (setf obj-b (make-mock-object "a treasure chest"))
             (setf (tempus::wear-flags-of obj-a) tempus::+item-wear-take+)
             (setf (tempus::wear-flags-of obj-b) tempus::+item-wear-take+)
             (tempus::obj-to-room obj-b (tempus::in-room-of alice))
             (tempus::obj-to-obj obj-a obj-b)
             (tempus::interpret-command alice "get armor from chest")
             (is (equal "You get some plate armor from a treasure chest.~%" (char-output alice)))
             (is (equal "Alice gets some plate armor from a treasure chest.~%" (char-output bob)))
             (is (eql alice (tempus::carried-by-of obj-a))))
        (progn
          (tempus::extract-obj obj-a)
          (tempus::extract-obj obj-b))))))

(test get-all-from-command
  (with-mock-players (alice bob)
    (let ((obj-a nil)
          (obj-b nil)
          (obj-c nil))
      (unwind-protect
           (progn
             (setf obj-a (make-mock-object "some plate armor"))
             (setf obj-b (make-mock-object "some plate armor"))
             (setf obj-c (make-mock-object "a treasure chest"))
             (setf (tempus::wear-flags-of obj-a) tempus::+item-wear-take+)
             (setf (tempus::wear-flags-of obj-b) tempus::+item-wear-take+)
             (tempus::obj-to-room obj-c (tempus::in-room-of alice))
             (tempus::obj-to-obj obj-a obj-c)
             (tempus::obj-to-obj obj-b obj-c)
             (tempus::interpret-command alice "get all.armor from chest")
             (is (equal "You get some plate armor from a treasure chest. (x2)~%" (char-output alice)))
             (is (equal "Alice gets some plate armor from a treasure chest. (x2)~%" (char-output bob)))
             (is (eql alice (tempus::carried-by-of obj-a)))
             (is (eql alice (tempus::carried-by-of obj-b))))
        (progn
          (tempus::extract-obj obj-a)
          (tempus::extract-obj obj-b)
          (tempus::extract-obj obj-c))))))

(test inventory-command
  (with-mock-players (alice)
    (let ((obj nil))
      (unwind-protect
           (progn
             (setf obj (make-mock-object "some plate armor"))
             (tempus::obj-to-char obj alice)
             (tempus::interpret-command alice "i")
             (is (equal "You are carrying:~%some plate armor~%" (char-output alice))))
        (tempus::extract-obj obj)))))

(test drop-command
  (object-command-test
    (tempus::obj-to-char armor-2 alice)
    (tempus::obj-to-char armor-1 alice)
    (do-cmd "drop armor")
    (self-emit-is "You drop some plate armor.~%")
    (other-emit-is "Alice drops some plate armor.~%")
    (is (eql (tempus::in-room-of alice) (tempus::in-room-of armor-1))))

  (object-command-test
    (tempus::obj-to-char armor-2 alice)
    (tempus::obj-to-char armor-1 alice)
    (do-cmd "drop all")
    (self-emit-is "You drop some plate armor. (x2)~%")
    (other-emit-is "Alice drops some plate armor. (x2)~%")
    (is (eql (tempus::in-room-of alice) (tempus::in-room-of armor-1)))
    (is (eql (tempus::in-room-of alice) (tempus::in-room-of armor-2)))))

(test drop-command-cursed
  (object-command-test
    (tempus::obj-to-char armor-2 alice)
    (tempus::obj-to-char armor-1 alice)
    (setf (tempus::extra-flags-of armor-1)
          (logior (tempus::extra-flags-of armor-1) tempus::+item-nodrop+))
    (do-cmd "drop armor")
    (self-emit-is "You can't drop some plate armor, it must be CURSED!~%")
    (is (eql alice (tempus::carried-by-of armor-1)))))

(test drop-command-cursed-imm
  (object-command-test
    (tempus::obj-to-char armor-2 alice)
    (tempus::obj-to-char armor-1 alice)
    (setf (tempus::level-of alice) 70)
    (setf (tempus::extra-flags-of armor-1)
          (logior (tempus::extra-flags-of armor-1) tempus::+item-nodrop+))
    (do-cmd "drop armor")
    (self-emit-is "You peel some plate armor off your hand...~%You drop some plate armor.~%")
    (is (eql (tempus::in-room-of alice) (tempus::in-room-of armor-1)))))
