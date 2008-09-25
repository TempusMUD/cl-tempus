(in-package #:tempus.tests)

(in-suite* #:tempus.obj :in :tempus)

(test objs-containing-objs
  (with-mock-objects ((obj-a "obj-a")
                      (obj-b "obj-b"))
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
    (with-mock-objects ((obj "a magic ring"))
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
    (with-mock-objects ((obj "a magic ring"))
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
    (with-mock-objects ((obj "a magic ring"))
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
    (with-mock-objects ((equip "a magic ring")
                        (implant "an internal booster"))
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
     (with-mock-objects ((armor-1 "some plate armor")
                         (armor-2 "some plate armor")
                         (chest "a treasure chest"))
       (setf (tempus::sex-of alice) 'tempus::female)
       (setf (tempus::wear-flags-of armor-1) (logior
                                              tempus::+item-wear-take+
                                              tempus::+item-wear-body+))
       (setf (tempus::wear-flags-of armor-2) (logior
                                              tempus::+item-wear-take+
                                              tempus::+item-wear-body+))
       (macrolet ((do-cmd (command-str)
                    `(tempus::interpret-command alice ,command-str))
                  (self-emit-is (emit-str)
                    `(is (equal ,emit-str (char-output alice))))
                  (other-emit-is (emit-str)
                    `(is (equal ,emit-str (char-output bob)))))
         ,@body))))

(test get-command-ok
  (with-mock-players (alice bob)
    (with-mock-objects ((obj "some plate armor"))
      (setf (tempus::wear-flags-of obj) tempus::+item-wear-take+)
      (tempus::obj-to-room obj (tempus::in-room-of alice))
      (tempus::interpret-command alice "get armor")
      (is (eql (tempus::carried-by-of obj) alice))
      (is (null (tempus::in-room-of obj)))
      (is (equal (list obj) (tempus::carrying-of alice)))
      (is (equal "You get some plate armor.~%" (char-output alice)))
      (is (equal "Alice gets some plate armor.~%" (char-output bob))))))

(test get-all-command
  (with-mock-players (alice)
    (with-mock-objects ((armor-1 "some plate armor")
                        (armor-2 "some plate armor")
                        (chest "a treasure chest"))
      (setf (tempus::wear-flags-of armor-1) tempus::+item-wear-take+)
      (setf (tempus::wear-flags-of armor-2) tempus::+item-wear-take+)
      (setf (tempus::wear-flags-of chest) tempus::+item-wear-take+)
      (tempus::obj-to-room armor-1 (tempus::in-room-of alice))
      (tempus::obj-to-room armor-2 (tempus::in-room-of alice))
      (tempus::obj-to-room chest (tempus::in-room-of alice))
      (tempus::interpret-command alice "get all")
      (is (equal "You get a treasure chest.~%You get some plate armor. (x2)~%" (char-output alice)))
      (is (eql alice (tempus::carried-by-of armor-1)))
      (is (eql alice (tempus::carried-by-of armor-2)))
      (is (eql alice (tempus::carried-by-of chest))))))

(test get-all-dot-command
  (with-mock-players (alice)
    (with-mock-objects ((armor-1 "some plate armor")
                        (armor-2 "some plate armor")
                        (chest "a treasure chest"))
      (setf (tempus::wear-flags-of armor-1) tempus::+item-wear-take+)
      (setf (tempus::wear-flags-of armor-2) tempus::+item-wear-take+)
      (setf (tempus::wear-flags-of chest) tempus::+item-wear-take+)
      (tempus::obj-to-room armor-1 (tempus::in-room-of alice))
      (tempus::obj-to-room armor-2 (tempus::in-room-of alice))
      (tempus::obj-to-room chest (tempus::in-room-of alice))
      (tempus::interpret-command alice "get all.armor")
      (is (equal "You get some plate armor. (x2)~%" (char-output alice)))
      (is (eql alice (tempus::carried-by-of armor-1)))
      (is (eql alice (tempus::carried-by-of armor-2))))))

(test get-command-no-take
  (with-mock-players (alice)
    (with-mock-objects ((obj "some plate armor"))
      (tempus::obj-to-room obj (tempus::in-room-of alice))
      (tempus::interpret-command alice "get armor")
      (is (eql (tempus::in-room-of obj) (tempus::in-room-of alice)))
      (is (null (tempus::carried-by-of obj)))
      (is (equal "Some plate armor: you can't take that!~%" (char-output alice))))))


(test get-command-imm-no-take
  (with-mock-players (alice)
    (with-mock-objects ((obj "some plate armor"))
      (setf (tempus::level-of alice) 55)
      (tempus::obj-to-room obj (tempus::in-room-of alice))
      (tempus::interpret-command alice "get armor")
      (is (eql (tempus::carried-by-of obj) alice))
      (is (null (tempus::in-room-of obj)))
      (is (equal "You get some plate armor.~%" (char-output alice))))))

(test get-gold
  (with-mock-players (alice)
    (with-mock-objects ((obj "a pile of gold"))
      (setf obj (make-mock-object "a pile of gold"))
      (setf (tempus::wear-flags-of obj) tempus::+item-wear-take+)
      (setf (tempus::kind-of obj) tempus::+item-money+)
      (setf (aref (tempus::value-of obj) 0) 12345)
      (setf (aref (tempus::value-of obj) 1) 0)
      (tempus::obj-to-room obj (tempus::in-room-of alice))
      (tempus::interpret-command alice "get gold")
      (is (null (tempus::carrying-of alice)))
      (is (= 12345 (tempus::gold-of alice)))
      (is (equal "You get a pile of gold.~%There were 12345 coins.~%" (char-output alice))))))

(test get-cash
  (with-mock-players (alice)
    (with-mock-objects ((obj "a pile of cash"))
      (setf (tempus::wear-flags-of obj) tempus::+item-wear-take+)
      (setf (tempus::kind-of obj) tempus::+item-money+)
      (setf (aref (tempus::value-of obj) 0) 12345)
      (setf (aref (tempus::value-of obj) 1) 1)
      (tempus::obj-to-room obj (tempus::in-room-of alice))
      (tempus::interpret-command alice "get cash")
      (is (null (tempus::carrying-of alice)))
      (is (= 12345 (tempus::cash-of alice)))
      (is (equal "You get a pile of cash.~%There were 12345 credits.~%" (char-output alice))))))

(test get-from-command
  (with-mock-players (alice bob)
    (with-mock-objects ((armor-1 "some plate armor")
                        (chest "a treasure chest"))
      (setf (tempus::wear-flags-of armor-1) tempus::+item-wear-take+)
      (setf (tempus::wear-flags-of chest) tempus::+item-wear-take+)
      (tempus::obj-to-room chest (tempus::in-room-of alice))
      (tempus::obj-to-obj armor-1 chest)
      (tempus::interpret-command alice "get armor from chest")
      (is (equal "You get some plate armor from a treasure chest.~%" (char-output alice)))
      (is (equal "Alice gets some plate armor from a treasure chest.~%" (char-output bob)))
      (is (eql alice (tempus::carried-by-of armor-1))))))

(test get-all-from-command
  (with-mock-players (alice bob)
    (with-mock-objects ((armor-1 "some plate armor")
                        (armor-2 "some plate armor")
                        (chest "a treasure chest"))
      (setf (tempus::wear-flags-of armor-1) tempus::+item-wear-take+)
      (setf (tempus::wear-flags-of armor-2) tempus::+item-wear-take+)
      (tempus::obj-to-room chest (tempus::in-room-of alice))
      (tempus::obj-to-obj armor-1 chest)
      (tempus::obj-to-obj armor-2 chest)
      (tempus::interpret-command alice "get all.armor from chest")
      (is (equal "You get some plate armor from a treasure chest. (x2)~%" (char-output alice)))
      (is (equal "Alice gets some plate armor from a treasure chest. (x2)~%" (char-output bob)))
      (is (eql alice (tempus::carried-by-of armor-1)))
      (is (eql alice (tempus::carried-by-of armor-2))))))

(test inventory-command
  (with-mock-players (alice)
    (with-mock-objects ((obj "some plate armor"))
             (tempus::obj-to-char obj alice)
             (tempus::interpret-command alice "i")
             (is (equal "You are carrying:~%some plate armor~%" (char-output alice))))))

(test put-command
  (object-command-test
    (tempus::obj-to-char armor-2 alice)
    (tempus::obj-to-char armor-1 alice)
    (tempus::obj-to-room chest (tempus::in-room-of alice))
    (do-cmd "put armor into chest")
    (self-emit-is "You put some plate armor into a treasure chest.~%")
    (other-emit-is "Alice puts some plate armor into a treasure chest.~%")
    (is (eql chest (tempus::in-obj-of armor-1)))))

(test put-command-no-container
  (object-command-test
    (tempus::obj-to-char armor-2 alice)
    (tempus::obj-to-char armor-1 alice)
    (tempus::obj-to-room chest (tempus::in-room-of alice))
    (do-cmd "put armor")
    (self-emit-is "What do you want to put it in?~%")))

(test put-command-numbered
  (object-command-test
    (tempus::obj-to-char armor-2 alice)
    (tempus::obj-to-char armor-1 alice)
    (tempus::obj-to-room chest (tempus::in-room-of alice))
    (do-cmd "put 2.armor into chest")
    (self-emit-is "You put some plate armor into a treasure chest.~%")
    (other-emit-is "Alice puts some plate armor into a treasure chest.~%")
   (is (eql chest (tempus::in-obj-of armor-2)))))

(test put-command-all
  (object-command-test
    (tempus::obj-to-char armor-2 alice)
    (tempus::obj-to-char armor-1 alice)
    (tempus::obj-to-room chest (tempus::in-room-of alice))
    (do-cmd "put all into chest")
    (self-emit-is "You put some plate armor into a treasure chest. (x2)~%")
    (other-emit-is "Alice puts some plate armor into a treasure chest. (x2)~%")
    (is (eql chest (tempus::in-obj-of armor-1)))
    (is (eql chest (tempus::in-obj-of armor-2)))))

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
    (is (eql alice (tempus::carried-by-of armor-1)))
    ;; Check immortal drop
    (clear-mock-buffers alice bob)
    (setf (tempus::level-of alice) 70)
    (do-cmd "drop armor")
    (self-emit-is "You peel some plate armor off your hand...~%You drop some plate armor.~%")
    (is (eql (tempus::in-room-of alice) (tempus::in-room-of armor-1)))))

(test wear-command
  (object-command-test
    (tempus::obj-to-char armor-2 alice)
    (tempus::obj-to-char armor-1 alice)
    (do-cmd "wear armor")
    (self-emit-is "You wear some plate armor on your body.~%")
    (other-emit-is "Alice wears some plate armor on her body.~%")
    (is (null (tempus::carried-by-of armor-1)))
    (is (not (member armor-1 (tempus::carrying-of alice))))
    (is (eql alice (tempus::worn-by-of armor-1)))
    (is (eql tempus::+wear-body+ (tempus::worn-on-of armor-1)))
    (is (eql armor-1 (aref (tempus::equipment-of alice) tempus::+wear-body+)))
    (clear-mock-buffers alice bob)
    (do-cmd "wear armor")
    (self-emit-is "You're already wearing something on your body.~%")))

(test wear-command-failure
  (object-command-test
    (tempus::obj-to-char armor-1 alice)
    (setf (tempus::wear-flags-of armor-1) tempus::+item-wear-hold+)
    (do-cmd "wear armor")
    (self-emit-is "You can't wear some plate armor.~%")))

(test wear-command-on-pos
  (object-command-test
    (tempus::obj-to-char armor-2 alice)
    (tempus::obj-to-char armor-1 alice)
    (do-cmd "wear armor on body")
    (self-emit-is "You wear some plate armor on your body.~%")
    (other-emit-is "Alice wears some plate armor on her body.~%")
    (is (null (tempus::carried-by-of armor-1)))
    (is (not (member armor-1 (tempus::carrying-of alice))))
    (is (eql alice (tempus::worn-by-of armor-1)))
    (is (eql tempus::+wear-body+ (tempus::worn-on-of armor-1)))
    (is (eql armor-1 (aref (tempus::equipment-of alice) tempus::+wear-body+)))
    (clear-mock-buffers alice bob)
    (do-cmd "wear armor on eyes")
    (self-emit-is "You can't wear some plate armor there.~%")))

(test wear-command-on-pos-failure
  (object-command-test
    (tempus::obj-to-char armor-2 alice)
    (tempus::obj-to-char armor-1 alice)
    (do-cmd "wear all.armor on body")
    (self-emit-is "You can't wear more than one item on a position.~%")
    (clear-mock-buffers alice)
    (do-cmd "wear armor on foo")
    (self-emit-is "'foo'?  What part of your body is THAT?~%")))


(test remove-command
  (object-command-test
    (tempus::equip-char alice armor-1 tempus::+wear-body+ :worn)
    (do-cmd "remove armor")
    (self-emit-is "You stop using some plate armor.~%")
    (other-emit-is "Alice stops using some plate armor.~%")
    (is (null (tempus::worn-by-of armor-1)))
    (is (equal (list armor-1) (tempus::carrying-of alice)))))

(test remove-command-from-pos
  (object-command-test
    (tempus::equip-char alice armor-1 tempus::+wear-body+ :worn)
    (do-cmd "remove earring from body")
    (self-emit-is "You aren't wearing an earring there.~%")
    (clear-mock-buffers alice)
    (do-cmd "remove armor from body")
    (self-emit-is "You stop using some plate armor.~%")
    (other-emit-is "Alice stops using some plate armor.~%")
    (is (null (tempus::worn-by-of armor-1)))
    (is (equal (list armor-1) (tempus::carrying-of alice)))
    (clear-mock-buffers alice)
    (do-cmd "remove armor from arms")
    (self-emit-is "You aren't wearing anything there.~%")))
