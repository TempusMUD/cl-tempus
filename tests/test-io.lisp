(in-package #:tempus.tests)

(in-suite (defsuite (tempus.io :in test)))

(deftest apply-attribute-to-spell/granted-node/level-and-gen-set ()
  (let ((spell (make-instance 'tempus::spell-info)))
    (tempus::apply-attribute-to-spell
     spell '("granted" (("class" "mage") ("level" "13") ("gen" "5"))))
    (is (= (aref (tempus::min-level-of spell) tempus::+class-mage+) 13))
    (is (= (aref (tempus::min-gen-of spell) tempus::+class-mage+) 5))))

(deftest apply-attribute-to-spell/manacost-node/mana-costs-set ()
  (let ((spell (make-instance 'tempus::spell-info)))
    (tempus::apply-attribute-to-spell
     spell '("manacost" (("initial" "55") ("level_dec" "13") ("minimum" "5"))))
    (is (= (tempus::mana-max-of spell) 55))
    (is (= (tempus::mana-change-of spell) 13))
    (is (= (tempus::mana-min-of spell) 5))))

(deftest apply-attribute-to-spell/position-node/min-pos-set ()
  (let ((spell (make-instance 'tempus::spell-info)))
    (tempus::apply-attribute-to-spell
     spell '("position" (("minimum" "sitting"))))
    (is (= (tempus::min-position-of spell) tempus::+pos-sitting+))))

(deftest apply-attribute-to-spell/target-node/door-set ()
  (let ((spell (make-instance 'tempus::spell-info)))
    (tempus::apply-attribute-to-spell
     spell '("target" (("type" "door"))))
    (is (= (tempus::targets-of spell) tempus::+tar-door+))))

(deftest apply-attribute-to-spell/target-node/direction-set ()
  (let ((spell (make-instance 'tempus::spell-info)))
    (tempus::apply-attribute-to-spell
     spell '("target" (("type" "direction"))))
    (is (= (tempus::targets-of spell) tempus::+tar-dir+))))

(deftest apply-attribute-to-spell/target-node/self-fighting-set ()
  (let ((spell (make-instance 'tempus::spell-info)))
    (tempus::apply-attribute-to-spell
     spell '("target" (("type" "self") ("scope" "fighting"))))
    (is (= (tempus::targets-of spell) tempus::+tar-fight-self+))))

(deftest apply-attribute-to-spell/target-node/self-only-set ()
  (let ((spell (make-instance 'tempus::spell-info)))
    (tempus::apply-attribute-to-spell
     spell '("target" (("type" "self") ("scope" "only"))))
    (is (= (tempus::targets-of spell) tempus::+tar-self-only+))))

(deftest apply-attribute-to-spell/target-node/self-never-set ()
  (let ((spell (make-instance 'tempus::spell-info)))
    (tempus::apply-attribute-to-spell
     spell '("target" (("type" "self") ("scope" "never"))))
    (is (= (tempus::targets-of spell) tempus::+tar-not-self+))))

(deftest apply-attribute-to-spell/target-node/creature-room-set ()
  (let ((spell (make-instance 'tempus::spell-info)))
    (tempus::apply-attribute-to-spell
     spell '("target" (("type" "creature") ("scope" "room"))))
    (is (= (tempus::targets-of spell) tempus::+tar-char-room+))))

(deftest apply-attribute-to-spell/target-node/creature-world-set ()
  (let ((spell (make-instance 'tempus::spell-info)))
    (tempus::apply-attribute-to-spell
     spell '("target" (("type" "creature") ("scope" "world"))))
    (is (= (tempus::targets-of spell) tempus::+tar-char-world+))))

(deftest apply-attribute-to-spell/target-node/creature-fighting-set ()
  (let ((spell (make-instance 'tempus::spell-info)))
    (tempus::apply-attribute-to-spell
     spell '("target" (("type" "creature") ("scope" "fighting"))))
    (is (= (tempus::targets-of spell) tempus::+tar-fight-vict+))))

(deftest apply-attribute-to-spell/target-node/object-room-set ()
  (let ((spell (make-instance 'tempus::spell-info)))
    (tempus::apply-attribute-to-spell
     spell '("target" (("type" "object") ("scope" "room"))))
    (is (= (tempus::targets-of spell) tempus::+tar-obj-room+))))

(deftest apply-attribute-to-spell/target-node/object-world-set ()
  (let ((spell (make-instance 'tempus::spell-info)))
    (tempus::apply-attribute-to-spell
     spell '("target" (("type" "object") ("scope" "world"))))
    (is (= (tempus::targets-of spell) tempus::+tar-obj-world+))))

(deftest apply-attribute-to-spell/target-node/object-inventory-set ()
  (let ((spell (make-instance 'tempus::spell-info)))
    (tempus::apply-attribute-to-spell
     spell '("target" (("type" "object") ("scope" "inventory"))))
    (is (= (tempus::targets-of spell) tempus::+tar-obj-inv+))))

(deftest apply-attribute-to-spell/target-node/object-equip-set ()
  (let ((spell (make-instance 'tempus::spell-info)))
    (tempus::apply-attribute-to-spell
     spell '("target" (("type" "object") ("scope" "equip"))))
    (is (= (tempus::targets-of spell) tempus::+tar-obj-equip+))))

(deftest apply-attribute-to-spell/flag-node/violent-set ()
  (let ((spell (make-instance 'tempus::spell-info)))
    (tempus::apply-attribute-to-spell
     spell '("flag" (("value" "violent"))))
    (is (tempus::violentp spell))))

(deftest apply-attribute-to-spell/flag-node/unpleasant-set ()
  (let ((spell (make-instance 'tempus::spell-info)))
    (tempus::apply-attribute-to-spell
     spell '("flag" (("value" "unpleasant"))))
    (is (= (tempus::targets-of spell) tempus::+tar-unpleasant+))))

(deftest apply-attribute-to-spell/flag-node/damage-set ()
  (let ((spell (make-instance 'tempus::spell-info)))
    (tempus::apply-attribute-to-spell
     spell '("flag" (("value" "damage"))))
    (is (= (tempus::routines-of spell) tempus::+mag-damage+))))

(deftest retrieve-player-account/existing-player/returns-account-id ()
  (is (= (tempus::retrieve-player-account 259) 2)))

(deftest retrieve-player-account/nonexisting-player/returns-nil ()
  (is (null (tempus::retrieve-player-account 99999))))

(deftest retrieve-account-name/existing-account/returns-account-name ()
  (is (equal "Azimuth" (tempus::retrieve-account-name 2))))

(deftest retrieve-account-name/account-in-cache/returns-account-name ()
  (tempus::load-account "azimuth")
  (is (equal "Azimuth" (tempus::retrieve-account-name 2))))

(deftest retrieve-account-name/nonexisting-account/returns-nil ()
  (is (null (tempus::retrieve-account-name 9999))))
