(in-package #:tempus.tests)

(in-suite (defsuite (tempus.wizard :in test)))

(deftest echo/normal/displays-to-room ()
  (with-mock-players (alice bob chuck)
    (setf (tempus::level-of alice) 51)
    (setf (tempus::level-of bob) 49)
    (setf (tempus::level-of chuck) 52)
    (tempus::interpret-command alice "echo testing")
    (is (equal "Testing~%" (char-output alice)))
    (is (equal "Testing~%" (char-output bob)))
    (is (equal "[Alice] testing~%" (char-output chuck)))))

(deftest send/normal/sends-to-char ()
  (with-mock-players (alice bob)
    (tempus::interpret-command alice "send .bob testing")
    (is (equal "You send 'testing' to Bob.~%" (char-output alice)))
    (is (equal "Testing~%" (char-output bob)))))

(deftest at/normal/performs-command-in-target-room ()
  (with-mock-players (alice bob)
    (tempus::char-from-room alice)
    (tempus::char-to-room alice (tempus::real-room 3001))
    (tempus::interpret-command alice "at 3002 say hi")
    (is (equal "&BYou say, &c'hi'&n~%" (char-output alice)))
    (is (equal "&BAlice says, &c'hi'&n~%" (char-output bob)))))

(deftest goto/numeric-target/changes-room ()
  (with-mock-players (alice bob)
    (tempus::char-from-room alice)
    (tempus::char-to-room alice (tempus::real-room 3001))
    (tempus::interpret-command alice "goto 3002")
    (is (= 3002 (tempus::number-of (tempus::in-room-of alice))))
    (is (equal "Alice appears with an ear-splitting bang.~%" (char-output bob)))))

(deftest goto/char-target/changes-room ()
  (with-mock-players (alice bob)
    (tempus::char-from-room alice)
    (tempus::char-to-room alice (tempus::real-room 3001))
    (tempus::interpret-command alice "goto .bob")
    (is (= 3002 (tempus::number-of (tempus::in-room-of alice))))
    (is (equal "Alice appears with an ear-splitting bang.~%" (char-output bob)))))

(deftest goto/following-imm/imm-in-same-room ()
  (with-mock-players (alice bob)
    (setf (tempus::level-of bob) 51)
    (tempus::add-follower bob alice)
    (clear-mock-buffers bob alice)
    (tempus::interpret-command alice "goto 3001")
    (is (= 3001 (tempus::number-of (tempus::in-room-of alice))))
    (is (= 3001 (tempus::number-of (tempus::in-room-of bob))))
    (is (search "Bob appears with an ear-splitting bang.~%" (char-output alice)))
    (is (search "Alice disappears in a puff of smoke.~%" (char-output bob)))))

(deftest distance/valid-rooms/returns-distance ()
  (with-mock-players (alice bob)
    (tempus::interpret-command alice "distance 24800")
    (is (equal "Room 24800 is 36 steps away.~%" (char-output alice)))))

(deftest distance/no-connection/returns-error ()
  (with-mock-players (alice bob)
    (tempus::interpret-command alice "distance 43000")
    (is (equal "There is no valid path to room 43000.~%" (char-output alice)))))

(deftest transport/normal/moves-target ()
  (with-mock-players (alice bob)
    (tempus::char-from-room alice)
    (tempus::char-to-room alice (tempus::real-room 3001))
    (with-captured-log log
        (tempus::do-transport-targets alice ".bob")
      (is (= 3001 (tempus::number-of (tempus::in-room-of bob))))
      (is (equal "Bob arrives from a puff of smoke.~%" (char-output alice)))
      (is (equal "Alice has transported you!~%" (char-output bob)))
      (is (search "Alice has transported Bob" log)))))

(deftest transport/no-such-target/error-message ()
  (with-mock-players (alice)
    (tempus::do-transport-targets alice ".zyzygy")
    (is (equal "You can't detect any '.zyzygy'~%" (char-output alice)))))

(deftest teleport/normal/moves-target ()
  (with-mock-players (alice bob)
    (with-captured-log log
        (tempus::do-teleport-name-to-target alice ".bob" "3001")
      (is (= 3001 (tempus::number-of (tempus::in-room-of bob))))
      (is (equal "Okay.~%Bob disappears in a puff of smoke.~%" (char-output alice)))
      (is (search "Alice has teleported you!~%" (char-output bob)))
      (is (search "Alice has teleported Bob" log)))))

(deftest vnum-mob/normal/lists-mobs ()
  (with-mock-players (alice)
    (tempus::do-vnum-mobiles-name alice "puff dragon")
    (is (equal "  1. &g[&n    1&g] &yPuff&n~%" (char-output alice)))))

(deftest vnum-mob/not-found/error-message ()
  (with-mock-players (alice)
    (tempus::do-vnum-mobiles-name alice "zyzygy")
    (is (equal "No mobiles by that name.~%" (char-output alice)))))

(deftest vnum-obj/normal/lists-objs ()
  (with-mock-players (alice)
    (tempus::do-vnum-objects-name alice "mixed potion")
    (is (equal "  1. &g[&n   15&g] &ga mixed potion&n~%"
                (char-output alice)))))

(deftest vnum-obj/not-found/error-message ()
  (with-mock-players (alice)
    (tempus::do-vnum-objects-name alice "zyzygy")
    (is (equal "No objects by that name.~%" (char-output alice)))))

(deftest stat-zone/normal/returns-zone-info ()
  (with-mock-players (alice)
    (tempus::do-stat-zone alice)
    (is (search "Zone #&y30: &cThe Holy City of Modrian&n~%" (char-output alice)))))

(deftest do-stat-trails/no-trails/returns-error ()
  (with-mock-players (alice)
    (tempus::do-stat-trails alice)
    (is (equal "No trails exist within this room.~%" (char-output alice)))))

(deftest do-stat-trails/normal/displays-no-NIL ()
  (with-mock-players (alice)
    ;; add some trails
    (unwind-protect
         (progn
           (push (make-instance 'tempus::room-trail-data
                                :name "Alice"
                                :aliases "alice .alice"
                                :idnum 2
                                :time (local-time:timestamp- (local-time:now)
                                                             35 :sec)
                                :from-dir 1
                                :to-dir 2
                                :track 60
                                :flags 0)
                 (tempus::trail-of (tempus::in-room-of alice)))
           (tempus::do-stat-trails alice)
           (is (null (search "NIL" (char-output alice)))))
      (setf (tempus::trail-of (tempus::in-room-of alice)) nil))))

(deftest force-command ()
  (with-mock-players (alice bob)
    (function-trace-bind ((calls tempus::interpret-command))
        (tempus::interpret-command alice "force bob to inventory")
      (is (equal "You got it.~%" (char-output alice)))
      (is (= (length calls) 2))
      (is (eql (first (first calls)) bob))
      (is (equal (second (first calls)) "inventory")))))

(deftest do-mload-vnum/normal/loads-mob ()
  (with-mock-players (alice)
    (unwind-protect
         (with-captured-log log
             (tempus::interpret-command alice "mload 1201")
           (is (search "(GC) Alice mloaded the Immortal Postmaster[1201] at 3002" log))
           (is (equal "You create the Immortal Postmaster.~%" (char-output alice)))
           (let ((mob (find 1201 (tempus::people-of (tempus::in-room-of alice)) :key 'tempus::vnum-of)))
             (is (not (null mob)))
             (is (= (tempus::vnum-of mob) 1201))))
      (let ((mobs (remove 1201
                          (remove-if-not #'tempus::is-npc (tempus::people-of (tempus::in-room-of alice)))
                          :test-not #'=
                          :key 'tempus::vnum-of)))
        ;; remove created mobs, if any
        (dolist (mob mobs)
          (tempus::char-from-room mob))))))

(deftest do-oload-vnum/normal/loads-object ()
  (with-mock-players (alice)
    (unwind-protect
         (with-captured-log log
             (tempus::interpret-command alice "oload 1203")
           (is (search "(GC) Alice oloaded his large coffee mug[1203] at 3002" log))
           (is (equal "You create his large coffee mug.~%" (char-output alice)))
           (let ((obj (find 1203 (tempus::contents-of (tempus::in-room-of alice)) :key 'tempus::vnum-of)))
             (is (not (null obj)))))
      (let ((objs (remove 1203
                          (tempus::contents-of (tempus::in-room-of alice))
                          :test-not #'=
                          :key 'tempus::vnum-of)))
        ;; remove created objects, if any
        (dolist (obj objs)
          (tempus::extract-obj obj))))))

(deftest do-oload-vnum/no-such-object/error-message ()
  (with-mock-players (alice)
    (unwind-protect
         (progn
           (tempus::interpret-command alice "oload 20")
           (is (equal "There is no object thang with that number.~%" (char-output alice)))
           (is (zerop (count 20 (tempus::contents-of (tempus::in-room-of alice))
                             :key 'tempus::vnum-of))))
      (let ((objs (remove 20
                          (tempus::contents-of (tempus::in-room-of alice))
                          :test-not #'=
                          :key 'tempus::vnum-of)))
        ;; remove created objects, if any
        (dolist (obj objs)
          (tempus::extract-obj obj))))))

(deftest do-oload-count-vnum/normal/loads-count-object ()
  (with-mock-players (alice)
    (unwind-protect
         (let ((count (random 100)))
           (with-captured-log log
               (tempus::interpret-command alice (format nil "oload ~d 1203" count))
             (is (search (format nil "(GC) Alice oloaded his large coffee mug[1203] at 3002 (x~d)" count)
                         log))
             (is (equal (format nil "You create his large coffee mug. (x~d)~~%" count)
                        (char-output alice)))
             (is (= count (count 1203 (tempus::contents-of (tempus::in-room-of alice))
                                 :key 'tempus::vnum-of)))))
      (let ((objs (remove 1203
                          (tempus::contents-of (tempus::in-room-of alice))
                          :test-not #'=
                          :key 'tempus::vnum-of)))
        ;; remove created objects, if any
        (dolist (obj objs)
          (tempus::extract-obj obj))))))

(deftest do-oload-count-vnum/count-too-large/error-message ()
  (with-mock-players (alice)
    (unwind-protect
         (progn
           (tempus::interpret-command alice "oload 101 1203")
           (is (equal "You can't possibly need THAT many!~%" (char-output alice)))
           (is (zerop (count 1203 (tempus::contents-of (tempus::in-room-of alice))
                             :key 'tempus::vnum-of))))
      (let ((objs (remove 1203
                          (tempus::contents-of (tempus::in-room-of alice))
                          :test-not #'=
                          :key 'tempus::vnum-of)))
        ;; remove created objects, if any
        (dolist (obj objs)
          (tempus::extract-obj obj))))))
