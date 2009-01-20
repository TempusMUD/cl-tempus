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
    (tempus::char-from-room alice t)
    (tempus::char-to-room alice (tempus::real-room 3001))
    (tempus::interpret-command alice "at 3002 say hi")
    (is (equal "&BYou say, &c'hi'&n~%" (char-output alice)))
    (is (equal "&BAlice says, &c'hi'&n~%" (char-output bob)))))

(deftest goto/numeric-target/changes-room ()
  (with-mock-players (alice bob)
    (tempus::char-from-room alice t)
    (tempus::char-to-room alice (tempus::real-room 3001))
    (tempus::interpret-command alice "goto 3002")
    (is (= 3002 (tempus::number-of (tempus::in-room-of alice))))
    (is (equal "Alice appears with an ear-splitting bang.~%" (char-output bob)))))

(deftest goto/char-target/changes-room ()
  (with-mock-players (alice bob)
    (tempus::char-from-room alice t)
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
    (tempus::char-from-room alice t)
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
          (tempus::char-from-room mob t))))))

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

(deftest perform-pload/single-on-self/loads-object ()
  (with-mock-players (alice bob)
    (with-captured-log log
        (tempus::perform-pload alice 1203 1 alice)
      (is (search "(GC) Alice ploaded his large coffee mug[1203] onto self at 3002" log))
      (is (equal "You create his large coffee mug.~%" (char-output alice)))
      (is (equal "Alice does something suspicious and alters reality.~%" (char-output bob)))
      (is (= 1 (count 1203 (tempus::carrying-of alice) :key 'tempus::vnum-of))))))

(deftest perform-pload/multiple-on-self/loads-objects ()
  (with-mock-players (alice bob)
    (with-captured-log log
        (tempus::perform-pload alice 1203 10 alice)
      (is (search "(GC) Alice ploaded his large coffee mug[1203] onto self at 3002 (x10)" log))
      (is (equal "You create his large coffee mug. (x10)~%" (char-output alice)))
      (is (equal "Alice does something suspicious and alters reality.~%" (char-output bob)))
      (is (= 10 (count 1203 (tempus::carrying-of alice) :key 'tempus::vnum-of))))))

(deftest perform-pload/single-on-other/loads-object ()
  (with-mock-players (alice bob eva)
    (with-captured-log log
        (tempus::perform-pload alice 1203 1 bob)
      ;; hackily skip the mock player id
      (is (search "(GC) Alice ploaded his large coffee mug[1203] onto PC Bob[" log))
      (is (search "] at 3002" log))
      (is (equal "You load his large coffee mug onto Bob.~%" (char-output alice)))
      (is (equal "Alice causes his large coffee mug to appear in your hands.~%" (char-output bob)))
      (is (equal "Alice does something suspicious and alters reality.~%" (char-output eva)))
      (is (= 1 (count 1203 (tempus::carrying-of bob) :key 'tempus::vnum-of))))))

(deftest perform-pload/multiple-on-other/loads-objects ()
  (with-mock-players (alice bob eva)
    (with-captured-log log
        (tempus::perform-pload alice 1203 10 bob)
      ;; hackily skip the mock player id
      (is (search "(GC) Alice ploaded his large coffee mug[1203] onto PC Bob[" log))
      (is (search "] at 3002 (x10)" log))
      (is (equal "You load his large coffee mug onto Bob. (x10)~%" (char-output alice)))
      (is (equal "Alice causes his large coffee mug to appear in your hands. (x10)~%" (char-output bob)))
      (is (equal "Alice does something suspicious and alters reality.~%" (char-output eva)))
      (is (= 10 (count 1203 (tempus::carrying-of bob) :key 'tempus::vnum-of))))))

(deftest do-pload-vnum/normal/calls-perform-pload ()
  (with-mock-players (alice)
    (with-captured-log log
        (function-trace-bind ((calls tempus::perform-pload))
            (tempus::interpret-command alice "pload 1203")
          (is (equal `((,alice 1203 1 ,alice)) calls))))))

(deftest do-pload-arg1-arg2/with-count-and-vnum/calls-perform-pload ()
  (with-mock-players (alice)
    (with-captured-log log
        (function-trace-bind ((calls tempus::perform-pload))
            (tempus::interpret-command alice "pload 10 1203")
          (is (equal `((,alice 1203 10 ,alice)) calls))))))

(deftest do-pload-arg1-arg2/with-vnum-and-target/calls-perform-pload ()
  (with-mock-players (alice bob)
    (with-captured-log log
        (function-trace-bind ((calls tempus::perform-pload))
            (tempus::interpret-command alice "pload 1203 bob")
          (is (equal `((,alice 1203 1 ,bob)) calls))))))

(deftest do-pload-count-vnum-target/normal/calls-perform-pload ()
  (with-mock-players (alice bob)
    (with-captured-log log
        (function-trace-bind ((calls tempus::perform-pload))
            (tempus::interpret-command alice "pload 10 1203 bob")
          (is (equal `((,alice 1203 10 ,bob)) calls))))))

(deftest gain-exp-regardless/normal/gains-exp ()
  (with-mock-players (alice)
    (setf (tempus::level-of alice) 10)
    (setf (tempus::exp-of alice) (aref tempus::+exp-scale+ 10))
    (with-captured-log log
        (tempus::gain-exp-regardless alice 500)
      (is (equal "" (char-output alice)))
      (is (= (tempus::level-of alice) 10))
      (is (= (tempus::exp-of alice) (+ 500 (aref tempus::+exp-scale+ 10))))
      (is (equal "" log)))))

(deftest gain-exp-regardless/enough-for-level/gains-exp-and-level ()
  (with-mock-players (alice)
    (setf (tempus::level-of alice) 10)
    (setf (tempus::exp-of alice) (aref tempus::+exp-scale+ 10))
    (with-captured-log log
        (tempus::gain-exp-regardless alice (- (aref tempus::+exp-scale+ 11)
                                              (aref tempus::+exp-scale+ 10)))
      (is (equal "You rise a level!~%" (char-output alice)))
      (is (= (tempus::level-of alice) 11))
      (is (= (tempus::exp-of alice) (aref tempus::+exp-scale+ 11)))
      (is (search "Alice advanced to level 11" log)))))

(deftest do-advance-target/normal/target-gains-exp ()
  (with-mock-players (alice bob)
    (setf (tempus::level-of alice) 72)
    (with-captured-log log
        (function-trace-bind ((calls tempus::gain-exp-regardless))
            (tempus::interpret-command alice "advance bob 10")
          (is (equal `((,bob ,(aref tempus::+exp-scale+ 10))) calls)))
      (is (search "You got it.~%" (char-output alice)))
      (is (search "Alice makes some strange gestures.~%" (char-output bob)))
      (is (search "You rise 9 levels!~%" (char-output bob)))
      (is (search "(GC) Alice has advanced Bob to level 10 (from 1)" log)))))

(deftest do-restore-target/normal/target-is-restored ()
  (with-mock-players (alice bob)
    (setf (tempus::level-of alice) 72)
    (with-captured-log log
        (function-trace-bind ((calls tempus::restore-creature))
            (tempus::interpret-command alice "restore bob")
          (is (equal `((,bob)) calls)))
      (is (search "You got it.~%" (char-output alice)))
      (is (equal "You have been fully healed by Alice!~%" (char-output bob)))
      (is (search "Bob has been restored by Alice" log)))))

(deftest perform-vis/already-visible/already-vis-message ()
  (with-mock-players (alice)
    (setf (tempus::level-of alice) 72)
    (tempus::perform-vis alice)
    (is (equal "You are already fully visible.~%" (char-output alice)))))

(deftest perform-vis/other-cant-see/other-gets-message ()
  (with-mock-players (alice bob)
    (setf (tempus::level-of alice) 72)
    (setf (tempus::invis-level-of alice) 72)
    (tempus::perform-vis alice)
    (is (equal "You are now fully visible.~%" (char-output alice)))
    (is (equal "You suddenly realize that Alice is standing beside you.~%" (char-output bob)))))

(deftest perform-vis/other-can-see/other-gets-no-message ()
  (with-mock-players (alice bob)
    (setf (tempus::level-of alice) 72)
    (setf (tempus::level-of bob) 50)
    (setf (tempus::invis-level-of alice) 50)
    (tempus::perform-vis alice)
    (is (equal "You are now fully visible.~%" (char-output alice)))
    (is (equal "" (char-output bob)))))

(deftest perform-invis/ch-is-npc/no-effect ()
  (with-mock-mobiles (mallory)
    ;; this should error if unhandled, since mobiles have no
    ;; invis-level slot
    (finishes (tempus::perform-invis mallory (tempus::level-of mallory)))))

(deftest perform-invis/ch-turns-invis/ch-is-invis ()
  (with-mock-players (alice)
    (setf (tempus::level-of alice) 72)
    (setf (tempus::invis-level-of alice) 0)
    (tempus::perform-invis alice (tempus::level-of alice))
    (is (= (tempus::invis-level-of alice) (tempus::level-of alice)))
    (is (equal "Your invisibility level is 72.~%" (char-output alice)))))

(deftest perform-invis/other-can-see/other-gets-message ()
  (with-mock-players (alice bob)
    (setf (tempus::level-of alice) 72)
    (setf (tempus::invis-level-of alice) 0)
    (tempus::perform-invis alice (tempus::level-of alice))
    (is (equal "You blink and suddenly realize that Alice is gone.~%"
               (char-output bob)))))

(deftest perform-invis/other-cant-see/other-gets-no-message ()
  (with-mock-players (alice bob)
    (setf (tempus::level-of alice) 72)
    (setf (tempus::invis-level-of alice) 51)
    (setf (tempus::level-of bob) 50)
    (tempus::perform-invis alice (tempus::level-of alice))
    (is (equal "" (char-output bob)))))

(deftest perform-invis/other-can-now-see/other-gets-message ()
  (with-mock-players (alice bob)
    (setf (tempus::level-of alice) 72)
    (setf (tempus::invis-level-of alice) 51)
    (setf (tempus::level-of bob) 50)
    (tempus::perform-invis alice 40)
    (is (equal "You suddenly realize that Alice is standing beside you.~%"
               (char-output bob)))))

(deftest do-invis/ch-is-npc/error-message ()
  (with-mock-mobiles (mallory)
    (tempus::interpret-command mallory "invis")
    (is (equal "You can't do that!~%" (char-output mallory)))))

(deftest do-invis/no-arg-visible/ch-is-max-invis ()
  (with-mock-players (alice)
    (setf (tempus::level-of alice) 72)
    (function-trace-bind ((calls tempus::perform-invis))
        (tempus::interpret-command alice "invis")
      (is (equal `((,alice 72)) calls)))))

(deftest do-invis/no-arg-invisible/ch-is-vis ()
  (with-mock-players (alice)
    (setf (tempus::level-of alice) 72)
    (setf (tempus::invis-level-of alice) 72)
    (function-trace-bind ((calls tempus::perform-vis))
        (tempus::interpret-command alice "invis")
      (is (equal `((,alice)) calls)))))

(deftest do-invis/one-arg-over-level/error-message ()
  (with-mock-players (alice)
    (setf (tempus::level-of alice) 50)
    (tempus::interpret-command alice "invis 70")
    (is (equal "You can't go invisible above your own level.~%" (char-output alice)))
    (is (zerop (tempus::invis-level-of alice)))))

(deftest do-invis/one-arg-is-zero/ch-is-vis ()
  (with-mock-players (alice)
    (setf (tempus::level-of alice) 50)
    (setf (tempus::invis-level-of alice) 50)
    (function-trace-bind ((calls tempus::perform-vis))
        (tempus::interpret-command alice "invis 0")
      (is (equal `((,alice)) calls)))
    (is (zerop (tempus::invis-level-of alice)))))

(deftest do-invis/one-arg-under-level/ch-is-invis ()
  (with-mock-players (alice)
    (setf (tempus::level-of alice) 72)
    (function-trace-bind ((calls tempus::perform-invis))
        (tempus::interpret-command alice "invis 60")
      (is (equal `((,alice 60)) calls)))
    (is (= 60 (tempus::invis-level-of alice)))))

(deftest do-gecho/no-arg/error-message ()
  (with-mock-players (alice)
    (tempus::interpret-command alice "gecho")
    (is (equal "That must be a mistake...~%" (char-output alice)))))

(deftest do-gecho/with-arg/message-sent-to-all ()
  (with-mock-players (alice bob ike)
    (setf (tempus::level-of ike) 60)
    (tempus::interpret-command alice "gecho Testing")
    (is (equal "Testing~%" (char-output alice)))
    (is (equal "Testing~%" (char-output bob)))
    (is (equal "[Alice-g] Testing~%" (char-output ike)))))

(deftest do-dc-num-str/normal/target-disconnected ()
  (with-mock-players (alice bob)
    (setf (tempus::level-of alice) 50)
    (with-captured-log log
        (tempus::interpret-command alice (format nil "dc ~d"
                                                 (tempus::cxn-fd
                                                  (tempus::link-of bob))))
      (is (search "(GC) Connection closed by Alice" log)))
    (is (equal (format nil "Connection #~d closed.~~%"
                       (tempus::cxn-fd (tempus::link-of bob)))
               (char-output alice)))
    (is (eql 'tempus::disconnecting (tempus::state-of (tempus::link-of bob))))))

(deftest do-last/no-args/error-message ()
  (with-mock-players (alice)
    (setf (tempus::level-of alice) 72)
    (finishes (tempus::interpret-command alice "last"))
    (is (equal "For whom do you wish to search?~%" (char-output alice)))))

(deftest do-last-playername/lower-level/returns-last ()
  (with-mock-players (alice)
    (setf (tempus::level-of alice) 70)
    (finishes (tempus::interpret-command alice "last azimuth"))
    (is (equal "You are not sufficiently godly for that!~%"
               (char-output alice)))))

(deftest do-last-playername/normal/returns-last ()
  (with-mock-players (alice)
    (setf (tempus::level-of alice) 72)
    (finishes (tempus::interpret-command alice "last azimuth"))
    (is (search "Azimuth" (char-output alice)))))
