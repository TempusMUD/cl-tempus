(in-package #:tempus.tests)

(in-suite (defsuite (tempus.olc :in test)))

(defmacro with-room-olc-fixture ((player room) &body body)
  `(with-mock-players (,player)
     (with-mock-rooms (,room)
       (tempus::char-from-room ,player nil)
       (tempus::char-to-room ,player ,room nil)
      (setf (override-security-p ,player) t)
      (setf (tempus::level-of ,player) 51)
      (setf (tempus::owner-idnum-of (tempus::zone-of (tempus::in-room-of alice))) (tempus::idnum-of ,player))
      ,@body)))

(deftest perform-create-and-destroy-room/creates-and-destroys-room ()
  (with-mock-players (alice)
    (setf (tempus::owner-idnum-of (tempus::zone-of (tempus::in-room-of alice))) (tempus::idnum-of alice))
    (tempus::perform-create-room alice (tempus::zone-containing-number 102) 102)
    (char-output-is alice "Room 102 successfully created.~%")
    (clear-mock-buffers alice)
    (let ((room (gethash 102 tempus::*rooms*)))
      (is (not (null room)))
      (is (find room (tempus::world-of (tempus::zone-of room))))
      (when room
        (tempus::perform-destroy-room alice room)
        (char-output-is alice "Room eliminated.~%")
        (is (null (gethash 102 tempus::*rooms*)))
        (is (null (find room (tempus::world-of (tempus::zone-of room)))))))))

(deftest perform-clear-room/clears-room ()
  (with-room-olc-fixture (alice test-room)
    (tempus::perform-clear-room alice test-room)
    (char-output-is alice "Room fully cleared.~%")
    (is (string= "A Blank Room" (tempus::name-of test-room)))
    (is (null (or
               (tempus::description-of test-room)
               (tempus::sounds-of test-room)
               (tempus::prog-text-of test-room)
               (tempus::prog-obj-of test-room)
               (tempus::prog-marker-of test-room)
               (tempus::prog-state-of test-room)
               (tempus::ex-description-of test-room)
               (tempus::searches-of test-room)
               (tempus::affects-of test-room)
               (tempus::trail-of test-room)
               (tempus::flow-dir-of test-room)
               (tempus::flow-speed-of test-room)
               (tempus::flow-kind-of test-room)
               (tempus::func-of test-room)
               (tempus::func-param-of test-room)
               (tempus::max-occupancy-of test-room))))
    (is (zerop (tempus::flags-of test-room)))
    (is (zerop (tempus::find-path-index-of test-room)))
    (is (zerop (tempus::light-of test-room)))))

(deftest do-rset-title/enters-editor ()
  (with-room-olc-fixture (alice test-room)
    (tempus::interpret-command alice "olc rset description")
    (is (eql (tempus::state-of (tempus::link-of alice)) 'tempus::editing))))

(deftest do-rset-sector/sets-sector ()
  (with-room-olc-fixture (alice test-room)
    (tempus::interpret-command alice "olc rset sector road")
    (char-output-is alice "Room sector type set to Road.~%")
    (is (= (tempus::terrain-of test-room) tempus::+sect-road+))))

(deftest do-rset-flags/using-plus/adds-room-flags ()
  (with-room-olc-fixture (alice test-room)
    (tempus::interpret-command alice "olc rset flags + dark indoor")
    (char-output-is alice "Flag dark set on room.~%Flag indoor set on room.~%")
    (is (= (tempus::flags-of test-room) (logior tempus::+room-dark+ tempus::+room-indoors+)))))

(deftest do-rset-flags/using-minus/removes-room-flags ()
  (with-room-olc-fixture (alice test-room)
    (setf (tempus::flags-of test-room) (logior tempus::+room-dark+ tempus::+room-indoors+))
    (tempus::interpret-command alice "olc rset flags - dark indoor")
    (char-output-is alice "Flag dark unset on room.~%Flag indoor unset on room.~%")
    (is (zerop (tempus::flags-of test-room)))))

(deftest do-rset-sounds-remove/removes-sounds ()
  (with-room-olc-fixture (alice test-room)
    (setf (tempus::sounds-of test-room) "You hear the sounds of testing.~%")
    (tempus::interpret-command alice "olc rset sound remove")
    (char-output-is alice "Sounds removed from room.~%")
    (is (null (tempus::sounds-of test-room)))))

(deftest do-rset-sounds/starts-editor ()
  (with-room-olc-fixture (alice test-room)
    (tempus::interpret-command alice "olc rset sound")
    (is (eql (tempus::state-of (tempus::link-of alice)) 'tempus::editing))))

(deftest do-rset-flow/sets-flow-vars ()
  (with-room-olc-fixture (alice test-room)
    (tempus::interpret-command alice "olc rset flow north 5 wind")
    (char-output-is alice "Flow state set.~%")
    (is (= (tempus::flow-dir-of test-room) tempus::+north+))
    (is (= (tempus::flow-speed-of test-room) 5))
    (is (= (tempus::flow-kind-of test-room) 1))))

(deftest do-rset-flow-remove/clears-flow-vars ()
  (with-room-olc-fixture (alice test-room)
    (tempus::interpret-command alice "olc rset flow remove")
    (char-output-is alice "Flow removed from room.~%")
    (is (zerop (tempus::flow-dir-of test-room)))
    (is (zerop (tempus::flow-speed-of test-room)))
    (is (zerop (tempus::flow-kind-of test-room)))))

(deftest do-rset-occupancy/sets-max-occupancy ()
  (with-room-olc-fixture (alice test-room)
    (tempus::interpret-command alice "olc rset occupancy 15")
    (char-output-is alice "Room occupancy set to 15.~%")
    (is (= (tempus::max-occupancy-of test-room) 15))))

(deftest do-olc-exit-doorflags/with-plus/adds-door-flags ()
  (with-room-olc-fixture (alice test-room)
    (tempus::interpret-command alice "olc exit s doorflags + door closed locked ")
    (char-output-is alice "Flag door set on exit.~%Flag closed set on exit.~%Flag locked set on exit.~%")
    (is (not (null (aref (tempus::dir-option-of test-room) tempus::+south+))))
    (is (= (tempus::exit-info-of (aref (tempus::dir-option-of test-room) tempus::+south+))
           (logior tempus::+door-open+ tempus::+door-closed+ tempus::+door-locked+)))))

(deftest do-olc-exit-doorflags/with-plus/removes-door-flags ()
  (with-room-olc-fixture (alice test-room)
    (setf (aref (tempus::dir-option-of test-room) tempus::+south+)
          (make-instance 'tempus::room-direction-data
                         :exit-info (logior tempus::+door-open+
                                            tempus::+door-closed+
                                            tempus::+door-locked+)))
    (tempus::interpret-command alice "olc exit s doorflags - door closed locked ")
    (char-output-is alice "Flag door unset on exit.~%Flag closed unset on exit.~%Flag locked unset on exit.~%")
    (is (zerop (tempus::exit-info-of (aref (tempus::dir-option-of test-room) tempus::+south+))))))

(deftest do-olc-create-search/creates-search ()
  (with-room-olc-fixture (alice test-room)
    (tempus::interpret-command alice "olc create search pull lever")
    (char-output-is alice "Search creation successful.~%Now editing search (pull)/(lever)~%")
    (is (not (null (tempus::searches-of test-room))))
    (is (equal "pull" (tempus::trigger-of (first (tempus::searches-of test-room)))))
    (is (equal "lever" (tempus::keywords-of (first (tempus::searches-of test-room)))))))

(deftest do-olc-create-search/search-exists/emits-error ()
  (with-room-olc-fixture (alice test-room)
    (push (make-instance 'tempus::special-search-data
                         :trigger "pull"
                         :keywords "lever")
          (tempus::searches-of test-room))
    (tempus::interpret-command alice "olc create search pull lever")
    (char-output-is alice "There is already a search here on that trigger.~%")
    (is (= 1 (length (tempus::searches-of test-room))))))

(deftest do-olc-destroy-search/destroys-search ()
  (with-room-olc-fixture (alice test-room)
    (push (make-instance 'tempus::special-search-data
                         :trigger "pull"
                         :keywords "lever")
          (tempus::searches-of test-room))
    (tempus::interpret-command alice "olc destroy search pull lever")
    (char-output-is alice "Search destroyed.~%")
    (is (null (tempus::searches-of test-room)))))

(deftest do-olc-destroy-search/no-such-search/emits-error ()
  (with-room-olc-fixture (alice test-room)
    (tempus::interpret-command alice "olc destroy search pull lever")
    (char-output-is alice "There is no such search here.~%")
    (is (null (tempus::searches-of test-room)))))

(deftest do-olc-xedit/search-exists/sets-search-edit ()
  (with-room-olc-fixture (alice test-room)
    (push (make-instance 'tempus::special-search-data
                         :trigger "pull"
                         :keywords "lever")
          (tempus::searches-of test-room))
    (tempus::interpret-command alice "olc xedit pull lever")
    (char-output-is alice "Now editing search (pull)/(lever)~%")
    (is (eql (tempus::olc-srch-of alice) (first (tempus::searches-of test-room))))))

(deftest do-olc-xedit/no-such-search/emits-error ()
  (with-room-olc-fixture (alice test-room)
    (tempus::interpret-command alice "olc xedit pull lever")
    (char-output-is alice "There is no such search here.~%")
    (is (null (tempus::olc-srch-of alice)))))

(deftest do-olc-xset-trigger/sets-trigger ()
  (with-room-olc-fixture (alice test-room)
    (let ((search (make-instance 'tempus::special-search-data
                                 :trigger "pull"
                                 :keywords "lever")))
      (setf (tempus::olc-srch-of alice) search)
      (tempus::interpret-command alice "olc xset trigger push")
      (char-output-is alice "Search command trigger set.~%")
      (is (equal "push" (tempus::trigger-of search))))))

(deftest do-olc-xset-to-vict/sets-to-vict-emit ()
  (with-room-olc-fixture (alice test-room)
    (let ((search (make-instance 'tempus::special-search-data
                                 :trigger "pull"
                                 :keywords "lever")))
      (setf (tempus::olc-srch-of alice) search)
      (tempus::interpret-command alice "olc xset to_vict You pull the lever.")
      (char-output-is alice "To_vict message set.~%")
      (is (equal "You pull the lever." (tempus::to-vict-of search))))))

(deftest do-olc-xset-to-room/sets-to-room-emit ()
  (with-room-olc-fixture (alice test-room)
    (let ((search (make-instance 'tempus::special-search-data
                                 :trigger "pull"
                                 :keywords "lever")))
      (setf (tempus::olc-srch-of alice) search)
      (tempus::interpret-command alice "olc xset to_room You pull the lever.")
      (char-output-is alice "To_room message set.~%")
      (is (equal "You pull the lever." (tempus::to-room-of search))))))

(deftest do-olc-xset-to-remote/sets-to-remote-emit ()
  (with-room-olc-fixture (alice test-room)
    (let ((search (make-instance 'tempus::special-search-data
                                 :trigger "pull"
                                 :keywords "lever")))
      (setf (tempus::olc-srch-of alice) search)
      (tempus::interpret-command alice "olc xset to_remote You pull the lever.")
      (char-output-is alice "To_remote message set.~%")
      (is (equal "You pull the lever." (tempus::to-remote-of search))))))

(deftest do-olc-xset-command/sets-search-command ()
  (with-room-olc-fixture (alice test-room)
    (let ((search (make-instance 'tempus::special-search-data
                                 :trigger "pull"
                                 :keywords "lever")))
      (setf (tempus::olc-srch-of alice) search)
      (tempus::interpret-command alice "olc xset command trans")
      (char-output-is alice "Search command set.~%")
      (is (= tempus::+search-com-transport+ (tempus::command-of search))))))

(deftest do-olc-xset-value/sets-value-arg ()
  (with-room-olc-fixture (alice test-room)
    (let ((search (make-instance 'tempus::special-search-data
                                 :trigger "pull"
                                 :keywords "lever")))
      (setf (tempus::olc-srch-of alice) search)
      (tempus::interpret-command alice "olc xset value 1 35")
      (char-output-is alice "Ok, value set.~%")
      (is (= 35 (aref (tempus::arg-of search) 1))))))

(deftest do-olc-xset-flags/with-plus/sets-flags ()
  (with-room-olc-fixture (alice test-room)
    (let ((search (make-instance 'tempus::special-search-data
                                 :trigger "pull"
                                 :keywords "lever")))
      (setf (tempus::olc-srch-of alice) search)
      (tempus::interpret-command alice "olc xset flag + repeatable noevil")
      (char-output-is alice "Flag REPEATABLE set on search.~%Flag NOEVIL set on search.~%")
      (is (= (logior tempus::+search-repeatable+
                     tempus::+search-noevil+)
             (tempus::flags-of search))))))

(deftest do-olc-xset-flags/with-minus/removes-flags ()
  (with-room-olc-fixture (alice test-room)
    (let ((search (make-instance 'tempus::special-search-data
                                 :trigger "pull"
                                 :keywords "lever")))
      (setf (tempus::olc-srch-of alice) search)
      (setf (tempus::flags-of search) (logior tempus::+search-repeatable+ tempus::+search-noevil+))
      (tempus::interpret-command alice "olc xset flag - repeatable noevil")
      (char-output-is alice "Flag REPEATABLE unset on search.~%Flag NOEVIL unset on search.~%")
      (is (zerop (tempus::flags-of search))))))

(deftest do-olc-xset-fail-chance/sets-fail-chance ()
  (with-room-olc-fixture (alice test-room)
    (let ((search (make-instance 'tempus::special-search-data
                                 :trigger "pull"
                                 :keywords "lever")))
      (setf (tempus::olc-srch-of alice) search)
      (tempus::interpret-command alice "olc xset fail_chance 42")
      (char-output-is alice "This search will now have a 42% chance of failure.~%")
      (is (= 42 (tempus::fail-chance-of search))))))

(deftest do-olc-xstat/calls-format-search-data ()
  (with-room-olc-fixture (alice test-room)
    (let ((search (make-instance 'tempus::special-search-data
                                 :trigger "pull"
                                 :keywords "lever")))
      (setf (tempus::olc-srch-of alice) search)
      (function-trace-bind ((calls tempus::format-search-data))
          (tempus::interpret-command alice "olc xstat")
        (is (equal `((,alice ,test-room ,search)) calls))))))
