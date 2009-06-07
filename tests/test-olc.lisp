(in-package #:tempus.tests)

(in-suite (defsuite (tempus.olc :in test)))

(defmacro with-room-olc-fixture ((player room) &body body)
  `(with-fixtures ((,player mock-player :level 51 :override-security t)
                   (,room mock-room))
     (tempus::char-from-room ,player nil)
     (tempus::char-to-room ,player ,room nil)
     (setf (tempus::owner-idnum-of (tempus::zone-of (tempus::in-room-of ,player)))
           (tempus::idnum-of ,player))
     ,@body))

(defmacro with-obj-olc-fixture ((player) &body body)
  (let ((obj (gensym "OBJ")))
    `(with-fixtures ((,player mock-player :level 51 :override-security t)
                     (,obj mock-obj-prototype :name "a test object"))
       (setf (tempus::owner-idnum-of (tempus::zone-of (tempus::in-room-of ,player)))
             (tempus::idnum-of ,player))
       (setf (tempus::olc-obj-of ,player) ,obj)
       ,@body)))

(defmacro with-zone-olc-fixture ((player room zone) &body body)
  `(with-fixtures ((,player mock-player :level 51 :override-security t)
                   (,zone mock-zone :zone-num 1)
                   (,room mock-room))
     (tempus::char-from-room ,player nil)
     (tempus::char-to-room ,player ,room nil)
     (setf (tempus::bitp (tempus::prefs-of ,player) tempus::+pref-worldwrite+) t)
     (setf (tempus::owner-idnum-of ,zone) (tempus::idnum-of ,player))
     ,@body))

(defmacro with-mob-olc-fixture ((player) &body body)
  (let ((mob (gensym "MOB")))
    `(with-fixtures ((,player mock-player :level 51 :override-security t)
                     (,mob mock-mob-prototype :name "a test mobile"))
       (setf (tempus::owner-idnum-of (tempus::zone-of (tempus::in-room-of ,player)))
             (tempus::idnum-of ,player))
       (setf (tempus::olc-mob-of ,player) ,mob)
       ,@body)))

(defun zcmd-equal (zcmd if-flag command arg1 arg2 arg3 prob)
  (and (eql command (tempus::command-of zcmd))
       (= if-flag (tempus::if-flag-of zcmd))
       (= arg1 (tempus::arg1-of zcmd))
       (= arg2 (tempus::arg2-of zcmd))
       (= arg3 (tempus::arg3-of zcmd))
       (= prob (tempus::prob-of zcmd))))

(deftest perform-create-and-destroy-room/creates-and-destroys-room ()
  (with-fixtures ((alice mock-player :level 51 :override-security t))
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

(deftest save-zone-rooms/saves-zone-file ()
  (with-fixtures ((alice mock-player :level 51 :override-security t))
    (unwind-protect
         (progn
           (rename-file (tempus::tempus-path "lib/world/wld/376.wld") (tempus::tempus-path "lib/world/wld/376.wld.test"))
           (with-captured-log log
               (tempus::save-zone-rooms alice (tempus::real-zone 376))
             (is (search "OLC: Alice rsaved 376" log))
             (is (equal (tempus::snarf-file (tempus::tempus-path "lib/world/wld/376.wld"))
                        (tempus::snarf-file (tempus::tempus-path "lib/world/wld/376.wld.test"))))))
      (progn
        (when (probe-file (tempus::tempus-path "lib/world/wld/376.wld"))
          (delete-file (tempus::tempus-path "lib/world/wld/376.wld")))
        (rename-file (tempus::tempus-path "lib/world/wld/376.wld.test") (tempus::tempus-path "lib/world/wld/376.wld"))))))

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

(deftest do-rset-title/sets-room-title ()
  (with-room-olc-fixture (alice test-room)
    (tempus::interpret-command alice "olc rset title testing")
    (char-output-is alice "Room title set to 'testing'.~%")
    (is (equal "testing" (tempus::name-of test-room)))))

(deftest do-rset-description/enters-editor ()
  (with-room-olc-fixture (alice test-room)
    (tempus::interpret-command alice "olc rset description")
    (is (eql (tempus::state-of (tempus::link-of alice)) 'tempus::editing))))

(deftest do-rset-sector/sets-sector ()
  (with-room-olc-fixture (alice test-room)
    (tempus::interpret-command alice "olc rset sector road")
    (char-output-is alice "Room sector type set to road.~%")
    (is (= (tempus::terrain-of test-room) tempus::+sect-road+))))

(deftest do-rset-flags/using-plus/adds-room-flags ()
  (with-room-olc-fixture (alice test-room)
    (tempus::interpret-command alice "olc rset flags + dark indoor")
    (char-output-is alice "Flag dark set on room flags.~%Flag indoor set on room flags.~%")
    (is (= (tempus::flags-of test-room) (logior tempus::+room-dark+ tempus::+room-indoors+)))))

(deftest do-rset-flags/using-minus/removes-room-flags ()
  (with-room-olc-fixture (alice test-room)
    (setf (tempus::flags-of test-room) (logior tempus::+room-dark+ tempus::+room-indoors+))
    (tempus::interpret-command alice "olc rset flags - dark indoor")
    (char-output-is alice "Flag dark unset on room flags.~%Flag indoor unset on room flags.~%")
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
    (char-output-is alice "Flag door set on exit flags.~%Flag closed set on exit flags.~%Flag locked set on exit flags.~%")
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
    (char-output-is alice "Flag door unset on exit flags.~%Flag closed unset on exit flags.~%Flag locked unset on exit flags.~%")
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
      (char-output-is alice "Flag REPEATABLE set on search flags.~%Flag NOEVIL set on search flags.~%")
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
      (char-output-is alice "Flag REPEATABLE unset on search flags.~%Flag NOEVIL unset on search flags.~%")
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

(deftest perform-create-and-destroy-object/creates-and-destroys-obj ()
  (with-fixtures ((alice mock-player :level 51 :override-security t))
    (tempus::perform-create-object alice 101)
    (char-output-is alice "Object 101 successfully created.~%Now editing object [101] &ga fresh blank object&n.~%")
    (let ((obj (gethash 101 tempus::*object-prototypes*)))
      (is (not (null obj)))
      (is (= 101 (tempus::vnum-of obj)))
      (is (eql obj (tempus::olc-obj-of alice)))
      (is (eql obj (tempus::proto-of (tempus::shared-of obj)))))
    (clear-mock-buffers alice)
    (tempus::perform-destroy-object alice 101)
    (char-output-is alice "The object you were editing has been destroyed.~%Object eliminated.~%")
    (is (null (tempus::olc-obj-of alice)))
    (is (null (gethash 101 tempus::*object-prototypes*)))))

(deftest save-zone-objects/saves-zone-file ()
  (with-fixtures ((alice mock-player :level 51 :override-security t))
    (unwind-protect
         (progn
           (rename-file (tempus::tempus-path "lib/world/obj/376.obj") (tempus::tempus-path "lib/world/obj/376.obj.test"))
           (with-captured-log log
               (tempus::save-zone-objects alice (tempus::real-zone 376))
             (is (search "OLC: Alice osaved 376" log))
             (is (equal (tempus::snarf-file (tempus::tempus-path "lib/world/obj/376.obj"))
                        (tempus::snarf-file (tempus::tempus-path "lib/world/obj/376.obj.test"))))))
      (progn
        (when (probe-file (tempus::tempus-path "lib/world/obj/376.obj"))
          (delete-file (tempus::tempus-path "lib/world/obj/376.obj")))
        (rename-file (tempus::tempus-path "lib/world/obj/376.obj.test") (tempus::tempus-path "lib/world/obj/376.obj"))))))

(deftest do-olc-oedit-number-and-exit/sets-olc-obj-of-char ()
  (with-fixtures ((alice mock-player :level 51 :override-security t))
    (setf (tempus::owner-idnum-of (tempus::zone-of (tempus::in-room-of alice)))
          (tempus::idnum-of alice))
    (tempus::interpret-command alice "olc oedit 100")
    (char-output-is alice "Now editing object [100] &ga test object&n.~%")
    (is (eql (tempus::real-object-proto 100) (tempus::olc-obj-of alice)))
    (clear-mock-buffers alice)
    (tempus::interpret-command alice "olc oedit exit")
    (char-output-is alice "Exiting object editor.~%")
    (is (null (tempus::olc-obj-of alice)))))

(deftest do-olc-stat/no-argument/stat-editing-object ()
  (with-obj-olc-fixture (alice)
    (tempus::interpret-command alice "olc ostat")
    (char-output-has alice "Name: '&ga test object&n'")))

(deftest do-olc-stat/with-argument/stat-editing-object ()
  (with-fixtures ((alice mock-player :level 51 :override-security t))
    (tempus::interpret-command alice "olc ostat 100")
    (char-output-has alice "Name: '&ga test object&n'")))

(deftest do-olc-oset/no-arguments/list-commands ()
  (with-obj-olc-fixture (alice)
    (tempus::interpret-command alice "olc oset")
    (char-output-has alice "alias")))

(deftest do-olc-oset-alias/sets-object-alias ()
  (with-obj-olc-fixture (alice)
    (setf (tempus::ex-description-of (tempus::olc-obj-of alice))
          (list (make-instance 'tempus::extra-descr-data
                               :keyword "a mock object"
                               :description "This is a mock object.")))
    (tempus::interpret-command alice "olc oset alias testing")
    (char-output-is alice "Object aliases set to 'testing'.~%")
    (is (equal "testing" (tempus::aliases-of (tempus::olc-obj-of alice))))))

(deftest do-olc-oset-name/sets-object-name ()
  (with-obj-olc-fixture (alice)
    (tempus::interpret-command alice "olc oset name testing")
    (char-output-is alice "Object name set to 'testing'.~%")
    (is (equal "testing" (tempus::name-of (tempus::olc-obj-of alice))))))

(deftest do-olc-oset-ldesc/not-tilde/sets-object-ldesc ()
  (with-obj-olc-fixture (alice)
    (tempus::interpret-command alice "olc oset ldesc testing")
    (char-output-is alice "Object line description set to 'testing'.~%")
    (is (equal "testing" (tempus::line-desc-of (tempus::olc-obj-of alice))))))

(deftest do-olc-oset-ldesc/with-tilde/nulls-object-ldesc ()
  (with-obj-olc-fixture (alice)
    (tempus::interpret-command alice "olc oset ldesc ~")
    (char-output-is alice "Object line description unset.~%")
    (is (null (tempus::line-desc-of (tempus::olc-obj-of alice))))))

(deftest do-olc-oset-action-desc/not-tilde/sets-object-action-desc ()
  (with-obj-olc-fixture (alice)
    (tempus::interpret-command alice "olc oset action_desc testing")
    (char-output-is alice "Object action description set to 'testing'.~%")
    (is (equal "testing" (tempus::action-desc-of (tempus::olc-obj-of alice))))))

(deftest do-olc-oset-action-desc/with-tilde/nulls-object-action-desc ()
  (with-obj-olc-fixture (alice)
    (tempus::interpret-command alice "olc oset action_desc ~")
    (char-output-is alice "Object action description unset.~%")
    (is (null (tempus::action-desc-of (tempus::olc-obj-of alice))))))

(deftest do-olc-oset-type/sets-object-kind ()
  (with-obj-olc-fixture (alice)
    (tempus::interpret-command alice "olc oset type light")
    (char-output-is alice "Object type set to light.~%")
    (is (= tempus::+item-light+ (tempus::kind-of (tempus::olc-obj-of alice))))))

(deftest do-olc-oset-extra1/sets-object-extra1-flags ()
  (with-obj-olc-fixture (alice)
    (tempus::interpret-command alice "olc oset extra1 + glow hum")
    (char-output-is alice "Flag glow set on extra1 object flags.~%Flag hum set on extra1 object flags.~%")
    (is (= (logior tempus::+item-hum+ tempus::+item-glow+)
           (tempus::extra-flags-of (tempus::olc-obj-of alice))))))

(deftest do-olc-oset-extra2/sets-object-extra2-flags ()
  (with-obj-olc-fixture (alice)
    (tempus::interpret-command alice "olc oset extra2 + ablaze noremove")
    (char-output-is alice "Flag ablaze set on extra2 object flags.~%Flag noremove set on extra2 object flags.~%")
    (is (= (logior tempus::+item2-ablaze+ tempus::+item2-noremove+)
           (tempus::extra2-flags-of (tempus::olc-obj-of alice))))))

(deftest do-olc-oset-extra3/sets-object-extra3-flags ()
  (with-obj-olc-fixture (alice)
    (tempus::interpret-command alice "olc oset extra3 + hardened hunted")
    (char-output-is alice "Flag hardened set on extra3 object flags.~%Flag hunted set on extra3 object flags.~%")
    (is (= (logior tempus::+item3-lattice-hardened+ tempus::+item3-hunted+)
           (tempus::extra3-flags-of (tempus::olc-obj-of alice))))))

(deftest do-olc-oset-worn/sets-object-worn-flags ()
  (with-obj-olc-fixture (alice)
    (tempus::interpret-command alice "olc oset worn + take hold")
    (char-output-is alice "Flag TAKE set on worn object flags.~%Flag HOLD set on worn object flags.~%")
    (is (= (logior tempus::+item-wear-take+ tempus::+item-wear-hold+)
           (tempus::wear-flags-of (tempus::olc-obj-of alice))))))

(deftest do-olc-oset-value/sets-object-value ()
  (with-obj-olc-fixture (alice)
    (let ((idx (random 4))
          (val (random 256)))
      (tempus::interpret-command alice (format nil "olc oset value ~d ~d" idx val))
      (char-output-is alice (format nil "Value ~d (UNDEFINED) set to ~d.~~%" idx val))
      (is (= val (aref (tempus::value-of (tempus::olc-obj-of alice)) idx))))))

(deftest do-olc-oset-material/sets-object-material ()
  (with-obj-olc-fixture (alice)
    (tempus::interpret-command alice "olc oset material glass")
    (char-output-is alice "Object material set to glass.~%")
    (is (= 160 (tempus::material-of (tempus::olc-obj-of alice))))))

(deftest do-olc-oset-maxdamage/sets-object-maxdamage ()
  (with-obj-olc-fixture (alice)
    (tempus::interpret-command alice "olc oset maxdamage 4242")
    (char-output-is alice "Object maxdamage set to 4242.~%")
    (is (= 4242 (tempus::max-dam-of (tempus::olc-obj-of alice))))))

(deftest do-olc-oset-apply/sets-object-apply ()
  (with-obj-olc-fixture (alice)
    (tempus::interpret-command alice "olc oset apply str 5")
    (char-output-is alice "Done.~%")
    (is (= tempus::+apply-str+ (tempus::location-of (aref (tempus::affected-of (tempus::olc-obj-of alice)) 0))))
    (is (= 5 (tempus::modifier-of (aref (tempus::affected-of (tempus::olc-obj-of alice)) 0))))))

(deftest do-olc-oset-affection/sets-object-affection ()
  (with-obj-olc-fixture (alice)
    (tempus::interpret-command alice "olc oset affection 1 + inflight")
    (char-output-is alice "Flag inflight set on affect object flags.~%")
    (is (= tempus::+aff-inflight+ (aref (tempus::bitvector-of (tempus::olc-obj-of alice)) 0)))))

(deftest do-olc-oload/no-argument-with-edited/loads-edited-object ()
  (with-obj-olc-fixture (alice)
    (with-captured-log log
        (tempus::interpret-command alice "olc oload")
      (is (search "OLC: Alice oloaded a test object(101)" log))
      (char-output-is alice "A test object appears in your hands.~%")
      (is (not (null (first (tempus::carrying-of alice)))))
      (is (= (tempus::vnum-of (first (tempus::carrying-of alice)))
             (tempus::vnum-of (tempus::olc-obj-of alice))))
      (is (= (tempus::timer-of (first (tempus::carrying-of alice))) (tempus::level-of alice))))))

(deftest do-olc-oload/with-argument/loads-object ()
  (with-obj-olc-fixture (alice)
    (with-captured-log log
        (tempus::interpret-command alice "olc oload 101")
      (is (search "OLC: Alice oloaded a test object(101)" log))
      (char-output-is alice "A test object appears in your hands.~%")
      (is (not (null (first (tempus::carrying-of alice)))))
      (is (= (tempus::vnum-of (first (tempus::carrying-of alice)))
             (tempus::vnum-of (tempus::olc-obj-of alice))))
      (is (= (tempus::timer-of (first (tempus::carrying-of alice))) (tempus::level-of alice))))))

(deftest save-zone-data/saves-zone-file ()
  (with-fixtures ((alice mock-player :level 51 :override-security t))
    (unwind-protect
         (progn
           (rename-file (tempus::tempus-path "lib/world/zon/376.zon") (tempus::tempus-path "lib/world/zon/376.zon.test"))
           (with-captured-log log
               (tempus::save-zone-data alice (tempus::real-zone 376))
             (is (search "OLC: Alice zsaved 376" log))
             (is (equal (tempus::snarf-file (tempus::tempus-path "lib/world/zon/376.zon"))
                        (tempus::snarf-file (tempus::tempus-path "lib/world/zon/376.zon.test"))))))
      (progn
        (when (probe-file (tempus::tempus-path "lib/world/zon/376.zon"))
          (delete-file (tempus::tempus-path "lib/world/zon/376.zon")))
        (rename-file (tempus::tempus-path "lib/world/zon/376.zon.test") (tempus::tempus-path "lib/world/zon/376.zon"))))))

(deftest do-olc-zcmd-list/no-args/lists-zone-commands ()
  (with-fixtures ((alice mock-player :level 51 :override-security t))
    (tempus::char-from-room alice nil)
    (tempus::char-to-room alice (tempus::real-room 3013) nil)
    (tempus::interpret-command alice "olc zcmd list")
    (char-output-has alice "Mobile")
    (char-output-has alice "Give")
    (char-output-has alice "Equip")))

(deftest do-olc-zcmd-list/with-args/lists-zone-commands ()
  (with-fixtures ((alice mock-player :level 51 :override-security t))
    (tempus::char-from-room alice nil)
    (tempus::char-to-room alice (tempus::real-room 3013) nil)
    (tempus::interpret-command alice "olc zcmd list mobile")
    (char-output-has alice "Mobile")
    (is (null (search "Give" (char-output alice))))
    (is (null (search "Equip" (char-output alice))))))

(deftest do-olc-zset-name/sets-zone-name ()
  (with-zone-olc-fixture (alice room zone)
    (tempus::interpret-command alice "olc zset name Testing")
    (is (equal "Testing" (tempus::name-of zone)))
    (char-output-is alice "Zone name set to 'Testing'.~%")))

(deftest do-olc-zset-respawn-pt/arg-is-none/clears-respawn-point ()
  (with-zone-olc-fixture (alice room zone)
    (tempus::interpret-command alice "olc zset respawn_pt none")
    (is (null (tempus::respawn-pt-of zone)))
    (char-output-is alice "Zone 1 respawn point cleared.~%")))

(deftest do-olc-zset-respawn-pt/arg-is-number/sets-respawn-point ()
  (with-zone-olc-fixture (alice room zone)
    (tempus::interpret-command alice "olc zset respawn_pt 100")
    (is (= 100 (tempus::respawn-pt-of zone)))
    (char-output-is alice "Zone 1 respawn point set to room 100.~%")))

(deftest do-olc-zset-top/sets-zone-top ()
  (with-zone-olc-fixture (alice room zone)
    (tempus::interpret-command alice "olc zset top 262")
    (is (= 262 (tempus::top-of zone)))
    (char-output-is alice "Zone 1 top number set to 262.~%")))

(deftest do-olc-zset-reset/sets-zone-reset ()
  (with-zone-olc-fixture (alice room zone)
    (tempus::interpret-command alice "olc zset reset always")
    (is (= 2 (tempus::reset-mode-of zone)))
    (char-output-is alice "Zone reset mode set to ALWAYS.~%")))

(deftest do-olc-zset-timeframe/sets-zone-timeframe ()
  (with-zone-olc-fixture (alice room zone)
    (tempus::interpret-command alice "olc zset tframe electro")
    (is (= tempus::+time-future+ (tempus::time-frame-of zone)))
    (char-output-is alice "Zone timeframe set to Electro Era.~%")))

(deftest do-olc-zset-plane/sets-zone-plane ()
  (with-zone-olc-fixture (alice room zone)
    (tempus::interpret-command alice "olc zset plane astral")
    (is (= tempus::+plane-astral+ (tempus::plane-of zone)))
    (char-output-is alice "Zone plane set to Astral.~%")))

(deftest do-olc-zset-author/sets-zone-author ()
  (with-zone-olc-fixture (alice room zone)
    (tempus::interpret-command alice "olc zset author Alice of TempusMUD")
    (is (equal "Alice of TempusMUD" (tempus::author-of zone)))
    (char-output-is alice "Zone author set to 'Alice of TempusMUD'.~%")))

(deftest do-olc-zset-owner/with-player/sets-zone-owner ()
  (with-zone-olc-fixture (alice room zone)
    (tempus::interpret-command alice "olc zset owner Azimuth")
    (is (= 2 (tempus::owner-idnum-of zone)))
    (char-output-is alice "Zone 1 owner set to Azimuth.~%")))

(deftest do-olc-zset-owner/with-none/clears-zone-owner ()
  (with-zone-olc-fixture (alice room zone)
    (tempus::interpret-command alice "olc zset owner none")
    (is (zerop (tempus::owner-idnum-of zone)))
    (char-output-is alice "Zone 1 owner unset.~%")))

(deftest do-olc-zset-co-owner/with-player/sets-zone-co-owner ()
  (with-zone-olc-fixture (alice room zone)
    (tempus::interpret-command alice "olc zset co-owner Azimuth")
    (is (= 2 (tempus::co-owner-idnum-of zone)))
    (char-output-is alice "Zone 1 co-owner set to Azimuth.~%")))

(deftest do-olc-zset-co-owner/with-none/clears-zone-co-owner ()
  (with-zone-olc-fixture (alice room zone)
    (tempus::interpret-command alice "olc zset co-owner none")
    (is (zerop (tempus::co-owner-idnum-of zone)))
    (char-output-is alice "Zone 1 co-owner unset.~%")))

(deftest do-olc-zset-flags/with-plus-and-minus/sets-and-clears-zone-flags ()
  (with-zone-olc-fixture (alice room zone)
    (setf (tempus::flags-of zone) 0)
    (tempus::interpret-command alice "olc zset flags + !magic !law !weather inplay")
    (is (= (logior tempus::+zone-nomagic+
                   tempus::+zone-nolaw+
                   tempus::+zone-noweather+
                   tempus::+zone-inplay+)
           (tempus::flags-of zone)))
    (char-output-is alice "Flag !MAGIC set on zone flags.~%Flag !LAW set on zone flags.~%Flag !WEATHER set on zone flags.~%Flag INPLAY set on zone flags.~%")
    (clear-mock-buffers alice)
    (tempus::interpret-command alice "olc zset flags - !magic !law !weather inplay")
    (is (zerop (tempus::flags-of zone)))
    (char-output-is alice "Flag !MAGIC unset on zone flags.~%Flag !LAW unset on zone flags.~%Flag !WEATHER unset on zone flags.~%Flag INPLAY unset on zone flags.~%")))

(deftest do-olc-zset-hour/sets-zone-hour-offset ()
  (with-zone-olc-fixture (alice room zone)
    (tempus::interpret-command alice "olc zset hour 10")
    (is (= 10 (tempus::hour-mod-of zone)))
    (char-output-is alice "Zone hour mod set to 10.~%")))

(deftest do-olc-zset-year/sets-zone-year-offset ()
  (with-zone-olc-fixture (alice room zone)
    (tempus::interpret-command alice "olc zset year 10")
    (is (= 10 (tempus::year-mod-of zone)))
    (char-output-is alice "Zone year mod set to 10.~%")))

(deftest do-olc-zset-pkstyle/set-zone-pkstyle ()
  (with-zone-olc-fixture (alice room zone)
    (tempus::interpret-command alice "olc zset pkstyle npk")
    (is (= tempus::+zone-neutral-pk+ (tempus::pk-style-of zone)))
    (char-output-is alice "Zone pk-style set to NPK.~%")))

(deftest do-olc-zset-min-lvl/set-zone-min-lvl ()
  (with-zone-olc-fixture (alice room zone)
    (tempus::interpret-command alice "olc zset min_lvl 35")
    (is (= 35 (tempus::min-lvl-of zone)))
    (char-output-is alice "Zone minimum recommended player level set to 35.~%")))

(deftest do-olc-zset-min-gen/set-zone-min-gen ()
  (with-zone-olc-fixture (alice room zone)
    (tempus::interpret-command alice "olc zset min_gen 6")
    (is (= 6 (tempus::min-gen-of zone)))
    (char-output-is alice "Zone minimum recommended player gen set to 6.~%")))

(deftest do-olc-zset-max-lvl/set-zone-max-lvl ()
  (with-zone-olc-fixture (alice room zone)
    (tempus::interpret-command alice "olc zset max_lvl 35")
    (is (= 35 (tempus::max-lvl-of zone)))
    (char-output-is alice "Zone maximum recommended player level set to 35.~%")))

(deftest do-olc-zset-max-gen/set-zone-max-gen ()
  (with-zone-olc-fixture (alice room zone)
    (tempus::interpret-command alice "olc zset max_gen 6")
    (is (= 6 (tempus::max-gen-of zone)))
    (char-output-is alice "Zone maximum recommended player gen set to 6.~%")))

(deftest do-olc-zcmd-cmdremove/removes-command ()
  (with-zone-olc-fixture (alice room zone)
    (let ((reset-cmds (list (make-instance 'tempus::reset-com
                                           :command #\M
                                           :if-flag 0
                                           :arg1 100
                                           :arg2 1
                                           :arg3 100
                                           :prob 100)
                            (make-instance 'tempus::reset-com
                                           :command #\G
                                           :if-flag 1
                                           :arg1 100
                                           :arg2 1
                                           :arg3 100
                                           :prob 100)
                            (make-instance 'tempus::reset-com
                                           :command #\O
                                           :if-flag 0
                                           :arg1 100
                                           :arg2 1
                                           :arg3 100
                                           :prob 100))))
      (setf (tempus::cmds-of zone) (copy-list reset-cmds))
      (tempus::interpret-command alice "olc zcmd cmdremove 2")
      (is (equal (list (nth 0 reset-cmds)
                       (nth 2 reset-cmds))
                 (tempus::cmds-of zone)))
      (char-output-is alice "Command 2 removed.~%"))))

(deftest do-olc-zcmd-m/adds-zone-command ()
  (with-zone-olc-fixture (alice room zone)
    (tempus::interpret-command alice "olc zcmd m 0 100 1 101 42")
    (char-output-is alice "Command completed ok.~%")
    (is (= 1 (length (tempus::cmds-of zone))))
    (is (zcmd-equal (first (tempus::cmds-of zone))
                    0 #\M 100 1 101 42))))

(deftest do-olc-zcmd-o/adds-zone-command ()
  (with-zone-olc-fixture (alice room zone)
    (tempus::interpret-command alice "olc zcmd o 1 100 1 101 42")
    (char-output-is alice "Command completed ok.~%")
    (is (= 1 (length (tempus::cmds-of zone))))
    (is (zcmd-equal (first (tempus::cmds-of zone))
                    1 #\O 100 1 101 42))))

(deftest do-olc-zcmd-p/adds-zone-command ()
  (with-zone-olc-fixture (alice room zone)
    (tempus::interpret-command alice "olc zcmd p 1 100 1 102 42")
    (char-output-is alice "Command completed ok.~%")
    (is (= 1 (length (tempus::cmds-of zone))))
    (is (zcmd-equal (first (tempus::cmds-of zone))
                    1 #\P 100 1 102 42))))

(deftest do-olc-zcmd-g/adds-zone-command ()
  (with-zone-olc-fixture (alice room zone)
    (tempus::interpret-command alice "olc zcmd g 1 100 1 100 42")
    (char-output-is alice "Command completed ok.~%")
    (is (= 1 (length (tempus::cmds-of zone))))
    (is (zcmd-equal (first (tempus::cmds-of zone))
                    1 #\G 100 1 100 42))))

(deftest do-olc-zcmd-e/adds-zone-command ()
  (with-zone-olc-fixture (alice room zone)
    (tempus::interpret-command alice "olc zcmd e 1 100 1 5 42")
    (char-output-is alice "Command completed ok.~%")
    (is (= 1 (length (tempus::cmds-of zone))))
    (is (zcmd-equal (first (tempus::cmds-of zone))
                    1 #\E 100 1 5 42))))

(deftest do-olc-zcmd-i/adds-zone-command ()
  (with-zone-olc-fixture (alice room zone)
    (tempus::interpret-command alice "olc zcmd i 1 100 1 5 42")
    (char-output-is alice "Command completed ok.~%")
    (is (= 1 (length (tempus::cmds-of zone))))
    (is (zcmd-equal (first (tempus::cmds-of zone))
                    1 #\I 100 1 5 42))))

(deftest do-olc-zcmd-r/adds-zone-command ()
  (with-zone-olc-fixture (alice room zone)
    (tempus::interpret-command alice "olc zcmd r 0 100 101 42")
    (char-output-is alice "Command completed ok.~%")
    (is (= 1 (length (tempus::cmds-of zone))))
    (is (zcmd-equal (first (tempus::cmds-of zone))
                    0 #\R 100 101 -1 42))))

(deftest do-olc-zcmd-d/adds-zone-command ()
  (with-zone-olc-fixture (alice room zone)
    (tempus::interpret-command alice "olc zcmd d 0 100 east closed locked")
    (char-output-is alice "Command completed ok.~%")
    (is (= 1 (length (tempus::cmds-of zone))))
    (is (zcmd-equal (first (tempus::cmds-of zone))
                    0 #\D 100 1
                    (logior tempus::+door-closed+
                            tempus::+door-locked+)
                    100))))

(deftest do-olc-zmob/adds-zcmd-and-mob ()
  (with-zone-olc-fixture (alice room zone)
    (tempus::interpret-command alice "olc zmob 100 1 42")
    (char-output-is alice "Command completed ok.~%")
    (is (= 1 (length (tempus::cmds-of zone))))
    (is (zcmd-equal (first (tempus::cmds-of zone))
                    0 #\M 100 1 (tempus::number-of room) 42))
    (is (typep (first (tempus::people-of room))
               'tempus::mobile))
    (is (= 100 (tempus::vnum-of (first (tempus::people-of room)))))))

(deftest do-olc-zequip/adds-zcmd-and-equips ()
  (with-zone-olc-fixture (alice room zone)
    (with-fixtures ((proto-mallory mock-mob-prototype))
      (tempus::push-zcmd zone nil #\M 0
                         (tempus::vnum-of proto-mallory) 1
                         (tempus::number-of room) 100)
      (let ((mallory (tempus::read-mobile (tempus::vnum-of proto-mallory))))
        (tempus::char-to-room mallory room nil)
        (tempus::interpret-command alice "olc zequip mobile 100 1 12 42")
        (char-output-is alice "Command completed ok.~%")
        (is (= 2 (length (tempus::cmds-of zone))))
        (is (zcmd-equal (second (tempus::cmds-of zone))
                    1 #\E 100 1 12 42))
        (is (= 100
               (tempus::vnum-of (aref (tempus::equipment-of mallory) 12))))))))

(deftest do-olc-zimplant/adds-zcmd-and-implants ()
  (with-zone-olc-fixture (alice room zone)
    (with-fixtures ((proto-mallory mock-mob-prototype))
      (tempus::push-zcmd zone nil #\M 0
                         (tempus::vnum-of proto-mallory) 1
                         (tempus::number-of room) 100)
      (let ((mallory (tempus::read-mobile (tempus::vnum-of proto-mallory))))
        (tempus::char-to-room mallory room nil)
        (tempus::interpret-command alice "olc zimplant mobile 100 1 12 42")
        (char-output-is alice "Command completed ok.~%")
        (is (= 2 (length (tempus::cmds-of zone))))
        (is (zcmd-equal (second (tempus::cmds-of zone))
                    1 #\I 100 1 12 42))
        (is (= 100
               (tempus::vnum-of (aref (tempus::implants-of mallory) 12))))))))

(deftest do-olc-zobj/adds-zcmd-and-obj ()
  (with-zone-olc-fixture (alice room zone)
    (tempus::interpret-command alice "olc zobj 100 1 42")
    (char-output-is alice "Command completed ok.~%")
    (is (= 1 (length (tempus::cmds-of zone))))
    (is (zcmd-equal (first (tempus::cmds-of zone))
                    0 #\O 100 1 (tempus::number-of room) 42))
    (is (typep (first (tempus::contents-of room))
               'tempus::obj-data))
    (is (= 100 (tempus::vnum-of (first (tempus::contents-of room)))))))

(deftest do-olc-zdoor/one-way/adds-zcmd ()
  (with-zone-olc-fixture (alice room zone)
    (setf (aref (tempus::dir-option-of room) tempus::+south+)
          (make-instance 'tempus::room-direction-data
                         :exit-info tempus::+door-open+
                         :to-room (tempus::real-room 100)))
    (tempus::interpret-command alice "olc zdoor south closed locked")
    (char-output-is alice "Command completed ok.~%")
    (is (= 1 (length (tempus::cmds-of zone))))
    (is (zcmd-equal (first (tempus::cmds-of zone))
                    0 #\D (tempus::number-of room) 2 (logior tempus::+door-closed+
                                                             tempus::+door-locked+)
                    100))))

(deftest do-olc-zdoor/two-way/adds-zcmd ()
  (with-zone-olc-fixture (alice room zone)
    (unwind-protect
         (progn
           (setf (aref (tempus::dir-option-of room) tempus::+south+)
                 (make-instance 'tempus::room-direction-data
                                :exit-info tempus::+door-open+
                                :to-room (tempus::real-room 100)))
           (setf (aref (tempus::dir-option-of (tempus::real-room 100)) tempus::+north+)
                 (make-instance 'tempus::room-direction-data
                                :exit-info tempus::+door-open+
                                :to-room room))
           (tempus::interpret-command alice "olc zdoor south closed locked")
           (char-output-is alice "Command completed ok.~%")
           (is (= 2 (length (tempus::cmds-of zone))))
           (is (zcmd-equal (first (tempus::cmds-of zone))
                           0 #\D (tempus::number-of room) 2 (logior tempus::+door-closed+
                                                                    tempus::+door-locked+)
                           100))
           (is (zcmd-equal (second (tempus::cmds-of zone))
                           0 #\D 100 0 (logior tempus::+door-closed+ tempus::+door-locked+)
                           100)))
      (setf (aref (tempus::dir-option-of (tempus::real-room 100)) tempus::+north+) nil))))

(deftest do-olc-zput/adds-zcmd-and-loads ()
  (with-zone-olc-fixture (alice room zone)
    (with-fixtures ((proto-obj mock-obj-prototype))
      (tempus::push-zcmd zone nil #\O 0
                         (tempus::vnum-of proto-obj) 1
                         (tempus::number-of room) 100)
      (let ((obj (tempus::read-object (tempus::vnum-of proto-obj))))
        (tempus::obj-to-room obj room)
        (tempus::interpret-command alice "olc zput mock 100 1 42")
        (char-output-is alice "Command completed ok.~%")
        (is (= 2 (length (tempus::cmds-of zone))))
        (is (zcmd-equal (second (tempus::cmds-of zone))
                    1 #\P 100 1 (tempus::vnum-of proto-obj) 42))
        (is (= 100 (tempus::vnum-of (first (tempus::contains-of obj)))))))))

(deftest do-olc-zgive/adds-zcmd-and-loads-obj ()
  (with-zone-olc-fixture (alice room zone)
    (with-fixtures ((proto-mallory mock-mob-prototype))
      (tempus::push-zcmd zone nil #\M 0
                         (tempus::vnum-of proto-mallory) 1
                         (tempus::number-of room) 100)
      (let ((mallory (tempus::read-mobile (tempus::vnum-of proto-mallory))))
        (tempus::char-to-room mallory room nil)
        (tempus::interpret-command alice "olc zgive mobile 100 1 42")
        (char-output-is alice "Command completed ok.~%")
        (is (= 2 (length (tempus::cmds-of zone))))
        (is (zcmd-equal (second (tempus::cmds-of zone))
                    1 #\G 100 1 (tempus::vnum-of proto-mallory) 42))
        (is (= 100 (tempus::vnum-of (first (tempus::carrying-of mallory)))))))))

(deftest do-olc-zpath-mobile/adds-zcmd ()
  (with-zone-olc-fixture (alice room zone)
    (with-fixtures ((proto-mallory mock-mob-prototype))
      (tempus::push-zcmd zone nil #\M 0
                         (tempus::vnum-of proto-mallory) 1
                         (tempus::number-of room) 100)
      (let ((mallory (tempus::read-mobile (tempus::vnum-of proto-mallory))))
        (tempus::char-to-room mallory room nil)
        (tempus::interpret-command alice "olc zpath mobile mock alpha")
        (char-output-is alice "Command completed ok.~%")
        (is (= 2 (length (tempus::cmds-of zone))))
        (is (zcmd-equal (second (tempus::cmds-of zone))
                    1 #\W 1 0 (tempus::vnum-of proto-mallory) 100))))))

(deftest do-olc-zpath-object/adds-zcmd ()
  (with-zone-olc-fixture (alice room zone)
    (with-fixtures ((proto-obj mock-obj-prototype))
      (tempus::push-zcmd zone nil #\O 0
                         (tempus::vnum-of proto-obj) 1
                         (tempus::number-of room) 100)
      (let ((obj (tempus::read-object (tempus::vnum-of proto-obj))))
        (tempus::obj-to-room obj room)
        (tempus::interpret-command alice "olc zpath object mock alpha")
        (char-output-is alice "Command completed ok.~%")
        (is (= 2 (length (tempus::cmds-of zone))))
        (is (zcmd-equal (second (tempus::cmds-of zone))
                    1 #\V 1 0 (tempus::vnum-of proto-obj) 100))))))

(deftest do-olc-zpurge/purges-zone ()
  (with-zone-olc-fixture (alice room zone)
    (with-captured-log log
        (tempus::interpret-command alice "olc zpurge")
      (is (search "(GC) Alice olc-purged zone 1 (Coder Test Zone)" log))
      (char-output-has alice "Zone 1 cleared of"))))

(deftest save-zone-mobiles/simple-mobiles/saves-zone-file ()
  (with-fixtures ((alice mock-player :level 51 :override-security t))
    (unwind-protect
         (progn
           (rename-file (tempus::tempus-path "lib/world/mob/348.mob") (tempus::tempus-path "lib/world/mob/348.mob.test"))
           (with-captured-log log
               (tempus::save-zone-mobiles alice (tempus::real-zone 348))
             (is (search "OLC: Alice msaved 348" log))
             (is (equal (tempus::snarf-file (tempus::tempus-path "lib/world/mob/348.mob"))
                        (tempus::snarf-file (tempus::tempus-path "lib/world/mob/348.mob.test"))))))
      (progn
        (when (probe-file (tempus::tempus-path "lib/world/mob/348.mob"))
          (delete-file (tempus::tempus-path "lib/world/mob/348.mob")))
        (rename-file (tempus::tempus-path "lib/world/mob/348.mob.test") (tempus::tempus-path "lib/world/mob/348.mob"))))))

(deftest save-zone-mobiles/complex-mobiles/saves-zone-file ()
  (with-fixtures ((alice mock-player :level 51 :override-security t))
    (unwind-protect
         (progn
           (rename-file (tempus::tempus-path "lib/world/mob/376.mob") (tempus::tempus-path "lib/world/mob/376.mob.test"))
           (with-captured-log log
               (tempus::save-zone-mobiles alice (tempus::real-zone 376))
             (is (search "OLC: Alice msaved 376" log))
             (is (equal (tempus::snarf-file (tempus::tempus-path "lib/world/mob/376.mob"))
                        (tempus::snarf-file (tempus::tempus-path "lib/world/mob/376.mob.test"))))))
      (progn
        (when (probe-file (tempus::tempus-path "lib/world/mob/376.mob"))
          (delete-file (tempus::tempus-path "lib/world/mob/376.mob")))
        (rename-file (tempus::tempus-path "lib/world/mob/376.mob.test") (tempus::tempus-path "lib/world/mob/376.mob"))))))

(deftest perform-create-and-destroy-mobile/creates-and-destroys-mobile ()
  (with-fixtures ((alice mock-player :level 51 :override-security t))
    (tempus::perform-create-mobile alice 101)
    (char-output-is alice "Mobile 101 successfully created.~%Now editing mobile 101.~%")
    (let ((mob (gethash 101 tempus::*mobile-prototypes*)))
      (is (not (null mob)))
      (is (= 101 (tempus::vnum-of mob)))
      (is (eql mob (tempus::olc-mob-of alice)))
      (is (eql mob (tempus::proto-of (tempus::shared-of mob)))))
    (clear-mock-buffers alice)
    (tempus::perform-destroy-mobile alice 101)
    (char-output-is alice "The mobile you were editing has been destroyed.~%Mobile eliminated.~%")
    (is (null (tempus::olc-mob-of alice)))
    (is (null (gethash 101 tempus::*mobile-prototypes*)))))

(deftest do-olc-mload/no-argument-with-edited/loads-edited-mobile ()
  (with-mob-olc-fixture (alice)
    (with-captured-log log
        (tempus::interpret-command alice "olc mload")
      (is (search "OLC: Alice mloaded a test mobile(101)" log))
      (char-output-is alice "A test mobile appears next to you.~%")
      (is (= (tempus::vnum-of (first (tempus::people-of (tempus::in-room-of alice))))
             (tempus::vnum-of (tempus::olc-mob-of alice)))))))

(deftest do-olc-mload/with-argument/loads-mobile ()
  (with-mob-olc-fixture (alice)
    (with-captured-log log
        (tempus::interpret-command alice "olc mload 101")
      (is (search "OLC: Alice mloaded a test mobile(101)" log))
      (char-output-is alice "A test mobile appears next to you.~%")
      (is (= (tempus::vnum-of (first (tempus::people-of (tempus::in-room-of alice)))) 101)))))

(deftest do-olc-mset-alias/sets-alias ()
  (with-mob-olc-fixture (alice)
    (tempus::interpret-command alice "olc mset alias testing")
    (char-output-is alice "Mobile alias set to 'testing'.~%")
    (is (equal (tempus::aliases-of (tempus::olc-mob-of alice)) "testing"))))

(deftest do-olc-mset-name/sets-name ()
  (with-mob-olc-fixture (alice)
    (tempus::interpret-command alice "olc mset name testing")
    (char-output-is alice "Mobile name set to 'testing'.~%")
    (is (equal (tempus::name-of (tempus::olc-mob-of alice)) "testing"))))

(deftest do-olc-mset-ldesc/sets-ldesc ()
  (with-mob-olc-fixture (alice)
    (tempus::interpret-command alice "olc mset ldesc testing")
    (char-output-is alice "Mobile ldesc set to 'testing'.~%")
    (is (equal (tempus::ldesc-of (tempus::olc-mob-of alice)) "testing"))))

(deftest do-olc-mset-flags/sets-flags ()
  (with-mob-olc-fixture (alice)
    (tempus::interpret-command alice "olc mset flags + sentinel scavenger")
    (char-output-is alice "Flag Sentinel set on mobile flags.~%Flag Scavenger set on mobile flags.~%")
    (is (logtest (tempus::mob-flags-of (tempus::olc-mob-of alice)) tempus::+mob-sentinel+))
    (is (logtest (tempus::mob-flags-of (tempus::olc-mob-of alice)) tempus::+mob-scavenger+))))

(deftest do-olc-mset-flags2/sets-flags ()
  (with-mob-olc-fixture (alice)
    (tempus::interpret-command alice "olc mset flags2 + mount hunt")
    (char-output-is alice "Flag Mount set on mobile flags.~%Flag Hunt set on mobile flags.~%")
    (is (logtest (tempus::mob2-flags-of (tempus::olc-mob-of alice)) tempus::+mob2-mount+))
    (is (logtest (tempus::mob2-flags-of (tempus::olc-mob-of alice)) tempus::+mob2-hunt+))))

(deftest do-olc-mset-aff/sets-flags ()
  (with-mob-olc-fixture (alice)
    (tempus::interpret-command alice "olc mset aff + blind invisible")
    (char-output-is alice "Flag Blind set on mobile aff flags.~%Flag Invisible set on mobile aff flags.~%")
    (is (logtest (tempus::aff-flags-of (tempus::olc-mob-of alice)) tempus::+aff-blind+))
    (is (logtest (tempus::aff-flags-of (tempus::olc-mob-of alice)) tempus::+aff-invisible+))))

(deftest do-olc-mset-aff2/sets-flags ()
  (with-mob-olc-fixture (alice)
    (tempus::interpret-command alice "olc mset aff2 + slow berserk")
    (char-output-is alice "Flag Slow set on mobile aff2 flags.~%Flag Berserk set on mobile aff2 flags.~%")
    (is (logtest (tempus::aff2-flags-of (tempus::olc-mob-of alice)) tempus::+aff2-slow+))
    (is (logtest (tempus::aff2-flags-of (tempus::olc-mob-of alice)) tempus::+aff2-berserk+))))

(deftest do-olc-mset-aff3/sets-flags ()
  (with-mob-olc-fixture (alice)
    (tempus::interpret-command alice "olc mset aff3 + nobreathe double_damage")
    (char-output-is alice "Flag Nobreathe set on mobile aff3 flags.~%Flag double_damage set on mobile aff3 flags.~%")
    (is (logtest (tempus::aff3-flags-of (tempus::olc-mob-of alice)) tempus::+aff3-nobreathe+))
    (is (logtest (tempus::aff3-flags-of (tempus::olc-mob-of alice)) tempus::+aff3-double-damage+))))

(deftest do-olc-mset-alignment/sets-alignment ()
  (with-mob-olc-fixture (alice)
    (tempus::interpret-command alice "olc mset alignment -666")
    (char-output-is alice "Mobile alignment set to -666.~%")
    (is (= (tempus::alignment-of (tempus::olc-mob-of alice)) -666))))

(deftest do-olc-mset-str/sets-str ()
  (with-mob-olc-fixture (alice)
    (tempus::interpret-command alice "olc mset str 5")
    (char-output-is alice "Mobile strength set to 5.~%")
    (is (= (tempus::str-of (tempus::real-abils-of (tempus::olc-mob-of alice))) 5))
    (is (= (tempus::str-of (tempus::aff-abils-of (tempus::olc-mob-of alice))) 5))))

(deftest do-olc-mset-int/sets-int ()
  (with-mob-olc-fixture (alice)
    (tempus::interpret-command alice "olc mset int 5")
    (char-output-is alice "Mobile intelligence set to 5.~%")
    (is (= (tempus::int-of (tempus::real-abils-of (tempus::olc-mob-of alice))) 5))
    (is (= (tempus::int-of (tempus::aff-abils-of (tempus::olc-mob-of alice))) 5))))

(deftest do-olc-mset-wis/sets-wis ()
  (with-mob-olc-fixture (alice)
    (tempus::interpret-command alice "olc mset wis 5")
    (char-output-is alice "Mobile wisdom set to 5.~%")
    (is (= (tempus::wis-of (tempus::real-abils-of (tempus::olc-mob-of alice))) 5))
    (is (= (tempus::wis-of (tempus::aff-abils-of (tempus::olc-mob-of alice))) 5))))

(deftest do-olc-mset-dex/sets-dex ()
  (with-mob-olc-fixture (alice)
    (tempus::interpret-command alice "olc mset dex 5")
    (char-output-is alice "Mobile dexterity set to 5.~%")
    (is (= (tempus::dex-of (tempus::real-abils-of (tempus::olc-mob-of alice))) 5))
    (is (= (tempus::dex-of (tempus::aff-abils-of (tempus::olc-mob-of alice))) 5))))

(deftest do-olc-mset-con/sets-con ()
  (with-mob-olc-fixture (alice)
    (tempus::interpret-command alice "olc mset con 5")
    (char-output-is alice "Mobile constitution set to 5.~%")
    (is (= (tempus::con-of (tempus::real-abils-of (tempus::olc-mob-of alice))) 5))
    (is (= (tempus::con-of (tempus::aff-abils-of (tempus::olc-mob-of alice))) 5))))

(deftest do-olc-mset-cha/sets-cha ()
  (with-mob-olc-fixture (alice)
    (tempus::interpret-command alice "olc mset cha 5")
    (char-output-is alice "Mobile charisma set to 5.~%")
    (is (= (tempus::cha-of (tempus::real-abils-of (tempus::olc-mob-of alice))) 5))
    (is (= (tempus::cha-of (tempus::aff-abils-of (tempus::olc-mob-of alice))) 5))))

(deftest do-olc-mset-level/sets-level ()
  (with-mob-olc-fixture (alice)
    (tempus::interpret-command alice "olc mset level 5")
    (char-output-is alice "Mobile level set to 5.~%")
    (is (= (tempus::level-of (tempus::olc-mob-of alice)) 5))))

(deftest do-olc-mset-hitp-mod/sets-hitp-mod ()
  (with-mob-olc-fixture (alice)
    (tempus::interpret-command alice "olc mset hitp_mod 5")
    (char-output-is alice "Mobile hitpoint modifier set to 5.~%")
    (is (= (tempus::move-of (tempus::olc-mob-of alice)) 5))))

(deftest do-olc-mset-hitd-num/sets-hitd-num ()
  (with-mob-olc-fixture (alice)
    (tempus::interpret-command alice "olc mset hitd_num 5")
    (char-output-is alice "Mobile hitpoint dice number set to 5.~%")
    (is (= (tempus::hitp-of (tempus::olc-mob-of alice)) 5))))

(deftest do-olc-mset-hitd-size/sets-hitd-size ()
  (with-mob-olc-fixture (alice)
    (tempus::interpret-command alice "olc mset hitd_size 5")
    (char-output-is alice "Mobile hitpoint dice size set to 5.~%")
    (is (= (tempus::mana-of (tempus::olc-mob-of alice)) 5))))

(deftest do-olc-mset-mana/sets-mana ()
  (with-mob-olc-fixture (alice)
    (tempus::interpret-command alice "olc mset mana 5")
    (char-output-is alice "Mobile mana set to 5.~%")
    (is (= (tempus::max-mana-of (tempus::olc-mob-of alice)) 5))))

(deftest do-olc-mset-move/sets-move ()
  (with-mob-olc-fixture (alice)
    (tempus::interpret-command alice "olc mset move 5")
    (char-output-is alice "Mobile movement set to 5.~%")
    (is (= (tempus::max-move-of (tempus::olc-mob-of alice)) 5))))

(deftest do-olc-mset-baredam/sets-baredam ()
  (with-mob-olc-fixture (alice)
    (tempus::interpret-command alice "olc mset baredam 5")
    (char-output-is alice "Mobile bare hand damage set to 5.~%")
    (is (= (tempus::damnodice-of (tempus::shared-of (tempus::olc-mob-of alice))) 5))))

(deftest do-olc-mset-baredsize/sets-baredsize ()
  (with-mob-olc-fixture (alice)
    (tempus::interpret-command alice "olc mset baredsize 5")
    (char-output-is alice "Mobile bare hand damage dice size set to 5.~%")
    (is (= (tempus::damsizedice-of (tempus::shared-of (tempus::olc-mob-of alice))) 5))))

(deftest do-olc-mset-gold/sets-gold ()
  (with-mob-olc-fixture (alice)
    (tempus::interpret-command alice "olc mset gold 5")
    (char-output-is alice "Mobile gold set to 5.~%")
    (is (= (tempus::gold-of (tempus::olc-mob-of alice)) 5))))

(deftest do-olc-mset-exp/sets-exp ()
  (with-mob-olc-fixture (alice)
    (tempus::interpret-command alice "olc mset exp 12345")
    (char-output-is alice "Mobile experience set to 12345.~%")
    (is (= (tempus::exp-of (tempus::olc-mob-of alice)) 12345))))

(deftest do-olc-mset-attacktype/sets-attacktype ()
  (with-mob-olc-fixture (alice)
    (tempus::interpret-command alice "olc mset attacktype sting")
    (char-output-is alice "Mobile attack type set to sting.~%")
    (is (= (tempus::attack-type-of (tempus::shared-of (tempus::olc-mob-of alice))) 1))))

(deftest do-olc-mset-position/sets-position ()
  (with-mob-olc-fixture (alice)
    (tempus::interpret-command alice "olc mset position sitting")
    (char-output-is alice "Mobile position set to sitting.~%")
    (is (= (tempus::position-of (tempus::olc-mob-of alice)) tempus::+pos-sitting+))))

(deftest do-olc-mset-sex/sets-sex ()
  (with-mob-olc-fixture (alice)
    (tempus::interpret-command alice "olc mset sex male")
    (char-output-is alice "Mobile sex set to male.~%")
    (is (= (tempus::sex-of (tempus::olc-mob-of alice)) 1))))

(deftest do-olc-mset-remort-class/sets-remort-class ()
  (with-mob-olc-fixture (alice)
    (tempus::interpret-command alice "olc mset remort_class barb")
    (char-output-is alice "Mobile remort class set to Barbarian.~%")
    (is (= (tempus::remort-char-class-of (tempus::olc-mob-of alice)) tempus::+class-barb+))))

(deftest do-olc-mset-cash/sets-cash ()
  (with-mob-olc-fixture (alice)
    (tempus::interpret-command alice "olc mset cash 12345")
    (char-output-is alice "Mobile cash credits set to 12345.~%")
    (is (= (tempus::cash-of (tempus::olc-mob-of alice)) 12345))))

(deftest do-olc-mset-hitroll/sets-hitroll ()
  (with-mob-olc-fixture (alice)
    (tempus::interpret-command alice "olc mset hitroll 123")
    (char-output-is alice "Mobile hitroll set to 123.~%")
    (is (= (tempus::hitroll-of (tempus::olc-mob-of alice)) 123))))

(deftest do-olc-mset-damroll/sets-damroll ()
  (with-mob-olc-fixture (alice)
    (tempus::interpret-command alice "olc mset damroll 123")
    (char-output-is alice "Mobile damroll set to 123.~%")
    (is (= (tempus::damroll-of (tempus::olc-mob-of alice)) 123))))

(deftest do-olc-mset-ac/sets-ac ()
  (with-mob-olc-fixture (alice)
    (tempus::interpret-command alice "olc mset ac 50")
    (char-output-is alice "Mobile armor class set to 50.~%")
    (is (= (tempus::armor-of (tempus::olc-mob-of alice)) 50))))

(deftest do-olc-mset-class/sets-class ()
  (with-mob-olc-fixture (alice)
    (tempus::interpret-command alice "olc mset class barb")
    (char-output-is alice "Mobile class set to Barbarian.~%")
    (is (= (tempus::char-class-of (tempus::olc-mob-of alice)) tempus::+class-barb+))))

(deftest do-olc-mset-race/sets-race ()
  (with-mob-olc-fixture (alice)
    (tempus::interpret-command alice "olc mset race minotaur")
    (char-output-is alice "Mobile race set to Minotaur.~%")
    (is (= (tempus::race-of (tempus::olc-mob-of alice)) tempus::+race-minotaur+))))

(deftest do-olc-mset-dpos/sets-dpos ()
  (with-mob-olc-fixture (alice)
    (tempus::interpret-command alice "olc mset dpos sitting")
    (char-output-is alice "Mobile default position set to sitting.~%")
    (is (= (tempus::default-pos-of (tempus::shared-of (tempus::olc-mob-of alice))) tempus::+pos-sitting+))))

(deftest do-olc-mset-height/sets-height ()
  (with-mob-olc-fixture (alice)
    (tempus::interpret-command alice "olc mset height 1234")
    (char-output-is alice "Mobile height set to 1234.~%")
    (is (= (tempus::height-of (tempus::olc-mob-of alice)) 1234))))

(deftest do-olc-mset-weight/sets-weight ()
  (with-mob-olc-fixture (alice)
    (tempus::interpret-command alice "olc mset weight 12345")
    (char-output-is alice "Mobile weight set to 12345.~%")
    (is (= (tempus::weight-of (tempus::olc-mob-of alice)) 12345))))

(deftest do-olc-mset-special/sets-special ()
  (with-mob-olc-fixture (alice)
    (tempus::interpret-command alice "olc mset special postmaster")
    (char-output-is alice "Mobile special set, you trickster you.~%")))

(deftest do-olc-mset-morale/sets-morale ()
  (with-mob-olc-fixture (alice)
    (tempus::interpret-command alice "olc mset morale 55")
    (char-output-is alice "Mobile morale set to 55.~%")
    (is (= (tempus::morale-of (tempus::shared-of (tempus::olc-mob-of alice))) 55))))

(deftest do-olc-mset-move-buf/sets-move-buf ()
  (with-mob-olc-fixture (alice)
    (tempus::interpret-command alice "olc mset move_buf testing")
    (char-output-is alice "Mobile move buffer set to 'testing'.~%")
    (is (equal (tempus::move-buf-of (tempus::shared-of (tempus::olc-mob-of alice))) "testing"))))

(deftest do-olc-mset-lair/sets-lair ()
  (with-mob-olc-fixture (alice)
    (tempus::interpret-command alice "olc mset lair 12345")
    (char-output-is alice "Mobile lair set to 12345.~%")
    (is (= (tempus::lair-of (tempus::shared-of (tempus::olc-mob-of alice))) 12345))))

(deftest do-olc-mset-leader/sets-leader ()
  (with-mob-olc-fixture (alice)
    (tempus::interpret-command alice "olc mset leader 12345")
    (char-output-is alice "Mobile leader vnum set to 12345.~%")
    (is (= (tempus::leader-of (tempus::shared-of (tempus::olc-mob-of alice))) 12345))))

(deftest do-olc-mset-specparam/sets-specparam ()
  (with-mob-olc-fixture (alice)
    (tempus::interpret-command alice "olc mset specparam")
    (is (eql (tempus::state-of (tempus::link-of alice)) 'tempus::editing))
    (is (logtest (tempus::plr-bits-of alice) tempus::+plr-olc+))))

(deftest do-olc-mset-generation/sets-generation ()
  (with-mob-olc-fixture (alice)
    (tempus::interpret-command alice "olc mset generation 5")
    (char-output-is alice "Mobile remort generation set to 5.~%")
    (is (= (tempus::remort-gen-of (tempus::olc-mob-of alice)) 5))))

(deftest do-olc-mset-loadparam/sets-loadparam ()
  (with-mob-olc-fixture (alice)
    (tempus::interpret-command alice "olc mset loadparam")
    (is (eql (tempus::state-of (tempus::link-of alice)) 'tempus::editing))
    (is (logtest (tempus::plr-bits-of alice) tempus::+plr-olc+))))

(deftest do-olc-mset-knownlanguage/adding-language/sets-language-to-100 ()
  (with-mob-olc-fixture (alice)
    (tempus::interpret-command alice "olc mset knownlanguage + klingon")
    (char-output-is alice "Adding mobile language klingon.~%")
    (is (= (aref (tempus::tongues-of (tempus::olc-mob-of alice)) 6) 100))))

(deftest do-olc-mset-knownlanguage/removing-language/sets-language-to-0 ()
  (with-mob-olc-fixture (alice)
    (setf (aref (tempus::tongues-of (tempus::olc-mob-of alice)) 6) 100)
    (tempus::interpret-command alice "olc mset knownlanguage - klingon")
    (char-output-is alice "Removing mobile language klingon.~%")
    (is (zerop (aref (tempus::tongues-of (tempus::olc-mob-of alice)) 6)))))

(deftest do-olc-mset-curlanguage/sets-curlanguage ()
  (with-mob-olc-fixture (alice)
    (tempus::interpret-command alice "olc mset curlanguage klingon")
    (char-output-is alice "Mobile language set to klingon.~%")
    (is (= (tempus::current-tongue-of (tempus::olc-mob-of alice)) 6))))

(deftest do-olc-mset-prog/sets-prog ()
  (with-mob-olc-fixture (alice)
    (tempus::interpret-command alice "olc mset prog")
    (is (eql (tempus::state-of (tempus::link-of alice)) 'tempus::editing))
    (is (logtest (tempus::plr-bits-of alice) tempus::+plr-olc+))))
