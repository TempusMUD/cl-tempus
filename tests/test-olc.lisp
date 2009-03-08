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
                     (,obj mock-obj-prototype))
       (setf (tempus::owner-idnum-of (tempus::zone-of (tempus::in-room-of ,player)))
             (tempus::idnum-of ,player))
       (setf (tempus::olc-obj-of ,player) ,obj)
       ,@body)))

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
    (char-output-is alice "Object 101 successfully created.~%Now editing object 101~%")
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
    (char-output-is alice "Now editing object [100] &ga test object&n~%")
    (is (eql (tempus::real-object-proto 100) (tempus::olc-obj-of alice)))
    (clear-mock-buffers alice)
    (tempus::interpret-command alice "olc oedit exit")
    (char-output-is alice "Exiting object editor.~%")
    (is (null (tempus::olc-obj-of alice)))))

(deftest do-olc-stat/no-argument/stat-editing-object ()
  (with-obj-olc-fixture (alice)
    (tempus::interpret-command alice "olc ostat")
    (char-output-has alice "Name: '&ga mock object&n'")))

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
    (tempus::interpret-command alice "olc oset alias testing")
    (char-output-is alice "Aliases set.~%")
    (is (equal "testing" (tempus::aliases-of (tempus::olc-obj-of alice))))))

(deftest do-olc-oset-name/sets-object-name ()
  (with-obj-olc-fixture (alice)
    (tempus::interpret-command alice "olc oset name testing")
    (char-output-is alice "Object name set.~%")
    (is (equal "testing" (tempus::name-of (tempus::olc-obj-of alice))))))

(deftest do-olc-oset-ldesc/not-tilde/sets-object-ldesc ()
  (with-obj-olc-fixture (alice)
    (tempus::interpret-command alice "olc oset ldesc testing")
    (char-output-is alice "Object L-desc set.~%")
    (is (equal "testing" (tempus::line-desc-of (tempus::olc-obj-of alice))))))

(deftest do-olc-oset-ldesc/with-tilde/nulls-object-ldesc ()
  (with-obj-olc-fixture (alice)
    (tempus::interpret-command alice "olc oset ldesc ~")
    (char-output-is alice "Object L-desc set.~%")
    (is (null (tempus::line-desc-of (tempus::olc-obj-of alice))))))

(deftest do-olc-oset-action-desc/not-tilde/sets-object-action-desc ()
  (with-obj-olc-fixture (alice)
    (tempus::interpret-command alice "olc oset action_desc testing")
    (char-output-is alice "Object action desc set.~%")
    (is (equal "testing" (tempus::action-desc-of (tempus::olc-obj-of alice))))))

(deftest do-olc-oset-action-desc/with-tilde/nulls-object-action-desc ()
  (with-obj-olc-fixture (alice)
    (tempus::interpret-command alice "olc oset action_desc ~")
    (char-output-is alice "Object action desc set.~%")
    (is (null (tempus::action-desc-of (tempus::olc-obj-of alice))))))

(deftest do-olc-oset-type/sets-object-kind ()
  (with-obj-olc-fixture (alice)
    (tempus::interpret-command alice "olc oset type light")
    (char-output-is alice (format nil "Object ~d type set to light.~~%"
                                  (tempus::vnum-of (tempus::olc-obj-of alice))))
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
    (char-output-is alice (format nil "Object ~d material set to glass (160).~~%"
                                  (tempus::vnum-of (tempus::olc-obj-of alice))))
    (is (= 160 (tempus::material-of (tempus::olc-obj-of alice))))))

(deftest do-olc-oset-maxdamage/sets-object-maxdamage ()
  (with-obj-olc-fixture (alice)
    (tempus::interpret-command alice "olc oset maxdamage 4242")
    (char-output-is alice (format nil "Object ~d maxdamage set to 4242.~~%"
                                  (tempus::vnum-of (tempus::olc-obj-of alice))))
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
