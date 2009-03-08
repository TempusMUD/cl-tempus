(in-package #:tempus.tests)

(in-suite (defsuite (tempus.security :in test-full)))

(deftest can-admin-group/is-level-72/returns-t ()
  (with-fixtures ((alice mock-player :fullp t))
    (setf (tempus::level-of alice) 72)
    (is (tempus::can-admin-group alice "Clan"))))

(deftest can-admin-group/in-admin-group/returns-nil ()
  (with-fixtures ((alice mock-player :fullp t))
    (let ((group (gethash "OLCAdmin" tempus::*access-groups-name*)))
      (push (tempus::idnum-of alice) (tempus::members-of group))
      (is (tempus::can-admin-group alice "OLC"))
      (setf (tempus::members-of group)
            (delete (tempus::idnum-of alice) (tempus::members-of group))))))

(deftest can-admin-group/not-in-admin-group/returns-t ()
  (with-fixtures ((alice mock-player :fullp t))
    (is (not (tempus::can-admin-group alice "OLC")))))

(deftest add-group-member/adds-member-to-group-and-db ()
  (with-fixtures ((alice mock-player :fullp t))
    (let ((group (gethash "OLC" tempus::*access-groups-name*)))
      (is (not (null (tempus::add-group-member group (tempus::name-of alice)))))
      (is (not (zerop (postmodern:query (:select (:count :*)
                                                 :from 'sgroup_members
                                                 :where (:and (:= 'sgroup (tempus::idnum-of group))
                                                              (:= 'player (tempus::idnum-of alice))))
                                        :single))))
      (is (not (null (find (tempus::idnum-of alice) (tempus::members-of group))))))))

(deftest remove-group-member/removes-member-from-group ()
  (with-fixtures ((alice mock-player :fullp t))
    (let ((group (gethash "OLC" tempus::*access-groups-name*)))
      (tempus::add-group-member group (tempus::name-of alice))
      (tempus::remove-group-member group (tempus::name-of alice))
      (is (zerop (postmodern:query (:select (:count :*)
                                            :from 'sgroup_members
                                            :where (:and (:= 'sgroup (tempus::idnum-of group))
                                                         (:= 'player (tempus::idnum-of alice))))
                                   :single)))
      (is (null (find (tempus::idnum-of alice) (tempus::members-of group)))))))

(deftest add-group-command/adds-command-to-group-and-db ()
  (let ((group (gethash "OLC" tempus::*access-groups-name*)))
    (unwind-protect
         (progn
           (is (not (null (tempus::add-group-command group "giggle"))))
           (is (not (zerop (postmodern:query (:select (:count :*)
                                                      :from 'sgroup_commands
                                                      :where (:and (:= 'sgroup (tempus::idnum-of group))
                                                                   (:= 'command "giggle")))
                                             :single))))
           (is (not (null (find "giggle" (tempus::commands-of group) :test #'string=)))))
      (tempus::remove-group-command group "giggle"))))

(deftest remove-group-command/removes-command-from-group ()
    (let ((group (gethash "OLC" tempus::*access-groups-name*)))
      (tempus::add-group-command group "giggle")
      (is (not (null (tempus::remove-group-command group "giggle"))))
      (is (zerop (postmodern:query (:select (:count :*)
                                            :from 'sgroup_commands
                                            :where (:and (:= 'sgroup (tempus::idnum-of group))
                                                         (:= 'command "giggle")))
                                   :single)))
      (is (null (find "giggle" (tempus::commands-of group) :test #'string=)))))

(deftest create-and-destroy-group/creates-and-destroys ()
  (with-fixtures ((alice mock-player :fullp t))
    (let ((new-group nil))
      (unwind-protect
           (progn
             (setf new-group (tempus::create-group "TestGroup"))
             (is (not (zerop (postmodern:query
                              (:select (:count :*) :from 'sgroups :where (:= 'name "TestGroup"))
                              :single))))
             (is (not (null (gethash "TestGroup" tempus::*access-groups-name*))))
             (is (not (null (gethash (tempus::idnum-of new-group) tempus::*access-groups-idnum*))))
             (is (not (null (tempus::add-group-member new-group "Alice"))))
             (is (not (null (tempus::add-group-command new-group "giggle"))))
             (tempus::remove-group "TestGroup")
             (is (zerop (postmodern:query
                         (:select (:count :*) :from 'sgroups :where (:= 'name "TestGroup"))
                         :single)))
             (is (null (gethash "TestGroup" tempus::*access-groups-name*)))
             (is (null (gethash (tempus::idnum-of new-group) tempus::*access-groups-idnum*))))
        (tempus::remove-group "TestGroup")))))

(deftest do-access-addmember/adds-member-and-emits ()
  (with-fixtures ((alice mock-player :fullp t)
                  (bob mock-player :fullp t)
                  (chuck mock-player :fullp t))
    (let ((group (gethash "OLC" tempus::*access-groups-name*)))
      (setf (tempus::level-of alice) 72)
      (function-trace-bind ((calls tempus::add-group-member))
          (tempus::interpret-command alice "access addmember OLC Bob Chuck")
        (is (equal `((,group "Chuck") (,group "Bob")) calls))
        (char-output-is alice "Member added: Bob~%Member added: Chuck~%")))))

(deftest do-access-addmember/cant-admin-group/emits-error ()
  (with-fixtures ((alice mock-player :fullp t)
                  (bob mock-player :fullp t))
    (setf (tempus::level-of alice) 51)
    (function-trace-bind ((calls tempus::add-group-member))
        (tempus::interpret-command alice "access addmember OLC Bob Chuck")
      (is (null calls))
      (char-output-is alice "You cannot add members to this group.~%"))))

(deftest do-access-addmember/already-added/emits-error ()
  (with-fixtures ((alice mock-player :fullp t))
    (let ((group (gethash "OLC" tempus::*access-groups-name*)))
      (setf (tempus::level-of alice) 72)
      (tempus::add-group-member group "Bob")
      (function-trace-bind ((calls tempus::add-group-member))
          (tempus::interpret-command alice "access addmember OLC Bob")
        (is (equal `((,group "Bob")) calls))
        (char-output-is alice "Unable to add member: Bob~%")))))

(deftest do-access-addcmd/adds-command-and-emits ()
  (with-fixtures ((alice mock-player :fullp t))
    (let ((group (gethash "OLC" tempus::*access-groups-name*)))
      (unwind-protect
           (progn
             (setf (tempus::level-of alice) 72)
             (function-trace-bind ((calls tempus::add-group-command))
                 (tempus::interpret-command alice "access addcmd OLC giggle fart")
               (is (equal `((,group "fart") (,group "giggle")) calls))
               (char-output-is alice "Command added: giggle~%Command added: fart~%")))
        (progn
          (tempus::remove-group-command group "giggle")
          (tempus::remove-group-command group "fart"))))))

(deftest do-access-addcmd/cant-admin-group/emits-error ()
  (with-fixtures ((alice mock-player :fullp t))
    (setf (tempus::level-of alice) 51)
    (function-trace-bind ((calls tempus::add-group-command))
        (tempus::interpret-command alice "access addcmd OLC giggle")
      (is (null calls))
      (char-output-is alice "You cannot add commands to this group.~%"))))

(deftest do-access-addcmd/already-added/emits-error ()
  (with-fixtures ((alice mock-player :fullp t))
    (let ((group (gethash "OLC" tempus::*access-groups-name*)))
      (unwind-protect
           (progn
             (setf (tempus::level-of alice) 72)
             (tempus::add-group-command group "giggle")
             (function-trace-bind ((calls tempus::add-group-command))
                 (tempus::interpret-command alice "access addcmd OLC giggle")
               (is (equal `((,group "giggle")) calls))
               (char-output-is alice "Unable to add command: giggle~%")))
        (tempus::remove-group-command group "giggle")))))

(deftest do-access-admin/cant-admin-group/emits-error ()
  (with-fixtures ((alice mock-player :fullp t))
    (setf (tempus::level-of alice) 51)
    (let ((group (gethash "CoderAdmin" tempus::*access-groups-name*)))
      (unwind-protect
           (progn
             (is (null (tempus::admin-group-of group)))
             (tempus::interpret-command alice "access admin CoderAdmin GroupsAdmin")
             (is (null (tempus::admin-group-of group)))
             (char-output-is alice "You cannot alter this group.~%"))
        (setf (tempus::admin-group-of group) nil)))))

(deftest do-access-admin/sets-group-admin ()
  (with-fixtures ((alice mock-player :fullp t))
    (setf (tempus::level-of alice) 72)
    (let ((group (gethash "CoderAdmin" tempus::*access-groups-name*)))
      (unwind-protect
           (progn
             (is (null (tempus::admin-group-of group)))
             (tempus::interpret-command alice "access admin CoderAdmin GroupsAdmin")
             (is (equal "GroupsAdmin" (tempus::admin-group-of group)))
             (char-output-is alice "Administrative group set.~%"))
        (setf (tempus::admin-group-of group) nil)))))

(deftest do-access-admin-none/cant-admin-group/emits-error ()
  (with-fixtures ((alice mock-player :fullp t))
    (setf (tempus::level-of alice) 51)
    (let ((group (gethash "CoderAdmin" tempus::*access-groups-name*)))
      (unwind-protect
           (progn
             (is (null (tempus::admin-group-of group)))
             (setf (tempus::admin-group-of group) "GroupsAdmin")
             (tempus::interpret-command alice "access admin CoderAdmin none")
             (is (equal "GroupsAdmin" (tempus::admin-group-of group)))
             (char-output-is alice "You cannot alter this group.~%"))
        (setf (tempus::admin-group-of group) nil)))))

(deftest do-access-admin-none/sets-group-admin ()
  (with-fixtures ((alice mock-player :fullp t))
    (setf (tempus::level-of alice) 72)
    (let ((group (gethash "CoderAdmin" tempus::*access-groups-name*)))
      (unwind-protect
           (progn
             (is (null (tempus::admin-group-of group)))
             (setf (tempus::admin-group-of group) "GroupsAdmin")
             (tempus::interpret-command alice "access admin CoderAdmin none")
             (is (null (tempus::admin-group-of group)))
             (char-output-is alice "Administrative group unset.~%"))
        (setf (tempus::admin-group-of group) nil)))))

(deftest do-access-cmdlist/lists-commands ()
  (with-fixtures ((alice mock-player))
    (setf (tempus::level-of alice) 51)
    (tempus::interpret-command alice "access cmdlist Coder")
    (char-output-has alice "Commands:~%")
    (char-output-has alice "shutdown")))

(deftest do-access-describe/sets-description ()
  (with-fixtures ((alice mock-player))
    (setf (tempus::level-of alice) 51)
    (setf (override-security-p alice) t)
    (let ((group nil))
      (unwind-protect
           (progn
             (setf group (tempus::create-group "TestGroup"))
             (with-captured-log log
                 (tempus::interpret-command alice "access describe TestGroup Testing")
               (char-output-is alice "Description set.~%")
               (is (equal "Testing" (tempus::description-of group)))
               (is (search "Security: Group 'TestGroup' described by Alice" log))
               (is (not (zerop (postmodern:query
                                (:select (:count :*) :from 'sgroups
                                         :where (:and (:= 'idnum (tempus::idnum-of group))
                                                      (:= 'descrip "Testing")))
                                :single))))))
        (tempus::remove-group "TestGroup")))))

(deftest do-access-grouplist/shows-grouplist ()
  (with-fixtures ((alice mock-player :fullp t))
    (setf (tempus::level-of alice) 51)
    (setf (override-security-p alice) t)
    (let ((group nil))
      (unwind-protect
           (progn
             (setf group (tempus::create-group "TestGroup"))
             (tempus::add-group-member group "Alice")
             (tempus::interpret-command alice "access grouplist alice")
             (char-output-has alice "Alice is a member of the following groups:~%")
             (char-output-has alice "&gTestGroup"))
        (tempus::remove-group "TestGroup")))))

(deftest do-access-list/shows-list-of-groups ()
  (with-fixtures ((alice mock-player))
    (setf (override-security-p alice) t)
    (setf (tempus::level-of alice) 51)
    (let ((group nil))
      (unwind-protect
           (progn
             (setf group (tempus::create-group "TestGroup"))
             (tempus::interpret-command alice "access list")
             (char-output-has alice "&gTestGroup"))
        (tempus::remove-group "TestGroup")))))

(deftest do-access-remmember/removes-member ()
  (with-fixtures ((alice mock-player :fullp t))
    (setf (override-security-p alice) t)
    (setf (tempus::level-of alice) 51)
    (let ((group nil))
      (unwind-protect
           (progn
             (setf group (tempus::create-group "TestGroup"))
             (tempus::add-group-member group "Alice")
             (tempus::interpret-command alice "access remmember TestGroup Alice")
             (char-output-is alice "Member removed: Alice~%")
             (is (null (find (tempus::idnum-of alice) (tempus::members-of group)))))
        (tempus::remove-group "TestGroup")))))

(deftest do-access-remcmd/removes-command ()
  (with-fixtures ((alice mock-player))
    (setf (override-security-p alice) t)
    (setf (tempus::level-of alice) 51)
    (let ((group nil))
      (unwind-protect
           (progn
             (setf group (tempus::create-group "TestGroup"))
             (tempus::add-group-command group "giggle")
             (tempus::interpret-command alice "access remcmd TestGroup giggle")
             (char-output-is alice "Command removed: giggle~%")
             (is (null (find "giggle" (tempus::members-of group) :test #'string=))))
        (tempus::remove-group "TestGroup")))))

(deftest do-access-destroy/destroys-group ()
  (with-fixtures ((alice mock-player))
    (setf (override-security-p alice) t)
    (setf (tempus::level-of alice) 51)
    (let ((group nil))
      (unwind-protect
           (progn
             (setf group (tempus::create-group "TestGroup"))
             (tempus::add-group-command group "giggle")
             (function-trace-bind ((calls tempus::remove-group))
                 (tempus::interpret-command alice "access destroy TestGroup")
               (is (equal `(("TestGroup")) calls))
               (char-output-is alice "Group removed.~%")))
        (tempus::remove-group "TestGroup")))))

(deftest do-access-stat/shows-group-stats ()
  (with-fixtures ((alice mock-player :fullp t))
    (setf (override-security-p alice) t)
    (setf (tempus::level-of alice) 51)
    (let ((group nil))
      (unwind-protect
           (progn
             (setf group (tempus::create-group "TestGroup"))
             (tempus::add-group-member group "Alice")
             (tempus::add-group-command group "giggle")
             (tempus::interpret-command alice "access stat TestGroup")
             (char-output-has alice "TestGroup")
             (char-output-has alice "Admin Group: None~%")
             (char-output-has alice "Alice")
             (char-output-has alice "giggle"))
        (tempus::remove-group "TestGroup")))))
