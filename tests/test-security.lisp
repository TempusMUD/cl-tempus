(in-package #:tempus.tests)

(in-suite (defsuite (tempus.security :in test-full)))

(deftest can-admin-group/is-level-72/returns-t ()
  (with-full-mock-players (alice)
    (setf (tempus::level-of alice) 72)
    (is (tempus::can-admin-group alice "Clan"))))

(deftest can-admin-group/in-admin-group/returns-nil ()
  (with-full-mock-players (alice)
    (let ((group (gethash "OLCAdmin" tempus::*access-groups-name*)))
      (push (tempus::idnum-of alice) (tempus::members-of group))
      (is (tempus::can-admin-group alice "OLC"))
      (setf (tempus::members-of group)
            (delete (tempus::idnum-of alice) (tempus::members-of group))))))

(deftest can-admin-group/not-in-admin-group/returns-t ()
  (with-full-mock-players (alice)
    (is (not (tempus::can-admin-group alice "OLC")))))

(deftest add-group-member/normal/adds-member-to-group-and-db ()
  (with-full-mock-players (alice)
    (let ((group (gethash "OLC" tempus::*access-groups-name*)))
      (is (not (null (tempus::add-group-member group (tempus::name-of alice)))))
      (is (not (zerop (postmodern:query (:select (:count :*)
                                                 :from 'sgroup_members
                                                 :where (:and (:= 'sgroup (tempus::idnum-of group))
                                                              (:= 'player (tempus::idnum-of alice))))
                                        :single))))
      (is (not (null (find (tempus::idnum-of alice) (tempus::members-of group))))))))

(deftest remove-group-member/normal/removes-member-from-group ()
  (with-full-mock-players (alice)
    (let ((group (gethash "OLC" tempus::*access-groups-name*)))
      (tempus::add-group-member group (tempus::name-of alice))
      (tempus::remove-group-member group (tempus::name-of alice))
      (is (zerop (postmodern:query (:select (:count :*)
                                            :from 'sgroup_members
                                            :where (:and (:= 'sgroup (tempus::idnum-of group))
                                                         (:= 'player (tempus::idnum-of alice))))
                                   :single)))
      (is (null (find (tempus::idnum-of alice) (tempus::members-of group)))))))

(deftest add-group-command/normal/adds-command-to-group-and-db ()
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

(deftest remove-group-command/normal/removes-command-from-group ()
    (let ((group (gethash "OLC" tempus::*access-groups-name*)))
      (tempus::add-group-command group "giggle")
      (is (not (null (tempus::remove-group-command group "giggle"))))
      (is (zerop (postmodern:query (:select (:count :*)
                                            :from 'sgroup_commands
                                            :where (:and (:= 'sgroup (tempus::idnum-of group))
                                                         (:= 'command "giggle")))
                                   :single)))
      (is (null (find "giggle" (tempus::commands-of group) :test #'string=)))))

(deftest do-access-addmember/normal/adds-member-and-emits ()
  (with-full-mock-players (alice bob chuck)
    (let ((group (gethash "OLC" tempus::*access-groups-name*)))
      (setf (tempus::level-of alice) 72)
      (function-trace-bind ((calls tempus::add-group-member))
          (tempus::interpret-command alice "access addmember OLC Bob Chuck")
        (is (equal `((,group "Chuck") (,group "Bob")) calls))
        (char-output-is alice "Member added: Bob~%Member added: Chuck~%")))))

(deftest do-access-addmember/cant-admin-group/emits-error ()
  (with-full-mock-players (alice bob)
    (function-trace-bind ((calls tempus::add-group-member))
        (tempus::interpret-command alice "access addmember OLC Bob Chuck")
      (is (null calls))
      (char-output-is alice "You cannot add members to this group.~%"))))

(deftest do-access-addmember/already-added/emits-error ()
  (with-full-mock-players (alice)
    (let ((group (gethash "OLC" tempus::*access-groups-name*)))
      (setf (tempus::level-of alice) 72)
      (tempus::add-group-member group "Bob")
      (function-trace-bind ((calls tempus::add-group-member))
          (tempus::interpret-command alice "access addmember OLC Bob")
        (is (equal `((,group "Bob")) calls))
        (char-output-is alice "Unable to add member: Bob~%")))))

(deftest do-access-addcmd/normal/adds-command-and-emits ()
  (with-full-mock-players (alice)
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
  (with-full-mock-players (alice)
    (function-trace-bind ((calls tempus::add-group-command))
        (tempus::interpret-command alice "access addcmd OLC giggle")
      (is (null calls))
      (char-output-is alice "You cannot add commands to this group.~%"))))

(deftest do-access-addcmd/already-added/emits-error ()
  (with-full-mock-players (alice)
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
  (with-full-mock-players (alice)
    (let ((group (gethash "CoderAdmin" tempus::*access-groups-name*)))
      (unwind-protect
           (progn
             (is (null (tempus::admin-group-of group)))
             (tempus::interpret-command alice "access admin CoderAdmin GroupsAdmin")
             (is (null (tempus::admin-group-of group)))
             (char-output-is alice "You cannot alter this group.~%"))
        (setf (tempus::admin-group-of group) nil)))))

(deftest do-access-admin/normal/sets-group-admin ()
  (with-full-mock-players (alice)
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
  (with-full-mock-players (alice)
    (let ((group (gethash "CoderAdmin" tempus::*access-groups-name*)))
      (unwind-protect
           (progn
             (is (null (tempus::admin-group-of group)))
             (setf (tempus::admin-group-of group) "GroupsAdmin")
             (tempus::interpret-command alice "access admin CoderAdmin none")
             (is (equal "GroupsAdmin" (tempus::admin-group-of group)))
             (char-output-is alice "You cannot alter this group.~%"))
        (setf (tempus::admin-group-of group) nil)))))

(deftest do-access-admin-none/normal/sets-group-admin ()
  (with-full-mock-players (alice)
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