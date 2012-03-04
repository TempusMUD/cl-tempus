(in-package :tempus)

(defvar *access-groups-idnum* (make-hash-table))
(defvar *access-groups-name* (make-hash-table :test 'equal))
(defvar *command-access-groups* (make-hash-table :test 'equal))

(defclass access-group ()
  ((idnum :accessor idnum-of :initarg :idnum)
   (name :accessor name-of :initarg :name)
   (admin-group :accessor admin-group-of :initarg :admin-group)
   (description :accessor description-of :initarg :description)
   (commands :accessor commands-of :initform nil)
   (members :accessor members-of :initform nil)))

(defmethod print-object ((group access-group) stream)
  (print-unreadable-object (group stream :type t)
    (format stream "~a ~s" (idnum-of group) (name-of group))))

(defun security-load-groups ()
  (clrhash *access-groups-idnum*)
  (clrhash *access-groups-name*)
  (clrhash *command-access-groups*)
  (dolist (tuple (postmodern:query (:select 'sgroups.idnum 'sgroups.name 'sgroups.descrip
                                 'admin.name
                                 :from 'sgroups :outer-join (:as 'sgroups 'admiN)
                                 :on (:= 'admin.idnum 'sgroups.admin))
                        :rows))
    (let ((new-group (make-instance 'access-group
                                    :idnum (first tuple)
                                    :name (second tuple)
                                    :description (third tuple)
                                    :admin-group (when (not (eql (fourth tuple) :null))
                                                   (fourth tuple)))))
      (setf (gethash (first tuple) *access-groups-idnum*) new-group)
      (setf (gethash (second tuple) *access-groups-name*) new-group)))

  (dolist (tuple (postmodern:query (:select 'sgroup 'command :from 'sgroup_commands) :rows))
    (let ((group (gethash (first tuple) *access-groups-idnum*)))
      (if group
          (progn
            (push group (gethash (second tuple) *command-access-groups* nil))
            (push (second tuple) (commands-of group)))
          (slog "WARNING: group #~a not found while loading commands." (first tuple)))))

  (dolist (tuple (postmodern:query (:select 'sgroup 'player :from 'sgroup_members) :rows))
    (let ((group (gethash (first tuple) *access-groups-idnum*)))
      (if group
          (push (second tuple) (members-of group))
          (slog "WARNING: group #~a not found while loading members." (first tuple)))))

  (slog "Security:  Access group data loaded."))

(defmethod security-is-member ((ch mobile) group-name)
  (declare (ignore ch group-name))
  nil)

(defmethod security-is-member ((ch player) group-name)
  (let ((group (gethash group-name *access-groups-name*)))
    (or (= (level-of ch) 72)
        (and group
             (find (idnum-of ch) (members-of group))))))

(defmethod security-is-really-member ((ch mobile) group-name)
  (declare (ignore ch group-name))
  nil)

(defmethod security-is-really-member ((ch player) group-name)
  (let ((group (gethash group-name *access-groups-name*)))
    (and group
         (find (idnum-of ch) (members-of group)))))

(defgeneric authorized? (ch op &key &allow-other-keys))

(defmethod authorized? :around (ch op &key)
  (or (= (level-of ch) +lvl-grimp+)
      (call-next-method)))

(defmethod authorized? (ch (op (eql 'enter-houses)) &key)
  (or (security-is-really-member ch "House")
      (security-is-really-member ch "AdminBasic")
      (security-is-really-member ch "WizardFull")))

(defmethod authorized? (ch (op (eql 'edit-house)) &key house)
  (or (security-is-really-member ch "House")
      (and house
           (eql (owner-idnum-of house)
                (idnum-of (account-of ch))))))

(defmethod authorized? (ch op &key)
  nil)

(defun can-admin-group (ch group-name)
  (let ((group (gethash group-name *access-groups-name*)))
    (and group
         (or
          (and (admin-group-of group)
               (security-is-member ch (admin-group-of group)))
          (security-is-member ch "GroupsAdmin")))))

(defun add-group-member (group player-name)
  (let ((player-id (retrieve-player-idnum player-name)))
    (when (and player-id
               (not (find player-id (members-of group))))
      (postmodern:execute (:insert-into 'sgroup_members :set
                             'sgroup (idnum-of group)
                             'player player-id))
      (setf (members-of group) (sort (cons player-id (members-of group)) #'<))
      t)))

(defun remove-group-member (group player-name)
  (let ((player-id (retrieve-player-idnum player-name)))
    (when (and player-id (find player-id (members-of group)))
      (postmodern:execute (:delete-from 'sgroup_members :where
                             (:and (:= 'sgroup (idnum-of group))
                                   (:= 'player player-id))))
      (setf (members-of group) (delete player-id (members-of group)))
      t)))

(defun add-group-command (group command)
  (unless (find command (commands-of group) :test #'string=)
    (postmodern:execute (:insert-into 'sgroup_commands :set
                           'sgroup (idnum-of group)
                           'command command))
    (setf (commands-of group) (sort (cons command (commands-of group)) #'string<))
    t))

(defun remove-group-command (group command)
  (when (find command (commands-of group) :test #'string=)
    (postmodern:execute (:delete-from 'sgroup_commands :where
                           (:and (:= 'sgroup (idnum-of group))
                                 (:= 'command command))))
    (setf (commands-of group) (delete command (commands-of group) :test #'string=))
    t))

(defun create-group (name)
  (unless (gethash name *access-groups-name*)
    (let ((new-idnum (1+ (postmodern:query (:select (:max 'idnum) :from 'sgroups) :single))))
      (let ((new-group (make-instance 'access-group
                                      :idnum new-idnum
                                      :name name
                                      :description "No description"
                                      :admin-group nil)))
        (postmodern:execute (:insert-into 'sgroups :set
                               'idnum new-idnum
                               'name name
                               'descrip "No description"))
        (setf (gethash new-idnum *access-groups-idnum*) new-group)
        (setf (gethash name *access-groups-name*) new-group)
        new-group))))

(defun remove-group (name)
  (let ((group (gethash name *access-groups-name*)))
    (when group
      (postmodern:execute (:update 'sgroups :set 'admin :null :where (:= 'admin (idnum-of group))))
      (postmodern:execute (:delete-from 'sgroup_members :where (:= 'sgroup (idnum-of group))))
      (postmodern:execute (:delete-from 'sgroup_commands :where (:= 'sgroup (idnum-of group))))
      (postmodern:execute (:delete-from 'sgroups :where (:= 'idnum (idnum-of group))))

      (remhash name *access-groups-name*)
      (remhash (idnum-of group) *access-groups-idnum*)
      (dolist (command (commands-of group))
        (setf (gethash command *command-access-groups*)
              (delete name (gethash command *command-access-groups*) :test #'string=)))
      t)))

(defcommand (ch "access" "addmember") (:immortal)
  (send-to-char ch "Add member to what group?~%"))

(defcommand (ch "access" "addmember" group-name) (:immortal)
  (declare (ignore group-name))
  (send-to-char ch "Add what member?~%"))

(defcommand (ch "access" "addmember" group-name members) (:immortal)
  (let ((group (gethash group-name *access-groups-name*)))
    (cond
      ((null group)
       (send-to-char ch "That's not a valid group.~%"))
      ((not (can-admin-group ch group-name))
       (send-to-char ch "You cannot add members to this group.~%"))
      (t
       (dolist (name (cl-ppcre:split "\\s+" members))
         (if (add-group-member group name)
             (send-to-char ch "Member added: ~a~%" name)
             (send-to-char ch "Unable to add member: ~a~%" name)))))))

(defcommand (ch "access" "addcmd" group-name cmd-names) (:immortal)
  (let ((group (gethash group-name *access-groups-name*)))
    (cond
      ((null group)
       (send-to-char ch "That's not a valid group.~%"))
      ((not (can-admin-group ch group-name))
       (send-to-char ch "You cannot add commands to this group.~%"))
      (t
       (dolist (command (cl-ppcre:split "\\s+" cmd-names))
         (if (add-group-command group command)
             (send-to-char ch "Command added: ~a~%" command)
             (send-to-char ch "Unable to add command: ~a~%" command)))))))

(defcommand (ch "access" "admin") (:immortal)
  (send-to-char ch "Set admin group of what group?~%"))

(defcommand (ch "access" "admin" group-name) (:immortal)
  (declare (ignore group-name))
  (send-to-char ch "Set admin group to what?~%"))

(defcommand (ch "access" "admin" group-name "none") (:immortal)
  (let ((group (gethash group-name *access-groups-name*)))
    (cond
      ((null group)
       (send-to-char ch "'~a' is not a valid group.~%" group-name))
      ((not (can-admin-group ch group-name))
       (send-to-char ch "You cannot alter this group.~%"))
      (t
       (postmodern:execute (:update 'sgroups :set 'admin :null :where (:= 'idnum (idnum-of group))))
       (setf (admin-group-of group) nil)
       (send-to-char ch "Administrative group unset.~%")))))

(defcommand (ch "access" "admin" group-name admin-name) (:immortal)
  (let ((group (gethash group-name *access-groups-name*))
        (admin-group (gethash admin-name *access-groups-name*)))
    (cond
      ((null group)
       (send-to-char ch "'~a' is not a valid group.~%" group-name))
      ((not (can-admin-group ch group-name))
       (send-to-char ch "You cannot alter this group.~%"))
      ((null admin-group)
       (send-to-char ch "'~a' is not a valid group.~%" admin-name))
      (t
       (postmodern:execute (:update 'sgroups
                         :set 'admin (idnum-of admin-group)
                         :where (:= 'idnum (idnum-of group))))
       (setf (admin-group-of group) admin-name)
       (send-to-char ch "Administrative group set.~%")))))

(defcommand (ch "access" "cmdlist") (:immortal)
  (send-to-char ch "List commands for what group?~%"))

(defcommand (ch "access" "cmdlist" group-name) (:immortal)
  (let ((group (gethash group-name *access-groups-name*)))
    (cond
      ((null group)
       (send-to-char ch "'~a' is not a valid group.~%" group-name))
      (t
       (with-pagination ((link-of ch))
         (send-to-char ch "Commands:~%&g~a&n~%"
                       (print-columns-to-string 3 15 (commands-of group))))))))

(defcommand (ch "access" "create") (:immortal)
  (send-to-char ch "What group do you wish to create?~%"))

(defcommand (ch "access" "create" group-names) (:immortal)
  (cond
    ((not (security-is-member ch "GroupsAdmin"))
     (send-to-char ch "You cannot create groups.~%"))
    (t
     (dolist (name (cl-ppcre:split "\\s+" group-names))
       (if (create-group name)
           (send-to-char ch "Group created: ~a~%" name)
           (send-to-char ch "Unable to create group: ~a~%" name))))))

(defcommand (ch "access" "describe") (:immortal)
  (send-to-char ch "Describe what group?~%"))

(defcommand (ch "access" "describe" group-name) (:immortal)
  (declare (ignore group-name))
  (send-to-char ch "You need to specify a description.~%"))

(defcommand (ch "access" "describe" group-name description) (:immortal)
  (let ((group (gethash group-name *access-groups-name*)))
    (cond
      ((null group)
       (send-to-char ch "'~a' is not a valid group.~%" group-name))
      (t
       (setf (description-of group) description)
       (postmodern:execute (:update 'sgroups :set 'descrip description :where (:= 'idnum (idnum-of group))))
       (send-to-char ch "Description set.~%")
       (slog "Security: Group '~a' described by ~a." (name-of group) (name-of ch))))))

(defun print-group-ldesc (ch group)
  (send-to-char ch "&g~16a &c[&n~3d&c] [&n~3d&c]&n - ~a~%"
                (name-of group)
                (length (commands-of group))
                (length (members-of group))
                (description-of group)))

(defcommand (ch "access" "grouplist" player-name) (:immortal)
  (let* ((player-id (retrieve-player-idnum player-name))
         (groups (delete-if-not #'(lambda (group)
                                   (find player-id (members-of group)))
                                (hash-values *access-groups-name*))))
    (cond
      (groups
       (with-pagination ((link-of ch))
         (send-to-char ch "~a is a member of the following groups:~%"
                       (retrieve-player-name player-id))
         (dolist (group groups)
           (print-group-ldesc ch group)))))))


(defcommand (ch "access" "list") (:immortal)
  (with-pagination ((link-of ch))
    (send-to-char ch "&g          Group &c[&ncmds&c] [&nmbrs&c]&n - Description~%")
    (dolist (group (sort (hash-values *access-groups-name*) #'string< :key 'name-of))
      (print-group-ldesc ch group))))

(defcommand (ch "access" "reload") (:immortal)
  (security-load-groups)
  (send-to-char ch "Reloaded."))

(defcommand (ch "access" "memberlist") (:immortal)
  (send-to-char ch "Which group's member list do you wish to view?~%"))

(defcommand (ch "access" "memberlist" group-name) (:immortal)
  (let ((group (gethash group-name *access-groups-name*)))
    (cond
      ((null group)
       (send-to-char ch "'~a' is not a valid group.~%" group-name))
      (t
       (let ((player-names (mapcar 'retrieve-player-name (members-of group))))
         (with-pagination ((link-of ch))
           (send-to-char ch "Members:~%&g~a&n~%"
                         (print-columns-to-string 3 15 player-names))))))))

(defcommand (ch "access" "remmember" group-name member-names) (:immortal)
  (let ((group (gethash group-name *access-groups-name*)))
    (cond
      ((null group)
       (send-to-char ch "That's not a valid group.~%"))
      ((not (can-admin-group ch group-name))
       (send-to-char ch "You cannot remove members from this group.~%"))
      (t
       (dolist (member-name (cl-ppcre:split "\\s+" member-names))
         (if (remove-group-member group member-name)
             (send-to-char ch "Member removed: ~a~%" member-name)
             (send-to-char ch "Unable to remove member: ~a~%" member-name)))))))

(defcommand (ch "access" "remcmd" group-name cmd-names) (:immortal)
  (let ((group (gethash group-name *access-groups-name*)))
    (cond
      ((null group)
       (send-to-char ch "That's not a valid group.~%"))
      ((not (can-admin-group ch group-name))
       (send-to-char ch "You cannot remove commands from this group.~%"))
      (t
       (dolist (cmd-name (cl-ppcre:split "\\s+" cmd-names))
         (if (remove-group-command group cmd-name)
             (send-to-char ch "Command removed: ~a~%" cmd-name)
             (send-to-char ch "Unable to remove command: ~a~%" cmd-name)))))))

(defcommand (ch "access" "destroy") (:immortal)
  (send-to-char ch "What group do you wish to destroy?~%"))

(defcommand (ch "access" "destroy" group-name) (:immortal)
  (cond
    ((null (gethash group-name *access-groups-name*))
     (send-to-char ch "That's not a valid group.~%"))
    ((not (security-is-member ch "GroupsAdmin"))
     (send-to-char ch "You cannot remove groups.~%"))
    (t
     (remove-group group-name)
     (send-to-char ch "Group removed.~%"))))

(defcommand (ch "access" "stat") (:immortal)
  (send-to-char ch "What group do you wish to stat?~%"))

(defcommand (ch "access" "stat" group-name) (:immortal)
  (let ((group (gethash group-name *access-groups-name*)))
    (cond
      ((null group)
       (send-to-char ch "That's not a valid group.~%"))
      (t
       (let ((player-names (mapcar 'retrieve-player-name (members-of group))))
         (with-pagination ((link-of ch))
           (send-to-char ch "Name: &g~a &c[&n~3d&c][&n~3d&c]&n~%"
                         (name-of group)
                         (length (commands-of group))
                         (length (members-of group)))
         (send-to-char ch "Admin Group: ~:[None~;&g~a&n~]~%" (admin-group-of group))
         (send-to-char ch "Description: ~a~%" (description-of group))
         (send-to-char ch "Commands:~%&g~a&n~%"
                       (print-columns-to-string 3 15 (commands-of group)))
         (send-to-char ch "Members:~%&g~a&n~%"
                       (print-columns-to-string 3 15 player-names))))))))

(defcommand (ch "access") (:immortal)
  (send-to-char ch "addmember       <group name> <member> [<member>...]
  addcmd          <group name> <command> [<command>...]
  admin           <group name> <admin group>
  cmdlist         <group name>
  create          <group name>
  describe        <group name> <description>
  grouplist       <player name>
  lpist
  load
  memberlist      <group name>
  remmember       <group name> <member> [<member>...]
  remcmd          <group name> <command> [<command>...]
  destroy         <group name>
  stat            <group name>
"))