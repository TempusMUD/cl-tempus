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
  (dolist (tuple (query (:select 'sgroups.idnum 'sgroups.name 'sgroups.descrip
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

  (dolist (tuple (query (:select 'sgroup 'command :from 'sgroup_commands) :rows))
    (let ((group (gethash (first tuple) *access-groups-idnum*)))
      (if group
          (progn
            (push group (gethash (second tuple) *command-access-groups* nil))
            (push (second tuple) (commands-of group)))
          (slog "WARNING: group #~a not found while loading commands." (first tuple)))))

  (dolist (tuple (query (:select 'sgroup 'player :from 'sgroup_members) :rows))
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
    (assert group nil
            "Unknown group ~s passed to security-is-member"
            group-name)
    (or (= (level-of ch) 72)
        (find (idnum-of ch) (members-of group)))))

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
      (execute (:insert-into 'sgroup_members :set
                             'sgroup (idnum-of group)
                             'player player-id))
      (setf (members-of group) (sort (cons player-id (members-of group)) #'<))
      t)))

(defun remove-group-member (group player-name)
  (let ((player-id (retrieve-player-idnum player-name)))
    (when (and player-id (find player-id (members-of group)))
      (execute (:delete-from 'sgroup_members :where
                             (:and (:= 'sgroup (idnum-of group))
                                   (:= 'player player-id))))
      (setf (members-of group) (delete player-id (members-of group)))
      t)))

(defun add-group-command (group command)
  (unless (find command (commands-of group) :test #'string=)
    (execute (:insert-into 'sgroup_commands :set
                           'sgroup (idnum-of group)
                           'command command))
    (setf (commands-of group) (sort (cons command (commands-of group)) #'string<))
    t))

(defun remove-group-command (group command)
  (when (find command (commands-of group) :test #'string=)
    (execute (:delete-from 'sgroup_commands :where
                           (:and (:= 'sgroup (idnum-of group))
                                 (:= 'command command))))
    (setf (commands-of group) (delete command (commands-of group) :test #'string=))
    t))

(defcommand (ch "access" "addmember") ()
  (send-to-char ch "Add member to what group?~%"))

(defcommand (ch "access" "addmember" group-name) ()
  (declare (ignore group-name))
  (send-to-char ch "Add what member?~%"))

(defcommand (ch "access" "addmember" group-name members) ()
  (let ((group (gethash group-name *access-groups-name*)))
    (cond
      ((null group)
       (send-to-char ch "That's not a valid group.~%"))
      ((not (can-admin-group ch group-name))
       (send-to-char ch "You cannot add members to this group.~%"))
      (t
       (dolist (name (split-sequence #\space members :remove-empty-subseqs t))
         (if (add-group-member group name)
             (send-to-char ch "Member added: ~a~%" name)
             (send-to-char ch "Unable to add member: ~a~%" name)))))))

(defcommand (ch "access" "addcmd" group-name cmd-names) ()
  (let ((group (gethash group-name *access-groups-name*)))
    (cond
      ((null group)
       (send-to-char ch "That's not a valid group.~%"))
      ((not (can-admin-group ch group-name))
       (send-to-char ch "You cannot add commands to this group.~%"))
      (t
       (dolist (command (split-sequence #\space cmd-names :remove-empty-subseqs t))
         (if (add-group-command group command)
             (send-to-char ch "Command added: ~a~%" command)
             (send-to-char ch "Unable to add command: ~a~%" command)))))))

(defcommand (ch "access" "admin") ()
  (send-to-char ch "Set admin group of what group?~%"))

(defcommand (ch "access" "admin" group-name) ()
  (declare (ignore group-name))
  (send-to-char ch "Set admin group to what?~%"))

(defcommand (ch "access" "admin" group-name "none") ()
  (let ((group (gethash group-name *access-groups-name*)))
    (cond
      ((null group)
       (send-to-char ch "'~a' is not a valid group.~%" group-name))
      ((not (can-admin-group ch group-name))
       (send-to-char ch "You cannot alter this group.~%"))
      (t
       (execute (:update 'sgroups :set 'admin :null :where (:= 'idnum (idnum-of group))))
       (setf (admin-group-of group) nil)
       (send-to-char ch "Administrative group unset.~%")))))

(defcommand (ch "access" "admin" group-name admin-name) ()
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
       (execute (:update 'sgroups
                         :set 'admin (idnum-of admin-group)
                         :where (:= 'idnum (idnum-of group))))
       (setf (admin-group-of group) admin-name)
       (send-to-char ch "Administrative group set.~%")))))
