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
  (clrhash *command-access-group*)
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