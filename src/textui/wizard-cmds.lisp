(in-package #:tempus)

(defun send-account-usage (ch)
  (send-to-char ch "Usage: account <command> <args>
Commands:
    movechar <char ID> <to ID>
    exhume <account ID> <char ID>
    reload <account ID>
"))

(defcommand (ch "account") (:immortal)
  (send-account-usage ch))

(defcommand (ch "account" "movechar") (:immortal)
  (send-account-usage ch))

(defcommand (ch "account" "movechar" junk) (:immortal)
  (declare (ignore junk))
  (send-account-usage ch))

(defcommand (ch "account" "movechar" char account) (:immortal)
  (let ((char-id (parse-integer char :junk-allowed t))
        (account-id (parse-integer account :junk-allowed t)))
    (cond
      ((null char-id)
       (send-to-char ch "Invalid character ID.~%"))
      ((null account-id)
       (send-to-char ch "Invalid account ID.~%"))
      ((not (player-idnum-exists char-id))
       (send-to-char ch "That player does not exist.~%"))
      ((null (account-by-idnum account-id))
       (send-to-char ch "That account does not exist.~%"))
      (t
       (let* ((src-account-id (retrieve-player-account char-id))
              (src-account (account-by-idnum src-account-id))
              (dest-account (account-by-idnum account-id))
              (vict (get-char-in-world-by-idnum char-id)))
         (account-move-char src-account char-id dest-account)
         (when (and vict (link-of vict))
           (setf (account-of (link-of vict)) dest-account))
         (send-to-char ch "~a[~d] has been moved from account ~a[~d] to ~a[~d]~%"
                       (retrieve-player-name char-id)
                       char-id
                       (name-of src-account)
                       (idnum-of src-account)
                       (name-of dest-account)
                       (idnum-of dest-account)))))))

(defcommand (ch "account" "exhume") (:immortal)
  (send-account-usage ch))

(defcommand (ch "account" "exhume" junk) (:immortal)
  (declare (ignore junk))
  (send-account-usage ch))

(defcommand (ch "account" "exhume" account char) (:immortal)
  (let ((char-id (parse-integer char :junk-allowed t))
        (account-id (parse-integer account :junk-allowed t)))
    (cond
      ((null char-id)
       (send-to-char ch "Invalid character ID~%"))
      ((null account-id)
       (send-to-char ch "Invalid account ID~%"))
      ((null (account-by-idnum account-id))
       (send-to-char ch "That account does not exist.~%"))
      (t
       (account-exhume-char (account-by-idnum account-id)
                            ch
                            char-id)))))

(defcommand (ch "account" "reload") (:immortal)
  (send-account-usage ch))

(defcommand (ch "account" "reload" junk) (:immortal)
  (declare (ignore junk))
  (send-account-usage ch))

(defcommand (ch "account" "reload" account) (:immortal)
  (let ((account-id (parse-integer account :junk-allowed t)))
    (cond
      ((null account-id)
       (send-to-char ch "Invalid account ID~%"))
      ((null (account-by-idnum account-id))
       (send-to-char ch "That account does not exist.~%"))
      (t
       (account-reload (account-by-idnum account-id))
       (send-to-char ch "Account reloaded.~%")))))

(defcommand (ch "addname") (:immortal)
  (send-to-char ch "Usage: addname <target> <new alias>~%"))

(defcommand (ch "addname" junk) (:immortal)
  (declare (ignore junk))
  (send-to-char ch "Usage: addname <target> <new alias>~%"))

(defcommand (ch "addname" target-str new-alias) (:immortal)
  (let* ((targets (resolve-alias ch target-str
                                (append (carrying-of ch)
                                        (people-of (in-room-of ch))
                                        (contents-of (in-room-of ch)))))
         (target (first targets)))
    (cond
      ((null targets)
       (send-to-char ch "No such object or mobile around.~%"))
      ((rest targets)
       (send-to-char ch "You can only do this to one thing at a time.~%"))
      ((is-pc target)
       (send-to-char ch "You can only do this with NPCs.~%"))
      ((is-alias-of new-alias (aliases-of target) :test #'string=)
       (send-to-char ch "'~a' is already an alias.~%" new-alias))
      (t
       (add-alias target new-alias)
       (send-to-char ch "Okay, you do it.~%")))))