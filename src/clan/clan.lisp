(in-package :tempus)

(defconstant +lvl-can-clan+ 10)
(defconstant +max-clan-members+ 255)
(defconstant +max-clan-badge+ 16)
(defconstant +max-clan-name+ 24)
(defconstant +max-clan-rankname+ 48)
(defconstant +num-clan-ranks+ 11)

(defclass clan-member ()
  ((idnum :accessor idnum-of :initarg :idnum)
   (rank :accessor rank-of :initarg :rank :initform 0)
   (no-mail :accessor no-mail-of :initarg :no-mail :initform nil)))

(defclass clan ()
  ((idnum :accessor idnum-of :initarg :idnum)
   (name :accessor name-of :initarg :name :initform "new clan")
   (badge :accessor badge-of :initarg :badge :initform "-- new clan --")
   (top-rank :accessor top-rank-of :initarg :top-rank :initform 0)
   (rank-names :accessor rank-names-of :initarg :rank-names
               :initform (make-array 11 :initial-element "the member"))
   (owner :accessor owner-of :initarg :owner :initform 0)
   (members :accessor members-of :initform nil)
   (rooms :accessor rooms-of :initform nil)
   (bank :accessor bank-of :initarg :bank :initform 0)))

(defvar *clans* (make-hash-table))

(defun real-clan (clan-id)
  (gethash clan-id *clans*))

(defun resolve-clan-alias (alias)
  (if (every #'digit-char-p alias)
      (real-clan (parse-integer alias))
      (find alias (sort (hash-values *clans*) #'< :key 'idnum-of)
            :test #'string-abbrev
            :key 'name-of)))

(defun add-clan-member (ch clan)
  (setf (clan-of ch) (idnum-of clan))
  (setf (plr-bits-of ch) (logandc2 (plr-bits-of ch) +plr-clan-leader+))
  (push (make-instance 'clan-member :idnum (idnum-of ch))
        (members-of clan))
  (execute (:insert-into 'clan_members :set
                         'clan (clan-of ch)
                         'player (idnum-of ch)
                         'rank 0
                         'no_mail nil)))

(defun remove-clan-member (ch clan)
  (setf (clan-of ch) 0)
  (setf (plr-bits-of ch) (logandc2 (plr-bits-of ch) +plr-clan-leader+))
  (setf (members-of clan) (delete (idnum-of ch) (members-of clan)
                                  :key 'idnum-of))
  (when (eql (owner-of clan) (idnum-of ch))
    (setf (owner-of clan) 0))
  (execute (:delete-from 'clan_members
                         :where (:= 'player (idnum-of ch)))))

(defun add-clan-room (room clan)
  (setf (flags-of room) (logior (flags-of room) +room-clan-house+))
  (push room (rooms-of clan))
  (execute (:insert-into 'clan_rooms :set
                         'clan (idnum-of clan)
                         'room (number-of room))))

(defun remove-clan-room (room clan)
  (setf (flags-of room) (logandc2 (flags-of room) +room-clan-house+))
  (setf (rooms-of clan) (delete room (rooms-of clan)))
  (execute (:delete-from 'clan_rooms :where (:= 'room (number-of room)))))

(defun send-to-clan (clan-id fmt &rest args)
  (let ((msg (format nil "~?" fmt args)))
    (dolist (cxn *cxns*)
      (when (and (typep cxn 'tempus-cxn)
                 (actor-of cxn)
                 (in-room-of (actor-of cxn))
                 (= (clan-of (actor-of cxn)) clan-id))
        (cxn-write cxn "&c~a&n~%" msg)))))

(defun char-can-enroll (ch target clan)
  (cond
    ((is-npc target)
     (send-to-char ch "You can only enroll player characters.~%")
     nil)
    ((aff-flagged ch +aff-charm+)
     (send-to-char ch "You obviously aren't in your right mind.~%")
     nil)
    ((eql (idnum-of clan) (clan-of target))
     (send-to-char ch "That person is already in the clan.~%")
     nil)
    ((not (zerop (clan-of target)))
     (send-to-char ch "You cannot while they are a member of another clan.~%")
     nil)
    ((security-is-member ch "Clan")
     t)
    ((plr-flagged target +plr-frozen+)
     (send-to-char ch "They are frozen right now.  Wait until a god has mercy.~%")
     nil)
    ((< (level-of target) +lvl-can-clan+)
     (send-to-char ch "Players must be level ~d before being inducted into the clan.~%"
                   +lvl-can-clan+)
     nil)
    ((>= (length (members-of clan)) +max-clan-members+)
     (send-to-char ch "The max number of members has been reached for this clan.~%")
     nil)
    ((eql (owner-of clan) (idnum-of ch))
     t)
    ((plr-flagged ch +plr-clan-leader+)
     t)
    (t
     (send-to-char ch "You are not a leader of the clan!~%")
     nil)))

(defun char-can-dismiss (ch target clan)
  (let ((ch-member (find (idnum-of ch) (members-of clan) :key 'idnum-of))
        (target-member (find (idnum-of target) (members-of clan)
                             :key 'idnum-of)))
  (cond
    ((eql target ch)
     (send-to-char ch "Try resigning if you want to leave the clan.~%")
     nil)
    ((aff-flagged ch +aff-charm+)
     (send-to-char ch "You obviously aren't quite in your right mind.~%")
     nil)
    ((security-is-member ch "Clan")
     t)
    ;; Dismissal conditions that don't apply to clan administrators
    ((zerop (clan-of ch))
     (send-to-char ch "Try joining a clan first.~%")
     nil)
    ((/= (clan-of ch) (clan-of target))
     (send-to-char ch "Umm, why don't you check the clan list, okay?~%")
     nil)
    ((eql (owner-of clan) (idnum-of ch))
     t)
    ;; Dismissal conditions that don't apply to clan owners
    ((or (/= (clan-of ch) (idnum-of clan))
         (not (plr-flagged ch +plr-clan-leader+)))
     (send-to-char ch "You are not a leader of the clan!~%")
     nil)
    ((<= (rank-of ch-member) (rank-of target-member))
     (send-to-char ch "You don't have the rank for that.~%")
     nil)
    ((plr-flagged target +plr-clan-leader+)
     (send-to-char ch "You cannot dismiss co-leaders.~%")
     nil)
    (t
     t))))

(defun char-can-promote (ch target clan)
  (let ((ch-member (find (idnum-of ch) (members-of clan) :key 'idnum-of))
        (target-member (find (idnum-of target) (members-of clan)
                             :key 'idnum-of)))
  (cond
    ((aff-flagged ch +aff-charm+)
     (send-to-char ch "You obviously aren't quite in your right mind.~%")
     nil)
    ((null target-member)
     (send-to-char ch "You are not a member of that person's clan!~%")
     nil)
    ((and (>= (rank-of target-member) (top-rank-of clan))
          (plr-flagged target +plr-clan-leader+))
     (send-to-char ch "That person is already at the top rank.~%")
     nil)
    ((security-is-member ch "Clan")
     t)
    ;; Promotion conditions that don't apply to clan administrators
    ((zerop (clan-of ch))
     (send-to-char ch "Try joining a clan first.~%")
     nil)
    ((not (eql (real-clan (clan-of target)) clan))
     (send-to-char ch "You are not a member of that person's clan!~%")
     nil)
    ((eql (owner-of clan) (idnum-of ch))
     t)
    ;; Promotion conditions that don't apply to clan owners
    ((eql ch target)
     (send-to-char ch "Promote yourself?  Haha.~%")
     nil)
    ((<= (rank-of ch-member) (rank-of target-member))
     (send-to-char ch "You don't have the rank for that.~%")
     nil)
    (t
     t))))

(defun char-can-demote (ch target clan)
  (let ((target-member (find (idnum-of target) (members-of clan)
                             :key 'idnum-of)))
  (cond
    ((aff-flagged ch +aff-charm+)
     (send-to-char ch "You obviously aren't quite in your right mind.~%")
     nil)
    ((null target-member)
     (send-to-char ch "You are not a member of that person's clan!~%")
     nil)
    ((zerop (rank-of target-member))
     (send-to-char ch "~:[That person is~;You are~] already at the bottom rank.~%"
                   (eql ch target))
     nil)
    ((security-is-member ch "Clan")
     t)
    ;; Promotion conditions that don't apply to clan administrators
    ((zerop (clan-of ch))
     (send-to-char ch "Try joining a clan first.~%")
     nil)
    ((not (eql (real-clan (clan-of target)) clan))
     (send-to-char ch "You are not a member of that person's clan!~%")
     nil)
    ((eql (owner-of clan) (idnum-of ch))
     t)
    ;; Promotion conditions that don't apply to clan owners
    ((not (plr-flagged ch +plr-clan-leader+))
     (send-to-char ch "You are unable to demote.~%")
     nil)
    (t
     t))))

(defun perform-clanlist (ch clan completep)
  (with-pagination ((link-of ch))
    (send-to-char ch "Members of clan ~a :~%" (name-of clan))
    (dolist (member (sort (members-of clan)
                          (lambda (a b)
                            (< (rank-of b) (rank-of a)))))
      (let ((player (or (gethash (idnum-of member) *character-map*)
                        (and completep
                             (load-player-from-xml (idnum-of member))))))
        (when player
        (cond
          ((immortal-level-p player)
           (send-to-char ch "&Y[&G~7<~;~:@(~a~)~;~>&Y]&g " (badge-of player)))
          ((testerp player)
           (send-to-char ch "&Y[&GTESTING&Y]&g "))
          ((immortalp ch)
           (send-to-char ch "&g[~:[&r~;&n~]~2d&c(&n~2d&c) ~a~a&g] &n"
                         (pref-flagged player +pref-anonymous+)
                         (level-of player)
                         (remort-gen-of player)
                         (char-class-color player (char-class-of player))
                         (char-class-name (char-class-of player))))
          (t
           (send-to-char ch "&g[&n~2d ~a~a&g] &n"
                         (level-of player)
                         (char-class-color player (char-class-of player))
                         (char-class-name (char-class-of player)))))

        (if (plr-flagged player +plr-clan-leader+)
            (send-to-char ch "&c~a ~a~@[~*~40t- &c~a~]&n~%"
                          (name-of player)
                          (aref (rank-names-of clan) (rank-of member))
                          (in-room-of player)
                          (and (in-room-of player)
                               (name-of (zone-of (in-room-of player)))))
            (send-to-char ch "&g~a ~a~@[~*~40t- &c~a~]&n~%"
                          (name-of player)
                          (aref (rank-names-of clan) (rank-of member))
                          (in-room-of player)
                          (and (in-room-of player)
                               (name-of (zone-of (in-room-of player)))))))))))

(defun boot-clans ()
  (slog "Reading clans")
  (clrhash *clans*)
  (dolist (clan-record (query (:select 'idnum
                                       'name
                                       'badge
                                       'bank
                                       'owner
                                       :from 'clans)
                              :rows))
    (let ((new-clan (make-instance 'clan
                     :idnum (first clan-record)
                     :name (second clan-record)
                     :badge (third clan-record)
                     :bank (fourth clan-record)
                     :owner (unless (eql :null (fifth clan-record))
                              (fifth clan-record)))))
      (setf (gethash (idnum-of new-clan) *clans*) new-clan)))

  ;; Add the ranks to the clan
  (dolist (rank-record (query (:select 'clan 'rank 'title :from 'clan_ranks)))
    (let ((clan (real-clan (first rank-record)))
          (rank (second rank-record)))
      (setf (top-rank-of clan) (max (top-rank-of clan) rank))
      (setf (aref (rank-names-of clan) rank) (third rank-record))))

  ;; Now add all the members to the clans
  (dolist (member-record (query (:order-by
                                 (:select 'clan 'player 'rank 'no_mail
                                          :from 'clan_members)
                                 'rank)))
    (let ((clan (real-clan (first member-record)))
          (player (second member-record))
          (rank (third member-record))
          (no-mail (string-equal "T" (fourth member-record))))
      (push (make-instance 'clan-member
                           :idnum player
                           :rank rank
                           :no-mail no-mail)
            (members-of clan))))

  ;; Add the rooms to the clans
  (dolist (room-record (query (:select 'clan 'room :from 'clan_rooms)))
    (let ((clan (real-clan (first room-record)))
          (room (real-room (second room-record))))
      (when room
        (push room (rooms-of clan)))))

  (slog "Clan boot successful"))

(defun create-clan (idnum)
  (assert (null (gethash idnum *clans*)) nil
          "Attempt to create clan with existing id")
  (let ((clan (make-instance 'clan :idnum idnum)))
    (setf (gethash idnum *clans*) clan)
    (execute (:insert-into 'clans :set
                           'idnum idnum
                           'name (name-of clan)
                           'badge (badge-of clan)
                           'bank 0))
    clan))

(defun delete-clan (idnum)
  (assert (gethash idnum *clans*) nil
          "Attempt to delete non-existing clan")
  (let ((clan (real-clan idnum)))
    (execute (:delete-from 'clan_rooms :where (:= 'clan idnum)))
    (execute (:delete-from 'clan_members :where (:= 'clan idnum)))
    (execute (:delete-from 'clan_ranks :where (:= 'clan idnum)))
    (execute (:delete-from 'clans :where (:= 'idnum idnum)))

    (dolist (member (members-of clan))
      (let ((ch (or (gethash (idnum-of member) *character-map*)
                    (ignore-errors (load-player-from-xml (idnum-of member))))))
        (when ch
          (setf (clan-of ch) 0)
          (save-player-to-xml ch)
          (when (in-room-of ch)
            (send-to-char ch "&cYour clan has been disbanded!&n~%")))))

    (remhash idnum *clans*)
    t))

(defun perform-show-clan (ch clan)
  (with-pagination ((link-of ch))
    (send-to-char ch "CLAN ~d - Name: &c~a&n, Badge: &c~a&n, Top Rank: ~d~%"
                  (idnum-of clan)
                  (name-of clan)
                  (badge-of clan)
                  (top-rank-of clan))
    (send-to-char ch "Bank: ~20d " (bank-of clan))
    (when (owner-of clan)
      (send-to-char ch "Owner: ~a[~d]"
                    (retrieve-player-name (owner-of clan))
                    (owner-of clan)))
    (send-to-char ch "~%")
    (dotimes (rank (top-rank-of clan))
      (send-to-char ch "Rank ~2d: &y~a&n~%" rank (aref (rank-names-of clan) rank)))

    (send-to-char ch "ROOMS:~%")

    (if (rooms-of clan)
        (loop
           for room in (rooms-of clan)
           as room-num from 1
           do (send-to-char ch "~3d) ~5d.  &c~a&n~%"
                            (number-of room)
                            (title-of room)))
        (send-to-char ch "None.~%"))

    (cond
      ((members-of clan)
       (send-to-char ch "~d MEMBERS:~%" (length (members-of clan)))
       (dolist (member (members-of clan))
         (let ((account-id (retrieve-player-account (idnum-of member))))
           (send-to-char ch "~5d &y~a&n ~a(~d) ~50t~a[~d]~%"
                         (idnum-of member)
                         (retrieve-player-name (idnum-of member))
                         (aref (rank-names-of clan) (rank-of member))
                         (rank-of member)
                         (retrieve-account-name account-id)
                         account-id))))
      (t
       (send-to-char ch "No members.~%")))))

(defun perform-list-clans (ch clan)
  (with-pagination ((link-of ch))
    (send-to-char ch "CLANS:~%")
    (send-to-char ch " ~3d - &c~20a&n  &c~20a&n  (~3d members)~%"
                  (idnum-of clan)
                  (name-of clan)
                  (badge-of clan)
                  (length (members-of clan)))))

(defun clan-house-can-enter (ch room)
  (or (not (room-flagged room +room-clan-house+))
      (security-is-member ch "Clan")
      (and (not (zerop (clan-of ch)))
           (real-clan (clan-of ch))
           (find room (rooms-of (real-clan (clan-of ch)))))))

(defun char-receives-clanmail (ch)
  (let* ((clan (real-clan (clan-of ch)))
         (member (and clan (find (idnum-of ch) (members-of clan)
                                 :key 'idnum-of))))
    (and member
         (not (no-mail-of member)))))

(defcommand (ch "enroll") ()
  (send-to-char ch "You must specify the player to enroll.~%"))

(defcommand (ch "enroll" target-str) ()
  (let* ((targets (resolve-alias ch target-str (people-of (in-room-of ch))))
         (target (first targets)))
    (cond
      ((null target)
       (send-to-char ch "You don't see that person.~%"))
      ((rest targets)
       (send-to-char ch "You can only enroll one person at a time.~%"))
      ((char-can-enroll ch target (real-clan (clan-of ch)))
       (let ((clan (real-clan (clan-of ch))))
         (send-to-char target "&cYou have been inducted into the clan ~a by ~a!&n~%"
                       (name-of clan)
                       (name-of ch))
         (mudlog 'notice t "~a has been inducted into clan ~a by ~a"
                 (name-of target)
                 (name-of clan)
                 (name-of ch))
         (send-to-clan (clan-of ch) "~a has been inducted into the clan by ~a!"
                       (name-of target)
                       (name-of ch))
         (add-clan-member ch clan))))))

(defcommand (ch "enroll" target-str clan-str) ()
  (let* ((targets (resolve-alias ch target-str (people-of (in-room-of ch))))
         (target (first targets))
         (clan (find clan-str (hash-values *clans*)
                     :key 'name-of
                     :test 'string-equal)))
    (cond
      ((null target)
       (send-to-char ch "You don't see that person.~%"))
      ((rest targets)
       (send-to-char ch "You can only enroll one person at a time.~%"))
      ((null clan)
       (send-to-char ch "'~a' isn't the name of a clan.~%" clan-str))
      ((char-can-enroll ch target clan)
       (send-to-char target "&cYou have been inducted into the clan ~a by ~a!&n~%"
                     (name-of clan)
                     (name-of ch))
       (mudlog 'notice t "~a has been inducted into clan ~a by ~a"
               (name-of target)
               (name-of clan)
               (name-of ch))
       (send-to-clan (idnum-of clan)
                     "~a has been inducted into the clan by ~a!"
                     (name-of target)
                     (name-of ch))
         (add-clan-member ch clan)))))

(defcommand (ch "dismiss" target-str) ()
  (let* ((targets (or (resolve-alias ch target-str (people-of (in-room-of ch)))
                      (resolve-alias ch target-str (remove-if 'is-npc
                                                              *characters*))))
         (target (or (first targets)
                     (load-player-from-xml (retrieve-player-idnum target-str)))))
    (cond
      ((zerop (clan-of ch))
       (send-to-char ch "Hmm... You need to be in a clan yourself, first.~%"))
      ((null target)
       (send-to-char ch "You don't see that person.~%"))
      ((rest targets)
       (send-to-char ch "You can only dismiss one person at a time.~%"))
      ((char-can-dismiss ch target (real-clan (clan-of ch)))
       (let ((clan (real-clan (clan-of ch))))
         (when (in-room-of target)
           (send-to-char target
                         "&cYou have been dismissed from the clan by ~a!&n~%"
                         (name-of ch)))
         (mudlog 'notice t "~a has been dismissed from clan ~a by ~a"
                 (name-of target)
                 (name-of clan)
                 (name-of ch))
         (send-to-clan (clan-of ch) "~a has been dismissed from the clan by ~a!"
                       (name-of target)
                       (name-of ch))
         (remove-clan-member target clan))))))

(defcommand (ch "resign") ()
  (send-to-char ch "You must type 'resign yes' to leave your clan.~%"))

(defcommand (ch "resign" "yes") ()
  (cond
    ((is-npc ch)
     (send-to-char ch "NPCs cannot resign...~%"))
    ((zerop (clan-of ch))
     (send-to-char ch "You need to be in a clan before you can resign from it.~%"))
    ((aff-flagged ch +aff-charm+)
     (send-to-char ch "You obviously aren't quite in your right mind.~%"))
    (t
     (let ((clan (real-clan (clan-of ch))))
       (send-to-char ch "You have resigned from clan ~a.~%" (name-of clan))
       (remove-clan-member ch clan)
       (send-to-clan (clan-of ch) "~a has resigned from the clan."
                     (name-of ch))
       (mudlog 'notice t "~a has resigned from clan ~a"
               (name-of ch)
               (name-of clan))
       (save-player-to-xml ch)))))

(defcommand (ch "promote") ()
  (send-to-char ch "You must specify the person to promote.~%"))

(defcommand (ch "promote" target-str) ()
  (let* ((targets (resolve-alias ch target-str (people-of (in-room-of ch))))
         (target (first targets))
         (clan (real-clan (clan-of ch)))
         (target-member (and target
                             clan
                             (find (idnum-of target) (members-of clan)
                                   :key 'idnum-of))))
    (cond
      ((zerop (clan-of ch))
       (send-to-char ch "Hmm... You need to be in a clan yourself, first.~%"))
      ((null target)
       (send-to-char ch "You don't see that person.~%"))
      ((rest targets)
       (send-to-char ch "You can only promote one person at a time.~%"))
      ((not (char-can-promote ch target clan))
       nil)
      ((>= (rank-of target-member) (top-rank-of clan))
       ;; promotion to clan leader
       (setf (plr-bits-of target) (logior (plr-bits-of target) +plr-clan-leader+))
       (send-to-clan (clan-of target) "~a has promoted ~a to clan leader status."
                     (name-of ch)
                     (name-of target))
       (mudlog 'notice t "~a has promoted ~a to clan leader status"
               (name-of ch)
               (name-of target))
       (save-player-to-xml target))
      (t
       ;; Normal rank promotion
       (send-to-clan (clan-of ch) "~a has promoted ~a to the rank of ~a!"
               (name-of ch)
               (if (eql ch target)
                   (format nil "~aself" (him-or-her ch))
                   (name-of target))
               (aref (rank-names-of clan) (rank-of target-member)))
       (mudlog 'notice t "~a has promoted ~a to the rank of ~a (~d)"
               (name-of ch)
               (if (eql ch target)
                   (format nil "~aself" (him-or-her ch))
                   (name-of target))
               (aref (rank-names-of clan) (rank-of target-member))
               (rank-of target-member))

       (incf (rank-of target-member))
       (execute (:update 'clan_members :set 'rank (rank-of target-member)
                         :where (:= 'player (idnum-of target))))))))

(defcommand (ch "demote") ()
  (send-to-char ch "You must specify the person to demote.~%"))

(defcommand (ch "demote" target-str) ()
  (let* ((targets (resolve-alias ch target-str (people-of (in-room-of ch))))
         (target (first targets))
         (clan (real-clan (clan-of ch)))
         (target-member (and target
                             clan
                             (find (idnum-of target) (members-of clan)
                                   :key 'idnum-of))))
    (cond
      ((zerop (clan-of ch))
       (send-to-char ch "Hmm... You need to be in a clan yourself, first.~%"))
      ((null target)
       (send-to-char ch "You don't see that person.~%"))
      ((rest targets)
       (send-to-char ch "You can only demote one person at a time.~%"))
      ((char-can-demote ch target clan)
       ;; Normal rank demotion
       (send-to-clan (clan-of ch) "~a has demoted ~a to the rank of ~a!"
               (name-of ch)
               (if (eql ch target)
                   (format nil "~aself" (him-or-her ch))
                   (name-of target))
               (aref (rank-names-of clan) (rank-of target-member)))
       (mudlog 'notice t "~a has demoted ~a to the rank of ~a (~d)"
               (name-of ch)
               (if (eql ch target)
                   (format nil "~aself" (him-or-her ch))
                   (name-of target))
               (aref (rank-names-of clan) (rank-of target-member))
               (rank-of target-member))

       (decf (rank-of target-member))
       (execute (:update 'clan_members :set 'rank (rank-of target-member)
                         :where (:= 'player (idnum-of target))))))))

(defcommand (ch "clanmail") ()
  (let* ((clan (real-clan (clan-of ch)))
         (member (and clan (find (idnum-of ch) (members-of clan)
                                 :key 'idnum-of))))
    (cond
      ((null clan)
       (send-to-char ch "You aren't even in a clan.~%"))
      (t
       (setf (no-mail-of member) (not (no-mail-of member)))
       (if (no-mail-of member)
           (send-to-char ch "You are not receiving any mail addressed to your clan.~%")
           (send-to-char ch "You now receive clan mailings.~%"))
       (execute (:update 'clan_members :set 'no_mail (no-mail-of member)
                         :where (:= 'player (idnum-of ch))))))))

(defcommand (ch "clanlist") ()
  (let ((clan (real-clan (clan-of ch))))
    (cond
      ((null clan)
       (send-to-char ch "You are not a member of any clan.~%"))
      (t
       (perform-clanlist ch clan nil)))))

(defcommand (ch "clanlist" "all") ()
  (let ((clan (real-clan (clan-of ch))))
    (cond
      ((null clan)
       (send-to-char ch "You are not a member of any clan.~%"))
      (t
       (perform-clanlist ch clan t)))))

(defcommand (ch "claninfo") ()
  (let ((clan (real-clan (clan-of ch))))
    (cond
      ((null clan)
       (send-to-char ch "You are not a member of any clan.~%"))
      (t
       (with-pagination ((link-of ch))
         (send-to-char ch "Information on clan &c~a&n:~%~%" (name-of clan))
         (send-to-char ch "Clan badge: '&c~a&n', Clan headcount: ~d, Clan bank account: ~d~%"
                       (badge-of clan)
                       (length (members-of clan))
                       (bank-of clan))
         (send-to-char ch "Clan ranks:~%")
         (loop for i from (top-rank-of clan) downto 0 do
              (send-to-char ch " (~2d)  &y~a&n~%"
                            i
                            (aref (rank-names-of clan) i)))
         (when (rooms-of clan)
           (send-to-char ch "Clan rooms:~%")
           (dolist (room (rooms-of clan))
             (send-to-char ch "&c~a&n~%" (name-of room)))))))))

(defcommand (ch "cedit") (:immortal)
  (send-to-char ch "Usage: cedit (create|delete|set|add|remove) [args]~%"))

(defcommand (ch "cedit" "create") (:immortal)
  (send-to-char ch "Create a clan with what vnum?~%"))

(defcommand (ch "cedit" "create" vnum-str) (:immortal)
  (let ((vnum (parse-integer vnum-str :junk-allowed t)))
    (cond
      ((null vnum)
       (send-to-char ch "You must specify the clan number numerically.~%"))
      ((real-clan vnum)
       (send-to-char ch "A clan already exists with that vnum.~%"))
      ((not (create-clan vnum))
       (send-to-char ch "There was an error creating the clan.~%"))
      (t
       (send-to-char ch "Clan #~d created.~%" vnum)))))

(defcommand (ch "cedit" "delete") (:immortal)
  (send-to-char ch "Delete what clan?~%"))

(defcommand (ch "cedit" "delete" clan-str) (:immortal)
  (let* ((clan (resolve-clan-alias clan-str)))
    (cond
      ((null clan)
       (send-to-char ch "Clan '~a' does not exist.~%" clan-str))
      (t
       (delete-clan (idnum-of clan))
       (send-to-char ch "Clan deleted.  Sucked anyway.~%")))))

(defcommand (ch "cedit" "set") (:immortal)
  (send-to-char ch "Usage: cedit set <clan> (name|badge|rank|bank|member|owner|toprank) <value>~%"))

(defcommand (ch "cedit" "set" clan-str) (:immortal)
  (declare (ignore clan-str))
  (send-to-char ch "Usage: cedit set <clan> (name|badge|rank|bank|member|owner|toprank) <value>~%"))

(defcommand (ch "cedit" "set" clan-str "name") (:immortal)
  (declare (ignore clan-str))
  (send-to-char ch "Set the name of the clan to what?~%"))

(defcommand (ch "cedit" "set" clan-str "name" new-name) (:immortal)
  (let* ((clan (resolve-clan-alias clan-str)))
    (cond
      ((null clan)
       (send-to-char ch "Clan '~a' does not exist.~%" clan-str))
      ((> (length new-name) +max-clan-name+)
       (send-to-char ch "Clan names can't be more than ~d characters.~%" +max-clan-name+))
      (t
       (let ((old-name (name-of clan)))
         (setf (name-of clan) new-name)
         (execute (:update 'clans :set 'name new-name :where (:= 'idnum (idnum-of clan))))
         (send-to-char ch "You got it.~%")
         (slog "(cedit) ~a set clan ~a[~d] name to '~a'"
               (name-of ch)
               old-name
               (idnum-of clan)
               new-name))))))

(defcommand (ch "cedit" "set" clan-str "badge") (:immortal)
  (declare (ignore clan-str))
  (send-to-char ch "Set the badge of the clan to what?~%"))

(defcommand (ch "cedit" "set" clan-str "badge" new-badge) (:immortal)
  (let* ((clan (resolve-clan-alias clan-str)))
    (cond
      ((null clan)
       (send-to-char ch "Clan '~a' does not exist.~%" clan-str))
      ((> (length new-badge) +max-clan-badge+)
       (send-to-char ch "Clan badges can't be more than ~d characters.~%" +max-clan-badge+))
      (t
       (setf (badge-of clan) new-badge)
       (execute (:update 'clans :set 'badge new-badge :where (:= 'idnum (idnum-of clan))))
       (send-to-char ch "You got it.~%")
       (slog "(cedit) ~a set clan ~a[~d] badge to '~a'"
             (name-of ch)
             (name-of clan)
             (idnum-of clan)
             new-badge)))))

(defcommand (ch "cedit" "set" clan-str "toprank") (:immortal)
  (declare (ignore clan-str))
  (send-to-char ch "Set the top rank of the clan to what?~%"))

(defcommand (ch "cedit" "set" clan-str "toprank" new-top-rank) (:immortal)
  (let* ((clan (resolve-clan-alias clan-str))
         (rank (parse-integer new-top-rank :junk-allowed t)))
    (cond
      ((null clan)
       (send-to-char ch "Clan '~a' does not exist.~%" clan-str))
      ((or (null rank)
           (not (< 0 rank +num-clan-ranks+)))
       (send-to-char ch "The new top rank must be a number between 0 and ~a.~%" +num-clan-ranks+))
      (t
       (setf (top-rank-of clan) rank)
       (execute (:update 'clan_members :set 'rank rank :where (:and (:= 'clan (idnum-of clan))
                                                                    (:> 'rank (top-rank-of clan)))))
       (execute (:delete-from 'clan_ranks :where (:and (:= 'clan (idnum-of clan))
                                                       (:> 'rank (top-rank-of clan)))))
       (send-to-char ch "You got it.~%")
       (slog "(cedit) ~a set clan ~a[~d] top rank to '~a'"
             (name-of ch)
             (name-of clan)
             (idnum-of clan)
             rank)))))

(defcommand (ch "cedit" "set" clan-str "bank") (:immortal)
  (declare (ignore clan-str))
  (send-to-char ch "Set the bank of the clan to what?~%"))

(defcommand (ch "cedit" "set" clan-str "bank" new-bank) (:immortal)
  (let* ((clan (resolve-clan-alias clan-str))
         (bank (parse-integer new-bank :junk-allowed t)))
    (cond
      ((null clan)
       (send-to-char ch "Clan '~a' does not exist.~%" clan-str))
      ((or (null bank)
           (minusp bank))
       (send-to-char ch "The bank account must be a positive number.~%"))
      (t
       (setf (bank-of clan) bank)
       (execute (:update 'clan :set 'bank bank :where (:= 'clan (idnum-of clan))))
       (send-to-char ch "You got it.~%")
       (slog "(cedit) ~a set clan ~a[~d] bank to '~a'"
             (name-of ch)
             (name-of clan)
             (idnum-of clan)
             bank)))))

(defcommand (ch "cedit" "set" clan-str "owner") (:immortal)
  (declare (ignore clan-str))
  (send-to-char ch "Set the owner of the clan to whom?~%"))

(defcommand (ch "cedit" "set" clan-str "owner" owner-str) (:immortal)
  (let* ((clan (resolve-clan-alias clan-str))
         (owner-id (or (parse-integer owner-str :junk-allowed t)
                       (retrieve-player-idnum owner-str))))
    (cond
      ((null clan)
       (send-to-char ch "Clan '~a' does not exist.~%" clan-str))
      ((null owner-id)
       (send-to-char ch "There is no such player as '~a'~%" owner-str))
      (t
       (setf (owner-of clan) owner-id)
       (execute (:update 'clan :set 'owner owner-id :where (:= 'clan (idnum-of clan))))
       (send-to-char ch "You got it.~%")
       (slog "(cedit) ~a set clan ~a[~d] owner to ~a[~d]"
             (name-of ch)
             (name-of clan)
             (idnum-of clan)
             (retrieve-player-name owner-id)
             owner-id)))))

(defcommand (ch "cedit" "set" clan-str "member") (:immortal)
  (declare (ignore clan-str))
  (send-to-char ch "Set the rank of what clanmember?~%"))

(defcommand (ch "cedit" "set" clan-str "member" ignore) (:immortal)
  (declare (ignore clan-str ignore))
  (send-to-char ch "Set the rank of the clanmember to what?~%"))

(defcommand (ch "cedit" "set" clan-str "member" member-str new-rank) (:immortal)
  (let* ((clan (resolve-clan-alias clan-str))
         (member-id (or (parse-integer member-str :junk-allowed t)
                        (retrieve-player-idnum member-str)))
         (member-rec (and clan member-id (find member-id (members-of clan)
                                               :key 'idnum-of)))
         (rank (parse-integer new-rank :junk-allowed t)))
    (cond
      ((null clan)
       (send-to-char ch "Clan '~a' does not exist.~%" clan-str))
      ((null member-id)
       (send-to-char ch "There is no such player as '~a'~%" member-str))
      ((null member-rec)
       (send-to-char ch "That player is not in that clan.~%"))
      ((or (null rank)
           (not (< 0 rank (top-rank-of clan))))
       (send-to-char ch "The clan rank must be a number from 0 to ~d.~%" (top-rank-of clan)))
      (t
       (setf (rank-of member-rec) rank)
       (execute (:update 'clan_members :set 'rank rank :where (:= 'player member-id)))
       (send-to-char ch "You got it.~%")
       (slog "(cedit) ~a set clan ~a[~d] member ~a[~d] to rank ~d"
             (name-of ch)
             (name-of clan)
             (idnum-of clan)
             (retrieve-player-name member-id)
             member-id
             rank)))))

(defcommand (ch "cedit" "show" clan-str) (:immortal)
  (let ((clan (resolve-clan-alias clan-str)))
    (if clan
        (perform-show-clan ch clan)
        (send-to-char ch "Clan '~a' does not exist.~%" clan-str))))

(defcommand (ch "cedit" "add" clan-str "room") (:immortal)
  (declare (ignore clan-str))
  (send-to-char ch "Add what room to the clan?~%"))

(defcommand (ch "cedit" "add" clan-str "room" room-str) (:immortal)
  (let* ((clan (resolve-clan-alias clan-str))
         (room-num (parse-integer room-str :junk-allowed t)))
    (cond
      ((null clan)
       (send-to-char ch "Clan '~a' does not exist.~%" clan-str))
      ((null room-num)
       (send-to-char ch "You must specify a valid room number.~%"))
      ((null (real-room room-num))
       (send-to-char ch "There is no such room ~d.~%" room-num))
      (t
       (add-clan-room room clan)
       (send-to-char ch "You got it.~%")
       (slog "(cedit) ~a added room ~a[~d] to clan ~a[~d]"
             (name-of ch)
             (name-of (real-room room-num))
             room-num
             (name-of clan)
             (idnum-of clan))))))

(defcommand (ch "cedit" "remove" clan-str "room") (:immortal)
  (declare (ignore clan-str))
  (send-to-char ch "Remove which room from the clan?~%"))

(defcommand (ch "cedit" "remove" "room" clan-str room-str) (:immortal)
  (let* ((clan (resolve-clan-alias clan-str))
         (room-num (parse-integer room-str :junk-allowed t)))
    (cond
      ((null clan)
       (send-to-char ch "Clan '~a' does not exist.~%" clan-str))
      ((null room-num)
       (send-to-char ch "You must specify a valid room number.~%"))
      ((null (find room-num (rooms-of clan)))
       (send-to-char ch "Room ~d does not belong to that clan.~%" room-num))
      (t
       (remove-clan-room (real-room room-num) clan)
       (send-to-char ch "You got it.~%")
       (slog "(cedit) ~a removed room ~a[~d] from clan ~a[~d]"
             (name-of ch)
             (name-of (real-room room-num))
             room-num
             (name-of clan)
             (idnum-of clan))))))
