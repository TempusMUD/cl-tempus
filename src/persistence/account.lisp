(in-package :tempus)

(defclass player-record ()
  ((idnum :accessor idnum-of :initarg :idnum)
   (account :accessor account-of :initarg :account)
   (name :accessor name-of :initarg :name)))

(defclass account ()
  ((idnum :accessor idnum-of :initarg :idnum)
   (name :accessor name-of :initarg :name)
   (password :reader password-of :initarg :password)
   (email :accessor email-of :initarg :email)
   (creation-time :accessor creation-time-of :initarg :creation-time)
   (creation-addr :accessor creation-addr-of :initarg :creation-addr)
   (login-time :accessor login-time-of :initarg :login-time)
   (login-addr :accessor login-addr-of :initarg :login-addr)
   (entry-time :accessor entry-time-of :initarg :entry-time)
   (ansi-level :accessor ansi-level-of :initarg :ansi-level :initform 0)
   (compact-level :accessor compact-level-of :initform 0)
   (term-height :accessor term-height-of :initarg :term-height :initform 22)
   (term-width :accessor term-width-of :initarg :term-width :initform 80)
   (trust :accessor trust-of :initarg :trust :initform 0)
   (reputation :accessor reputation-of :initarg :reputation :initform 0)
   (quest-points :accessor quest-points-of :initarg :quest-points :initform 0)
   (banned :accessor banned-of :initarg :banned :initform nil)
   (quest-banned :accessor quest-banned-of :initarg :quest-banned :initform nil)
   (bank-past :accessor past-bank-of :initform 0)
   (bank-future :accessor future-bank-of :initform 0)
   (metric-units :accessor metric-units-of :initform nil)
   (players :accessor players-of :initform nil))
  (:default-initargs :password "" :email ""
                                :creation-time (now)
                                :creation-addr ""
                                :login-time (now)
                                :login-addr ""
                                :entry-time (now)))


(defvar *account-max-idnum* 0)
(defvar *account-idnum-cache* (make-hash-table))
(defvar *account-name-cache* (make-hash-table :test 'equal))

(defun random-salt ()
  "Returns a string of the format $1$xxxxxxxx$ where x is a random
alphanumeric character.  This is for use in making crypt() return an
md5 hash."
  (let ((salt-table
         "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz./"))
    (format nil "$1$~{~a~}$"
            (loop repeat 8
                  collect
                  (char salt-table (random 64))))))

(defun (setf password-of) (password account)
  "This is a slot writer for account password which automatically hashes
the password."
  (setf (slot-value account 'password)
        (crypt-password password (random-salt))))

(defun account-boot ()
  (clrhash *account-idnum-cache*)
  (clrhash *account-name-cache*)
  (slog "Getting max account idnum")
  (setf *account-max-idnum* (max-account-id))

  (slog "Getting character count")
  (let ((player-count (postmodern:query (:select (:count '*) :from 'players) :single)))
    (if (zerop player-count)
        (slog "WARNING: No characters loaded")
        (slog "... ~d character~p in db" player-count player-count))))

(defun max-account-id ()
  "Returns the maximum account id in the database"
  (let ((result (postmodern:query (:select (:max 'idnum) :from 'accounts) :single)))
    (if (eql result :null)
        0
        result)))

(defun max-player-id ()
  "Returns the maximum player id in the database"
  (let ((result (postmodern:query (:select (:max 'idnum) :from 'players) :single)))
    (if (eql result :null)
        0
        result)))

(defun account-exists (name)
  "Returns true if the account with the given name exists.  The comparison
is case-insensitive."
  (= (postmodern:query (:select (:count '*)
             :from 'accounts
             :where (:= (:lower 'name) (string-downcase name)))
            :single)
     1))

(defun create-account (name)
  (let ((new-account (make-instance 'account
                                    :idnum (1+ (max-account-id))
                                    :name name)))
    (postmodern:execute (:insert-into 'accounts :set
                           'idnum (idnum-of new-account)))
    (save-account new-account)
    new-account))

(defun account-by-idnum (idnum)
  "Returns the account associated with the given id. The account may
be loaded from the database or it may be retrieved from a cache."
  (let* ((cached (gethash idnum *account-idnum-cache* nil)))
    (if cached
        cached
        (let ((result (postmodern:query (:select '*
                                      :from 'accounts
                                      :where (:= 'idnum idnum))
                             :alist))
              (account (make-instance 'account)))
          (when result
            (loop
               with tempus-pkg = (find-package :tempus)
               for tuple in result
               unless (eql (cdr tuple) :null)
               do (setf (slot-value account (intern (symbol-name (car tuple))
                                                    tempus-pkg))
                        (cdr tuple)))
            (setf (gethash (name-of account) *account-name-cache*) account)
            (setf (gethash (idnum-of account) *account-idnum-cache*) account)
            (setf (players-of account)
                  (mapcar (lambda (info)
                            (make-instance 'player-record
                                           :idnum (cdr (assoc :idnum info))
                                           :account (idnum-of account)
                                           :name (cdr (assoc :name info))))
                          (postmodern:query (:order-by (:select 'idnum 'name :from 'players :where (:= 'account (idnum-of account))) 'idnum) :alists)))
            account)))))

(defun load-account (name)
  "Returns the account associated with the given name. The account may
be loaded from the database or it may be retrieved from a cache."
  (let* ((canonical-name (string-downcase name))
         (cached (gethash canonical-name *account-name-cache* nil)))
    (if cached
        cached
        (let ((result (postmodern:query (:select '*
                                      :from 'accounts
                                      :where (:= (:lower 'name) canonical-name))
                             :alist))
              (account (make-instance 'account)))
          (when result
            (loop
               with tempus-pkg = (find-package :tempus)
               for tuple in result
               unless (eql (cdr tuple) :null)
               do (setf (slot-value account (intern (symbol-name (car tuple))
                                                    tempus-pkg))
                        (cdr tuple)))
            (setf (gethash canonical-name *account-name-cache*) account)
            (setf (gethash (idnum-of account) *account-idnum-cache*) account)
            (setf (players-of account)
                  (mapcar (lambda (info)
                            (make-instance 'player-record
                                           :idnum (cdr (assoc :idnum info))
                                           :account (idnum-of account)
                                           :name (cdr (assoc :name info))))
                          (postmodern:query (:order-by (:select 'idnum 'name :from 'players :where (:= 'account (idnum-of account))) 'idnum) :alists)))
            account)))))

(defmethod save-account ((account account))
  "Saves the account information into the database."
  (postmodern:execute (:update 'accounts :set
            'name (name-of account)
            'password (password-of account)
            'email (email-of account)
            'login-time (login-time-of account)
            'login-addr (login-addr-of account)
            'entry-time (entry-time-of account)
            'ansi-level (ansi-level-of account)
            'compact-level (compact-level-of account)
            'term-height (term-height-of account)
            'term-width (term-width-of account)
            'bank-past (past-bank-of account)
            'bank-future (future-bank-of account)
            :where (:= 'idnum (idnum-of account)))))

(defun account-login (account cxn)
  "Performs necessary tasks for an account upon a successful login."
  (setf (login-time-of account) (now))
  (setf (login-addr-of account) (peer-addr-of cxn))
  (save-account account)
  (syslog "~a logged in" (name-of account)))

(defun account-logout (account cxn)
  (setf (login-time-of account) (now))
  (setf (login-addr-of account) (peer-addr-of cxn))
  (save-account account)
  (syslog "~a logged out" (name-of account)))

;;; Support for encrypted password transmission
(cffi:define-foreign-library libcrypt
    (:unix (:default "libcrypt")))

(cffi:use-foreign-library libcrypt)

(defun crypt-password (password salt)
  "Encrypt a password with a hash"
  (cffi:foreign-funcall "crypt"
                        :string password
                        :string salt
                        :string))

(defun check-password (account password)
  "Determines if the hash of the given password matches the stored hash."
  (string= (password-of account) (crypt-password password (password-of account))))

(defun validate-name (name)
  "Returns T if the given string is ok to use as a player name.  Returns
NIL if it is invalid in some way."
  (and (not (string= name ""))
       (every (lambda (c) (or (eql c #\') (alpha-char-p c))) name)))

(defun player-name-exists (name)
  "Returns T if a player exists with the given name."
  (plusp (postmodern:query (:select (:count '*)
                 :from 'players
                 :where (:= (:lower 'name) (string-downcase name)))
                :single)))

(defun retrieve-player-idnum (name)
  "Retrieves the idnum of the player with the given name from the database.
Returns NIL if the player did not exist."
  (postmodern:query (:select 'idnum
                  :from 'players
                  :where (:= (:lower 'name) (string-downcase name)))
         :single))

(defun retrieve-player-name (idnum)
  "Retrieves the name of the player with the given idnum from the database.
Returns NIL if the player did not exist."
  (postmodern:query (:select 'name
                  :from 'players
                  :where (:= 'idnum idnum))
         :single))

(defun retrieve-player-account (idnum)
  "Retrieves the account id of the player with the given idnum from the database.
Returns NIL if the player did not exist."
  (postmodern:query (:select 'account :from 'players :where (:= 'idnum idnum))
         :single))

(defun retrieve-account-name (idnum)
  "Retrieves the name of the account with the given idnum from the database
or the cache.  Returns NIL if the account does not exist."
  (let ((account (gethash idnum *account-idnum-cache*)))
    (if account
        (name-of account)
        (postmodern:query (:select 'name :from 'accounts :where (:= 'idnum idnum))
               :single))))

(defun create-new-player (actor account)
  "Creates a new player from the given actor.  The player is stored into
the account database, while the actor is stored in the proper player
file."
  (let* ((now (now))
         (player-record (make-instance 'player-record
                                       :idnum (idnum-of actor)
                                       :account (idnum-of account)
                                       :name (name-of actor))))
    (postmodern:execute (:insert-into 'players :set
                           'idnum (idnum-of actor)
                           'account (idnum-of account)
                           'name (name-of actor)))
    (setf (players-of account)
          (nconc (players-of account) (list player-record)))
    (save-account account)
    (setf (title-of actor) "the utter newbie")
    (setf (birth-time-of actor) now)
    (setf (login-time-of actor) now)
    (setf (rentcode-of actor) 'creating)
    (save-player-to-xml actor)))

(defun delete-player (ch)
  ;; Clear the owner of any clans this player might own in memory
  (dolist (clan-id (hash-keys *clans*))
    (let ((clan (real-clan clan-id)))
      (when (eql (owner-of clan) (idnum-of ch))
        (setf (owner-of clan) 0))))

  ;; Clear the owner of any clans this player might own on the db
  (postmodern:execute (:update 'clans
                    :set 'owner :null
                    :where (:= 'owner (idnum-of ch))))

  ;; Remove character from clan
  (let ((clan (real-clan (clan-of ch))))
    (when clan
      (setf (members-of clan) (delete (idnum-of ch) (members-of clan)))))
  (postmodern:execute (:delete-from 'clan_members :where (:= 'player (idnum-of ch))))

  ;; Remove character from any access groups
  (dolist (group (hash-values *access-groups-idnum*))
    (setf (members-of group) (delete (idnum-of ch) (members-of group))))
  (postmodern:execute (:delete-from 'sgroup_members :where (:= 'player (idnum-of ch))))

  ;; Remove character from any quests they might have joined
  (unless (zerop (quest-id-of ch))
    (let ((quest (quest-by-vnum (quest-id-of ch))))
      (when quest
        (remove-from-quest (idnum-of ch)))))

  ;; Remove character from trusted lists - we have to take the accounts
  ;; in memory into consideration when we do this, so we have to go
  ;; through each account
  (dolist (account-id (postmodern:query (:select 'account
                                      :from 'trusted
                                      :where (:= 'player (idnum-of ch)))
                             :list))
    (let ((account (load-account account-id)))
      (setf (trust-of account) (delete (idnum-of ch) (trust-of account)))))
  (postmodern:execute (:delete-from 'trusted :where (:= 'player (idnum-of ch))))

  ;; TODO: Remove from the bounty list
  ;; (remove-bounties (idnum-of ch))
  (postmodern:execute (:delete-from 'bounty_hunters :where (:or (:= 'idnum (idnum-of ch))
                                                     (:= 'victim (idnum-of ch)))))

  ;; Disassociate author from board messages
  (postmodern:execute (:update 'board_messages
                    :set 'author :null
                    :where (:= 'author (idnum-of ch))))

  ;; Remove character from account
  (setf (players-of (account-of ch))
        (delete (idnum-of ch) (players-of (account-of ch))
                :key 'idnum-of))
  (postmodern:execute (:delete-from 'players :where (:= 'idnum (idnum-of ch))))

  ;; Remove character from game
  (when (in-room-of ch)
    (send-to-char ch "A cold wind blows through your soul, and you disappear... forever.~%")
    (purge-creature ch nil)))

(defun deposit-future-bank (account amount)
  "Deposits AMOUNT into the future bank of ACCOUNT."
  (check-type account account)
  (assert (not (minusp amount)))
  (unless (zerop amount)
    (incf (future-bank-of account) amount)
    (save-account account)))

(defun withdraw-future-bank (account amount)
  "Withdraws AMOUNT from the future bank of ACCOUNT."
  (check-type account account)
  (assert (not (minusp amount)))
  (unless (zerop amount)
    (decf (future-bank-of account) amount)
    (save-account account)))

(defun deposit-past-bank (account amount)
  "Deposits AMOUNT into the future bank of ACCOUNT."
  (check-type account account)
  (assert (not (minusp amount)))
  (unless (zerop amount)
    (incf (past-bank-of account) amount)
    (save-account account)))

(defun withdraw-past-bank (account amount)
  "Withdraws AMOUNT from the past bank of ACCOUNT."
  (check-type account account)
  (assert (not (minusp amount)))
  (unless (zerop amount)
    (decf (past-bank-of account) amount)
    (save-account account)))

