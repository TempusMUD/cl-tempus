(in-package :tempus)

(defclass player-record ()
  ((idnum :accessor idnum-of :initarg :idnum)
   (account :accessor account-of :initarg :account)
   (name :accessor name-of :initarg :name)
   (birth-time :accessor birth-time-of :initarg :birth-time)
   (login-time :accessor login-time-of :initarg :login-time)))

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
   (players :accessor players-of :initform nil)))

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
  (let ((player-count (query (:select (:count '*) :from 'players) :single)))
    (if (zerop player-count)
        (slog "WARNING: No characters loaded")
        (slog "... ~d character~p in db" player-count player-count))))

(defun max-account-id ()
  "Returns the maximum account id in the database"
  (or (query (:select (:max 'idnum) :from 'accounts) :single) 0))

(defun max-player-id ()
  "Returns the maximum player id in the database"
  (or (query (:select (:max 'idnum) :from 'players) :single) 0))

(defun account-exists (name)
  "Returns true if the account with the given name exists.  The comparison
is case-insensitive."
  (= (query (:select (:count '*)
             :from 'accounts
             :where (:= (:lower 'name) (string-downcase name)))
            :single)
     1))

(defun load-account (name)
  "Returns the account associated with the given name. The account may
be loaded from the database or it may be retrieved from a cache."
  (let* ((canonical-name (string-downcase name))
		 (cached (gethash canonical-name *account-name-cache* nil)))
	(if cached
		cached
		(let ((result (query (:select '*
                              :from 'accounts
                              :where (:= (:lower 'name) canonical-name))
                             :alist))
              (account (make-instance 'account)))
		  (when result
            (loop
               for tuple in result
               unless (eql (cdr tuple) :null)
               do (setf (slot-value account (intern (symbol-name (car tuple)))) (cdr tuple)))
            (setf (gethash canonical-name *account-name-cache*) account)
            (setf (gethash (idnum-of account) *account-idnum-cache*) account)
            (setf (players-of account)
                  (mapcar (lambda (info)
                            (make-instance 'player-record
                                           :idnum (cdr (assoc :idnum info))
                                           :account (idnum-of account)
                                           :name (cdr (assoc :name info))
                                           :birth-time (if (eql (cdr (assoc :birth-time info)) :null) (now) (cdr (assoc :birth-time info)))
                                           :login-time (if (eql (cdr (assoc :login-time info)) :null) (now) (cdr (assoc :login-time info)))))
                           (query (:order-by (:select 'idnum 'name 'birth-time 'login-time :from 'players :where (:= 'account (idnum-of account))) 'idnum) :alists)))
            account)))))

(defmethod save-account ((account account))
  "Saves the account information into the database."
  (execute (:update 'accounts :set
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

(defun account-login (account)
  "Performs necessary tasks for an account upon a successful login."
  (syslog "~a logged in" (name-of account)))

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
  (plusp (query (:select (:count '*)
                 :from 'players
                 :where (:= (:lower 'name) (string-downcase name)))
                :single)))

(defun retrieve-player-idnum (name)
  "Retrieves the idnum of the player with the given name from the database.
Returns NIL if the player did not exist."
  (query (:select 'idnum
                  :from 'players
                  :where (:= (:lower 'name) (string-downcase name)))
         :single))

(defun retrieve-player-name (idnum)
  "Retrieves the name of the player with the given idnum from the database.
Returns NIL if the player did not exist."
  (query (:select 'name
                  :from 'players
                  :where (:= 'idnum idnum))
         :single))

(defun create-new-player (actor account)
  "Creates a new player from the given actor.  The player is stored into
the account database, while the actor is stored in the proper player
file."
  (let* ((now (now))
         (player-record (make-instance 'player-record
									   :idnum (idnum-of actor)
									   :account (idnum-of account)
									   :name (name-of actor)
									   :birth-time now
									   :login-time now)))
    (execute (:insert-into 'players :set
                           'idnum (idnum-of actor)
                           'account (idnum-of account)
                           'name (name-of actor)
                           'birth_time now
                           'login_time now))
	(setf (players-of account)
		  (nconc (players-of account) (list player-record)))
	(save-account account)
	(setf (title-of actor) "the utter newbie")
    (setf (birth-time-of actor) now)
    (setf (login-time-of actor) now)
	(save-player-to-xml actor)))

(defun deposit-future-bank (account amount)
  "Deposits AMOUNT into the future bank of ACCOUNT."
  (check-type account account)
  (assert (not (minusp amount)))
  (unless (zerop amount)
    (incf (future-bank-of account) amount)
    (save-account account)))

(defun deposit-past-bank (account amount)
  "Deposits AMOUNT into the past bank of ACCOUNT."
  (check-type account account)
  (assert (not (minusp amount)))
  (unless (zerop amount)
    (incf (past-bank-of account) amount)
    (save-account account)))
