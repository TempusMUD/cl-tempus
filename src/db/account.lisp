(in-package :tempus)

(defclass player-record ()
  ((idnum :accessor idnum-of :initarg :idnum)
   (account :accessor account-of :initarg :account)
   (name :accessor name-of :initarg :name)
   (birth-time :accessor birth-time-of :initarg :birthtime)
   (login-time :accessor login-time-of :initarg :logintime)))

(defclass account ()
  ((idnum :accessor idnum-of :initarg :idnum)
   (name :accessor name-of :initarg :name)
   (password :accessor password-of :initarg :password)
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
   (players :accessor players-of)
   (bank-past :accessor past-bank-of :initform 0)
   (bank-future :accessor future-bank-of :initform 0)))

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
              (retrieved (make-instance 'account)))
          (loop for (field value) in retrieved
               do (setf (slot-value account field) value))
		  (cond
			(retrieved
			 (setf (gethash (name-of retrieved) *account-name-cache*)
				   retrieved)
			 (setf (gethash (idnum-of retrieved) *account-idnum-cache*)
				   retrieved)
			 (setf (players-of retrieved)
				   (sort (players-of retrieved)
						 #'<
						 :key #'idnum-of))
			 retrieved)
			(t
			 nil))))))

(defun save-account (account)
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

#-sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *crypt-library-loaded* nil)

  (unless *crypt-library-loaded*
    (uffi:load-foreign-library 
     (uffi:find-foreign-library "libcrypt"
			   '(#+64bit "/usr/lib64/"
			     "/usr/lib/" "/usr/local/lib/" "/lib/"))
     :supporting-libraries '("c"))
    (setq *crypt-library-loaded* t)))

(uffi:def-function ("crypt" crypt)
    ((key :cstring)
     (salt :cstring))
  :returning :cstring)

(defun crypt-password (password salt)
  "Encrypt a password with a hash"
  (uffi:with-cstring (password-cstring password)
    (uffi:with-cstring (salt-cstring salt)
      (uffi:convert-from-cstring 
       (crypt password-cstring salt-cstring)))))

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
									   :birthtime now
									   :logintime now)))
    (execute (:insert-into 'players :set
                           'idnum (idnum-of actor)
                           'account (account-of actor)
                           'name (name-of actor)
                           'birthtime now
                           'logintime now))
	(setf (players-of account)
		  (nconc (players-of account) (list player-record)))
	(save-account account)
	(setf (title-of actor) "the utter newbie")
	(save-player actor)))

(defun save-player (player)
  "Saves the player into the player file and its equipment into its
equipment file."
  (with-open-file (ouf (player-pathname (idnum-of player))
				   :direction :output
				   :if-exists :supersede
				   :if-does-not-exist :create)
	(format ouf "(player
 :idnum ~d
 :name ~s
 :full-desc ~s
 :sex ~s
 :species ~s
 :eye-desc ~s
 :hair-desc ~s
 :overall-desc ~s
 :align ~d
 :height ~d
 :weight ~d
 :strength ~d
 :agility ~d
 :title ~s
 :prefs ~s
 :player-aliases ~s
 :home ~s
 :loadplace ~s
 :poofin ~s
 :poofout ~s
 :created-time ~s
 :deaths ~s
 :mkills ~s
 :pkills ~s
 :private-names ~s)~%"
			(idnum-of player)
			(name-of player)
			(full-desc-of player)
			(sex player)
            (species player)
			(eye-desc-of player)
			(hair-desc-of player)
			(overall-desc-of player)
			(align player)
			(height player)
            (weight-of player)
            (strength player)
            (agility player)
			(title player)
			(player-prefs-list (prefs player))
			(player-aliases player)
			(home player)
			(loadplace player)
			(poofin player)
			(poofout player)
            (created-time-of player)
            (deaths-of player)
            (mkills-of player)
            (pkills-of player)
			(hash-to-assoc (private-names-of player))))

  ;; Save the equipment now, or delete if the player has none
  (let ((path (equipment-pathname (idnum-of player))))
	(if (carrying player)
		(with-open-file (ouf path
						 :direction :output
						 :if-exists :supersede
						 :if-does-not-exist :create)
		  (dolist (item (carrying player))
			(write-item item ouf)))
		(when (probe-file path)
		  (delete-file path)))))

(defun load-equipment (idnum)
  "Returns a list of all the equipment in the equipment file."
  (when (probe-file (equipment-pathname idnum))
	(with-open-file (inf (equipment-pathname idnum) :direction :input)
	  (loop for desc = (read inf nil nil)
		   while desc
		   collect (load-item desc) into result
		   finally (return (nreverse result))))))
