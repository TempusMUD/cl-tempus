(in-package #:tempus)

(defmacro define-connection-state (state-name &body funcs)
  (assert (every #'consp funcs))
  (assert (symbolp state-name))
  (assert (every (lambda (x) (member (first x) '(menu prompt input))) funcs))
  (assert (every (lambda (x) (consp (second x))) funcs))

  `(progn
     (pushnew ',state-name +valid-cxn-states+)
     ,@(loop for func in funcs collect
            (let ((specifier (first func))
                  (args (second func))
                  (body (cddr func)))
              (case specifier
                (menu
                 `(defmethod send-state-menu ((,(first args) tempus-cxn)
                                              (state (eql ',state-name)))
                    ,@body))
                (prompt
                 `(defmethod send-state-prompt ((,(first args) tempus-cxn)
                                                (state (eql ',state-name)))
                    ,@body))
                (input
                 `(defmethod handle-state-input ((,(first args) tempus-cxn)
                                                 (state (eql ',state-name))
                                                 ,(second args))
                    ,@body)))))))

(defparameter +valid-cxn-states+
  '(login
    authenticate
    new-account
    new-account-verify
    new-account-password
    verify-password
    new-account-ansi
    new-account-compact
    new-account-email
    set-password-auth
    set-password-new
    new-player-name
	new-player-species
	new-player-sex
	new-player-eyes
	new-player-hair
	new-player-overall
	wait-for-menu
    main-menu
    playing
	afterlife
    disconnecting))

(defparameter +eye-descriptions+ 
  '("nondescript" "red" "green" "blue" "yellow" "purple" "hazel" "cheerful"
	"sad" "sinister" "slotted" "slitted" "searching" "probing" "hard" "soft"
	"smiling" "striking"))

(defparameter +hair-descriptions+ 
  '("bald" "blonde" "brown" "crimson" "curly" "grey" "flowing"
    "nondescript" "platinum" "red" "silver" "white"))

(defparameter +overall-descriptions+
  '("arrogant" "bitter" "bold" "calm" "careful" "cautious" "cheerful"
	"contented" "crazy" "cunning" "curious" "defiant" "desperate"
	"dreamy" "evil" "fearful" "fierce" "fond" "gallant" "gentle"
	"gracious" "happy" "hateful" "icy" "impatient" "innocent" "kind"
	"knowing" "language" "lazy" "longing" "loud" "loving" "lusty"
	"malicious" "merry" "mocking" "morbid" "nervous" "nice" "outrageous"
	"passionate" "patient" "pious" "playful" "polite" "proud" "quick" "quiet"
	"rude" "ruthless" "sad" "sarcastic" "savage" "seductive" "sheepish"
	"shy" "silent" "skeptical" "slow" "sly" "soft" "solemn" "stoic"
	"sweet" "vicious" "wary" "warm" "weary" "wild" "wishful" "wistful"
	"valiant"))

(defvar *cxn-paginate* nil)

(defclass tempus-cxn (data-cxn)
  ((state :accessor cxn-state :initform 'login :type symbol)
   (need-prompt :accessor cxn-need-prompt :initform t)
   (account :accessor cxn-account :type (or null account) :initform nil)
   (actor :accessor cxn-actor :initform nil)
   (page-buf :accessor cxn-page-buf :initform "")
   (mode-data :accessor cxn-mode-data :initform nil)))

(defmethod send-state-menu ((cxn tempus-cxn) state)
  (declare (ignore cxn state))
  nil)
(defmethod send-state-prompt ((cxn tempus-cxn) state)
  (declare (ignore cxn state))
  nil)
(defmethod handle-state-input ((cxn tempus-cxn) state line)
  (declare (ignore cxn state line))
  nil)

(defun password-state-p (state)
  (or (eql state 'authenticate)
	  (eql state 'new-account-password)
	  (eql state 'verify-password)
	  (eql state 'set-password-auth)
	  (eql state 'set-password-new)))

(defmethod (setf cxn-state) :around (state (cxn tempus-cxn))
  ;;; send telnet DO ECHO when the new state isn't a password state
  (when (password-state-p (cxn-state cxn))
	(cxn-write cxn "~c~c~c"
			   (code-char #xff)
			   (code-char #xfc)
			   (code-char #x01)))
  (cond
    ((member state +valid-cxn-states+)
     (send-menu cxn state)
	 (call-next-method))
    (t
     (mudlog 'warning t "attempt to set cxn to state ~a" state)))

  ;;; send telnet DONT ECHO when the new state is a password state
  (when (password-state-p (cxn-state cxn))
	(cxn-write cxn "~c~c~c"
			   (code-char #xff)
			   (code-char #xfb)
			   (code-char #x01))))

(defmethod cxn-write ((cxn tempus-cxn) fmt &rest args)
  (cond
	((eql *cxn-paginate* cxn)
     ;; Queue up text to be paginated later
	 (setf (cxn-page-buf cxn)
		   (concatenate 'string (cxn-page-buf cxn)
						(colorize cxn (format nil "~?" fmt args)))))
	(t
     ;; Normal writing to connection
	 (unless (or (cxn-need-prompt cxn)
				 (and (cxn-actor cxn)
					  (not (autoprompt-p (prefs (cxn-actor cxn))))))
	   (when (or (null (cxn-account cxn))
				 (oddp (compact-level-of (cxn-account cxn))))
		 (setf (cxn-output-buf cxn)
			   (concatenate '(vector (unsigned-byte 8))
							(cxn-output-buf cxn)
							'(#x0d #x0a))))
	   (when (or (null (cxn-actor cxn))
				 (autoprompt-p (prefs (cxn-actor cxn))))
		 (setf (cxn-need-prompt cxn) t)))
	 (call-next-method cxn "~a"
					   (colorize cxn (format nil "~?" fmt args))))))

(defmethod handle-accept ((cxn tempus-cxn))
  (mudlog 'info t "New connection received from ~a" (peer-addr cxn))
  (setf (cxn-state cxn) 'login))

(defmethod handle-flush :before ((cxn tempus-cxn))
  (when (cxn-need-prompt cxn)
	(when (and (eql (cxn-state cxn) 'playing)
			   (< (compact-level-of (cxn-account cxn)) 2))
	  (cxn-write cxn "~%"))
    (send-state-prompt cxn (cxn-state cxn))
	(when (and (eql (cxn-state cxn) 'playing)
			   (evenp (compact-level-of (cxn-account cxn))))
	  (cxn-write cxn "~%"))
	(setf (cxn-need-prompt cxn) nil)))

(defmethod handle-close ((cxn tempus-cxn))
  (cond
	((null (cxn-account cxn))
	 (mudlog 'info t "Closing connection without account"))
	((and (eql (cxn-state cxn) 'playing) (cxn-actor cxn))
	 (mudlog 'notice t "~a has lost link; ~a logged out"
			 (name-of (cxn-actor cxn))
			 (name-of (cxn-account cxn)))
	 (act (cxn-actor cxn) :place-emit "$n has lost $s link.")
	 (setf (link (cxn-actor cxn)) nil))
	(t
	 (mudlog 'notice t "~a logged out" (name-of (cxn-account cxn))))))

(defun apply-player-alias (actor alias args)
  ;; split replacements into lines
  (let ((replacements (split-sequence #\; (second alias))))
    (dolist (cmd-line (reverse replacements))
      ;; push each alias expansion into the command queue of the actor
      ;; (in reverse for proper ordering)
      (push (concatenate 'string
                         "\\"
                         (string-left-trim '(#\space #\tab)
                                           (string-replace "$*" cmd-line args)))
            (cxn-commands (link actor))))
    ;; return the first command string
    (string-left-trim '(#\\) (pop (cxn-commands (link actor))))))

(defun expand-aliases (actor str)
  (cond
    ((eql (char str 0) #\\)
     ;; Backslashes reference a literal command
     (string-left-trim '(#\\) str))
    ((not (player-p actor))
     ;; Mobiles don't have aliases
     str)
    (t
     ;; Find any alias and apply it
     (with-words str (cmd &rest args)
       (let ((alias (assoc cmd (player-aliases actor) :test #'string-equal)))
         (if alias
             (apply-player-alias actor alias args)
             str))))))


(defun player-pathname (idnum)
  (make-pathname :name (princ-to-string idnum)
				 :type "plyr"
				 :defaults
				 (merge-pathnames (format nil "data/players/~d/"
										  (mod idnum 10))
								  (asdf:component-pathname
								   (asdf:find-system "tempus")))))

(defun mail-pathname (idnum)
  (make-pathname :name (princ-to-string idnum)
				 :type "mail"
				 :defaults
				 (merge-pathnames (format nil "data/mail/~d/"
										  (mod idnum 10))
								  (asdf:component-pathname
								   (asdf:find-system "tempus")))))

(defun equipment-pathname (idnum)
  (make-pathname :name (princ-to-string idnum)
				 :type "equip"
				 :defaults
				 (merge-pathnames (format nil "data/equipment/~d/"
										  (mod idnum 10))
								  (asdf:component-pathname
								   (asdf:find-system "tempus")))))

(defun load-player-actor (idnum)
  "Given the player id number, loads the player's actor into memory
and returns it."
  (let ((*package* (find-package 'tempus)))
	(with-open-file (inf (player-pathname idnum) :direction :input)
	  (let* ((new-player (apply #'make-instance (read inf)))
			 (alias (string-downcase (name-of new-player))))
		(setf (line-desc-of new-player)
			  (if (title new-player)
				  (concatenate 'string (name-of new-player) " " (title new-player))
				  (name-of new-player)))
        (setf (aliases-of new-player)
              (list alias (concatenate 'string "." alias)))
        (setf (short-desc-of new-player)
              (format nil "~:[a~;an~] ~a, ~a-eyed ~(~a~)"
                      (char-vowel-p (char (overall-desc-of new-player) 0))
                      (overall-desc-of new-player)
                      (eye-desc-of new-player)
                      (name-of (get-species (species new-player)))))
		(setf (prefs new-player)
			  (apply 'make-player-prefs (prefs new-player)))
		(setf (private-names-of new-player)
			  (assoc-to-hash (private-names-of new-player)))
		new-player))))

(defun player-to-game (player)
  (mudlog 'notice t "~a entering game as ~a"
		  (name-of (cxn-account (link player)))
		  (name-of player))
  (actor-to-place player
				  (get-place (or (loadplace player)
								 (home player)
								 10002)))
  (setf (loadplace player) nil)
  (dolist (equip (load-equipment (idnum-of player)))
	(item-to-actor equip player))
  (unless (plusp (hitp player))
	(setf (hitp player) 1))
  (act player :all-emit "$n enter$% the game.")
  (push player *characters*)
  (display-contents (place player) player (brief-p (prefs player)))
  (when (probe-file (mail-pathname (idnum-of player)))
	(send-to-actor player "You have new mail.~%"))
  (save-player player)
  (setf (cxn-state (link player)) 'playing))

(defun send-section-header (cxn str)
  (if (string= str "")
	  (cxn-write cxn "&b~v,1,,'-a&n~%" 72 "")
	  (cxn-write cxn "&b~v,1,,'-a &y~a &b~v,1,,'-a&n~%"
				 (- (/ 70 2) (/ (1+ (length str)) 2)) ""
				 (string-capitalize str)
				 (- (/ 70 2) (/ (length str) 2)) "")))

(define-connection-state login
  (menu (cxn)
	 (cxn-write cxn "&@~a" *welcome-message*))
  (prompt (cxn)
    (cxn-write cxn "Please enter your account name, or 'new': "))
  (input (cxn line)
   (cond
     ((string= line "")
      (setf (cxn-connected cxn) nil)
      (setf (cxn-state cxn) 'disconnecting))
     ((string-equal line "new")
      (setf (cxn-state cxn) 'new-account))
     ((account-exists line)
      (setf (cxn-account cxn) (load-account line))
      (setf (cxn-state cxn) 'authenticate))
     (t
      (cxn-write cxn
                 "Sorry, that account does not exist.  Type 'new' to create another account.~%"))))
  (menu (cxn)
        ""))

(define-connection-state authenticate
  (prompt (cxn)
    (cxn-write cxn "Password: "))
  (input (cxn line)
   (cond
     ((not (check-password (cxn-account cxn) line))
      (mudlog 'warning t "PASSWORD: account ~d (~a) failed to authenticate."
              (idnum-of (cxn-account cxn))
              (name-of (cxn-account cxn)))
      (cxn-write cxn "~%Invalid password.~%~%")
      (setf (cxn-state cxn) 'login))
     ((players-of (cxn-account cxn))
      (account-login (cxn-account cxn))
      (setf (cxn-state cxn) 'main-menu))
     (t
      (account-login (cxn-account cxn))
      (setf (cxn-state cxn) 'new-player-name)))))

(define-connection-state new-account
  (menu (cxn)
    (cxn-write cxn "&@")
    (send-section-header cxn "account creation")
    (cxn-write cxn "
   You have an account, which is a handy way of keeping
track of all your characters here.  All your characters share a bank
account, and you can see at a single glance which of your characters
have received mail.  Quest points are also shared by all your characters.

"))
  (prompt (cxn)
    (cxn-write cxn "What would you like the name of your account to be? "))
  (input (cxn line)
   (cond
     ((zerop (length line))
      (setf (cxn-state cxn) 'login))
     ((account-exists line)
      (cxn-write cxn "~%Sorry, that account name is already taken.~%"))
     ((validate-name line)
      (setf (cxn-mode-data cxn) line)
      (setf (cxn-state cxn) 'new-account-verify))
     (t
      nil))))

(define-connection-state new-account-verify
  (prompt (cxn)
    (cxn-write cxn "~%~%Are you sure you want your account name to be '~a' (Y/N)? "))
  (input (cxn line)
   (case (char-downcase (char line 0))
     (#\y
      (setf (cxn-account cxn)
            (make-instance 'account :name (cxn-mode-data cxn)))
      (setf (idnum-of (cxn-account cxn))
            (1+ (max-account-id)))
      (save-account (cxn-account cxn))
      (setf (cxn-state cxn) 'new-account-ansi))
     (#\n
      (setf (cxn-state cxn) 'new-account))
     (t
      (cxn-write cxn "Please enter Y or N.~%")))))

(define-connection-state new-account-ansi
  (menu (cxn)
   (cxn-write cxn "&@")
   (send-section-header cxn "ansi color")
   (cxn-write cxn "
   This game supports ANSI color standards.  If you have a color capable
terminal and wish to see useful color-coding of text, select the amount of
coloring you desire.  You may experiment within the game with the 'color'
command to see which level suits your personal taste.

   If your terminal does not support color, you will want to select
'none', as the color codes may mess up your display.  Use of at least
some color is HIGHLY recommended, as it improves gameplay dramatically.

                None - No color will be used
              Sparse - Minimal amounts of color will be used.
              Normal - Color will be used a medium amount.
            Complete - Use the maximum amount of color available.

"))
  (prompt (cxn)
    (cxn-write cxn "Enter the level of color you prefer: "))
  (input (cxn line)
   (let ((pos (position line '("none" "sparse" "normal" "complete")
                        :test #'string-abbrev)))
     (cond
       (pos
        (setf (ansi-level-of (cxn-account cxn)) pos)
        (save-account (cxn-account cxn))
        (setf (cxn-state cxn) 'new-account-compact))
       (t
        (cxn-write cxn "~%Please enter one of the selections.~%~%"))))))

(define-connection-state new-account-compact
  (menu (cxn)
   (cxn-write cxn "&@")
   (send-section-header cxn "text compactness")
   (cxn-write cxn "
   Many players have differing tastes as to the vertical spacing of their
display.  A less compact view is often easier to read, while a more
compact view allows for more lines to fit on the screen.

Compact off:                         Compact minimal:
<---H ---M ---V ---A>                <---H ---M ---V ---A>
A goblin spits in your face!         A goblin tries to steal from you!

<---H ---M ---V ---A>                <---H ---M ---V ---A> kill goblin
kill goblin

Compact partial:                     Compact full:
<---H ---M ---V ---A>                <---H ---M ---V ---A>
A goblin gives you a wedgie!         A goblin laughs insultingly at you!
<---H ---M ---V ---A>                <---H ---M ---V ---A> kill goblin
kill goblin

"))
  (prompt (cxn)
    (cxn-write cxn "Enter the level of compactness you prefer (off,minimal,partial,full): "))
  (input (cxn line)
   (let ((pos (position line '("off" "minimal" "partial" "full")
                        :test #'string-abbrev)))
     (cond
       (pos
        (setf (compact-level-of (cxn-account cxn)) pos)
        (save-account (cxn-account cxn))
        (setf (cxn-state cxn) 'new-account-email))
       (t
        (cxn-write cxn "~%Please enter one of the selections.~%~%"))))))

(define-connection-state new-account-email
  (menu (cxn)
   (cxn-write cxn "&@")
   (send-section-header cxn "email address")
   (cxn-write cxn "
   You may elect to associate an email address with this account.  This
is entirely optional, and will not be sold to anyone.  Its primary use is
password reminders.

"))
  (prompt (cxn)
    (cxn-write cxn "Please enter your email address:"))
  (input (cxn line)
   (setf (email-of (cxn-account cxn)) line)
   (save-account (cxn-account cxn))
   (setf (cxn-state cxn) 'new-account-password)))

(define-connection-state new-account-password
  (menu (cxn)
   (cxn-write cxn "&@")
   (send-section-header cxn "set password")
   (cxn-write cxn "
   In order to protect your character against intrusion, you must
choose a password to use on this system.
"))
  (prompt (cxn)
    (cxn-write cxn "        Enter your desired password:"))
  (input (cxn line)
   (setf (password-of (cxn-account cxn)) line)
   (save-account (cxn-account cxn))
   (setf (cxn-state cxn) 'verify-password)))

(define-connection-state verify-password
  (prompt (cxn)
    ;; This awkward wording due to lame "assword:" search in tintin instead
    ;; of actually implementing one facet of telnet protocol
    (cxn-write cxn
               "        Enter it again to verify your password:"))
  (input (cxn line)
   (cond
     ((check-password (cxn-account cxn) line)
      (setf (cxn-state cxn) 'main-menu))
     (t
      (cxn-write cxn "~%The passwords did not match!  Try again!~%")
      (setf (password-of (cxn-account cxn)) nil)
      (setf (cxn-state cxn) 'new-account-password)))))

(define-connection-state new-player-name
  (menu (cxn)
    (cxn-write cxn "&@")
	(send-section-header cxn "new character"))
  (prompt (cxn)
    (cxn-write cxn "Enter the true name you wish for this character: "))
  (input (cxn line)
   (cond
     ((string= line "")
      (setf (cxn-state cxn) 'main-menu))
     ((player-name-exists line)
      (cxn-write cxn "~%Sorry, that name is already taken.~%"))
     ((validate-name line)
      (setf (cxn-actor cxn)
            (make-instance 'player
                           :idnum (1+ (max-player-id))
                           :name (string-capitalize line)
                           :prefs (make-player-prefs)
                           :private-names (make-hash-table)))
      (setf (cxn-state cxn) 'new-player-species))
     (t
      (cxn-write cxn "~%You have entered an invalid name.  Try again.~%")))))

(define-connection-state new-player-species
  (menu (cxn)
   (cxn-write cxn "&@")
   (send-section-header cxn "species")
   (cxn-write cxn "
             Human                       Elf
             Dwarf                       Halfling
             Pixie                       Sidhe

"))
  (prompt (cxn)
    (cxn-write cxn "Enter the species you want for this character: "))
  (input (cxn line)
    (cond
      ((or (string= line "")
           (null (species-by-name line)))
       (cxn-write cxn "~%You need to enter one of the listed species.~%~%"))
      (t
       (setf (species (cxn-actor cxn)) (idnum-of (species-by-name line)))
       (setf (cxn-state cxn) 'new-player-sex)))))

(define-connection-state new-player-sex
  (menu (cxn)
    (cxn-write cxn "&@")
	(send-section-header cxn "sex"))
  (prompt (cxn)
    (cxn-write cxn "What sex is your character: "))
  (input (cxn line)
   (cond
     ((string= line "")
      (cxn-write cxn "~%You really should enter your sex, you know.~%~%"))
     ((not (member (char-downcase (char line 0)) '(#\m #\f)))
      (cxn-write cxn "~%You can only be a male or a female!~%~%"))
     (t
      (if (eql (char-downcase (char line 0)) #\m)
          (setf (sex (cxn-actor cxn)) 'male)
          (setf (sex (cxn-actor cxn)) 'female))
      (setf (cxn-state cxn) 'new-player-eyes)))))

(define-connection-state new-player-eyes
  (menu (cxn)
    (cxn-write cxn "&@")
	(send-section-header cxn "eye color")
    (columnar-list-to-cxn
     cxn 5 14
     (sort +eye-descriptions+ #'string<)))
  (prompt (cxn)
    (cxn-write cxn "What is your character's eye color: "))
  (input (cxn line)
   (cond
     ((or (string= line "")
          (not (member line +eye-descriptions+ :test 'string-equal)))
      (cxn-write cxn "~%Please select a listed eye description.~%~%"))
     (t
      (setf (eye-desc-of (cxn-actor cxn)) line)
      (setf (cxn-state cxn) 'new-player-hair)))))

(define-connection-state new-player-hair
  (menu (cxn)
    (cxn-write cxn "&@")
	(send-section-header cxn "hair description")
    (columnar-list-to-cxn
     cxn 5 14
     (sort +hair-descriptions+ #'string<)))
  (prompt (cxn)
    (cxn-write cxn "What is your character's hair color?"))
  (input (cxn line)
   (cond
     ((or (string= line "")
          (not (member line +hair-descriptions+ :test 'string-equal)))
      (cxn-write cxn "~%Please selected a listed hair description.~%~%"))
     (t
      (setf (hair-desc-of (cxn-actor cxn)) line)
      (setf (cxn-state cxn) 'new-player-overall)))))

(define-connection-state new-player-overall
  (menu (cxn)
    (cxn-write cxn "&@")
	(send-section-header cxn "overall description")
    (columnar-list-to-cxn
     cxn 5 14
     (sort +overall-descriptions+ #'string<)))
  (prompt (cxn)
    (cxn-write cxn "Select an overall adjective for your character: "))
  (input (cxn line)
   (cond
     ((or (string= line "")
          (not (member line +overall-descriptions+ :test 'string-equal)))
      (cxn-write cxn "~%Please selected a listed overall description.~%~%"))
     (t
      (setf (overall-desc-of (cxn-actor cxn)) line)
      (create-new-player (cxn-actor cxn) (cxn-account cxn))
      (setf (cxn-actor cxn) nil)
      (setf (cxn-state cxn) 'main-menu)))))

(define-connection-state set-password-auth
  (menu (cxn)
    (cxn-write cxn "&@")
	(send-section-header cxn "changing password"))
  (prompt (cxn)
    (cxn-write cxn "~%     For security purposes, please enter your old password: "))
  (input (cxn line)
   (cond
     ((string= line "")
      (cxn-write cxn "~%Password change aborted.~%")
      (setf (cxn-state cxn) 'wait-for-menu))
     ((check-password (cxn-account cxn) line)
      (setf (cxn-state cxn) 'set-password-new))
     (t
      (cxn-write cxn "~%Wrong password!  Password change cancelled.~%")
      (setf (cxn-state cxn) 'wait-for-menu)))))

(define-connection-state set-password-new
  (prompt (cxn)
   (cxn-write cxn "~%~%     Enter your new password: "))
  (input (cxn line)
   (cond
     ((string= line "")
      (cxn-write cxn "~%You may not have a blank password.~%~%"))
     (t
      (cxn-write cxn "~%~%Your password has been set.~%")
      (setf (password-of (cxn-account cxn)) line)
      (setf (cxn-state cxn) 'wait-for-menu)))))

(define-connection-state wait-for-menu
  (prompt (cxn)
    (cxn-write cxn "~%Press RETURN to return to the main menu.~%"))
  (input (cxn line)
   (setf (cxn-state cxn) 'main-menu)))

(define-connection-state main-menu
  (menu (cxn)
   (cxn-write cxn "&@")
   (send-section-header cxn "")
   (send-section-header cxn "main menu")
   (send-section-header cxn "")
   (loop for player in (players-of (cxn-account cxn))
      for idx from 1 do
      (cxn-write cxn " &b[&y~2,' d&b]&n  ~10a ~a~%"
                 idx
                 (name-of player)
                 (multiple-value-bind (ms second minute hour day month year dow)
                     (decode-time (login-time-of player))
                   (declare (ignorable ms second minute hour dow))
                   (format nil "~d/~2,'0d/~2,'0d" year month day))))
   (cxn-write cxn "~%~5tPast bank: ~d~50tFuture bank: ~d~%"
              (past-bank-of (cxn-account cxn))
              (future-bank-of (cxn-account cxn)))
   (cxn-write cxn "~%~23t&b[&yC&b]&n &cCreate a new character")
   (cxn-write cxn "~%~23t&b[&yP&b]&n &cChange your password")
   (cxn-write cxn "~%~23t&b[&yL&b]&n &cLog out of the game&n~%~%"))
  (prompt (cxn)
    (cxn-write cxn "Enter your selection: "))
  (input (cxn line)
   (cond 
     ((string= line "")
      (send-menu cxn 'main-menu))
     ((or (char-equal (char line 0) #\l) (eql (char line 0) #\0))
      (cxn-write cxn "Goodbye!~%")
      (setf (cxn-connected cxn) nil)
      (setf (cxn-state cxn) 'disconnecting))
     ((char-equal (char line 0) #\c)
      (setf (cxn-state cxn) 'new-player-name))
     ((char-equal (char line 0) #\p)
      (setf (cxn-state cxn) 'set-password-auth))
     ((every #'digit-char-p line)
      (let* ((player (nth (1- (parse-integer line)) (players-of (cxn-account cxn))))
             (prev-actor (and player (player-in-world (idnum-of player)))))
        (cond
          ((null player)
           (cxn-write cxn "That character selection does not exist!~%~%"))
          ((null prev-actor)
           (setf (cxn-actor cxn) (load-player-actor (idnum-of player)))
           (setf (login-time-of player) (now))
           (execute (:update 'players :set
                             'login-time (login-time-of player)))
           (setf (link (cxn-actor cxn)) cxn)
           (player-to-game (cxn-actor cxn)))
          ((link prev-actor)
           (send-to-actor prev-actor "You have logged on from another location!~%")
           (setf (cxn-actor (link prev-actor)) nil)
           (setf (cxn-state (link prev-actor)) 'disconnecting)
           (setf (cxn-connected (link prev-actor)) nil)
           (setf (link prev-actor) cxn)
           (setf (cxn-actor cxn) prev-actor)
           (setf (cxn-state cxn) 'playing)
           (act prev-actor
			   	:subject-emit "You take over your own body, already in use!"
				:place-emit "$n has reconnected.")
           (mudlog 'notice t "~a has reconnected" (name-of prev-actor)))
          (t
           (setf (actor-of cxn) prev-actor)
           (setf (link-of prev-actor) cxn)
           (setf (state-of cxn) 'playing)
           (act prev-actor
			   	:subject-emit "You take over your own body!"
				:place-emit "$n has reconnected.")
           (mudlog 'notice t "~a has reconnected from linkless" (name-of prev-actor))))))
     (t
      (cxn-write cxn "That's not an option!~%")))))

(define-connection-state afterlife
  (prompt (cxn)
    (cxn-write cxn "~%Press RETURN to reincarnate.~%"))
  (input (cxn line)
   ;; hitting return causes re-entry into the game
   (player-to-game (cxn-actor cxn))))

(define-connection-state playing
  (prompt (cxn)
   (cond
     ((and (action (cxn-actor cxn))
           (send-action-prompt (action (cxn-actor cxn))))
      ;; if send-action-prompt returns t, we don't need to send the
      ;; standard prompt
      nil)
     ((not (or (display-hitp-p (prefs (cxn-actor cxn)))
               (display-mana-p (prefs (cxn-actor cxn)))
               (display-move-p (prefs (cxn-actor cxn)))))
      (cxn-write cxn "&W>&n "))
     (t
      (cxn-write cxn "&W< ~@[&G~a&YH ~]~@[&M~a&YM ~]~@[&C~a&YV ~]&W>&n "
                 (when (display-hitp-p (prefs (cxn-actor cxn)))
                   (hitp (cxn-actor cxn)))
                 (when (display-mana-p (prefs (cxn-actor cxn)))
                   (mana (cxn-actor cxn)))
                 (when (display-move-p (prefs (cxn-actor cxn)))
                   (move (cxn-actor cxn)))))))
  (input (cxn line)
   (unless (and (action (cxn-actor cxn))
                (interpret (action (cxn-actor cxn)) line))
     (let ((trimmed-line (string-left-trim '(#\/ #\space #\tab) line)))
       (when (plusp (length trimmed-line))
         (interpret-command (cxn-actor cxn)
                            (expand-aliases (cxn-actor cxn)
                                            trimmed-line)))))))

(defun send-menu (cxn state)
  (send-state-menu cxn state))

(defun send-prompt (cxn)
  (send-state-prompt cxn (cxn-state cxn)))

(defun cxn-do-command (cxn line)
  (restart-case
      (handle-state-input cxn (cxn-state cxn) line)
   (continue () nil)))

(defun cxn-handle-commands ()
  (dolist (cxn *cxns*)
	(when (cxn-commands cxn)
	  (setf (cxn-need-prompt cxn) t)
	  (let ((cmd (pop (cxn-commands cxn))))
		(handler-bind ((error (lambda (str)
                               (cxn-write cxn "You become mildly queasy as reality distorts momentarily.~%")
                               (errlog "System error: ~a" str)
                               (invoke-restart 'continue))))
            (cxn-do-command cxn cmd))))))

