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
    view-policy
    set-password-auth
    set-password-new
    set-password-verify
    new-player-name
	new-player-class
    new-player-race
	new-player-sex
    new-player-align
    new-player-stats
    main-menu
	wait-for-menu
    remort-class
    delete-character
    delete-verify
    delete-password
    describe-character
    describe-editing
    playing
	afterlife
    disconnecting))

(defvar *cxn-paginate* nil)

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

(defmethod (setf state-of) :around (state (cxn tempus-cxn))
  ;;; send telnet DO ECHO when the new state isn't a password state
  (when (password-state-p (state-of cxn))
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
  (when (password-state-p (state-of cxn))
	(cxn-write cxn "~c~c~c"
			   (code-char #xff)
			   (code-char #xfb)
			   (code-char #x01))))

(defmethod cxn-write ((cxn tempus-cxn) fmt &rest args)
  (cond
	((eql *cxn-paginate* cxn)
     ;; Queue up text to be paginated later
	 (setf (page-buf-of cxn)
		   (concatenate 'string (page-buf-of cxn)
						(colorize cxn (format nil "~?" fmt args)))))
	(t
     ;; Normal writing to connection
	 (unless (or (need-prompt-p cxn)
				 (and (actor-of cxn)
					  (not (pref-flagged (actor-of cxn) +pref-autoprompt+))))
	   (when (or (null (account-of cxn))
				 (oddp (compact-level-of (account-of cxn))))
		 (setf (cxn-output-buf cxn)
			   (concatenate '(vector (unsigned-byte 8))
							(cxn-output-buf cxn)
							'(#x0d #x0a))))
	   (when (or (null (actor-of cxn))
                 (pref-flagged (actor-of cxn) +pref-autoprompt+))
		 (setf (need-prompt-p cxn) t)))
	 (call-next-method cxn "~a"
					   (colorize cxn (format nil "~?" fmt args))))))

(defmethod handle-accept ((cxn tempus-cxn))
  (mudlog 'info t "New connection received from ~a" (peer-addr cxn))
  (setf (state-of cxn) 'login))

(defmethod handle-flush :before ((cxn tempus-cxn))
  (when (need-prompt-p cxn)
	(when (and (eql (state-of cxn) 'playing)
			   (< (compact-level-of (account-of cxn)) 2))
	  (cxn-write cxn "~%"))
    (send-state-prompt cxn (state-of cxn))
	(when (and (eql (state-of cxn) 'playing)
			   (evenp (compact-level-of (account-of cxn))))
	  (cxn-write cxn "~%"))
	(setf (need-prompt-p cxn) nil)))

(defmethod handle-close ((cxn tempus-cxn))
  (cond
	((null (account-of cxn))
	 (mudlog 'info t "Closing connection without account"))
	((and (eql (state-of cxn) 'playing) (actor-of cxn))
	 (mudlog 'notice t "~a has lost link; ~a logged out"
			 (name-of (actor-of cxn))
			 (name-of (account-of cxn)))
	 (act (actor-of cxn) :place-emit "$n has lost $s link.")
	 (setf (link-of (actor-of cxn)) nil))
	(t
	 (mudlog 'notice t "~a logged out" (name-of (account-of cxn))))))

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
            (cxn-commands (link-of actor))))
    ;; return the first command string
    (string-left-trim '(#\\) (pop (cxn-commands (link-of actor))))))

(defun expand-aliases (actor str)
  (cond
    ((eql (char str 0) #\\)
     ;; Backslashes reference a literal command
     (string-left-trim '(#\\) str))
    ((not (typep actor 'player))
     ;; Mobiles don't have aliases
     str)
    (t
     ;; Find any alias and apply it
     (with-words str (cmd &rest args)
       (let ((alias (assoc cmd (command-aliases-of actor) :test #'string-equal)))
         (if alias
             (apply-player-alias actor alias args)
             str))))))

(defun player-pathname (idnum)
  (make-pathname :name (princ-to-string idnum)
				 :type "dat"
				 :defaults
				 (merge-pathnames (format nil "lib/players/character/~d/"
										  (mod idnum 10))
								  (asdf:component-pathname
								   (asdf:find-system "tempus")))))

(defun mail-pathname (idnum)
  (make-pathname :name (princ-to-string idnum)
				 :type "mail"
				 :defaults
				 (merge-pathnames (format nil "lib/players/mail/~d/"
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

(defun player-to-game (player)
  (mudlog 'notice t "~a entering game as ~a"
		  (name-of (account-of (link-of player)))
		  (name-of player))

  (char-to-room player
                (or (real-room (or (load-room-of player)
                                   (home-room-of player)
                                   3001))
                    (real-room 3001)))
  (setf (load-room-of player) nil)
#+nil  (dolist (equip (load-equipment (idnum-of player)))
	(item-to-actor equip player))
  (unless (plusp (hitp-of player))
	(setf (hitp-of player) 1))
  (act player :all-emit "$n enter$% the game.")
  (push player *characters*)
#+nil  (display-contents (place player) player (brief-p (prefs-of player)))
  (when (probe-file (mail-pathname (idnum-of player)))
	(send-to-char player "You have new mail.~%"))
  (save-player-to-xml player)
  (setf (state-of (link-of player)) 'playing))

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
      (setf (state-of cxn) 'disconnecting))
     ((string-equal line "new")
      (setf (state-of cxn) 'new-account))
     ((account-exists line)
      (setf (account-of cxn) (load-account line))
      (setf (state-of cxn) 'authenticate))
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
     ((not (check-password (account-of cxn) line))
      (mudlog 'warning t "PASSWORD: account ~d (~a) failed to authenticate."
              (idnum-of (account-of cxn))
              (name-of (account-of cxn)))
      (cxn-write cxn "~%Invalid password.~%~%")
      (setf (state-of cxn) 'login))
     ((players-of (account-of cxn))
      (account-login (account-of cxn))
      (setf (state-of cxn) 'main-menu))
     (t
      (account-login (account-of cxn))
      (setf (state-of cxn) 'new-player-name)))))

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
      (setf (state-of cxn) 'login))
     ((account-exists line)
      (cxn-write cxn "~%Sorry, that account name is already taken.~%"))
     ((validate-name line)
      (setf (mode-data-of cxn) line)
      (setf (state-of cxn) 'new-account-verify))
     (t
      nil))))

(define-connection-state new-account-verify
  (prompt (cxn)
    (cxn-write cxn "~%~%Are you sure you want your account name to be '~a' (Y/N)? "))
  (input (cxn line)
   (case (char-downcase (char line 0))
     (#\y
      (setf (account-of cxn)
            (make-instance 'account :name (mode-data-of cxn)))
      (setf (idnum-of (account-of cxn))
            (1+ (max-account-id)))
      (save-account (account-of cxn))
      (setf (state-of cxn) 'new-account-ansi))
     (#\n
      (setf (state-of cxn) 'new-account))
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
        (setf (ansi-level-of (account-of cxn)) pos)
        (save-account (account-of cxn))
        (setf (state-of cxn) 'new-account-compact))
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
        (setf (compact-level-of (account-of cxn)) pos)
        (save-account (account-of cxn))
        (setf (state-of cxn) 'new-account-email))
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
   (setf (email-of (account-of cxn)) line)
   (save-account (account-of cxn))
   (setf (state-of cxn) 'new-account-password)))

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
   (setf (password-of (account-of cxn)) line)
   (save-account (account-of cxn))
   (setf (state-of cxn) 'verify-password)))

(define-connection-state verify-password
  (prompt (cxn)
    ;; This awkward wording due to lame "assword:" search in tintin instead
    ;; of actually implementing one facet of telnet protocol
    (cxn-write cxn
               "        Enter it again to verify your password:"))
  (input (cxn line)
   (cond
     ((check-password (account-of cxn) line)
      (setf (state-of cxn) 'main-menu))
     (t
      (cxn-write cxn "~%The passwords did not match!  Try again!~%")
      (setf (password-of (account-of cxn)) nil)
      (setf (state-of cxn) 'new-account-password)))))

(define-connection-state new-player-name
  (menu (cxn)
    (cxn-write cxn "&@")
	(send-section-header cxn "new character"))
  (prompt (cxn)
    (cxn-write cxn "Enter the true name you wish for this character: "))
  (input (cxn line)
   (cond
     ((string= line "")
      (setf (state-of cxn) 'main-menu))
     ((player-name-exists line)
      (cxn-write cxn "~%Sorry, that name is already taken.~%"))
     ((validate-name line)
      (setf (actor-of cxn)
            (make-instance 'player
                           :idnum (1+ (max-player-id))
                           :name (string-capitalize line)))
      (setf (state-of cxn) 'new-player-race))
     (t
      (cxn-write cxn "~%You have entered an invalid name.  Try again.~%")))))

(define-connection-state new-player-race
  (menu (cxn)
   (cxn-write cxn "&@")
   (send-section-header cxn "race")
   (cxn-write cxn "
             Human                       Elf
             Dwarf                       Halfling
             Pixie                       Sidhe

"))
  (prompt (cxn)
    (cxn-write cxn "Enter the race you want for this character: "))
  (input (cxn line)
    (cond
      ((or (string= line "")
           (null (race-by-name line)))
       (cxn-write cxn "~%You need to enter one of the listed races.~%~%"))
      (t
       (setf (race-of (actor-of cxn)) (idnum-of (race-by-name line)))
       (setf (state-of cxn) 'new-player-sex)))))

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
          (setf (sex-of (actor-of cxn)) 'male)
          (setf (sex-of (actor-of cxn)) 'female))
      (setf (state-of cxn) 'new-player-eyes)))))

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
      (setf (state-of cxn) 'wait-for-menu))
     ((check-password (account-of cxn) line)
      (setf (state-of cxn) 'set-password-new))
     (t
      (cxn-write cxn "~%Wrong password!  Password change cancelled.~%")
      (setf (state-of cxn) 'wait-for-menu)))))

(define-connection-state set-password-new
  (prompt (cxn)
   (cxn-write cxn "~%~%     Enter your new password: "))
  (input (cxn line)
   (cond
     ((string= line "")
      (cxn-write cxn "~%You may not have a blank password.~%~%"))
     (t
      (cxn-write cxn "~%~%Your password has been set.~%")
      (setf (password-of (account-of cxn)) line)
      (setf (state-of cxn) 'wait-for-menu)))))

(define-connection-state wait-for-menu
  (prompt (cxn)
    (cxn-write cxn "~%Press RETURN to return to the main menu.~%"))
  (input (cxn line)
   (setf (state-of cxn) 'main-menu)))

(define-connection-state main-menu
  (menu (cxn)
   (cxn-write cxn "&@")
   (send-section-header cxn "")
   (send-section-header cxn "main menu")
   (send-section-header cxn "")
   (loop for player in (players-of (account-of cxn))
      for idx from 1 do
      (cxn-write cxn " &b[&y~2,' d&b]&n  ~10a ~a~%"
                 idx
                 (name-of player)
                 (multiple-value-bind (ms second minute hour day month year dow)
                     (decode-timestamp (login-time-of player))
                   (declare (ignorable ms second minute hour dow))
                   (format nil "~d/~2,'0d/~2,'0d" year month day))))
   (cxn-write cxn "~%~5tPast bank: ~d~50tFuture bank: ~d~%"
              (past-bank-of (account-of cxn))
              (future-bank-of (account-of cxn)))
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
      (setf (state-of cxn) 'disconnecting))
     ((char-equal (char line 0) #\c)
      (setf (state-of cxn) 'new-player-name))
     ((char-equal (char line 0) #\p)
      (setf (state-of cxn) 'set-password-auth))
     ((every #'digit-char-p line)
      (let* ((player (nth (1- (parse-integer line)) (players-of (account-of cxn))))
             (prev-actor (and player (player-in-world (idnum-of player)))))
        (cond
          ((null player)
           (cxn-write cxn "That character selection does not exist!~%~%"))
          ((null prev-actor)
           (setf (actor-of cxn) (load-player-from-xml (idnum-of player)))
           (setf (login-time-of player) (now))
           (execute (:update 'players :set
                             'login-time (login-time-of player)
                             :where (:= 'idnum (idnum-of player))))
           (setf (link-of (actor-of cxn)) cxn)
           (player-to-game (actor-of cxn)))
          ((link-of prev-actor)
           (send-to-char prev-actor "You have logged on from another location!~%")
           (setf (actor-of (link-of prev-actor)) nil)
           (setf (state-of (link-of prev-actor)) 'disconnecting)
           (setf (cxn-connected (link-of prev-actor)) nil)
           (setf (link-of prev-actor) cxn)
           (setf (actor-of cxn) prev-actor)
           (setf (state-of cxn) 'playing)
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
   (player-to-game (actor-of cxn))))

(define-connection-state playing
  (prompt (cxn)
   (cond
#+nil     ((and (action-of (actor-of cxn))
           (send-action-prompt (action-of (actor-of cxn))))
      ;; if send-action-prompt returns t, we don't need to send the
      ;; standard prompt
      nil)
     ((not (or (pref-flagged (actor-of cxn) +pref-disphp+)
               (pref-flagged (actor-of cxn) +pref-dispmana+)
               (pref-flagged (actor-of cxn) +pref-dispmove+)))
      (cxn-write cxn "&W>&n "))
     (t
      (cxn-write cxn "&W< ~@[&G~a&YH ~]~@[&M~a&YM ~]~@[&C~a&YV ~]&W>&n "
                 (when (pref-flagged (actor-of cxn) +pref-disphp+)
                   (hitp-of (actor-of cxn)))
                 (when (pref-flagged (actor-of cxn) +pref-dispmana+)
                   (mana-of (actor-of cxn)))
                 (when (pref-flagged (actor-of cxn) +pref-dispmove+)
                   (move-of (actor-of cxn)))))))
  (input (cxn line)
;;   (unless (and (action (actor-of cxn))
;;                (interpret (action (actor-of cxn)) line))
     (let ((trimmed-line (string-left-trim '(#\/ #\space #\tab) line)))
       (when (plusp (length trimmed-line))
         (interpret-command (actor-of cxn)
                            (expand-aliases (actor-of cxn)
                                            trimmed-line))))))
;;)

(defun send-menu (cxn state)
  (send-state-menu cxn state))

(defun send-prompt (cxn)
  (send-state-prompt cxn (state-of cxn)))

(defun cxn-do-command (cxn line)
  (restart-case
      (handle-state-input cxn (state-of cxn) line)
   (continue () nil)))

(defun cxn-handle-commands ()
  (dolist (cxn *cxns*)
	(when (cxn-commands cxn)
	  (setf (need-prompt-p cxn) t)
	  (let ((cmd (pop (cxn-commands cxn))))
		(handler-bind ((error (lambda (str)
                               (cxn-write cxn "You become mildly queasy as reality distorts momentarily.~%")
                               (errlog "System error: ~a" str)
                               (invoke-restart 'continue))))
            (cxn-do-command cxn cmd))))))

