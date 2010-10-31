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
    new-player-sex
    new-player-class
    new-player-race
    new-player-align
    new-player-stats
    new-player-desc
    main-menu
    wait-for-menu
    remort-class
    delete-character
    delete-verify
    delete-password
    describe-character
    describe-editor
    playing
    afterlife
    editing
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
      (eql state 'set-password-new)
      (eql state 'delete-password)))

(defmethod (setf state-of) :around (state (cxn tempus-cxn))
  ;; send telnet DO ECHO when the new state isn't a password state
  (when (password-state-p (state-of cxn))
    (cxn-write cxn "~c~c~c"
               (code-char #xff)
               (code-char #xfc)
               (code-char #x01)))
  (cond
    ((member state +valid-cxn-states+)
     (call-next-method)
     (send-state-menu cxn state)
     (setf (need-prompt-p cxn) t))
    (t
     (mudlog 'warning t "attempt to set cxn to state ~a" state)))

  ;; send telnet DONT ECHO when the new state is a password state
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
         (cxn-write-string cxn (coerce '(#\return #\newline) 'string)))
       (when (or (null (actor-of cxn))
                 (pref-flagged (actor-of cxn) +pref-autoprompt+))
         (setf (need-prompt-p cxn) t)))
     (call-next-method cxn "~a"
                       (colorize cxn (format nil "~?" fmt args))))))

(defmethod handle-accept ((cxn tempus-cxn))
  (mudlog 'info t "New connection received from ~a" (peer-addr-of cxn))
  (cxn-write cxn "~a" +greetings+)
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

(defmethod cxn-close :before ((cxn tempus-cxn) &key abort)
  (declare (ignore abort))
  (cond
    ((null (account-of cxn))
     (mudlog 'info t "Closing connection without account"))
    ((and (eql (state-of cxn) 'playing) (actor-of cxn))
     (account-logout (account-of cxn) cxn)
     (setf (link-of (actor-of cxn)) nil)
     (mudlog 'notice t "~a has lost link; ~a logged out"
             (name-of (actor-of cxn))
             (name-of (account-of cxn)))
     (when (in-room-of (actor-of cxn))
       (act (actor-of cxn) :place-emit "$n has lost $s link.")))
    (t
     (account-logout (account-of cxn) cxn)
     (mudlog 'notice t "~a logged out" (name-of (account-of cxn))))))

(defun expand-single-alias (alias args split-args)
  (format nil "\\~a"
          (string-trim '(#\space #\tab)
                       (cl-ppcre:regex-replace-all
                        #/\$([*1-9])/
                        alias
                        (lambda (target start end mstart mend rstarts rends)
                          (declare (ignore start end mstart mend rends))
                          (let ((digit (digit-char-p (char target (aref rstarts 0)))))
                            (if digit
                                (format nil "~@[ ~a~]" (nth (1- digit) split-args))
                                (format nil "~:[ ~a~;~]" (null split-args) args))))))))

(defun apply-player-alias (actor alias args)
  ;; split replacements into lines
  (let ((replacements (split-sequence #\; (second alias)))
        (split-args (split-sequence #\space args :remove-empty-subseqs t)))
    (dolist (cmd-line (reverse replacements))
      ;; push each alias expansion into the command queue of the actor
      ;; (in reverse for proper ordering)
      (push (expand-single-alias cmd-line args split-args)
            (commands-of (link-of actor))))
    ;; return the first command string
    (string-left-trim '(#\\) (pop (commands-of (link-of actor))))))

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
                 :defaults (tempus-path (format nil "lib/players/character/~d/"
                                          (mod idnum 10)))))

(defun equipment-pathname (idnum)
  (make-pathname :name (princ-to-string idnum)
                 :type "dat"
                 :defaults (tempus-path (format nil "lib/players/equipment/~d/"
                                          (mod idnum 10)))))

(defun corpse-pathname (idnum)
  (make-pathname :name (princ-to-string idnum)
                 :type "dat"
                 :defaults (tempus-path (format nil "lib/players/corpses/~d/"
                                          (mod idnum 10)))))

(defun player-to-game (player)
  (send-to-char player "&R~a&n" +welcome-message+)
  (char-to-room player
                (or (real-room (or (load-room-of player)
                                   (home-room-of player)
                                   3001))
                    (real-room 3001)))

  (show-mud-date-to-char player)
  (send-to-char player "~%")

  (setf (state-of (link-of player)) 'playing)

  (mudlog 'notice t "~a has entered the game in room #~d~@[~a~]"
          (name-of player)
          (number-of (in-room-of player))
          (when (banned-of (account-of (link-of player))) " [BANNED]"))

  (act player :place-emit "$n enter$% the game.")

  (setf (load-room-of player) 0)
  (unrent player)
  (unless (plusp (hitp-of player))
    (setf (hitp-of player) 1))
  (push player *characters*)
  (setf (gethash (idnum-of player) *character-map*) player)

  (look-at-room player (in-room-of player) nil)

  (when (has-mail (idnum-of player))
    (send-to-char player "You have new mail.~%"))
  (when (and (plusp (clan-of player))
             (null (real-clan (clan-of player))))
    (setf (clan-of player) 0)
    (send-to-char player "Your clan has been disbanded.~%"))

  ;; Remove bits
  (setf (plr-bits-of player) (logandc2 (plr-bits-of player)
                                       (logior +plr-cryo+ +plr-writing+
                                                +plr-olc+ +plr-mailing+
                                                +plr-afk+)))
  (setf (bitp (prefs-of player) +pref-worldwrite+) nil)

  (check-dyntext-updates player nil)

  (setf (login-time-of player) (now))
  (save-player-to-xml player))

(defun send-section-header (cxn str)
  (if (string= str "")
      (cxn-write cxn "&@&b~v,1,,'-a&n~%~%" 72 "")
      (cxn-write cxn "&@&b~v,1,,'-a &y~a &b~v,1,,'-a&n~%~%"
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
      (cxn-close cxn))
     ((string-equal line "new")
      (setf (state-of cxn) 'new-account))
     ((account-exists line)
      (setf (account-of cxn) (load-account line))

      (setf (state-of cxn) (if *production-mode* 'authenticate 'main-menu)))
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
      (account-login (account-of cxn) cxn)
      (setf (state-of cxn) 'main-menu))
     (t
      (account-login (account-of cxn) cxn)
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
      (setf (account-of cxn) (create-account (mode-data-of cxn)))
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
      (setf (state-of cxn) 'new-player-name))
     (t
      (cxn-write cxn "~%The passwords did not match!  Try again!~%")
      (setf (password-of (account-of cxn)) nil)
      (setf (state-of cxn) 'new-account-password)))))

(define-connection-state new-player-name
  (menu (cxn)
    (cxn-write cxn "&@")
    (send-section-header cxn "new character")
    (cxn-write cxn "
    Now that you have created your account, you probably want to create a
character to play on the mud.  This character will be your persona on the
mud, allowing you to interact with other people and things.  You may press
return at any time to cancel the creation of your character.

"))
  (prompt (cxn)
    (cxn-write cxn "Enter the name you wish for this character: "))
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
      (create-new-player (actor-of cxn) (account-of cxn))
      (setf (state-of cxn) 'new-player-sex))
     (t
      (cxn-write cxn "~%You have entered an invalid name.  Try again.~%")))))

(define-connection-state new-player-sex
  (menu (cxn)
    (cxn-write cxn "&@")
    (send-section-header cxn "sex")
    (cxn-write cxn "~%    Is your character a male or a female?~%~%"))
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
      (setf (state-of cxn) 'new-player-class)))))

(define-connection-state new-player-class
  (menu (cxn)
   (cxn-write cxn "&@")
   (send-section-header cxn "profession")
   (cxn-write cxn "
    Your character class is the special training your character has had
before embarking on the life of an adventurer.  Your class determines
most of your character's capabilities within the game.  You may type
'help <class>' for an overview of a particular class.

")
   (show-char-class-menu cxn nil))
  (prompt (cxn)
   (cxn-write cxn "             Choose your profession from the above list: "))
  (input (cxn line)
   (cond
     ((or (string= line "")
          (null (parse-player-class line)))
      (cxn-write cxn "~%You need to enter one of the listed classes.~%~%"))
     (t
      (setf (char-class-of (actor-of cxn)) (parse-player-class line))
      (setf (state-of cxn) 'new-player-race)))))

(define-connection-state new-player-race
  (menu (cxn)
   (cxn-write cxn "&@")
   (send-section-header cxn "race")
   (cxn-write cxn "
    Races on Tempus have nothing to do with coloration.  Your character's
race refers to the intelligent species that your character can be.  Each
has its own advantages and disadvantages, and some have special abilities!
You may type 'help <race>' for information on any of the available races.

")
   (show-pc-race-menu cxn))
  (prompt (cxn)
    (cxn-write cxn "Enter the race you want for this character: "))
  (input (cxn line)
    (cond
      ((or (string= line "")
           (null (parse-pc-race line)))
       (cxn-write cxn "~%You need to enter one of the listed races.~%~%"))
      (t
       (let ((race (parse-pc-race line)))
         (cond
           ((null race)
            (cxn-write cxn "That's not an allowable race!~%"))
           ((not (valid-race-p (char-class-of (actor-of cxn)) race))
            (cxn-write cxn "That race is not allowed to your profession!~%"))
           (t
            (setf (race-of (actor-of cxn)) race)
            (setf (state-of cxn) 'new-player-align))))))))

(define-connection-state new-player-align
  (menu (cxn)
    (cxn-write cxn "&@")
    (send-section-header cxn "alignment")
    (cxn-write cxn "~%~%    ALIGNMENT is a measure of your philosophies and morals.~%~%"))
  (prompt (cxn)
    (cond
      ((is-drow (actor-of cxn))
       (cxn-write cxn "The Drow race is inherently evil.  Thus you begin your life as evil.~%~%Press return to continue.~%"))
      ((is-monk (actor-of cxn))
       (cxn-write cxn "The monastic ideology requires that you remain neutral in alignment.~%Therefore you begin your life with a perfect neutrality.~%Press return to continue.~%"))
      ((or (is-knight (actor-of cxn)) (is-cleric (actor-of cxn)))
       (cxn-write cxn "Do you wish to be good or evil? "))
      ((is-bard (actor-of cxn))
       (cxn-write cxn "Do you wish to be good or neutral? "))
      (t
       (cxn-write cxn "Do you wish to be good, neutral or evil? "))))
  (input (cxn line)
    (let ((input (position line '("good" "neutral" "evil")
                           :test 'string-abbrev)))
      (cond
        ((is-drow (actor-of cxn))
         (setf (alignment-of (actor-of cxn)) -666)
         (setf (state-of cxn) 'new-player-stats))
        ((is-monk (actor-of cxn))
         (setf (alignment-of (actor-of cxn)) 0)
         (setf (state-of cxn) 'new-player-stats))
        ((null input)
         (cxn-write cxn "~%That's not one of the alignment selections.~%"))
        ((and (or (is-knight (actor-of cxn)) (is-cleric (actor-of cxn)))
              (= input 1))
         (cxn-write cxn "~%~a must align with either good or evil.~%~%"
                    (string-capitalize (aref +class-names+
                                             (char-class-of (actor-of cxn))))))
        ((and (is-bard (actor-of cxn)) (= input 2))
         (cxn-write cxn "~%Bards must be of good or neutral alignment.~%~%"))
        ((= input 0)
         (setf (alignment-of (actor-of cxn)) 777)
         (setf (state-of cxn) 'new-player-stats))
        ((= input 1)
         (setf (alignment-of (actor-of cxn)) 0)
         (setf (state-of cxn) 'new-player-stats))
        ((= input 2)
         (setf (alignment-of (actor-of cxn)) -666)
         (setf (state-of cxn) 'new-player-stats))
        (t
         (error "Can't happen here"))))))

(define-connection-state new-player-stats
  (prompt (cxn)
    (cxn-write cxn "&@")
    (roll-real-abils (actor-of cxn))
    (send-section-header cxn "character attributes")
    (cxn-write cxn "~a~%" (describe-attributes (actor-of cxn)))
    (cxn-write cxn "&cWould you like to &gREROLL&c or &gKEEP&c these attributes?&n "))
  (input (cxn line)
    (cond
      ((string-abbrev line "reroll")
       (setf (wait-of cxn) 4))
      ((string-abbrev line "keep")
       (mudlog 'info t "~a[~d] has created new character ~a[~d]"
             (name-of (account-of cxn))
             (idnum-of (account-of cxn))
             (name-of (actor-of cxn))
             (idnum-of (actor-of cxn)))
       (setf (rentcode-of (actor-of cxn)) 'new-char)
       (calculate-height-weight (actor-of cxn))
       (save-player-to-xml (actor-of cxn))
       (setf (state-of cxn) 'describe-editor)))))

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

(defun show-account-chars (cxn acct immort brief)
  (unless immort
    (if brief
        (cxn-write cxn "  # Name         Last on   Status  Mail   # Name         Last on   Status  Mail~% -- --------- ---------- --------- ----  -- --------- ---------- --------- ----~%")
        (cxn-write cxn  "&y  # Name           Lvl Gen Sex     Race     Class      Last on    Status  Mail~%&b -- -------------- --- --- --- -------- --------- ------------- --------- ----~%")))
  (loop
     for player in (players-of acct)
     for idx from 1
     do (let ((tmp-ch (load-player-from-xml (idnum-of player)))
              (real-ch (player-in-world (idnum-of player))))
          (cond
            ((null tmp-ch)
             (cxn-write cxn "&R------ BAD PROBLEMS ------  PLEASE REPORT ------[~d]&n~%"
                        (idnum-of player)))
            ((and (not immort)
                  (plr2-flagged tmp-ch +plr2-buried+))
             nil)
            (t
             (let* ((+immort-time-format+ '(:short-weekday #\, #\space
                                            (:day 2 #\0) #\space
                                            :short-month #\space
                                            :year #\space
                                            :hour #\: :min #\: :sec))
                    (+brief-time-format+ '(:year #\/ (:month 2) #\/ (:day 2)))
                    (+long-time-format+ '(:long-month #\space :day #\,
                                          #\space :year))
                    (max-name-length (if brief 8 13))
                    (truncated-name (if (> (length (name-of tmp-ch))
                                           max-name-length)
                                        (subseq (name-of tmp-ch) 0 max-name-length)
                                        (name-of tmp-ch)))
                    (name-str (if (immortal-level-p tmp-ch)
                                  (format nil "&g~:[~13a~;~8a~]&n" brief truncated-name)
                                  truncated-name))
                    (class-str (if (is-remort tmp-ch)
                                   (format nil "~a~a&n/~a~a&n"
                                           (char-class-color tmp-ch (char-class-of tmp-ch))
                                           (char-class-name (char-class-of tmp-ch))
                                           (char-class-color tmp-ch (remort-char-class-of tmp-ch))
                                           (char-class-name (remort-char-class-of tmp-ch)))
                                   (format nil "~a~9a&n"
                                           (char-class-color tmp-ch (char-class-of tmp-ch))
                                           (char-class-name (char-class-of tmp-ch)))))
                    (sex-str (case (sex-of tmp-ch)
                               (male "&bmale")
                               (female "&mfemale")
                               (t "neuter")))
                    (laston-str (format-timestring nil (login-time-of tmp-ch)
                                                   :format (cond
                                                             (immort +immort-time-format+)
                                                             (brief  +brief-time-format+)
                                                             (t      +long-time-format+))))
                    (status-str (cond
                                  ((plr-flagged tmp-ch +plr-frozen+)
                                   "&C  FROZEN!")
                                  ((and real-ch (link-of real-ch))
                                   "&g  Playing")
                                  (real-ch
                                   "&c Linkless")
                                  ((eql (desc-mode-of tmp-ch) 'afterlife)
                                   "&R     Died")
                                  (t
                                   (case (rentcode-of tmp-ch)
                                     (creating "&Y Creating")
                                     (new-char "&Y      New")
                                     (undef    "&r    UNDEF")
                                     (cryo     "&c   Cryoed")
                                     (crash    "&yCrashsave")
                                     (rented   "&m   Rented")
                                     (forced   "&yForcerent")
                                     (quit     "&m     Quit")
                                     (remorting "&YRemorting")
                                     (t        "&R REPORTME")))))
                    (mail-str (if (has-mail (idnum-of player)) "&Y Yes" "&n No ")))
               (cond
                 (immort
                  (cxn-write cxn "&y~5d &n~13a ~a&n ~a  ~2d&c(&n~2d&c)&n ~a~%"
                             player
                             name-str
                             status-str
                             laston-str
                             (level-of tmp-ch)
                             (remort-gen-of tmp-ch)
                             class-str))
                 ((and brief (eql (rentcode-of tmp-ch) 'creating))
                  (cxn-write cxn "&b[&y~2d&b] &n%~8a     &yNever  Creating&n  -- "
                             idx name-str))
                 ((eql (rentcode-of tmp-ch) 'creating)
                  (cxn-write cxn "&b[&y~2d&b] &n%~13a   &y-   -  -         -         -         Never  Creating&n  --~%"
                             idx name-str))
                 (brief
                  (cxn-write cxn "&b[&y~2d&b] &n~8a ~10a ~a ~a&n "
                             idx name-str laston-str status-str mail-str))
                 (t
                  (cxn-write cxn "&b[&y~2d&b] &n~13a ~3d ~3d  ~a  ~8a ~a ~13a ~a ~a&n~%"
                             idx name-str
                             (level-of tmp-ch) (remort-gen-of tmp-ch)
                             sex-str
                             (aref +player-races+ (race-of tmp-ch))
                             class-str laston-str status-str mail-str))))
             (when (and brief (not (logtest idx 1)))
               (cxn-write cxn "~%")))))
     finally (when (and brief (not (logtest idx 1)))
               (cxn-write cxn "~%"))))

(define-connection-state main-menu
  (menu (cxn)
   (cxn-write cxn "&@&c*&n&b-----------------------------------------------------------------------------&c*
&n&b|                                 &YT E M P U S&n                                 &b|
&c*&b-----------------------------------------------------------------------------&c*&n

")
   (show-account-chars cxn
                       (account-of cxn)
                       nil
                       (> (length (players-of (account-of cxn))) 5))
   (cxn-write cxn "~%~%~5tPast bank: ~d~50tFuture bank: ~d~%~%"
              (past-bank-of (account-of cxn))
              (future-bank-of (account-of cxn)))
   (cxn-write cxn "    &b[&yP&b] &cChange your account password     &b[&yV&b] &cView the background story~%")
   (cxn-write cxn "    &b[&yC&b]&n &cCreate a new character")
   (when (players-of (account-of cxn))
      (cxn-write cxn "           &b[&yS&b] &cShow character details~%")
      (cxn-write cxn "    &b[&yE&b] &cEdit a character's description   &b[&yD&b] &cDelete an existing character~%"))
   (cxn-write cxn "~%~%                            &b[&yL&b] &cLog out of the game&n~%"))
  (prompt (cxn)
    (cxn-write cxn "Enter your selection: "))
  (input (cxn line)
   (cond
     ((string= line "")
      (send-state-menu cxn 'main-menu))
     ((or (char-equal (char line 0) #\l) (eql (char line 0) #\0))
      (cxn-write cxn "Goodbye!~%")
      (setf (connectedp cxn) nil)
      (setf (state-of cxn) 'disconnecting)
      (cxn-close cxn))
     ((char-equal (char line 0) #\c)
      (setf (state-of cxn) 'new-player-name))
     ((char-equal (char line 0) #\p)
      (setf (state-of cxn) 'set-password-auth))
     ((char-equal (char line 0) #\e)
      (cond
        ((null (players-of (account-of cxn)))
         (cxn-write cxn "~%That isn't a valid command.~%~%"))
        ((null (rest (players-of (account-of cxn))))
         ;; only one character to delete, so skip prompt
         (setf (actor-of cxn) (load-player-from-xml
                               (idnum-of (first
                                          (players-of (account-of cxn))))))
         (setf (state-of cxn) 'describe-editor))
        (t
         ;; multiple possibilities, so go to selection prompt
         (setf (state-of cxn) 'describe-character))))
     ((char-equal (char line 0) #\d)
      (cond
        ((null (players-of (account-of cxn)))
         (cxn-write cxn "~%That isn't a valid command.~%~%"))
        ((null (rest (players-of (account-of cxn))))
         ;; only one character to delete, so skip prompt
         (setf (actor-of cxn) (load-player-from-xml
                               (idnum-of (first
                                          (players-of (account-of cxn))))))
         (setf (state-of cxn) 'delete-password))
        (t
         ;; multiple possibilities, so go to selection prompt
         (setf (state-of cxn) 'delete-character))))
     ((every #'digit-char-p line)
      (let* ((player (nth (1- (parse-integer line)) (players-of (account-of cxn))))
             (prev-actor (and player (player-in-world (idnum-of player)))))
        (cond
          ((null player)
           (cxn-write cxn "That character selection does not exist!~%~%"))
          ((null prev-actor)
           (setf (actor-of cxn) (load-player-from-xml (idnum-of player)))
           (setf (login-time-of player) (now))
           (postmodern:execute (:update 'players :set
                             'login-time (login-time-of player)
                             :where (:= 'idnum (idnum-of player))))
           (setf (link-of (actor-of cxn)) cxn)
           (setf (account-of (actor-of cxn)) (account-of cxn))
           (player-to-game (actor-of cxn)))
          ((link-of prev-actor)
           (send-to-char prev-actor "You have logged on from another location!~%")
           (setf (actor-of (link-of prev-actor)) nil)
           (setf (state-of (link-of prev-actor)) 'disconnecting)
           (setf (connectedp (link-of prev-actor)) nil)
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

(define-connection-state delete-character
  (menu (cxn)
    (cxn-write cxn "&@")
    (cxn-write cxn "&r~%                                DELETE CHARACTER~%*******************************************************************************&n~%~%")
    (loop for player-record in (players-of (account-of cxn))
       as player-idnum = (idnum-of player-record)
       as player = (load-player-from-xml player-idnum)
       as idx from 1
       do (cxn-write cxn "    &r[&y~2d&r] &y~20a ~10a ~10a ~6a ~a~%"
                     idx (name-of player)
                     (aref +player-races+ (race-of player))
                     (aref +class-names+ (char-class-of player))
                     (case (sex-of player)
                       (male "Male")
                       (female "Female")
                       (t "Neuter"))
                     (if (plusp (level-of player))
                         (format nil "lvl ~d" (level-of player))
                         " new")))
    (cxn-write cxn "&n~%"))
  (prompt (cxn)
    (cxn-write cxn "~%              &yWhich character would you like to delete:&n "))
  (input (cxn line)
    (let* ((idx (parse-integer line :junk-allowed t))
           (player-record (and idx (nth (1- idx) (players-of (account-of cxn)))))
           (idnum (and player-record (idnum-of player-record))))
      (cond
        ((zerop (length line))
         (setf (state-of cxn) 'main-menu))
        ((null idnum)
         (cxn-write cxn "~%That character selection doesn't exist.~%~%")
         (setf (state-of cxn) 'wait-for-menu))
        (t
         (setf (actor-of cxn) (gethash idnum *character-map*))
         (unless (actor-of cxn)
           (setf (actor-of cxn) (load-player-from-xml idnum)))
         (cond
           ((actor-of cxn)
            (setf (account-of (actor-of cxn)) (account-of cxn))
            (setf (state-of cxn) 'delete-password))
           (t
            (cxn-write cxn "~%That character cannot be loaded.~%~%")
            (slog "Couldn't retrieve char idnum ~d" idnum)
            (setf (state-of cxn) 'wait-for-menu))))))))

(define-connection-state delete-password
  (prompt (cxn)
    (cxn-write cxn "~%              &yTo confirm deletion of ~a, enter your account password: &n"
               (name-of (actor-of cxn))))
  (input (cxn line)
    (cond
      ((check-password (account-of cxn) line)
       (setf (state-of cxn) 'delete-confirm))
      (t
       (cxn-write cxn "~%~%              &yWrong password!  ~a will not be deleted.~%"
                  (name-of (actor-of cxn)))
       (setf (state-of cxn) 'wait-for-menu)))))

(define-connection-state delete-confirm
  (prompt (cxn)
    (cxn-write cxn "~%~%              &yType 'yes' for final confirmation: &n"))
  (input (cxn line)
    (cond
      ((string= line "yes")
       (slog "~a[~d] has deleted character ~a[~d]"
             (name-of (account-of cxn))
             (idnum-of (account-of cxn))
             (name-of (actor-of cxn))
             (idnum-of (actor-of cxn)))
       (cxn-write cxn "~%              &y~a has been deleted.&n~%~%"
                  (name-of (actor-of cxn)))
       (delete-player (actor-of cxn))
       (setf (actor-of cxn) nil)
       (setf (state-of cxn) 'wait-for-menu))
      (t
       (cxn-write cxn "~%              &yDelete cancelled.  ~a will not be deleted.~%~%"
                  (name-of (actor-of cxn)))
       (setf (actor-of cxn) nil)
       (setf (state-of cxn) 'wait-for-menu)))))

(define-connection-state describe-character
  (menu (cxn)
    (send-section-header cxn "edit character description")
    (loop for player-record in (players-of (account-of cxn))
       as player-idnum = (idnum-of player-record)
       as player = (load-player-from-xml player-idnum)
       as idx from 1
       do (cxn-write cxn "    &c[&y~2d&c] &c~20a &n~10a ~10a ~6a ~a~%"
                     idx (name-of player)
                     (aref +player-races+ (race-of player))
                     (aref +class-names+ (char-class-of player))
                     (case (sex-of player)
                       (male "Male")
                       (female "Female")
                       (t "Neuter"))
                     (if (plusp (level-of player))
                         (format nil "lvl ~d" (level-of player))
                         " new")))
    (cxn-write cxn "&n~%"))
  (prompt (cxn)
    (cxn-write cxn "~%           &yWhich character's description do you want to edit:&n "))
  (input (cxn line)
    (let* ((idx (parse-integer line :junk-allowed t))
           (player-record (and idx (nth (1- idx) (players-of (account-of cxn)))))
           (idnum (and player-record (idnum-of player-record))))
      (cond
        ((zerop (length line))
         (setf (state-of cxn) 'main-menu))
        ((null idnum)
         (cxn-write cxn "~%That character selection doesn't exist.~%~%")
         (setf (state-of cxn) 'wait-for-menu))
        (t
         (setf (actor-of cxn) (gethash idnum *character-map*))
         (unless (actor-of cxn)
           (setf (actor-of cxn) (load-player-from-xml idnum)))
         (cond
           ((actor-of cxn)
            (setf (account-of (actor-of cxn)) (account-of cxn))
            (setf (state-of cxn) 'describe-editor))
           (t
            (cxn-write cxn "~%That character cannot be loaded.~%~%")
            (slog "Couldn't retrieve char idnum ~d" idnum)
            (setf (state-of cxn) 'wait-for-menu))))))))

(define-connection-state describe-editor
  (menu (cxn)
    (send-section-header cxn "description")
    (cxn-write cxn "    Other players will usually be able to determine your general
size, as well as your race and gender, by looking at you.  What
else is noticable about your character?

")
    (start-text-editor cxn (actor-of cxn)
                       (format nil "the description of ~a"
                               (name-of (actor-of cxn)))
                       (fdesc-of (actor-of cxn))
                       (lambda (cxn ch buffer)
                         (setf (fdesc-of ch) buffer)
                         (save-player-to-xml ch)
                         (setf (state-of cxn) 'main-menu))
                       (lambda (cxn ch)
                         (declare (ignore ch))
                         (setf (state-of cxn) 'main-menu)))))

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
      (cxn-write cxn "~:[&B(debug) ~;~]&N>&n " *production-mode*))
     (t
      (cxn-write cxn "&N< ~:[&B(debug) ~;~]~@[&G~a&YH ~]~@[&M~a&YM ~]~@[&C~a&YV ~]&N>&n "
                 *production-mode*
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
                            (string-trim '(#\space)
                                         (expand-aliases (actor-of cxn)
                                                         trimmed-line)))))))

(define-connection-state editing
  (prompt (cxn)
    (cxn-write cxn "&n~3d&B] &n"
               (1+ (length (buffer-of (mode-data-of cxn))))))
  (menu (cxn)
    (editor-start (mode-data-of cxn) cxn))
  (input (cxn line)
    (editor-input (mode-data-of cxn) cxn line)))

(defun send-prompt (cxn)
  (send-state-prompt cxn (state-of cxn)))

(defun cxn-do-command (cxn line)
  (with-simple-restart (continue "Continue from signal in command handler")
    (handle-state-input cxn (state-of cxn) line)))

(defvar *break-on-error* t)

(defun handle-command (cxn)
  (when (commands-of cxn)
    (setf (need-prompt-p cxn) t)
    (let ((cmd (pop (commands-of cxn))))
      (if *break-on-error*
          (cxn-do-command cxn cmd)
          (handler-bind ((error (lambda (str)
                                  (cxn-write cxn "You become mildly queasy as reality distorts momentarily.~%")
                                  (errlog "System error: ~a" str)
                                  (invoke-restart 'continue))))
            (cxn-do-command cxn cmd))))))

(defun cxn-handle-commands ()
  (dolist (cxn *cxns*)
    (when (commands-of cxn)
      (setf (need-prompt-p cxn) t)
      (let ((cmd (pop (commands-of cxn))))
        (if *break-on-error*
            (cxn-do-command cxn cmd)
            (handler-bind ((error (lambda (str)
                                    (cxn-write cxn "You become mildly queasy as reality distorts momentarily.~%")
                                    (errlog "System error: ~a" str)
                                    (invoke-restart 'continue))))
              (cxn-do-command cxn cmd)))))))

