(in-package #:tempus)

(defmacro define-toggle-command (command flags pref on-message off-message)
  `(defcommand (ch ,command) ,(cons :player (cons :config flags))
     (setf (bitp (prefs-of ch) ,pref)
           (not (bitp (prefs-of ch) ,pref)))
     (send-to-char ch "~a~%"
                   (if (bitp (prefs-of ch) ,pref)
                       ,on-message ,off-message))))

(define-toggle-command "autodiagnose" () +pref-auto-diagnose+
                       "You will now automatically be aware of your opponent's condition."
                       "You have disabled auto-diagnose.")
(define-toggle-command "autoexits" () +pref-autoexit+
                       "Autoexits enabled."
                       "Autoexits disabled.")
(define-toggle-command "autopage" () +pref-autopage+
                       "You will now receive a bell tone if someone tells to you."
                       "You will no longer be beeped if someone tells to you.")
(define-toggle-command "autoprompt" () +pref-autoprompt+
                       "Your prompt will now be redrawn after every message you receive."
                       "Your prompt will now be displayed only after carriage returns.")
(define-toggle-command "brief" () +pref-brief+
                       "Brief mode on."
                       "Brief mode off.")
(define-toggle-command "debug" (:immortal) +pref-debug+
                       "You are now debugging fights."
                       "You stop debugging fights.")
(define-toggle-command "gagmiss" () +pref-gagmiss+
                       "You will no longer see the misses in your fights."
                       "You will now see all misses in your fights.")
(define-toggle-command "holylight" (:immortal) +pref-holylight+
                       "Holylight mode on."
                       "Holylight mode off.")
(define-toggle-command "nasty" () +pref-nasty+
                       "You are now subjected to the nastiness of your peers."
                       "You are now protected from the nastiness of your peers.")
(define-toggle-command "noaffects" () +pref-noaffects+
                       "You will no longer see your affections on the score page."
                       "You will now see all affections on the score page.")
(define-toggle-command "noauction" () +pref-noauct+
                       "You are now deaf to auctions."
                       "You can now hear auctions.")
(define-toggle-command "noclansay" () +pref-noclansay+
                       "You have closed yourself to clan communication."
                       "You have opened yourself to clan communication.")
(define-toggle-command "nodream" () +pref-nodream+
                       "You will no longer hear dreams."
                       "You will now hear the dreams of other players while you sleep.")
(define-toggle-command "nogecho" () +pref-nogecho+
                       "You open yourself to the echoing thoughts of the gods."
                       "You close your mind to the echoing thoughts of the gods.")
(define-toggle-command "nogossip" () +pref-nogoss+
                       "You are now deaf to gossip."
                       "You can now hear gossip.")
(define-toggle-command "nograts" () +pref-nogratz+
                       "You are now deaf to the congratulation messages."
                       "You can now hear the congratulation messages.")
(define-toggle-command "noguildsay" () +pref-noguildsay+
                       "You are now deaf to the rumors within your guild."
                       "You can now hear the rumors within your guild.")
(define-toggle-command "nohaggle" () +pref-nohaggle+
                       "You are now deaf to the haggling of your peers."
                       "You can now hear the haggling of your peers.")
(define-toggle-command "nohassle" (:immortal) +pref-nohassle+
                       "Nohassle enabled."
                       "Nohassle disabled.")
(define-toggle-command "noholler" () +pref-noholler+
                       "You now ignore the hollering."
                       "You now hear the hollering.")
(define-toggle-command "noimmchat" (:immortal) +pref-noimmchat+
                       "You are now closed off from the immchat channel."
                       "You are now on the immchat channel.")
(define-toggle-command "nonewbie" () +pref-newbie-helper+
                       "You are now monitoring the newbies."
                       "You are now ignoring the newbies")
(define-toggle-command "nopetition" (:immortal) +pref-nopetition+
                       "You are now deaf to the petitions of mortals."
                       "You can now hear the petitions of mortals.")
(define-toggle-command "noplug" () +pref-noplug+
                       "You no longer hear criers plug tempus."
                       "You can now hear criers plug tempus.")
(define-toggle-command "noproject" () +pref-noproject+
                       "You will now ignore the projections of other remorts."
                       "You will now be aware of the projections of other remorts.")
(define-toggle-command "noshout" (:immortal) +pref-deaf+
                       "You are now deaf to shouts."
                       "You can now hear shouts.")
(define-toggle-command "nosing" () +pref-nomusic+
                       "You are now deaf to the music of your peers."
                       "You will now hear the music of your peers.")
(define-toggle-command "nospew" () +pref-nospew+
                       "You will no longer be subjected to the spewings of your peers."
                       "You will now be subjected to the spewings of your peers.")
(define-toggle-command "notell" (:immortal) +pref-notell+
                       "You are now deaf to tells."
                       "You can now hear tells.")
(define-toggle-command "notrailers" () +pref-notrailers+
                       "You will now ignore affect trailers on characters."
                       "You will now see affect trailers on characters.")
(define-toggle-command "nowho" () +pref-nowho+
                       "You will no longer be shown on the who list."
                       "You will now be shown on the who list.")
(define-toggle-command "nowiz" (:wizard) +pref-nowiz+
                       "You are now deaf to the wiz channel."
                       "You can now hear the wiz channel.")
(define-toggle-command "roomflags" (:immortal) +pref-roomflags+
                       "You will now see the room flags."
                       "You will no longer see the room flags.")

(defcommand (ch "toggles") ()
  (send-to-char ch "
-- DISPLAY -------------------------------------------------------------------
Display Hit Pts: ~a       Display Mana: ~a       Display Move: ~a
       Autoexit: ~a       Autodiagnose: ~a         Autoprompt: ~a
     Brief Mode: ~a         See misses: ~a    Compact Display: ~a
  Terminal Size: ~a     Notrailers: ~a        Color Level: ~a

-- CHANNELS ------------------------------------------------------------------
       Autopage: ~a     Newbie Helper?: ~a        Projections: ~a
 Gossip Channel: ~a    Auction Channel: ~a      Grats Channel: ~a
   Spew Channel: ~a      Music Channel: ~a      Dream Channel: ~a
  Guild Channel: ~a       Clan Channel: ~a     Haggle Channel: ~a

-- GAMEPLAY ------------------------------------------------------------------
      Autosplit: ~a           Autoloot: ~a         Wimp Level: ~a
           Deaf: ~a             NoTell: ~a           On Quest: ~a
   Show Affects: ~a          Clan hide: ~a          Clan mail: ~a
      Anonymous: ~a            PKILLER: ~a
"
                (if (pref-flagged ch +pref-disphp+) " ON" "OFF")
                (if (pref-flagged ch +pref-dispmana+) " ON" "OFF")
                (if (pref-flagged ch +pref-dispmove+) " ON" "OFF")
                (if (pref-flagged ch +pref-autoexit+) " ON" "OFF")
                (if (pref-flagged ch +pref-auto-diagnose+) " ON" "OFF")
                (if (pref-flagged ch +pref-autoprompt+) "YES" " NO")
                (if (pref-flagged ch +pref-brief+) " ON" "OFF")
                (if (pref-flagged ch +pref-gagmiss+) "YES" " NO")
                (nth (compact-level-of (account-of ch)) +compact-levels+)
                (format nil "~dx~d~7t"
                        (term-height-of (account-of ch))
                        (term-width-of (account-of ch)))
                (if (pref-flagged ch +pref-notrailers+) " ON" "OFF")
                (nth (ansi-level-of (account-of ch)) +ansi-levels+)
                (if (pref-flagged ch +pref-autopage+) " ON" "OFF")
                (if (pref-flagged ch +pref-newbie-helper+) "YES" " NO")
                (if (not (pref-flagged ch +pref-noproject+)) " ON" "OFF")
                (if (not (pref-flagged ch +pref-nogoss+)) " ON" "OFF")
                (if (not (pref-flagged ch +pref-noauct+)) " ON" "OFF")
                (if (not (pref-flagged ch +pref-nogratz+)) " ON" "OFF")
                (if (not (pref-flagged ch +pref-nospew+)) " ON" "OFF")
                (if (not (pref-flagged ch +pref-nomusic+)) " ON" "OFF")
                (if (not (pref-flagged ch +pref-nodream+)) " ON" "OFF")
                (if (not (pref-flagged ch +pref-noguildsay+)) " ON" "OFF")
                (if (not (pref-flagged ch +pref-noclansay+)) " ON" "OFF")
                (if (not (pref-flagged ch +pref-nohaggle+)) " ON" "OFF")
                (if (pref-flagged ch +pref-autosplit+) " ON" "OFF")
                (if (pref-flagged ch +pref-autoloot+) " ON" "OFF")
                (format nil "~[OFF~;~:*~3d~]" (wimp-level-of ch))
                (if (pref-flagged ch +pref-deaf+) "YES" " NO")
                (if (pref-flagged ch +pref-notell+) " ON" "OFF")
                (if (zerop (quest-id-of ch)) " NO" "YES")
                (if (not (pref-flagged ch +pref-noaffects+)) "YES" " NO")
                (if (pref-flagged ch +pref-clan-hide+) "YES" " NO")
                (if (char-receives-clanmail ch) "YES" " NO")
                (if (pref-flagged ch +pref-anonymous+) "YES" " NO")
                (if (pref-flagged ch +pref-pkiller+) "YES" " NO")))

(defcommand (ch "halt") (:immortal)
  (setf (plr-bits-of ch) (logxor (plr-bits-of ch) +plr-halt+))
  (send-to-char ch
                (if (plr-flagged ch +plr-halt+)
                    "You are now in halt mode.~%"
                    "You are no longer halted.~%")))

(defcommand (ch "mortalize") (:immortal)
  (when (fighting-of ch)
    (send-to-char ch "You can't do this while fighting.~%")
    (return))

  (setf (plr-bits-of ch) (logxor (plr-bits-of ch) +plr-mortalized+))
  (mlog (format nil "(GC): ~a has ~amortalized at ~d"
                (name-of ch)
                (if (plr-flagged ch +plr-mortalized+) "" "im")
                (number-of (in-room-of ch)))
        :level (invis-level-of ch)
        :write-to-file t)

  (send-to-char ch
                (if (plr-flagged ch +plr-mortalized+)
                    "Other gods may now kill you...  if you don't watch it.~%"
                    "You resume your immortal status.~%"))
  (when (plr-flagged ch +plr-mortalized+)
    (setf (max-hitp-of ch) (+ (* (level-of ch) 100)
                              (dice (level-of ch) (con-of ch))))
    (setf (max-mana-of ch) (+ (* (level-of ch) 100)
                              (dice (level-of ch) (wis-of ch))))
    (setf (max-move-of ch) (* (level-of ch) 100))
    (setf (hitp-of ch) (max-hitp-of ch))
    (setf (mana-of ch) (max-mana-of ch))
    (setf (move-of ch) (max-move-of ch))))

(defcommand (ch "display") ()
  (send-to-char ch "Usage: display { H | M | V | A | T | all | normal | none~:[~; | vnums~] }~%"
                (immortal-level-p ch)))

(defcommand (ch "display" "vnums") (:immortal)
  (setf (bitp (prefs-of ch) +pref-disp-vnums+)
        (not (bitp (prefs-of ch) +pref-disp-vnums+)))
  (send-to-char ch
                (if (pref-flagged ch +pref-disp-vnums+)
                    "You will now see vnums on mobs and object ldescs.~%"
                    "You will no longer see vnums in ldescs.~%")))

(defun set-display-prefs (ch hp mana move align time)
  (setf (bitp (prefs-of ch) +pref-disphp+) hp)
  (setf (bitp (prefs-of ch) +pref-dispmana+) mana)
  (setf (bitp (prefs-of ch) +pref-dispmove+) move)
  (setf (bitp (prefs-of ch) +pref-dispalign+) align)
  (setf (bitp (prefs-of ch) +pref-disptime+) time))

(defcommand (ch "display" "on") ()
  (set-display-prefs ch t t t nil nil)
  (send-to-char ch "You got it.~%"))

(defcommand (ch "display" "normal") ()
  (set-display-prefs ch t t t nil nil)
  (send-to-char ch "You got it.~%"))

(defcommand (ch "display" "all") ()
  (set-display-prefs ch t t t t t)
  (send-to-char ch "You got it.~%"))

(defcommand (ch "display" options) ()
  (set-display-prefs ch
                     (find #\h options :test #'char-equal)
                     (find #\m options :test #'char-equal)
                     (find #\v options :test #'char-equal)
                     (find #\a options :test #'char-equal)
                     (find #\t options :test #'char-equal))
  (send-to-char ch "You got it.~%"))

(defcommand (ch "save") ()
  (save-player-to-xml ch)
  (send-to-char ch "Saved.~%"))

(defcommand (ch "quit") ()
  (slog "~a has departed from the known multiverse" (name-of ch))
  (act ch
       :subject-emit "Goodbye.  We will be awaiting your return."
       :place-emit "$n steps out of the universe.")
  (setf (load-room-of ch) (number-of (in-room-of ch)))
  (save-player-to-xml ch)
  (extract-creature ch 'main-menu))

(defcommand (ch "title") ()
  (setf (title-of ch) nil)
  (send-to-char ch "Okay, you're now ~a.~%" (name-of ch)))

(defcommand (ch "title" title) ()
  (cond
    ((not (typep ch 'player))
     (send-to-char ch "Mobs don't have titles.~%"))
    ((scan #/[()]/ title)
     (send-to-char ch "Titles can't contain the ( or ) characters.~%"))
    (t
     (setf (title-of ch) (concatenate 'string " " title))
     (send-to-char ch "Okay, you're now ~a~a.~%" (name-of ch) (title-of ch)))))

(defun perform-feedback (ch kind msg)
  (with-open-file (ouf (tempus-path (format nil "lib/misc/~as" kind))
                   :direction :output
                   :if-exists :append
                   :if-does-not-exist :create)
    (format ouf "~8a (~a) [~5d] ~a~%"
            (name-of ch)
            (format-timestring nil (now)
                               :format '(:short-month #\space (:day 2)))
            (number-of (in-room-of ch))
            msg))
  (send-to-char ch "Your ~a has been recorded.  Thanks!~%" kind))

(macrolet ((define-feedback-command (str)
             `(progn
                (defcommand (ch ,str) ()
                  (send-to-char ch "You had ~a ~a to submit?~%"
                                (a-or-an ,str)
                                ,str))
                (defcommand (ch ,str msg) ()
                  (perform-feedback ch ,str msg)))))
  (define-feedback-command "idea")
  (define-feedback-command "bug")
  (define-feedback-command "typo"))

(defun find-visible-group-members (ch)
  "Returns a list containing the group members of CH.  If CH is not in a group, returns NIL."
  (when (aff-flagged ch +aff-group+)
    (loop for follower in (if (master-of ch)
                              (cons (master-of ch)
                                    (followers-of (master-of ch)))
                              (followers-of ch))
       when (and (aff-flagged follower +aff-group+)
                 (not (eql follower ch))
                 (eql (in-room-of follower) (in-room-of ch))
                 (is-visible-to follower ch))
       collect follower)))

(defun perform-split (ch amount mode)
  (cond
    ((<= amount 0)
     (send-to-char ch "Sorry, you can't do that.~%"))
    ((and (eql mode :cash) (> amount (cash-of ch)))
     (send-to-char ch "You don't seem to have that many credits.~%"))
    ((and (eql mode :gold) (> amount (gold-of ch)))
     (send-to-char ch "You don't seem to have that much gold.~%"))
    ((not (aff-flagged ch +aff-group+))
     (send-to-char ch "You aren't in a group.~%"))
    (t
     (let ((members (find-visible-group-members ch)))
       (cond
         (members
          (let* ((member-count (length members))
                 (share (floor amount (1+ member-count))))
            (if (eql mode :gold)
                (decf (gold-of ch) (* share member-count))
                (decf (cash-of ch) (* share member-count)))
            (dolist (member members)
              (if (eql mode :gold)
                  (incf (gold-of member) share)
                  (incf (cash-of member) share))
              (act ch :target member
                   :target-emit (format nil "$n splits ~d ~a; you receive ~d."
                                        amount
                                        (if (eql mode :gold) "coins" "credits")
                                        share)))
            (send-to-char ch "You split ~d ~a among ~d member~:*~p -- ~d each.~%"
                          amount
                          (if (eql mode :gold) "coins" "credits")
                          (1+ member-count)
                          share)))
         (t
          (send-to-char ch "You don't see anyone to split it with.~%")))))))

(defcommand (ch "split") (:resting)
  (send-to-char ch "You may split an amount of gold or cash.~%"))

(defcommand (ch "split" amount) (:resting)
  (if (notevery #'digit-char-p amount)
      (send-to-char ch "That's not a proper amount.~%")
      (perform-split ch (parse-integer amount) :gold)))

(defcommand (ch "split" amount "gold") (:resting)
  (if (notevery #'digit-char-p amount)
      (send-to-char ch "That's not a proper amount.~%")
      (perform-split ch (parse-integer amount) :gold)))

(defcommand (ch "split" amount "credits") (:resting)
  (if (notevery #'digit-char-p amount)
      (send-to-char ch "That's not a proper amount.~%")
      (perform-split ch (parse-integer amount) :cash)))

(defcommand (ch "more") (:important)
  (cond
    ((null (link-of ch))
     (send-to-char ch "You don't get more.~%"))
    ((null (page-buf-of (link-of ch)))
     (send-to-char ch "You weren't in the middle of reading anything.~%"))
    (t
     (send-page (link-of ch)))))

(defcommand (ch "color") ()
  (send-to-char ch "Your current color level is ~a.~%"
                (nth (ansi-level-of (account-of ch)) +ansi-levels+)))

(defcommand (ch "color" level-str) ()
  (let ((level (position level-str +ansi-levels+ :test 'string-abbrev)))
    (cond
      (level
       (setf (ansi-level-of (account-of ch)) level)
       (save-account (account-of ch))
       (send-to-char ch "Your color is now &Y~a&n.~%"
                (nth (ansi-level-of (account-of ch)) +ansi-levels+)))
      (t
       (send-to-char ch  "Usage: color { ~{~a~^ | ~} }~%" +ansi-levels+)))))

(defcommand (ch "compact") ()
  (send-to-char ch "Your current compact level is ~a.~%"
                (nth (compact-level-of (account-of ch)) +compact-levels+)))

(defcommand (ch "compact" level-str) ()
  (let ((level (position level-str +compact-levels+ :test 'string-abbrev)))
    (cond
      (level
       (setf (compact-level-of (account-of ch)) level)
       (save-account (account-of ch))
       (send-to-char ch "Your &rcompact setting&n is now &Y~a&n.~%"
                (nth (compact-level-of (account-of ch)) +compact-levels+)))
      (t
       (send-to-char ch "Usage: compact { ~{~a~^ | ~} }~%" +compact-levels+)))))
