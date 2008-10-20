(in-package #:tempus)

(macrolet ((define-toggle-command (command flags pref on-message off-message)
             `(defcommand (ch ,command) ,(cons :player (cons :config flags))
                (setf (bitp (prefs-of ch) ,pref)
                      (not (bitp (prefs-of ch) ,pref)))
                    (send-to-char ch "~a~%"
                                  (if (bitp (prefs-of ch) ,pref)
                                      ,on-message ,off-message)))))
  (define-toggle-command "nohassle" (:immortal) +pref-nohassle+
                         "Nohassle enabled."
                         "Nohassle disabled.")
  (define-toggle-command "brief" () +pref-brief+
                         "Brief mode on."
                         "Brief mode off.")
  (define-toggle-command "notell" (:immortal) +pref-notell+
                         "You are now deaf to tells."
                         "You can now hear tells.")
  (define-toggle-command "noauction" () +pref-noauct+
                         "You are now deaf to auctions."
                         "You can now hear auctions.")
  (define-toggle-command "noshout" (:immortal) +pref-deaf+
                         "You are now deaf to shouts."
                         "You can now hear shouts.")
  (define-toggle-command "nogossip" () +pref-nogoss+
                         "You are now deaf to gossip."
                         "You can now hear gossip.")
  (define-toggle-command "nogratz" () +pref-nogratz+
                         "You are now deaf to the congratulation messages."
                         "You can now hear the congratulation messages.")
  (define-toggle-command "nowiz" (:wizard) +pref-nowiz+
                         "You are now deaf to the wiz channel."
                         "You can now hear the wiz channel.")
  (define-toggle-command "roomflags" (:immortal) +pref-roomflags+
                         "You will now see the room flags."
                         "You will no longer see the room flags.")
  (define-toggle-command "holylight" (:immortal) +pref-holylight+
                         "Holylight mode on."
                         "Holylight mode off.")
  (define-toggle-command "autoexits" () +pref-autoexit+
                         "Autoexits enabled."
                         "Autoexits disabled.")
  (define-toggle-command "nosing" () +pref-nomusic+
                         "You are now deaf to the music of your peers."
                         "You will now hear the music of your peers.")
  (define-toggle-command "nospew" () +pref-nospew+
                         "You will no longer be subjected to the spewings of your peers."
                         "You will now be subjected to the spewings of your peers.")
  (define-toggle-command "gagmiss" () +pref-gagmiss+
                         "You will no longer see the misses in your fights."
                         "You will now see all misses in your fights.")
  (define-toggle-command "autopage" () +pref-autopage+
                         "You will now receive a bell tone if someone tells to you."
                         "You will no longer be beeped if someone tells to you.")
  (define-toggle-command "noclan" () +pref-noclansay+
                         "You have closed yourself to clan communication."
                         "You have opened yourself to clan communication.")
  (define-toggle-command "debug" (:immortal) +pref-debug+
                         "You are now debugging fights."
                         "You stop debugging fights.")
  (define-toggle-command "nonewbie" () +pref-newbie-helper+
                         "You are now monitoring the newbies."
                         "You are now ignoring the newbies")
  (define-toggle-command "autodiagnose" () +pref-auto-diagnose+
                         "You will now automatically be aware of your opponent's condition."
                         "You have disabled auto-diagnose.")
  (define-toggle-command "nodream" () +pref-nodream+
                         "You will no longer hear dreams."
                         "You will now hear the dreams of other players while you sleep.")
  (define-toggle-command "noproject" () +pref-noproject+
                         "You will now ignore the projections of other remorts."
                         "You will now be aware of the projections of other remorts.")
  (define-toggle-command "notrailers" () +pref-notrailers+
                         "You will now ignore affect trailers on characters."
                         "You will now see affect trailers on characters.")
  (define-toggle-command "nowho" () +pref-nowho+
                         "You will no longer be shown on the who list."
                         "You will now be shown on the who list.")
  (define-toggle-command "noholler" () +pref-noholler+
                         "You now ignore the hollering."
                         "You now hear the hollering."))


(defcommand (ch "save") ()
  (save-player-to-xml ch)
  (send-to-char ch "Saved.~%"))

(defcommand (ch "quit") ()
  (char-from-room ch)
  (setf *characters* (delete ch *characters*))
  (save-player-to-xml ch)
  (setf (state-of (link-of ch)) 'main-menu))

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
                 (can-see-creature ch follower))
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
