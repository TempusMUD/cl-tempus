(in-package #:tempus)

(macrolet ((define-toggle-command (command flags pref on-message off-message)
             `(defcommand (ch ,command) ,(cons :player flags)
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
  (define-toggle-command "" () +pref-debug+
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
                         "You will now see affect trailers on characters."))


(defcommand (ch "quit") ()
  (char-from-room ch)
  (setf *characters* (delete ch *characters*))
  (setf (state-of (link-of ch)) 'main-menu))