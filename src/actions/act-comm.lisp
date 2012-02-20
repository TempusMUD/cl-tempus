(in-package #:tempus)

(defun perform-tell (ch target message)
  (cond
    ((pref-flagged ch +pref-notell+)
     (send-to-char ch "You can't tell other people while you have notell on.~%"))
    ((and (room-flagged (in-room-of ch) +room-soundproof+)
          (not (immortal-level-p ch))
          (not (eql (in-room-of ch) (in-room-of target))))
     (send-to-char ch "The walls seem to absorb your words.~%"))
    ((and (not (is-npc target))
          (null (link-of target)))
     (act ch :target target
          :subject-emit "$E's linkless at the moment."))
    ((or (plr-flagged target +plr-writing+)
         (plr-flagged target +plr-mailing+))
     (act ch :target target
          :subject-emit "$E's writing a message right now; try again later."))
    ((or (pref-flagged target +pref-notell+)
         (plr-flagged target +plr-olc+))
     (act ch :target target :subject-emit "$E can't hear you."))
    ((and (not (can-send-tell ch target))
          (not (or (affected-by-spell ch +spell-telepathy+)
                   (affected-by-spell ch +spell-telepathy+))))
     (act ch :target target :subject-emit "Your telepathic voice cannot reach $M."))
    (t
     (act ch :target target
          :subject-emit (format nil "&rYou tell$% $N,&n '~a'"
                                (act-escape message))
          :target-emit (format nil "&r$n tells you,&n '~a'"
                               (act-escape message)))

     (unless (is-npc target)
       (when (and (plr-flagged target +plr-afk+)
                  (null (find (idnum-of ch) (afk-notifies-of target))))
         (push (idnum-of ch) (afk-notifies-of target))

         (act ch :target target
              :subject-emit (format nil "$N is away from the keyboard~:[.~;~:*: ~a~]"
                                    (afk-reason-of target))))

       (when (pref-flagged target +pref-autopage+)
         (send-to-char target "~c~c" (code-char 7) (code-char 7)))

       (unless (is-npc ch)
         (setf (last-tell-from-of target) (idnum-of ch))
         (setf (last-tell-to-of ch) (idnum-of target)))))))

(defun perform-say (ch say-cmd message)
  (let ((message (act-escape message)))
    (act ch
         :all-emit (format nil "&B$n ~a$%$a, &c'$[~a]'&n" say-cmd message))))

(defun perform-say-to (ch target message)
  (let ((message (act-escape message)))
    (act ch :target target
         :all-emit (format nil "&B$n$a say$% to $N, &c'$[~a]'&n" message))))

(defun perform-emote (ch message)
  (let ((message (act-escape message)))
    (act ch
         :subject-emit (format nil "~a ~a" (name-of ch) message)
         :place-emit (format nil "$n ~a" message))))

(defun select-say-cmd (ch message)
  (let ((len (length message)))
  (cond
    ((and (> len 3) (string-equal "???" message :start2 (- len 3)))
     "yell")
    ((and (> len 2) (string-equal "??" message :start2 (- len 2)))
     "demand")
    ((string-equal "?" message :start2 len)
     "ask")
    ((and (> len 3) (string-equal "!!!" message :start2 (- len 3)))
     "scream")
    ((and (> len 2) (string-equal "!!" message :start2 (- len 2)))
     "yell")
    ((string-equal "!" message :start2 len)
     "exclaim")
    ((and (> len 3) (string-equal "..." message :start2 (- len 3)))
     "mutter")
    ((> len 320)
     "drone")
    ((> len 160)
     "ramble")
    ((< (hitp-of ch) (floor (max-hitp-of ch) 20))
     "moan")
    ((< (move-of ch) (floor (max-move-of ch) 20))
     "gasp")
    ((room-is-underwater (in-room-of ch))
     "gurgle")
    ((and (aff2-flagged ch +aff2-ablaze+)
          (not (char-withstands-fire ch)))
     "scream")
    ((is-poisoned ch)
     "choke")
    ((is-sick ch)
     "wheeze")
    ((> (get-condition ch +drunk+) 10)
     "slur")
    ((zerop (get-condition ch +thirst+))
     "rasp")
    ((or (search "y'all" message :test #'char-equal)
         (search "ain't" message :test #'char-equal))
     "drawl")
    ((aff2-flagged ch +aff2-berserk+)
     "rave")
    ((aff2-flagged ch +aff2-intimidated+)
     "whimper")
    ((aff-flagged ch +aff-confusion+)
     "stammer")
    (t
     "say"))))

(defcommand (ch "say") (:resting)
  (send-to-char ch "Yes, but WHAT do you want to say?~%"))

(defcommand (ch "say" message) (:resting)
  (perform-say ch (select-say-cmd ch message) message))

(defcommand (ch #\') (:resting)
  (send-to-char ch "Yes, but WHAT do you want to say?~%"))

(defcommand (ch #\' message) (:resting)
  (perform-say ch (select-say-cmd ch message) message))

(macrolet ((define-say-commands (&rest commands)
             `(progn
                ,@(mapcar (lambda (command)
                            (let ((msg (gensym "MSG")))
                              `(progn
                                 (defcommand (ch ,command ,msg) (:resting)
                                   (perform-say ch ,command ,msg))
                                 (defcommand (ch ,command) (:resting)
                                   (let ((social (gethash ,command *socials*)))
                                     (if social
                                         (act ch
                                              :subject-emit (social-message-char-no-arg social)
                                              :place-emit (social-message-others-no-arg social))
                                         (send-to-char ch "Yes, but WHAT do you want to say?~%")))))))
                            commands))))
           (define-say-commands "add" "admit" "apologize" "assert" "ask"
                                "babble" "bellow" "chant" "complain" "coo"
                                "declare" "demand" "describe" "drawl"
                                "enthuse" "exclaim" "expostulate" "grunt"
                                "groan" "growl" "grumble" "howl" "indicate"
                                "intone" "mourn" "moan" "mumble" "murmur"
                                "mutter" "note" "plead" "prattle" "pronounce"
                                "quote" "ramble" "rant" "rave" "respond"
                                "request" "retort" "smirk" "snarl" "sneer"
                                "state" "stutter" "suggest" "threaten" "utter"
                                "wail" "whimper" "whine" "yell"))

(defcommand (ch "tell") ()
  (send-to-char ch "Who do you wish to tell what?~%"))

(defcommand (ch "tell" target-str) ()
  (declare (ignore target-str))
  (send-to-char ch "Who do you wish to tell what?~%"))

(defun can-send-tell (ch target)
  (or (immortal-level-p ch)
      (eql (in-room-of ch) (in-room-of target))
      (not (or (room-flagged (in-room-of ch) +room-soundproof+)
               (room-flagged (in-room-of ch) +room-soundproof+)))
      (eql (zone-of (in-room-of ch)) (zone-of (in-room-of target)))
      (not (or (zone-flagged (zone-of (in-room-of ch)) +zone-isolated+)
               (zone-flagged (zone-of (in-room-of ch)) +zone-soundproof+)
               (zone-flagged (zone-of (in-room-of target)) +zone-isolated+)
               (zone-flagged (zone-of (in-room-of target)) +zone-soundproof+)))
      (eql (plane-of (zone-of (in-room-of ch)))
           (plane-of (zone-of (in-room-of target))))
      (eql (time-frame-of (zone-of (in-room-of ch)))
           (time-frame-of (zone-of (in-room-of target))))
      (or (eql (time-frame-of (zone-of (in-room-of ch))) +time-timeless+)
          (eql (time-frame-of (zone-of (in-room-of target))) +time-timeless+))))

(defcommand (ch "tell" target-str message) ()
  (let ((target (get-player-vis ch target-str)))
    (cond
      ((null target)
       (send-to-char ch "No-one by that name here.~%"))
      ((eql target ch)
       (send-to-char ch "Talking to yourself is a sign of impending mental collapse.~%"))
      (t
       (perform-tell ch target message)))))

(defcommand (ch "reply" message) ()
  (let ((target (and (last-tell-from-of ch)
                     (gethash (last-tell-from-of ch) *character-map*))))
    (cond
      ((null (last-tell-from-of ch))
       (send-to-char ch "You have no-one to reply to!~%"))
      ((null target)
       (send-to-char ch "They are no longer playing.~%"))
      (t
       (perform-tell ch target message)))))

(defcommand (ch "retell" message) ()
  (let ((target (and (last-tell-to-of ch)
                     (gethash (last-tell-to-of ch) *character-map*))))
    (cond
      ((null (last-tell-to-of ch))
       (send-to-char ch "You have no-one to retell!~%"))
      ((null target)
       (send-to-char ch "They are no longer playing.~%"))
      (t
       (perform-tell ch target message)))))

(macrolet ((define-moods (&rest moods)
             `(progn
                ,@(mapcar (lambda (mood)
                            `(defcommand (ch ,mood) (:mood)
                                 (send-to-char ch "What do you want to ~a do?~%"
                                               ,mood)))
                          moods)
                ,@(mapcar (lambda (mood)
                            `(defcommand (ch ,mood command) (:mood)
                               (unwind-protect
                                    (progn
                                      (setf (mood-of ch) ,mood)
                                      (interpret-command ch command))
                                 (setf (mood-of ch) nil))))
                          moods))))
  (define-moods "accusingly" "angelically" "angrily" "apologetically"
                "arrogantly" "bitterly" "boldly" "brutally" "callously"
                "calmly" "carefully" "cautiously" "cheerfully"
                "contentedly" "craftily" "crazily" "curiously"
                "defiantly" "depressingly" "desperately" "diabolically"
                "dismally" "disdainfully" "divinely" "drunkenly"
                "dumbly" "dreamily" "enthusiastically" "enviously"
                "evilly" "excitedly" "fearfully" "fiercely" "fondly"
                "gallantly" "gently" "graciously" "gravely" "grumpily"
                "happily" "harshly" "hatefully" "hungrily" "icily"
                "impatiently" "indignantly" "innocently" "inquisitively"
                "jealously" "jokingly" "kindly" "knowingly"
                "lazily" "longingly" "loudly" "lovingly" "lustily"
                "maliciously" "meekly" "merrily" "mischeviously"
                "mockingly" "modestly" "moodily" "morbidly"
                "mysteriously" "nervously" "nicely" "obediently"
                "ominously" "outrageously" "passionately" "patiently"
                "peacefully" "playfully" "pleasantly" "politely"
                "proudly" "quickly" "quietly" "respectfully" "rudely"
                "ruthlessly" "sadly" "sagely" "sarcastically"
                "savagely" "seductively" "sensually" "seriously"
                "shamefully" "sheepishly" "shyly" "skeptically"
                "sleepily" "slowly" "slyly" "softly" "solemnly"
                "somberly" "soothingly" "sourly" "stoically"
                "suavely" "suspiciously" "sycophantically"
                "teasingly" "tenderly" "tiredly" "thoughtfully"
                "uncertainly" "viciously" "warily" "warmly" "weakly"
                "wearily" "whimsically" "wildly" "wickedly" "wisely"
                "wishfully" "wistfully" "wryly" "valiantly"
                "vehemently"))

(defcommand (ch #\> target-str) (:resting)
  (declare (ignore target-str))
  (send-to-char ch "Yes, but WHAT do you want to say?~%"))

(defcommand (ch #\>) (:resting)
  (send-to-char ch "Say what to who?~%"))

(defcommand (ch #\> target-str message) (:resting)
  (let ((target (resolve-alias-in-room ch target-str)))
    (if target
       (perform-say-to ch target message)
       (send-to-char ch "There's no '~a' here.~%" target))))

(defcommand (ch "emote") (:resting)
  (send-to-char ch "Yes... but what?~%"))

(defcommand (ch "emote" message) (:resting)
  (perform-emote ch message))

(defcommand (ch #\:) (:resting)
  (send-to-char ch "Yes... but what?~%"))

(defcommand (ch #\: message) (:resting)
  (perform-emote ch message))

(defcommand (ch "me") (:resting)
  (send-to-char ch "Yes... but what?~%"))

(defcommand (ch "me" message) (:resting)
  (perform-emote ch message))

(defcommand (ch "whisper") (:resting)
  (send-to-char ch "To whom do you want to whisper... and what?~%"))

(defcommand (ch "whisper" ignore) (:resting)
  (declare (ignore ignore))
  (send-to-char ch "To whom do you want to whisper... and what?~%"))

(defun perform-whisper (ch target message)
  (let ((escaped-msg (act-escape message)))
    (act ch :target target
         :subject-emit (format nil "&yYou$a whisper to $N$l,&n '$[~a]'"
                               escaped-msg)
         :target-emit (format nil "&y$n$a whispers to you$l,&n '$[~a]'"
                               escaped-msg)
         :not-target-emit "$n$a whispers something to $N.")))

(defcommand (ch "whisper" target-str message) (:resting)
  (let ((target (resolve-alias-in-room ch target-str)))
    (cond
      ((null target)
       (send-to-char ch "No-one by that name here.~%"))
      ((eql target ch)
       (send-to-char ch "You can't get your mouth close enough to your ear...~%"))
      (t
       (perform-whisper ch target message)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +channels+
    '((:name "holler"
       :deaf-flag #.+pref-noholler+
       :scope universe
       :desc-color #\Y
       :text-color #\r
       :minimum-level 6
       :move-cost 10
       :not-on-message "Ha!  You are noholler buddy."
       :muted-message "You find yourself unable to holler.")
      (:name "shout"
       :scope zone
       :desc-color #\y
       :text-color #\c
       :muted-message "You cannot shout!!")
      (:name "gossip"
       :scope plane
       :deaf-flag #.+pref-nogoss+
       :desc-color #\g
       :text-color #\n
       :minimum-level 6
       :not-on-message "You aren't even on the channel!"
       :muted-message "You cannot gossip!!")
      (:name "auction"
       :scope plane
       :deaf-flag #.+pref-noauct+
       :desc-color #\m
       :text-color #\n
       :npc-only-message "Only licensed auctioneers can use that channel!"
       :not-on-message "You aren't even on the channel!"
       :muted-message "You cannot auction!!")
      (:name "haggle"
       :scope plane
       :deaf-flag #.+pref-noauct+
       :desc-color #\m
       :text-color #\n
       :not-on-message "You aren't even on the channel!"
       :muted-message "You cannot haggle!!")
      (:name "congrat"
       :scope plane
       :deaf-flag #.+pref-nogratz+
       :desc-color #\g
       :text-color #\m
       :minimum-level 6
       :not-on-message "You aren't even on the channel!"
       :muted-message "You cannot congratulate!!")
      (:name "sing"
       :scope plane
       :deaf-flag #.+pref-nomusic+
       :desc-color #\c
       :text-color #\y
       :minimum-level 6
       :not-on-message "You aren't even on the channel!"
       :muted-message "You cannot sing!!")
      (:name "spew"
       :scope plane
       :deaf-flag #.+pref-nospew+
       :desc-color #\r
       :text-color #\y
       :minimum-level 6
       :not-on-message "You aren't even on the channel!"
       :muted-message "You cannot spew!!")
      (:name "dream"
       :scope plane
       :deaf-flag #.+pref-nodream+
       :desc-color #\c
       :text-color #\W
       :minimum-level 6
       :maximum-position +pos-sleeping+
       :not-on-message "You aren't even on the channel!"
       :muted-message "You cannot dream!!")
      (:name "project"
       :scope universe
       :deaf-flag #.+pref-noproject+
       :desc-color #\W
       :text-color #\c
       :minimum-gen 6
       :not-on-message "You are not open to projections yourself..."
       :muted-message "You cannot project.  The immortals have muted you.")
      (:name "newbie"
       :scope plane
       :deaf-flag #.+pref-newbie-helper+
       :desc-color #\y
       :text-color #\w
       :not-on-message "You aren't on the illustrious newbie channel."
       :muted-message "The immortals have muted you for bad behavior!")
      (:name "petition"
       :scope universe
       :deaf-flag #.+pref-nopetition+
       :desc-color #\m
       :text-color #\c
       :minimum-level-heard 50
       :not-on-message "You aren't listening to petitions at this time."
       :muted-message "The immortals have turned a deaf ear to your petitions.")
      (:name "guildsay"
       :scope plane
       :guild-channel t
       :deaf-flag #.+pref-noguildsay+
       :desc-color #\m
       :text-color #\y
       :not-on-message "You aren't listening to the rumors of your guild."
       :muted-message "You may not guild-say, for the immortals have muted you.")
      (:name "clansay"
       :scope plane
       :clan-channel t
       :deaf-flag #.+pref-noclansay+
       :desc-color #\c
       :text-color #\n
       :not-on-message "You aren't listening to the words of your clan."
       :muted-message "The immortals have muted you.  You may not clansay.")
      (:name "clanemote"
       :scope plane
       :clan-channel t
       :emote t
       :deaf-flag #.+pref-noclansay+
       :desc-color #\c
       :text-color #\c
       :not-on-message "You aren't listening to the words of your clan."
       :muted-message "The immortals have muted you.  You may not clan emote.")
      (:name "imm"
       :scope universe
       :deaf-flag #.+pref-noimmchat+
       :desc-color #\y
       :text-color #\y
       :not-on-message "You aren't on the immchat channel."
       :group "immortal")
      (:name "wiz"
       :scope universe
       :deaf-flag #.+pref-nowiz+
       :desc-color #\c
       :text-color #\c
       :minimum-level 60
       :not-on-message "You aren't on the wizchat channel."
       :group "immortal"))))

(defun can-use-channel (ch chan)
  (cond
    ;; Pets can't shout on interplanar channels
    ((and (null (link-of ch))
          (master-of ch)
          (eql (getf chan :scope) 'universe))
     nil)
    ;; Drunk people not allowed
    ((and (> (get-condition ch +drunk+) 5)
          (>= (random-range 0 3) 2))
     (send-to-char ch "You try to ~a, but somehow it doesn't come out right.~%"
                   (name-of chan))
     nil)
    ;; Player is muted
    ((plr-flagged ch +plr-noshout+)
     (send-to-char ch "~a" (getf chan :muted-message))
     nil)
    ;; Make sure the char is on the channel
    ((let ((flag (getf chan :deaf-flag)))
           (and flag (pref-flagged ch flag)))
     (send-to-char ch "~a~%" (getf chan :not-on-message))
     nil)
    ;; Afterwards are all restrictions to which immortals and npcs are uncaring
    ((or (is-npc ch) (immortalp ch))
     t)
    ;; Channel is NPC only
    ((getf chan :npc-only-message)
        (send-to-char ch "~a~%" (getf chan :npc-only-message))
     nil)
    ;; High enough level to shout
    ((let ((min-level (getf chan :minimum-level)))
       (and min-level
            (< (level-of ch) min-level)
            (<= (remort-gen-of ch) 0)))
     (send-to-char ch "You must be at least level ~d before you can ~a.~%"
                   (getf chan :minimum-level)
                   (first chan))
     (send-to-char ch "Try using the newbie channel instead.~%")
     nil)
    ;; Only remort players can project
    ((and (getf chan :minimum-gen)
          (not (is-remort ch))
          (not (immortal-level-p ch)))
     (send-to-char ch "You do not know how to project yourself that way.~%")
     nil)
    ;; Maximum position of channel
    ((and (getf chan :maximum-position)
          (> (position-of ch) (getf chan :maximum-position)))
     (send-to-char ch "You attempt to dream, but realize you need to sleep first.~%")
     nil)
    ;; Guild channel checks
    ((and (getf chan :guild-channel)
          (or (and (= (char-class-of ch) +class-monk+)
                   (not (is-neutral ch)))
              (and (= (char-class-of ch) +class-cleric+)
                   (is-neutral ch))
              (and (= (char-class-of ch) +class-knight+)
                   (is-neutral ch))))

     (case (char-class-of ch)
       (#.+class-monk+
        (send-to-char ch "You have been cast out of the monks until your neutrality is regained.~%"))
       (#.+class-cleric+
        (send-to-char ch "You have been cast out of the ranks of the blessed.~%"))
       (#.+class-knight+
        (send-to-char ch "You have been cast out of the ranks of the honored.~%")))
     nil)
    ;; Ensure they are in a clan for clan channels
    ((and (getf chan :clan-channel)
               (zerop (clan-of ch)))
     (send-to-char ch "You aren't even in a clan!~%")
     nil)
    ;; Ensure they have enough movement for the movement cost
    ((let ((move-cost (getf chan :move-cost)))
       (and move-cost
            (not (immortalp ch))
            (< (move-of ch) move-cost)))
     (send-to-char ch "You're too exhausted to holler.~%")
     nil)
    ;; All checks pass
    (t
     t)))

(defun can-receive-channel (ch cxn chan evilp goodp char-class clan-id)
  (let ((target (actor-of cxn)))
    (and
     (eql (state-of cxn) 'playing)
     target
     (not (or (plr-flagged target +plr-writing+)
              (plr-flagged target +plr-olc+)))
     (let ((deaf-flag (getf chan :deaf-flag)))
       (or (not deaf-flag)
           (not (pref-flagged target deaf-flag))))
     (or (is-npc ch)
         (immortalp ch)
         (immortalp target)
         (eql ch target)
         (and
          (or (not (getf chan :clan-channel))
              (= (clan-of target) clan-id))
          (or (not (getf chan :guild-channel))
              (and (= (char-class-of target) char-class)
                   (or (not (= (char-class-of target) +class-monk+))
                       (is-neutral target))
                   (or (and (not (= (char-class-of target) +class-cleric+))
                            (not (= (char-class-of target) +class-knight+)))
                       (and goodp (is-good target))
                       (and evilp (is-evil target)))))
          (or (not (getf chan :min-gen))
              (is-remort target))
          (or (not (getf chan :maximum-pos))
              (/= (position-of target) +pos-sleeping+))
          (not (getf chan :minimum-level-heard))
          (or (eql (in-room-of ch) (in-room-of target))
              (and (not (room-flagged (in-room-of ch) +room-soundproof+))
                   (not (room-flagged (in-room-of target) +room-soundproof+))))
          (or (not (eql (getf chan :scope) 'zone))
              (eql (zone-of (in-room-of ch))
                   (zone-of (in-room-of target))))
          (or (not (eql (getf chan :scope) 'plane))
              (eql (plane-of (zone-of (in-room-of ch)))
                   (plane-of (zone-of (in-room-of target))))))))))


(defun perform-channel-emits (ch chan evilp goodp char-class clan-id message)
  (let* ((subchannel-desc (cond
                            ((getf chan :guild-channel)
                             (format nil "[~(~:[~*~a~;~c-~a~]~)] "
                                     (or (= char-class +class-cleric+)
                                         (= char-class +class-knight+))
                                     (if goodp #\g #\e)
                                     (if (<= 0 char-class +top-class+)
                                         (aref +class-names+ char-class)
                                         (format nil "#~d" char-class))))
                            ((getf chan :clan-channel)
                             (let ((clan (real-clan clan-id)))
                               (format nil "[~:[#~d~;~(~a~)~]] "
                                       clan
                                       (if clan
                                           (name-of clan)
                                           clan-id))))
                            (t "")))
         (imm-actstr (if (getf chan :emote)
                         (format nil "&~c~a$n &~c~a"
                                 (getf chan :desc-color)
                                 subchannel-desc
                                 (getf chan :text-color)
                                 (act-escape message))
                         (format nil "&~c~a$n$a ~a$%$l, &~c'$[~a]'"
                                 (getf chan :desc-color)
                                 subchannel-desc
                                 (getf chan :name)
                                 (getf chan :text-color)
                                 (act-escape message))))
         (mort-actstr (if (getf chan :emote)
                          (format nil "&~c$n$a &~c~a"
                                  (getf chan :desc-color)
                                  (getf chan :text-color)
                                  (act-escape message))
                          (format nil "&~c$n$a ~a$%$l, &~c'$[~a]'"
                                  (getf chan :desc-color)
                                  (getf chan :name)
                                  (getf chan :text-color)
                                  (act-escape message)))))
    (dolist (cxn *cxns*)
      (when (and (typep cxn 'tempus-cxn)
                 (can-receive-channel ch cxn chan evilp goodp char-class clan-id))
        (send-act-str (actor-of cxn)
                      (if (immortalp (actor-of cxn))
                          imm-actstr
                          mort-actstr)
                      ch (actor-of cxn) nil nil
                      (if (eql ch (actor-of cxn))
                          :self
                          :target))))

    (when (and (room-flagged (in-room-of ch) +room-soundproof+)
               (not (immortalp ch))
               (string/= (getf chan :name) "petition"))
      (send-to-char ch "The walls seem to absorb your words...~%"))))

(defun do-gen-comm (ch chan-name message)
  (let ((chan (find chan-name +channels+ :test (lambda (x y)
                                                 (string= x (getf y :name))))))
    (assert chan nil "Channel ~a not found" chan-name)
    (when (can-use-channel ch chan)
      (perform-channel-emits ch chan
                             (is-evil ch)
                             (is-good ch)
                             (char-class-of ch)
                             (if (is-pc ch) (clan-of ch) nil)
                             message))))

(defcommand (ch #\; msg) (:resting)
  (do-gen-comm ch "wiz" msg))

(defcommand (ch #\;) (:resting)
  (send-to-char ch "Yes, wiz, fine, wiz we must, but WHAT???~%"))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defmacro define-comm-channel (command channel)
    `(progn
       (defcommand (ch ,command msg) (:resting)
         (do-gen-comm ch ,channel msg))
       (defcommand (ch ,command) (:resting)
         (send-to-char ch "Yes, ~a, fine, ~a we must, but WHAT???~%"
                       ,channel
                       ,channel))))

  ;; Define the commands identical to channel names
  (macrolet ((define-channel-commands ()
               `(progn
                  ,@(mapcar (lambda (name)
                              `(define-comm-channel ,name ,name))
                            (mapcar #'second +channels+)))))
    (define-channel-commands))

  ;; Define command aliases
  (define-comm-channel "chat" "gossip")
  (define-comm-channel "csay" "clansay")
  (define-comm-channel "cemote" "clanemote")
  (define-comm-channel "grats" "congrat")
  (define-comm-channel "congratulate" "congrat"))
