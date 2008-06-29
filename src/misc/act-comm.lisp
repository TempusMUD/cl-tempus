(in-package #:tempus)

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
                              `(defcommand (ch ,command ,msg) (:resting)
                                 (perform-say ch ,command ,msg))))
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

(macrolet ((define-moods (&rest moods)
             `(progn
                ,@(mapcar (lambda (mood)
                            `(defcommand (ch ,mood) (:mood)
                                 (send-to-char ch "What do you want to ~a do?~%"
                                               ,mood)))
                          moods)
                ,@(mapcar (lambda (mood)
                            `(defcommand (ch ,mood command) (:resting :mood)
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
  (send-to-char ch "Yes, but WHAT do you want to say?~%"))

(defcommand (ch #\>) (:resting)
  (send-to-char ch "Say what to who?~%"))

(defcommand (ch #\> target-str message) (:resting)
  (let ((target (resolve-alias ch target-str)))
    (cond
      ((null target)
       (send-to-char ch "There's no '~a' here.~%" target))
      ((typep target 'creature)
       (perform-say-to ch target message))
      ((typep target 'obj-data)
       (perform-say-to-obj ch target message))
      (t
       (error "Shouldn't happen")))))

(defcommand (ch "emote") (:resting)
  (send-to-char ch "Yes... but what?~%"))

(defcommand (ch "emote" message) (:resting)
  (perform-emote ch message))

(defcommand (ch #\:) (:resting)
  (send-to-char ch "Yes... but what?~%"))

(defcommand (ch #\: message) (:resting)
  (perform-emote ch message))
