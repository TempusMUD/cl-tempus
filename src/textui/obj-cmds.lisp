(in-package #:tempus)

(defcommand (ch "get") (:resting)
  (send-to-char ch "Get what?~%"))

(defcommand (ch "get" thing) (:resting)
    (cond
      ((>= (carry-items-of ch) (can-carry-items ch))
       (send-to-char ch "Your arms are already full!~%"))
      (t
       (get-from-room ch (resolve-alias ch thing (contents-of (in-room-of ch))) thing))))

(defcommand (ch "get" thing "from" container) (:resting)
  (cond
    ((>= (carry-items-of ch) (can-carry-items ch))
     (send-to-char ch "Your arms are already full!~%"))
    (t
     (let ((containers (append (resolve-alias ch container
                                                     (carrying-of ch))
                               (resolve-alias ch
                                              container
                                              (contents-of (in-room-of ch))))))
       (if containers
           (dolist (container containers)
             (get-from-container ch thing container))
           (send-to-char ch "You don't see any '~a'.~%" container))))))

(defcommand (ch "put") (:resting)
  (send-to-char ch "Put what in what?~%"))

(defcommand (ch "put" thing) (:resting)
  (send-to-char ch "What do you want to put ~a in?~%"
                (if (eql (find-all-dots thing) :find-indiv) "it" "them")))

(defcommand (ch "put" thing "into" container-str) (:resting)
  (let* ((locations (append (carrying-of ch)
                            (coerce (remove nil (equipment-of ch)) 'list)
                            (contents-of (in-room-of ch))))
         (containers (resolve-alias ch container-str locations))
         (container (first containers))
         (objs (resolve-alias ch thing (carrying-of ch))))
    (cond
      ((null containers)
       (send-to-char ch "You don't see ~a ~a here.~%"
                     (a-or-an container-str) container-str))
      ((> (length containers) 1)
       (send-to-char ch "You can only put things into one container at a time.~%"))
      ((and (is-obj-kind container +item-container+)
            (is-obj-kind container +item-vehicle+)
            (is-obj-kind container +item-pipe+))
       (act ch :item container
            :subject-emit "$p is not a container.~%"))
      ((and (not (is-obj-kind container +item-pipe+))
            (logtest (aref (value-of container) 1) +cont-closed+))
       (send-to-char ch "You'd better open it first!~%"))
      ((null objs)
       (send-to-char ch "You aren't carrying ~a ~a.~%"
                     (a-or-an thing) thing))
      (t
       (loop
          for obj-sublist on objs
          as obj = (first obj-sublist)
          as next-obj = (second obj-sublist)
          as counter from 1
          do
          (cond
            ((eql obj container)
             (act ch :item obj
                  :subject-emit "You attempt to fold $p into itself, but fail."))
            ((and (is-obj-kind obj +item-bomb+)
                  (contains-of obj)
                  (is-obj-kind (contains-of obj) +item-fuse+)
                  (plusp (fuse-state (contains-of obj))))
             (act ch :item obj :target-item container
                  :subject-emit "It would really be best if you didn't put $p into $P."))
            ((and (is-obj-kind container +item-pipe+)
                  (not (is-obj-kind obj +item-tobacco+)))
             (act ch :item obj :target-item container
                  :subject-emit "You can't pack $P with $p!"))
            (t
             (perform-put ch obj container)

             (when (or (null next-obj)
                       (string/= (name-of next-obj) (name-of obj)))
               (act ch :item obj
                    :target-item container
                    :all-emit (format nil "$n put$% $p into $P.~[~;~:;~:* (x~d)~]" counter))
               (setf counter 0)))))
       (when (is-obj-kind container +item-pipe+)
         (loop while (contains-of container) do
              (extract-obj (contains-of container))))))))


(defcommand (ch "drop") (:resting)
  (send-to-char ch "What do you want to drop?"))

(defcommand (ch "drop" thing) (:resting)
  "Drop one or more objects"
  (let* ((dot-mode (find-all-dots thing))
         (objs (if (eql dot-mode :find-all)
                  (carrying-of ch)
                  (resolve-alias ch thing (carrying-of ch)))))
    (cond
      (objs
       (perform-drop ch objs :drop))
      ((eql dot-mode :find-all)
       (send-to-char ch "You don't seem to be carrying anything.~%"))
      (t
       (send-to-char ch "You don't seem to have any ~as.~%" thing)))))

(defcommand (ch "drop" amount-str "gold") (:resting)
  "Drop an amount of gold"
  (let ((amount (parse-integer amount-str :junk-allowed t)))
    (cond
      ((null amount)
       (send-to-char ch "That's not a proper amount of gold."))
      (t
       (perform-drop-money ch amount 'gold)))))

(defcommand (ch "drop" amount-str "coin") (:resting)
  "Drop an amount of gold"
  (let ((amount (parse-integer amount-str :junk-allowed t)))
    (cond
      ((null amount)
       (send-to-char ch "That's not a proper amount of gold."))
      (t
       (perform-drop-money ch amount 'gold)))))

(defcommand (ch "drop" amount-str "coins") (:resting)
  "Drop an amount of gold"
  (let ((amount (parse-integer amount-str :junk-allowed t)))
    (cond
      ((null amount)
       (send-to-char ch "That's not a proper amount of gold."))
      (t
       (perform-drop-money ch amount 'gold)))))

(defcommand (ch "drop" amount-str "credits") (:resting)
  "Drop an amount of gold"
  (let ((amount (parse-integer amount-str :junk-allowed t)))
    (cond
      ((null amount)
       (send-to-char ch "That's not a proper amount of credits."))
      (t
       (perform-drop-money ch amount 'cash)))))

(defcommand (ch "wear") (:resting)
  (send-to-char ch "What do you want to wear?~%"))

(defcommand (ch "wear" thing) (:resting)
  (let* ((objs (resolve-alias ch thing (carrying-of ch)))
         (wear-objs (delete nil
                            (mapcar (lambda (obj)
                                      (let ((pos (find-eq-pos obj)))
                                        (when pos
                                          (cons obj pos))))
                                    objs))))
    (cond
      ((null objs)
       (case (find-all-dots thing)
         (:find-all
          (send-to-char ch "You aren't carrying anything.~%"))
         (:find-alldot
          (send-to-char ch "You don't seem to have any '~a'.~%"
                        thing))
         (:find-indiv
          (send-to-char ch "You don't seem to have ~a ~a.~%"
                        (a-or-an thing) thing))))
      ((null wear-objs)
       (case (find-all-dots thing)
         (:find-all
          (send-to-char ch "You don't seem to have anything wearable.~%"))
         (:find-alldot
          (send-to-char ch "You don't seem to have any wearable '~a'.~%"
                        thing))
         (:find-indiv
          (act ch :item (first objs)
               :subject-emit "You can't wear $p."))))
      (t
       (dolist (tuple wear-objs)
         (perform-wear ch (car tuple) (cdr tuple)))))))

(defcommand (ch "wear" thing "on" pos-str) (:resting)
    (let ((pos (position pos-str +wear-keywords+ :test #'string-abbrev))
          (objs (resolve-alias ch thing (carrying-of ch))))
      (cond
        ((null objs)
         (send-to-char "You don't seem to have any '~a'.~%"
                       thing))
        ((> (length objs) 1)
         (send-to-char ch "You can't wear more than one item on a position.~%"))
        ((null pos)
         (send-to-char ch "'~a'?  What part of your body is THAT?~%" pos-str))
        (t
         (perform-wear ch (car objs) pos)))))

(defcommand (ch "wear" thing "about" "body") (:resting)
    (let ((objs (resolve-alias ch thing (carrying-of ch))))
      (cond
        ((null objs)
         (send-to-char "You don't seem to have any '~a'.~%"
                       thing))
        ((> (length objs) 1)
         (send-to-char ch "You can't wear more than one item on a position.~%"))
        (t
         (perform-wear ch (car objs) +wear-about+)))))

(defcommand (ch "wear" thing "up" "ass") (:resting)
    (let ((objs (resolve-alias ch thing (carrying-of ch))))
      (cond
        ((null objs)
         (send-to-char "You don't seem to have any '~a'.~%"
                       thing))
        ((> (length objs) 1)
         (send-to-char ch "You can't wear more than one item on a position.~%"))
        (t
         (perform-wear ch (car objs) +wear-ass+)))))

(defcommand (ch "remove") (:resting)
  (send-to-char ch "Remove what?~%"))

(defcommand (ch "remove" thing) (:resting)
  (let* ((objs (resolve-alias ch thing
                                     (coerce (remove nil (equipment-of ch)) 'list))))
    (cond
      ((and (aff3-flagged ch +aff3-attraction-field+)
            (not (pref-flagged ch +pref-nohassle+)))
       (send-to-char ch "You cannot remove anything while generating an attraction field!"))
      ((null objs)
       (case (find-all-dots thing)
         (:find-all
          (send-to-char ch "You're not using anything.~%"))
         (:find-alldot
          (send-to-char ch "You don't seem to be using any ~as.~%"
                        thing))
         (:find-indiv
          (send-to-char ch "You don't seem to have ~a ~a.~%"
                        (a-or-an thing) thing))))
      (t
       (dolist (obj objs)
         (perform-remove ch (worn-on-of obj)))))))

(defcommand (ch "remove" thing "from" pos-str) (:resting)
  (let* ((pos (position pos-str +wear-keywords+ :test #'string-abbrev))
         (obj (when pos (aref (equipment-of ch) pos))))
    (cond
      ((null pos)
       (send-to-char ch "'~a'?  What part of your body is THAT?~%" pos-str))
      ((null obj)
       (send-to-char ch "You aren't wearing anything there.~%"))
      ((not (is-alias-of thing (aliases-of obj)))
       (send-to-char ch "You aren't wearing ~a ~a there.~%"
                     (a-or-an thing) thing))
      (t
       (perform-remove ch pos)))))

(defcommand (ch "give") (:resting)
  (send-to-char ch "Give what to who?~%"))

(defcommand (ch "give" thing) (:resting)
  (declare (ignore thing))
  (send-to-char ch "Who do you want to give it to?~%"))

(defcommand (ch "give" thing "to" target) (:resting)
  (let* ((objs (resolve-alias ch thing (carrying-of ch)))
         (victs (resolve-alias ch target (people-of (in-room-of ch))))
         (vict (first victs))
         (mode (find-all-dots thing)))
    (cond
      ((null vict)
       (send-to-char ch "No-one by that name here.~%"))
      ((rest victs)
       (send-to-char ch "You can't give to more than one person.~%"))
      ((eql ch vict)
       (send-to-char ch "What's the point of that?~%"))
      (objs
       (loop
          for obj-sublist on objs
          as obj = (first obj-sublist)
          as next-obj = (second obj-sublist)
          as counter from 1
          do
          (cond
            ((perform-give ch (first victs) obj)
             (when (or (null next-obj)
                       (string/= (name-of next-obj) (name-of obj)))
               (act ch :target vict :item obj
                    :all-emit (format nil "$n give$% $p to $N.~[~;~:;~:* (x~d)~]" counter))
               (setf counter 0)))
            (t
             (setf counter 0)))))
      ((eql mode :find-indiv)
       (send-to-char ch "You aren't carrying ~a ~a.~%" (a-or-an thing) thing))
      ((eql mode :find-all)
       (send-to-char ch "You aren't carrying anything.~%"))
      (t
       (send-to-char ch "You aren't carrying any ~as.~%" (a-or-an thing) thing)))))

(defcommand (ch "give" amount-str "coins" "to" target) (:resting)
  (let ((amount (parse-integer amount-str :junk-allowed t))
        (targets (resolve-alias ch target (people-of (in-room-of ch)))))
    (cond
      ((notevery #'digit-char-p amount-str)
       (send-to-char ch "That's a bogus number of coins.~%"))
      ((> amount (gold-of ch))
       (send-to-char ch "You don't have that much money!~%"))
      ((null targets)
       (send-to-char ch "No-one by that name here.~%"))
      ((rest targets)
       (send-to-char ch "You can't give money to multiple people at once.~%"))
      (t
       (perform-give-money ch (first targets) amount 'gold)))))


(defcommand (ch "give" amount-str "credits" "to" target) (:resting)
  (let ((amount (parse-integer amount-str :junk-allowed t))
        (targets (resolve-alias ch target (people-of (in-room-of ch)))))
    (cond
      ((notevery #'digit-char-p amount-str)
       (send-to-char ch "That's a bogus number of credits.~%"))
      ((> amount (gold-of ch))
       (send-to-char ch "You don't have that much money!~%"))
      ((null targets)
       (send-to-char ch "No-one by that name here.~%"))
      ((rest targets)
       (send-to-char ch "You can't give money to multiple people at once.~%"))
      (t
       (perform-give-money ch (first targets) amount 'cash)))))

(defcommand (ch "plant" thing) (:resting)
  (declare (ignore thing))
  (send-to-char ch "Plant what on whom?~%"))

(defcommand (ch "plant" thing "on" target) (:resting)
  (let* ((objs (resolve-alias ch thing (carrying-of ch)))
         (victs (resolve-alias ch target (people-of (in-room-of ch))))
         (vict (first victs))
         (mode (find-all-dots thing)))
    (cond
      ((null vict)
       (send-to-char ch "No-one by that name here.~%"))
      ((rest objs)
       (send-to-char ch "You can't plant more than one item at a time.~%"))
      ((rest victs)
       (send-to-char ch "You can't give to more than one person.~%"))
      ((eql ch vict)
       (send-to-char ch "What's the point of that?~%"))
      (objs
       (perform-plant ch (first victs) (first objs)))
      ((eql mode :find-indiv)
       (send-to-char ch "You aren't carrying ~a ~a.~%" (a-or-an thing) thing))
      ((eql mode :find-all)
       (send-to-char ch "You aren't carrying anything.~%"))
      (t
       (send-to-char ch "You aren't carrying any ~as.~%" (a-or-an thing) thing)))))

(defcommand (ch "drink" thing) (:resting)
  (let* ((objs (resolve-alias ch thing (append
                                               (carrying-of ch)
                                               (contents-of (in-room-of ch)))))
         (obj (first objs)))
    (cond
      ((null obj)
       (send-to-char ch "You can't find it!~%"))
      ((rest objs)
       (send-to-char ch "You can only drink from one thing at a time!~%"))
      (t
       (perform-drink ch obj)))))

(defcommand (ch "eat") (:resting)
  (send-to-char ch "Eat what?~%"))

(defcommand (ch "eat" thing) (:resting)
  (let* ((objs (resolve-alias ch thing (append
                                               (carrying-of ch)
                                               (contents-of (in-room-of ch)))))
         (obj (first objs)))
    (cond
      ((null obj)
       (send-to-char ch "You don't seem to have ~a ~a.~%"
                     (a-or-an thing) thing))
      ((rest objs)
       (send-to-char ch "You can only eat one thing at a time!~%"))
      (t
       (perform-eating ch obj)))))

(defcommand (ch "pour") (:resting)
  (send-to-char ch "What do you want to pour?~%"))

(defcommand (ch "pour" "out") (:resting)
  (send-to-char ch "What do you want to pour out?~%"))

(defcommand (ch "pour" "out" thing) (:resting)
  (let* ((objs (resolve-alias ch thing (carrying-of ch))))
    (cond
      ((null objs)
       (send-to-char ch "You can't find ~a ~a.~%" (a-or-an thing) thing))
      ((rest objs)
       (send-to-char ch "You can't pour out than one container at a time!~%"))
      (t
       (perform-pour-out ch (first objs))))))

(defcommand (ch "pour" from-thing "into" to-thing) (:resting)
  (let* ((from-objs (resolve-alias ch from-thing (carrying-of ch)))
         (from-obj (first from-objs))
         (to-objs (resolve-alias ch to-thing (append
                                                     (carrying-of ch)
                                                     (contents-of (in-room-of ch)))))
         (to-obj (first to-objs)))
    (cond
      ((null from-obj)
       (send-to-char ch "You can't find ~a ~a.~%"
                     (a-or-an from-thing)
                     from-thing))
      ((null to-obj)
       (send-to-char ch "You can't find ~a ~a.~%"
                     (a-or-an to-thing)
                     to-thing))
      ((rest from-objs)
       (send-to-char ch "You can't pour from more than one container at a time!~%"))
      ((rest to-objs)
       (send-to-char ch "You can't pour into more than one container at a time!~%"))
      (t
       (perform-pour ch from-obj to-obj)))))

(defcommand (ch "wield") (:resting)
  (send-to-char ch "Wield what?~%"))

(defcommand (ch "wield" thing) (:resting)
  (let* ((objs (resolve-alias ch thing (carrying-of ch)))
         (obj (first objs))
         (hands-free (char-hands-free ch)))
    (cond
      ((null objs)
       (send-to-char ch "You don't seem to have ~a ~a.~%"
                     (a-or-an thing)
                     thing))
      ((is-animal ch)
       (send-to-char ch "Animals don't wield weapons.~%"))
      ((not (can-wear obj +item-wear-wield+))
       (send-to-char ch "You can't wield that.~%"))
      ((> (weight-of obj) (getf (aref +str-app+
                                                (str-of ch))
                                          :wield-w))
       (send-to-char ch "It's too damn heavy.~%"))
      ((and (is-cleric ch)
            (not (is-evil ch))
            (member (+ (aref (value-of obj) 3) +type-hit+)
                    (list +type-slash+
                          +type-pierce+
                          +type-stab+
                          +type-rip+
                          +type-chop+
                          +type-claw+)))
       (send-to-char ch "You can't wield that as a cleric.~%"))
      ((zerop hands-free)
       (send-to-char ch "You don't have a hand free to wield it with.~%"))
      ((and (/= hands-free 2)
            (is-obj-stat2 obj +item2-two-handed+))
       (act ch :item obj
            :subject-emit "You need both hands free to wield $p."))
      ((and (get-eq ch +wear-hands+)
            (is-obj-stat2 (get-eq ch +wear-hands+) +item2-two-handed+))
       (act ch :item (get-eq ch +wear-hands+)
            :subject-emit "You can't wield anything while wearing $p on your hands."))
      ((null (get-eq ch +wear-wield+))
       ;; normal wield
       (perform-wear ch obj +wear-wield+))
      ((and (is-merc ch)
            (is-any-gun (get-eq ch +wear-wield+))
            (is-any-gun obj))
       ;; mercs can dual wield any gun
       (perform-wear ch obj +wear-wield-2+))
      ((if (<= (weight-of (get-eq ch +wear-wield+)) 6)
           (> (weight-of obj) (weight-of (get-eq ch +wear-wield+)))
           (> (weight-of obj) (floor (weight-of (get-eq ch +wear-wield+)) 2)))
       ;; dual wield weight restrictions
       (send-to-char ch "Your secondary weapon must weigh less than half of your primary weapon,~%if your primary weighs more than six pounds.~%"))
      (t
       ;; dual wield
       (perform-wear ch obj +wear-wield-2+)))))

(defcommand (ch "hold") (:resting)
  (send-to-char ch "Hold what?"))

(defcommand (ch "hold" thing) (:resting)
  (let* ((objs (resolve-alias ch thing (carrying-of ch)))
         (obj (first objs)))
    (cond
      ((null objs)
       (send-to-char ch "You don't seem to have ~a ~a.~%"
                     (a-or-an thing)
                     thing))
      ((cdr objs)
       (send-to-char ch "You can only hold one thing at a time!~%"))
      ((is-obj-kind obj +item-light+)
       (perform-hold-light ch obj))
      (t
       (perform-hold ch obj)))))

(defcommand (ch "attach") (:resting)
  (send-to-char ch "Attach what to what?~%"))

(defcommand (ch "attach" thing) (:resting)
  (declare (ignore thing))
  (send-to-char ch "Attach what to what?~%"))

(defcommand (ch "attach" thing "to" to-thing) (:resting)
  (let ((objs (resolve-alias ch thing (carrying-of ch)))
        (to-objs (resolve-alias ch to-thing (append
                                                          (carrying-of ch)
                                                          (contents-of (in-room-of ch))))))
    (cond
      ((null objs)
       (send-to-char ch "You don't seem to have ~a ~a.~%"
                     (a-or-an thing)
                     thing))
      ((null to-objs)
       (send-to-char ch "You don't see any '~a'.~%" to-thing))
      ((cdr objs)
       (send-to-char ch "You can only attach one thing at a time.~%"))
      ((cdr to-objs)
       (send-to-char ch "You can only attach to one thing at a time.~%"))
      (t
       (perform-attach ch (first objs) (first to-objs))))))

(defcommand (ch "detach") (:resting)
  (send-to-char ch "Detach what from what?~%"))

(defcommand (ch "detach" thing) (:resting)
  (declare (ignore thing))
  (send-to-char ch "Detach what from what?~%"))

(defcommand (ch "detach" thing "from" from-thing) (:resting)
  (let ((objs (resolve-alias ch thing (carrying-of ch)))
        (from-objs (resolve-alias ch from-thing (append
                                                          (carrying-of ch)
                                                          (contents-of (in-room-of ch))))))
    (cond
      ((null objs)
       (send-to-char ch "You don't seem to have ~a ~a.~%"
                     (a-or-an thing)
                     thing))
      ((null from-objs)
       (send-to-char ch "You don't see any '~a'.~%" from-thing))
      ((cdr objs)
       (send-to-char ch "You can only detach one thing at a time.~%"))
      ((cdr from-objs)
       (send-to-char ch "You can only detach to one thing at a time.~%"))
      (t
       (perform-detach ch (first objs) (first from-objs))))))

(defcommand (ch "conceal") (:resting)
  (send-to-char ch "Conceal what?~%"))

(defcommand (ch "conceal" thing) (:resting)
  (let* ((locations (append (carrying-of ch)
                            (coerce (remove nil (equipment-of ch)) 'list)
                            (contents-of (in-room-of ch))))
         (objs (resolve-alias ch thing locations))
         (obj (first objs)))
    (cond
      ((null objs)
       (send-to-char ch "You don't seem to have ~a ~a.~%"
                     (a-or-an thing)
                     thing))
      ((rest objs)
       (send-to-char ch "You can conceal only one thing at a time!~%"))
      (t
       (perform-conceal ch obj)))))

(defcommand (ch "sacrifice") (:resting)
  (send-to-char ch "Sacrifice what object?~%"))

(defcommand (ch "sacrifice" thing) (:resting)
  (let* ((objs (resolve-alias ch thing (contents-of (in-room-of ch))))
         (obj (first objs)))
    (cond
      ((null objs)
       (send-to-char ch "You don't seem to have ~a ~a.~%"
                     (a-or-an thing)
                     thing))
      ((rest objs)
       (send-to-char ch "You can sacrifice only one thing at a time!~%"))
      (t
       (perform-sacrifice ch obj)))))

(defcommand (ch "junk" thing) (:resting)
  (let* ((dot-mode (find-all-dots thing))
         (objs (resolve-alias ch thing (carrying-of ch))))
    (cond
      ((and objs (eql dot-mode :find-all))
       (send-to-char ch "Go to the dump if you want to junk EVERYTHING!~%"))
      (objs
       (perform-drop ch objs :junk))
      ((eql dot-mode :find-all)
       (send-to-char ch "You don't seem to be carrying anything.~%"))
      (t
       (send-to-char ch "You don't seem to have any ~as.~%" thing)))))

(defcommand (ch "donate" thing) (:resting)
  (let* ((dot-mode (find-all-dots thing))
         (objs (resolve-alias ch thing (carrying-of ch))))
    (cond
      ((and objs (eql dot-mode :find-all))
       (send-to-char ch "Go to the donation room if you want to donate EVERYTHING!~%"))
      (objs
       (perform-drop ch objs :donate))
      ((eql dot-mode :find-all)
       (send-to-char ch "You don't seem to be carrying anything.~%"))
      (t
       (send-to-char ch "You don't seem to have any ~as.~%" thing)))))

(defcommand (ch "empty" thing) (:resting)
  (let* ((objs (resolve-alias ch thing (carrying-of ch)))
         (obj (first objs)))
    (cond
      ((null objs)
       (send-to-char ch "You don't seem to have ~a ~a.~%"
                     (a-or-an thing)
                     thing))
      ((rest objs)
       (send-to-char ch "You can only empty one thing at a time!~%"))
      (t
       (perform-empty ch obj)))))

(defcommand (ch "empty" from-thing "into" to-thing) (:resting)
  (let* ((from-objs (resolve-alias ch from-thing (carrying-of ch)))
         (from-obj (first from-objs))
         (to-objs (resolve-alias ch to-thing (append
                                                     (carrying-of ch)
                                                     (contents-of (in-room-of ch)))))
         (to-obj (first to-objs)))
    (cond
      ((null from-obj)
       (send-to-char ch "You can't find ~a ~a.~%"
                     (a-or-an from-thing)
                     from-thing))
      ((null to-obj)
       (send-to-char ch "You can't find ~a ~a.~%"
                     (a-or-an to-thing)
                     to-thing))
      ((rest from-objs)
       (send-to-char ch "You can't empty from more than one container at a time!~%"))
      ((rest to-objs)
       (send-to-char ch "You can't empty into more than one container at a time!~%"))
      (t
       (perform-empty-into ch from-obj to-obj)))))


(defcommand (ch "activate") (:resting)
  (send-to-char ch "Activate what?"))

(defcommand (ch "activate" "internal") (:resting)
  (send-to-char ch "Activate which implant?"))

(defcommand (ch "activate" "internal" name) (:resting)
  (let ((obj (resolve-alias ch name (coerce (remove nil (implants-of ch)) 'list))))
    (if obj
        (perform-activate-object ch obj)
        (send-to-char ch "You are not implanted with ~a '~a'~%"
                      (a-or-an name)
                      name))))

(defcommand (ch "activate" name) (:resting)
  (let ((skill (and (is-cyborg ch)
                    (find-if (lambda (spell)
                               (and (logtest (routines-of spell) +cyb-activate+)
                                    (string-abbrev name (name-of spell))))
                             *spell-info*))))
    (if skill
        (perform-cyborg-activate ch (idnum-of skill))
        (let ((obj (resolve-alias ch name (append (coerce (remove nil (equipment-of ch)) 'list)
                                                  (carrying-of ch)
                                                  (contents-of (in-room-of ch))))))
          (cond
            ((null obj)
             (send-to-char ch "You don't seem to have ~a '~a'~%"
                           (a-or-an name)
                           name))
            ((rest obj)
             (send-to-char ch "You can only activate one object at a time.~%"))
            (t
             (perform-activate-object ch (first obj))))))))

