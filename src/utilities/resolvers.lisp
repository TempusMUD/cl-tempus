(in-package #:tempus)

(defun resolve-player-idnum (alias)
  "Given ALIAS containing digits or a player name, returns the player
idnum associated with the player.  Returns NIL if no such player
exists."
  (cond
    ((every #'digit-char-p alias)
     (let ((idnum (parse-integer alias :junk-allowed t)))
       (if (and idnum
                (player-idnum-exists idnum))
           idnum
           nil)))
    (t
     (retrieve-player-idnum alias))))

(defun resolve-account (alias)
  "Given ALIAS containing digits, an account name, or a player name
  preceded by a period, returns the associated account.  Returns NIL
  if no match is found."
  (cond
    ((every #'digit-char-p alias)
     (let ((idnum (parse-integer alias :junk-allowed t)))
       (and idnum
            (account-by-idnum idnum))))
    ((string= alias "." :end1 1)
     (let ((player-idnum (retrieve-player-idnum (subseq alias 1))))
       (and player-idnum
            (account-by-idnum (retrieve-player-account player-idnum)))))
    (t
     (load-account alias))))

(defun resolve-house (ch alias)
  "ALIAS should contain digits or a single period.  In the case of
 digits, returns the house with the given numeric idnum, or NIL if no
 such house exists.  In the case of the period, the house CH is
 currently in is returned, or NIL if CH is not standing in a house.
 Returns NIL if ALIAS is neither digits or a period"
  (cond
    ((every #'digit-char-p alias)
     (let ((idnum (parse-integer alias :junk-allowed t)))
       (if idnum
           (find-house-by-idnum idnum)
           NIL)))
    ((string= alias ".")
     (find-house-by-room (in-room-of ch)))
    (t
     nil)))

(defun resolve-room (ch alias)
  "ALIAS should contain digits or a single period.  In the case of
 digits, returns the room with the given numeric idnum, or NIL if no
 such room exists.  In the case of the period, the room CH is
 currently inside is returned.  If ALIAS is neither digits nor a period,
 returns NIL."
  (cond
    ((every #'digit-char-p alias)
     (let ((idnum (parse-integer alias :junk-allowed t)))
       (if idnum
           (real-room idnum)
           nil)))
    ((string= alias ".")
     (in-room-of ch))
    (t
     nil)))

(defun resolve-clan (alias)
  "Given ALIAS containing digits or a clan name, returns the associated
clan.  Returns NIL if no such clan exists."
  (if (every #'digit-char-p alias)
      (real-clan (parse-integer alias))
      (find alias (sort (hash-values *clans*) #'< :key 'idnum-of)
            :test #'string-abbrev
            :key 'name-of)))