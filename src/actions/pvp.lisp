(in-package #:tempus)

(defparameter +criminal-reputation+ 300)

(defun criminalp (ch)
  (and (is-pc ch)
       (>= (reputation-of ch) +criminal-reputation+)))

(defun arena-questing-p (ch)
  "Returns T if ch is a member of a quest that has been set ARENA, otherwise returns NIL"
  (unless (is-npc ch)
    (and (plusp (quest-id-of ch))
         (quest-flagged (quest-by-vnum (quest-id-of ch)) +quest-arena+))))

(defun arena-combat-p (ch vict)
  "Returns true if a combat between ch and vict follows arena PK rules.  Otherwise returns NIL"
  (or
   (and (in-room-of vict)
        (or (room-flagged (in-room-of vict) +room-arena+)
            (arena-questing-p vict)))
   (and ch
        (in-room-of ch)
        (or (room-flagged (in-room-of ch) +room-arena+)
            (arena-questing-p ch)))))

(defun npk-combat-p (ch vict)
  "Returns true if a combat between ch and vict follows neutral PK rules.  Otherwise returns NIL"
  (and ch
       vict
       (not (is-npc ch))
       (not (is-npc vict))
       (eql (pk-style-of (zone-of (in-room-of vict))) +zone-neutral-pk+)))

(defun reputation-rank (ch)
  (cond
    ((zerop (reputation-of ch))
     0)
    ((>= (reputation-of ch) 1000)
     11)
    (t
     (1+ (floor (reputation-of ch) 100)))))