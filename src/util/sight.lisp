(in-package #:tempus)

(defun room-is-dark (room)
  "Returns T if the room is too dark to see unaided."
  (and (not (plusp (light-of room)))
       (or (eql (terrain-of room) +sect-elemental-ooze+)
           (room-flagged room +room-dark+)
           (and (room-in-prime-plane room)
                (not (room-flagged room +room-indoors+))
                (not (eql (terrain-of room) +sect-inside+))
                (not (eql (terrain-of room) +sect-vehicle+))
                (not (eql (terrain-of room) +sect-city+))
                (room-is-sunny room)))))

(defun has-infravision (ch)
  "Returns T if the creature CH has infravision"
  (or (aff-flagged ch +aff-infravision+)
      (is-race ch +race-elf+)
      (is-race ch +race-drow+)
      (is-race ch +race-dwarf+)
      (is-race ch +race-half-orc+)
      (is-race ch +race-tabaxi+)
      (is-race ch +race-dragon+)
      (is-race ch +race-orc+)
      (is-race ch +race-ogre+)
      (is-race ch +race-goblin+)
      (is-race ch +race-troll+)
      (is-race ch +race-bugbear+)
      (is-undead ch)))

(defun has-dark-sight (ch)
  "Returns T if the creature can see in the dark"
  (or (has-infravision ch)
      (pref-flagged ch +pref-holylight+)
      (aff3-flagged ch +aff3-sonic-imagery+)
      (aff-flagged ch +aff-retina+)
      (plusp (check-skill ch +skill-night-vision+))))

(defun check-sight-self (ch)
  "Returns T if the player can see at all"
  (or (not (aff-flagged ch +aff-blind+))
      (aff3-flagged ch +aff3-sonic-imagery+)))

(defun can-see-room (ch room)
  "Returns T if the player can see in the room."
  (not (or (and (room-is-dark room) (not (has-dark-sight ch)))
           (and (room-flagged room +room-smoke-filled+)
                (not (aff3-flagged ch +aff3-sonic-imagery+))))))

(defun can-see-object (ch obj)
  "Returns T if a player can see an object."
  (or (pref-flagged ch +pref-holylight+)
      (and (or (approvedp obj)
               (mob2-flagged ch +mob2-unapproved+)
               (immortalp ch)
               (testerp ch))
           (or (not (is-obj-stat obj +item-transparent+))
               (aff3-flagged ch +aff3-sonic-imagery+)
               (aff-flagged ch +aff-retina+)
               (affected-by-spell ch +zen-awareness+))
           (or (not (is-obj-stat obj +item-invisible+))
               (aff-flagged ch +aff-detect-invis+)
               (aff2-flagged ch +aff2-true-seeing+)))))

(defun can-see-creature (ch tch)
  (cond
    ;; Immortal players can always see non-immortal players
    ((and (immortalp ch) (not (immortalp tch)))
     t)
    ;; Nothing at all gets through immort invis
    ((and (is-pc ch)
          (immortalp tch)
          (< (level-of ch) (invis-level-of tch)))
     nil)
    ;; Mortals can't see unapproved mobs
    ((and (not (approvedp tch))
          (approvedp ch)
          (not (immortalp ch))
          (not (testerp ch)))
     nil)
    ;; Non-tester mortal players can't see testers
    ((and (not (immortalp ch))
          (is-pc ch)
          (not (testerp ch))
          (testerp tch))
     nil)
    ;; Holy is the light that shines on the chosen
    ((pref-flagged ch +pref-holylight+)
     t)
    ;; Sonic imagery and retina detects transparent creatures
    ((and (aff2-flagged tch +aff2-transparent+)
          (not (or (aff3-flagged ch +aff3-sonic-imagery+)
                   (aff-flagged ch +aff-retina+)
                   (affected-by-spell ch +zen-awareness+))))
     nil)
    ;; True seeing and detect invisibility counteract all magical invis
    ((or (aff2-flagged ch +aff2-true-seeing+)
         (aff-flagged ch +aff-detect-invis+))
     t)
    ;; Invisibility
    ((aff-flagged tch +aff-invisible+)
     nil)
    ;; Invis to undead
    ((and (is-undead ch)
          (aff2-flagged tch +aff2-invis-to-undead+))
     nil)
    (t
     t)))

(defmethod is-visible-to ((target creature) ch)
  (cond
    ;; Can always see self
    ((eql ch target)
     t)
    ;; Immortal players can always see non-immortal players
    ((and (immortalp ch) (not (immortalp target)))
     t)
    ;; Only immortals can see utility mobs
    ((and (mob-flagged target +mob-utility+) (not (immortalp ch)))
     nil)
    ;; Nothing at all gets through immort invis
    ((and (is-pc ch)
          (is-pc target)
          (immortalp target)
          (< (level-of ch) (invis-level-of target)))
     nil)
    ;; Holylight can see everything else
    ((pref-flagged ch +pref-holylight+)
     t)
    ((not (check-sight-self ch))
     nil)
    ((not (can-see-room ch (in-room-of target)))
     nil)
    ((not (can-see-creature ch target))
     nil)
    (t
     t)))


(defmethod is-visible-to ((obj obj-data) ch)
  (cond
    ;; If they can't see the object itself, none of the rest of it is
    ;; going to matter much
    ((not (can-see-object ch obj))
     nil)
    ;; If the object is in a container, it inherits visibility of
    ;; the container
    ((in-obj-of obj)
     (is-visible-to (in-obj-of obj) ch))
    ;; If the object is being carried by someone, it
    ;; also inherits visibility
    ((carried-by-of obj)
     (or (eql (carried-by-of obj) ch)
         (is-visible-to (carried-by-of obj) ch)))
    ;; Ditto if the object is being worn by someone
    ((worn-by-of obj)
     (or (eql (worn-by-of obj) ch)
         (is-visible-to (worn-by-of obj) ch)))
    ;; If they are carrying or wearing the item, they can see it
    ;; even if blind
    ((not (check-sight-self ch))
     nil)
    ((not (can-see-room ch (in-room-of obj)))
     nil)
    (t
     t)))

(defmethod is-visible-to ((room room-data) ch)
  (and (check-sight-self ch)
       (can-see-room ch room)))
