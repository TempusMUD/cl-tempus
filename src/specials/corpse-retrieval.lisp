(in-package #:tempus)

(defcommand (ch "retrieve") (:standing)
  (send-to-char ch "You can't do that here.~%"))

(define-special corpse-retrieval (trigger self ch command vars) (+spec-mob+)
  (declare (ignore vars))
  (when (and (eql trigger 'command)
             (string= (first (command-info-pattern command)) "retrieve"))
    (let* ((corpse (find-if (lambda (obj)
                              (and (is-obj-kind obj +item-container+)
                                   (plusp (obj-val-of obj 2))
                                   (= (corpse-idnum obj)
                                      (idnum-of ch))))
                            *object-list*))
           (price (and corpse
                       (adjusted-cost ch self
                                      (* (level-of ch)
                                         (if (contains-of corpse)
                                             4000
                                             100)))))
           (money (if (char-in-future? self)
                      (cash-of ch)
                      (gold-of ch)))
           (currency (if (char-in-future? self)
                         "credit"
                         "coin")))
      (cond
        ((not (can-see-creature self ch))
         (perform-say self "say" "Who's there?  I can't see you."))
        ((is-npc ch)
         (act self :target ch :all-emit "$n snicker$% at $N."))
        ((let ((corpse-room (and corpse (find-object-room corpse))))
           (or (null corpse)
               (null corpse-room)
               (room-flagged corpse-room +room-norecall+)
               (and (not (eql (zone-of corpse-room) (zone-of (in-room-of ch))))
                    (or (zone-flagged (zone-of corpse-room) +zone-isolated+)
                        (zone-flagged (zone-of (in-room-of ch)) +zone-isolated+)))))
         (perform-say-to self ch "Sorry, but your corpse cannot be located."))
        ((eql (carried-by-of corpse) ch)
         (perform-say-to self ch "You already have it, you dolt!"))
        ((eql (in-room-of corpse) (in-room-of self))
         (perform-say-to self ch "You fool.  It's already in this room!"))
        ((> price money)
         (perform-say-to self ch (format nil "You don't have enough money.  It costs ~d ~a~p"
                                         price currency price)))
        (t
         (cond
           ((in-room-of corpse)
            (act nil :item corpse :place-emit "$p disappears with a flash!")
            (obj-from-room corpse))
           ((carried-by-of corpse)
            (act nil :item corpse :all-emit "$p disappears out of your hands!")
            (obj-from-char corpse))
           ((worn-by-of corpse)
            (act nil :item corpse :all-emit "$p disappears off of your body!")
            (cond
              ((eql corpse (get-eq (worn-by-of corpse) (worn-on-of corpse)))
               (unequip-char (worn-by-of corpse) (worn-on-of corpse) :worn nil))
              ((eql corpse (get-implant (worn-by-of corpse) (worn-on-of corpse)))
               (unequip-char (worn-by-of corpse) (worn-on-of corpse) :implant nil))
              ((eql corpse (get-tattoo (worn-by-of corpse) (worn-on-of corpse)))
               (unequip-char (worn-by-of corpse) (worn-on-of corpse) :tattoo nil)))))

         (perform-say-to self ch (format nil "Very well.  I will retrieve your corpse for ~d ~a~p"
                                         price currency price))
         (obj-to-char corpse ch)
         (alexandria:switch ((time-frame-of (zone-of (in-room-of self))))
           (+time-past+
            (decf (gold-of ch) price)
            (act self :all-emit "$n make$% some strange gestures and howl$%!"))
           (+time-future+
            (decf (cash-of ch) price)
            (act self :all-emit "$n slip$% into a deep concentration and there is a flash of light!"))
           (+time-timeless+
            (decf (gold-of ch) price)
            (act self :all-emit "The air shimmers as $n lifts $s hands to the heavens!")))
         (act ch :item corpse
              :subject-emit "$p appears in your hands!")
         (save-player-to-xml ch))))
    t))
