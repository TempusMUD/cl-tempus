(in-package #:tempus)

(define-special fido (trigger self ch command vars) (+spec-mob+)
  (declare (ignore command vars ch))
  (when (and (eql trigger 'tick)
             (awakep self)
             (not (fighting-of self)))
    (let ((victim (get-char-random-vis self (in-room-of self))))
      (case (random-range 0 70)
        (0
         (act self :place-emit "$n scratches at some fleas."))
        (1
         (act self :place-emit "$n starts howling mournfully."))
        (2
         (act self :place-emit "$n licks $s chops."))
        (3
         (act self :place-emit "$n pukes all over the place."))
        (4
         (when (or (is-pc victim)
                   (not (eql (func-of (shared-of victim)) 'special-fido)))
           (act self :target victim
                :target-emit "$n takes a leak on your shoes."
                :not-target-emit "$n takes a leak on $N's shoes.")))
        (5
         (if (and (eql (sex-of self) 'male)
                  (zerop (random-range 0 1)))
             (act self :place-emit "$n licks $s balls.")
             (act self :place-emit "$n licks $s ass.")))
        (6
         (act self :target victim
              :target-emit "$n sniffs your crotch."
              :not-target-emit "$n sniffs $N's crotch."))
        (7
         (when (or (is-pc victim)
                   (not (eql (func-of (shared-of victim))
                             'special-fido)))
           (act self :target victim
                :target-emit "$n slobbers on your hand."
                :not-target-emit "$n slobbers on $N's hand.")))
        (8
         (act self :place-emit "$n starts biting on $s leg."))
        (t
         (return-from special-fido nil))))
    t))
