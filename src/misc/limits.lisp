(in-package #:tempus)

(defun gain-condition (ch condition value)
  (assert (<= 0 condition 2) nil "Invalid condition passed to gain-condition")
  (unless (or (zerop value)
              (minusp (aref (conditions-of ch) condition)))
    (setf (aref (conditions-of ch) condition)
          (pin (+ (aref (conditions-of ch) condition) value) 0 24))
    (when (and (zerop (get-condition ch condition))
               (link-of ch)
               (eql (state-of (link-of ch)) 'playing))
      (case condition
        (#.+full+
         (send-to-char ch
                       (case (random-range 0 2)
                         (0 "You feel quite hungry.~%")
                         (1 "You are famished.~%")
                         (2 "You are hungry.~%"))))
        (#.+thirst+
         (send-to-char ch
                       (case (random-range 0 1)
                         (0 "Your throat is parched.~%")
                         (1 "You are thirsty.~%"))))
        (#.+drunk+
         (send-to-char ch "You are now sober.~%"))))))

(defun graf (age race p0 p1 p2 p3 p4 p5 p6)
  (let* ((race (if (> race +race-troll+) +race-human+ race))
         (lifespan (aref +racial-lifespans+ race)))
    (cond
      ((< age (* lifespan 0.2))
       p0)
      ((<= age (* lifespan 0.3))
       (+ p1 (/ (* (- age (* lifespan 0.2)) (- p2 p1)) (* lifespan 0.2))))
      ((<= age (* lifespan 0.55))
       (+ p2 (/ (* (- age (* lifespan 0.35)) (- p3 p2)) (* lifespan 0.2))))
      ((<= age (* lifespan 0.75))
       (+ p3 (/ (* (- age (* lifespan 0.55)) (- p4 p3)) (* lifespan 0.2))))
      ((<= age lifespan)
       (+ p4 (/ (* (- age (* lifespan 0.75)) (- p5 p4)) (* lifespan 0.2))))
      (t
       p6))))

(defun age (ch)
  (let ((player-age (mud-time-passed (timestamp-to-unix (now))
                                     (timestamp-to-unix (birth-time-of ch)))))
    (cond
      ((or (eql (race-of ch) +race-elf+)
           (eql (race-of ch) +race-drow+))
       (+ player-age 80))
      ((eql (race-of ch) +race-dwarf+)
       (+ player-age 40))
      ((eql (race-of ch) +race-half-orc+)
       (+ player-age 12))
      ((eql (race-of ch) +race-human+)
       (+ player-age 13))
      ((eql (race-of ch) +race-halfling+)
       (+ player-age 33))
      (t
       (+ player-age 13)))))

(defun hit-gain (ch)
  (+ (if (is-npc ch)
         (level-of ch)
         (+ (graf (age ch) (race-of ch) 10 14 22 34 18 12 6)
            (floor (level-of ch) 8)))
     (* 2 (con-of ch))
     (floor (check-skill ch +skill-speed-healing+) 3)
     (if (aff3-flagged ch +aff3-damage-control+)
         (+ (level-of ch) (floor (check-skill ch +skill-damage-control+) 4))
         0)
     ;; TODO: finish this
))

(defun mana-gain (ch)
  0)

(defun move-gain (ch)
  0)

(defun gain-exp (ch gain)
  (unless (or (and ch
                   (in-room-of ch)
                   (room-flagged (in-room-of ch) +room-arena+))
              (and (not (is-npc ch))
                   (immortal-level-p ch)
                   (< (level-of ch) 1))
              (and (not (is-npc ch))
                   (aff-flagged ch +aff-charm+)))
    (if (is-npc ch)
        (setf (exp-of ch) (min 2000000000 (+ (exp-of ch) gain)))
        (incf (exp-of ch) (min +max-exp-gain+ gain)))
    (when (minusp (exp-of ch))
      (setf (exp-of ch) 0))

    (unless (is-npc ch)
      (let ((num-levels 0))
        (loop
           while (and (< (level-of ch) (1- +lvl-ambassador+))
                      (>= (exp-of ch) (aref +exp-scale+ (1+ (level-of ch)))))
           do
             (incf num-levels)
             (incf (level-of ch))
             (advance-level ch t))
        (cond
          ((= num-levels 1)
           (send-to-char ch "You rise a level!~%"))
          ((plusp num-levels)
           (send-to-char ch "You rise ~d levels!~%" num-levels)))))))

(defun gain-exp-regardless (ch gain)
  (incf (exp-of ch) gain)
  (when (minusp (exp-of ch))
    (setf (exp-of ch) 0))

  (unless (is-npc ch)
    (let ((num-levels 0))
      (loop
         while (and (< (level-of ch) +lvl-grimp+)
                    (>= (exp-of ch) (aref +exp-scale+ (1+ (level-of ch)))))
         do
           (incf num-levels)
           (incf (level-of ch))
           (advance-level ch t))
      (cond
        ((= num-levels 1)
         (send-to-char ch "You rise a level!~%"))
        ((plusp num-levels)
         (send-to-char ch "You rise ~d levels!~%" num-levels))))))