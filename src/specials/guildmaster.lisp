(in-package #:tempus)

(defcommand (ch "practice") ()
  (list-skills ch :only-known-p t))
(defcommand (ch "practice" args) ()
  (declare (ignore args))
  (send-to-char ch "You'll have to do that someplace else.~%"))
(defcommand (ch "learn") ()
  (list-skills ch :only-known-p t))
(defcommand (ch "learn" args) ()
  (declare (ignore args))
  (send-to-char ch "You'll have to do that someplace else.~%"))

(defun skill/spell (ch)
  (if (> (char-class-of ch)
         +num-classes+)
      "spell"
      (string-downcase (aref +class-prac-type+ (char-class-of ch)))))

(defun how-good (percent)
  (cond
    ((minusp percent) "terrible")
    ((zerop percent) "not learned")
    ((<= percent 10) "awful")
    ((<= percent 20) "bad")
    ((<= percent 40) "poor")
    ((<= percent 55) "average")
    ((<= percent 70) "fair")
    ((<= percent 80) "good")
    ((<= percent 85) "very good")
    ((<= percent 100) "superb")
    ((<= percent 150) "extraordinary")
    (t "superhuman")))

(defun list-skills (ch &key only-known-p only-spells-p only-skills-p)
  (with-pagination ((link-of ch))
    (let ((spells nil)
          (skills nil))
      (loop
         with unsorted-skills = nil
         with unsorted-spells = nil
         for ability across *spell-info*
         as idnum = (idnum-of ability) do
           (when (and (char/= #\! (char (name-of ability) 0))
                      (or (not (zerop (skill-of ch idnum)))
                          (and (not only-known-p)
                               (can-learn-spell ch idnum))))
             (if (<= idnum +max-spells+)
                 (push ability unsorted-spells)
                 (push ability unsorted-skills)))
         finally (progn
                   (setf spells (sort unsorted-spells #'string< :key 'name-of))
                   (setf skills (sort unsorted-skills #'string< :key 'name-of))))

      (when (and (not only-skills-p)
                 (or (and (not (eql (aref +class-prac-type+ (char-class-of ch)) 'skill))
                          (not (eql (aref +class-prac-type+ (char-class-of ch)) 'program)))
                     (and (remort-gen-of ch)
                          (not (eql (aref +class-prac-type+ (remort-char-class-of ch)) 'skill))
                          (not (eql (aref +class-prac-type+ (remort-char-class-of ch)) 'program)))))
        (send-to-char ch "&YYou know ~:[~;of ~]the following ~as:&n~%" only-known-p
                      (skill/spell ch))
        (dolist (spell spells)
          (if (immortalp ch)
              (send-to-char ch "&g~3d. ~31a &G~16@<(~a)~>&Y[~3d] &R(~3d mana)&n~%"
                            (idnum-of spell)
                            (name-of spell)
                            (how-good (skill-of ch (idnum-of spell)))
                            (skill-of ch (idnum-of spell))
                            (spell-mana-cost ch (idnum-of spell)))
              (send-to-char ch "&g ~31a &G~16@<(~a)~>&R(~3d mana)&n~%"
                            (name-of spell)
                            (how-good (skill-of ch (idnum-of spell)))
                            (spell-mana-cost ch (idnum-of spell))))))

      (unless only-spells-p
        (send-to-char ch "~%&YYou know ~:[~;of ~]the following ~a:&n~%"
                      only-known-p (if (is-cyborg ch)
                                       "programs" "skills"))
        (dolist (skill skills)
          (if (immortalp ch)
              (send-to-char ch "&g~3d. ~31a &G~16@<(~a)~>&Y[~3d]&n~%"
                            (idnum-of skill)
                            (name-of skill)
                            (how-good (skill-of ch (idnum-of skill)))
                            (skill-of ch (idnum-of skill)))
              (send-to-char ch "&g ~31a &G~16@<(~a)~>&n~%"
                            (name-of skill)
                            (how-good (skill-of ch (idnum-of skill))))))))))

(defun get-skill-cost (ch skill)
  (let ((skill-level (spell-level skill (char-class-of ch)))
        cost)
    (when (and (is-remort ch)
               (> skill-level (spell-level skill (remort-char-class-of ch))))
      (setf skill-level (spell-level skill (remort-char-class-of ch))))
    (setf cost (* skill-level skill-level 100))

    (when (spell-gen skill (char-class-of ch))
      (setf cost (* cost (1+ (spell-gen skill (char-class-of ch))))))
    cost))

(defun skill-gain (ch wants-average-p)
  (let ((min-gain (+ (int-of ch)
                     (remort-gen-of ch)))
        (max-gain (+ (* 2 (int-of ch))
                     (remort-gen-of ch))))
  (if wants-average-p
      (floor (+ min-gain max-gain) 2)
      (random-range min-gain max-gain))))

(define-special guildmaster (trigger self ch command vars) (+spec-mob+)
  (when (and (eql trigger 'command)
             (awakep self)
             (awakep ch)
             (not (fighting-of self)))
    (cond
      ((string= "list" (first (command-info-pattern command)))

       (list-skills ch)
       t)
      ((not (member (first (command-info-pattern command))
                    '("practice" "train" "learn" "offer")
                    :test #'string-equal))
       nil)
      ((null vars)
       (list-skills ch)
       t)
      (t
       (let ((skill (position (first vars) *spell-info* :key 'name-of :test 'string-abbrev)))
         (cond
           ((null skill)
            (perform-tell self ch (format nil "You do not know of that ~a!" (skill/spell self))))
           ((not (can-learn-spell ch skill))
            (if (not (zerop (check-skill ch skill)))
                (perform-tell self ch (format nil "I cannot teach you ~a yet."
                                              (spell-to-str skill)))
                (perform-tell self ch (format nil "You are not yet ready to practice ~a."
                                              (spell-to-str skill)))))
           ((and (not (eql (char-class-of self) +class-normal+))
                 (< (level-of self) (spell-level skill (char-class-of self)))
                 (or (not (is-remort self))
                     (< (level-of self)
                        (spell-level skill (remort-char-class-of self)))))
            (perform-tell self ch "I am not able to teach you that."))
           ((and (plusp (spell-gen skill (char-class-of self)))
                 (not (is-remort self)))
            (perform-tell self ch "You must go elsewhere to learn that remort skill."))
           ((and (> (check-skill ch skill)
                    10)
                 (or (eql skill +skill-read-scrolls+)
                     (eql skill +skill-use-wands+)))
            (perform-tell self ch (format nil "You cannot practice ~a any further." (spell-to-str skill))))
           ((or (and (spell-is-good skill) (not (is-good self)))
                (and (spell-is-evil skill) (not (is-evil self))))
            (perform-tell self ch "You have no business dealing with such magic."))
           (t
            (let ((cost (get-skill-cost ch skill)))
              (incf cost (floor (* cost (cost-modifier-of ch self)) 100))

              (if (eql (time-frame-of (zone-of (in-room-of self))) +time-future+)
                  (cond
                    ((string= "offer" (first (command-info-pattern command)))
                     (perform-tell self ch (format nil "It will cost you ~a creds to train ~a."
                                                   cost (spell-to-str skill)))
                     (return-from special-guildmaster t))
                    ((< (cash-of ch) cost)
                     (perform-tell self ch (format nil "You don't have the ~a creds I require to train ~a."
                                                   cost (spell-to-str skill)))
                     (return-from special-guildmaster t))
                    (t
                     (send-to-char ch "You buy training for ~a creds and practice for a while...~%"
                                   cost)
                     (decf (cash-of ch) cost)))
                  (cond
                    ((string= "offer" (first (command-info-pattern command)))
                     (perform-tell self ch (format nil "It will cost you ~a gold to train ~a."
                                                   cost (spell-to-str skill)))
                     (return-from special-guildmaster t))
                    ((< (gold-of ch) cost)
                     (perform-tell self ch (format nil "You don't have the ~a gold I require to train ~a."
                                                   cost (spell-to-str skill)))
                     (return-from special-guildmaster t))
                    (t
                     (send-to-char ch "You buy training for ~a gold and practice for a while...~%"
                                   cost)
                     (decf (gold-of ch) cost))))
              (act ch :place-emit "$n practices for a while.")
              (wait-state ch (rl-sec 2))

              (let ((percent (skill-of ch skill)))
                (incf percent (skill-gain ch nil))
                (when (> percent (learned ch))
                  (decf percent (floor (- percent (learned ch))
                                       2)))
                (setf (skill-of ch skill)
                      percent)
                (when (>= (skill-of ch skill)
                          (learned ch))
                  (send-to-char ch "You are now well learned in that area.~%"))))))
         t)))))