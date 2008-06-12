(in-package #:tempus)

(define-condition invalid-creature-file (error)
  ())

(defun xml-attr (node name &key numeric default numeric-with-nil hex)
  (let ((val (assoc name (second node) :test #'string=)))
    (cond
      ((null (second val))
       default)
      (numeric-with-nil
       (let ((i (parse-integer (second val))))
         (if (zerop i)
             nil
             i)))
      (numeric
       (parse-integer (second val)))
      (hex
       (parse-integer (second val) :radix 16))
      (t
       (second val)))))

(defun load-player-from-xml (idnum)
  (let ((ch (make-instance 'player))
        (xml (with-open-file (inf (player-pathname idnum))
               (xmls:parse inf))))
    (assert (string= (first xml) "creature") nil 'invalid-creature-file)
    (setf (idnum-of ch) (xml-attr xml "idnum" :numeric t))
    (setf (name-of ch) (xml-attr xml "name"))
    (dolist (node (cddr xml))
      (string-case (first node)
        ("points"
         (setf (hitp-of ch) (xml-attr node "hit" :numeric t))
         (setf (mana-of ch) (xml-attr node "mana" :numeric t))
         (setf (move-of ch) (xml-attr node "move" :numeric t))
         (setf (max-hitp-of ch) (xml-attr node "maxhit" :numeric t))
         (setf (max-mana-of ch) (xml-attr node "maxmana" :numeric t))
         (setf (max-move-of ch) (xml-attr node "maxmove" :numeric t)))
        ("money"
         (setf (exp-of ch) (xml-attr node "xp" :numeric t))
         (setf (cash-of ch) (xml-attr node "cash" :numeric t))
         (setf (gold-of ch) (xml-attr node "gold" :numeric t)))
        ("stats"
         (setf (alignment-of ch) (xml-attr node "align" :numeric t))
         (setf (weight-of ch) (xml-attr node "weight" :numeric t))
         (setf (height-of ch) (xml-attr node "height" :numeric t))
         (setf (race-of ch) (xml-attr node "race"))
         (setf (sex-of ch) (string-case (xml-attr node "sex")
                             ("Male" 'male)
                             ("Female" 'female)
                             ("Neuter" 'neuter)))
         (setf (level-of ch) (xml-attr node "level" :numeric t)))
        ("class"
         (setf (remort-gen-of ch) (xml-attr node "gen" :numeric t))
         (setf (char-class-of ch) (xml-attr node "name"))
         (setf (remort-char-class-of ch) (xml-attr node "remort")))
        ("time"
         (setf (login-time-of ch) (unix-to-timestamp
                                   (xml-attr node "last" :numeric t)))
         (setf (birth-time-of ch) (unix-to-timestamp
                                   (xml-attr node "birth" :numeric t)))
         (setf (death-time-of ch) (xml-attr node "death" :numeric-with-nil t))
         (setf (played-time-of ch) (xml-attr node "played" :numeric t)))
        ("carnage" nil)
        ("attr" nil)
        ("condition" nil)
        ("player" nil)
        ("rent" nil)
        ("home"
         (setf (home-room-of ch) (xml-attr node "homeroom" :numeric-with-nil t))
         (setf (load-room-of ch) (xml-attr node "loadroom" :numeric-with-nil t))
         (when (= (home-room-of ch) -1)
           (setf (home-room-of ch) nil)))
        ("quest" nil)
        ("bits" nil)
        ("prefs"
         (let ((prefs (make-array 64 :element-type 'bit)))
           (let ((num (xml-attr node "flag1" :hex t)))
             (dotimes (bit-index 32)
               (setf (bit prefs bit-index) (if (logbitp bit-index num) 1 0))))
           (let ((num (xml-attr node "flag2" :hex t)))
             (dotimes (bit-index 32)
               (setf (bit prefs (+ 30 bit-index)) (if (logbitp bit-index num) 1 0))))
           (setf (prefs-of ch) prefs)))
        ("affects" nil)
        ("immort" nil)
        ("poofin" nil)
        ("poofout" nil)
        ("alias" nil)))
    ch))
    