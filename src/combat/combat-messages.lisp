(in-package :tempus)

(defclass combat-message ()
  ((idnum :accessor idnum-of :initarg idnum)
   (attacker-die-emit :accessor attacker-die-emit-of :initarg :attacker-die-emit)
   (victim-die-emit :accessor victim-die-emit-of :initarg :victim-die-emit)
   (room-die-emit :accessor room-die-emit-of :initarg :room-die-emit)
   (attacker-miss-emit :accessor attacker-miss-emit-of :initarg :attacker-miss-emit)
   (victim-miss-emit :accessor victim-miss-emit-of :initarg :victim-miss-emit)
   (room-miss-emit :accessor room-miss-emit-of :initarg :room-miss-emit)
   (attacker-hit-emit :accessor attacker-hit-emit-of :initarg :attacker-hit-emit)
   (victim-hit-emit :accessor victim-hit-emit-of :initarg :victim-hit-emit)
   (room-hit-emit :accessor room-hit-emit-of :initarg :room-hit-emit)
   (attacker-god-emit :accessor attacker-god-emit-of :initarg :attacker-god-emit)
   (victim-god-emit :accessor victim-god-emit-of :initarg :victim-god-emit)
   (room-god-emit :accessor room-god-emit-of :initarg :room-god-emit)))

(defvar *combat-messages* (make-hash-table))

(defun load-messages ()
  (let ((path (tempus-path "lib/misc/messages")))
    (clrhash *combat-messages*)
    (with-open-file (inf path :direction :input)
      (let ((linenum 0)
            (message nil)
            (emits-left nil))
        (labels ((expect-m (line)
                   (unless (string= line "$")
                     (assert (string= line "M") nil
                             "~a:~d M expected, got ~s" path linenum line)
                     (setf message (make-instance 'combat-message))
                     #'expect-idnum))
                 (expect-idnum (line)
                   (let ((idnum (parse-integer line :junk-allowed t)))
                     (assert idnum nil
                             "~a:~d Integer expected, got ~s" path linenum line)
                     (setf (idnum-of message) idnum)
                     (setf emits-left '(attacker-die-emit
                                        victim-die-emit
                                        room-die-emit
                                        attacker-miss-emit
                                        victim-miss-emit
                                        room-miss-emit
                                        attacker-hit-emit
                                        victim-hit-emit
                                        room-hit-emit
                                        attacker-god-emit
                                        victim-god-emit
                                        room-god-emit))
                     #'expect-emit))
                 (expect-emit (line)
                   (setf (slot-value message (pop emits-left))
                         (unless (string= line "#")
                           line))
                   (cond
                     (emits-left
                      #'expect-emit)
                     (t
                      (push message (gethash (idnum-of message) *combat-messages*))
                      #'expect-m))))
          (loop
             with state = #'expect-m
             while state
             for line = (read-line inf)
             do
               (incf linenum)
               (unless (or (string= line "")
                           (string= line "*" :end1 1))
                 (setf state (funcall state line)))))))))


(defun can-go (ch dir)
  (let ((exit (exit ch dir)))
    (and (not (null exit))
         (not (logtest (exit-info-of exit) (logior +ex-closed+ +ex-nopass+)))
         (not (null (to-room-of exit))))))

(defun death-cry (ch)
  (cond
    ((and (is-npc ch)
          (eql (func-of (shared-of ch)) 'fate))
     (act ch :all-emit "$n dissipates in a cloud of mystery, leaving you to your fate."))
    ((or (is-ghoul ch)
         (is-wight ch)
         (is-mummy ch))
     (act ch :all-emit "$n falls lifeless to the floor with a shriek."))
    ((is-skeleton ch)
     (act ch :all-emit "$n clatters noisily into a lifeless heap."))
    ((or (is-ghost ch)
         (is-shadow ch)
         (is-wraith ch)
         (is-spectre ch))
     (act ch :all-emit "$n vanishes into the void with a terrible shriek."))
    ((is-vampire ch)
     (act ch :all-emit "$n screams as $e is consumed in a blazing fire!"))
    ((is-lich ch)
     (act ch :all-emit "A roar fills your ears as $n is ripped into the void!"))
    ((is-undead ch)
     (act ch :all-emit "Your skin crawls as you hear $n's final shriek."))
    (t
     (let ((bloodlust nil))
       (dolist (tch (people-of (in-room-of ch)))
         (cond
           ((and (not bloodlust)
                 (is-barb tch)
                 (zerop (random 2)))
            (setf bloodlust t)
            (act tch :target ch :subject-emit "You feel a rising bloodlust as you hear $n's death cry."))
           (t
            (act tch :target ch :subject-emit "Your blood freezes as you hear $N's death cry.")))))))

  (dotimes (dir +num-of-dirs+)
    (when (and (can-go ch dir)
               (exit ch dir)
               (plusp (to-room-of (exit ch dir)))
               (not (eql (exit ch dir) (in-room-of ch))))
      (let ((adjoining-room (real-room (to-room-of (exit ch dir)))))
        (send-to-room adjoining-room
                      "Your blood freezes as you hear someone's death cry from ~a."
                      (aref +from-dirs+ dir))
        (when (eql (in-room-of ch) (abs-exit adjoining-room (aref +rev-dir+ dir)))
          (dolist (tch (copy-list (people-of adjoining-room)))
            (when (and (is-npc tch)
                       (not (mob-flagged tch +mob-sentinel+))
                       (null (fighting-of tch))
                       (awakep tch)
                       (mob-flagged tch +mob-helper+)
                       (< (random-range 0 40) (level-of tch))
                       (or (not (room-flagged (in-room-of ch) +room-flame-filled+))
                           (char-withstands-fire tch))
                       (or (not (room-flagged (in-room-of ch) +room-ice-cold+))
                           (char-withstands-cold tch))
                       (can-see-room tch (in-room-of ch))
                       (zerop (random 3)))
              (do-simple-move tch (aref +rev-dir+ dir) :rush t))))))))

(defparameter +dam-weapons+
  '((0
     "$n tries to #w $N, but misses."
     "You try to #w $N, but miss."
     "$n tries to #w you, but misses.")
    (4
     "$n tickles $N as $e #W $M."
     "You tickle $N as you #w $M."
     "$n tickles you as $e #W you.")
    (6
     "$n barely #W $N."
     "You barely #w $N."
     "$n barely #W you.")
    (10
     "$n #W $N."
     "You #w $N."
     "$n #W you.")
    (14
     "$n #W $N hard."
     "You #w $N hard."
     "$n #W you hard.")
    (19
     "$n #W $N very hard."
     "You #w $N very hard."
     "$n #W you very hard.")
    (23
     "$n #W $N extremely hard."
     "You #w $N extremely hard."
     "$n #W you extremely hard.")
    (27
     "$n massacres $N to small fragments with $s #w."
     "You massacre $N to small fragments with your #w."
     "$n massacres you to small fragments with $s #w.")
    (32
     "$n devastates $N with $s incredible #w!!"
     "You devastate $N with your incredible #w!!"
     "$n devastates you with $s incredible #w!!")
    (37
     "$n OBLITERATES $N with $s deadly #w!!"
     "You OBLITERATE $N with your deadly #w!!"
     "$n OBLITERATES you with $s deadly #w!!")
    (45
     "$n utterly DEMOLISHES $N with $s unbelievable #w!!"
     "You utterly DEMOLISH $N with your unbelievable #w!!"
     "$n utterly DEMOLISHES you with $s unbelievable #w!!")
    (69
     "$n PULVERIZES $N with $s vicious #w!!"
     "You PULVERIZE $N with your vicious #w!!"
     "$n PULVERIZES you with $s vicious #w!!")
    (99
     "$n *DECIMATES* $N with $s horrible #w!!"
     "You *DECIMATE* $N with your horrible #w!!"
     "$n *DECIMATES* you with $s horrible #w!!")
    (139
     "$n *LIQUIFIES* $N with $s incredibly vicious #w!!"
     "You **LIQUIFY** $N with your incredibly vicious #w!!"
     "$n *LIQUIFIES* you with $s incredibly vicious #w!!")
    (189
     "$n **VAPORIZES** $N with $s terrible #w!!"
     "You **VAPORIZE** $N with your terrible #w!!"
     "$n **VAPORIZES** you with $s terrible #w!!")
    (9999
     "$n **ANNIHILATES** $N with $s ultra powerful #w!!"
     "You **ANNIHILATE** $N with your ultra powerful #w!!"
     "$n **ANNIHILATES** you with $s ultra powerful #w!!")))

(defparameter +dam-weapons-2+
  '((0
     "$n tries to #w $N with $p, but misses."
     "You try to #w $N with $p, but miss."
     "$n tries to #w you with $p, but misses.")
    (4
     "$n tickles $N as $e #W $M with $p."
     "You tickle $N as you #w $M with $p."
     "$n tickles you as $e #W you with $p.")
    (6
     "$n barely #W $N with $p."
     "You barely #w $N with $p."
     "$n barely #W you with $p.")
    (10
     "$n #W $N with $p."
     "You #w $N with $p."
     "$n #W you with $p.")
    (14
     "$n #W $N hard with $p."
     "You #w $N hard with $p."
     "$n #W you hard with $p.")
    (19
     "$n #W $N very hard with $p."
     "You #w $N very hard with $p."
     "$n #W you very hard with $p.")
    (23
     "$n #W $N extremely hard with $p."
     "You #w $N extremely hard with $p."
     "$n #W you extremely hard with $p.")
    (27
     "$n massacres $N to small fragments with $p."
     "You massacre $N to small fragments with $p."
     "$n massacres you to small fragments with $p.")
    (32
     "$n devastates $N with $s incredible #w!!"
     "You devastate $N with a #w from $p!!"
     "$n devastates you with $s incredible #w!!")
    (37
     "$n OBLITERATES $N with $p!!"
     "You OBLITERATE $N with $p!!"
     "$n OBLITERATES you with $p!!")
    (45
     "$n deals a DEMOLISHING #w to $N with $p!!"
     "You deal a DEMOLISHING #w to $N with $p!!"
     "$n deals a DEMOLISHING #w to you with $p!!")
    (79
     "$n PULVERIZES $N with $p!!"
     "You PULVERIZE $N with $p!!"
     "$n PULVERIZES you with $p!!")
    (99
     "$n *DECIMATES* $N with $p!!"
     "You *DECIMATE* $N with $p!!"
     "$n *DECIMATES* you with $p!!")
    (139
     "$n *LIQUIFIES* $N with a #w from $p!!"
     "You **LIQUIFY** $N with a #w from $p!!"
     "$n *LIQUIFIES* you with a #w from $p")
    (189
     "$n **VAPORIZES** $N with $p!!"
     "You **VAPORIZE** $N with a #w from $p!!"
     "$n **VAPORIZES** you with a #w from $p!!")
    (9999
     "$n **ANNIHILATES** $N with $p!!"
     "You **ANNIHILATE** $N with your #w from $p!!"
     "$n **ANNIHILATES** you with $s #w from $p!!")))

(defparameter +dam-weapons-location+
  '((0
     "$n tries to #w $N's #p, but misses."
     "You try to #w $N's #p, but miss."
     "$n tries to #w your #p, but misses.")
    (4
     "$n tickles $N's #p as $e #W $M."
     "You tickle $N's #p as you #w $M."
     "$n tickles you as $e #W your #p.")
    (6
     "$n barely #W $N's #p."
     "You barely #w $N's #p."
     "$n barely #W your #p.")
    (10
     "$n #W $N's #p."
     "You #w $N's #p."
     "$n #W your #p.")
    (14
     "$n #W $N's #p hard."
     "You #w $N's #p hard."
     "$n #W your #p hard.")
    (19
     "$n #W $N's #p very hard."
     "You #w $N's #p very hard."
     "$n #W your #p very hard.")
    (23
     "$n #W $N's #p extremely hard."
     "You #w $N's #p extremely hard."
     "$n #W your #p extremely hard.")
    (27
     "$n massacres $N's #p to fragments with $s #w."
     "You massacre $N's #p to small fragments with your #w."
     "$n massacres your #p to small fragments with $s #w.")
    (32
     "$n devastates $N's #p with $s incredible #w!!"
     "You devastate $N's #p with your incredible #w!!"
     "$n devastates your #p with $s incredible #w!!")
    (37
     "$n OBLITERATES $N's #p with $s #w!!"
     "You OBLITERATE $N's #p with your #w!!"
     "$n OBLITERATES your #p with $s #w!!")
    (45
     "$n deals a DEMOLISHING #w to $N's #p!!"
     "You deal a DEMOLISHING #w to $N's #p!!"
     "$n deals a DEMOLISHING #w to your #p!!")
    (69
     "$n PULVERIZES $N's #p with $s vicious #w!!"
     "You PULVERIZE $N's #p with your vicious #w!!"
     "$n PULVERIZES your #p with $s vicious #w!!")
    (99
     "$n *DECIMATES* $N's #p with $s horrible #w!!"
     "You *DECIMATE* $N's #p with your horrible #w!!"
     "$n *DECIMATES* your #p with $s horrible #w!!")
    (139
     "$n *LIQUIFIES* $N's #p with $s vicious #w!!"
     "You **LIQUIFY** $N's #p with your vicious #w!!"
     "$n *LIQUIFIES* your #p with $s vicious #w!!")
    (189
     "$n **VAPORIZES** $N's #p with $s terrible #w!!"
     "You **VAPORIZE** $N's #p with your terrible #w!!"
     "$n **VAPORIZES** your #p with $s terrible #w!!")
    (9999
     "$n **ANNIHILATES** $N's #p with $s ultra #w!!"
     "You **ANNIHILATE** $N's #p with your ultra #w!!"
     "$n **ANNIHILATES** your #p with $s ultra #w!!")))

(defparameter +dam-guns+
  '((0
     "$n tries to #w $N with $p, but misses."
     "You try to #w $N with $p, but miss."
     "$n tries to #w you with $p, but misses.")
    (4
     "$n grazes $N with a #w from $p."
     "You graze $N as you #w at $M with $p."
     "$n grazes you as $e #W you with $p.")
    (6
     "$n barely #W $N with $p."
     "You barely #w $N with $p."
     "$n barely #W you with $p.")
    (10
     "$n #W $N with $p."
     "You #w $N with $p."
     "$n #W you with $p.")
    (14
     "$n #W $N hard with $p."
     "You #w $N hard with $p."
     "$n #W you hard with $p.")
    (19
     "$n #W $N very hard with $p."
     "You #w $N very hard with $p."
     "$n #W you very hard with $p.")
    (23
     "$n #W the hell out of $N with $p."
     "You #w the hell out of $N with $p."
     "$n #W the hell out of you with $p.")
    (27
     "$n #W $N to small fragments with $p."
     "You #w $N to small fragments with $p."
     "$n #W you to small fragments with $p.")
    (32
     "$n devastates $N with a #w from $p!!"
     "You devastate $N with a #W from $p!!"
     "$n devastates you with $s #w from $p!!")
    (37
     "$n OBLITERATES $N with a #w from $p!!"
     "You OBLITERATE $N with a #w from $p!!"
     "$n OBLITERATES you with a #w from $p!!")
    (45
     "$n DEMOLISHES $N with a dead on #w!!"
     "You DEMOLISH $N with a dead on blast from $p!!"
     "$n DEMOLISHES you with a dead on #w from $p!!")
    (79
     "$n PULVERIZES $N with a #w from $p!!"
     "You PULVERIZE $N with a #w from $p!!"
     "$n PULVERIZES you with a #w from $p!!")
    (99
     "$n *DECIMATES* $N with a #w from $p!!"
     "You *DECIMATE* $N with a #w from $p!!"
     "$n *DECIMATES* you with a #w from $p!!")
    (139
     "$n *LIQUIFIES* $N with a #w from $p!!"
     "You **LIQUIFY** $N with a #w from $p!!"
     "$n *LIQUIFIES* you with a #w from $p")
    (189
     "$n **VAPORIZES** $N with $p!!"
     "You **VAPORIZE** $N with a #w from $p!!"
     "$n **VAPORIZES** you with a #w from $p!!")
    (9999
     "$n **ANNIHILATES** $N with $p!!"
     "You **ANNIHILATE** $N with your #w from $p!!"
     "$n **ANNIHILATES** you with $s #w from $p!!")))

(defparameter +dam-energy-guns+
  '((0
     "$n misses $N with #s."
     "You miss $N with #s."
     "$n misses you with #s.")
    (4
     "$n grazes $N with #s."
     "You graze $N with #s."
     "$n grazes you with #s.")
    (6
     "$n barely marks $N with $p."
     "You barely mark $N with #s."
     "$n barely marks you with #s.")
    (10
     "$n hurts $N with #s."
     "You hurt $N with #s."
     "$n hurts you with #s.")
    (14
     "$n hurts $N badly with #s."
     "You hurt $N badly with #s."
     "$n hurts you badly with #s.")
    (19
     "$n hurts $N very badly with #s."
     "You hurt $N very badly with #s."
     "$n hurts you very badly with #s.")
    (23
     "$n ravages $N with #s!!"
     "You ravage $N with #s!"
     "$n ravages you with #s!")
    (27
     "$n massacre $N with #s!"
     "You massacre $N with #s!"
     "$n massacres you with #s!")
    (32
     "$n devastates $N with #s!"
     "You devastate $N with #s!"
     "$n devastates you with #s!")
    (37
     "$n OBLITERATES $N with #s!!"
     "You OBLITERATE $N with #s!!"
     "$n OBLITERATES you with #s!!")
    (45
     "$n DEMOLISHES $N with #s!!"
     "You DEMOLISH $N with #s!!"
     "$n DEMOLISHES you with #s!!")
    (79
     "$n PULVERIZES $N with #s!!"
     "You PULVERIZE $N with #s!!"
     "$n PULVERIZES you with #s!!")
    (99
     "$n *DECIMATES* $N with #s!!"
     "You *DECIMATE* $N with #s!!"
     "$n *DECIMATES* you with #s!!")
    (139
     "$n *LIQUIFIES** $N with #s!!"
     "You *LIQUIFY* $N with #s!!"
     "$n *LIQUIFIES* you with #s!!")
    (189
     "$n **VAPORIZES** $N with #s!!"
     "You **VAPORIZE** $N with #s!!"
     "$n **VAPORIZES** you with #s!!")
    (9999
     "$n **ANNIHILATES** $N with #s!!"
     "You **ANNIHILATE** $N with #s!!"
     "$n **ANNIHILATES** you with #s!!")))

(defparameter +dam-energy-guns2+
  '((0
     "#s misses $N as $n #W $p."
     "#s misses $N as you #w $p."
     "#s misses you as $n #W $p.")
    (4
     "#s grazes $N as $n #W $p."
     "#s grazes $N as you #w $p."
     "#s grazes you as $n #W $p.")
    (6
     "#s barely marks $N as $n #W $p."
     "#s barely marks $N as you #w $p."
     "#s barely marks you as $n #W $p.")
    (10
     "#s hurts $N as $n #W $p."
     "#s hurts $N as you #w $p."
     "#s hurts you as $n #W $p.")
    (14
     "#s hurts $N badly as $n #W $p."
     "#s hurts $N badly as you #w $p."
     "#s hurts you badly as $n #W $p.")
    (19
     "#s hurts $N very badly as $n #W $p."
     "#s hurts $N very badly as you #w $p."
     "#s hurts you very badly as $n #W $p.")
    (23
     "#s ravages $N as $n #W $p."
     "#s ravages $N as you #w $p."
     "#s ravages you as $n #W $p.")
    (27
     "#s massacres $N as $n #W $p."
     "#s massacres $N as you #w $p."
     "#s massacres you as $n #W $p.")
    (32
     "#s devastates $N as $n #W $p!"
     "#s devastates $N as you #w $p!"
     "#s devastates you as $n #W $p!")
    (37
     "#s OBLITERATES $N as $n #W $p!"
     "#s OBLITERATES $N as you #w $p!"
     "#s OBLITERATES you as $n #W $p!")
    (45
     "#s DEMOLISHES $N as $n #W $p!"
     "#s DEMOLISHES $N as you #w $p!"
     "#s DEMOLISHES you as $n #W $p!")
    (79
     "#s PULVERIZES $N as $n #W $p!"
     "#s PULVERIZES $N as you #w $p!"
     "#s PULVERIZES you as $n #W $p!")
    (99
     "#s *DECIMATES* $N as $n #W $p!"
     "#s *DECIMATES* $N as you #w $p!"
     "#s *DECIMATES* you as $n #W $p!")
    (139
     "#s *LIQUIFIES* $N as $n #W $p!!"
     "#s *LIQUIFIES* $N as you #w $p!!"
     "#s *LIQUIFIES* you as $n #W $p!!")
    (189
     "#s **VAPORIZES** $N as $n #W $p!!"
     "#s **VAPORIZES** $N as you #w $p!!"
     "#s **VAPORIZES** you as $n #W $p!!")
    (9999
     "#s **ANNIHILATES** $N as $n #W $p!!"
     "#s **ANNIHILATES** $N as you #w $p!!"
     "#s **ANNIHILATES** you as $n #W $p!!")))

(defparameter +dam-energy-guns3+
  '((0
     "$n misses $N with $p's #S."
     "You miss $N with $p's #S."
     "$n misses you with $p's #S.")
    (4
     "$n grazes $N with $p's #S."
     "You graze $N with $p's #S."
     "$n grazes you with $p's #S.")
    (6
     "$n barely marks $N with $p's #S."
     "You barely mark $N with $p's #S."
     "$n barely marks you with $p's #S.")
    (10
     "$n hurts $N with $p's #S."
     "You hurt $N with $p's #S."
     "$n hurts you with $p's #S.")
    (14
     "$n hurts $N badly with $p's #S."
     "You hurt $N badly with $p's #S."
     "$n hurts you badly with $p's #S.")
    (19
     "$n hurts $N very badly with $p's #S."
     "You hurt $N very badly with $p's #S."
     "$n hurts you very badly with $p's #S.")
    (23
     "$n ravages $N with $p's #S."
     "You ravage $N with $p's #S."
     "$n ravages you with $p's #S.")
    (27
     "$n massacres $N with $p's #S."
     "You massacre $N with $p's #S."
     "$n massacres you with $p's #S.")
    (32
     "$n devastates $N with $p's #S!"
     "You devastate $N with $p's #S!"
     "$n devastates you with $p's #S!")
    (37
     "$n OBLITERATES $N with $p's #S!"
     "You OBLITERATE $N with $p's #S!"
     "$n OBLITERATES you with $p's #S!")
    (45
     "$n DEMOLISHES $N with $p's #S!"
     "You DEMOLISH $N with $p's #S!"
     "$n DEMOLISHES you with $p's #S!")
    (79
     "$n PULVERIZES $N with $p's #S!"
     "You PULVERIZE $N with $p's #S!"
     "$n PULVERIZES you with $p's #S!")
    (99
     "$n **DECIMATES** $N with $p's #S!"
     "You **DECIMATE** $N with $p's #S!"
     "$n **DECIMATES** you with $p's #S!")
    (139
     "$n **LIQUIFIES** $N with $p's #S!!"
     "You **LIQUIFY** $N with $p's #S!!"
     "$n **LIQUIFIES** you with $p's #S!!")
    (189
     "$n **VAPORIZES** $N with $p's #S!!"
     "You **VAPORIZE** $N with $p's #S!!"
     "$n **VAPORIZES** you with $p's #S!!")
    (9999
     "$n **ANNIHILATES** $N with $p's #S!!"
     "You **ANNIHILATE** $N with $p's #S!!"
     "$n **ANNIHILATES** you with $p's #S!!")))

(defun generic-template (str escape-char codes &rest replacements)
  "Returns a string where the characters in CODES preceded by
  ESCAPE-CHAR are replaced in STR by their corresponding REPLACEMENTS.
  If ESCAPE-CHAR appears in STR twice, it is replaced by a single
  instance of the ESCAPE-CHAR."
  (with-input-from-string (in str)
    (with-output-to-string (result)
      (loop
           with escaped = nil
         for c across str do
           (cond
             ((and (not escaped) (char= c escape-char))
              (setf escaped t))
             ((not escaped)
              (princ c result))
             ((char= c escape-char)
              (princ c result)
              (setf escaped nil))
             (t
              (let ((code-pos (position c codes)))
                (if code-pos
                    (write-string (elt replacements code-pos)
                                  result)
                    (format result "<ILLEGAL CODE ~a>" c))
                (setf escaped nil))))))))

(defun build-damage-messages (templates singular plural location substance)
  (loop for template in templates
       collect (generic-template template #\# "wWsSp"
                    singular
                    plural
                    (a-or-an substance)
                    substance
                    location)))

(defun pos-damage-ok (location)
  (not (member location
               (list +wear-light+
                     +wear-about+
                     +wear-hold+
                     +wear-belt+
                     +wear-wield+
                     +wear-wield-2+
                     +wear-ass+))))

(defun select-damage-message (weapon hit-location)
  (cond
    ((and weapon (is-energy-gun weapon))
     (random-elt
      (list +dam-energy-guns+ +dam-energy-guns2+ +dam-energy-guns3+)))
    ((and weapon (is-gun weapon))
     +dam-guns+)
    ((and (plusp hit-location)
          (pos-damage-ok hit-location)
          (randomly-true 3)
          (or (null weapon)
              (eql (worn-on-of weapon)
                   +wear-wield+)))
     +dam-weapons-location+)
    ((and weapon
          (or (randomly-true 3)
              (not (eql (worn-on-of weapon)
                        +wear-wield+))))
     +dam-weapons-2+)
    (t
     +dam-weapons+)))

(defvar *search-nomessage* nil)

(defun damage-message (ch victim amount weapon kind hit-location)
  (unless *search-nomessage*
    (let* ((gun-type (if (and weapon (is-gun weapon))
                         (gun-type weapon)
                         0))
           (messages (build-damage-messages
                      (rest (assoc amount (select-damage-message weapon hit-location) :test '<=))
                      (aref +attack-hit-text+ (- kind +type-hit+) 0)
                      (aref +attack-hit-text+ (- kind +type-hit+) 1)
                      (aref +wear-keywords+ (aref +wear-translator+ hit-location))
                      (aref +gun-hit-text+ gun-type 2))))

      (act ch :target victim :item weapon :not-target-emit (first messages))
      (unless (and (zerop amount) (pref-flagged ch +pref-gagmiss+))
        (act ch :target victim :item weapon
             :subject-emit (format nil "~a~a&n"
                                   (if (= hit-location +wear-mshield+)
                                       "&m" "&y")
                                   (second messages))))
      (unless (and (zerop amount) (pref-flagged victim +pref-gagmiss+))
        (act ch :target victim :item weapon
             :target-emit (format nil "&r~a&n"(third messages)))))))

(defun bloodlet (damage kind)
  nil)

(defun blood-spray (ch victim kind)
  nil)

(defun skill-message (ch victim damage weapon kind)
  (unless *search-nomessage*
    (let ((msg (random-elt (gethash kind *combat-messages*))))
      (cond
        ((null msg)
         nil)
        ((and (is-pc victim)
              (immortalp victim)
              (plusp damage))
         (act ch :target victim :item weapon
              :subject-emit (attacker-god-emit-of msg)
              :target-emit (victim-god-emit-of msg)
              :not-target-emit (room-god-emit-of msg)))
        ((= damage 0)
         (when (and ch (or (not (is-weapon kind))
                           (not (pref-flagged ch +pref-gagmiss+))))
           (send-to-char ch "&y")
           (act ch :target victim :item weapon
                :subject-emit (attacker-miss-emit-of msg))
           (send-to-char ch "&n"))
         (when (and (not (eql ch victim))
                    (or (not (is-weapon kind))
                        (not (pref-flagged victim +pref-gagmiss+))))
           (send-to-char victim "&r")
           (act ch :target victim :item weapon
                :target-emit (victim-miss-emit-of msg))
           (send-to-char victim "&n"))
         (act ch :target victim :item weapon
              :not-target-emit (room-miss-emit-of msg)))
        ((eql (position-of victim) +pos-dead+)
         (when ch
           (send-to-char ch "&y")
           (act ch :target victim :item weapon
                :subject-emit (attacker-die-emit-of msg))
           (send-to-char ch "&n"))
         (when (not (eql ch victim))
           (send-to-char victim "&r")
           (act ch :target victim :item weapon
                :target-emit (victim-die-emit-of msg))
           (send-to-char victim "&n"))
         (act ch :target victim :item weapon
              :not-target-emit (room-die-emit-of msg)))
        (t
         (when ch
           (send-to-char ch "&y")
           (act ch :target victim :item weapon
                :subject-emit (attacker-hit-emit-of msg))
           (send-to-char ch "&n"))
         (when (not (eql ch victim))
           (send-to-char victim "&r")
           (act ch :target victim :item weapon
                :target-emit (victim-hit-emit-of msg))
           (send-to-char victim "&n"))
         (act ch :target victim :item weapon
              :not-target-emit (room-hit-emit-of msg)))))
    (when (bloodlet damage kind)
      (blood-spray ch victim kind))))
