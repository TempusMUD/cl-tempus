(in-package #:tempus)

(defmacro with-numeric-input (vars &body body)
  `(let ,(loop for var-tuple in vars
              as var = (first var-tuple)
            collect `(,var (parse-integer ,var :junk-allowed t)))
     (cond
       ,@(loop for var-tuple in vars
              as var = (first var-tuple)
              as message = (second var-tuple)
              as pred = (third var-tuple)
            if pred collect `((not (and ,var (funcall ,pred ,var))) (signal 'parser-error :message ,message))
            else collect  `((null ,var) (signal 'parser-error :message ,message)))
       (t
        ,@body))))

(defun can-edit-zone (ch zone &optional flag)
  (or (and (security-is-member ch "OLCWorldWrite")
           (pref-flagged ch +pref-worldwrite+))
      (and (or (and (security-is-member ch "OLCProofer")
                    (not (zone-approvedp zone)))
               (and (owner-idnum-of zone)
                    (= (owner-idnum-of zone) (idnum-of ch)))
               (and (co-owner-idnum-of zone)
                    (= (co-owner-idnum-of zone) (idnum-of ch))))
           (or (null flag)
               (zone-flagged zone +zone-fullcontrol+)
               (zone-flagged zone flag)))))

(defun check-is-editing (ch desc thing)
  (unless thing
    (send-to-char ch "You aren't editing a~:[~;n~] ~a.~%"
                  (find (char desc 0) '(#\a #\e #\i #\o #\u))
                  desc))
  thing)

(defun check-can-edit (ch zone flag)
  (let ((can-edit (can-edit-zone ch zone flag)))
    (unless can-edit
      (cond
        ((security-is-member ch "OLCWorldWrite")
         (send-to-char ch "You seem to be forgetting something...~%"))
        (t
         (send-to-char ch "Piss off, Beanhead.  Permission DENIED.~%")
         (slog "Failed attempt for ~a to edit zone ~d" (name-of ch) (number-of zone)))))

    can-edit))

(defun perform-set-string (ch input target desc vnum zone-approval-flag nil-allowed-p setter)
  (when (and (check-is-editing ch desc target)
             (check-can-edit ch (zone-containing-number vnum) zone-approval-flag))
    (cond
      ((and (string= input "~") nil-allowed-p)
       (funcall setter nil target)
       (send-to-char ch "~a unset.~%" desc))
      ((find #\~ input)
       (send-to-char ch "Tildes are not allowed in ~a ~a.~%" desc))
      (t
       (funcall setter input target)
       (send-to-char ch "~a set to '~a'.~%" desc input)))))

(defun perform-set-number (ch input target desc vnum zone-approval-flag min max setter)
  (when (and (check-is-editing ch desc target)
             (check-can-edit ch (zone-containing-number vnum) zone-approval-flag))
    (let ((num (parse-integer input :junk-allowed t)))
      (cond
        ((null num)
         (send-to-char ch "'~a' is an invalid ~a.~%" input desc))
        ((and min (< num min))
         (send-to-char ch "The ~a must be greater than ~d.~%" desc min))
        ((and max (> num max))
         (send-to-char ch "The ~a must be less than ~d.~%" desc max))
        (t
         (funcall setter num target)
         (send-to-char ch "~a set to ~d.~%" desc input))))))

(defun perform-set-enumerated (ch input target desc vnum zone-approval-flag allowed-values setter)
  (when (and (check-is-editing ch desc target)
             (check-can-edit ch (zone-containing-number vnum) zone-approval-flag))
    (let ((number (if (every #'digit-char-p input)
                      (parse-integer input)
                      (position input allowed-values :test 'string-abbrev))))
      (cond
        ((null number)
          (send-to-char ch "'~a' is an invalid ~a.~%" input desc))
        ((not (<= 0 number (1- (length allowed-values))))
         (send-to-char ch "~a is out of range for ~a.~%" number desc))
        (t
         (funcall setter number target)
         (send-to-char ch "~a set to ~(~a~).~%" desc (elt allowed-values number)))))))

(defun perform-set-text (ch target desc vnum zone-approval-flag text setter)
  (when (and (check-is-editing ch desc target)
             (check-can-edit ch (zone-containing-number vnum) zone-approval-flag))
    (let ((desc (format nil "~a ~a" (a-or-an desc) desc)))
      (act ch :place-emit (format nil "$n begins to edit ~a." desc))
      (setf (plr-bits-of ch) (logior (plr-bits-of ch) +plr-olc+))
      (start-text-editor (link-of ch)
                         target
                         desc
                         (or text "")
                         (lambda (cxn target buf)
                           (declare (ignore target))
                           (setf (plr-bits-of (actor-of cxn))
                                 (logandc2 (plr-bits-of (actor-of cxn)) +plr-olc+))
                           (setf (state-of cxn) 'playing)
                           (funcall setter buf))
                         (lambda (cxn target)
                           (declare (ignore target))
                           (setf (plr-bits-of (actor-of cxn))
                                 (logandc2 (plr-bits-of (actor-of cxn)) +plr-olc+))
                           (setf (state-of cxn) 'playing))))))

(defun perform-set-flags (ch plus-or-minus flag-names valid-flags target-desc usage getter setter)
  (if (or (string= plus-or-minus "+") (string= plus-or-minus "-"))
      (dolist (flag-name (split-sequence #\space flag-names :remove-empty-subseqs t))
        (let ((flag-id (position flag-name valid-flags :test 'string-abbrev)))
          (cond
            ((null flag-id)
             (send-to-char ch "'~a' is not a valid ~a flag.~%Valid flags: ~{  ~a~%~}"
                           flag-name
                           target-desc
                           (coerce valid-flags 'list)))
            ((string= plus-or-minus "-")
             (funcall setter (logandc2 (funcall getter) (ash 1 flag-id)))
             (send-to-char ch "Flag ~a unset on ~a flags.~%" (aref valid-flags flag-id) target-desc))
            (t
             (funcall setter (logior (funcall getter) (ash 1 flag-id)))
             (send-to-char ch "Flag ~a set on ~a flags.~%" (aref valid-flags flag-id) target-desc)))))
      (send-to-char ch "Usage: ~a~%" usage)))

(defun update-index-file (path number kind)
  (let ((entries (with-open-file (inf path)
                   (loop for line = (read-line inf nil nil)
                      while (and line (string/= line "$"))
                      collect line into result
                      finally (return (nreverse result))))))
    (pushnew (format nil "~d.~a" number kind)
             entries
             :test #'string=)
    (with-open-file (ouf path
                         :direction :output
                         :if-exists :rename-and-delete)
      (format ouf "~{~a~%~}$~%" (sort entries #'<
                                      :key (lambda (line)
                                             (parse-integer line :junk-allowed t)))))))

(defun perform-approve-zone (ch vnum-str approve-mobs-p approve-objs-p)
  (let* ((vnum (if (string= vnum-str ".")
                   (number-of (zone-of (in-room-of ch)))
                   (parse-integer vnum-str :junk-allowed t)))
         (zone (and vnum (real-zone vnum))))
    (cond
      ((null vnum)
       (send-to-char ch "You need to specify . or the zone number.~%"))
      ((null zone)
       (send-to-char ch "No such zone.~%"))
      (t
       (send-to-char ch "Zone approved for full inclusion in the game.~%")
       (send-to-char ch "Zone modification from this point on must be approved by an olc god.~%")
       (slog "~a approved zone ~a[~d]" (name-of ch) vnum)

       (setf (flags-of zone) (logandc2 (flags-of zone)
                                       (logior +zone-mobs-approved+
                                               +zone-objs-approved+
                                               +zone-rooms-approved+
                                               +zone-zcmds-approved+
                                               +zone-search-approved+)))
       (save-zone-data ch zone)

       (when approve-mobs-p
         (loop for mob-vnum from (* (number-of zone) 100) upto (top-of zone)
              as mob = (real-mobile-proto mob-vnum)
              when mob
              do (setf (mob2-flags-of mob) (logandc2 (mob2-flags-of mob)
                                                     +mob2-unapproved+)))
         (save-zone-objects ch zone)
         (send-to-char ch "Mobs approved for full inclusion in the game.~%")
         (slog "~a approved mobs in zone ~a[~d]" (name-of ch) (name-of zone) (number-of zone)))

       (when approve-objs-p
         (loop for obj-vnum from (* (number-of zone) 100) upto (top-of zone)
              as obj = (real-object-proto obj-vnum)
              when obj
              do (setf (extra2-flags-of obj) (logandc2 (extra2-flags-of obj)
                                                       +item2-unapproved+)))
         (save-zone-objects ch zone)
         (send-to-char ch "Objects approved for full inclusion in the game.~%")
         (slog "~a approved objects in zone ~a[~d]" (name-of ch) (name-of zone) (number-of zone)))))))

(defun perform-unapprove-zone (ch vnum-str mobiles-p objects-p)
  (let* ((vnum (if (string= vnum-str ".")
                   (number-of (zone-of (in-room-of ch)))
                   (parse-integer vnum-str :junk-allowed t)))
         (zone (and vnum (real-zone vnum))))
    (cond
      ((null vnum)
       (send-to-char ch "You need to specify . or the zone number.~%"))
      ((null zone)
       (send-to-char ch "No such zone.~%"))
      (t
       (send-to-char ch "Zone approved for olc.~%")
       (slog "~a unapproved zone ~a[~d]" (name-of ch) vnum)

       (setf (flags-of zone) (logior (flags-of zone)
                                     +zone-mobs-approved+
                                     +zone-objs-approved+
                                     +zone-rooms-approved+
                                     +zone-zcmds-approved+
                                     +zone-search-approved+))
       (save-zone-data ch zone)

       (when mobiles-p
         (loop for mob-vnum from (* (number-of zone) 100) upto (top-of zone)
            as mob = (real-mobile-proto mob-vnum)
            when mob
            do (setf (mob2-flags-of mob) (logior (mob2-flags-of mob)
                                                 +mob2-unapproved+)))
         (save-zone-objects ch zone)
         (send-to-char ch "Mobs approved for olc.~%")
         (slog "~a unapproved mobs in zone ~a[~d]" (name-of ch) (name-of zone) (number-of zone)))

       (when objects-p
         (loop for obj-vnum from (* (number-of zone) 100) upto (top-of zone)
            as obj = (real-object-proto obj-vnum)
            when obj
            do (setf (extra2-flags-of obj) (logior (extra2-flags-of obj)
                                                   +item2-unapproved+)))
         (save-zone-objects ch zone)
         (send-to-char ch "Objects approved for olc.~%")
         (slog "~a unapproved objects in zone ~a[~d]" (name-of ch) (name-of zone) (number-of zone)))))))

(defcommand (ch "worldwrite") (:immortal)
  (setf (bitp (prefs-of ch) +pref-worldwrite+) (not (bitp (prefs-of ch) +pref-worldwrite+)))
  (send-to-char ch "Worldwrite ~:[disabled~;enabled~].~%"
                (bitp (prefs-of ch) +pref-worldwrite+)))

(defcommand (ch "olc") (:immortal)
  (with-pagination ((link-of ch))
    (send-to-char ch "~
Usage:
olc clear <room | obj | mob>
olc create/destroy <room|zone|obj|mob|search> <number>
olc exit <direction> <parameter> <value> ['one-way']
olc medit [number | 'exit']
olc mload <vnum>
olc mmimic <vnum>
olc mset
olc mstat
olc oedit [number | 'exit']
olc oexdesc <create | remove | edit | addkey> <keywords> [new keywords
olc oload [number]
olc omimic <obj number>
olc oset <option> <value>
olc ostat [number]
olc rexdesc <create | remove | edit | addkey> <keywords> [new keywords
olc rmimic <room number> [all, sound, desc, exdesc, flags, sector, title]
olc rset <parameter> <arguments>
olc xedit <search trigger> <search keyword>
olc xset <arg> <val>
olc xstat
olc zcmd [zone] [cmdrenumber] [list] [cmdremove <number>] <M|O|P|R|D|E|G> <if_flag> [arg1] [arg2] [arg3]
olc zdoor <direction> [+/-] [OPEN|CLOSED|LOCKED|HIDDEN]
olc zequip <mob name> <item number> <max> <position> [prob]
olc zgive <mob name> <obj vnum> <max loaded> [prob]
olc zimplant <mob name> <item number> <max> <position> [prob]
olc zmob <mobile vnum> <max loaded> [prob]
olc zobj <object vnum> <max loaded> [prob]
olc zpath <'mob'|'obj'> <name> <path name>
olc zput <obj name> <obj vnum> <max loaded> [prob]
olc zreset/zpurge
olc zset [zone] <option> <value>
olc recalculate { obj | mob } <vnum>
olc msave
olc osave
olc rsave
olc zsave [zone]
")))

(defcommand (ch "olc" "create") (:immortal)
  (send-to-char ch "Usage: olc create (room|zone|object|mobile) (vnum|next)~%"))

(defcommand (ch "approve") (:immortal)
  (send-to-char ch "Usage: approve (zone|object|mobile) ...~%"))

(defcommand (ch "approve" "object") (:immortal)
  (send-to-char ch "Usage: approve object <vnum>~%"))

(defcommand (ch "approve" "object" vnum) (:immortal)
  (with-numeric-input ((vnum "That's no object vnum.~%"))
    (let* ((obj (real-object-proto vnum))
           (zone (zone-containing-number vnum)))
      (cond
        ((null obj)
         (send-to-char ch "There exists no object with that number, slick.~%"))
        ((null zone)
         (send-to-char ch "That object belongs to no zone.~%"))
        ((not (logtest (extra2-flags-of obj) +item2-unapproved+))
         (send-to-char ch "That item is already approved.~%"))
        (t
         (setf (extra2-flags-of obj) (logandc2 (extra2-flags-of obj) +item2-unapproved+))
         (send-to-char ch "Object approved for full inclusion in the game.~%")
         (slog "~a approved object ~a[~d]" (name-of ch) (name-of obj) (vnum-of obj))
         (save-zone-objects ch zone))))))

(defcommand (ch "approve" "mobile") (:immortal)
  (send-to-char ch "Usage: approve mobile <vnum>~%"))

(defcommand (ch "approve" "mobile" vnum) (:immortal)
  (with-numeric-input ((vnum "That's no mobile vnum.~%"))
    (let* ((mob (real-mobile-proto vnum))
           (zone (zone-containing-number vnum)))
      (cond
        ((null mob)
         (send-to-char ch "There exists no mobile with that number, slick.~%"))
        ((null zone)
         (send-to-char ch "That mobile belongs to no zone.~%"))
        ((not (logtest (mob2-flags-of mob) +mob2-unapproved+))
         (send-to-char ch "That item is already approved.~%"))
        (t
         (setf (mob2-flags-of mob) (logandc2 (mob2-flags-of mob) +mob2-unapproved+))
         (send-to-char ch "Mobile approved for full inclusion in the game.~%")
         (slog "~a approved mobile ~a[~d]" (name-of ch) (name-of mob) (vnum-of mob))
         (save-zone-mobiles ch zone))))))

(defcommand (ch "approve" "zone") (:immortal)
  (send-to-char ch "Usage: approve zone (.|<vnum>) [all|mobile|object]~%"))

(defcommand (ch "approve" "zone" vnum) (:immortal)
  (perform-approve-zone ch vnum nil nil))

(defcommand (ch "approve" "zone" vnum "all") (:immortal)
  (perform-approve-zone ch vnum t t))

(defcommand (ch "approve" "zone" vnum "mobile") (:immortal)
  (perform-approve-zone ch vnum t nil))

(defcommand (ch "approve" "zone" vnum "object") (:immortal)
  (perform-approve-zone ch vnum nil t))

(defcommand (ch "unapprove" "mobile") (:immortal)
  (send-to-char ch "Usage: unapprove mobile <vnum>~%"))

(defcommand (ch "unapprove" "mobile" vnum) (:immortal)
  (with-numeric-input ((vnum "That's no mobile vnum.~%"))
    (let* ((mob (real-mobile-proto vnum))
           (zone (zone-containing-number vnum)))
      (cond
        ((null mob)
         (send-to-char ch "There exists no mobile with that number, slick.~%"))
        ((null zone)
         (send-to-char ch "That mobile belongs to no zone.~%"))
        ((logtest (mob2-flags-of mob) +mob2-unapproved+)
         (send-to-char ch "That item is already approved.~%"))
        (t
         (setf (mob2-flags-of mob) (logior (mob2-flags-of mob) +mob2-unapproved+))
         (send-to-char ch "Mobile unapproved.~%")
         (slog "~a unapproved mobile ~a[~d]" (name-of ch) (name-of mob) (vnum-of mob))
         (save-zone-mobiles ch zone))))))

(defcommand (ch "unapprove" "object") (:immortal)
  (send-to-char ch "Usage: unapprove object <vnum>~%"))

(defcommand (ch "unapprove" "object" vnum) (:immortal)
  (with-numeric-input ((vnum "That's no object vnum.~%"))
    (let* ((obj (real-object-proto vnum))
           (zone (zone-containing-number vnum)))
      (cond
        ((null obj)
         (send-to-char ch "There exists no object with that number, slick.~%"))
        ((null zone)
         (send-to-char ch "That object belongs to no zone.~%"))
        ((logtest (extra2-flags-of obj) +item2-unapproved+)
         (send-to-char ch "That item is already unapproved.~%"))
        (t
         (setf (extra2-flags-of obj) (logior (extra2-flags-of obj) +item2-unapproved+))
         (send-to-char ch "Object unapproved.~%")
         (slog "~a unapproved object ~a[~d]" (name-of ch) (name-of obj) (vnum-of obj))
         (save-zone-objects ch zone))))))

(defcommand (ch "unapprove" "zone") (:immortal)
  (send-to-char ch "Usage: unapprove zone (.|<vnum>) [all|mobile|object]~%"))

(defcommand (ch "unapprove" "zone" vnum) (:immortal)
  (perform-unapprove-zone ch vnum nil nil))

(defcommand (ch "unapprove" "zone" vnum "all") (:immortal)
  (perform-unapprove-zone ch vnum t t))

(defcommand (ch "unapprove" "zone" vnum "mobile") (:immortal)
  (perform-unapprove-zone ch vnum t nil))

(defcommand (ch "unapprove" "zone" vnum "object") (:immortal)
  (perform-unapprove-zone ch vnum nil t))

(defcommand (ch "olc" "show") (:immortal)
  nil)