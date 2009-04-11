(in-package #:tempus)

(defun push-zcmd (zone insert-after cmd if-flag arg1 arg2 arg3 prob)
  "Adds a new RESET-COM to the zone's command list.  If INSERT-AFTER is NIL, the new command is appended to the end of the command list, otherwise it is inserted after the given cons."
  (let ((new-reset-com (make-instance 'reset-com
                                      :command cmd
                                      :if-flag if-flag
                                      :arg1 arg1
                                      :arg2 arg2
                                      :arg3 arg3
                                      :prob prob)))
    (if insert-after
        (setf (cdr insert-after) (cons new-reset-com (cdr insert-after)))
        (setf (cmds-of zone) (append (cmds-of zone) (list new-reset-com))))))

(defun make-range-p (min max)
  (lambda (i) (<= min i max)))

(defun save-zone-data (ch zone)
  (let ((context nil))
    (handler-case
        (progn
          (setf context "zone index")
          (update-index-file (tempus-path "lib/world/zon/index") (number-of zone) "zon")

          (setf context "zone file")
          (with-open-file (ouf (tempus-path "lib/world/zon/~d.zon" (number-of zone))
                               :direction :output
                               :if-exists :rename-and-delete
                               :if-does-not-exist :create)
            (format ouf "#~d~%~a~~~%" (number-of zone) (name-of zone))
            (format ouf "~@[owner: ~d~%~]~@[co-owner: ~d~%~]"
                    (owner-idnum-of zone)
                    (co-owner-idnum-of zone))
            (format ouf "~@[respawn-pt: ~d~%~]" (respawn-pt-of zone))
            (format ouf "~@[minimum-level: ~d~%~]~@[minimum-gen: ~d~%~]"
                    (min-lvl-of zone) (min-gen-of zone))
            (format ouf "~@[maximum-level: ~d~%~]~@[maximum-gen: ~d~%~]"
                    (max-lvl-of zone) (max-gen-of zone))
            (format ouf "~@[public-desc:~%~a~~~%~]~@[private-desc:~%~a~~~%~]"
                    (public-desc-of zone) (private-desc-of zone))
            (format ouf "~@[author: ~a~%~]" (author-of zone))
            (format ouf "~d ~d ~d ~d ~d ~a ~d ~d ~d~%"
                    (top-of zone)
                    (lifespan-of zone)
                    (reset-mode-of zone)
                    (time-frame-of zone)
                    (plane-of zone)
                    (bits-to-asciiflag (flags-of zone))
                    (hour-mod-of zone)
                    (year-mod-of zone)
                    (pk-style-of zone))

            (dolist (zcmd (cmds-of zone))
              (if (eql (command-of zcmd) #\D)
                  (format ouf "D ~d ~3d ~5d ~5d ~5@a~%"
                          (if-flag-of zcmd)
                          (prob-of zcmd)
                          (arg1-of zcmd)
                          (arg2-of zcmd)
                          (bits-to-asciiflag (arg3-of zcmd)))
                  (format ouf "~c ~d ~3d ~5d ~5d ~5d        ~a~%"
                          (command-of zcmd)
                          (if-flag-of zcmd)
                          (prob-of zcmd)
                          (arg1-of zcmd)
                          (arg2-of zcmd)
                          (arg3-of zcmd)
                          (case (command-of zcmd)
                            (#\M
                             (let ((mob (real-mobile-proto (arg1-of zcmd))))
                               (if mob (name-of mob) "BOGUS")))
                            ((#\O #\E #\I #\P #\G)
                             (let ((obj (real-object-proto (arg1-of zcmd))))
                               (if obj (name-of obj) "BOGUS")))
                            (t
                             " ---")))))
            (format ouf "*~%$~%"))

          (slog "OLC: ~a zsaved ~d" (name-of ch) (number-of zone))
          t)
      (error (err)
        (slog "Error while saving ~a: ~a!" context err)
        nil))))

(defun perform-create-zone (ch number)
  (cond
    ((not (< 0 number 999))
     (send-to-char ch "ERROR: Zone number must be between 0 and 999.~%"))
    ((find number *zone-table* :key 'number-of)
     (send-to-char ch "ERROR: Zone already exists.~%"))
    ((some (lambda (zone)
             (and (< (number-of zone) number)
                  (> (top-of zone) (* number 100))))
           *zone-table*)
     (send-to-char ch "ERROR: Zone overlaps existing zone.~%"))
    (t
     (let ((new-zone (make-instance 'zone-data
                                    :name "A Freshly made Zone"
                                    :top (+ (* number 100) 99)
                                    :number number)))
       (setf (weather-of new-zone) (make-instance 'weather-data))
       (setf *zone-table* (sort (cons new-zone *zone-table*) #'< :key 'number-of))
       (save-zone-data ch new-zone))
     (send-to-char ch "Zone ~d structure created OK.~%" number)
     (slog "Zone ~d created by ~a" number (name-of ch)))))

(defun perform-zcmd-list (ch zone options start end)
  (with-pagination ((link-of ch))
    (let* ((print-objects (find :objects options))
           (print-removes (find :removes options))
           (print-mobiles (find :mobiles options))
           (print-equips (find :equips options))
           (print-implants (find :implants options))
           (print-gives (find :gives options))
           (print-puts (find :puts options))
           (print-doors (find :doors options))
           (print-paths (find :paths options)))
      (loop
         for cmd in (nthcdr start (cmds-of zone))
         as lineno from start
         until (or (null cmd) (and end (eql lineno (1+ end)))) do
           (case (command-of cmd)
             (#\M
              (when print-mobiles
                (let ((mob (real-mobile-proto (arg1-of cmd))))
                  (send-to-char ch "~3d. &yMobile&n: ~d [~3d] ~5d to   ~5d, max ~3d: (&y~a&n)~%"
                                lineno
                                (if-flag-of cmd)
                                (prob-of cmd)
                                (arg1-of cmd)
                                (arg3-of cmd)
                                (arg2-of cmd)
                                (if mob (name-of mob) "null-desc")))))
             (#\O
              (when print-objects
                (let ((obj (real-object-proto (arg1-of cmd))))
                  (send-to-char ch "~3d. &gObject&n: ~d [~3d] ~5d to   ~5d, max ~3d: (&g~a&n)~%"
                                lineno
                                (if-flag-of cmd)
                                (prob-of cmd)
                                (arg1-of cmd)
                                (arg3-of cmd)
                                (arg2-of cmd)
                                (if obj (name-of obj) "null-desc")))))
             (#\P
              (when print-puts
                (let ((obj (real-object-proto (arg1-of cmd))))
                  (send-to-char ch "~3d.    &BPut&n: ~d [~3d] ~5d to   ~5d, max ~3d: (&g~a&n)~%"
                                lineno
                                (if-flag-of cmd)
                                (prob-of cmd)
                                (arg1-of cmd)
                                (arg3-of cmd)
                                (arg2-of cmd)
                                (if obj (name-of obj) "null-desc")))))
             (#\G
              (when print-gives
                (let ((obj (real-object-proto (arg1-of cmd))))
                  (send-to-char ch "~3d.   &BGive&n: ~d [~3d] ~5d to   ~5d, max ~3d: (&g~a&n)~%"
                                lineno
                                (if-flag-of cmd)
                                (prob-of cmd)
                                (arg1-of cmd)
                                (arg3-of cmd)
                                (arg2-of cmd)
                                (if obj (name-of obj) "null-desc")))))
             (#\E
              (when print-equips
                (let ((obj (real-object-proto (arg1-of cmd))))
                  (send-to-char ch "~3d.  &mEquip&n: ~d [~3d] ~5d to   ~5d, max ~3d: (&g~a&n)~%"
                                lineno
                                (if-flag-of cmd)
                                (prob-of cmd)
                                (arg1-of cmd)
                                (arg3-of cmd)
                                (arg2-of cmd)
                                (if obj (name-of obj) "null-desc")))))
             (#\I
              (when print-implants
                (let ((obj (real-object-proto (arg1-of cmd))))
                  (send-to-char ch "~3d.&mImplant&n: ~d [~3d] ~5d to   ~5d, max ~3d: (&g~a&n)~%"
                                lineno
                                (if-flag-of cmd)
                                (prob-of cmd)
                                (arg1-of cmd)
                                (arg3-of cmd)
                                (arg2-of cmd)
                                (if obj (name-of obj) "null-desc")))))
             (#\R
              (when print-removes
                (let ((obj (real-object-proto (arg1-of cmd))))
                  (send-to-char ch "~3d.&rRem Obj&n: ~d [~3d] ~5d from ~5d,          (&g~a&n)~%"
                                lineno
                                (if-flag-of cmd)
                                (prob-of cmd)
                                (arg1-of cmd)
                                (arg2-of cmd)
                                (if obj (name-of obj) "null-desc")))))
             (#\D
              (when print-doors
                (send-to-char ch "~3d.   &cDoor&n: ~d [~3d] ~5d dir  ~5a,          (~a)~%"
                              lineno
                              (if-flag-of cmd)
                              (prob-of cmd)
                              (arg1-of cmd)
                              (aref +dirs+ (arg2-of cmd))
                              (printbits (arg3-of cmd) +exit-bits+ "NONE"))))
             (#\V
              (when print-paths
                (send-to-char ch "~3d.   &YPath&n: ~d [~3d] ~5d   to obj ~5d     : (&c~a&n)~%"
                              lineno
                              (if-flag-of cmd)
                              (prob-of cmd)
                              (arg1-of cmd)
                              (arg3-of cmd)
                              (path-name-by-vnum (arg1-of cmd)))))
             (#\W
              (when print-paths
                (send-to-char ch "~3d.   &YPath&n: ~d [~3d] ~5d   to mob ~5d     : (&c~a&n)~%"
                              lineno
                              (if-flag-of cmd)
                              (prob-of cmd)
                              (arg1-of cmd)
                              (arg3-of cmd)
                              (path-name-by-vnum (arg1-of cmd)))))
             (t
              (send-to-char ch "~3d.   ~c: ~d [~3d] ~5d     ~5d    ~5d~%"
                            lineno
                            cmd
                            (if-flag-of cmd)
                            (prob-of cmd)
                            (arg1-of cmd)
                            (arg2-of cmd)
                            (arg3-of cmd))))))))

(defun perform-olc-zcmd (ch cmd if-flag arg1 arg2 arg3 prob &key test insert-after)
  "Performs the addition of the zone reset command CMD by the creature CH.  The arguments IF-FLAG, ARG1, ARG2, ARG3, and PROB should all be integers.  The VALIDATION-FUNC, if non-NIL, should be a function specifier that should perform further, specific validations.  Returns T or NIL if the operation did or did not successfully complete."
  (check-type if-flag integer)
  (check-type arg1 integer)
  (check-type arg2 integer)
  (check-type arg3 integer)
  (check-type prob integer)
  (let ((zone (zone-of (in-room-of ch))))
    (when (check-can-edit ch zone +zone-zcmds-approved+)
      (cond
        ((null (find cmd "MOPGEIRDVW"))
         (send-to-char ch "Invalid zcmd ~a~%" cmd) nil)
        ((or (null if-flag)
             (not (member if-flag '(-1 0 1))))
         (send-to-char ch "If-flag must be -1, 0, or 1.~%") nil)
        ((or (null prob)
             (not (<= 1 prob 100)))
         (send-to-char ch "Probability must be a number from 1 to 100.~%") nil)
        ((and test (not (funcall test ch arg1 arg2 arg3)))
         nil)
        (t
         (push-zcmd zone insert-after cmd if-flag arg1 arg2 arg3 prob)
         t)))))

(defun parse-zcmd-exitflags (ch flag-names)
  (let* ((bits (mapcar (lambda (flag-name)
                         (position flag-name +exit-bits+ :test 'string-abbrev))
                       (split-sequence #\space flag-names
                                       :remove-empty-subseqs t)))
         (null-pos (position-if #'null bits)))
    (cond
      (null-pos
       (send-to-char ch "~a is not a valid exit flag.~%" (elt flag-names null-pos))
       nil)
      (t
       (apply #'logior (mapcar (lambda (bit) (ash 1 bit)) bits))))))

(defun check-zcmd-mobile (ch arg1 arg2 arg3)
  (cond
    ((null (real-mobile-proto arg1))
     (send-to-char ch "Mobile ~a does not exist.~%" arg1))
    ((not (<= 1 arg2 1000))
     (send-to-char ch "Number loaded must be between 1 and 1000.~%"))
    ((null (real-room arg3))
     (send-to-char ch "Room ~d does not exist.~%" arg3))
    ((not (can-edit-zone ch (zone-of (real-room arg3))))
     (send-to-char ch "Let's not load mobs in other people's zones, shall we?~%"))
    (t
     (return-from check-zcmd-mobile t)))
  nil)

(defun check-zcmd-object (ch arg1 arg2 arg3)
  (cond
    ((null (real-object-proto arg1))
     (send-to-char ch "Object ~a does not exist.~%" arg1))
    ((not (<= 1 arg2 1000))
     (send-to-char ch "Number loaded must be between 1 and 1000.~%"))
    ((null (real-room arg3))
     (send-to-char ch "Room ~d does not exist.~%" arg3))
    ((not (can-edit-zone ch (zone-of (real-room arg3))))
     (send-to-char ch "Let's not load objects in other people's zones, shall we?~%"))
    (t
     (return-from check-zcmd-object t)))
  nil)

(defun check-zcmd-put (ch arg1 arg2 arg3)
  (cond
    ((null (real-object-proto arg1))
     (send-to-char ch "Object ~d does not exist.~%" arg1))
    ((not (<= 1 arg2 1000))
     (send-to-char ch "Number loaded must be between 1 and 1000.~%"))
    ((null (real-object-proto arg3))
     (send-to-char ch "Object ~d does not exist.~%" arg3))
    (t
     (return-from check-zcmd-put t)))
  nil)

(defun check-zcmd-give (ch arg1 arg2 arg3)
  (cond
    ((null (real-object-proto arg1))
     (send-to-char ch "Object ~d does not exist.~%" arg1))
    ((not (<= 1 arg2 1000))
     (send-to-char ch "Number loaded must be between 1 and 1000.~%"))
    ((null (real-mobile-proto arg3))
     (send-to-char ch "Mobile ~d does not exist.~%" arg3))
    (t
     (return-from check-zcmd-give t)))
  nil)

(defun check-zcmd-equip (ch arg1 arg2 arg3)
  (cond
    ((null (real-object-proto arg1))
     (send-to-char ch "Object ~d does not exist.~%" arg1))
    ((not (<= 1 arg2 1000))
     (send-to-char ch "Number loaded must be between 1 and 1000.~%"))
    ((not (<= 0 arg3 +num-wears+))
     (send-to-char ch "Invalid equipment position ~d, must be 0-~d.~%"
                   arg3
                   +num-wears+))
    (t
     (return-from check-zcmd-equip t)))
  nil)


(defun check-zcmd-implant (ch arg1 arg2 arg3)
  (cond
    ((null (real-object-proto arg1))
     (send-to-char ch "Object ~d does not exist.~%" arg1))
    ((not (<= 1 arg2 1000))
     (send-to-char ch "Number loaded must be between 1 and 1000.~%"))
    ((not (<= 0 arg3 +num-wears+))
     (send-to-char ch "Invalid implant position ~d, must be 0-~d.~%"
                   arg3
                   +num-wears+))
    (t
     (return-from check-zcmd-implant t)))
  nil)

(defun check-zcmd-remove (ch arg1 arg2 arg3)
  (declare (ignore arg3))
  (cond
    ((null (real-object-proto arg1))
     (send-to-char ch "Object ~d does not exist.~%" arg1))
    ((null (real-room arg2))
     (send-to-char ch "Room ~d does not exist.~%" arg2))
    ((not (can-edit-zone ch (zone-of (real-room arg2))))
     (send-to-char ch "Let's not remove objects from other people's zones.~%"))
    (t
     (return-from check-zcmd-remove t)))
  nil)

(defparameter +zcmd-list-options+
  '(:objects :removes :mobiles :equips :implants :gives :puts :doors :paths))

(defcommand (ch "olc" "create" "zone") (:immortal)
  (send-to-char ch "Create a zone with what number?~%"))

(defcommand (ch "olc" "create" "zone" number) (:immortal)
  (let ((num (parse-integer number :junk-allowed t)))
    (cond
      ((not (security-is-member ch "WorldWrite"))
       (send-to-char ch "You cannot create zones.~%"))
      ((null num)
       (send-to-char ch "You must specify a number for the zone.~%"))
      (t
       (perform-create-zone ch number)))))

(defcommand (ch "olc" "zset" "name") (:immortal)
  (send-to-char ch "Usage: olc zset name <zone name>~%"))

(defcommand (ch "olc" "zset" "name" value) (:immortal)
  (let ((zone (zone-of (in-room-of ch))))
    (when (check-can-edit ch zone +zone-zcmds-approved+)
      (setf (name-of zone) value)
      (setf (flags-of zone) (logior (flags-of zone) +zone-zone-modified+))
      (send-to-char ch "Zone ~d name set to: ~a~%" (number-of zone) (name-of zone)))))

(defcommand (ch "olc" "zset" "respawn_pt") (:immortal)
  (send-to-char ch "Usage: olc zset respawn_pt <respawn point>~%"))

(defcommand (ch "olc" "zset" "respawn_pt" "none") (:immortal)
  (let ((zone (zone-of (in-room-of ch))))
    (when (check-can-edit ch zone +zone-zcmds-approved+)
      (setf (respawn-pt-of zone) nil)
      (setf (flags-of zone) (logior (flags-of zone) +zone-zone-modified+))
      (send-to-char ch "Zone ~d respawn point cleared.~%" (number-of zone)))))

(defcommand (ch "olc" "zset" "respawn_pt" value) (:immortal)
  (let ((zone (zone-of (in-room-of ch))))
    (when (check-can-edit ch zone +zone-zcmds-approved+)
      (with-numeric-input ((value "The respawn point must be a room number."))
        (setf (respawn-pt-of zone) value)
        (setf (flags-of zone) (logior (flags-of zone) +zone-zone-modified+))
        (if (real-room value)
            (send-to-char ch "Zone ~d respawn point set to room ~d.~%"
                          (number-of zone)
                          value)
            (send-to-char ch "Zone ~d respawn point set to nonexistent room ~d.~%"
                          (number-of zone)
                          value))))))

(defcommand (ch "olc" "zset" "top") (:immortal)
  (send-to-char ch "Usage: olc zset top <top of zone>~%"))

(defcommand (ch "olc" "zset" "top" value) (:immortal)
  (let ((zone (zone-of (in-room-of ch))))
    (when (check-can-edit ch zone +zone-zcmds-approved+)
      (with-numeric-input ((value "The zone top must be a number."))
        (cond
          ((not (security-is-member ch "OLCWorldWrite"))
           (send-to-char ch "You cannot alter zones in this way.~%"))
          ((some (lambda (zone)
                   (>= (* (number-of zone) 100) value (top-of zone)))
                 *zone-table*)
           (send-to-char ch "The new top would overlap another zone.~%"))
          (t
           (setf (top-of zone) value)
           (setf (flags-of zone) (logior (flags-of zone) +zone-zone-modified+))
           (send-to-char ch "Zone ~d top number set to ~d.~%"
                         (number-of zone)
                         value)))))))

(defcommand (ch "olc" "zset" "reset") (:immortal)
  (send-to-char ch "Usage: olc zset reset (0|1|2)~%"))

(defcommand (ch "olc" "zset" "reset" value) (:immortal)
  (let ((zone (zone-of (in-room-of ch))))
    (when (check-can-edit ch zone +zone-zcmds-approved+)
      (with-numeric-input ((value "The reset mode must be 0, 1 or 2."))
        (cond
          ((not (<= 0 value 2))
           (send-to-char ch "The zone reset mode must be 0, 1, or 2.~%"))
          (t
           (setf (reset-mode-of zone) value)
           (setf (flags-of zone) (logior (flags-of zone) +zone-zone-modified+))
           (send-to-char ch "Zone ~d reset mode set to ~d.~%"
                         (number-of zone)
                         value)))))))

(defcommand (ch "olc" "zset" "tframe") (:immortal)
  (send-to-char ch "Usage: olc zset tframe (past|future|timeless)~%"))

(defcommand (ch "olc" "zset" "tframe" value) (:immortal)
  (let ((zone (zone-of (in-room-of ch))))
    (when (check-can-edit ch zone +zone-zcmds-approved+)
      (let ((value (position value +time-frames+ :test 'string-abbrev)))
        (cond
          ((null value)
           (send-to-char ch "Invalid timeframe.  Valid timeframes are: ~{~a~^, ~}~%"
                         (coerce +time-frames+ 'list)))
          (t
           (setf (time-frame-of zone) value)
           (setf (flags-of zone) (logior (flags-of zone) +zone-zone-modified+))
           (send-to-char ch "Zone ~d timeframe set to ~a.~%"
                         (number-of zone)
                         (aref +time-frames+ value))))))))

(defcommand (ch "olc" "zset" "plane" value) (:immortal)
  (let ((zone (zone-of (in-room-of ch))))
    (when (check-can-edit ch zone +zone-zcmds-approved+)
      (let ((value (position value +planes+ :test 'string-abbrev)))
        (cond
          ((null value)
           (send-to-char ch "Invalid plane."))
          (t
           (setf (plane-of zone) value)
           (setf (flags-of zone) (logior (flags-of zone) +zone-zone-modified+))
           (send-to-char ch "Zone ~d plane set to ~d.~%"
                         (number-of zone)
                         (aref +planes+ value))))))))

(defcommand (ch "olc" "zset" "author") (:immortal)
  (send-to-char ch "Usage: olc zset author <zone author>~%"))

(defcommand (ch "olc" "zset" "author" value) (:immortal)
  (let ((zone (zone-of (in-room-of ch))))
    (when (check-can-edit ch zone +zone-zcmds-approved+)
      (setf (author-of zone) value)
      (setf (flags-of zone) (logior (flags-of zone) +zone-zone-modified+))
      (send-to-char ch "Zone ~d author set to: ~a~%" (number-of zone) (author-of zone)))))

(defcommand (ch "olc" "zset" "owner") (:immortal)
  (send-to-char ch "You must supply a value to owner.~%"))

(defcommand (ch "olc" "zset" "owner" value) (:immortal)
  (let ((zone (zone-of (in-room-of ch))))
    (when (check-can-edit ch zone +zone-zcmds-approved+)
      (let ((idnum (retrieve-player-idnum value)))
        (cond
          ((null idnum)
           (send-to-char ch "No such player '~a'.~%" value))
          (t
           (setf (owner-idnum-of zone) idnum)
           (setf (flags-of zone) (logior (flags-of zone) +zone-zone-modified+))
           (send-to-char ch "Zone ~d owner set to ~a.~%"
                         (number-of zone)
                         (retrieve-player-name idnum))))))))

(defcommand (ch "olc" "zset" "owner" "none") (:immortal)
  (let ((zone (zone-of (in-room-of ch))))
    (when (check-can-edit ch zone +zone-zcmds-approved+)
      (setf (owner-idnum-of zone) 0)
      (setf (flags-of zone) (logior (flags-of zone) +zone-zone-modified+))
      (send-to-char ch "Zone ~d owner unset.~%"
                         (number-of zone)))))

(defcommand (ch "olc" "zset" "co-owner") (:immortal)
  (send-to-char ch "You must supply a value to co-owner.~%"))

(defcommand (ch "olc" "zset" "co-owner" value) (:immortal)
  (let ((zone (zone-of (in-room-of ch))))
    (when (check-can-edit ch zone +zone-zcmds-approved+)
      (let ((idnum (retrieve-player-idnum value)))
        (cond
          ((null idnum)
           (send-to-char ch "No such player '~a'.~%" value))
          (t
           (setf (co-owner-idnum-of zone) idnum)
           (setf (flags-of zone) (logior (flags-of zone) +zone-zone-modified+))
           (send-to-char ch "Zone ~d co-owner set to ~a.~%"
                         (number-of zone)
                         (retrieve-player-name idnum))))))))

(defcommand (ch "olc" "zset" "co-owner" "none") (:immortal)
  (let ((zone (zone-of (in-room-of ch))))
    (when (check-can-edit ch zone +zone-zcmds-approved+)
      (setf (co-owner-idnum-of zone) 0)
      (setf (flags-of zone) (logior (flags-of zone) +zone-zone-modified+))
      (send-to-char ch "Zone ~d co-owner unset.~%"
                         (number-of zone)))))

(defcommand (ch "olc" "zset" "flags") (:immortal)
  (send-to-char ch "Usage: olc zset flags (+|-) <flags>~%"))

(defcommand (ch "olc" "zset" "flags" junk) (:immortal)
  (declare (ignore junk))
  (send-to-char ch "Usage: olc zset flags (+|-) <flags>~%"))

(defcommand (ch "olc" "zset" "flags" plus-or-minus flag-names) (:immortal)
  (perform-set-flags ch plus-or-minus flag-names +zone-flags+
                     "zone"
                     "olc zset flags (+|-) <flags>"
                     (lambda ()
                       (flags-of (zone-of (in-room-of ch))))
                     (lambda (val)
                       (setf (flags-of (zone-of (in-room-of ch))) val))))

(defcommand (ch "olc" "zset" "hours") (:immortal)
  (send-to-char ch "Usage: olc zset hours <hours offset from Modrian Time~%"))

(defcommand (ch "olc" "zset" "hours" value) (:immortal)
  (let ((zone (zone-of (in-room-of ch))))
    (when (check-can-edit ch zone +zone-zcmds-approved+)
      (let ((value (parse-integer value :junk-allowed t)))
        (cond
          ((null value)
           (send-to-char ch "The argument must be a number.~%"))
          (t
           (setf (hour-mod-of zone) value)
           (setf (flags-of zone) (logior (flags-of zone) +zone-zone-modified+))
           (send-to-char ch "Zone ~d hour mod set to ~d.~%"
                         (number-of zone)
                         value)))))))

(defcommand (ch "olc" "zset" "years") (:immortal)
  (send-to-char ch "Usage: olc zset years <years offset from Modrian Time~%"))

(defcommand (ch "olc" "zset" "years" value) (:immortal)
  (let ((zone (zone-of (in-room-of ch))))
    (when (check-can-edit ch zone +zone-zcmds-approved+)
      (let ((value (parse-integer value :junk-allowed t)))
        (cond
          ((null value)
           (send-to-char ch "The argument must be a number.~%"))
          (t
           (setf (year-mod-of zone) value)
           (setf (flags-of zone) (logior (flags-of zone) +zone-zone-modified+))
           (send-to-char ch "Zone ~d year mod set to ~d.~%"
                         (number-of zone)
                         value)))))))

(defcommand (ch "olc" "zset" "pkstyle") (:immortal)
  (send-to-char ch "Usage: olc zset pkstyle (!PK|NPK|CPK)~%"))

(defcommand (ch "olc" "zset" "pkstyle" value) (:immortal)
  (let ((zone (zone-of (in-room-of ch))))
    (when (check-can-edit ch zone +zone-zcmds-approved+)
      (let ((value (position value +zone-pk-flags+ :test 'string-abbrev)))
        (cond
          ((null value)
           (send-to-char ch "Invalid pk style.  Valid pk styles are: ~{~a~^, ~}~%"
                         (coerce +zone-pk-flags+ 'list)))
          (t
           (setf (pk-style-of zone) value)
           (setf (flags-of zone) (logior (flags-of zone) +zone-zone-modified+))
           (send-to-char ch "Zone ~d pk-style set to ~a.~%"
                         (number-of zone)
                         (aref +zone-pk-flags+ value))))))))

(defcommand (ch "olc" "zset" "public_desc") (:immortal)
  (let ((zone (zone-of (in-room-of ch))))
    (when (check-can-edit ch zone +zone-zcmds-approved+)
      (act ch :place-emit "$n begins to edit a zone description.")
      (setf (plr-bits-of ch) (logior (plr-bits-of ch) +plr-olc+))
      (start-text-editor (link-of ch)
                         zone
                         "a zone description"
                         (public-desc-of zone)
                         (lambda (cxn target buf)
                           (setf (public-desc-of target) buf)
                           (setf (plr-bits-of (actor-of cxn))
                                 (logandc2 (plr-bits-of (actor-of cxn)) +plr-olc+))
                           (setf (state-of cxn) 'playing))
                         (lambda (cxn target)
                           (declare (ignore target))
                           (setf (plr-bits-of (actor-of cxn))
                                 (logandc2 (plr-bits-of (actor-of cxn)) +plr-olc+))
                           (setf (state-of cxn) 'playing))))))

(defcommand (ch "olc" "zset" "private_desc") (:immortal)
  (let ((zone (zone-of (in-room-of ch))))
    (when (check-can-edit ch zone +zone-zcmds-approved+)
      (act ch :place-emit "$n begins to edit a zone description.")
      (setf (plr-bits-of ch) (logior (plr-bits-of ch) +plr-olc+))
      (start-text-editor (link-of ch)
                         zone
                         "a zone description"
                         (private-desc-of zone)
                         (lambda (cxn target buf)
                           (setf (private-desc-of target) buf)
                           (setf (plr-bits-of (actor-of cxn))
                                 (logandc2 (plr-bits-of (actor-of cxn)) +plr-olc+))
                           (setf (state-of cxn) 'playing))
                         (lambda (cxn target)
                           (declare (ignore target))
                           (setf (plr-bits-of (actor-of cxn))
                                 (logandc2 (plr-bits-of (actor-of cxn)) +plr-olc+))
                           (setf (state-of cxn) 'playing))))))

(defcommand (ch "olc" "zset" "min_lvl") (:immortal)
  (send-to-char ch "Usage: olc zset min_lvl <minimum recommended zone level>~%"))

(defcommand (ch "olc" "zset" "min_lvl" value) (:immortal)
  (let ((zone (zone-of (in-room-of ch))))
    (when (check-can-edit ch zone +zone-zcmds-approved+)
      (let ((value (parse-integer value :junk-allowed t)))
        (cond
          ((null value)
           (send-to-char ch "The argument must be a number.~%"))
          ((not (<= 0 value 49))
           (send-to-char ch "You must supply a numerical argument from 0 to 49.~%"))
          (t
           (setf (min-lvl-of zone) value)
           (setf (flags-of zone) (logior (flags-of zone) +zone-zone-modified+))
           (send-to-char ch "Zone ~d minimum recommended player level set to ~d.~%"
                         (number-of zone)
                         value)))))))

(defcommand (ch "olc" "zset" "max_lvl") (:immortal)
  (send-to-char ch "Usage: olc zset max_lvl <minimum recommended zone level>~%"))

(defcommand (ch "olc" "zset" "max_lvl" value) (:immortal)
  (let ((zone (zone-of (in-room-of ch))))
    (when (check-can-edit ch zone +zone-zcmds-approved+)
      (let ((value (parse-integer value :junk-allowed t)))
        (cond
          ((null value)
           (send-to-char ch "The argument must be a number.~%"))
          ((not (<= 0 value 49))
           (send-to-char ch "You must supply a numerical argument from 0 to 49.~%"))
          (t
           (setf (max-lvl-of zone) value)
           (setf (flags-of zone) (logior (flags-of zone) +zone-zone-modified+))
           (send-to-char ch "Zone ~d maximum recommended player level set to ~d.~%"
                         (number-of zone)
                         value)))))))

(defcommand (ch "olc" "zset" "min_gen") (:immortal)
  (send-to-char ch "Usage: olc zset min_gen <minimum recommended zone gen>~%"))

(defcommand (ch "olc" "zset" "min_gen" value) (:immortal)
  (let ((zone (zone-of (in-room-of ch))))
    (when (check-can-edit ch zone +zone-zcmds-approved+)
      (let ((value (parse-integer value :junk-allowed t)))
        (cond
          ((null value)
           (send-to-char ch "The argument must be a number.~%"))
          ((not (<= 0 value 49))
           (send-to-char ch "You must supply a numerical argument from 0 to 49.~%"))
          (t
           (setf (min-gen-of zone) value)
           (setf (flags-of zone) (logior (flags-of zone) +zone-zone-modified+))
           (send-to-char ch "Zone ~d minimum recommended player gen set to ~d.~%"
                         (number-of zone)
                         value)))))))

(defcommand (ch "olc" "zset" "max_gen") (:immortal)
  (send-to-char ch "Usage: olc zset max_gen <minimum recommended zone gen>~%"))

(defcommand (ch "olc" "zset" "max_gen" value) (:immortal)
  (let ((zone (zone-of (in-room-of ch))))
    (when (check-can-edit ch zone +zone-zcmds-approved+)
      (let ((value (parse-integer value :junk-allowed t)))
        (cond
          ((null value)
           (send-to-char ch "The argument must be a number.~%"))
          ((not (<= 0 value 49))
           (send-to-char ch "You must supply a numerical argument from 0 to 49.~%"))
          (t
           (setf (max-gen-of zone) value)
           (setf (flags-of zone) (logior (flags-of zone) +zone-zone-modified+))
           (send-to-char ch "Zone ~d maximum recommended player gen set to ~d.~%"
                         (number-of zone)
                         value)))))))

(defcommand (ch "olc" "zcmd") (:immortal)
  (send-to-char ch "~
Usage:
olc zcmd [zone] list [range # #] [obj|mob|eq|give|door|etc...]
olc zcmd [zone] cmdremove <number>
olc zcmd [zone] cmdrenumber
olc zcmd move   <original num> <target num>
olc zcmd [zone] <M> <if_flag> <mob> <num> <room> <prob>
olc zcmd [zone] <O> <if_flag> <obj> <num> <room> <prob>
olc zcmd [zone] <P> <if_flag> <obj> <num> <obj> <prob>
olc zcmd [zone] <R> <if_flag> <obj> <room>
olc zcmd [zone] <E> <if_flag> <obj> <num> <pos> <mob> <prob>
olc zcmd [zone] <G> <if_flag> <obj> <num> <mob> <prob>
olc zcmd [zone] <D> <if_flag> <room> <door> <state>
olc zcmd [zone] <I> <if_flag> <obj> <num> <pos> <mob> <prob>
"))

(defcommand (ch "olc" "zcmd" "list") (:immortal)
  (perform-zcmd-list ch
                     (zone-of (in-room-of ch))
                     +zcmd-list-options+
                     0 100))

(defcommand (ch "olc" "zcmd" "list" args) (:immortal)
  (let ((option-strs (split-sequence #\space args :remove-empty-subseqs t))
        (options nil)
        (start-pos 0)
        (end-pos nil))
    (loop for option-str = option-strs then (cdr option-str)
       while option-str do
         (string-abbrev-case (first option-str)
           ("all"
            (setf options +zcmd-list-options+))
           ("objects"
            (pushnew :objects options))
           ("removes"
            (pushnew :removes options))
           ("mobiles"
            (pushnew :mobiles options))
           ("equips"
            (pushnew :equips options))
           ("implants"
            (pushnew :implants options))
           ("gives"
            (pushnew :gives options))
           ("puts"
            (pushnew :puts options))
           ("doors"
            (pushnew :doors options))
           ("paths"
            (pushnew :paths options))
           ("range"
            (let ((start (and (second option-str) (parse-integer (second option-str) :junk-allowed t)))
                  (end (and (third option-str) (parse-integer (third option-str) :junk-allowed t))))
              (setf option-str (cddr option-str))
              (cond
                ((or (null start) (null end))
                 (send-to-char ch "A start and end number is required for a range.~%")
                 (return))
                ((or (minusp start) (minusp end))
                 (send-to-char ch "Both the start and end number must be positive.~%"))
                (t
                 (setf start-pos start)
                 (setf end-pos end)))))
           (t
            (send-to-char ch "Invalid zone command segment.~%")
            (return))))

    (perform-zcmd-list ch (zone-of (in-room-of ch)) (or options +zcmd-list-options+)
                       start-pos (or end-pos (+ start-pos 100)))))

(defcommand (ch "olc" "zcmd" "cmdremove") (:immortal)
  (send-to-char ch "Usage: olc zcmd cmdremove <zcmd number>~%"))

(defcommand (ch "olc" "zcmd" "cmdremove" number) (:immortal)
  (let ((zone (zone-of (in-room-of ch))))
    (when (check-can-edit ch zone +zone-zcmds-approved+)
      (let ((doomed-idx (parse-integer number :junk-allowed t)))
        (cond
          ((null doomed-idx)
           (send-to-char ch "Usage: olc zcmd cmdremove <zcmd number>~%"))
          ((minusp doomed-idx)
           (send-to-char ch "The zcmd number must be positive.~%"))
          ((> doomed-idx (1- (length (cmds-of zone))))
           (send-to-char ch "There isn't a zcmd with that number.~%"))
          (t
           (setf (cmds-of zone) (delete (nth (1- doomed-idx) (cmds-of zone)) (cmds-of zone)))
           (send-to-char ch "Command ~d removed.~%" doomed-idx)))))))

(defcommand (ch "olc" "zcmd" "m") (:immortal)
  (send-to-char ch "Usage; olc zcmd m <if-flag> <vnum> <max-loaded> <room num> <prob>~%"))

(defcommand (ch "olc" "zcmd" "m" junk) (:immortal)
  (declare (ignore junk))
  (send-to-char ch "Usage; olc zcmd m <if-flag> <vnum> <max-loaded> <room num> <prob>~%"))

(defcommand (ch "olc" "zcmd" "m" if-flag vnum max-loaded room prob) (:immortal)
  (with-numeric-input ((if-flag "If-flag must be -1, 0, or 1.")
                       (vnum "Invalid vnum specified.")
                       (max-loaded "Maxload must be a number from 1 to 1000." (make-range-p 1 1000))
                       (room "Invalid room specified")
                       (prob "Prob must be a number from 1 to 100." (make-range-p 1 100)))
    (when (perform-olc-zcmd ch #\M if-flag vnum max-loaded room prob :test 'check-zcmd-mobile)
      (send-to-char ch "Command completed ok.~%"))))

(defcommand (ch "olc" "zcmd" "o") (:immortal)
  (send-to-char ch "olc zcmd o <if-flag> <vnum> <max-loaded> <room> <prob>~%"))

(defcommand (ch "olc" "zcmd" "o" junk) (:immortal)
  (declare (ignore junk))
  (send-to-char ch "olc zcmd o <if-flag> <vnum> <max-loaded> <room> <prob>~%"))

(defcommand (ch "olc" "zcmd" "o" if-flag vnum max-loaded room prob) (:immortal)
  (with-numeric-input ((if-flag "If-flag must be -1, 0, or 1.")
                       (vnum "Invalid vnum specified.")
                       (max-loaded "Maxload must be a number from 1 to 1000." (make-range-p 1 1000))
                       (room "Invalid room specified")
                       (prob "Prob must be a number from 1 to 100." (make-range-p 1 100)))
    (when (perform-olc-zcmd ch #\O if-flag vnum max-loaded room prob :test 'check-zcmd-object)
      (send-to-char ch "Command completed ok.~%"))))

(defcommand (ch "olc" "zcmd" "p") (:immortal)
  (send-to-char ch "Usage: olc zcmd p <if-flag> <vnum> <max-loaded> <dest-vnum> <prob>~%"))

(defcommand (ch "olc" "zcmd" "p" junk) (:immortal)
  (declare (ignore junk))
  (send-to-char ch "Usage: olc zcmd p <if-flag> <vnum> <max-loaded> <dest-vnum> <prob>~%"))

(defcommand (ch "olc" "zcmd" "p" if-flag vnum max-loaded dest-vnum prob) (:immortal)
  (with-numeric-input ((if-flag "If-flag must be -1, 0, or 1.")
                       (vnum "Invalid vnum specified.")
                       (max-loaded "Maxload must be a number from 1 to 1000." (make-range-p 1 1000))
                       (dest-vnum "Invalid dest object specified")
                       (prob "Prob must be a number from 1 to 100." (make-range-p 1 100)))
    (when (perform-olc-zcmd ch #\P if-flag vnum max-loaded dest-vnum prob :test 'check-zcmd-put)
      (send-to-char ch "Command completed ok.~%"))))

(defcommand (ch "olc" "zcmd" "g") (:immortal)
  (send-to-char ch "Usage: olc zcmd g <if-flag> <vnum> <max-loaded> <dest-vnum> <prob>~%"))

(defcommand (ch "olc" "zcmd" "g" junk) (:immortal)
  (declare (ignore junk))
  (send-to-char ch "Usage: olc zcmd g <if-flag> <vnum> <max-loaded> <dest-vnum> <prob>~%"))

(defcommand (ch "olc" "zcmd" "g" if-flag vnum max-loaded dest-vnum prob) (:immortal)
  (with-numeric-input ((if-flag "If-flag must be -1, 0, or 1.")
                       (vnum "Invalid vnum specified.")
                       (max-loaded "Maxload must be a number from 1 to 1000." (make-range-p 1 1000))
                       (dest-vnum "Invalid dest mob specified")
                       (prob "Prob must be a number from 1 to 100." (make-range-p 1 100)))
    (when (perform-olc-zcmd ch #\G if-flag vnum max-loaded dest-vnum prob :test 'check-zcmd-give)
      (send-to-char ch "Command completed ok.~%"))))

(defcommand (ch "olc" "zcmd" "e") (:immortal)
  (send-to-char ch "Usage: olc zcmd e <if-flag> <vnum> <max-loaded> <pos> <prob>~%"))

(defcommand (ch "olc" "zcmd" "e" junk) (:immortal)
  (declare (ignore junk))
  (send-to-char ch "Usage: olc zcmd e <if-flag> <vnum> <max-loaded> <pos> <prob>~%"))

(defcommand (ch "olc" "zcmd" "e" if-flag vnum max-loaded pos prob) (:immortal)
  (with-numeric-input ((if-flag "If-flag must be -1, 0, or 1.")
                       (vnum "Invalid vnum specified.")
                       (max-loaded "Maxload must be a number from 1 to 1000." (make-range-p 1 1000))
                       (pos "Invalid wear position" (make-range-p 0 +num-wears+))
                       (prob "Prob must be a number from 1 to 100." (make-range-p 1 100)))
    (when (perform-olc-zcmd ch #\E if-flag vnum max-loaded pos prob :test 'check-zcmd-equip)
      (send-to-char ch "Command completed ok.~%"))))

(defcommand (ch "olc" "zcmd" "i") (:immortal)
  (send-to-char ch "Usage: olc zcmd i <if-flag> <vnum> <max-loaded> <pos> <prob>~%"))

(defcommand (ch "olc" "zcmd" "i" junk) (:immortal)
  (declare (ignore junk))
  (send-to-char ch "Usage: olc zcmd i <if-flag> <vnum> <max-loaded> <pos> <prob>~%"))

(defcommand (ch "olc" "zcmd" "i" if-flag vnum max-loaded pos prob) (:immortal)
  (with-numeric-input ((if-flag "If-flag must be -1, 0, or 1.")
                       (vnum "Invalid vnum specified.")
                       (max-loaded "Maxload must be a number from 1 to 1000." (make-range-p 1 1000))
                       (pos "Invalid implant position" (make-range-p 0 +num-wears+))
                       (prob "Prob must be a number from 1 to 100." (make-range-p 1 100)))
    (when (perform-olc-zcmd ch #\I if-flag vnum max-loaded pos prob :test 'check-zcmd-implant)
      (send-to-char ch "Command completed ok.~%"))))

(defcommand (ch "olc" "zcmd" "r") (:immortal)
  (send-to-char ch "Usage: olc zcmd r <if-flag> <vnum> <room> <prob>~%"))

(defcommand (ch "olc" "zcmd" "r" junk) (:immortal)
  (declare (ignore junk))
  (send-to-char ch "Usage: olc zcmd r <if-flag> <vnum> <room> <prob>~%"))

(defcommand (ch "olc" "zcmd" "r" if-flag vnum room prob) (:immortal)
  (with-numeric-input ((if-flag "If-flag must be -1, 0, or 1.")
                       (vnum "Invalid vnum specified.")
                       (room "Invalid room specified.")
                       (prob "Prob must be a number from 1 to 100." (make-range-p 1 100)))
    (when (perform-olc-zcmd ch #\R if-flag vnum room -1 prob :test 'check-zcmd-remove)
      (send-to-char ch "Command completed ok.~%"))))

(defcommand (ch "olc" "zcmd" "d") (:immortal)
  (send-to-char ch "Usage: olc zcmd d <if-flag> <room> <direction> <flag-names>~%"))

(defcommand (ch "olc" "zcmd" "d" junk) (:immortal)
  (declare (ignore junk))
  (send-to-char ch "Usage: olc zcmd d <if-flag> <room> <direction> <flag-names>~%"))

(defcommand (ch "olc" "zcmd" "d" if-flag room direction flag-names) (:immortal)
  (with-numeric-input ((if-flag "If-flag must be -1, 0, or 1.")
                       (room "Invalid room specified."))
    (let ((zone (zone-of (in-room-of ch))))
      (when (check-can-edit ch zone +zone-zcmds-approved+)
        (let ((dir (position direction +dirs+ :test 'string-abbrev))
              (exit-flags (parse-zcmd-exitflags ch flag-names)))
          (cond
            ((not (member if-flag '(-1 0 1)))
             (send-to-char ch "If-flag must be -1, 0, or 1.~%"))
            ((null (real-room room))
             (send-to-char ch "Room ~d does not exist.~%" room))
            ((null dir)
             (send-to-char ch "Illegal direction ~a~%" direction))
            ((null (aref (dir-option-of (real-room room)) dir))
             (send-to-char ch "Room ~d does not have a door leading ~a.~%"
                           room
                           (aref +dirs+ dir)))
            ((zerop exit-flags)
             (send-to-char ch "That wouldn't do anything.~%"))
            (t
             (push-zcmd zone nil #\D if-flag room dir exit-flags 100)
             (send-to-char ch "Command completed ok.~%"))))))))

(defcommand (ch "olc" "zmob") (:immortal)
  (send-to-char ch "Usage: olc zmob <vnum> <max-loaded> <prob>~%"))

(defcommand (ch "olc" "zmob" junk) (:immortal)
  (declare (ignore junk))
  (send-to-char ch "Usage: olc zmob <vnum> <max-loaded> <prob>~%"))

(defcommand (ch "olc" "zmob" vnum max-loaded prob) (:immortal)
  (with-numeric-input ((vnum "Invalid vnum specified.")
                       (max-loaded "Maxload must be a number from 1 to 1000." (make-range-p 1 1000))
                       (prob "Prob must be a number from 1 to 100." (make-range-p 1 100)))
    (when (perform-olc-zcmd ch #\M 0 vnum max-loaded (number-of (in-room-of ch)) prob
                            :test 'check-zcmd-mobile)
      (char-to-room (read-mobile vnum) (in-room-of ch) nil)
      (send-to-char ch "Command completed ok.~%"))))

(defcommand (ch "olc" "zequip") (:immortal)
  (send-to-char ch "Usage: olc zequip <mob-name> <obj-vnum> <max-loaded> <pos> <prob>~%"))

(defcommand (ch "olc" "zequip" junk) (:immortal)
  (declare (ignore junk))
  (send-to-char ch "Usage: olc zequip <mob-name> <obj-vnum> <max-loaded> <pos> <prob>~%"))

(defcommand (ch "olc" "zequip" mob-name obj-vnum max-loaded pos prob) (:immortal)
  (with-numeric-input ((obj-vnum "Invalid vnum specified.")
                       (max-loaded "Maxload must be a number from 1 to 1000." (make-range-p 1 1000))
                       (pos "Invalid wear position" (make-range-p 0 +num-wears+))
                       (prob "Prob must be a number from 1 to 100." (make-range-p 1 100)))
    (let* ((mob (first (resolve-alias ch mob-name (people-of (in-room-of ch)))))
           (mob-vnum (and mob (vnum-of mob)))
           (mob-reset-cmd (and mob-vnum
                               (member-if (lambda (cmd)
                                            (and (eql #\M (command-of cmd))
                                                 (= mob-vnum (arg1-of cmd))))
                                   (cmds-of (zone-of (in-room-of ch)))))))
      (cond
        ((null mob)
         (send-to-char ch "Cannot find that mobile in this room.~%"))
        ((not (is-npc mob))
         (send-to-char ch "Pretty funny.~%"))
        ((null mob-reset-cmd)
         (send-to-char ch "Zone command required for mobile ~d before this can be set.~%"))
        ((perform-olc-zcmd ch #\E 1 obj-vnum max-loaded pos prob
                           :test 'check-zcmd-equip
                           :insert-after mob-reset-cmd)
         (let ((obj (real-object-proto obj-vnum)))
           (when (and (< (number-of (shared-of obj)) max-loaded)
                      (null (aref (equipment-of mob) pos)))
             (equip-char mob (read-object obj-vnum) pos :worn)))
         (send-to-char ch "Command completed ok.~%"))))))

(defcommand (ch "olc" "zimplant") (:immortal)
  (send-to-char ch "Usage: olc zimplant <mob-name> <obj-vnum> <max-loaded> <pos> <prob>~%"))

(defcommand (ch "olc" "zimplant" junk) (:immortal)
  (declare (ignore junk))
  (send-to-char ch "Usage: olc zimplant <mob-name> <obj-vnum> <max-loaded> <pos> <prob>~%"))

(defcommand (ch "olc" "zimplant" mob-name obj-vnum max-loaded pos prob) (:immortal)
  (with-numeric-input ((obj-vnum "Invalid vnum specified.")
                       (max-loaded "Maxload must be a number from 1 to 1000." (make-range-p 1 1000))
                       (pos "Invalid wear position" (make-range-p 0 +num-wears+))
                       (prob "Prob must be a number from 1 to 100." (make-range-p 1 100)))
    (let* ((mob (first (resolve-alias ch mob-name (people-of (in-room-of ch)))))
           (mob-vnum (and mob (vnum-of mob)))
           (mob-reset-cmd (and mob-vnum
                               (member-if (lambda (cmd)
                                            (and (eql #\M (command-of cmd))
                                                 (= mob-vnum (arg1-of cmd))))
                                          (cmds-of (zone-of (in-room-of ch)))))))
      (cond
        ((null mob)
         (send-to-char ch "Cannot find that mobile in this room.~%"))
        ((not (is-npc mob))
         (send-to-char ch "Pretty funny.~%"))
        ((null mob-reset-cmd)
         (send-to-char ch "Zone command required for mobile ~d before this can be set.~%"))
        ((perform-olc-zcmd ch #\I 1 obj-vnum max-loaded pos prob
                           :test 'check-zcmd-implant
                           :insert-after mob-reset-cmd)
         (let ((obj (real-object-proto obj-vnum)))
           (when (and (< (number-of (shared-of obj)) max-loaded)
                      (null (aref (implants-of mob) pos)))
             (equip-char mob (read-object obj-vnum) pos :implant)))
         (send-to-char ch "Command completed ok.~%"))))))

(defcommand (ch "olc" "zobj") (:immortal)
  (send-to-char ch "Usage: olc zobj <obj-vnum> <max-loaded> <prob>~%"))

(defcommand (ch "olc" "zobj" junk) (:immortal)
  (declare (ignore junk))
  (send-to-char ch "Usage: olc zobj <obj-vnum> <max-loaded> <prob>~%"))

(defcommand (ch "olc" "zobj" obj-vnum max-loaded prob) (:immortal)
  (with-numeric-input ((obj-vnum "Invalid vnum specified.")
                       (max-loaded "Maxload must be a number from 1 to 1000." (make-range-p 1 1000))
                       (prob "Prob must be a number from 1 to 100." (make-range-p 1 100)))
    (when (perform-olc-zcmd ch #\O 0 obj-vnum max-loaded (number-of (in-room-of ch)) prob
                            :test 'check-zcmd-object)
      (obj-to-room (read-object obj-vnum) (in-room-of ch))
      (send-to-char ch "Command completed ok.~%"))))

(defcommand (ch "olc" "zdoor") (:immortal)
  (send-to-char ch "Usage: olc zdoor <direction> <flag-names>~%"))

(defcommand (ch "olc" "zdoor" junk) (:immortal)
  (declare (ignore junk))
  (send-to-char ch "Usage: olc zdoor <direction> <flag-names>~%"))

(defcommand (ch "olc" "zdoor" direction flag-names) (:immortal)
  (let ((zone (zone-of (in-room-of ch))))
    (when (check-can-edit ch zone +zone-zcmds-approved+)
      (let ((dir (position direction +dirs+ :test 'string-abbrev))
            (exit-flags (parse-zcmd-exitflags ch flag-names)))
        (cond
          ((null dir)
           (send-to-char ch "Illegal direction ~a~%" direction))
          ((null (exit ch dir))
           (send-to-char ch "Room ~d does not have a door leading ~a.~%"
                         (number-of (in-room-of ch))
                         (aref +dirs+ dir)))
          ((zerop exit-flags)
           (send-to-char ch "That wouldn't do anything.~%"))
          (t
           (push-zcmd zone nil #\D 0 (number-of (in-room-of ch))
                      dir exit-flags 100)
           (let ((to-room (to-room-of (exit ch dir))))
             (when (abs-exit to-room (aref +rev-dir+ dir))
               (push-zcmd zone nil #\D 0 (number-of to-room)
                          (aref +rev-dir+ dir) exit-flags 100)))
           (send-to-char ch "Command completed ok.~%")))))))

(defcommand (ch "olc" "zput") (:immortal)
  (send-to-char ch "Usage: olc zput <container-name> <obj-vnum> <max-loaded> <prob>~%"))

(defcommand (ch "olc" "zput" junk) (:immortal)
  (declare (ignore junk))
  (send-to-char ch "Usage: olc zput <container-name> <obj-vnum> <max-loaded> <prob>~%"))

(defcommand (ch "olc" "zput" container-name obj-vnum max-loaded prob) (:immortal)
  (with-numeric-input ((obj-vnum "Invalid vnum specified.")
                       (max-loaded "Maxload must be a number from 1 to 1000." (make-range-p 1 1000))
                       (prob "Prob must be a number from 1 to 100." (make-range-p 1 100)))
    (let* ((container (first (resolve-alias ch container-name (contents-of (in-room-of ch)))))
           (cont-vnum (and container (vnum-of container)))
           (cont-reset-cmd (and cont-vnum
                               (member-if (lambda (cmd)
                                     (and (eql #\O (command-of cmd))
                                          (= cont-vnum (arg1-of cmd))))
                                   (cmds-of (zone-of (in-room-of ch)))))))
      (cond
        ((null container)
         (send-to-char ch "Cannot find the container in this room.~%"))
        ((null cont-reset-cmd)
         (send-to-char ch "Zone command required for object ~d before this can be set.~%"))
        ((perform-olc-zcmd ch #\P 1 obj-vnum max-loaded cont-vnum prob
                           :test 'check-zcmd-put
                           :insert-after cont-reset-cmd)
         (let ((proto (real-object-proto obj-vnum)))
           (when (< (number-of (shared-of proto)) max-loaded)
             (let ((obj (read-object obj-vnum)))
               (when (zone-flagged (zone-of (in-room-of ch)) +zone-zcmds-approved+)
                 (setf (extra2-flags-of obj) (logior (extra2-flags-of obj) +item2-unapproved+)))
               (obj-to-obj obj container))))
         (send-to-char ch "Command completed ok.~%"))))))

(defcommand (ch "olc" "zgive") (:immortal)
  (send-to-char ch "Usage: olc zgive <mob-name> <obj-vnum> <max-loaded> <prob>~%"))

(defcommand (ch "olc" "zgive" junk) (:immortal)
  (declare (ignore junk))
  (send-to-char ch "Usage: olc zgive <mob-name> <obj-vnum> <max-loaded> <prob>~%"))

(defcommand (ch "olc" "zgive" mob-name obj-vnum max-loaded prob) (:immortal)
  (with-numeric-input ((obj-vnum "Invalid vnum specified.")
                       (max-loaded "Maxload must be a number from 1 to 1000." (make-range-p 1 1000))
                       (prob "Prob must be a number from 1 to 100." (make-range-p 1 100)))
    (let* ((mob (first (resolve-alias ch mob-name (people-of (in-room-of ch)))))
           (mob-vnum (and mob (vnum-of mob)))
           (mob-reset-cmd (and mob-vnum
                               (member-if (lambda (cmd)
                                     (and (eql #\M (command-of cmd))
                                          (= mob-vnum (arg1-of cmd))))
                                   (cmds-of (zone-of (in-room-of ch)))))))
      (cond
        ((null mob)
         (send-to-char ch "Cannot find that mobile in this room.~%"))
        ((not (is-npc mob))
         (send-to-char ch "Pretty funny.~%"))
        ((null mob-reset-cmd)
         (send-to-char ch "Zone command required for mobile ~d before this can be set.~%"))
        ((perform-olc-zcmd ch #\G 1 obj-vnum max-loaded (vnum-of mob) prob
                           :test 'check-zcmd-give
                           :insert-after mob-reset-cmd)
         (let ((proto (real-object-proto obj-vnum)))
           (when (< (number-of (shared-of proto)) max-loaded)
             (let ((obj (read-object obj-vnum)))
               (when (zone-flagged (zone-of (in-room-of ch)) +zone-zcmds-approved+)
                 (setf (extra2-flags-of obj) (logior (extra2-flags-of obj) +item2-unapproved+)))
               (obj-to-char obj mob))))
         (send-to-char ch "Command completed ok.~%"))))))

(defcommand (ch "olc" "zpath") (:immortal)
  (send-to-char ch "Usage: olc zpath (mobile|object) <name> <path-name>~%"))

(defcommand (ch "olc" "zpath" junk) (:immortal)
  (declare (ignore junk))
  (send-to-char ch "Usage: olc zpath (mobile|object) <name> <path-name>~%"))

(defcommand (ch "olc" "zpath" "mobile" mob-name path-name) (:immortal)
  (let* ((mob (first (resolve-alias ch mob-name (people-of (in-room-of ch)))))
         (mob-vnum (and mob (vnum-of mob)))
         (mob-reset-cmd (and mob-vnum
                             (member-if (lambda (cmd)
                                        (and (eql #\M (command-of cmd))
                                             (= mob-vnum (arg1-of cmd))))
                                      (cmds-of (zone-of (in-room-of ch))))))
         (path-vnum (path-vnum-by-name path-name) ))
    (cond
      ((null mob)
       (send-to-char ch "Cannot find that mobile in this room.~%"))
      ((not (is-npc mob))
       (send-to-char ch "Pretty funny.~%"))
      ((null path-vnum)
       (send-to-char ch "No such path '~a'~%" path-name))
      ((null mob-reset-cmd)
       (send-to-char ch "Zone command required for mobile ~d before this can be set.~%"))
      ((perform-olc-zcmd ch #\W 1 path-vnum 0 (vnum-of mob) 100 :insert-after mob-reset-cmd)
       (send-to-char ch "Command completed ok.~%")))))

(defcommand (ch "olc" "zpath" "object" obj-name path-name) (:immortal)
  (let* ((obj (first (resolve-alias ch obj-name (contents-of (in-room-of ch)))))
         (obj-vnum (and obj (vnum-of obj)))
         (obj-reset-cmd (and obj-vnum
                             (member-if (lambda (cmd)
                                        (and (eql #\O (command-of cmd))
                                             (= obj-vnum (arg1-of cmd))))
                                      (cmds-of (zone-of (in-room-of ch))))))
         (path-vnum (path-vnum-by-name path-name) ))
    (cond
      ((null obj)
       (send-to-char ch "Cannot find that object in this room.~%"))
      ((null path-vnum)
       (send-to-char ch "No such path '~a'~%" path-name))
      ((null obj-reset-cmd)
       (send-to-char ch "Zone command required for object ~d before this can be set.~%"))
      ((perform-olc-zcmd ch #\V 1 path-vnum 0 (vnum-of obj) 100 :insert-after obj-reset-cmd)
       (send-to-char ch "Command completed ok.~%")))))

(defcommand (ch "olc" "zsave") (:immortal)
  (let ((zone (zone-of (in-room-of ch))))
    (when (check-can-edit ch zone +zone-zcmds-approved+)
      (if (save-zone-data ch zone)
          (send-to-char ch "You save the zone information for zone #~d (~a).~%"
                        (number-of zone)
                        (name-of zone))
          (send-to-char ch "The zone information for zone #~d (~a) could not be saved.~%"
                        (number-of zone)
                        (name-of zone))))))

(defcommand (ch "olc" "zpurge") (:immortal)
  (let ((mob-count 0)
        (obj-count 0)
        (zone (zone-of (in-room-of ch))))
    (when (check-can-edit ch zone +zone-zcmds-approved+)
      (dolist (room (world-of zone))
        (unless (or (room-flagged room +room-godroom+)
                    (room-flagged room +room-house+))
          (dolist (mob (remove-if-not 'is-npc (people-of room)))
            (incf mob-count)
            (purge-creature mob t))
          (loop
             for obj = (first (contents-of room))
             until (null obj) do
               (incf obj-count)
               (extract-obj obj))))
      (send-to-char ch "Zone ~d cleared of ~d mobile~:p.~%" (number-of zone) mob-count)
      (send-to-char ch "Zone ~d cleared of ~d object~:p.~%" (number-of zone) obj-count)
      (slog "(GC) ~a olc-purged zone ~d (~a)" (name-of ch) (number-of zone) (name-of zone)))))