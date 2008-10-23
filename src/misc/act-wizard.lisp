(in-package #:tempus)

(defun find-target-room (ch roomstr)
  (cond
    ((string= "" roomstr)
     (send-to-char ch "You must supply a room number or name.~%"))
    ((every #'digit-char-p roomstr)
     (let ((result (real-room (parse-integer roomstr))))
       (prog1 result
         (unless result
          (send-to-char ch "No room exists with that number.~%")))))
    ((string-abbrev roomstr "previous")
     (loop
        with bottom-room = (* (number-of (zone-of (in-room-of ch))) 100)
        for roomnum from (1- (idnum-of (in-room-of ch))) downto bottom-room
        as result = (real-room roomnum)
        until result
        finally (progn
                  (unless result
                    (send-to-char ch "No previous room exists in this zone!~%"))
                  (return result))))
    ((string-abbrev roomstr "next")
     (loop
        with top-room = (top-of (zone-of (in-room-of ch)))
        for roomnum from (1+ (idnum-of (in-room-of ch))) upto top-room
        as result = (real-room roomnum)
        until result
        finally (progn
                  (unless result
                    (send-to-char ch "No next room exists in this zone!~%"))
                  (return result))))
    (t
     (let ((obj (or (get-char-vis ch roomstr)
                    (get-obj-vis ch roomstr))))
       (cond
         ((null obj)
          (send-to-char ch "No such creature or object around.~%")
          nil)
         ((null (in-room-of obj))
          (send-to-char ch "That object is not available.~%")
          nil)
         (t
          (in-room-of obj)))))))

(defun perform-goto (ch room allow-follow)
  (if (can-enter-room ch room)
      (let ((was-in (in-room-of ch)))
        (act ch :place-emit (cond
                              ((or (null (poofout-of ch))
                                   (string= "" (poofout-of ch)))
                               "$n disappears in a puff of smoke.")
                              ((search "$n" (poofout-of ch))
                               (poofout-of ch))
                              (t
                               (format nil "$n ~a" (poofout-of ch)))))
        (when (in-room-of ch)
          (char-from-room ch))
        (char-to-room ch room)
        (when (room-is-open-air room)
          (setf (position-of ch) +pos-flying+))
        (act ch :place-emit (cond
                              ((or (null (poofin-of ch))
                                   (string= "" (poofin-of ch)))
                               "$n appears with an ear-splitting bang.")
                              ((search "$n" (poofin-of ch))
                               (poofin-of ch))
                              (t
                               (format nil "$n ~a" (poofin-of ch)))))
        (look-at-room ch room nil)
        (when (and allow-follow (followers-of ch))
          (dolist (tch (followers-of ch))
            (when (and (eql was-in (in-room-of tch))
                       (immortal-level-p tch)
                       (not (plr-flagged ch (logior +plr-olc+ +plr-writing+ +plr-mailing+)))
                       (can-see-creature tch ch))
              (perform-goto tch room t))))

        (when (and (room-flagged room +room-death+)
                   (< (level-of ch) +lvl-impl+))
          (slog "(GC) ~a goto deathtrap [~d] ~a."
                (name-of ch)
                (number-of room)
                (name-of room))))
      (send-to-char ch "You cannot enter there.~%")))

(defun perform-transport (ch tch)
  (cond
    ((eql tch ch)
     (send-to-char ch "Sure, sure.  Try to transport yourself.~%"))
    ((and (< (level-of ch) (level-of tch))
          (not (is-npc tch)))
     (send-to-char ch "~a is far too powerful for you to transport.~%"
                   (name-of tch)))
    ((and (room-is-open-air (in-room-of ch))
          (/= (position-of tch) +pos-flying+))
     (send-to-char ch "You are in midair and ~a isn't flying.~%"
                   (name-of tch)))
    (t
     (let ((was-in (in-room-of tch)))
       (act tch :place-emit "$n disappears in a mushroom cloud.")
       (char-from-room tch)
       (char-to-room tch (in-room-of ch) nil)
       (act ch :target tch
            :subject-emit "$N arrives from a puff of smoke."
            :target-emit "$n has transported you!"
            :not-target-emit "$N arrives from a puff of smoke.")
       (dolist (fch (followers-of tch))
         (when (and (eql was-in (in-room-of fch))
                    (immortal-level-p fch)
                    (not (plr-flagged ch (logior +plr-olc+ +plr-writing+ +plr-mailing+)))
                    (can-see-creature fch ch))
           (perform-goto fch (in-room-of tch) t)))
       (slog "(GC) ~a has transported ~a to ~a[~d]"
             (name-of ch)
             (name-of tch)
             (name-of (in-room-of ch))
             (number-of (in-room-of ch)))))))

(defun hash-keys (hash)
  (with-hash-table-iterator (next hash)
    (loop for (more key value) = (multiple-value-list (next))
         while more
         collect key)))

(defun vnum-prototypes (ch namelist-str name-color prototype-hash)
  "Displays to CH a list of the objects or mobiles in PROTOTYPE-HASH which match NAME.  The name of the object or mobile will be in the color designated by NAME-COLOR.  Returns the number displayed."
  (let ((count 0)
        (namelist (split-sequence #\space namelist-str
                                  :remove-empty-subseqs t)))
    (send-to-char ch "~a"
                  (with-output-to-string (str)
                    (loop
                       for vnum in (sort (hash-keys prototype-hash) #'<)
                       as proto = (gethash vnum prototype-hash)
                       when (every (lambda (name)
                                     (is-name name (aliases-of proto)))
                                   namelist)
                       do
                       (incf count)
                       (format str "~3d. &g[&n~5d&g] &~c~a&n~%"
                               count
                               vnum
                               name-color
                               (name-of proto)))))
    count))

(defcommand (ch "echo") (:immortal :dead)
  (send-to-char ch "Yes, but what?~%"))

(defcommand (ch "echo" str) (:immortal :dead)
  (let* ((mort-see (act-escape str))
         (imm-see (format nil "[$n] ~a" mort-see)))
    (dolist (tch (people-of (in-room-of ch)))
      (act ch :target tch
           :target-emit (if (> (level-of tch) (level-of ch))
                            imm-see
                            mort-see)))))

(defcommand (ch "send") (:immortal)
  (send-to-char ch "Send what to who?~%"))

(defcommand (ch "send" stuff) (:immortal)
  (declare (ignore stuff))
  (send-to-char ch "What do you want to send?~%"))

(defcommand (ch "send" name stuff) (:immortal)
  (let* ((tchs (get-matching-objects ch name (mapcar 'actor-of *cxns*)))
         (tch (first tchs)))
    (cond
      ((null tchs)
       (send-to-char ch "Nobody by that name.~%"))
      (t
       (send-to-char ch "You send '~a' to ~a.~%" stuff (name-of tch))
       (send-to-char tch "~a~%" stuff)))))

(defcommand (ch "at") (:immortal)
  (send-to-char ch "Where do you want to do something?~%"))

(defcommand (ch "at" ignore) (:immortal)
  (declare (ignore ignore))
  (send-to-char ch "What do you want to do there?~%"))

(defcommand (ch "at" roomstr command) (:immortal)
  (let ((original-room (in-room-of ch))
        (target-room (find-target-room ch roomstr)))
    (when target-room
      (char-from-room ch)
      (char-to-room ch target-room)
      (interpret-command ch command)
      ;; only return the char if they're still there
      (when (eql target-room (in-room-of ch))
        (char-from-room ch)
        (char-to-room ch original-room)))))

(defcommand (ch "goto") (:immortal)
  (send-to-char ch "Where do you want to go today?~%"))

(defcommand (ch "goto" target) (:immortal)
  (let ((destination (find-target-room ch target)))
    (when destination
      (perform-goto ch destination t))))

(defcommand (ch "distance") (:immortal)
  (send-to-char ch "You need to supply a target to calculate the distance.~%"))

(defcommand (ch "distance" target) (:immortal)
  (let ((room (find-target-room ch target)))
    (when room
      (let ((steps (find-distance (in-room-of ch) room)))
        (if steps
            (send-to-char ch "Room ~d is ~d steps away.~%"
                          (number-of room)
                          steps)
            (send-to-char ch "There is no valid path to room ~d.~%"
                          (number-of room)))))))

(defcommand (ch "transport") (:immortal)
  (send-to-char ch "Whom do you wish to transport?~%"))

(defcommand (ch "transport" targets) (:immortal)
  (dolist (name (split-sequence #\space targets :remove-empty-subseqs t))
    (let ((tch (get-char-vis ch name)))
      (if tch
          (perform-transport ch tch)
          (send-to-char ch "You can't detect any '~a'~%" name)))))

(defcommand (ch "teleport") (:immortal)
  (send-to-char ch "Whom do you wish to teleport?~%"))

(defcommand (ch "teleport" name) (:immortal)
  (send-to-char ch "Where do you wish to send this person?~%"))

(defcommand (ch "teleport" name "to" target) (:immortal)
  (let ((tch (get-char-vis ch name))
        (room (find-target-room ch target)))
    (cond
      ((null tch)
       (send-to-char ch "Nobody around by that name.~%"))
      ((null room)
       nil)
      ((eql ch tch)
       (send-to-char ch "Use 'goto' to teleport yourself.~%"))
      (t
       (send-to-char ch "Okay.~%")
       (act tch :place-emit "$n disappears in a puff of smoke.")
       (char-from-room tch)
       (char-to-room tch room nil)
       (act tch :target ch
            :subject-emit "$N has teleported you!"
            :place-emit "$n arrives from a puff of smoke.")
       (look-at-room tch (in-room-of tch) nil)
       (slog "(GC) ~a has teleported ~a to ~a[~d]"
             (name-of ch)
             (name-of tch)
             (name-of room)
             (number-of room))))))

(defcommand (ch "vnum") (:immortal)
  (send-to-char ch "Usage: vnum { obj | mob } <name>~%"))

(defcommand (ch "vnum" other) (:immortal)
  (declare (ignore other))
  (send-to-char ch "Usage: vnum { obj | mob } <name>~%"))

(defcommand (ch "vnum" "mobiles" name) (:immortal)
  (when (zerop (vnum-prototypes ch name #\y *mobile-prototypes*))
    (send-to-char ch "No mobiles by that name.~%")))

(defcommand (ch "vnum" "objects" name) (:immortal)
  (when (zerop (vnum-prototypes ch name #\g *object-prototypes*))
    (send-to-char ch "No objects by that name.~%")))

(defcommand (ch "shutdown") (:immortal)
  (send-to-char ch "Shutting down...~%")
  (setf *shutdown* t))

(defcommand (ch "shutdown" mode) (:immortal)
  (declare (ignore mode))
  (send-to-char ch "Shutting down...~%")
  (setf *shutdown* t))

(defcommand (ch "eval" expr) (:wizard)
  "Command function to evaluate an arbitrary lisp expression."
  (handler-case
      (let* ((*standard-output* (make-string-output-stream))
             (*package* (find-package "TEMPUS"))
             (results (multiple-value-list (eval `(let ((self ,ch))
                                                    (declare (ignorable self))
                                                    ,(read-from-string expr))))))
        (cxn-write (link-of ch) "~a~{~s~%~}"
                   (get-output-stream-string *standard-output*)
                   results))
    (error (err)
        (cxn-write (link-of ch) "ERROR: ~a~%" err))))

(defcommand (ch "force") (:wizard)
  (send-to-char ch "You want to force who what?~%"))

(defcommand (ch "force" target) (:wizard)
  (declare (ignore target))
  (send-to-char ch "What do you want to force them to do?~%"))

(defcommand (ch "force" target "to" command) (:wizard)
  (let ((victs (get-matching-objects ch target (people-of (in-room-of ch)))))
    (cond
      ((null victs)
       (send-to-char ch "You don't see any '~a' here.~%" target))
      (t
       (send-to-char ch "You got it.~%")
       (dolist (vict victs)
         (tempus::interpret-command vict command))))))
