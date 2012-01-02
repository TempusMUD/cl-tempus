(in-package :tempus)

(defclass help-item ()
  ((idnum :accessor idnum-of :initarg :idnum)
   (name :accessor name-of :initarg :name)
   (keywords :accessor keywords-of :initarg :keywords)
   (text :accessor text-of :initarg :text)
   (groups :accessor groups-of :initarg :groups :initform 0)
   (counter :accessor counter-of :initarg :counter :initform 0)
   (flags :accessor flags-of :initarg :flags :initform 0)
   (owner :accessor owner-of :initarg :owner :initform 0)
   (editor :accessor editor-of :initarg :editor :initform nil)))

(defmethod print-object ((item help-item) stream)
  (print-unreadable-object (item stream :type t)
    (format stream "~d ~s" (idnum-of item) (name-of item))))

(defconstant +hflag-unapproved+ (ash 1 0)) ; help topic cannot be found with commands
(defconstant +hflag-immhelp+ (ash 1 1)) ; help topic can only be found by gods
(defconstant +hflag-modified+ (ash 1 2)) ; help topic has been modified

(defparameter +help-flags+ #("!APP" "IMM+" "MOD"))
(defparameter +help-flag-names+ #("unapproved" "immortal" "modified"))

(defparameter +help-group-flags+
  #("OLC" "MISC" "NEWB" "SKIL" "SPEL" "CLAS" "CITY" "PLR" "MAGE" "CLE"
    "THI" "WAR" "BARB" "PSI" "PHY" "CYB" "KNIG" "RANG" "BARD" "MONK"
    "MERC" "HEDT" "HGOD" "IMM" "QCTR"))

(defparameter +help-group-names+
  #("olc" "misc" "newbie" "skill" "spell" "class" "city" "player" "mage"
    "cleric" "thief" "warrior" "barb" "psionic" "physic" "cyborg" "knight"
    "ranger" "bard" "monk" "merc" "helpeditors" "helpgod" "immhelp" "qcontrol"))

(defvar *help-items* nil)

(defun boot-help-system ()
  (setf *help-items* nil)
  (with-open-file (inf (tempus-path "lib/text/help_data/index"))
    (let ((item-count (parse-integer (read-line inf))))
      (dotimes (idx item-count)
        (destructuring-bind (idnum groups counter flags owner)
            (mapcar #'parse-integer (cl-ppcre:split "\\s+" (read-line inf)))
          (let ((name (read-line inf))
                (keywords (read-line inf)))
            (push (make-instance 'help-item
                                 :idnum idnum
                                 :name name
                                 :keywords keywords
                                 :groups groups
                                 :counter counter
                                 :flags flags
                                 :owner owner)
                  *help-items*))))))
  (setf *help-items* (sort *help-items* #'< :key 'idnum-of))
  (slog "~d items read from help file index" (length *help-items*))
  t)

(defun load-help-item (item)
  (with-open-file (inf (tempus-path "lib/text/help_data/~4,'0d.topic"
                                    (idnum-of item)))
    (destructuring-bind (idnum length)
        (mapcar 'parse-integer (cl-ppcre:split "\\s+" (read-line inf)))
      (assert (= idnum (idnum-of item))
              nil
              "idnums don't match in load-help-item")
      (read-line inf)                   ; this is the item name
      (setf (text-of item) (make-string length))
      (read-sequence (text-of item) inf)
      (setf (text-of item) (delete #\return (text-of item))))))

(defun save-help-item (item)
  (unless (null (text-of item))
    (with-open-file (ouf (tempus-path "lib/text/help_data/~4,'0d.topic"
                                      (idnum-of item))
                         :direction :output
                         :if-exists :rename-and-delete)
      (format ouf "~d ~d~%~a~%~a"
              (idnum-of item)
              (length (text-of item))
              (name-of item)
              (text-of item)))
    (setf (flags-of item) (logandc2 (flags-of item) +hflag-modified+))))

(defun save-help-index ()
  (with-open-file (ouf (tempus-path "lib/text/help_data/index")
                       :direction :output
                       :if-exists :rename-and-delete
                       :if-does-not-exist :create)
    (format ouf "~d~%" (length *help-items*))
    (dolist (item (sort *help-items* #'< :key 'idnum-of))
      (format ouf "~d ~d ~d ~d ~d~%~a~%~a~%"
              (idnum-of item)
              (groups-of item)
              (counter-of item)
              (flags-of item)
              (owner-of item)
              (name-of item)
              (keywords-of item)))))

(defun term-matches-keywords (term keywords)
  (let ((start-pos 0))
    (loop
       while (< start-pos (length keywords))
       do
         (cond
            ((eql (char keywords start-pos) #\")
             (let ((end-pos (or (position #\" keywords :start (1+ start-pos))
                                (1+ (length keywords)))))
               (when (string-abbrev term keywords
                                    :start2 (1+ start-pos)
                                    :end2 end-pos)
                 (return-from term-matches-keywords t))
               (setf start-pos (or (position #\space keywords :start (1- end-pos)
                                             :test-not #'eql)
                                   (length keywords)))))
            (t
             (let ((end-pos (or (position #\space keywords :start (1+ start-pos))
                                (length keywords))))
               (when (string-abbrev term keywords :start2 start-pos :end2 end-pos)
                 (return-from term-matches-keywords t))
               (setf start-pos (1+ end-pos))))))
    nil))

(defun send-help-item-stat (ch item)
  (load-help-item item)
  (with-pagination ((link-of ch))
    (send-to-char ch "&c~3d. &y~25a&cGroups:&n ~20a &cFlags: &n~a~%"
                  (idnum-of item)
                  (name-of item)
                  (printbits (groups-of item) +help-group-flags+ "NOBITS")
                  (printbits (flags-of item) +help-flags+ "NOBITS"))
    (send-to-char ch "     &cKeywords: [ &n~a&c ]&n~%~a~%"
                  (keywords-of item)
                  (text-of item))))

(defun send-help-item (ch item)
  (load-help-item item)
  (with-pagination ((link-of ch))
    (send-to-char ch "&c~a&n~%~a~%"
                  (name-of item)
                  (text-of item))))

(defcommand (ch "hcollect") (:immortal)
  (send-to-char ch "~
hcollect commands:
  approve         <topic #>
  create
  edit            <topic #>
  info
  set             <topic #> <value>
  stat            <topic #>
  search          <keyword>
  unapprove       <topic #>
  swap            <topic #> <topic #>
"))

(defcommand (ch "hcollect" "approve" topic) (:immortal)
  (let* ((idnum (parse-integer topic :junk-allowed t))
         (item (find idnum *help-items* :key 'idnum-of)))
    (cond
      ((null idnum)
       (send-to-char ch "Approve which item?~%"))
      ((null item)
       (send-to-char ch "Unable to find item.~%"))
      (t
       (setf (flags-of item) (logandc2 (flags-of item) +hflag-unapproved+))
       (setf (flags-of item) (logior (flags-of item) +hflag-modified+))
       (send-to-char ch "Item #~d approved.~%" idnum)))))

(defcommand (ch "hcollect" "create") (:immortal)
  (let* ((new-id (1+ (loop for item in *help-items* maximize (idnum-of item))))
         (new-item (make-instance 'help-item
                         :idnum new-id
                         :flags +hflag-modified+
                         :owner (idnum-of ch)
                         :editor (idnum-of ch))))
    (push new-item *help-items*)
    (save-help-item new-item)
    (save-help-index)
    (send-to-char ch "Item #~d created.~%" new-id)
    (slog "~a has created help topic #~d" new-id)))

(defcommand (ch "hcollect" "edit" topic) (:immortal)
  (let* ((idnum (parse-integer topic :junk-allowed t))
         (item (find idnum *help-items* :key 'idnum-of)))
    (cond
      ((null idnum)
       (send-to-char ch "Approve which item?~%"))
      ((null item)
       (send-to-char ch "Unable to find item.~%"))
      ((and (editor-of item)
            (= (editor-of item) (idnum-of ch)))
       (send-to-char ch "I don't see how editing it _again_ will help any.~%"))
      ((editor-of item)
       (send-to-char ch "~a is already editing that item.  Tough!~%"
                     (retrieve-player-name (editor-of item))))
      (t
       (when (olc-help-of ch)
         (setf (editor-of (olc-help-of ch)) nil))
       (setf (olc-help-of ch) item)
       (setf (editor-of item) (idnum-of ch))
       (send-to-char ch "You are now editing help item #~d.~%" idnum)))))

(defcommand (ch "hcollect" "info") (:immortal)
  (let ((num-topics 0)
        (num-modified 0)
        (num-unapproved 0)
        (num-editing 0)
        (num-groupless 0))
    (dolist (item *help-items*)
      (incf num-topics)
      (unless (zerop (logand (flags-of item) +hflag-modified+))
        (incf num-modified))
      (unless (zerop (logand (flags-of item) +hflag-unapproved+))
        (incf num-unapproved))
      (when (editor-of item)
        (incf num-editing))
      (when (zerop (groups-of item))
        (incf num-groupless)))
    (send-to-char ch "&cTopics [&n~d&c] &yUnapproved [&n~d&y] &gModified [&n~d&g] &cEditing [&n~d&c] &rGroupless [&n~d&r]&n~%"
                     num-topics
                     num-unapproved
                     num-modified
                     num-editing
                     num-groupless)))

(defcommand (ch "hcollect" "set" topic) (:immortal)
  (declare (ignore topic))
  (send-to-char ch "hcollect set <groups[+/-]|flags[+/-]|name|keywords|description> [args]~%"))

(defcommand (ch "hcollect" "set" "name" name) (:immortal)
  (cond
    ((null (olc-help-of ch))
     (send-to-char ch "You have to be editing an item to set it.~%"))
    (t
     (setf (name-of (olc-help-of ch)) name)
     (save-help-item (olc-help-of ch))
     (save-help-index)
     (send-to-char ch "Name set!~%"))))

(defcommand (ch "hcollect" "set" "keywords" keywords) (:immortal)
  (cond
    ((null (olc-help-of ch))
     (send-to-char ch "You have to be editing an item to set it.~%"))
    (t
     (setf (keywords-of (olc-help-of ch)) keywords)
     (save-help-item (olc-help-of ch))
     (save-help-index)
     (send-to-char ch "Keywords set!~%"))))

(defcommand (ch "hcollect" "set" "description") (:immortal)
  (cond
    ((null (olc-help-of ch))
     (send-to-char ch "You have to be editing an item to set it.~%"))
    (t
     (load-help-item (olc-help-of ch))
     (start-text-editor (link-of ch)
                        (olc-help-of ch)
                        "a help item"
                        (text-of (olc-help-of ch))
                        (lambda (cxn target buf)
                          (setf (text-of target) buf)
                          (save-help-item target)
                          (cxn-write cxn "Description set!~%")
                          (setf (state-of cxn) 'playing))
                        (lambda (cxn target)
                          (declare (ignore target))
                          (setf (state-of cxn) 'playing))))))

(defcommand (ch "hcollect" "set" "groups" plus-or-minus groups) (:immortal)
  (let ((group-names (cl-ppcre:split "\\s+" groups)))
    (cond
      ((null (olc-help-of ch))
       (send-to-char ch "You have to be editing an item to set it.~%"))
      ((not (or (string= plus-or-minus "+")
                (string= plus-or-minus "-")))
       (send-to-char ch "Usage: hcollect set groups +/- <groups>~%"))
      (t
       (dolist (group-name group-names)
         (let ((group-id (position group-name +help-group-names+
                                   :test 'string-abbrev)))
           (cond
             ((null group-id)
              (send-to-char ch "'~a' is not a valid help group.~%" group-name))
             ((string= plus-or-minus "-")
              (setf (groups-of (olc-help-of ch))
                    (logandc2 (groups-of (olc-help-of ch))
                              (ash 1 group-id)))
              (send-to-char ch "Item removed from group ~a.~%"
                            (aref +help-group-names+ group-id)))
             (t
              (setf (groups-of (olc-help-of ch))
                    (logior (groups-of (olc-help-of ch))
                            (ash 1 group-id)))
              (send-to-char ch "Item added to group ~a.~%" (aref +help-group-names+ group-id))))))))))

(defcommand (ch "hcollect" "set" "flags" plus-or-minus flags) (:immortal)
  (let ((flag-names (cl-ppcre:split "\\s+" flags)))
    (cond
      ((null (olc-help-of ch))
       (send-to-char ch "You have to be editing an item to set it.~%"))
      ((not (or (string= plus-or-minus "+")
                (string= plus-or-minus "-")))
       (send-to-char ch "Usage: hcollect set flags +/- <flags>~%"))
      (t
       (dolist (flag-name flag-names)
         (let ((flag-id (position flag-name +help-flag-names+
                                   :test 'string-abbrev)))
           (cond
             ((null flag-id)
              (send-to-char ch "'~a' is not a valid help flag.~%" flag-name))
             ((string= plus-or-minus "-")
              (setf (flags-of (olc-help-of ch))
                    (logandc2 (flags-of (olc-help-of ch))
                              (ash 1 flag-id)))
              (send-to-char ch "Flag ~a unset on item.~%"
                            (aref +help-flag-names+ flag-id)))
             (t
              (setf (flags-of (olc-help-of ch))
                    (logior (flags-of (olc-help-of ch))
                            (ash 1 flag-id)))
              (send-to-char ch "Flag ~a set on item.~%" (aref +help-flag-names+ flag-id))))))))))

(defcommand (ch "hcollect" "stat") (:immortal)
  (if (olc-help-of ch)
      (send-help-item-stat ch (olc-help-of ch))
      (send-to-char ch "Stat which item?~%")))

(defcommand (ch "hcollect" "stat" topic) (:immortal)
  (let* ((idnum (parse-integer topic :junk-allowed t))
         (item (find idnum *help-items* :key 'idnum-of)))
    (cond
      ((null idnum)
       (send-to-char ch "Stat which item?~%"))
      ((null item)
       (send-to-char ch "Unable to find item.~%"))
      (t
       (send-help-item-stat ch item)))))

(defcommand (ch "hcollect" "search" term) (:immortal)
  (with-pagination ((link-of ch))
    (dolist (item *help-items*)
      (when (term-matches-keywords term (keywords-of item))
        (send-to-char ch "&c~3d. &y~25a&cGroups:&n ~20a &cFlags: &n~a~%"
                      (idnum-of item)
                      (name-of item)
                      (printbits (groups-of item) +help-group-flags+ "NOBITS")
                      (printbits (flags-of item) +help-flags+ "NOBITS"))))))

(defcommand (ch "hcollect" "unapprove" topic) (:immortal)
  (let* ((idnum (parse-integer topic :junk-allowed t))
         (item (find idnum *help-items* :key 'idnum-of)))
    (cond
      ((null idnum)
       (send-to-char ch "Unapprove which item?~%"))
      ((null item)
       (send-to-char ch "Unable to find item.~%"))
      (t
       (setf (flags-of item) (logior (flags-of item)
                                     +hflag-unapproved+
                                     +hflag-modified+))
       (send-to-char ch "Item #~d unapproved.~%" idnum)))))

(defcommand (ch "hcollect" "swap" topic-a topic-b) (:immortal)
  (let* ((idnum-a (parse-integer topic-a :junk-allowed t))
         (item-a (find idnum-a *help-items* :key 'idnum-of))
         (idnum-b (parse-integer topic-b :junk-allowed t))
         (item-b (find idnum-b *help-items* :key 'idnum-of)))
    (cond
      ((or (null idnum-a) (null idnum-b))
       (send-to-char ch "Invalid item numbers.~%"))
      ((or (null item-a) (null item-b))
       (send-to-char ch "Unable to find topics.~%"))
      ((and (editor-of item-a) (/= (editor-of item-a) (idnum-of ch)))
       (send-to-char ch "~a is already editing the first item.  Tough!~%"
                     (retrieve-player-name (editor-of item-a))))
      ((and (editor-of item-b) (/= (editor-of item-b) (idnum-of ch)))
       (send-to-char ch "~a is already editing the second item.  Tough!~%"
                     (retrieve-player-name (editor-of item-b))))
      (t
       (load-help-item item-a)
       (load-help-item item-b)
       (rotatef (idnum-of item-a)
                (idnum-of item-b))
       (save-help-item item-a)
       (save-help-item item-b)
       (save-help-index)
       (send-to-char ch "Help items ~d and ~d swapped.~%" idnum-a idnum-b)))))

(defcommand (ch "policy") ()
  (send-help-item ch (find 667 *help-items* :key 'idnum-of)))

(defcommand (ch "help") ()
  (send-help-item ch (find 666 *help-items* :key 'idnum-of)))

(defcommand (ch "help" topic) ()
  (dolist (item *help-items*)
    (when (and (not (zerop (logand (ash 1 7) (groups-of item))))
               (term-matches-keywords topic (keywords-of item))
               (or (zerop (logand (flags-of item) +hflag-immhelp+))
                   (immortal-level-p ch))
               (or (zerop (logand (flags-of item) +hflag-unapproved+))
                   (security-is-member ch "Help")))
      (send-help-item ch item)
      (return-from do-help-topic)))
  (send-to-char ch "No items were found matching your search criteria.~%"))

(defcommand (ch "immhelp") (:immortal)
  (send-help-item ch (find 699 *help-items* :key 'idnum-of)))

(defcommand (ch "immhelp" topic) (:immortal)
  (dolist (item *help-items*)
    (when (and (not (zerop (logand (ash 1 23) (groups-of item))))
               (term-matches-keywords topic (keywords-of item)))
      (send-help-item ch item)
      (return-from do-immhelp-topic)))
  (send-to-char ch "No items were found matching your search criteria.~%"))

(defcommand (ch "olchelp") (:immortal)
  (send-help-item ch (find 700 *help-items* :key 'idnum-of)))

(defcommand (ch "olchelp" topic) (:immortal)
  (dolist (item *help-items*)
    (when (and (not (zerop (logand 1 (groups-of item))))
               (term-matches-keywords topic (keywords-of item)))
      (send-help-item ch item)
      (return-from do-olchelp-topic)))
  (send-to-char ch "No items were found matching your search criteria.~%"))
