(in-package #:tempus)

(defparameter +xset-params+
  '(("trigger" :type string :slot trigger :desc "search trigger")
    ("keywords" :type string :slot keywords :desc "search keywords")
    ("to_vict" :type string :slot to_vict :desc "search vict emit")
    ("to_room" :type string :slot to_room :desc "search room emit")
    ("to_remote" :type string :slot to_remote :desc "search remote emit")
    ("command" :type enumerated :slot command :desc "search command"
     :table +search-commands+)
    ("value" :type number :slot value :desc "search value" :indexed 3)
    ("flags" :type bitflag :slot flags :desc "search" :table +search-bits+)
    ("fail_chance" :type number :slot fail_chance :desc "search failure chance")))

(defun perform-create-search (ch trigger keywords)
  (cond
    ((find-if (lambda (search)
                (and (equal trigger (trigger-of search))
                     (equal keywords (keywords-of search))))
              (searches-of (in-room-of ch)))
     (send-to-char ch "There is already a search here on that trigger.~%"))
    (t
     (let ((new-search (make-instance 'special-search-data
                                      :trigger trigger
                                      :keywords keywords)))
       (push new-search (searches-of (in-room-of ch)))
       (setf (olc-srch-of ch) new-search)
       (send-to-char ch "Search creation successful.~%")
       (send-to-char ch "Now editing search (~a)/(~a)~%"
                     trigger keywords)))))

(defun perform-destroy-search (ch trigger keywords)
  (let ((search (find-if (lambda (search)
                           (and (equal trigger (trigger-of search))
                                (equal keywords (keywords-of search))))
                         (searches-of (in-room-of ch)))))
    (cond
      ((null search)
       (send-to-char ch "There is no such search here.~%"))
      (t
       (setf (searches-of (in-room-of ch)) (delete search (searches-of (in-room-of ch))))
       (dolist (tch *characters*)
         (when (and (typep tch 'player)
                    (eql (olc-srch-of tch) search))
           (setf (olc-srch-of tch) nil)
           (send-to-char tch "The search you were editing has been destroyed.~%")))
       (send-to-char ch "Search destroyed.~%")))))

(defun perform-edit-search (ch trigger keywords)
  (let ((search (find-if (lambda (search)
                           (and (equal trigger (trigger-of search))
                                (equal keywords (keywords-of search))))
                         (searches-of (in-room-of ch)))))
    (cond
      ((null search)
       (send-to-char ch "There is no such search here.~%"))
      (t
       (setf (olc-srch-of ch) search)
       (send-to-char ch "Now editing search (~a)/(~a)~%"
                     (trigger-of search)
                     (keywords-of search))))))

(defun perform-xlist (ch zone command)
  (with-pagination ((link-of ch))
    (dolist (room (world-of zone))
      (let ((searches (remove command (searches-of room) :test-not #'= :key 'command-of)))
        (when searches
          (send-to-char ch "-- Room [&c~d&n] &c~a&n~%" (number-of room) (name-of room))
          (dolist (search searches)
            (format-search-data ch room search)))))))

(defcommand (ch "olc" "create" "search") (:immortal)
  (send-to-char ch "Usage: olc create search <trigger word> [<keywords>]~%"))

(defcommand (ch "olc" "create" "search" junk) (:immortal)
  (declare (ignore junk))
  (send-to-char ch "Usage: olc create search <trigger word> [<keywords>]~%"))

(defcommand (ch "olc" "create" "search" trigger) (:immortal)
  (when (check-can-edit ch (zone-of (in-room-of ch)) +zone-rooms-approved+)
    (perform-create-search ch trigger nil)))

(defcommand (ch "olc" "create" "search" trigger keywords) (:immortal)
  (when (check-can-edit ch (zone-of (in-room-of ch)) +zone-rooms-approved+)
    (perform-create-search ch trigger keywords)))

(defcommand (ch "olc" "destroy" "search") (:immortal)
  (send-to-char ch "Usage: olc destroy search <trigger word> [<keywords>]~%"))

(defcommand (ch "olc" "destroy" "search" trigger) (:immortal)
  (when (check-can-edit ch (zone-of (in-room-of ch)) +zone-rooms-approved+)
    (perform-destroy-search ch trigger nil)))

(defcommand (ch "olc" "destroy" "search" trigger keywords) (:immortal)
  (when (check-can-edit ch (zone-of (in-room-of ch)) +zone-rooms-approved+)
    (perform-destroy-search ch trigger keywords)))

(defcommand (ch "olc" "xedit" trigger) (:immortal)
  (when (check-can-edit ch (zone-of (in-room-of ch)) +zone-rooms-approved+)
    (perform-edit-search ch trigger nil)))

(defcommand (ch "olc" "xedit" trigger keywords) (:immortal)
  (when (check-can-edit ch (zone-of (in-room-of ch)) +zone-rooms-approved+)
    (perform-edit-search ch trigger keywords)))

(defcommand (ch "olc" "xset") (:immortal)
  (when (and (check-is-editing ch "search" (olc-srch-of ch))
             (check-can-edit ch (zone-of (in-room-of ch)) +zone-rooms-approved+))
    (send-to-char ch "Valid xset commands:~%~{  ~a~%~}"
                  (delete "xset"
                          (remove "olc" *commands* :test-not #'string=
                                  :key (lambda (cmd)
                                         (first (command-info-pattern cmd))))
                          :test-not #'string=
                          :key (lambda (cmd)
                                 (second (command-info-pattern cmd)))))))

(defcommand (ch "olc" "xset" param value) (:immortal)
  (when (and (check-is-editing ch "search" (olc-srch-of ch))
             (check-can-edit ch (zone-of (in-room-of ch)) +zone-rooms-approved+))
    (perform-set ch (olc-srch-of ch) t +xset-params+ param value)))

(defcommand (ch "olc" "xset" junk) (:immortal)
  (when (and (check-is-editing ch "search" (olc-srch-of ch))
             (check-can-edit ch (zone-of (in-room-of ch)) +zone-rooms-approved+))
    (send-to-char ch "No such xset command '~a'~%" junk)))

(defcommand (ch "olc" "xset" "trigger" trigger) (:immortal)
  (when (and (check-is-editing ch "search" (olc-srch-of ch))
             (check-can-edit ch (zone-of (in-room-of ch)) +zone-rooms-approved+))
    (setf (trigger-of (olc-srch-of ch)) trigger)
    (send-to-char ch "Search command trigger set.~%")))

(defcommand (ch "olc" "xset" "keywords") (:immortal)
  (when (and (check-is-editing ch "search" (olc-srch-of ch))
             (check-can-edit ch (zone-of (in-room-of ch)) +zone-rooms-approved+))
    (setf (keywords-of (olc-srch-of ch)) nil)
    (send-to-char ch "Search argument keywords cleared.~%")))

(defcommand (ch "olc" "xset" "keywords" keywords) (:immortal)
  (when (and (check-is-editing ch "search" (olc-srch-of ch))
             (check-can-edit ch (zone-of (in-room-of ch)) +zone-rooms-approved+))
    (setf (keywords-of (olc-srch-of ch)) keywords)
    (send-to-char ch "Search argument keywords set.~%")))

(defcommand (ch "olc" "xset" "to_vict" to-vict) (:immortal)
  (when (and (check-is-editing ch "search" (olc-srch-of ch))
             (check-can-edit ch (zone-of (in-room-of ch)) +zone-rooms-approved+))
    (setf (to-vict-of (olc-srch-of ch)) to-vict)
    (send-to-char ch "To_vict message set.~%")))

(defcommand (ch "olc" "xset" "to_room" to-room) (:immortal)
  (when (and (check-is-editing ch "search" (olc-srch-of ch))
             (check-can-edit ch (zone-of (in-room-of ch)) +zone-rooms-approved+))
    (setf (to-room-of (olc-srch-of ch)) to-room)
    (send-to-char ch "To_room message set.~%")))

(defcommand (ch "olc" "xset" "to_remote" to-remote) (:immortal)
  (when (and (check-is-editing ch "search" (olc-srch-of ch))
             (check-can-edit ch (zone-of (in-room-of ch)) +zone-rooms-approved+))
    (setf (to-remote-of (olc-srch-of ch)) to-remote)
    (send-to-char ch "To_remote message set.~%")))

(defcommand (ch "olc" "xset" "command" command) (:immortal)
  (when (and (check-is-editing ch "search" (olc-srch-of ch))
             (check-can-edit ch (zone-of (in-room-of ch)) +zone-rooms-approved+))
    (let ((cmd (position command +search-commands+ :test 'string-abbrev)))
      (cond
        ((null cmd)
         (send-to-char ch "No such search command '~a'~%" command))
        (t
         (setf (command-of (olc-srch-of ch)) cmd)
         (send-to-char ch "Search command set.~%"))))))

(defcommand (ch "olc" "xset" "value" index value) (:immortal)
  (when (and (check-is-editing ch "search" (olc-srch-of ch))
             (check-can-edit ch (zone-of (in-room-of ch)) +zone-rooms-approved+))
    (let ((idx (parse-integer index :junk-allowed t))
          (val (parse-integer value :junk-allowed t)))
      (cond
        ((or (null idx) (null val))
         (send-to-char ch "Usage: olc xset value (0|1|2) <value>~%"))
        (t
         (setf (aref (arg-of (olc-srch-of ch)) idx) val)
         (send-to-char ch "Ok, value set.~%"))))))

(defcommand (ch "olc" "xset" "flags" junk) (:immortal)
  (declare (ignore junk))
  (send-to-char ch "Usage: olc xset flags (+|-) <flags>~%"))

(defcommand (ch "olc" "xset" "flags" plus-or-minus flag-names) (:immortal)
  (when (and (check-is-editing ch "search" (olc-srch-of ch))
             (check-can-edit ch (zone-of (in-room-of ch)) +zone-rooms-approved+))
    (perform-set-flags ch plus-or-minus (ppcre:split "\\s+" flag-names)
                       +search-bits+
                       "search"
                       "olc xset flags (+|-) <flags>"
                       (lambda ()
                         (flags-of (olc-srch-of ch)))
                       (lambda (val)
                         (setf (flags-of (olc-srch-of ch)) val)))))

(defcommand (ch "olc" "xset" "fail_chance" chance) (:immortal)
  (when (and (check-is-editing ch "search" (olc-srch-of ch))
             (check-can-edit ch (zone-of (in-room-of ch)) +zone-rooms-approved+))
    (let ((chance (parse-integer chance :junk-allowed t)))
      (cond
        ((null chance)
         (send-to-char ch "Usage: olc xset fail_chance <percent chance>~%"))
        ((not (<= 0 chance 100))
         (send-to-char ch "The failure chance must be from 0 to 100.~%"))
        (t
         (setf (fail-chance-of (olc-srch-of ch)) chance)
         (send-to-char ch "This search will now have a ~d% chance of failure.~%" chance))))))

(defcommand (ch "olc" "xstat") (:immortal)
  (when (and (check-is-editing ch "search" (olc-srch-of ch))
             (check-can-edit ch (zone-of (in-room-of ch)) +zone-rooms-approved+))
    (format-search-data ch (in-room-of ch) (olc-srch-of ch))))

(defcommand (ch "xlist") (:immortal)
  (perform-xlist ch (zone-of (in-room-of ch)) nil))

(defcommand (ch "xlist" option-str) (:immortal)
  (let ((zone (zone-of (in-room-of ch)))
        (command nil))
    (loop
       for (opt val) on (split-sequence #\space option-str :remove-empty-subseqs t) by #'cddr
       do
         (cond
           ((string-abbrev opt "zone")
            (let ((num (parse-integer val :junk-allowed t)))
              (cond
                ((null num)
                 (send-to-char ch "That's not a valid zone number.~%")
                 (return-from do-xlist-option-str))
                ((null (real-zone num))
                 (send-to-char ch "There's not a zone by that number.~%")
                 (return-from do-xlist-option-str))
                (t
                 (setf zone (real-zone num))))))
           ((string-abbrev opt "type")
            (let ((num (position val +search-commands+ :test 'string-abbrev)))
              (cond
                ((null num)
                 (send-to-char ch "That's not a valid search type.~%")
                 (return-from do-xlist-option-str))
                (t
                 (setf command num)))))
           (t
            (send-to-char ch "Usage: xlist [(help|zone <zone number>|type <search type>)...]~%")
            (return-from do-xlist-option-str))))
    (perform-xlist ch zone command)))