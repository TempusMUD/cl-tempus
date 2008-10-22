(in-package #:tempus)

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



(defun perform-goto (ch room)
  (act ch :place-emit (cond
                        ((or (null (poofout-of ch))
                             (string= "" (poofout-of ch)))
                          "$n disappears in a cloud of smoke!")
                        ((search "$n" (poofout-of ch))
                         (poofout-of ch))
                        (t
                         (format nil "$n ~a" (poofout-of ch)))))
  (when (in-room-of ch)
    (char-from-room ch))
  (char-to-room ch room)
  (act ch :place-emit (cond
                        ((or (null (poofin-of ch))
                             (string= "" (poofin-of ch)))
                          "$n appears in a puff of smoke!")
                        ((search "$n" (poofin-of ch))
                         (poofin-of ch))
                        (t
                         (format nil "$n ~a" (poofin-of ch)))))
  (look-at-room ch room nil))

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

(defcommand (ch "goto" target) (:immortal)
  (let ((destination (find-target-room ch target)))
    (when destination
        (perform-goto ch destination))))

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


(defcommand (ch "shutdown") (:immortal)
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
