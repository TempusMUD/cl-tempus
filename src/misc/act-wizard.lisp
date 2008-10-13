(in-package #:tempus)

(defun perform-goto (ch room)
  (act ch :place-emit (if (poofout-of ch)
                          (format nil "$n ~a" (poofout-of ch))
                          "$n disappears in a cloud of smoke!"))
  (when (in-room-of ch)
    (char-from-room ch))
  (char-to-room ch room)
  (act ch :place-emit (if (poofout-of ch)
                          (format nil "$n ~a" (poofout-of ch))
                          "$n appears in a puff of smoke!"))
  (look-at-room ch room nil))

(defcommand (ch "goto" target) (:immortal)
  (if (every #'digit-char-p target)
      (let ((destination (real-room (parse-integer target))))
        (if destination
            (perform-goto ch destination)
            (send-to-char ch "No room exists with that number.~%")))
      (let ((target-chs (get-matching-objects ch target *characters*)))
        (if target-chs
            (perform-goto ch (in-room-of (first target-chs)))
            (send-to-char ch "Couldn't find any '~a'~%" target)))))

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
