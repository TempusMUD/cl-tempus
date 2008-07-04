(in-package #:tempus)

(defun perform-goto (ch room-num)
  (let ((destination (real-room room-num)))
    (cond
      ((null destination)
       (send-to-char ch "No room exists with that number.~%"))
      (t
       (when (in-room-of ch)
         (char-from-room ch))
       (char-to-room ch destination)
       (look-at-room ch destination nil)))))

(defcommand (ch "goto" room-num) (:immortal)
  (perform-goto ch (parse-integer room-num)))
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