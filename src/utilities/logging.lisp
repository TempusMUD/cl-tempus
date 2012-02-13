(in-package #:tempus)

(defun mlog (message &key group level type write-to-file)
  (declare (ignorable group level type))
  (when write-to-file
    (let ((*print-pretty* nil)
          (*print-lines* nil))
      (format *log-output* "(~s ~s)~%" (local-time:now) message))
    (force-output))

  (unless (and group level)
    (return-from mlog))

  ;; Write to the people online
  nil)

(defun errlog (fmt &rest args)
  (let ((msg (format nil "SYSERR: ~?" fmt args))
        (backtrace (format nil "~{TRACE: ~s~%~}"
                           (let ((backt (sb-debug:backtrace-as-list)))
                             (subseq backt 2 (min 20 (length backt)))))))
    (mlog msg :group "coder" :level +level-ambassador+ :level :normal :write-to-file t)
    (mlog backtrace :group "coder" :level +level-ambassador+ :level :normal :write-to-file t)))

(defun slog (fmt &rest args)
  (let ((msg (format nil "~?" fmt args)))
    (mlog msg :level :complete :write-to-file t)))

(defparameter +mudlog-priorities+ '(emergency alert critical error warning notice info debug))

(defun mudlog (priority write-to-file fmt &rest args)
  "Logs the message given to file with a timestamp"
  (let ((message (format nil "~?" fmt args))
        (time-str (format-timestring nil (now)
                                      :format '(:short-month #\space
                                                (:day 2 #\space) #\space
                                                :hour #\:
                                                (:min 2) #\:
                                                (:sec 2))))
        (priority-num (position priority +mudlog-priorities+)))
    (assert priority-num)
    (check-type write-to-file boolean)
    (dolist (cxn *cxns*)
      (when (and (typep cxn 'tempus-cxn)
                 (eql (state-of cxn) 'playing)
                 (actor-of cxn)
                 (immortalp (actor-of cxn))
                 (or (pref-flagged (actor-of cxn) +pref-log1+)
                     (pref-flagged (actor-of cxn) +pref-log2+)))
        (cxn-write cxn "&g[ ~a - ~a ]&n~%"
                   (string-replace "&" message "&&")
                   time-str)))
    (when write-to-file
      (format *log-output* "(~a \"~?\" ~a)~%" priority fmt args
              (local-time:now))
      (force-output))))

(defun syslog (fmt &rest args)
  "Logs a message to *STANDARD-OUTPUT* with a timestamp, and sends it to all immortals who aren't editing something."
  (format *log-output* "(~s \"~?\")~%" (local-time:now) fmt args)
  (force-output))

#+nil (defun errlog (fmt &rest args)
  "Logs an error to syslog with attached backtrace"
  (let ((*standard-output* *error-output*))
    (mudlog 'debug t "ERROR: ~?" fmt args)
    (format t "~{:::: ~s~%~}"
            (let ((backtrace (sb-debug:backtrace-as-list)))
              (subseq backtrace 2
                      (let ((bottom (position 'game-loop backtrace :key #'first)))
                        (when bottom
                          (1+ bottom))))))
    (force-output)))