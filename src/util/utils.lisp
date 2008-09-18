(in-package :tempus)

(defun interpret-scan-directive (stream format-idx format)
  (case (char format format-idx)
    (#\s
     (if (< (length format) (1+ format-idx))
         (with-output-to-string (result)
           (loop for c = (read-char stream nil nil)
                 until (or (null c) (char= c (char format (1+ format-idx)))) do
                 (princ c result)))))))

#+nil (defun scan (stream format)
  (let ((values nil))
    (loop with format-idx = 0
          until (> format-idx (1- (length format))) do
          (cond
            ((char= (char format format-idx) #\~)
             ;; Interpret format directive
             (incf format-idx)
             (let ((val (interpret-scan-directive stream format-idx format)))
               (if val
                   (push values val)
                   (return-from scan nil)))
             (incf format-idx))
            ((char= (char format format-idx) #\space)
             ;; space characters match any number of any space chars
             (when (graphic-char-p (read-char stream nil nil))
               (incf format-idx)))
            ((not (char= c (char format format-idx)))
             ;; Didn't match
             (return-from scan nil))
            (t
             ;; literal match
             (incf format-idx)))
    (values-list values))))

(defun pin (val min max)
  (min (max val min) max))

(defun get-line (inf)
  (loop for line = (read-line inf nil nil)
        for count from 0
        while (and line (string/= line "") (char= (char line 0) #\*))
        finally (return (values line count))))

(defun mlog (message &key group level type write-to-file)
  (declare (ignorable group level type))
  (when write-to-file
    (format *standard-output* "(~s ~s)~%" (local-time:now) message)
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

(defclass regex-result ()
  ((target :accessor target-of :initarg :target)
   (pattern :accessor pattern-of :initarg :pattern)
   (match-start :accessor match-start-of :initarg :match-start)
   (match-end :accessor match-end-of :initarg :match-end)
   (reg-starts :accessor reg-starts-of :initarg :reg-starts)
   (reg-ends :accessor reg-ends-of :initarg :reg-ends)))

(defun regref (result idx)
  (assert result (result) "NIL regex-result passed to regref")
  (cond
    ((zerop idx)
      (subseq (target-of result)
              (match-start-of result)
              (match-end-of result)))
    ((null (aref (reg-starts-of result) (1- idx)))
     nil)
    (t
      (subseq (target-of result)
              (aref (reg-starts-of result) (1- idx))
              (aref (reg-ends-of result) (1- idx))))))

(defun scan (regex target &key start end)
  (assert target nil "Target for SCAN is NIL")
  (multiple-value-bind (match-start match-end reg-starts reg-ends)
      (cl-ppcre:scan regex target :start (or start 0) :end (or end (length target)))
    (when match-start
      (make-instance 'regex-result
                     :target target
                     :pattern regex
                     :match-start match-start
                     :match-end match-end
                     :reg-starts reg-starts
                     :reg-ends reg-ends))))

(defun snarf-file (path)
  "Returns a string with the contents of the file at PATH."
  (handler-case
      (with-open-file (inf path :direction :input)
        (let ((buf (make-string (file-length inf))))
          (read-sequence buf inf)
          buf))
    (error ()
      "")))

(defun mud-time-passed (t2 t1)
  "Calculate the MUD time passed over the last t2-t1 centuries (secs)"
  (let ((secs (- t2 t1)))
    (let* ((hour (mod (floor secs +secs-per-mud-hour+) 24))
           (secs (- secs (* hour +secs-per-mud-hour+))))
      (let* ((day (mod (floor secs +secs-per-mud-day+) 35))
             (secs (- secs (* day +secs-per-mud-day+))))
        (let* ((month (mod (floor secs +secs-per-mud-month+) 17))
               (secs (- secs (* month +secs-per-mud-month+))))
          (let* ((year (floor secs +secs-per-mud-year+)))
            (values hour day month year)))))))

(defparameter +mudlog-priorities+ '(emergency alert critical error warning notice info debug))

(defun mudlog (priority write-to-file fmt &rest args)
  "Logs the message given to file with a timestamp"
  (let ((message (format nil "~?" fmt args))
        (priority-num (position priority +mudlog-priorities+)))
    (assert priority-num)
	(dolist (cxn *cxns*)
	  (when (and (typep cxn 'tempus-cxn)
                 (eql (state-of cxn) 'playing)
                 (actor-of cxn)
                 (immortalp (actor-of cxn)))
		(cxn-write cxn "&g[ ~a - ~a ]&n~%"
                   (string-replace "&" message "&&")
                   (format-timestring nil (now)
                                      :format '(:short-month #\space
                                                (:day 2 #\space) #\space
                                                :hour #\:
                                                (:min 2) #\:
                                                (:sec 2))))))
	(when write-to-file
      (syslog "~a" message))))


(defun syslog (fmt &rest args)
  "Logs a message to *STANDARD-OUTPUT* with a timestamp, and sends it to all immortals who aren't editing something."
  (format t "(~s \"~?\")~%" (local-time:now) fmt args)
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


(defun curry (function &rest args)
  (lambda (&rest more-args)
    (apply function (append args more-args))))

(defun join-sequence (result-type delimiter seq-list)
	"Conceptually the opposite of split-sequence, joins together the
sequences in seq-list with the delimiter between each element"
	(apply #'concatenate
		result-type
		(first seq-list)
		(loop for elt in (rest seq-list) append (list delimiter elt))))

(defun hash-to-assoc (hash)
  (let ((result nil))
	(maphash (lambda (key val)
			   (push (list key val) result))
			 hash)
	(nreverse result)))

(defun assoc-to-hash (assoc &rest options)
  (let ((result (apply #'make-hash-table options)))
    (dolist (tuple assoc result)
      (setf (gethash (first tuple) result) (second tuple)))))

(defun char-vowel-p (c)
	(or (char= c #\a)
		(char= c #\e)
		(char= c #\i)
		(char= c #\o)
		(char= c #\u)
		(char= c #\y)))

(defun desc (viewer subject)
  (declare (ignore viewer))
  (name-of subject))

(defun expand-dollar (c viewer subject target item pov)
  (case c
	(#\n
	 (if (eql pov :self)
		 "you"
		 (desc viewer subject)))
	(#\e
	 (cond
	   ((eql pov :self)
		"you")
	   ((eql (sex-of subject) 'male)
		"he")
	   ((eql (sex-of subject) 'female)
		"she")
	   (t
		"it")))
	(#\m
	 (cond
	   ((and (eql pov :self) (eql subject target))
		"yourself")
	   ((eql pov :self)
		"you")
	   ((eql (sex-of subject) 'male)
		"him")
	   ((eql (sex-of subject) 'female)
		"her")
	   (t
		"it")))
	(#\s
	 (cond
	   ((eql pov :self)
		"your")
	   ((eql (sex-of subject) 'male)
		"his")
	   ((eql (sex-of subject) 'female)
		"her")
	   (t
		"its")))
	(#\N
	 (cond
	   ((eql subject target)
		(cond
		  ((eql pov :self)
		   "yourself")
		  ((eql (sex-of target) 'male)
		   "himself")
		  ((eql (sex-of target) 'female)
		   "herself")
		  (t
		   "itself")))
	   ((eql pov :target)
		"you")
	   (t
		(desc viewer target))))
	(#\E
	 (cond
	   ((eql pov :target)
		"you")
	   ((eql (sex-of target) 'male)
		"he")
	   ((eql (sex-of target) 'female)
		"she")
	   (t
		"it")))
	(#\M
	 (cond
	   ((and (eql pov :target) (eql target target))
		"yourself")
	   ((eql pov :target)
		"you")
	   ((eql (sex-of target) 'male)
		"him")
	   ((eql (sex-of target) 'female)
		"her")
	   (t
		"it")))
	(#\S
	 (cond
	   ((eql pov :target)
		"your")
	   ((eql (sex-of target) 'male)
		"his")
	   ((eql (sex-of target) 'female)
		"her")
	   (t
		"its")))
	(#\p
	 (name-of item))
	(#\%
	 (if (eql pov :self)
		 ""
		 "s"))
	(#\^
	 (if (eql pov :self)
		 ""
		 "es"))
	(#\a
     (if (mood-of subject)
         (concatenate 'string " " (mood-of subject))
	 	 ""))
    (#\l
     "")
	(#\$
	 "$")))



(defun act-escape (str)
  "Given STR, returns a string, which has escaped the characters considered meaningful by the ACT and ACT-EVENT function"
  (with-output-to-string (result)
    (loop for idx from 0 to (1- (length str)) do
          (princ
           (case (char str idx)
             (#\\
              "\\\\")
             (#\&
              "&&")
             (#\$
              "\\$")
             (#\]
              "\\]")
             (t
              (char str idx)))
           result))))

(defun act-unescape (str)
  (with-output-to-string (result)
    (loop for idx from 0 to (1- (length str)) do
          (princ
           (case (char str idx)
             (#\\
              (incf idx)
              (case (char str idx)
                (#\\
                 "\\")
                (#\$
                 "$")
                (#\]
                 "]")))
             (t
              (char str idx)))
           result))))

(defun act-str (viewer fmt subject target item pov)
  (with-output-to-string (result)
	(loop
       for idx from 0 to (1- (length fmt)) do
       (princ
        (case (char fmt idx)
          (#\\
           (incf idx)
           (char fmt idx))
          (#\$
           (incf idx)
           (cond
             ((eql (char fmt idx) #\{)
              (let ((default-mood
                     (subseq fmt
                             (1+ idx)
                             (position #\} fmt :start idx))))
                (incf idx (1+ (length default-mood)))
                (if (mood-of subject)
                    (format nil " ~a" (mood-of subject))
                    default-mood)))
             ((eql (char fmt idx) #\[)
              (let* ((match-pos (cl-ppcre:scan #/[^\\]\]/ fmt :start idx))
                     (end-brace-pos (when match-pos (1+ match-pos))))
                (prog1
                    (act-unescape (subseq fmt (1+ idx) end-brace-pos))
                  (setf idx end-brace-pos))))
             (t
              (expand-dollar (char fmt idx) viewer subject target item pov))))
          (#\&
           (incf idx)
           (concatenate 'string "&" (list (char fmt idx))))
          (t
           (char fmt idx)))
        result))))

(defun send-act-str (viewer emit subject target item pov)
  (when (or (can-see viewer subject)
            (and target (can-see viewer target)))
    (send-to-char viewer "~a~%" (act-str viewer emit subject target item pov))))

(defun act (subject &key (target nil) (item nil) (subject-emit nil) (target-emit nil) (not-target-emit nil) (place-emit nil) (all-emit nil))
  ;; Handle "all" emit
  (when all-emit
    (send-act-str subject all-emit subject target item :self)
	(when (and target (not (eql subject target)))
	  (send-act-str target all-emit subject target item :target))
	   (dolist (other (people-of (in-room-of subject)))
		 (unless (or (eql other subject) (eql other target))
		   (send-act-str other all-emit subject target item :other))))
  (when subject-emit
	(send-act-str subject subject-emit subject target item :self))
  (when (and target-emit target)
	(send-act-str target target-emit subject target item :target))
  (when not-target-emit
    (dolist (other (people-of (in-room-of subject)))
      (unless (or (eql other subject) (eql other target))
		(send-act-str other not-target-emit subject target item :other))))
  (when place-emit
    (dolist (other (people-of (in-room-of subject)))
      (unless (eql other subject)
		(send-act-str other place-emit subject target item
					  (if (eql other target) :target :other))))))

(defun act-event (subject &key (target nil) (item nil) (subject-emit nil) (target-emit nil) (not-target-emit nil) (place-emit nil) (all-emit nil))
  "Enqueues an emit event that acts like act, except in event order instead
of immediately."
  (enqueue-event 'emit
				 :kind 'emit
				 :actor subject
				 :target target
				 :item item
				 :subject-emit subject-emit
				 :target-emit target-emit
				 :not-target-emit not-target-emit
				 :place-emit place-emit
				 :all-emit all-emit))

(defun colorize (cxn str)
  (let ((ansi-level (if (account-of cxn)
						(ansi-level-of (account-of cxn))
						0)))
	(with-output-to-string (result)
	  (loop for idx from 0 to (1- (length str)) do
		   (princ
            (cond
              ((eql (char str idx) #\&)
               (case ansi-level
                 (0
                  (case (char str (incf idx))
                    (#\@ "[H[J")     (#\& "&")
                    (t "")))
                 (1
                  (case (char str (incf idx))
                    (#\n "[0m")    (#\r "[0;31m")
                    (#\g "[0m") (#\y "[0m")
                    (#\b "[0m") (#\m "[0m")
                    (#\c "[0;36m") (#\w "[0m")
                    (#\@ "[H[J")     (#\& "&")
                    (t "")))
                 (2
                  (case (char str (incf idx))
                    (#\n "[0m")    (#\r "[0;31m")
                    (#\g "[0;32m") (#\y "[0;33m")
                    (#\b "[0;34m") (#\m "[0;35m")
                    (#\c "[0;36m") (#\w "[0;37m")
                    (#\N "[0m")    (#\R "[0;31m")
                    (#\G "[0;32m") (#\Y "[0;33m")
                    (#\B "[0;34m") (#\M "[0;35m")
                    (#\C "[0;36m") (#\W "[0;37m")
                    (#\@ "[H[J")     (#\& "&")
                    (t "<BUG PLEASE REPORT>")))
                 (t
                  (case (char str (incf idx))
                    (#\n "[0m")    (#\r "[0;31m")
                    (#\g "[0;32m") (#\y "[0;33m")
                    (#\b "[0;34m") (#\m "[0;35m")
                    (#\c "[0;36m") (#\w "[0;37m")
                    (#\N "[1m")    (#\R "[1;31m")
                    (#\G "[1;32m") (#\Y "[1;33m")
                    (#\B "[1;34m") (#\M "[1;35m")
                    (#\C "[1;36m") (#\W "[1;37m")
                    (#\@ "[H[J")     (#\& "&")
                    (t "<BUG PLEASE REPORT>")))))
              (t
               (char str idx)))
              result)))))

(defun string-abbrev (abbrev str)
  "Returns T if ABBREV is at least one character, and is an abbreviation of STR."
  (let ((abbrev-len (length abbrev)))
    (unless (or (zerop abbrev-len) (> abbrev-len (length str)))
      (string-equal abbrev str :end2 abbrev-len))))

(defun string-replace (needle haystack replacement)
  "Returns a copy of HAYSTACK with all instances of NEEDLE replaced by REPLACMENT.  NEEDLE must be a string of at least one character."
  (assert (plusp (length needle)) nil "string-replace called with zero-length search pattern!")
  (with-output-to-string (result)
    (loop
       with needle-length = (length needle)
       for left = 0 then (+ right needle-length)
       as right = (search needle haystack :start2 left)
       while right
       do (write-string haystack result :start left :end right)
       (write-string replacement result)
       finally (write-string haystack result :start left))))

(defun string-replace-func (needle haystack func)
  "Returns a copy of HAYSTACK with all instances of NEEDLE replaced by the return value of FUNC.  NEEDLE must be a string of at least one character."
  (assert (plusp (length needle)) nil "string-replace-func called with zero-length search pattern!")
  (with-output-to-string (result)
    (loop
       with needle-length = (length needle)
       for left = 0 then (+ right needle-length)
       as right = (search needle haystack :start2 left)
       while right
       do (write-string haystack result :start left :end right)
       (write-string (funcall func needle) result)
       finally (write-string haystack result :start left))))

(defun hash-table-keys (hash)
  "Returns all the keys of the hash table HASH"
  (with-hash-table-iterator (next-entry hash)
	(loop for (more key value) = (multiple-value-list (next-entry))
		 while more
		 collect key)))

(eval-when (:load-toplevel :execute)
  (defparameter *tempus-root-pathname*
    (asdf:component-pathname (asdf:find-system "tempus"))))

(defun tempus-path (path)
  "Returns the local pathname merged with the root tempus path."
  (merge-pathnames path *tempus-root-pathname*))

(defun send-page (cxn)
  "Sends a single buffered page to CXN.  If any of the page is left, displays the more prompt."
  (let* ((buf (cxn-page-buf cxn))
		 (buf-len (length buf)))
	(loop
	 for count = 1 then (1+ count)
	 for line-begin = 0 then (1+ line-end)
	 for line-end = (and (< line-begin buf-len)
						 (position #\newline buf :start line-begin))
	 while (and line-end (< count 22))
	 finally
	 (cond
	  (line-end
	   (cxn-write cxn "~a~a~%" (subseq buf 0 (1+ line-end))
				  (colorize cxn
				   "&r**** &nUse the 'more' command to continue. &r****&n"))
	   (setf (cxn-page-buf cxn) (subseq buf (1+ line-end))))
	  (t
	   (cxn-write cxn "~a" buf)
	   (setf (cxn-page-buf cxn) ""))))))

(defun columnar-list-to-cxn (cxn cols width list)
  (let ((col 0))
	(dolist (element list)
	  (cxn-write cxn "~VA~[~%~]" width element
                 (rem (incf col) cols)))
    (unless (zerop (rem col cols))
      (cxn-write cxn "~%"))))

(defun safe-parse-integer (str)
  (ignore-errors (parse-integer str)))

(defun parse-integer-range (str &key (minimum nil) (maximum nil))
  (let* ((divider-pos (position #\- str))
		 (min-str (and divider-pos (subseq str 0 divider-pos)))
		 (max-str (and divider-pos (subseq str (1+ divider-pos)))))
	(cond
	  ((null divider-pos)
	   ;; no divider - just a single number
	   (let ((num (safe-parse-integer str)))
		 (values
		  (if minimum (max minimum num) num)
		  (if maximum (min maximum num) num))))
	  ((= (length str) 1)
	   ;; only a divider - no range
		(values nil nil))
	  (t
	   ;; either min-max -max or min-
		(values
		 (if (= divider-pos 0)
			 minimum
			 (max minimum (safe-parse-integer min-str)))
		 (if (= divider-pos (length str))
			 maximum
			 (min maximum (safe-parse-integer max-str))))))))

(defun circle-follow (ch victim)
  "Returns T if CH following VICTIM would make a follow loop."
  (loop
     for k = victim then (master-of k)
     until (or (null k)
               (eql k ch))
     finally (return (eql k ch))))

(defun add-follower (ch leader)
  (assert (null (master-of ch)) nil "Master of creature is non-NULL in add-follower")
  (setf (master-of ch) leader)
  (push ch (followers-of leader))

  (act ch :target leader
       :subject-emit "You now follow $N."
       :target-emit "$n starts following you."
       :not-target-emit "$n starts to follow $N."))

(defun describe-char (viewer subject)
  (cond
    ((can-see-creature viewer subject)
     (get-disguised-name subject viewer))
    ((immortalp subject)
     "a divine presence")
    (t
     "someone")))

(defun a-or-an (noun)
  (if (find (char noun 0) "aeiou")
      "an"
      "a"))

(defun he-or-she (ch)
  (case (sex-of ch)
    (male "he")
    (female "she")
    (t "it")))

(defun him-or-her (ch)
  (case (sex-of ch)
    (male "him")
    (female "her")
    (t "it")))

(defun his-or-her (ch)
  (case (sex-of ch)
    (male "his")
    (female "her")
    (t "its")))

(defun is-are (str)
  (if (eql (char str (1- (length str))) #\s) "are" "is"))

(defun rl-sec (sec)
  (* sec 10))

(defun strength-apply-index (ch)
  (cond
    ((not (<= 0 (str-of ch) 25))
     11)
    ((/= (str-of ch) 18)
     (str-of ch))
    ((= (str-add-of ch) 99)
     35)
    ((= (str-add-of ch) 100)
     36)
    (t
     (+ (floor (str-add-of ch) 10) 25))))