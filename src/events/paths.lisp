(in-package :tempus)

(defvar *paths* (make-hash-table))

(defconstant +path-locked+ (ash 1 0))
(defconstant +path-reversible+ (ash 1 1))
(defconstant +path-save+ (ash 1 2))

(defclass path ()
  ((number :accessor number-of :initarg :number)
   (name :accessor name-of :initarg :name)
   (owner :accessor owner-of :initarg :owner)
   (wait-time :accessor wait-time-of :initarg :wait-time)
   (flags :accessor flags-of :initarg :flags :initform 0)
   (path :accessor path-of :initarg :path :initform nil)))

(defun pread-string (inf)
  "Reads a tilde-delimited file like fread-string, but ignores lines starting with a # comment delimiter"
  (with-output-to-string (s)
    (loop
       for line = (read-line inf nil nil) ; read line by line
       as tilde-pos = (and line
                           (not (char= #\# (char line 0)))
                           (position #\~ line)) ; find tildes
       until (or (null line) tilde-pos) ; finish when tilde found
       unless (char= #\# (char line 0)) ; ignore comments
       do (write-string line s)
         (write-char #\space s)
       finally (when line
                 (write-string line s :end tilde-pos)))))

(defun parse-path-commands (path-name str)
  (let ((commands (cl-ppcre:all-matches-as-strings
                   (cl-ppcre:create-scanner
                    "[0-9]+\s*|x\s*|d[neswudfp]\s*|w[0-9]+\s*|c\"[^\"]+\"\s*|e\"[^\"]+\"\s*|r\s*"
                    :case-insensitive-mode t)
                   str :sharedp t))
        (result nil)
        (reversible nil))
    (loop for raw-command in commands
       as command = (string-trim " " raw-command)
       do
         (cond
           ((every #'digit-char-p command)
            (push (list :room (parse-integer command)) result))
           ((char-equal #\x (char command 0))
            (push '(:exit) result))
           ((char-equal #\d (char command 0))
            (push (list :dir (char command 1)) result))
           ((char-equal #\w (char command 0))
            (push (list :wait (parse-integer command :start 1)) result))
           ((char-equal #\e (char command 0))
            (push (list :echo (subseq command 2 (1- (length command)))) result))
           ((char-equal #\c (char command 0))
            (push (list :cmd (subseq command 2 (1- (length command)))) result))
           ((char-equal #\r (char command 0))
            (setf reversible t))
           (t
            (error "Invalid command '~a' detected in path ~a" command path-name))))
    (values (nreverse result) reversible)))

(defun load-paths ()
  (clrhash *paths*)
  (with-open-file (inf (tempus-path "lib/etc/paths"))
    (loop
       for line = (pread-string inf)
       while (string/= "" line)
       as match = (scan #/^(\d+) (\S+) (\d+) (\d+) (\d+) (.*)/ line)
       do
         (assert match nil "Bad line detected in paths: ~a" line)
         (multiple-value-bind (path reversiblep)
             (parse-path-commands (regref match 2) (regref match 6))
         (let ((new-path (make-instance 'path
                                        :number (parse-integer (regref match 1))
                                        :name (regref match 2)
                                        :owner (parse-integer (regref match 3))
                                        :wait-time (parse-integer (regref match 4))
                                        :path path
                                        :flags (if reversiblep +path-reversible+ 0))))
           (setf (gethash (number-of new-path) *paths*) new-path))))))

(defun real-path-by-num (vnum)
  (gethash vnum *paths*))

(defun real-path-by-name (name)
  (find name (hash-values *paths*) :test 'string-equal :key 'name-of))

(defun path-name-by-vnum (vnum)
  (let ((path (real-path-by-num vnum)))
    (when path (name-of path))))

(defun path-vnum-by-name (name)
  (let ((path (real-path-by-name name)))
    (when path (number-of path))))