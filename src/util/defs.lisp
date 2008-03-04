(in-package :tempus)

(defparameter +max-messages+ 200)
(defparameter +max-char-desc+ 1023)
(defparameter +max-tongues+ 50)
(defparameter +max-skills+ 700)
(defparameter +max-affect+ 96)
(defparameter +max-obj-affect+ 16)

(defparameter +level-ambassador+ 50)

(defparameter +spec-file-mob+ "etc/spec_ass_mob")
(defparameter +spec-file-obj+ "etc/spec_ass_obj")
(defparameter +spec-file-rm+ "etc/spec_ass_room")
(defparameter +timewarp-file+ "etc/timewarps")

(defparameter +spec-mob+ 1)
(defparameter +spec-obj+ 2)
(defparameter +spec-rm+ 4)
(defparameter +spec-res+ 8)

(defparameter *regex-macro-character* #\/)

;; mud-life time
(defparameter +secs-per-mud-hour+ 60)
(defparameter +secs-per-mud-day+ (* 24 +secs-per-mud-hour+))
(defparameter +secs-per-mud-month+ (* 35 +secs-per-mud-day+))
(defparameter +secs-per-mud-year+ (* 16 +secs-per-mud-month+))

(defmacro enable-regex-reader-syntax ()
  `(eval-when (:compile-toplevel :load-toplevel :execute)
    (%enable-regex-reader-syntax)))

(defun read-regex-string (stream)
  (let ((eof (gensym "EOF")))
    (with-output-to-string (str)
      (loop for prev-char = nil then cur-char
            for cur-char = (read-char stream nil eof)
            until (or (eql cur-char eof)
                      (and (not (eql prev-char #\\))
                           (eql cur-char *regex-macro-character*)))
            do (write-char cur-char str)

            finally (when (eql cur-char eof)
                      (error "Unexpected end-of-file while reading regex"))))))

(defun read-regex-options (stream)
  (let ((eof (gensym "EOF")))
    (loop for char = (read-char stream nil eof)
          until (or (eql char eof)
                    (not (alphanumericp char)))
          collect char)))

(defun read-regex (stream char arg)
  (declare (ignore char arg))
  (let ((pattern (read-regex-string stream))
        (options (read-regex-options stream)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
      (cl-ppcre:create-scanner ,pattern
       :case-insensitive-mode ,(member #\i options)
       :multi-line-mode ,(member #\m options)
       :single-line-mode ,(member #\s options)
       :extended-mode ,(member #\x options)))))

(defun %enable-regex-reader-syntax ()
  (set-dispatch-macro-character #\# *regex-macro-character* #'read-regex)
  (values))

(defmacro string-case (key &body str-prog-pairs)
  "Acts like case, except all its comparisons are done in strings."
  (let ((key-sym (gensym)))
    `(let ((,key-sym ,key))
      (cond
        ,@(mapcar (lambda (pair)
                    (cond
                      ((eql (first pair) t)
                       `(t ,(second pair)))
                      ((listp (first pair))
                       `((or ,@(mapcar (lambda (str) `(string-equal ,key-sym ,str)) (first pair)))
                         ,@(rest pair)))
                      (t
                       `((string-equal ,key-sym ,(first pair))
                         ,@(rest pair)))))
                  str-prog-pairs)))))
