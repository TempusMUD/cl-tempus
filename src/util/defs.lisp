(in-package #:tempus)

(defparameter +max-messages+ 200)
(defparameter +max-char-desc+ 1023)
(defparameter +max-tongues+ 50)
(defparameter +max-skills+ 700)
(defparameter +max-affect+ 96)
(defparameter +max-obj-affect+ 16)
(defparameter +max-badge-length+ 7)

(defparameter +level-ambassador+ 50)

(defparameter +spec-file-mob+ "lib/etc/spec_ass_mob")
(defparameter +spec-file-obj+ "lib/etc/spec_ass_obj")
(defparameter +spec-file-rm+ "lib/etc/spec_ass_room")
(defparameter +timewarp-file+ "lib/etc/timewarps")

(defparameter +spec-mob+ 1)
(defparameter +spec-obj+ 2)
(defparameter +spec-rm+ 4)
(defparameter +spec-res+ 8)

;; mud-life time
(defparameter +secs-per-mud-hour+ 60)
(defparameter +secs-per-mud-day+ (* 24 +secs-per-mud-hour+))
(defparameter +secs-per-mud-month+ (* 35 +secs-per-mud-day+))
(defparameter +secs-per-mud-year+ (* 16 +secs-per-mud-month+))

(defvar *mini-mud* nil)

(defvar *help* nil)
(defvar *restrict* nil)
(defvar *olc-lock* nil)
(defvar *no-rent-check* nil)
(defvar *no-initial-zreset* nil)
(defvar *log-cmds* nil)
(defvar *max-players* nil)
(defvar *max-descriptors-available* nil)
(defvar *cur-car* nil)
(defvar *default-quad-zone* nil)
(defvar *object-list* nil)
(defvar *production-mode* nil)
(defvar *nameserver-is-slow* nil)
(defvar *auto-save* nil)
(defvar *autosave-time* nil)
(defparameter *default-port* 4040)
(defparameter *default-dir* "lib/")

(eval-when (:load-toplevel :compile-toplevel)
  (defparameter *regex-macro-character* #\/)

  (defun %read-regex-string (stream)
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

  (defun %read-regex-options (stream)
    (let ((eof (gensym "EOF")))
      (loop for char = (read-char stream nil eof)
         until (or (eql char eof)
                   (not (alphanumericp char)))
         collect char)))

  (defun %read-regex (stream char arg)
    (declare (ignore char arg))
    (let ((pattern (%read-regex-string stream))
          (options (%read-regex-options stream)))
      `(load-time-value
         (cl-ppcre:create-scanner ,pattern
                                  :case-insensitive-mode ,(member #\i options)
                                  :multi-line-mode ,(member #\m options)
                                  :single-line-mode ,(member #\s options)
                                  :extended-mode ,(member #\x options)))))

  (defun %enable-regex-reader-syntax ()
    (set-dispatch-macro-character #\# *regex-macro-character* #'%read-regex)
    (values))

  (defmacro enable-regex-reader-syntax ()
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (%enable-regex-reader-syntax)))

  (enable-regex-reader-syntax))

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

(defmacro string-abbrev-case (key &body str-prog-pairs)
  "Acts like string-case, except abbreviations count."
    (let ((key-sym (gensym)))
        `(let ((,key-sym ,key))
            (cond
                ,@(mapcar (lambda (pair)
                    (cond
                        ((eql (first pair) t)
                            `(t ,(second pair)))
                        ((listp (first pair))
                            `((or ,@(mapcar (lambda (str) `(string-abbrev ,str ,key-sym)) (first pair)))
                                ,@(rest pair)))
                        (t
                            `((string-abbrev ,key-sym ,(first pair))
                                ,@(rest pair)))))
                    str-prog-pairs)))))

(defmacro with-pagination ((cxn) &body body)
  "Ensures that proper use of *cxn-pagination* takes place.  Within its
context, all output sent to the given cxn will be queued for pagination.
On exit, it sends the first page to the cxn."
  (let ((cxn-sym (gensym)))
    `(let ((,cxn-sym ,cxn))
       (when ,cxn-sym
         (unwind-protect
             (progn
               (setf (page-buf-of ,cxn-sym) "")
               (setf *cxn-paginate* ,cxn-sym)
               ,@body)
           (progn
             (setf *cxn-paginate* nil)
             (send-page ,cxn-sym)))))))

(defmacro with-words (str var-list &body body)
  "Binds each symbol in VAR-LIST to a string containing a word from STR.
Understands the &rest qualifier."
  (let ((my-str (gensym))
        (my-fields (gensym))
        (my-rest-pos (gensym))
        (my-var-list var-list)
        (my-rest-var nil)
        (my-rest-var-pos (position '&rest var-list)))

    (when my-rest-var-pos
      (setf my-rest-var (nth (1+ my-rest-var-pos) var-list))
      (setf my-var-list
            (loop for var in var-list
               unless (or (eql var '&rest) (eql var my-rest-var))
               collect var)))

    (if (and my-rest-var-pos (zerop my-rest-var-pos))
        ;; If we only have a rest variable, then all we wanted was a
        ;; trimmed string
        `(let ((,my-rest-var (string-trim '(#\space) ,str)))
           ,@body)
        `(let ((,my-str ,str))
           (multiple-value-bind (,my-fields ,my-rest-pos)
               (split-sequence #\space ,my-str :count ,(length my-var-list))
             ,@(unless my-rest-var
                 `((declare (ignorable ,my-rest-pos))))
             (let (,@(loop for my-field in my-var-list
                        for my-idx = 0 then (1+ my-idx)
                        collect `(,my-field (nth ,my-idx ,my-fields)))
                     ,@(when my-rest-var
                         `((,my-rest-var (subseq ,my-str ,my-rest-pos)))))
               ,@body))))))

(defun bitp (bitv idx)
  (plusp (bit bitv idx)))
(defsetf bitp (bitv idx) (val)
`(setf (bit ,bitv ,idx) (if ,val 1 0)))

(defun new-hash-table (&rest args)
  (let ((table (make-hash-table)))
    (loop for (key val) on args by #'cddr do
         (setf (gethash key table) val))
    table))