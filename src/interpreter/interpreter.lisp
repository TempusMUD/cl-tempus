(in-package :tempus)

(defun expand-aliases (ch arg)
  (declare (ignore ch))
  arg)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct command-info
    arity
    pattern
    flags
    function)

  (defvar *commands* nil)

  (defmethod print-object ((cmd command-info) stream)
    (format stream "#<CMD-INFO ~s~{ ~a~}>"
            (command-info-pattern cmd)
            (command-info-flags cmd)))

  (define-condition parser-error (simple-error)
    ((message :reader message-of :initarg :message))
    (:report (lambda (err stream)
               (princ (message-of err) stream)))))

;; "get" thing "from" container
;; "get" thing "from"
;; "get" thing container
;; "get" thing
;; "get"

(defun literal-token-p (token)
  (or (stringp token)
      (characterp token)))

(defun var-token-p (token)
  (not (literal-token-p token)))

(defun pattern-sort< (a b)
  (cond
    ((and (null a) (null b))
     ;; identical patterns
     nil)
    ((null a)
     ;; a is shorter, therefore should go later
     nil)
    ((null b)
     ;; a is longer, therefore should go first
     t)
    ((and (var-token-p (car a)) (literal-token-p (car b)))
     ;; a is a symbol and b isn't, so a should go later
     nil)
    ((and (literal-token-p (car a)) (var-token-p (car b)))
     ;; a isn't a symbol and b is, so a should go first
     t)
    ((and (var-token-p (car a)) (var-token-p (car b)))
     ;; both a and b are symbols, so check the next element
     (pattern-sort< (cdr a) (cdr b)))
    ((string= (car a) (car b))
     ;; a and b are the same strings, so check the next element
     (pattern-sort< (cdr a) (cdr b)))
    ;; a is an abbrevation of b, so a goes first
    ((string-abbrev (string (car a)) (string (car b)))
     t)
    ;; b is an abbreviation of a, so b goes first
    ((string-abbrev (string (car b)) (string (car a)))
     nil)
    (t
     ;; alphabetize in the absence of meaningful difference
     (string< (car a) (car b)))))

(defun command-sort-compare (a b)
  (cond
    ((not (eql (first (member :direction (command-info-flags a)))
               (first (member :direction (command-info-flags b)))))
     (member :direction (command-info-flags a)))
    ((not (eql (first (member :important (command-info-flags a)))
               (first (member :important (command-info-flags b)))))
     (member :important (command-info-flags a)))
    ((not (eql (first (member :config (command-info-flags a)))
               (first (member :config (command-info-flags b)))))
     (member :config (command-info-flags b)))
    ((not (eql (first (member :mood (command-info-flags a)))
               (first (member :mood (command-info-flags b)))))
     (member :mood (command-info-flags b)))
    ((not (eql (first (member :social (command-info-flags a)))
               (first (member :social (command-info-flags b)))))
     (member :social (command-info-flags b)))
    (t
     (pattern-sort< (command-info-pattern a) (command-info-pattern b)))))

(defun sort-commands ()
  (setf *commands* (sort *commands* 'command-sort-compare))
  (values))

(defun get-command (&rest pattern)
  (find pattern *commands* :test #'equal :key #'command-info-pattern))

(defun add-command (pattern flags func)
  "Adds a command to the parser.  PATTERN is the pattern for matching user input, FLAGS involves the restrictions on the command.  FUNC is the function to execute when the user input matches."
  (let ((cmd (find pattern *commands* :test #'equal :key 'command-info-pattern)))
    (cond
      (cmd
       (setf (command-info-pattern cmd) pattern)
       (setf (command-info-arity cmd) (length pattern))
       (setf (command-info-flags cmd) flags)
       (setf (command-info-function cmd) func))
      (t
       (push
        (make-command-info :pattern pattern
                           :arity (length pattern)
                           :flags flags
                           :function func)
        *commands*)))))

(defmacro defcommand ((actor &rest pattern) flags &body body)
  (let* ((body-docstr (when (stringp (first body))
                        (prog1
                            (list (first body))
                          (setf body (rest body)))))
         (body-declare (when (and (consp (first body))
                                  (eql (first (first body)) 'declare))
                         (let ((non-declare-pos (position-if-not (lambda (x)
                                                           (eql (first x) 'declare))
                                                         body)))
                           (prog1
                               (subseq body 0 non-declare-pos)
                             (setf body (nthcdr non-declare-pos body))))))
         (func-name (intern (format nil "DO-~@:(~{~a~^-~}~)" pattern)))
         (func `(defun ,func-name (,actor ,@(remove-if-not 'symbolp pattern))
                  ,@body-docstr
                  ,@body-declare
                  (check-type ,actor creature)
                      (block nil ,@body))))
    (assert (not (symbolp (first pattern))) nil
            "First token of pattern must not be a symbol.")
    `(progn
       ,func
       (add-command (quote ,pattern) (quote ,flags) ',func-name)
       ',func-name)))

(defparameter *parser-trace* t)
(defun trace-msg (fmt &rest args)
  (when *parser-trace*
    (slog "~?" fmt args)))

(defun pattern-match-wildcard (string tokens vars)
  ;; wildcard matching
  (trace-msg "Matching symbol ~a" (first tokens))
  (setf tokens (rest tokens))
  (cond
    ((string= string "")
     ;; wildcards don't match the empty string
     (trace-msg "No match - empty string")
     nil)
    ((null tokens)
     (trace-msg "Last token, rest of string is var")
     (values t nil nil (cons (string-trim '(#\space) string) vars)))
    ((symbolp (first tokens))
     (let ((space-pos (position #\space string)))
       (cond
         ((null space-pos)
          (trace-msg "No match - Next token is sym and no space found")
          nil)
         (t
          (trace-msg "Next tokens is sym - Pushing single word into var")
          (values t
                  (subseq string (1+ space-pos))
                  tokens
                  (cons (subseq string 0 space-pos) vars))))))
    ((stringp (first tokens))
     (let* ((space-pos (position #\space string))
            (match-pos (and space-pos (search (first tokens) string :start2 space-pos))))
       (cond
         ((null space-pos)
          (trace-msg "No match - Next token is string and no space found")
          nil)
         ((null match-pos)
          (trace-msg "No match - Next token is string and not found")
          nil)
         (t
          (trace-msg "Next token is str - Pushing subseq into var")
          (values t
                  (string-trim '(#\space) (subseq string (+ match-pos (length (first tokens)))))
                  (rest tokens) ; We skip the next token, since we've already matched it
                  (cons (string-trim '(#\space) (subseq string 0 match-pos)) vars))))))))

(defun pattern-match-char (string tokens)
  (trace-msg "Matching character ~a" (first tokens))
  (cond
    ((not (eql (first tokens) (char string 0)))
     (trace-msg "No match - single character didn't match")
     nil)
    (t
     (trace-msg "Single character matched")
     (values t
             (string-left-trim '(#\space) (subseq string 1))
             (rest tokens)))))

(defun pattern-match-string (string tokens)
  (trace-msg "Matching string ~a in middle" (first tokens))
  ;; string matching
  (cond
    ((null (rest tokens))
     ;; end of string
     (trace-msg "Matching string ~a at end" (first tokens))
     (when (string-abbrev string (first tokens))
       (values t nil nil)))
    (t
     (let* ((space-pos (position #\space string))
            (word (if space-pos (subseq string 0 space-pos) string)))
       (cond
         ((not (string-abbrev word (first tokens)))
          (trace-msg "No match - didn't match string")
          nil)
         ((null space-pos)
          (trace-msg "String matched at end")
          (values t
                  ""
                  (rest tokens)))
         (t
          (trace-msg "String matched in middle")
          (values t
                  (string-left-trim '(#\space) (subseq string (1+ space-pos)))
                  (rest tokens))))))))

(defun command-pattern-matches (pattern string)
  (loop
     with vars = nil
     with match-p = t
     with tokens = pattern
     while tokens
     for token = (car tokens)
     do
       (trace-msg "string=~s" string)
       (trace-msg "vars=~s" vars)
       (cond
         ((var-token-p token)
          (multiple-value-setq (match-p string tokens vars)
            (pattern-match-wildcard string tokens vars)))
         ((characterp token)
          (multiple-value-setq (match-p string tokens)
            (pattern-match-char string tokens)))
         (t
          (multiple-value-setq (match-p string tokens)
            (pattern-match-string string tokens))))
     finally (return (when match-p (list t (nreverse vars))))))

(defun can-do-command (ch command)
  (and (or (not (member :immortal (command-info-flags command)))
           (immortal-level-p ch))
       (let ((groups (gethash (first (command-info-pattern command))
                              *command-access-groups*)))
         (or (null groups)
             (some (lambda (group)
                     (security-is-member ch (name-of group)))
                   groups)))))

(defun command-matches (ch command arg)
  (let ((match (command-pattern-matches (command-info-pattern command) arg)))
    (when (and match (can-do-command ch command))
      match)))

(defun find-command (ch arg)
  (loop for command in *commands*
       as (match vars) = (command-matches ch command arg)
       until match
       finally (return (when match (values command vars)))))

(defun select-unknown-cmd-error ()
  (case (random 12)
    (1 "Beg pardon?")
    (2 "Come again?")
    (3 "Huh?!?")
    (4 "What's that?")
    (5 "Que?!?")
    (6 "You must enter a proper command!")
    (7 "I don't understand that.")
    (8 "Wie bitte?")
    (9 "You're talking nonsense to me.")
    (10 "I didn't get that.")
    (t "Hmm, I don't understand that command.")))

(defun interpret-command (ch arg)
  (multiple-value-bind (command vars)
      (find-command ch arg)
    (cond
      ((null command)
        (send-to-char ch "~a~%" (select-unknown-cmd-error)))
      ((and (is-npc ch) (member :player (command-info-flags command)))
       (send-to-char ch "Sorry, players ONLY!~%"))
      ((not (check-specials 'command ch command vars))
       (handler-case
           (apply (command-info-function command) ch vars)
         (parser-error (err)
           (send-to-char ch "~a~%" (message-of err))))))))

(defun check-specials (trigger ch command vars)
  (or
    ;; special in room?
    (and (func-of (in-room-of ch))
         (funcall (func-of (in-room-of ch)) trigger (in-room-of ch) ch command vars))
    ;; special in self?  (for activiting special abilities in switched mobs)
    (and (is-npc ch)
         (mob-flagged ch +mob-spec+)
         (func-of (shared-of ch))
         (funcall (func-of (shared-of ch)) trigger ch ch command vars))
    ;; special in equipment list?
    (some (lambda (obj)
            (and obj
                 (func-of (shared-of obj))
                 (funcall (func-of (shared-of obj)) trigger obj ch command vars)))
          (equipment-of ch))
    ;; special in inventory?
    (some (lambda (obj)
            (and obj
                 (func-of (shared-of obj))
                 (funcall (func-of (shared-of obj)) trigger obj ch command vars)))
          (carrying-of ch))
    ;; special in mobile?
    (some (lambda (tch)
            (and (is-npc tch)
                 (func-of (shared-of tch))
                 (funcall (func-of (shared-of tch)) trigger tch ch command vars)))
          (people-of (in-room-of ch)))
    ;; special in object?
    (some (lambda (obj)
            (and (func-of (shared-of obj))
                 (funcall (func-of (shared-of obj)) trigger obj ch command vars)))
          (contents-of (in-room-of ch)))))

