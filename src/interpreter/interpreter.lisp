(in-package :tempus)

(defun sort-commands ()
  nil)

(defun expand-aliases (ch arg)
  arg)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct command-info
    arity
    pattern
    flags
    function)

  (defvar *commands* nil)

  (defun command-sort-compare (a b)
    (cond
      ((not (eql (first (member :direction (command-info-flags a)))
                 (first (member :direction (command-info-flags b)))))
       (member :direction (command-info-flags a)))
      ((not (eql (first (member :mood (command-info-flags a)))
                 (first (member :mood (command-info-flags b)))))
       (member :mood (command-info-flags b)))
      ((/= (command-info-arity a) (command-info-arity b))
       (> (command-info-arity a) (command-info-arity b)))
      (t
       (string< (first (command-info-pattern a))
                (first (command-info-pattern b))))))

  (defmethod print-object ((cmd command-info) stream)
    (format stream "#<CMD-INFO ~s~{ ~a~}>"
            (command-info-pattern cmd)
            (command-info-flags cmd)))
  (define-condition parser-error ()
    ((message :reader message-of :initarg :message)))
  (defmethod print-object ((err parser-error) stream)
    (princ (message-of err) stream)))

(defun get-command (&rest pattern)
  (find pattern *commands* :test #'equal :key #'command-info-pattern))

(defmacro defcommand ((actor &rest pattern) flags &body body)
  (let* ((cmd (gensym "CMD"))
         (err (gensym "ERR"))
         (func `(lambda (,actor ,@(remove-if-not 'symbolp pattern))
                  (handler-case
                      (progn ,@body)
                    (parser-error (,err)
                      (send-to-char ,actor "~a~%" ,err))))))
    `(let ((,cmd (find ',pattern *commands* :test 'equal :key 'command-info-pattern)))
         (cond
           (,cmd
            (setf (command-info-pattern ,cmd) ',pattern)
            (setf (command-info-arity ,cmd) ,(length pattern))
            (setf (command-info-flags ,cmd) ',flags)
            (setf (command-info-function ,cmd) ,func))
           (t
            (push
             (make-command-info :arity ,(length pattern)
                                :pattern ',pattern
                                :flags ',flags
                                :function ,func)
              *commands*)
            (setf *commands* (sort *commands* 'command-sort-compare)))))))

(defun command-matches (cmd string)
  (loop
     with vars = nil
     with tokens = (command-info-pattern cmd)
     while tokens
     for token = (car tokens)
     do (cond
          ((symbolp token)
           ;; wildcard matching
           (setf tokens (rest tokens))
           (cond
             ((string= string "")
              ;; wildcards don't match the empty string
              (return-from command-matches nil))
             ((null tokens)
              (push (string-trim '(#\space) string) vars))
             ((symbolp (first tokens))
              (let ((space-pos (position #\space string)))
                (unless space-pos
                  (return-from command-matches nil))
                (push (subseq string 0 space-pos) vars)
                (setf string (subseq string (1+ space-pos)))))
             ((stringp (first tokens))
              (let ((match-pos (search (first tokens) string)))
                (unless match-pos
                  (return-from command-matches nil))
                (push (string-trim '(#\space)
                                   (subseq string 0 match-pos)) vars)
                (setf string (subseq string (+ match-pos (length (first tokens)))))))))
          ((characterp token)
           (unless (eql token (char string 0))
             (return-from command-matches nil))
           (setf string (string-left-trim '(#\space) (subseq string 1)))
           (setf tokens (rest tokens)))
          ((rest tokens)
           ;; string matching
           (let* ((space-pos (position #\space string))
                  (word (if space-pos (subseq string 0 space-pos) string)))
             (unless (string-abbrev word token)
               (return-from command-matches nil))
             (if space-pos
                 (setf string (string-left-trim '(#\space)
                                                (subseq string (1+ space-pos))))
                 (setf string ""))
             (setf tokens (rest tokens))))
          (t
           ;; end of string
           (unless (string-abbrev string token)
             (return-from command-matches nil))
           (setf tokens nil)))
     finally (return (list t (nreverse vars)))))

(defun find-command (arg)
  (loop for command in *commands*
       as (match vars) = (command-matches command arg)
       until match
       finally (return (when match (values command vars)))))

(defun interpret-command (ch arg)
  (multiple-value-bind (command vars)
      (find-command arg)
    (cond
      ((null command)
        (send-to-char ch "I didn't get that.~%"))
      ((and (is-npc ch) (member :player (command-info-flags command)))
       (send-to-char ch "Sorry, players ONLY!~%"))
      (t
       (apply (command-info-function command) ch vars)))))