(in-package #:tempus)

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
             (princ (message-of err) stream))))

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