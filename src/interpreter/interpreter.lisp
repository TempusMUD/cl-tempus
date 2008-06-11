(in-package :tempus)

(defun sort-commands ()
  nil)

(defun expand-aliases (ch arg)
  arg)

(defun interpret-command (ch arg)
  (string-case arg
    ("quit"
     (setf (state-of (link-of ch)) 'main-menu))
    ("shutdown"
     (setf *shutdown* t))
    (t
     (send-to-char ch "You typed: ~a~%" arg))))