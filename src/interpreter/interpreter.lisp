(in-package :tempus)

(defun sort-commands ()
  nil)

(defun expand-aliases (ch arg)
  arg)

(defun interpret-command (ch arg)
  (string-case arg
    ("quit"
     (setf (state-of (link-of ch)) 'main-menu))
    ("look"
     (look-at-room ch (in-room-of ch) t))
    ("roomflags"
     (setf (bit (prefs-of ch) +pref-roomflags+)
           (if (zerop (bit (prefs-of ch) +pref-roomflags+)) 1 0)))
    ("shutdown"
     (setf *shutdown* t))
    (t
     (send-to-char ch "You typed: ~a~%" arg))))