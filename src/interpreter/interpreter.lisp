(in-package :tempus)

(defun sort-commands ()
  nil)

(defun expand-aliases (ch arg)
  arg)

#|
(defcommand ("look")
  (describe-contents ch (room-of ch)))

(defcommand ("look" "at")
  (send-to-char ch "What do you want to look at?"))

(defcommand ("examine" dirobj)
  (let ((obj (resolve-alias dirobj
                            '(:creature :object)
                            '(:equipped :inventory :room))))
    (send-to-char ch (description-of obj))))

(defalias ("look" "at" dirobj) "examine" dirobj)

(defcommand ("look" "in")
  (send-to-char ch "What do you want to look in?"))

(defcommand ("look" "in" dirobj)
  (let ((obj (resolve-alias dirobj
                            '(:creature :object)
                            '(:equipped :inventory :room))))
    (describe-contents ch obj)))

(defun interpret-command (ch line)
  (let ((line (string-left-trim '(#\space #\\) line)))
    (unless (string= line "")
      (let* ((args (cl-ppcre:split #/\s+/ line))
             (command (match-command args)))
|#
        
(defun interpret-command (ch arg)
  (let* ((space-pos (position #\space arg))
         (command-str (if space-pos (subseq arg 0 space-pos) arg))
         (args (if space-pos
                   (subseq arg (position #\space arg :test-not #'eql :start space-pos))
                   "")))
    (string-case command-str
      ("north"
       (perform-move ch 0 nil t))
      ("east"
       (perform-move ch 1 nil t))
      ("south"
       (perform-move ch 2 nil t))
      ("west"
       (perform-move ch 3 nil t))
      ("up"
       (perform-move ch 4 nil t))
      ("down"
       (perform-move ch 5 nil t))
      ("future"
       (perform-move ch 6 nil t))
      ("past"
       (perform-move ch 7 nil t))
      ("goto"
       (perform-goto ch (parse-integer args)))
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
       (send-to-char ch "You typed: ~a~%" arg)))))