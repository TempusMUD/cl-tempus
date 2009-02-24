(in-package :tempus)

(defun sort-spells ()
  nil)

(defun sort-skills ()
  nil)

(defmacro define-special (name (&rest args) (&rest flags) &body body)
  (let ((func-sym (intern (format nil "SPECIAL-~a" name)
                          (find-package :tempus)))
        (name-str (string-downcase name)))
    `(progn
       (defun ,func-sym ,args ,@body)
       (setf (gethash ,name-str *special-funcs*) ',func-sym)
       (setf (gethash ,name-str *special-flags*) (logior ,@flags)))))

(define-special postmaster (trigger self ch command vars) (+spec-mob+)
  (when (and (eql trigger 'command)
             (not (is-npc ch)))
    (string-case (first (command-info-pattern command))
      ("mail"
       (let* ((names (split-sequence #\space (first vars)
                                     :remove-empty-subseqs t))
              (idnums (mapcar 'retrieve-player-idnum names)))
         (cond
           ((null names)
            (send-to-char ch "You must specify some people to mail!~%"))
           ((notevery #'identity idnums)
            (dolist (bad-name (mapcan (lambda (x y)
                                        (unless y (list x)))
                                      names idnums))
              (perform-tell self ch
                            (format nil "No one by the name ~a is registered here!"
                                    bad-name))))
           (t
            (start-mail-editor (link-of ch) (delete-duplicates idnums)))))
       t)
      ("receive"
       (let* ((mail-path (mail-pathname (idnum-of ch)))
              (letters (ignore-errors
                         (when (probe-file mail-path)
                           (with-open-file (inf mail-path)
                             (cddr (xmls:parse inf)))))))
         (cond
           ((null letters)
            (perform-tell self ch "Sorry, you don't have any mail waiting."))
           (t
            (dolist (letter letters)
              (unserialize-object nil ch nil letter))
            (save-player-objects ch)
            (delete-file mail-path)
            (act self :target ch
                 :target-emit (format nil "$n gives you ~d piece~:p of mail"
                                      (length letters))
                 :not-target-emit "$n gives some mail to $N."))))
       t))))
