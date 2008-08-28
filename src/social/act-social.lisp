(in-package :tempus)

(defstruct social-message
  cmd
  hide
  min-pos
  char-no-arg
  others-no-arg
  char-found
  others-found
  vict-found
  not-found
  char-auto
  others-auto)

(defparameter *socials* (make-hash-table :test 'equal))

(defun fread-action (inf)
  (let ((str (read-line inf)))
    (when (string/= str "#")
      str)))

(defun read-nonblank-line (inf)
  (loop
     for line = (read-line inf)
     while (string= line "")
     finally (return line)))

(defun read-social (inf)
  (cl-ppcre:register-groups-bind (cmd hide-str min-pos-str)
      (#/(\S+)(?:\s+([01])\s+(\d+))?/ (read-nonblank-line inf))
    (cond
      ((string= cmd "$")
       nil)
      (t
       (let ((hide (when hide-str (parse-integer hide-str)))
             (min-pos (when min-pos-str (parse-integer min-pos-str)))
             (char-no-arg (fread-action inf))
             (others-no-arg (fread-action inf))
             (char-found (fread-action inf)))
         (if char-found
             (let ((others-found (fread-action inf))
                   (vict-found (fread-action inf))
                   (not-found (fread-action inf))
                   (char-auto (fread-action inf)))
               (if char-auto
                   (let ((others-auto (fread-action inf)))
                     (make-social-message
                      :cmd cmd
                      :hide hide
                      :min-pos min-pos
                      :char-no-arg char-no-arg
                      :others-no-arg others-no-arg
                      :char-found char-found
                      :vict-found vict-found
                      :others-found others-found
                      :not-found not-found
                      :char-auto char-auto
                      :others-auto others-auto))

                   (make-social-message
                    :cmd cmd
                    :hide hide
                    :min-pos min-pos
                    :char-no-arg char-no-arg
                    :others-no-arg others-no-arg
                    :char-found char-found
                    :vict-found vict-found
                    :others-found others-found
                    :not-found not-found)))
             (make-social-message
              :cmd cmd
              :hide hide
              :min-pos min-pos
              :char-no-arg char-no-arg
              :others-no-arg others-no-arg)))))))

(defun perform-social (ch social-str target-str)
  (let ((target (resolve-alias ch target-str))
        (social (gethash social-str *socials*)))
    (cond
      ((null target-str)
       (act ch
            :subject-emit (social-message-char-no-arg social)
            :place-emit (social-message-others-no-arg social)))
      ((null target)
       (act ch
            :subject-emit (social-message-not-found social)))
      ((eql target ch)
       (act ch
            :target target
            :subject-emit (social-message-char-auto social)
            :place-emit (social-message-others-auto social)))
      ((null (social-message-char-found social))
       (act ch
            :subject-emit (social-message-char-no-arg social)
            :place-emit (social-message-others-no-arg social)))
      (t
       (act ch
            :target target
            :subject-emit (social-message-char-found social)
            :target-emit (social-message-vict-found social)
            :not-target-emit (social-message-others-found social))))))

(defun boot-social-messages (path)
  (with-open-file (fl path)
    (loop
       for new-social = (read-social fl)
       while new-social do
       (setf (gethash (social-message-cmd new-social) *socials*)
             new-social)
       ;; Add command for no argument
       (add-command (list (social-message-cmd new-social))
                    '(:resting :social)
                    (compile nil
                             `(lambda (ch)
                                (perform-social ch
                                                ,(social-message-cmd new-social)
                                                nil))))

       ;; Add command for argument
       (add-command (list (social-message-cmd new-social) 'target)
                    '(:resting :social)
                    (compile nil
                             `(lambda (ch target-str)
                                (perform-social ch
                                                ,(social-message-cmd new-social)
                                                target-str))))))
  (sort-commands))
