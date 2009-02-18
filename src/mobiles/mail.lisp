(in-package #:tempus)

(defun mail-pathname (idnum)
  (make-pathname :name (princ-to-string idnum)
                 :type "mail"
                 :defaults
                 (merge-pathnames (format nil "lib/players/mail/~d/"
                                          (mod idnum 10))
                                  (asdf:component-pathname
                                   (asdf:find-system "tempus")))))

(defun has-mail (idnum)
  (probe-file (mail-pathname idnum)))