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
