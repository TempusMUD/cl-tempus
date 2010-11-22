(in-package :tempus)

(defparameter +special-cmd+ 0)          ; special command response
(defparameter +special-tick+ 1)         ; special periodic action
(defparameter +special-death+ 2)        ; special death notification
(defparameter +special-fight+ 3)        ; special fight starting
(defparameter +special-combat+ 4)       ; special in-combat ability
(defparameter +special-enter+ 5)        ; special upon entrance
(defparameter +special-leave+ 6)        ; special upon exit
(defparameter +special-reset+ 7)        ; zone reset

(defvar *special-funcs* (make-hash-table :test 'equal))
(defvar *special-flags* (make-hash-table :test 'equal))

(defun find-special-by-func (search-func)
  (maphash (lambda (name func)
             (when (eql search-func func)
               (return-from find-special-by-func name)))
           *special-funcs*)
  nil)

(defun assign-specials (kind flag config-path retrieval-func setter-func)
  (handler-case
      (with-open-file (inf config-path :direction :input)
        (loop for line = (get-line inf)
           while line
           as result = (scan #/^(\d+)\s+([^\s#]+)/ line)
           when result do
           (let* ((vnum (parse-integer (regref result 1)))
                  (thing (funcall retrieval-func vnum))
                  (func (gethash (regref result 2) *special-funcs*))
                  (flags (gethash (regref result 2) *special-flags*)))
             (when (and (null thing) (not *mini-mud*))
               (slog "Error in ~a spec file: ~a <~d> does not exist."
                     kind kind vnum))
             (cond
               ((null func)
                #+nil (slog "Error in ~a spec file: special <~a> does not exist."
                      kind (regref result 2)))
               ((not (logtest flags flag))
                (slog "Attempt to assign special <~a> to a ~a."
                      (regref result 2) kind))
               (t
                (funcall setter-func thing func))))))
    (error (err)
        (slog "~a" err))))

(defun assign-mobiles ()
  (assign-specials "mobile" +spec-mob+ +spec-file-mob+ #'real-mobile-proto
                   (lambda (mob func)
                     (setf (func-of (shared-of mob)) func))))
(defun assign-objects ()
  (assign-specials "object" +spec-obj+ +spec-file-obj+ #'real-object-proto
                   (lambda (obj func)
                     (setf (func-of (shared-of obj)) func))))
(defun assign-rooms ()
  (assign-specials "room" +spec-rm+ +spec-file-rm+ #'real-room
                   (lambda (room func)
                     (setf (func-of room) func))))