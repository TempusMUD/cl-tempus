(in-package #:tempus)

(defvar *spell-info* (make-array 1000))

(defun spell-gen (spell class)
  (aref (min-gen-of (aref *spell-info* spell)) class))

(defun spell-level (spell class)
  (aref (min-level-of (aref *spell-info* spell)) class))

(defun calc-able-to-learn (skill char-class remort-char-class level gen)
  (let ((info (aref *spell-info* skill)))
    (or (and (>= level (aref (min-level-of info) char-class))
             (>= gen (aref (min-gen-of info) char-class))))
        (and (plusp gen)
             (>= level (aref (min-level-of info) remort-char-class)))))

(defun able-to-learn (ch skill)
  (calc-able-to-learn skill
                      (char-class-of ch)
                      (remort-char-class-of ch)
                      (level-of ch)
                      (remort-gen-of ch)))

(defun load-corpse-owner (obj)
  (if (minusp (corpse-idnum obj))
      (real-mobile-proto (- (corpse-idnum obj)))
      (load-player-from-xml (corpse-idnum obj))))