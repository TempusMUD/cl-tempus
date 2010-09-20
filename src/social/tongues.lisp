(in-package :tempus)

(defclass tongue ()
  ((idnum :accessor idnum-of :initarg :idnum)
   (name :accessor name-of :initarg :name)
   (syllables :accessor syllables-of :initform nil)
   (letters :accessor letters-of :initform (make-hash-table))
   (nospeak :accessor nospeak-of :initarg :nospeak)))

(define-condition invalid-tongues-file (error)
  ())

(defvar *tongues* (make-hash-table))

(defun boot-tongues ()
  (clrhash *tongues*)
  (let ((doc (with-open-file (inf (tempus-path "lib/etc/tongues.xml"))
               (xmls:parse inf :compress-whitespace nil))))
    (assert (string= "tongues" (first doc)) nil 'invalid-tongues-file)
    (dolist (node (cddr doc))
      (when (and (consp node)
                 (string= "tongue" (first node)))
        (let ((new-tongue (unserialize-tongue node)))
          (setf (gethash (idnum-of new-tongue) *tongues*) new-tongue)))))

  (slog "~d tongues loaded" (hash-table-count *tongues*)))

(defun unserialize-tongue (xml)
  (let ((tongue (make-instance 'tongue
                               :idnum (xml-attr xml "idnum" :numeric t)
                               :name (xml-attr xml "name"))))
    (dolist (node (cddr xml))
      (when (consp node)
        (string-case (first node)
          ("syllable"
           (push (list (xml-attr node "pattern") (xml-attr node "replacement"))
                 (syllables-of tongue)))
          ("letter"
           (let ((pattern (char (xml-attr node "pattern") 0))
                 (replace (char (xml-attr node "replacement") 0)))
             (setf (gethash (char-downcase pattern) (letters-of tongue)) replace)
             (setf (gethash (char-upcase pattern) (letters-of tongue)) replace)))
          ("nospeak"
           (setf (nospeak-of tongue) (caddr node))))))

    (setf (syllables-of tongue) (nreverse (syllables-of tongue)))

    tongue))

(defun tongue-name (idnum)
  (let ((tongue (gethash idnum *tongues*)))
    (if tongue
        (name-of tongue)
        (format nil "<ILLEGAL #~d>" idnum))))

(defun find-tongue-idx-by-name (name)
  (maphash (lambda (idnum tongue)
             (when (string-equal name (name-of tongue))
               (return-from find-tongue-idx-by-name idnum)))
           *tongues*))

(defun fluency-desc (ch tongue-id)
  (let ((fluency (check-tongue ch tongue-id)))
    (cond
      ((minusp fluency)   "(terrible)")
      ((zerop fluency)    "(not learned)")
      ((< fluency 10)     "(awful)")
      ((< fluency 20)     "(bad)")
      ((< fluency 40)     "(poor)")
      ((< fluency 55)     "(average)")
      ((< fluency 80)     "(good)")
      ((< fluency 90)     "(very good)")
      (t                  "(fluent)"))))

(defun translate-word (wordlist letter-map word)
  (let ((word-match (assoc word wordlist :test #'string-equal)))
    (if word-match
        (second word-match)
        (map 'string (lambda (c)
                       (gethash c letter-map c))
             word))))

(defun translate-with-tongue (tongue phrase amount)
  (when (or (string= "" phrase)
            (null (syllables-of tongue))
            (= amount 100))
    (return-from translate-with-tongue phrase))

  (format nil "~{~a~^ ~}"
          (loop
             for word in (cl-ppcre:split "\\s+" phrase)
             if (> (random-range 1 100) amount)
             collect (translate-word (syllables-of tongue)
                                     (letters-of tongue)
                                     word)
             else
             collect word)))
