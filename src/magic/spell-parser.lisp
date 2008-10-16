(in-package :tempus)

(defclass spell-info ()
  ((name :accessor name-of :initarg :name :initform "!UNUSED!")
   (min-position :accessor min-position-of :initarg :min-position :initform 0)
   (mana-min :accessor mana-min-of :initarg :mana-min :initform 0)
   (mana-max :accessor mana-max-of :initarg :mana-max :initform 0)
   (mana-change :accessor mana-change-of :initarg :mana-change :initform 0)
   (min-level :accessor min-level-of :initarg :min-level
              :initform (make-array +num-classes+ :initial-element 73))
   (min-gen :accessor min-gen-of :initarg :min-gen
              :initform (make-array +num-classes+ :initial-element 0))
   (routines :accessor routines-of :initarg :routines :initform 0)
   (violentp :accessor violentp :initarg :violentp :initform nil)
   (targets :accessor targets-of :initarg :targets :initform 0)
   (song-kind :accessor song-kind-of :initarg :song-kind :initform nil)
   (lyrics :accessor lyrics-of :initarg :lyrics :initform nil)
   (instrumentalp :accessor instrumentalp :initarg :instrumentalp :initform nil)))

(defun clear-spells ()
  (setf *spell-info* (make-array 1000))
  (dotimes (idx 1000)
    (setf (aref *spell-info* idx) (make-instance 'spell-info)))
  (setf (name-of (aref *spell-info* 0)) "!RESERVED!"))

(defun apply-attribute-to-spell (spell child)
  (string-case (first child)
    ("granted"
     (let ((char-class (parse-pc-char-class (xml-attr child "class")))
           (level (xml-attr child "level" :numeric t))
           (gen (xml-attr child "gen" :numeric t :default 0)))
       (assert char-class nil
               "Granted class '~a' is not a valid class in spell!"
               (xml-attr child "class"))
       (assert (<= 1 level +lvl-ambassador+) nil
               "Granted level ~d is not a valid level" level)
       (assert (<= 0 gen 10) nil
               "Granted gen ~d is not a valid gen" level)
       (setf (aref (min-level-of spell) char-class) level)
       (setf (aref (min-gen-of spell) char-class) gen)))
    ("manacost"
     (setf (mana-max-of spell) (xml-attr child "initial" :numeric t))
     (setf (mana-change-of spell) (xml-attr child "level_dec" :numeric t))
     (setf (mana-min-of spell) (xml-attr child "minimum" :numeric t)))
    ("position"
     (let ((min (xml-attr child "minimum")))
       (assert min nil "Required property minimum missing from position element.")
       (let ((pos (position min +position-types+ :test #'string-equal)))
         (assert pos nil "Invalid minimum position '~a' for spell" min)
         (setf (min-position-of spell) pos))))
    ("target"
     (let ((type-str (xml-attr child "type"))
           (scope-str (xml-attr child "scope")))
       (setf (targets-of spell)
             (logior (targets-of spell)
                     (string-case type-str
                       ("door" +tar-door+)
                       ("direction" +tar-dir+)
                       ("self"
                        (string-case scope-str
                          ("fighting" +tar-fight-self+)
                          ("only" +tar-self-only+)
                          ("never" +tar-not-self+)
                          (t 0)))
                       ("creature"
                        (string-case scope-str
                          ("room" +tar-char-room+)
                          ("world" +tar-char-world+)
                          ("fighting" +tar-fight-vict+)
                          (t 0)))
                       ("object"
                        (string-case scope-str
                          ("room" +tar-obj-room+)
                          ("world" +tar-obj-world+)
                          ("inventory" +tar-obj-inv+)
                          ("equip" +tar-obj-equip+)
                          (t 0)))
                       (t 0))))))
    ("flag"
     (let ((value-str (xml-attr child "value")))
       (string-case value-str
         ("violent"
          (setf (violentp spell) t))
         ("unpleasant"
          (setf (targets-of spell) (logior (targets-of spell)
                                           +tar-unpleasant+)))
         (t
          (let ((flag (position value-str +spell-bit-keywords+ :test #'string-equal)))
            (assert flag nil "Invalid flag '~a' in spell" value-str)
            (setf (routines-of spell) (logior (routines-of spell)
                                              (ash 1 flag))))))))
    ("instrument"
     (string-case (xml-attr child "type")
       ("wind"
        (setf (song-kind-of spell) 'wind))
       ("percussion"
        (setf (song-kind-of spell) 'percussion))
       ("string"
        (setf (song-kind-of spell) 'string))
       (t
        (error "Invalid instrument type ~s in spell" (xml-attr child "type")))))
    ("description"
     (setf (lyrics-of spell) (third child))
     (setf (instrumentalp spell) t))
    ("lyrics"
     (setf (lyrics-of spell) (third child))
     (setf (instrumentalp spell) nil))))

(defun load-spell (node)
  (let* ((idnum (xml-attr node "id" :numeric t))
         (spell (aref *spell-info* idnum)))
    ;; for defined classes, initialize minimum level to ambassador
    (dotimes (idx +num-classes+)
      (setf (aref (min-level-of spell) idx) +lvl-ambassador+))

    (setf (name-of spell) (xml-attr node "name"))
    (dolist (child (cddr node))
      (apply-attribute-to-spell spell child))
    (when (zerop (targets-of spell))
      (setf (targets-of spell) +tar-ignore+))))

(defun boot-spells ()
  (clear-spells)
  (let ((xml (with-open-file (inf (tempus-path "lib/etc/spells.xml"))
               (xmls:parse inf))))
    (assert xml nil "Empty spells.xml file")
    (dolist (node (cddr xml))
      (when (or (string= (first node) "spell")
                (string= (first node) "skill"))
        (load-spell node)))))

(defun call-magic (ch vict ovict dvict spellnum level casttype)
  nil)

(defun mag-objectmagic (ch object arg)
  nil)