(in-package #:tempus)

(defparameter +oset-params+
  '(("action_desc" :type string :slot action-desc :desc "object action description"
     :nil-allowed t :update-objs t)
    ("cost" :type string :slot cost :desc "object cost")
    ("damage" :type number :slot damage :desc "object damage")
    ("extra1" :type bitflag :slot extra-flags :desc "extra1 object" :table +extra-names+)
    ("extra2" :type bitflag :slot extra2-flags :desc "extra2 object" :table +extra2-names+)
    ("extra3" :type bitflag :slot extra3-flags :desc "extra3 object" :table +extra3-names+)
    ("ldesc" :type string :slot line-desc :desc "object line description"
     :nil-allowed t :update-objs t)
    ("material" :type enumerated :slot material :desc "object material"
     :table +material-names+)
    ("maxdamage" :type number :slot max-dam :desc "object maxdamage")
    ("name" :type string :slot name :desc "object name" :update-objs t)
    ("owner" :type number :slot owner :desc "object owner" :shared t)
    ("rent" :type number :slot rent :desc "object rent" :shared t)
    ("specparam" :type text :slot specparam :desc "object specparam" :shared t)
    ("timer" :type number :slot timer :desc "object timer")
    ("type" :type enumerated :slot kind :desc "object type"
     :table +item-kinds+)
    ("weight" :type number :slot weight :desc "object weight")
    ("worn" :type bitflag :slot wear-flags :desc "worn object" :table +wear-bits-desc+)
    ("affection" :func perform-oset-affected)
    ("alias" :func perform-oset-alias)
    ("apply" :func perform-oset-apply)
    ("description" :func perform-oset-description)
    ("special" :func perform-oset-special :shared t)
    ("value" :func perform-oset-value)))

(defun update-objlist-full (vnum)
  (let ((proto (real-object-proto vnum)))
    (dolist (obj *object-list*)
      (when (= vnum (vnum-of obj))
        (unless (is-obj-stat2 obj +item2-renamed+)
          (setf (aliases-of obj) (aliases-of proto))
          (setf (name-of obj) (name-of proto)))
        (setf (line-desc-of obj) (line-desc-of proto))
        (setf (action-desc-of obj) (action-desc-of proto))
        (setf (ex-description-of obj) (ex-description-of proto))))))

(defun perform-create-object (ch vnum)
  (let* ((new-object-shared (make-instance 'obj-shared-data :vnum vnum))
         (new-object (make-instance 'obj-data
                                    :name "a fresh blank object"
                                    :aliases "fresh blank object"
                                    :line-desc "A fresh blank object is here."
                                    :shared new-object-shared)))
    (setf (proto-of new-object-shared) new-object)
    (setf (gethash vnum *object-prototypes*) new-object)
    (setf (olc-obj-of ch) new-object)
    (send-to-char ch "Object ~d successfully created.~%" vnum)
       (send-to-char ch "Now editing object [~d] &g~a&n.~%"
                     vnum (name-of new-object))))

(defun perform-destroy-object (ch vnum)
  (dolist (obj (copy-list *object-list*))
    (when (= (vnum-of obj) vnum)
      (extract-obj obj)))

  (remhash vnum *object-prototypes*)

  (dolist (tch *characters*)
    (when (and (typep tch 'player)
               (olc-obj-of tch)
               (eql (vnum-of (olc-obj-of tch)) vnum))
      (setf (olc-obj-of tch) nil)
      (send-to-char tch "The object you were editing has been destroyed.~%")))

  (send-to-char ch "Object eliminated.~%"))

(defun save-object (ouf obj)
  (format ouf "#~d~%" (vnum-of obj))
  (format ouf "~a~~~%" (aliases-of obj))
  (format ouf "~a~~~%" (name-of obj))
  (format ouf "~a~~~%" (line-desc-of obj))
  (format ouf "~a~~~%" (action-desc-of obj))
  (format ouf "~d ~a ~a ~a ~a~%"
          (kind-of obj)
          (bits-to-asciiflag (extra-flags-of obj))
          (bits-to-asciiflag (extra2-flags-of obj))
          (bits-to-asciiflag (wear-flags-of obj))
          (bits-to-asciiflag (extra3-flags-of obj)))
  (format ouf "~{~d~^ ~}~%" (coerce (value-of obj) 'list))
  (format ouf "~d ~d ~d~%"
          (material-of obj)
          (max-dam-of obj)
          (damage-of obj))
  (format ouf "~d ~d ~d ~d~%"
          (weight-of obj)
          (cost-of (shared-of obj))
          (cost-per-day-of (shared-of obj))
          (timer-of obj))
  (dolist (exd (reverse (ex-description-of obj)))
    (format ouf "E~%~a~~~%~a~~~%" (keyword-of exd) (description-of exd)))
  (loop for aff-idx from 0 upto (1- +max-obj-affect+)
       unless (= (location-of (aref (affected-of obj) aff-idx)) +apply-none+) do
       (format ouf "A~%~d ~d~%"
               (location-of (aref (affected-of obj) aff-idx))
               (modifier-of (aref (affected-of obj) aff-idx))))
  (loop for idx from 0 upto 2
       unless (zerop (aref (bitvector-of obj) idx)) do
       (format ouf "V~%~d ~a~%" (1+ idx) (bits-to-asciiflag (aref (bitvector-of obj) idx))))
  (when (func-param-of (shared-of obj))
    (format ouf "P~%~a~~~%" (func-param-of (shared-of obj))))
  (unless (zerop (owner-id-of (shared-of obj)))
    (format ouf "O ~d~%" (owner-id-of (shared-of obj)))))

(defun save-zone-objects (ch zone)
  (let ((context nil))
    (handler-case
        (progn
          (setf context "object index")
          (update-index-file (tempus-path "lib/world/obj/index") (number-of zone) "obj")

          (setf context "object file")
          (with-open-file (ouf (tempus-path "lib/world/obj/~d.obj" (number-of zone))
                               :direction :output
                               :if-exists :rename-and-delete
                               :if-does-not-exist :create)
            (loop
               for vnum from (* (number-of zone) 100) upto (top-of zone)
               as obj-proto = (real-object-proto vnum)
               when obj-proto
               do (save-object ouf obj-proto))
            (format ouf "$~%"))

          (slog "OLC: ~a osaved ~d" (name-of ch) (number-of zone))
          t)
      (error (err)
        (slog "Error while saving ~a: ~a!" context err)
        nil))))

(defun save-object-special-assignments ()
  (with-open-file (ouf (tempus-path "lib/etc/spec_ass_obj")
                       :direction :output
                       :if-exists :rename-and-delete
                       :if-does-not-exist :create)
    (dolist (obj (sort (hash-values *object-prototypes*) #'< :key 'vnum-of))
      (format ouf "~6d ~20s ## ~a~%"
              (vnum-of (shared-of obj))
              (find-special-by-func (func-of (shared-of obj)))
              (name-of obj)))))

(defun perform-olc-oload (ch vnum)
  (when (check-can-edit ch (zone-containing-number vnum) +zone-objs-approved+)
    (cond
      ((not (eql (zone-containing-number vnum) (zone-of (in-room-of ch))))
       (send-to-char ch "You cannot olc oload objects from other zones.~%"))
      ((and (not (security-is-member ch "OLCWorldWrite")) (approvedp (real-object-proto vnum)))
       (send-to-char ch "You cannot olc oload approved items.~%"))
      (t
       (let ((obj (read-object vnum)))
         (obj-to-char obj ch)
         (setf (timer-of obj) (level-of ch))
         (act ch :target-item obj
              :subject-emit "$P appears in your hands."
              :place-emit "$n creates $P in $s hands.")
         (slog "OLC: ~a oloaded ~a(~d)" (name-of ch) (name-of obj) (vnum-of obj)))))))

(defun perform-omimic (dst src)
   (setf (name-of dst) (name-of src))
   (setf (aliases-of dst) (aliases-of src))
   (setf (line-desc-of dst) (line-desc-of src))
   (setf (action-desc-of dst) (action-desc-of src))
   (setf (kind-of dst) (kind-of src))
   (setf (wear-flags-of dst) (wear-flags-of src))
   (setf (extra-flags-of dst) (extra-flags-of src))
   (setf (extra2-flags-of dst) (extra2-flags-of src))
   (setf (extra3-flags-of dst) (extra3-flags-of src))
   (setf (weight-of dst) (weight-of src))
   (setf (timer-of dst) (timer-of src))
   (setf (material-of dst) (material-of src))
   (setf (max-dam-of dst) (max-dam-of src))
   (setf (damage-of dst) (damage-of src))
   (setf (sigil-idnum-of dst) (sigil-idnum-of src))
   (setf (sigil-level-of dst) (sigil-level-of src))
   (setf (cost-of (shared-of dst)) (cost-of (shared-of src)))
   (setf (cost-per-day-of (shared-of dst)) (cost-per-day-of (shared-of src)))

   (dotimes (i 4)
     (setf (aref (value-of dst) i) (aref (value-of src) i)))
   (dotimes (i 3)
     (setf (aref (bitvector-of dst) i) (aref (bitvector-of src) i)))
   (dotimes (i +max-obj-affect+)
     (setf (location-of (aref (affected-of dst) i)) (location-of (aref (affected-of src) i)))
     (setf (modifier-of (aref (affected-of dst) i)) (modifier-of (aref (affected-of src) i))))
   (setf (ex-description-of dst) (copy-extra-descs src)))

(defun perform-olist (ch vnum-start vnum-end)
  (with-pagination ((link-of ch))
    (loop
       with index = 1
       for vnum from vnum-start upto vnum-end
       as obj = (real-object-proto vnum)
       when obj
       do
         (send-to-char ch "~4d. &g[&n~5d&g]&g ~40a &n<~3d>~:[~; (!ap)~]~:[~; (nodesc)~]~%"
                       index vnum
                       (name-of obj)
                       (number-of (shared-of obj))
                       (is-obj-stat2 obj +item2-unapproved+)
                       (zerop (length (line-desc-of obj))))
         (incf index))))

(defun perform-oset-apply (ch object input)
  (let* ((inputs (ppcre:split "\\s+" input))
         (location (or (parse-integer (first inputs) :junk-allowed t)
                       (position (first inputs) +apply-types+ :test 'string-abbrev)))
         (value (parse-integer (second inputs) :junk-allowed t))
         (idx (position 0 (affected-of object) :key 'location-of)))
    (cond
      ((null location)
       (send-to-char ch "Unknown apply type... Type olchelp apply.~%"))
      ((not (< 0 location +num-applies+))
       (send-to-char ch "Location out of range.  Try olchelp apply.~%"))
      ((not (<= -125 value 125))
       (send-to-char ch "Modifier out of range. [-125, 125].~%"))
      ((null idx)
       (send-to-char ch "All apply slots are filled.  Set an existing apply modifier to zero to remove.~%"))
      (t
       (setf (modifier-of (aref (affected-of object) idx)) value)
       (if (zerop value)
           (setf (location-of (aref (affected-of object) idx)) 0)
           (setf (location-of (aref (affected-of object) idx)) location))
       (send-to-char ch "Done.~%")))))

(defun perform-oset-value (ch object input)
  (let* ((inputs (ppcre:split "\\s+" input))
         (idx (parse-integer (first inputs) :junk-allowed t))
         (value (parse-integer (second inputs) :junk-allowed t)))
    (cond
      ((or (null idx)
           (null value)
           (not (<= 0 idx 3)))
       (send-to-char ch "Usage: olc oset value (0|1|2|3) <value>~%"))
      (t
       (setf (aref (value-of object) idx) value)
       (send-to-char ch "Value ~d (~a) set to ~d.~%"
                     idx (aref +item-kind-values+ (kind-of object) idx) value)))))

(defun perform-oset-affected (ch object input)
  (let* ((inputs (ppcre:split "\\s+" input))
         (index (parse-integer (first inputs) :junk-allowed t)))
    (cond
      ((not (<= 1 index 3))
       (send-to-char ch "Index must be between 1 and 3.~%"))
      (t
       (perform-set-flags ch (second inputs) (cddr inputs)
                          (case index
                            (1 +affected-bits-desc+)
                            (2 +affected2-bits-desc+)
                            (3 +affected3-bits-desc+))
                          "affect object"
                          "olc oset affection <index> (+/-) <flag>"
                          (lambda ()
                            (aref (bitvector-of object) (1- index)))
                          (lambda (val)
                            (setf (aref (bitvector-of object) (1- index)) val)))))))

(defun perform-oset-special (ch object input)
  (let ((special (find input (hash-keys *special-funcs*) :test 'string-abbrev)))
    (cond
      ((null special)
       (send-to-char ch "That is not a valid special.~%"))
      ((not (logtest (gethash special *special-flags*) +spec-obj+))
       (send-to-char ch "This special is not for objects.~%"))
      ((and (logtest (gethash special *special-flags*) +spec-res+)
            (not (security-is-member ch "OLCWorldWrite")))
       (send-to-char ch "This special is reserved.~%"))
      (t
       (setf (func-of (shared-of object)) (gethash special *special-funcs*))
       (save-object-special-assignments)
       (send-to-char ch "Object special set, you trickster you.~%")))))

(defun perform-oset-description (ch object)
  (let ((exd (find (aliases-of object)
                   (ex-description-of object)
                   :test #'string=
                   :key 'keyword-of)))
    (act ch :place-emit "$n begins to edit an object description.")
    (setf (plr-bits-of ch) (logior (plr-bits-of ch) +plr-olc+))
    (start-text-editor (link-of ch)
                       object
                       "an object description"
                       (if exd (description-of exd) "")
                       (lambda (cxn target buf)
                         (when (and (not (eql (proto-of (shared-of object)) object))
                                    (eql (ex-description-of (proto-of (shared-of object)))
                                         (ex-description-of object)))
                           (setf (ex-description-of object)
                                 (copy-extra-descs (ex-description-of object))))

                         (if exd
                             (setf (description-of exd) buf)
                             (push (make-instance 'extra-desc-data
                                                  :keyword (aliases-of target)
                                                  :description buf)
                                   (ex-description-of target)))
                         (when (eql (proto-of (shared-of object)) object)
                           (update-objlist-full (vnum-of object)))
                         (setf (plr-bits-of (actor-of cxn))
                               (logandc2 (plr-bits-of (actor-of cxn)) +plr-olc+))
                         (setf (state-of cxn) 'playing))
                       (lambda (cxn target)
                         (declare (ignore target))
                         (setf (plr-bits-of (actor-of cxn))
                               (logandc2 (plr-bits-of (actor-of cxn)) +plr-olc+))
                         (setf (state-of cxn) 'playing)))))

(defun perform-oset-alias (ch object input)
  (perform-set-string ch input object "object aliases" nil
                      (lambda (val)
                        (let ((proto (proto-of (shared-of object))))
                          (unless (or (null proto) (eql proto object))
                            (when (eql (ex-description-of proto) (ex-description-of object))
                              (setf (ex-description-of object)
                                    (copy-extra-descs proto)))
                            (dolist (exd (ex-description-of object))
                              (when (string= (aliases-of object) (keyword-of exd))
                                (setf (keyword-of exd) val))))
                          (setf (aliases-of object) val)))))

(defcommand (ch "olc" "create" "object") (:immortal)
  (send-to-char ch "Create an object with what vnum?~%"))

(defcommand (ch "olc" "create" "object" "next") (:immortal)
  (let* ((zone (zone-of (in-room-of ch)))
         (obj-vnum (loop for num from (* (number-of zone) 100) upto (top-of zone)
                      when (null (real-object-proto num)) do (return num)
                      finally (return nil))))
    (cond
      ((not (check-can-edit ch zone +zone-objs-approved+))
       nil)
      ((null obj-vnum)
       (send-to-char ch "No allocatable objects found in zone.~%"))
      (t
       (perform-create-object ch obj-vnum)))))

(defcommand (ch "olc" "create" "object" number) (:immortal)
  (let* ((obj-vnum (parse-integer number :junk-allowed t))
         (zone (and obj-vnum (zone-containing-number obj-vnum))))
    (cond
      ((null obj-vnum)
       (send-to-char ch "Usage: olc create object <vnum>~%"))
      ((null zone)
       (send-to-char ch "No zone for the object to be in.~%"))
      ((not (check-can-edit ch zone +zone-objs-approved+))
       nil)
      ((real-object-proto obj-vnum)
       (send-to-char ch "The object already exists.~%"))
      (t
       (perform-create-object ch obj-vnum)))))

(defcommand (ch "approve" "object") (:immortal)
  (send-to-char ch "Usage: approve object <vnum>~%"))

(defcommand (ch "approve" "object" vnum) (:immortal)
  (with-numeric-input ((vnum "That's no object vnum.~%"))
    (let* ((obj (real-object-proto vnum))
           (zone (zone-containing-number vnum)))
      (cond
        ((null obj)
         (send-to-char ch "There exists no object with that number, slick.~%"))
        ((null zone)
         (send-to-char ch "That object belongs to no zone.~%"))
        ((not (logtest (extra2-flags-of obj) +item2-unapproved+))
         (send-to-char ch "That item is already approved.~%"))
        (t
         (setf (extra2-flags-of obj) (logandc2 (extra2-flags-of obj) +item2-unapproved+))
         (send-to-char ch "Object approved for full inclusion in the game.~%")
         (slog "~a approved object ~a[~d]" (name-of ch) (name-of obj) (vnum-of obj))
         (save-zone-objects ch zone))))))

(defcommand (ch "unapprove" "object") (:immortal)
  (send-to-char ch "Usage: unapprove object <vnum>~%"))

(defcommand (ch "unapprove" "object" vnum) (:immortal)
  (with-numeric-input ((vnum "That's no object vnum.~%"))
    (let* ((obj (real-object-proto vnum))
           (zone (zone-containing-number vnum)))
      (cond
        ((null obj)
         (send-to-char ch "There exists no object with that number, slick.~%"))
        ((null zone)
         (send-to-char ch "That object belongs to no zone.~%"))
        ((logtest (extra2-flags-of obj) +item2-unapproved+)
         (send-to-char ch "That item is already unapproved.~%"))
        (t
         (setf (extra2-flags-of obj) (logior (extra2-flags-of obj) +item2-unapproved+))
         (send-to-char ch "Object unapproved.~%")
         (slog "~a unapproved object ~a[~d]" (name-of ch) (name-of obj) (vnum-of obj))
         (save-zone-objects ch zone))))))

(defcommand (ch "olc" "clear" "object") (:immortal)
  (when (and (check-is-editing ch "object" (olc-obj-of ch))
             (check-can-edit ch
                             (zone-containing-number (vnum-of (shared-of (olc-obj-of ch))))
                             +zone-objs-approved+))
    (with-slots (name aliases line-desc action-desc ex-description soilage value kind wear-flags
                      extra-flags extra2-flags extra3-flags weight timer bitvector material max-dam damage
                      sigil-idnum sigil-level)
        (olc-obj-of ch)
      (setf name "a fresh blank object")
      (setf aliases "fresh blank object")
      (setf line-desc "A fresh blank object is here.")
      (setf value (make-array 4 :initial-element 0))
      (setf bitvector (make-array 3 :initial-element 0))
      (setf action-desc nil)
      (setf ex-description nil)
      (setf soilage 0)
      (setf kind 0)
      (setf wear-flags 0)
      (setf extra-flags 0)
      (setf extra2-flags 0)
      (setf extra3-flags 0)
      (setf weight 0)
      (setf timer 0)
      (setf material 0)
      (setf max-dam 100)
      (setf damage 100)
      (setf sigil-idnum 0)
      (setf sigil-level 0))
    (setf (owner-id-of (shared-of (olc-obj-of ch))) 0)
    (update-objlist-full (vnum-of (olc-obj-of ch)))
    (send-to-char ch "Okay, object #~d fully cleared.~%" (vnum-of (olc-obj-of ch)))))

(defcommand (ch "olc" "destroy" "object") (:immortal)
  (send-to-char ch "Usage: olc destroy object <vnum>~%"))

(defcommand (ch "olc" "destroy" "object" number) (:immortal)
  (let* ((vnum (parse-integer number :junk-allowed t))
         (zone (and vnum (zone-containing-number vnum))))
    (cond
      ((null vnum)
       (send-to-char ch "Usage: olc destroy object <vnum>~%"))
      ((null zone)
       (send-to-char ch "That object doesn't belong to any zone.~%"))
      ((can-edit-zone ch zone +zone-objs-approved+)
       (perform-destroy-object ch vnum)))))

(defcommand (ch "olc" "osave") (:immortal)
  (let ((zone (if (olc-obj-of ch)
                  (zone-containing-number (vnum-of (olc-obj-of ch)))
                  (zone-of (in-room-of ch)))))
    (when (check-can-edit ch zone +zone-objs-approved+)
      (if (save-zone-objects ch zone)
        (send-to-char ch "You save the objects for zone #~d (~a).~%"
                      (number-of zone)
                      (name-of zone))
        (send-to-char ch "The objects for zone #~d (~a) could not be saved.~%"
                      (number-of zone)
                      (name-of zone))))))

(defcommand (ch "olc" "oedit") (:immortal)
  (if (olc-obj-of ch)
      (send-to-char ch "Current olc object: [~d] &g~a&n~%"
                    (vnum-of (shared-of (olc-obj-of ch)))
                    (name-of (olc-obj-of ch)))
      (send-to-char ch "You are not currently editing an object.~%")))

(defcommand (ch "olc" "oedit" "exit") (:immortal)
  (setf (olc-obj-of ch) nil)
  (send-to-char ch "Exiting object editor.~%"))

(defcommand (ch "olc" "oedit" number) (:immortal)
  (let* ((vnum (parse-integer number :junk-allowed t))
         (obj (and vnum (real-object-proto vnum))))
    (cond
      ((null vnum)
       (send-to-char ch "Usage: olc oedit (<number>|exit)~%"))
      ((null obj)
       (send-to-char ch "There is no such object.~%"))
      ((check-can-edit ch (zone-containing-number vnum) +zone-objs-approved+)
       (setf (olc-obj-of ch) obj)
       (send-to-char ch "Now editing object [~d] &g~a&n.~%"
                     vnum (name-of obj))))))

(defcommand (ch "olc" "ostat") (:immortal)
  (if (check-is-editing ch "object" (olc-obj-of ch))
      (send-stats-to-char ch (olc-obj-of ch))
      (send-to-char ch "You aren't editing an object.~%")))

(defcommand (ch "olc" "ostat" number) (:immortal)
  (let* ((vnum (parse-integer number :junk-allowed t))
         (obj (and vnum (real-object-proto vnum))))
    (cond
      ((null vnum)
       (send-to-char ch "Usage: olc ostat [<number>]~%"))
      ((null obj)
       (send-to-char ch "There is no such object.~%"))
      (t
       (send-stats-to-char ch obj)))))

(defcommand (ch "olc" "oset") (:immortal)
  (with-pagination ((link-of ch))
    (send-to-char ch "Valid oset commands:~%&y~a&n"
                  (print-columns-to-string 5 15
                                           (sort (mapcar #'first +oset-params+)
                                                 #'string<)))))

(defcommand (ch "olc" "oset" param) (:immortal)
  (when (and (check-is-editing ch "object" (olc-obj-of ch))
             (check-can-edit ch
                             (zone-containing-number (vnum-of (olc-obj-of ch)))
                             +zone-objs-approved+))
    (perform-set ch (olc-obj-of ch) t +oset-params+ param nil)))

(defcommand (ch "olc" "oset" param value) (:immortal)
  (when (and (check-is-editing ch "object" (olc-obj-of ch))
             (check-can-edit ch
                             (zone-containing-number (vnum-of (olc-obj-of ch)))
                             +zone-objs-approved+))
    (perform-set ch (olc-obj-of ch) t +oset-params+ param value)))

(defcommand (ch "olc" "oexdesc" "create" keywords) (:immortal)
  (when (and (check-is-editing ch "object" (olc-obj-of ch))
             (check-can-edit ch (zone-containing-number (vnum-of (olc-obj-of ch)))
                             +zone-objs-approved+))
    (cond
      ((find keywords (ex-description-of (olc-obj-of ch))
             :test #'string-abbrev
             :key 'keyword-of)
       (send-to-char ch "~
An extra description already exists with that keyword.
Use the 'olc oexdesc remove' command to remove it, or the
'olc oexdesc edit' command to change it.
"))
      (t
       (setf (plr-bits-of ch) (logior (plr-bits-of ch) +plr-olc+))
       (act ch :place-emit "$n begins to write an extra description.~%")
       (start-text-editor (link-of ch)
                          (olc-obj-of ch)
                          "an object extradesc"
                          ""
                          (lambda (cxn obj buf)
                            (push (make-instance 'extra-descr-data
                                                 :keyword keywords
                                                 :description buf)
                                  (ex-description-of obj))
                            (setf (plr-bits-of (actor-of cxn))
                                  (logandc2 (plr-bits-of (actor-of cxn)) +plr-olc+))
                            (update-objlist-full (vnum-of (olc-obj-of ch)))
                            (setf (state-of cxn) 'playing))
                          (lambda (cxn obj)
                            (declare (ignore obj))
                            (setf (plr-bits-of (actor-of cxn))
                                  (logandc2 (plr-bits-of (actor-of cxn)) +plr-olc+))
                            (setf (state-of cxn) 'playing)))))))

(defcommand (ch "olc" "oexdesc" "remove" keywords) (:immortal)
  (when (and (check-is-editing ch "object" (olc-obj-of ch))
             (check-can-edit ch (zone-containing-number (vnum-of (olc-obj-of ch)))
                             +zone-objs-approved+))
    (let ((exd (find keywords (ex-description-of (olc-obj-of ch))
                     :test #'string-abbrev
                     :key 'keyword-of)))
      (cond
        ((null exd)
         (send-to-char ch "No such description.~%"))
        (t
         (setf (ex-description-of (olc-obj-of ch))
               (delete exd (ex-description-of (olc-obj-of ch))))
         (update-objlist-full (vnum-of (olc-obj-of ch)))
         (send-to-char ch "Description removed.~%"))))))

(defcommand (ch "olc" "oexdesc" "edit" keywords) (:immortal)
  (when (and (check-is-editing ch "object" (olc-obj-of ch))
             (check-can-edit ch (zone-containing-number (vnum-of (olc-obj-of ch)))
                             +zone-objs-approved+))
    (let ((exd (find keywords (ex-description-of (olc-obj-of ch))
                     :test #'string-abbrev
                     :key 'keyword-of)))
      (cond
        ((null exd)
         (send-to-char ch "No such description.~%"))
        (t
         (setf (plr-bits-of ch) (logior (plr-bits-of ch) +plr-olc+))
         (act ch :place-emit "$n begins to write an extra description.~%")
         (start-text-editor (link-of ch)
                            exd
                            "an extradesc"
                            ""
                            (lambda (cxn exd buf)
                              (setf (description-of exd) buf)
                              (update-objlist-full (vnum-of (olc-obj-of ch)))
                              (setf (plr-bits-of (actor-of cxn))
                                    (logandc2 (plr-bits-of (actor-of cxn)) +plr-olc+))
                              (setf (state-of cxn) 'playing))
                            (lambda (cxn exd)
                              (declare (ignore exd))
                              (setf (plr-bits-of (actor-of cxn))
                                    (logandc2 (plr-bits-of (actor-of cxn)) +plr-olc+))
                              (setf (state-of cxn) 'playing))))))))

(defcommand (ch "olc" "oexdesc" "addkey" keyword more-keywords) (:immortal)
  (when (and (check-is-editing ch "object" (olc-obj-of ch))
             (check-can-edit ch (zone-containing-number (vnum-of (olc-obj-of ch)))
                             +zone-objs-approved+))
    (let ((exd (find keyword (ex-description-of (olc-obj-of ch))
                     :test #'string-abbrev
                     :key 'keyword-of)))
      (cond
        ((null exd)
         (send-to-char ch "No such description.~%"))
        (t
         (setf (keyword-of exd) (format nil "~a ~a" (keyword-of exd) more-keywords))
         (update-objlist-full (vnum-of (olc-obj-of ch)))
         (send-to-char ch "Keywords added.~%"))))))

(defcommand (ch "olc" "oload") (:immortal)
  (if (olc-obj-of ch)
      (perform-olc-oload ch (vnum-of (olc-obj-of ch)))
      (send-to-char ch "Which object?~%")))

(defcommand (ch "olc" "oload" number) (:immortal)
  (let ((vnum (parse-integer number :junk-allowed t)))
    (cond
      ((null vnum)
       (send-to-char ch "The argument must be a vnum.~%"))
      ((null (real-object-proto vnum))
       (send-to-char ch "No such object exists.~%"))
      (t
       (perform-olc-oload ch vnum)))))

(defcommand (ch "olc" "omimic") (:immortal)
  (send-to-char ch "Usage: olc omimic <vnum>~%"))

(defcommand (ch "olc" "omimic" number) (:immortal)
  (when (and (check-is-editing ch "object" (olc-obj-of ch))
             (check-can-edit ch (zone-containing-number (vnum-of (olc-obj-of ch)))
                             +zone-objs-approved+))
    (let* ((vnum (parse-integer number :junk-allowed t))
           (proto (and vnum (real-object-proto vnum))))
      (cond
        ((null proto)
         (send-to-char ch "That's not a valid object, genius.~%"))
        ((eql proto (olc-obj-of ch))
         (send-to-char ch "Real funny.~%"))
        (t
         (perform-omimic (olc-obj-of ch) proto)
         (unless (security-is-member ch "OLCWorldWrite")
           (setf (extra2-flags-of (olc-obj-of ch)) (logandc2 (extra2-flags-of (olc-obj-of ch))
                                                             +item2-unapproved+)))
         (update-objlist-full vnum)
         (send-to-char ch "Okay, done mimicing.~%"))))))

(defcommand (ch "olist") (:immortal)
  (let ((zone (zone-of (in-room-of ch))))
    (perform-olist ch (* (number-of zone) 100) (top-of zone))))

(defcommand (ch "olist" junk) (:immortal)
  (declare (ignore junk))
  (send-to-char ch "Usage: olist [<start vnum> <end vnum>]~%"))

(defcommand (ch "olist" start end) (:immortal)
  (let ((start-num (parse-integer start :junk-allowed t))
        (end-num (parse-integer end :junk-allowed t)))
    (if (or (null start-num) (null end-num))
        (send-to-char ch "Usage: olist [<start vnum> <end vnum>]~%")
        (perform-olist ch (min start-num end-num) (max start-num end-num)))))