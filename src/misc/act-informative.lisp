(in-package #:tempus)

(defparameter +dirs+ #("n" "e" "s" "w" "u" "d" "f" "p"))
(defparameter +num-of-dirs+ (length +dirs+))

(defun do-auto-exits (ch room)
  (send-to-char ch "&c[ Exits: ~:[None obvious~;~:*~{~a~^ ~}~] ]"
                (loop for door across (dir-option-of room)
                     for dir across +dirs+
                   unless (or (null door)
                              (zerop (to-room-of door))
                              (logtest (exit-info-of door)
                                       (logior +ex-hidden+ +ex-secret+))) 
                   collect (if (logtest (exit-info-of door)
                                        (logior +ex-closed+))
                               (format nil "|~a|" dir)
                               (format nil "~a" dir))))
  (when (immortalp ch)
    (send-to-char ch " [ Hidden doors: ~:[None~;~:*~{~a~^ ~}~] ]"
                  (loop for door across (dir-option-of room)
                     for dir across +dirs+
                     unless (or (null door)
                                (zerop (to-room-of door))
                                (not (logtest (exit-info-of door)
                                              (logior +ex-hidden+ +ex-secret+))))
                     collect (if (logtest (exit-info-of door)
                                          (logior +ex-closed+))
                                 (format nil "|~a|" dir)
                                 (format nil "~a" dir))))))

(defun show-room-obj (object ch stream)
  (cond
    ((line-desc-of object)
     (princ (line-desc-of object) stream))
    ((immortalp ch)
     (format stream "~a exists here."
             (string-upcase (name-of object) :end 1)))))

(defun show-obj-bits (object ch stream)
  nil)

(defun show-obj-extra (object ch stream)
  nil)

(defun show-obj-to-char (stream object ch mode count)
    (cond
      ((eql mode :room)
       (show-room-obj object ch stream))
      ((and (or (eql mode :inv)
                (eql mode :content))
            (name-of object))
       (princ (name-of object) stream))
      ((eql mode :extra)
       (show-obj-extra object ch stream)))

    (unless (eql mode :nobits)
      (show-obj-bits object ch stream))

    (when (> count 1)
      (format stream " [~d]" count))
    (format stream "~%")

    (when (and (= (kind-of object) +item-vehicle+)
               (eql mode :bits)
               (car-openable object))
      (format stream "The door of ~a is ~a."
              (describe object ch)
              (if (car-closed object) "closed" "open"))))

(defun list-obj-to-char (stream obj-list ch mode show)
  (let ((corpse (and obj-list
                     (in-obj-of (first obj-list))
                     (is-corpse (first obj-list))))
        (found nil))
    (loop with o = obj-list
       for i = (car obj-list) then (car o)
       while i do
       (cond
         ((or (not (can-see-object ch i))
              (is-soilage i)
              (and (is-obj-stat2 i +item2-hidden+)
                   (not (pref-flagged ch +pref-holylight+))
                   (> (random-range 50 120) (hidden-obj-prob ch i))))
          nil)
         ((and corpse
               (is-implant i)
               (not (can-wear i +item-wear-take+))
               (not (pref-flagged ch +pref-holylight+))
               (< (+ (check-skill ch +skill-cyberscan+)
                     (if (aff3-flagged ch +aff3-sonic-imagery+) 50 0))
                  (random-range 80 150)))
          nil)
         ((or (and (proto-of (shared-of i))
                   (string/= (name-of i) (name-of (proto-of (shared-of i)))))
              (is-obj-stat2 i +item2-broken+))
          (setf o (cdr o))
          (setf found t)
          (show-obj-to-char stream i ch mode 1))
         (t
          (setf found t)
          (setf o (cdr o))
          (show-obj-to-char stream i ch mode
                            (1+ (loop while (and o (same-obj (car o) i))
                                   do (setf o (cdr o))
                                   count (can-see-object ch o)))))))
    (when (and (not found) show)
      (format stream " Nothing.~%"))))


(defun look-at-room (ch room ignore-brief)
  (unless (link-of ch)
    (return-from look-at-room))

  (when (and (room-is-dark ch) (not (has-dark-sight ch)))
    (send-to-char ch "It is pitch black...~%")
    (return-from look-at-room))

  (if (pref-flagged ch +pref-roomflags+)
      (progn
        (send-to-char ch "&c[~5d] ~a [ ~a ] [ ~a ]"
                      (number-of room)
                      (name-of room)
                      (if (zerop (flags-of room))
                          "NONE"
                          (printbits (flags-of room) +room-bits+))
                      (aref +sector-types+ (terrain-of room)))
        #+nil        (when (< (max-occupancy-of room) 256)
                       (send-to-char ch " [ Max: ~d ]" (max-occupancy-of room)))

        #+nil        (let ((house (find-house-by-room (number-of room))))
                       (when house
                         (send-to-char ch " [ House: ~d ]" (id-of house)))))
      (send-to-char ch "&c~a" (name-of room)))

  (send-to-char ch "&n~%")

  (when (or (not (pref-flagged ch +pref-brief+))
            ignore-brief
            (room-flagged room +room-death+))
    (if (and (room-flagged room +room-smoke-filled+)
             (not (pref-flagged ch +pref-holylight+))
             #+nil (not (aff3-flagged ch +aff3-sonic-imagery+)))
        (send-to-char ch "The smoke swirls around you...~%")
        (when (description-of room)
          (send-to-char ch "~a" (description-of room)))))

  (case (pk-style-of (zone-of room))
    (0
     (send-to-char ch "&c[ &g!PK&c ] "))
    (1
     (send-to-char ch "&c[ &YNPK&c ] "))
    (2
     (send-to-char ch "&c[ &RCPK&c ] ")))

  (unless (or (immortalp ch)
              (not (room-flagged room +room-smoke-filled+))
              #+nil (aff3-flagged ch +aff3-sonic-imagery+))
    (send-to-char ch "~%")
    (return-from look-at-room))

  ;; autoexits
  (when (pref-flagged ch +pref-autoexit+)
      (do-auto-exits ch room))
  (send-to-char ch "~%")

  ;; now list characters & objects
  (let ((blood-shown nil)
        (ice-shown nil))
    (dolist (o (contents-of room))
      (when (and (= (vnum-of (shared-of o)) +blood-vnum+)
                 (not blood-shown)
                 (send-to-char ch "&r~a.&n~%"
                               (cond
                                 ((< (timer-of o) 10)
                                  "Some spots of blood have been splattered around")
                                 ((< (timer-of o) 20)
                                  "Small pools of blood are here")
                                 ((< (timer-of o) 30)
                                  "Large pools of blood are here")
                                 ((< (timer-of o) 40)
                                  "Blood is pooled and splattered over everything")
                                 (t
                                  "Dark red blood covers everything in sight.")))
                 (setf blood-shown t)))
      (when (and (= (vnum-of (shared-of o)) +ice-vnum+)
                 (not ice-shown))
        (send-to-char ch "&r~a.&n~%"
                      (cond
                        ((< (timer-of o) 10)
                         "A few patches of ice are scattered around")
                        ((< (timer-of o) 20)
                         "A thin coating of ice covers everything")
                        ((< (timer-of o) 30)
                         "A thick coating of ice covers everything")
                        (t
                         "Everything is covered with a thick coating of ice")))
        (setf ice-shown t))))
  (send-to-char ch "&g~a&n"
          (with-output-to-string (s)
            (list-obj-to-char s (contents-of room) ch :room nil)))
#+nil
  (list-char-to-char (people-of room) ch))
    
