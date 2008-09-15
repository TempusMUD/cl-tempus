(in-package #:tempus)

(defun can-carry-items (ch)
  (+ 1
     (dex-of ch)
     (if (immortal-level-p ch) 100000 0)
     (if (aff2-flagged ch +aff2-telekinesis+) (floor (level-of ch) 4) 0)))

(defun can-carry-weight (ch)
  (let ((raw-weight (getf (aref +str-app+ (strength-apply-index ch)) :carry-w)))
    (max 10
         (+ (if (aff2-flagged ch +aff2-telekinesis+)
                (* raw-weight 2)
                raw-weight)
            (if (immortal-level-p ch) 100000 0)))))

(defun can-take-obj (ch obj check-weight print)
  (cond
    ((>= (carry-items-of ch) (can-carry-items ch))
     (when print
       (act ch :item obj
            :subject-emit "$p: you can't carry that many items."))
     nil)
    ((and check-weight
          (> (+ (carry-weight-of ch) (weight-of obj)) (can-carry-weight ch)))
     (when print
       (act ch :item obj
            :subject-emit "$p: you can't carry that much weight."))
     nil)
    ((and (not (can-wear obj +item-wear-take+))
          (not (immortalp ch)))
     (when print
       (act ch :item obj
            :subject-emit "$p: you can't take that!"))
     nil)
    (t
     t)))

(defun perform-get-from-room (ch obj display counter)
  (let ((retval nil))
    (cond
      ((can-take-obj ch obj t t)
       (obj-from-room obj)
       (obj-to-char obj ch)
       (setf retval t))
      ((> counter 1)
       (setf display t)
       (decf counter))
      (t
       (return-from perform-get-from-room nil)))

    (cond
      ((or (not display) (not (plusp counter)))
       nil)
      ((= counter 1)
       (act ch :item obj :all-emit "$n get$% $p."))
      (t
       (act ch :item obj :all-emit (format nil "$n get$% $p. (x~d)" counter))))

    retval))

(defun get-from-room (ch arg)
  (let ((dotmode (find-all-dots arg))
        (money-found nil)
        (quad-found nil)
        (sigil-found nil))
    (case dotmode
      (:find-indiv
       ;; 'get <item>'
       (let ((obj (get-obj-in-list-vis ch arg (contents-of (in-room-of ch)))))
         (cond
           ((null obj)
            (send-to-char ch "You don't see ~a ~a here.~%"
                          (a-or-an arg) arg))
           ((and (is-corpse obj)
                 (/= (corpse-idnum obj) (idnum-of ch))
                 (not (immortalp ch))
                 (eql (pk-style-of (zone-of (in-room-of ch))) +zone-neutral-pk+)
                 (not (is-npc ch))
                 (plusp (corpse-idnum obj)))
            (send-to-char ch "You can't take corpses in NPK zones!~%"))
           ((not (perform-get-from-room ch obj t 1))
            nil)
           ((is-obj-kind obj +item-money+)
            (setf money-found t))
           ((eql (vnum-of obj) +quad-vnum+)
            (setf quad-found t))
           ((and (not (zerop (sigil-idnum-of obj)))
                 (/= (sigil-idnum-of obj) (idnum-of ch)))
            (setf sigil-found t)))))
      (:find-alldot
       nil))))


(defcommand (ch "get" thing) (:resting)
    (cond
      ((>= (carry-items-of ch) (can-carry-items ch))
       (send-to-char ch "Your arms are already full!~%"))
      ((string= thing "")
       (send-to-char ch "Get what?~%"))
      (t
       (get-from-room ch thing))))
