(in-package #:tempus)

(defclass shop-data ()
  ((room :accessor room-of :initarg room :initform nil)
   (produced :accessor produced-of :initarg produced :initform nil)
   (will-buy :accessor will-buy-of :initarg will-buy :initform nil)
   (closed-hours :accessor closed-hours-of :initarg closed-hours :initform nil)
   (denied-msg :accessor denied-msg-of :initarg denied-msg)
   (badobj-msg :accessor badobj-msg-of :initarg badobj-msg)
   (sell-noobj-msg :accessor sell-noobj-msg-of :initarg sell-noobj-msg)
   (buy-noobj-msg :accessor buy-noobj-msg-of :initarg buy-noobj-msg)
   (selfbroke-msg :accessor selfbroke-msg-of :initarg selfbroke-msg)
   (buyerbroke-msg :accessor buyerbroke-msg-of :initarg buyerbroke-msg)
   (buy-msg :accessor buy-msg-of :initarg buy-msg)
   (sell-msg :accessor sell-msg-of :initarg sell-msg)
   (temper-cmd :accessor temper-cmd-of :initarg temper-cmd :initform nil)
   (closed-msg :accessor closed-msg-of :initarg closed-msg)
   (markup :accessor markup-of :initarg markup)
   (markdown :accessor markdown-of :initarg markdown)
   (currency :accessor currency-of :initarg currency)
   (revenue :accessor revenue-of :initarg revenue)
   (steal-ok-p :accessor steal-ok-p :initarg steal-ok-p)
   (attack-ok-p :accessor attack-ok-p :initarg attack-ok-p)
   (call-for-help-p :accessor call-for-help-p :initarg call-for-help-p)
   (subfunc :accessor subfunc-of :initarg subfunc)
   (reaction :accessor reaction-of :initarg reaction))
  (:default-initargs
    denied-msg "I'm not doing business with YOU!"
    badobj-msg "I don't buy that sort of thing."
    selfbroke-msg "Sorry, but I don't have the cash."
    buyerbroke-msg "You don't have enough money to buy this!"
    sell-noobj-msg "Sorry, but I don't carry that item."
    buy-noobj-msg "You don't have that item!"
    buy-msg "Here you go."
    sell-msg "There you go."
    closed-msg "Come back later!"
    markup 120
    markdown 70
    currency 'gold
    revenue 0
    steal-ok-p nil
    attack-ok-p nil
    call-for-help-p nil
    subfunc nil
    reaction nil))

(define-condition specparam-error ()
  ((msg :accessor msg-of :initarg :msg)
   (lineno :accessor lineno-of :initarg :lineno)))

(defun shop-is-closed (self shop)
  (let ((time (local-time-of (zone-of (in-room-of self)))))
    (find-if (lambda (shop-hours)
               (<= (car shop-hours) time (cdr shop-hours)))
             (closed-hours-of shop))))

(defun vendor-will-deal-with (self ch shop)
  (cond
    ((not (is-visible-to ch self))
     (perform-say self "yell" "Show yourself if you want to do business with me!")
     nil)
    ((criminalp ch)
     (perform-say self "sneer" "I don't deal with CRIMINALS.")
     nil)
    ((shop-is-closed self shop)
     (perform-say self "say" (closed-msg-of shop))
     nil)
    ((not (funcall (reaction-of shop) ch))
     (perform-say self "say" (denied-msg-of shop))
     nil)
    (t
     ;; yeah, the vendor will deal with the person
     t)))

(defun vendor-get-value (obj percent modifier currency)
  "Returns the value of an OBJ, checking for buyability."
  (let ((cost (cost-of obj))
        (min-cost (if (= currency 2) 1 100)))
    ;; Adjust cost for wear and tear on a direct percentage basis
    (when (and (/= (damage-of obj) -1)
               (/= (max-dam-of obj) -1)
               (/= (max-dam-of obj) 0))
      (setf percent (/ (* percent (damage-of obj)) (max-dam-of obj))))

    ;; Adjust cost for missing charges
    (when (and (or (is-obj-kind obj +item-wand+)
                   (is-obj-kind obj +item-staff+))
               (/= (aref (value-of obj) 1) 0))
      (setf percent (/ (* percent (aref (value-of obj) 2)) (aref (value-of obj) 1))))

    ;; Other item flag modifiers
    (when (is-obj-stat2 obj +item2-reinforced+)
      (incf percent 25))
    (when (is-obj-stat2 obj +item2-reinforced+)
      (incf percent 25))

    (incf percent modifier)

    (max min-cost (floor (* cost percent) 100))))

(defun vendor-list-object (ch obj count index cost)
  (let ((obj-desc (format nil "~a~@[ of ~a~]~
                               ~:[~; (partially used)~]~
                               ~:[~; [reinforced]~]~
                               ~:[~; |augmented|~]~
                               ~:[~; <broken>~]~
                               ~:[~; (humming)~]~
                               ~:[~; (glowing)~]~
                               ~:[~; (invisible)~]~
                               ~:[~; (transparent)~]~
                               ~:[~; (holy aura)~]~
                               ~:[~; (unholy aura)~]"
                          (act-escape (string-capitalize (name-of obj) :end 1))
                          (when (and (is-obj-kind obj +item-drinkcon+)
                                     (/= (aref (value-of obj) 1) 0))
                            (aref +drink-names+ (aref (value-of obj) 2)))
                          (and (or (is-obj-kind obj +item-wand+)
                                   (is-obj-kind obj +item-staff+))
                               (< (aref (value-of obj) 2) (aref (value-of obj) 1)))
                          (is-obj-stat2 obj +item2-reinforced+)
                          (is-obj-stat2 obj +item2-enhanced+)
                          (is-obj-stat2 obj +item2-broken+)
                          (is-obj-stat obj +item-hum+)
                          (is-obj-stat obj +item-glow+)
                          (is-obj-stat obj +item-invisible+)
                          (is-obj-stat obj +item-transparent+)
                          (and (aff-flagged ch +aff-detect-align+) (is-obj-stat obj +item-bless+))
                          (and (aff-flagged ch +aff-detect-align+) (is-obj-stat obj +item-damned+)))))
    (if count
        (send-to-char ch " ~2d&r)  &y~5d&n       ~48a ~6d~%"
                      index
                      count
                      obj-desc
                      cost)
        (send-to-char ch " ~2d&r)  &gUnlimited&n   ~48a ~6d~%"
                      index
                      obj-desc
                      cost))))

(defun cost-modifier-of (ch seller)
  (* (- (cha-of seller) (cha-of ch)) 2))

(defun vendor-list-wares (self ch vars shop)
  (unless (carrying-of self)
    (perform-say-to self ch "I'm out of stock at the moment.")
    (return-from vendor-list-wares))

  (with-pagination ((link-of ch))
    ;; list header
    (send-to-char ch " ##   Available   Item                                       ~12@a~%"
                  (aref #("Gold" "Creds" "Quest Points") (currency-of shop)))
    (send-to-char ch "-------------------------------------------------------------------------~%")

    ;; list body
    (loop
       with index = 1
       for obj-sublist on (carrying-of self)
       as obj = (first obj-sublist)
       as next-obj = (second obj-sublist)
       as counter from 1
       do
         (when (or (null next-obj)
                   (not (same-obj next-obj obj)))
           (when (or (null vars)
                     (is-alias-of vars (aliases-of obj)))
             (vendor-list-object ch obj
                                 (unless (member (vnum-of obj) (produced-of shop)) counter)
                                 index
                                 (vendor-get-value obj (markup-of shop) (cost-modifier-of ch self) (currency-of shop)))
             (incf index))
           (setf counter 0)))
    (act ch :place-emit "$n peruses the shop's wares.")))

(defun perform-vendor-command (self shop ch command vars)
  (string-abbrev-case (string (first (command-info-pattern command)))
    ("steal"
     (cond
       ((or (steal-ok-p shop) (immortalp ch))
        nil)
       ((is-visible-to ch self)
        (do-shout-msg ch "Help, help!  An invisible thief is in my store!")
        t)
       (t
        (do-shout-msg ch (format nil "~a is a bloody thief!" (name-of ch)))
        t)))
    ("buy"
     (when (vendor-will-deal-with self ch shop)
       (vendor-make-sale self ch vars shop))
     t)
    ("sell"
     (when (vendor-will-deal-with self ch shop)
       (vendor-buy-from self ch vars shop))
     t)
    ("list"
     (when (vendor-will-deal-with self ch shop)
       (vendor-list-wares self ch vars shop))
     t)
    ("value"
     (when (vendor-will-deal-with self ch shop)
       (vendor-value-ware self ch vars shop))
     t)
    (t
     nil)))

(defun vendor-parse-param (text)
  (let ((shop (make-instance 'shop-data)))
    (handler-bind ((matcher-parse-error (lambda (e)
                     (signal 'specparam-error :msg (msg-of e) :lineno (lineno-of e)))))
      (setf (reaction-of shop) (make-creature-matcher text)))
    (with-input-from-string (str text)
      (loop
         for line = (read-line str nil)
         as lineno from 0
         while line
         unless (string= (string-trim " " line) "")
         do
           (with-words line (param-key &rest value)
             (string-abbrev-case param-key
               ("room"
                (setf (room-of shop) (parse-integer value :junk-allowed t)))
               ("produce"
                (let ((vnum (parse-integer value :junk-allowed t)))
                  (unless (real-object-proto vnum)
                    (signal 'specparam-error :msg "nonexistent produced item" :lineno lineno))
                  (push vnum (produced-of shop))))
               ("accept"
                (let ((kind (if (string= value "all")
                                0
                                (position value +item-kinds+ :test 'string-abbrev))))
                  (unless kind
                    (signal 'specparam-error :msg "an invalid accept line" :lineno lineno))
                  (push (cons kind t) (will-buy-of shop))))
               ("refuse"
                (let ((kind (if (string= value "all")
                                0
                                (position value +item-kinds+ :test 'string-abbrev))))
                  (unless kind
                    (signal 'specparam-error :msg "an invalid refuse line" :lineno lineno))
                  (push (cons kind nil) (will-buy-of shop))))
               ("denied-msg"
                (setf (denied-msg-of shop) value))
               ("keeper-broke-msg"
                (setf (selfbroke-msg-of shop) value))
               ("buyer-broke-msg"
                (setf (buyerbroke-msg-of shop) value))
               ("buy-msg"
                (setf (buy-msg-of shop) value))
               ("sell-msg"
                (setf (sell-msg-of shop) value))
               ("closed-msg"
                (setf (closed-msg-of shop) value))
               ("no-buy-msg"
                (setf (denied-msg-of shop) value))
               ("sell-noobj-msg"
                (setf (sell-noobj-msg-of shop) value))
               ("buy-noobj-msg"
                (setf (buy-noobj-msg-of shop) value))
               ("temper-cmd"
                (setf (temper-cmd-of shop) value))
               ("closed-hours"
                (with-words value (close-str open-str)
                  (let ((close-time (parse-integer close-str :junk-allowed t))
                        (open-time (parse-integer open-str :junk-allowed t)))
                    (unless (<= 0 close-time 23)
                      (signal 'specparam-error :msg "an out of bounds closing hour" :lineno lineno))
                    (unless (<= 0 open-time 23)
                      (signal 'specparam-error :msg "an out of bounds opening hour" :lineno lineno))
                    (push (cons close-time open-time) (closed-hours-of shop)))))
               ("markup"
                (setf (markup-of shop) (parse-integer value :junk-allowed t))
                (unless (<= 0 (markup-of shop) 1000)
                  (signal 'specparam-error :msg "an invalid markup" :lineno lineno)))
               ("markdown"
                (setf (markdown-of shop) (parse-integer value :junk-allowed t))
                (unless (<= 0 (markdown-of shop) 1000)
                  (signal 'specparam-error :msg "an invalid markdown" :lineno lineno)))
               ("revenue"
                (setf (revenue-of shop) (parse-integer value :junk-allowed t))
                (when (minusp (revenue-of shop))
                  (signal 'specparam-error :msg "a negative revenue" :lineno lineno)))
               ("currency"
                (let ((currency (assoc value '(("past" 0)
                                               ("gold" 0)
                                               ("future" 1)
                                               ("cash" 1)
                                               ("qp" 2)
                                               ("quest" 2))
                                       :test 'string-abbrev)))
                  (unless currency
                    (signal 'specparam-error :msg "invalid currency" :lineno lineno))
                  (setf (currency-of shop) (second currency))))
               ("steal-ok"
                (setf (steal-ok-p shop) (member value '("yes" "on" "1" "true") :test 'string-abbrev)))
               ("attack-ok"
                (setf (attack-ok-p shop) (member value '("yes" "on" "1" "true") :test 'string-abbrev)))
               ("call-for-help"
                (setf (call-for-help-p shop) (member value '("yes" "on" "1" "true") :test 'string-abbrev)))
               ("special"
                (let ((func (gethash value *special-funcs*)))
                  (unless func
                    (signal 'specparam-error :msg "invalid special" :lineno lineno))
                  (setf (func-of shop) func)))
               ("allow"
                ;; ignore - handled by creature matcher
                nil)
               ("deny"
                ;; ignore - handled by creature matcher
                nil)
               (t
                (signal 'specparam-error :msg "an invalid specparam line" :lineno lineno))))))
    shop))

(defun renew-vendor-revenue (self shop)
  (let* ((vkeeper (proto-of (shared-of self)))
         (max-money (slot-value vkeeper (currency-of shop)))
         (cur-money (slot-value self (currency-of shop))))
    (when (< cur-money max-money)
      (setf (slot-value self (currency-of shop))
            (min max-money (+ cur-money (revenue-of shop)))))))

(defcommand (ch "list") (:resting)
  (send-to-char ch "You can't do that here.~%"))
(defcommand (ch "list" args) (:resting)
  (declare (ignore args))
  (send-to-char ch "You can't do that here.~%"))

(define-special vendor (trigger self ch command vars) (+spec-mob+)
  (handler-bind ((specparam-error
                  (lambda (e)
                    (unless (or (null ch) (is-npc ch))
                      (cond
                        ((immortalp ch)
                         (perform-tell self ch (format nil "I have ~a in line ~d of my specparam."
                                                  (msg-of e) (lineno-of e))))
                        (t
                         (mudlog 'error t "ERR: Mobile ~d has ~a in line ~d of specparam"
                                 (vnum-of self)
                                 (msg-of e)
                                 (lineno-of e))
                         (perform-tell self ch "Sorry.  I'm broken, but a god has already been notified.")))))))
    (let ((shop (vendor-parse-param (func-param-of (shared-of self)))))
      (when shop
        (case trigger
          (command
           (perform-vendor-command self shop ch command vars))
          (reset
           (renew-vendor-revenue self shop))
          (tick
           (when (fighting-of self)
             (call-for-help self))))))))
