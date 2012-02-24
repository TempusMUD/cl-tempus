(in-package #:tempus)


(defcommand (ch "backstab") (:standing)
  (send-to-char ch "Backstab who?")
  (wait-state ch 4))

(defcommand (ch "backstab" name) (:standing)
  (perform-backstab ch (get-char-room-vis ch name)))

(defcommand (ch "bash") (:standing)
  (let ((vict (random-elt (fighting-of ch))))
    (if vict
        (perform-offensive-skill ch vict +skill-bash+)
        (send-to-char ch "What is it you want to bash?~%"))))

(defcommand (ch "bash" name) (:standing)
  (let ((target (resolve-alias ch name
                               (append (people-of (in-room-of ch))
                                       (contents-of (in-room-of ch))))))
    (cond
      ((null target)
       (let ((dir (find-door ch name "bash")))
         (if dir
             (perform-bash-door ch dir)
             (wait-state ch 4))))
      ((cdr target)
       (send-to-char ch "You can only bash one thing at a time!~%"))
      ((typep (first target) 'obj-data)
       (act ch :item target
            :subject-emit "You bash $p!"
            :place-emit "$n bashes $p!")
       (when (is-obj-kind target +item-vehicle+)
         (let ((room (real-room (room-number target))))
           (when (and room (people-of room))
             (act (first (people-of room)) :target ch :item target
                  :subject-emit "$N bashes the outside of $p!"
                  :place-emit "$N bashes the outside of $p!")))))
      (t
       (perform-offensive-skill ch (first target) +skill-bash+)))))

(defun do-offensive-skill (ch name skill)
  (let* ((targets (if name
                      (resolve-alias ch name
                                     (append (people-of (in-room-of ch))
                                             (contents-of (in-room-of ch))))
                      (list (random-elt (fighting-of ch)))))
         (target (first targets))
         (skill-name (name-of (aref *spell-info* skill))))

    (cond
      ((null targets)
       (send-to-char ch "~a who?~%" skill-name))
      ((rest targets)
       (send-to-char ch "You can only ~a one thing at a time!~%" skill-name))
      ((typep target 'obj-data)
       (act ch :item target
            :all-emit (format nil "$n fiercely ~a$% $p!" skill-name)))
      ((eql ch target)
       (send-to-char ch "Aren't we funny today...~%"))
      (t
       (perform-offensive-skill ch target skill)))))

(defmacro define-offensive-skill (name &optional skill-sym)
  (let ((name-str (format nil "~(~a~)" name))
        (skill-sym (or skill-sym (intern (format nil "+SKILL-~a+" name)))))
  `(progn
     (defcommand (ch ,name-str) (:standing)
       (do-offensive-skill ch nil ,skill-sym))
     (defcommand (ch ,name-str name)
         (:standing)
       (do-offensive-skill ch name ,skill-sym)))))

(define-offensive-skill bearhug)
(define-offensive-skill behead)
(define-offensive-skill bite)
(define-offensive-skill bodyslam)
(define-offensive-skill choke)
(define-offensive-skill claw)
(define-offensive-skill clothesline)
(define-offensive-skill cranekick +skill-crane-kick+)
(define-offensive-skill deathtouch +skill-death-touch+)
(define-offensive-skill elbow)
(define-offensive-skill garotte)
(define-offensive-skill gouge)
(define-offensive-skill groinkick)
(define-offensive-skill headbutt)
(define-offensive-skill hiptoss +skill-hip-toss+)
(define-offensive-skill hook)
(define-offensive-skill jab)
(define-offensive-skill kick)
(define-offensive-skill kneethrust +skill-knee+)
(define-offensive-skill lungepunch +skill-lunge-punch+)
(define-offensive-skill palmstrike +skill-palm-strike+)
(define-offensive-skill pelekick +skill-pele-kick+)
(define-offensive-skill piledrive)
(define-offensive-skill psiblast)
(define-offensive-skill punch)
(define-offensive-skill rabbitpunch)
(define-offensive-skill ridgehand)
(define-offensive-skill roundhouse)
(define-offensive-skill scissorkick +skill-scissor-kick+)
(define-offensive-skill scream)
(define-offensive-skill shoulderthrow +skill-shoulder-throw+)
(define-offensive-skill sidekick)
(define-offensive-skill spinfist)
(define-offensive-skill spinkick)
(define-offensive-skill strike)
(define-offensive-skill stomp)
(define-offensive-skill sweepkick)
(define-offensive-skill throatstrike +skill-throat-strike+)
(define-offensive-skill trip)
(define-offensive-skill uppercut)
