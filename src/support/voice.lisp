(in-package #:tempus)

(defparameter +voice-situations+
  '(taunt                    ; NPC remembers creature in room
    attack                   ; NPC attacking remembered creature
    panic                    ; NPC is running away from remembered
    hunt-found               ; Hunter found his prey
    hunt-lost                ; Hunter lost his prey
    hunt-gone                ; Hunter's prey disappeared
    hunt-taunt               ; Hunter is busy hunting
    hunt-unseen              ; Hunter found invisible prey
    hunt-openair             ; Hunter can't track over air
    hunt-water               ; Hunter can't track over water
    fight-winning            ; NPC is winning the fight
    fight-losing             ; NPC is losing the fight
    fight-helping            ; NPC is assisting another NPC
    obeying                  ; NPC is obeying a command
    ))

(defparameter +voice-none+ 0)
(defparameter +voice-mobile+ 1)
(defparameter +voice-animal+ 2)

(defclass voice ()
  ((idnum :accessor idnum-of :initarg :idnum)
   (name :accessor name-of :initarg :name)
   (emits :accessor emits-of :initform nil)))

(defvar *voices* (make-hash-table))

(defun voice-name (voice-id)
  (let ((voice (gethash voice-id *voices*)))
    (and voice (name-of voice))))

(defun voice-idnum-by-name (name)
  (alexandria:maphash-values
   (lambda (voice)
     (when (string-equal name (name-of voice))
       (return-from voice-idnum-by-name (idnum-of voice))))
   *voices*))

(defun voice-perform (voice ch target situation)
  (let ((emit (cdr (random-elt (remove situation (emits-of voice)
                                       :test-not #'eql
                                       :key #'first)))))
    (when emit
      (interpret-command ch (act-str ch emit ch target nil nil :self)))))

(defun emit-voice (ch target situation)
  (when (is-pc ch)
    (let* ((voice-id (or (voice-of ch)
                        (if (is-animal ch)
                            +voice-animal+
                            +voice-mobile+)))
           (voice (gethash voice-id *voices*)))
      (when voice
        (voice-perform voice ch target situation)))))

(defun show-voices (ch)
  (with-pagination ((link-of ch))
    (send-to-char ch "VOICES:~%")
    (dolist (idnum (sort (alexandria:hash-table-keys *voices*) #'<))
      (send-to-char ch "~8d &c~10a&n~%" idnum (name-of (gethash idnum *voices*))))))

(defun boot-voices ()
  (clrhash *voices*)
  (klacks:with-open-source (s (cxml:make-source (tempus-path "lib/etc/voices.xml")))
    (loop
       with new-voice = nil
       for event = (klacks:peek s)
       while event do
         (case event
           (:start-element
            (string-case (klacks:current-lname s)
              ("voices"
               nil)
              ("voice"
               (setf new-voice (make-instance 'voice))
               (klacks:map-attributes (lambda (ns name qname val explicit?)
                                        (declare (ignore ns qname explicit?))
                                        (string-case name
                                          ("idnum" (setf (idnum-of new-voice)
                                                         (parse-integer val)))
                                          ("name" (setf (name-of new-voice)
                                                        val))))
                                      s))
              (t
               (push (cons (intern (string-upcase (substitute #\- #\_ (klacks:current-lname s)))
                                   (find-package "TEMPUS"))
                           (nth-value 1 (klacks:find-event s :characters)))
                     (emits-of new-voice))))
            (klacks:consume s))
           (:end-element
            (when (string= (klacks:current-lname s) "voice")
              (setf (gethash (idnum-of new-voice) *voices*) new-voice))
            (klacks:consume s))
           (t
            (klacks:consume s))))))
