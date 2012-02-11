(in-package #:tempus)

(defcommand (ch "balance") (:resting)
  (send-to-char ch "You can't do that here.~%"))
(defcommand (ch "balance" "clan") (:resting)
  (send-to-char ch "You can't do that here.~%"))
(defcommand (ch "withdraw") (:resting)
  (send-to-char ch "You can't do that here.~%"))
(defcommand (ch "withdraw" amount) (:resting)
  (declare (ignore amount))
  (send-to-char ch "You can't do that here.~%"))
(defcommand (ch "withdraw" "clan" amount) (:resting)
  (declare (ignore amount))
  (send-to-char ch "You can't do that here.~%"))
(defcommand (ch "deposit") (:resting)
  (send-to-char ch "You can't do that here.~%"))
(defcommand (ch "deposit" amount) (:resting)
  (declare (ignore amount))
  (send-to-char ch "You can't do that here.~%"))
(defcommand (ch "deposit" "clan" amount) (:resting)
  (declare (ignore amount))
  (send-to-char ch "You can't do that here.~%"))
(defcommand (ch "transfer") (:resting)
  (send-to-char ch "You can't do that here.~%"))
(defcommand (ch "transfer" amount player) (:resting)
  (declare (ignore amount player))
  (send-to-char ch "You can't do that here.~%"))
(defcommand (ch "transfer" "clan" amount player) (:resting)
  (declare (ignore amount player))
  (send-to-char ch "You can't do that here.~%"))

(defun show-clan-balance (ch clan futurep)
  (let ((balance (bank-of clan)))
    (if (zerop balance)
        (send-to-char ch "The clan currently has no money deposited.~%")
        (send-to-char ch "The current clan balance is ~d ~:[coin~;cred~]~2:*~p.~%"
                      balance
                      futurep))))

(defun withdraw-from-clan-bank (ch clan futurep amount-str)
  (if (plr-flagged ch +plr-clan-leader+)
      (let ((amount (if (string-equal amount-str "all")
                        (bank-of clan)
                        (parse-integer amount-str :junk-allowed t))))
        (cond
          ((and (string-equal amount-str "all")
                (zerop (bank-of clan)))
           (send-to-char ch "There's nothing there for you to withdraw!~%"))
          ((null amount)
           (send-to-char ch "You can give a number or \"all\" as an amount.~%"))
          ((minusp amount)
           (send-to-char ch "Ha ha.  Very funny.~%"))
          ((zerop amount)
           (send-to-char ch "That's not much of a withdrawal.~%"))
          ((aff-flagged ch +aff-charm+)
           (send-to-char ch "You can't do that while charmed!~%"))
          ((> amount (bank-of clan))
           (send-to-char ch "The clan doesn't have that much deposited.~%"))
          (t
           (decf (bank-of clan) amount)
           (postmodern:execute (:update 'clans
                                        :set 'bank (bank-of clan)
                                        :where (:= 'idnum (idnum-of clan))))
           (if futurep
               (incf (cash-of ch) amount)
               (incf (gold-of ch) amount))
           (save-player-to-xml ch)
           (send-to-char ch "You withdraw ~d ~:[coin~;cred~]~2:*~p.~%"
                         amount
                         futurep)
           (show-clan-balance ch clan futurep)
           (act ch :place-emit "$n makes a bank transaction."))))
      (send-to-char ch "You can't do that.~%")))

(defun transfer-from-clan-bank (ch clan futurep amount-str player-name)
  (if (plr-flagged ch +plr-clan-leader+)
      (let* ((amount (if (string-equal amount-str "all")
                         (bank-of clan)
                         (parse-integer amount-str :junk-allowed t)))
             (player-id (retrieve-player-idnum player-name))
             (vict-name (and player-id
                             (retrieve-player-name player-id)))
             (account (and player-id
                           (account-by-idnum (retrieve-player-account player-id)))))
        (cond
          ((and (string-equal amount-str "all")
                (zerop (bank-of clan)))
           (send-to-char ch "There's nothing there for you to transfer!~%"))
          ((null amount)
           (send-to-char ch "You can give a number or \"all\" as an amount.~%"))
          ((minusp amount)
           (send-to-char ch "Ha ha.  Very funny.~%"))
          ((zerop amount)
           (send-to-char ch "That's not much of a transfer.~%"))
          ((aff-flagged ch +aff-charm+)
           (send-to-char ch "You can't do that while charmed!~%"))
          ((> amount (bank-of clan))
           (send-to-char ch "The clan doesn't have that much deposited.~%"))
          ((null player-id)
           (send-to-char ch "You can't transfer money to someone who doesn't exist!~%"))
          (t
           (decf (bank-of clan) amount)
           (postmodern:execute (:update 'clans
                                        :set 'bank (bank-of clan)
                                        :where (:= 'idnum (idnum-of clan))))
           (if futurep
               (deposit-future-bank account amount)
               (deposit-past-bank account amount))
           (send-to-char ch "You transfer ~d ~:[coin~;cred~]~2:*~p to ~*~a's account.~%"
                         amount
                         futurep
                         vict-name)
           (show-clan-balance ch clan futurep)
           (act ch :place-emit "$n makes a bank transaction."))))
      (send-to-char ch "You can't do that.~%")))

(defun deposit-to-clan-bank (ch clan futurep amount-str)
  (let* ((money (if futurep
                    (cash-of ch)
                    (gold-of ch)))
         (amount (if (string-equal amount-str "all")
                     money
                     (parse-integer amount-str :junk-allowed t))))
    (cond
      ((and (string-equal amount-str "all")
            (zerop amount))
       (send-to-char ch "You don't have any ~:[coins~;creds~] to deposit!~%" futurep))
      ((null amount)
       (send-to-char ch "You can give a number or \"all\" as an amount.~%"))
      ((minusp amount)
       (send-to-char ch "Ha ha.  Very funny.~%"))
      ((zerop amount)
       (send-to-char ch "That's not much of a deposit.~%"))
      ((> amount money)
       (send-to-char ch "You don't have that many ~:[coins~;creds~]!~%" futurep))
      (t
       (if futurep
           (decf (cash-of ch) amount)
           (decf (gold-of ch) amount))
       (save-player-to-xml ch)
       (incf (bank-of clan) amount)
       (postmodern:execute (:update 'clans
                                    :set 'bank (bank-of clan)
                                    :where (:= 'idnum (idnum-of clan))))
       (send-to-char ch "You deposit ~d ~:[coin~;cred~]~2:*~p.~%"
                     amount
                     futurep)
       (show-clan-balance ch clan futurep)
       (act ch :place-emit "$n makes a bank transaction.")))))

(defun show-bank-balance (ch futurep)
  (let ((balance (if futurep
                     (future-bank-of (account-of ch))
                     (past-bank-of (account-of ch)))))
    (if (zerop balance)
        (send-to-char ch "You currently have no money deposited.~%")
        (send-to-char ch "Your current balance is ~d ~:[coin~;cred~]~2:*~p.~%"
                      balance
                      futurep))))

(defun withdraw-from-bank (ch futurep amount-str)
  (let* ((money (if futurep
                    (future-bank-of (account-of ch))
                    (past-bank-of (account-of ch))))
         (amount (if (string-equal amount-str "all")
                     money
                     (parse-integer amount-str :junk-allowed t))))
    (cond
      ((and (string-equal amount-str "all")
            (zerop money))
       (send-to-char ch "There's nothing there for you to withdraw!~%"))
      ((null amount)
       (send-to-char ch "You can give a number or \"all\" as an amount.~%"))
      ((minusp amount)
       (send-to-char ch "Ha ha.  Very funny.~%"))
      ((zerop amount)
       (send-to-char ch "That's not much of a withdrawal.~%"))
      ((aff-flagged ch +aff-charm+)
       (send-to-char ch "You can't do that while charmed!~%"))
      ((> amount money)
       (send-to-char ch "You don't have that much deposited.~%"))
      (t
       (cond
         (futurep
          (withdraw-future-bank (account-of ch) amount)
          (incf (cash-of ch) amount))
         (t
          (withdraw-past-bank (account-of ch) amount)
          (incf (gold-of ch) amount)))
       (save-player-to-xml ch)
       (send-to-char ch "You withdraw ~d ~:[coin~;cred~]~2:*~p.~%"
                     amount
                     futurep)
       (show-bank-balance ch futurep)
       (act ch :place-emit "$n makes a bank transaction.")))))

(defun transfer-from-bank (ch futurep amount-str player-name)
  (let* ((money (if futurep
                    (future-bank-of (account-of ch))
                    (past-bank-of (account-of ch))))
         (amount (if (string-equal amount-str "all")
                     money
                     (parse-integer amount-str :junk-allowed t)))
         (player-id (retrieve-player-idnum player-name))
         (vict-name (and player-id
                         (retrieve-player-name player-id)))
         (account (and player-id
                       (account-by-idnum (retrieve-player-account player-id)))))
    (cond
      ((and (string-equal amount-str "all")
            (zerop money))
       (send-to-char ch "There's nothing there for you to transfer!~%"))
      ((null amount)
       (send-to-char ch "You can give a number or \"all\" as an amount.~%"))
      ((minusp amount)
       (send-to-char ch "Ha ha.  Very funny.~%"))
      ((zerop amount)
       (send-to-char ch "That's not much of a transfer.~%"))
      ((aff-flagged ch +aff-charm+)
       (send-to-char ch "You can't do that while charmed!~%"))
      ((> amount money)
       (send-to-char ch "You don't have that much deposited.~%"))
      ((null player-id)
       (send-to-char ch "You can't transfer money to someone who doesn't exist!~%"))
      ((= (idnum-of (account-of ch)) (idnum-of account))
       (send-to-char ch "Transferring money to your own account?  Odd...~%"))
      (t
       (cond
         (futurep
          (withdraw-future-bank (account-of ch) amount)
          (deposit-future-bank account amount))
         (t
          (withdraw-past-bank (account-of ch) amount)
          (deposit-past-bank account amount)))
       (send-to-char ch "You transfer ~d ~:[coin~;cred~]~2:*~p to ~*~a's account.~%"
                     amount
                     futurep
                     vict-name)
       (show-bank-balance ch futurep)
       (act ch :place-emit "$n makes a bank transaction.")))))

(defun deposit-to-bank (ch futurep amount-str)
  (let* ((money (if futurep
                    (cash-of ch)
                    (gold-of ch)))
         (amount (if (string-equal amount-str "all")
                     money
                     (parse-integer amount-str :junk-allowed t))))
    (cond
      ((and (string-equal amount-str "all")
            (zerop amount))
       (send-to-char ch "You don't have any ~:[coins~;creds~] to deposit ~a ~a!~%" futurep money amount))
      ((null amount)
       (send-to-char ch "You can give a number or \"all\" as an amount.~%"))
      ((minusp amount)
       (send-to-char ch "Ha ha.  Very funny.~%"))
      ((zerop amount)
       (send-to-char ch "That's not much of a deposit.~%"))
      ((> amount money)
       (send-to-char ch "You don't have that many ~:[coins~;creds~]!~%" futurep))
      (t
       (cond
         (futurep
          (decf (cash-of ch) amount)
          (deposit-future-bank (account-of ch) amount))
         (t
          (decf (gold-of ch) amount)
          (deposit-past-bank (account-of ch) amount)))
       (save-player-to-xml ch)
       (send-to-char ch "You deposit ~d ~:[coins~;creds~]~2:*~p.~%"
                     amount
                     futurep)
       (show-bank-balance ch futurep)
       (act ch :place-emit "$n makes a bank transaction.")))))

(define-special bank (trigger self ch command vars) (+spec-mob+ +spec-obj+ +spec-rm+)
  (when (and (eql trigger 'command)
             (member (first (command-info-pattern command))
                     '("balance" "withdraw" "deposit" "transfer")
                     :test #'string=))
    (let* ((cmd (command-info-pattern command))
           (futurep (eql (time-frame-of (zone-of (in-room-of self)))
                         +time-future+))
           (amount-str (first vars))
           (clanp (equal "clan" (second cmd))))
      (cond
        ((aff-flagged ch +aff-charm+)
         (send-to-char ch "You can't do that while charmed!~%"))
        (clanp
         (let ((clan (real-clan (clan-of ch))))
           (if clan
               (string-case (first (command-info-pattern command))
                 ("balance"
                  (show-clan-balance ch clan futurep))
                 ("withdraw"
                  (if (null amount-str)
                      (send-to-char ch "How much do you want to withdraw?~%")
                      (withdraw-from-clan-bank ch clan futurep amount-str)))
                 ("deposit"
                  (if (null amount-str)
                      (send-to-char ch "How much do you want to deposit?~%")
                      (deposit-to-clan-bank ch clan futurep amount-str)))
                 ("transfer"
                  (cond
                    ((null amount-str)
                     (send-to-char ch "How much do you want to transfer?~%"))
                    ((null (second vars))
                     (send-to-char ch "To whom do you wish to transfer money?~%"))
                    (t
                     (transfer-from-clan-bank ch clan futurep amount-str (second vars))))))
               (send-to-char ch "You can't do that.~%"))))
        (t
               (string-case (first (command-info-pattern command))
                 ("balance"
                  (show-bank-balance ch futurep))
                 ("withdraw"
                  (if (null amount-str)
                      (send-to-char ch "How much do you want to withdraw?~%")
                      (withdraw-from-bank ch futurep amount-str)))
                 ("deposit"
                  (if (null amount-str)
                      (send-to-char ch "How much do you want to deposit?~%")
                      (deposit-to-bank ch futurep amount-str)))
                 ("transfer"
                  (cond
                    ((null amount-str)
                     (send-to-char ch "How much do you want to transfer?~%"))
                    ((null (second vars))
                     (send-to-char ch "To whom do you wish to transfer money?~%"))
                    (t
                     (transfer-from-bank ch futurep amount-str (second vars))))))))
      t)))