(in-package #:tempus.tests)

(in-suite (defsuite (tempus.misc :in test)))

(tempus::enable-regex-reader-syntax)

(deftest feedback-command ()
  (let ((path (tempus::tempus-path "lib/misc/ideas"))
        (real-path (tempus::tempus-path "lib/misc/ideas.real")))
    (with-mock-players (alice)
      (unwind-protect
           (progn
             (when (probe-file path)
               (rename-file path real-path))
             (tempus::interpret-command alice "idea I have an idea!")
             (is (equal "Your idea has been recorded.  Thanks!~%" (char-output alice)))
             (with-open-file (inf path)
               (loop
                  for read-line = (read-line inf nil nil)
                  as last-line = (if read-line read-line last-line)
                  while read-line
                  finally (is (cl-ppcre:scan #/^Alice    \(... ..\) \[ 3002\] I have an idea!$/ last-line)))))
        (when (probe-file real-path)
          (rename-file real-path path))))))

(deftest find-visible-group-members/no-group/returns-nil ()
  (with-mock-players (alice)
    (is (null (tempus::find-visible-group-members alice)))))

(deftest find-visible-group-members/master-of-group/returns-followers ()
  (with-mock-players (alice bob chuck)
    (setf (tempus::master-of bob) alice)
    (setf (tempus::master-of chuck) alice)
    (setf (tempus::followers-of alice) (list bob chuck))
    (setf (tempus::aff-flags-of alice) tempus::+aff-group+)
    (setf (tempus::aff-flags-of bob) tempus::+aff-group+)
    (setf (tempus::aff-flags-of chuck) tempus::+aff-group+)
    (is (equal (list bob chuck) (tempus::find-visible-group-members alice)))))

(deftest find-visible-group-members/not-master-of-group/returns-groupies ()
  (with-mock-players (alice bob chuck)
    (setf (tempus::master-of bob) alice)
    (setf (tempus::master-of chuck) alice)
    (setf (tempus::followers-of alice) (list bob chuck))
    (setf (tempus::aff-flags-of alice) tempus::+aff-group+)
    (setf (tempus::aff-flags-of bob) tempus::+aff-group+)
    (setf (tempus::aff-flags-of chuck) tempus::+aff-group+)
    (is (equal (list alice chuck) (tempus::find-visible-group-members bob)))))

(deftest perform-split/no-group/error-message ()
  (with-mock-players (alice)
    (setf (tempus::gold-of alice) 1000)
    (tempus::perform-split alice 1000 :gold)
    (is (equal "You aren't in a group.~%" (char-output alice)))))

(deftest perform-split/not-enough-gold/error-message ()
  (with-mock-players (alice)
    (tempus::perform-split alice 1000 :gold)
    (is (equal "You don't seem to have that much gold.~%" (char-output alice)))))

(deftest perform-split/not-enough-cash/error-message ()
  (with-mock-players (alice)
    (tempus::perform-split alice 1000 :cash)
    (is (equal "You don't seem to have that many credits.~%" (char-output alice)))))

(deftest perform-split/with-gold/splits-gold ()
  (with-mock-players (alice bob)
    (setf (tempus::gold-of alice) 1000)
    (setf (tempus::master-of bob) alice)
    (setf (tempus::followers-of alice) (list bob))
    (setf (tempus::aff-flags-of alice) tempus::+aff-group+)
    (setf (tempus::aff-flags-of bob) tempus::+aff-group+)
    (tempus::perform-split alice 1000 :gold)
    (is (equal "You split 1000 coins among 2 members -- 500 each.~%" (char-output alice)))
    (is (equal "Alice splits 1000 coins; you receive 500.~%" (char-output bob)))
    (is (= 500 (tempus::gold-of alice)))
    (is (= 500 (tempus::gold-of bob)))))

(deftest do-color/no-arg/shows-current-setting ()
  (with-mock-players (alice)
    (setf (tempus::ansi-level-of (tempus::account-of alice)) 0)
    (tempus::interpret-command alice "color")
    (is (equal "Your current color level is none.~%" (char-output alice)))))

(deftest do-color/with-arg/changes-setting ()
  (with-mock-players (alice)
    (setf (tempus::ansi-level-of (tempus::account-of alice)) 0)
    (tempus::interpret-command alice "color complete")
    (is (equal "Your color is now &Ycomplete&n.~%" (char-output alice)))
    (is (= (tempus::ansi-level-of (tempus::account-of alice)) 3))))

(deftest do-compact/no-arg/shows-current-setting ()
  (with-mock-players (alice)
    (setf (tempus::compact-level-of (tempus::account-of alice)) 0)
    (tempus::interpret-command alice "compact")
    (is (equal "Your current compact level is off.~%" (char-output alice)))))

(deftest do-compact/with-arg/changes-setting ()
  (with-mock-players (alice)
    (setf (tempus::compact-level-of (tempus::account-of alice)) 0)
    (tempus::interpret-command alice "compact full")
    (is (equal "Your &rcompact setting&n is now &Yfull&n.~%" (char-output alice)))
    (is (= (tempus::compact-level-of (tempus::account-of alice)) 3))))

(deftest do-mortalize/not-mortalized/mortalize ()
  (with-mock-players (alice)
    (with-captured-log log
        (tempus::interpret-command alice "mortalize")
      (is (search "(GC): Alice has mortalized at 3002" log))
      (is (equal "Other gods may now kill you...  if you don't watch it.~%"
                 (char-output alice)))
      (is (logtest (tempus::plr-bits-of alice) tempus::+plr-mortalized+)))))

(deftest do-mortalize/mortalized/immortalize ()
  (with-mock-players (alice)
    (setf (tempus::plr-bits-of alice) tempus::+plr-mortalized+)
    (with-captured-log log
        (tempus::interpret-command alice "mortalize")
      (is (search "(GC): Alice has immortalized at 3002" log))
      (is (equal "You resume your immortal status.~%" (char-output alice)))
      (is (not (logtest (tempus::plr-bits-of alice)
                        tempus::+plr-mortalized+))))))

(deftest do-display-vnums/normal/turns-on-vnum-bit ()
  (with-mock-players (alice)
    (tempus::interpret-command alice "display vnums")
    (is (equal "You will now see vnums on mobs and object ldescs.~%"
               (char-output alice)))
      (is (tempus::pref-flagged alice tempus::+pref-disp-vnums+))))

(deftest do-display-all/normal/turns-on-all-disp-bits ()
  (with-mock-players (alice)
    (tempus::interpret-command alice "display all")
    (is (equal "You got it.~%" (char-output alice)))
    (is (tempus::pref-flagged alice tempus::+pref-disphp+))
    (is (tempus::pref-flagged alice tempus::+pref-dispmana+))
    (is (tempus::pref-flagged alice tempus::+pref-dispmove+))
    (is (tempus::pref-flagged alice tempus::+pref-dispalign+))
    (is (tempus::pref-flagged alice tempus::+pref-disptime+))))

(deftest do-display-normal/normal/turns-on-normal-disp-bits ()
  (with-mock-players (alice)
    (tempus::interpret-command alice "display normal")
    (is (equal "You got it.~%" (char-output alice)))
    (is (tempus::pref-flagged alice tempus::+pref-disphp+))
    (is (tempus::pref-flagged alice tempus::+pref-dispmana+))
    (is (tempus::pref-flagged alice tempus::+pref-dispmove+))
    (is (not (tempus::pref-flagged alice tempus::+pref-dispalign+)))
    (is (not (tempus::pref-flagged alice tempus::+pref-disptime+)))))