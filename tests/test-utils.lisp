(in-package #:tempus.tests)

(in-suite* #:tempus.util :in :tempus)

(test isname
  (is-true (tempus::is-name "foo" "foo bar baz"))
  (is-true (tempus::is-name "bar" "foo bar baz"))
  (is-true (tempus::is-name "baz" "foo bar baz"))
  (is-true (tempus::is-name "ba" "foo bar baz"))
  (is-true (tempus::is-name "f" "foo bar baz"))
  (is-false (tempus::is-name "quux" "foo bar baz")))

(test pin
  (for-all ((a (gen-integer))
            (b (gen-integer))
            (c (gen-integer)))
    (let* ((sorted (sort (list a b c) #'<))
           (min (first sorted))
           (middle (second sorted))
           (max (third sorted))
           (result (tempus::pin middle min max)))
      (is (<= min result max))
      (is-true (or (= min middle)
                   (= result middle)))
      (is-true (or (= max middle)
                   (= result middle))))))

(test act-str
  (with-mock-players (alice)
    (is (equal "Alice says, 'checking $ in languages'"
               (tempus::act-str alice
                        (format nil "Alice says, '$[~a]'"
                                (tempus::act-escape "checking $ in languages"))
                        alice nil nil :self)))
    (is (equal "Alice nods solemnly."
               (tempus::act-str alice "Alice nods${ solemnly}."
                                alice nil nil :self)))
    (setf (tempus::mood-of alice) "sagely")
    (is (equal "Alice nods sagely."
               (tempus::act-str alice "Alice nods${ solemnly}."
                                alice nil nil :self)))))

(test first-word
      (is (equal "first" (tempus::first-word "first second third fourth")))
      (is (equal "first" (tempus::first-word "first")))
      (is (equal "" (tempus::first-word ""))))

(test string-replace
      (is (equal "cccc" (tempus::string-replace "ab" "abc" "ccc")))
      (is (equal "cccc" (tempus::string-replace "ba" "cba" "ccc")))
      (is (equal "" (tempus::string-replace "foo" "" "bar")))
      (is (equal "foo" (tempus::string-replace "bar" "foobar" "")))
      (is (equal "barbar" (tempus::string-replace "foo" "foobar" "bar")))
      (is (equal "bar" (tempus::string-replace "foo" "foobar" "")))
      (is (equal "foo" (tempus::string-replace "foobar" "foo" "foo"))))

(test get-number
  (is (equal '(1 "foo") (multiple-value-list (tempus::get-number "foo"))))
  (is (equal '(1 "foo") (multiple-value-list (tempus::get-number "1.foo"))))
  (is (equal '(3 "foo") (multiple-value-list (tempus::get-number "3.foo"))))
  (is (equal '(nil) (multiple-value-list (tempus::get-number "0.foo"))))
  (is (equal '(nil) (multiple-value-list (tempus::get-number "-5.foo")))))

(test get-obj-in-list-vis
  (with-mock-players (alice)
    (let ((objs (loop
                   for name in '("armor" "armor" "armor" "book" "book"
                                 "candle" "doublet")
                   collect (make-mock-object name))))
      (is (eql (first objs) (tempus::get-obj-in-list-vis alice "armor" objs)))
      (is (eql (second objs) (tempus::get-obj-in-list-vis alice "2.armor" objs)))
      (is (eql (third objs) (tempus::get-obj-in-list-vis alice "3.armor" objs)))
      (is (eql (fourth objs) (tempus::get-obj-in-list-vis alice "1.book" objs)))
      (is (eql (fifth objs) (tempus::get-obj-in-list-vis alice "2.book" objs)))
      (is (eql (sixth objs) (tempus::get-obj-in-list-vis alice "candle" objs)))
      (is (eql (seventh objs) (tempus::get-obj-in-list-vis alice "doublet" objs)))
      (is-false (tempus::get-obj-in-list-vis alice "torch" objs))
      (is-false (tempus::get-obj-in-list-vis alice "2.candle" objs))
      (is-false (tempus::get-obj-in-list-vis alice "0.armor" objs))
      (is-false (tempus::get-obj-in-list-vis alice "-32.candle" objs)))))

(test find-all-dots
  (is (eql :find-all (tempus::find-all-dots "all")))
  (is (eql :find-alldot (tempus::find-all-dots "all.foo")))
  (is (eql :find-indiv (tempus::find-all-dots "foo"))))
