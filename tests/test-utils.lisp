(in-package #:tempus.tests)

(in-suite (defsuite (tempus.util :in test)))

(deftest isname ()
  (is (tempus::is-name "foo" "foo bar baz"))
  (is (tempus::is-name "bar" "foo bar baz"))
  (is (tempus::is-name "baz" "foo bar baz"))
  (is (tempus::is-name "ba" "foo bar baz"))
  (is (tempus::is-name "f" "foo bar baz"))
  (is (not (tempus::is-name "quux" "foo bar baz"))))

(deftest pin ()
  (is (= 50 (tempus::pin 50 10 100)))
  (is (= 10 (tempus::pin 0 10 100)))
  (is (= 100 (tempus::pin 200 10 100))))

(deftest act-str ()
  (with-mock-players (alice)
    (is (equal "Alice says, 'checking $ in languages'"
               (tempus::act-str alice
                        (format nil "Alice says, '$[~a]'"
                                (tempus::act-escape "checking $ in languages"))
                        alice nil nil nil :self)))
    (is (equal "Alice nods solemnly."
               (tempus::act-str alice "Alice nods${ solemnly}."
                                alice nil nil nil :self)))
    (setf (tempus::mood-of alice) "sagely")
    (is (equal "Alice nods sagely."
               (tempus::act-str alice "Alice nods${ solemnly}."
                                alice nil nil nil :self)))))

(deftest first-word ()
      (is (equal "first" (tempus::first-word "first second third fourth")))
      (is (equal "first" (tempus::first-word "first")))
      (is (equal "" (tempus::first-word ""))))

(deftest string-abbrev ()
  (is (tempus::string-abbrev "foo" "foobar"))
  (is (tempus::string-abbrev "foobar" "foobar"))
  (is (not (tempus::string-abbrev "" "foobar")))
  (is (not (tempus::string-abbrev "bar" "foobar"))))

(deftest string-replace ()
      (is (equal "cccc" (tempus::string-replace "ab" "abc" "ccc")))
      (is (equal "cccc" (tempus::string-replace "ba" "cba" "ccc")))
      (is (equal "" (tempus::string-replace "foo" "" "bar")))
      (is (equal "foo" (tempus::string-replace "bar" "foobar" "")))
      (is (equal "barbar" (tempus::string-replace "foo" "foobar" "bar")))
      (is (equal "bar" (tempus::string-replace "foo" "foobar" "")))
      (is (equal "foo" (tempus::string-replace "foobar" "foo" "foo"))))

(deftest get-number ()
  (is (equal '(1 "foo") (multiple-value-list (tempus::get-number "foo"))))
  (is (equal '(1 "foo") (multiple-value-list (tempus::get-number "1.foo"))))
  (is (equal '(3 "foo") (multiple-value-list (tempus::get-number "3.foo"))))
  (is (equal '(nil) (multiple-value-list (tempus::get-number "0.foo"))))
  (is (equal '(nil) (multiple-value-list (tempus::get-number "-5.foo")))))

(deftest get-matching-objects ()
  (with-mock-players (alice)
    (let ((objs (mapcar 'make-mock-object
                        '("armor" "armor" "armor" "book" "book"
                          "candle" "doublet"))))
      (is (equal objs (tempus::get-matching-objects alice "all" objs)))
      (is (equal (subseq objs 0 3) (tempus::get-matching-objects alice "all.armor" objs)))
      (is (equal (subseq objs 3 5) (tempus::get-matching-objects alice "all.book" objs)))
      (is (equal (subseq objs 6 7) (tempus::get-matching-objects alice "all.doublet" objs)))
      (is (equal (subseq objs 0 1) (tempus::get-matching-objects alice "armor" objs)))
      (is (equal (subseq objs 1 2) (tempus::get-matching-objects alice "2.armor" objs)))
      (is (equal (subseq objs 2 3) (tempus::get-matching-objects alice "3.armor" objs))))))

(deftest get-obj-in-list-vis ()
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
      (is (null (tempus::get-obj-in-list-vis alice "torch" objs)))
      (is (null (tempus::get-obj-in-list-vis alice "2.candle" objs)))
      (is (null (tempus::get-obj-in-list-vis alice "0.armor" objs)))
      (is (null (tempus::get-obj-in-list-vis alice "-32.candle" objs))))))

(deftest find-all-dots ()
  (is (eql :find-all (tempus::find-all-dots "all")))
  (is (eql :find-alldot (tempus::find-all-dots "all.foo")))
  (is (eql :find-indiv (tempus::find-all-dots "foo"))))
