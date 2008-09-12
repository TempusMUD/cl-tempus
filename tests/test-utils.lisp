(in-package #:tempus.tests)

(in-suite* #:tempus.util :in :tempus)

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