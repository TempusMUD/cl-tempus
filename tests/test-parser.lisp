(in-package #:tempus.tests)

(defsuite (tempus.parser :in test))
(in-suite tempus.parser)

(deftest command-match ()
  (is (equal '(t nil)
             (tempus::command-pattern-matches '("look") "look")))
  (is (null (tempus::command-pattern-matches '("look" "at" thing) "look at")))
  (is (equal '(t ("foo"))
             (tempus::command-pattern-matches '("look" "at" thing) "look at foo")))
  (is (equal '(t ("foo" "bar"))
             (tempus::command-pattern-matches '("get" thing "from" container)
                                     "get foo from bar")))
  (is (null (tempus::command-pattern-matches
             '("dynedit" "show" filename "new")
             "dynedit show news")))
  (is (equal '(t ("news")) (tempus::command-pattern-matches
             '("dynedit" "show" filename "new")
             "dynedit show news new"))))

(deftest expand-single-alias ()
  (is (equal "\\nod bob" (tempus::expand-single-alias "nod$*" "bob" '("bob"))))
  (is (equal "\\nod" (tempus::expand-single-alias "nod$*" "" '())))
  (is (equal "\\olc exit north remove"
             (tempus::expand-single-alias "olc exit$1 remove" "north" '("north"))))
  (is (equal "\\give foo to bar"
             (tempus::expand-single-alias "give$1 to$2" "foo bar" '("foo" "bar"))))
  (is (equal "\\give foo to foo" (tempus::expand-single-alias "give$1 to$1" "foo" '("foo")))))