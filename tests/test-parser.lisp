(in-package #:tempus.tests)

(in-suite (defsuite (tempus.parser :in test)))

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