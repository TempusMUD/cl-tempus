(in-package #:tempus.tests)

(in-suite (defsuite (tempus.parser :in test)))

(deftest command-match ()
  (is (equal '(t nil)
             (tempus::command-pattern-matches (tempus::make-command-info
                                       :pattern '("look"))
                                      "look")))
  (is (null (tempus::command-pattern-matches (tempus::make-command-info
                                      :pattern '("look" "at" thing))
                                     "look at")))
  (is (equal '(t ("foo"))
             (tempus::command-pattern-matches (tempus::make-command-info
                                       :pattern '("look" "at" thing))
                                      "look at foo")))
  (is (equal '(t ("foo" "bar"))
             (tempus::command-pattern-matches (tempus::make-command-info
                                     :pattern '("get" thing "from" container))
                                     "get foo from bar"))))