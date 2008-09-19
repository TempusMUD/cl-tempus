(in-package #:tempus.tests)

(in-suite* #:tempus.parser :in :tempus)

(test command-match
  (is (equal '(t nil)
             (tempus::command-matches (tempus::make-command-info
                                       :pattern '("look"))
                                      "look")))
  (is-false (tempus::command-matches (tempus::make-command-info
                                     :pattern '("look" "at" thing))
                                     "look at"))
  (is (equal '(t ("foo"))
             (tempus::command-matches (tempus::make-command-info
                                       :pattern '("look" "at" thing))
                                      "look at foo")))
  (is (equal '(t ("foo" "bar"))
             (tempus::command-matches (tempus::make-command-info
                                     :pattern '("get" thing "from" container))
                                     "get foo from bar"))))