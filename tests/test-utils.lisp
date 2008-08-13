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

      
