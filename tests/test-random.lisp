(in-package #:tempus.tests)

(def-suite :tempus)

(in-suite* #:tempus.make :in :tempus)

(test random-range
  (for-all ((a (gen-integer :min 0 :max 100))
            (b (gen-integer :min 0 :max 100)))
    (let ((min (min a b))
          (max (max a b)))
      (is (<= min (tempus::random-range min max) max))
      (is (= a (tempus::random-range a a))))))

(test dice
  (for-all ((num (gen-integer :min 1 :max 100))
            (size (gen-integer :min 1 :max 100)))
    (is (= (tempus::dice (- num) size) 0))
    (is (= (tempus::dice num (- size)) 0))
    (is (= (tempus::dice 0 size) 0))
    (is (= (tempus::dice num 0) 0))
    (let ((result (tempus::dice num size)))
      (is (<= num result (* num size))))))

(test rand-value
  (for-all ((variance (gen-integer :min 1 :max 100))
            (a (gen-integer :min 1 :max 100))
            (b (gen-integer :min 1 :max 100))
            (c (gen-integer :min 1 :max 100)))
    (let* ((min (min a b c))
           (max (max a b c))
           (val (cond
                  ((and (= min a) (= max b)) c)
                  ((and (= min b) (= max a)) c)
                  ((and (= min a) (= max c)) b)
                  ((and (= min b) (= max c)) b)
                  (t                         a)))
           (result (tempus::rand-value val variance min max)))
      (is (<= (abs (- result val)) variance))
      (is (<= min val max)))))