(in-package #:borba-cad/tests)

(def-suite prng
  :description "The seeded SplitMix64 generator used by tests and benchmarks."
  :in all)

(in-suite prng)

(test splitmix64-matches-the-published-reference-sequence
  ;; Reference outputs of SplitMix64 seeded with 0 (Vigna's reference implementation).
  (let ((prng (borba-cad/test-support:make-prng 0)))
    (is (= #xE220A8397B1DCDAF (borba-cad/test-support:prng-next-integer-64 prng)))
    (is (= #x6E789E6AA1B965F4 (borba-cad/test-support:prng-next-integer-64 prng)))
    (is (= #x06C45D188009454F (borba-cad/test-support:prng-next-integer-64 prng)))))

(test equal-seeds-give-equal-sequences-and-different-seeds-differ
  (let ((a (borba-cad/test-support:make-prng 42))
        (b (borba-cad/test-support:make-prng 42))
        (c (borba-cad/test-support:make-prng 43)))
    (is (equal (loop repeat 20 collect (borba-cad/test-support:prng-next-integer-64 a))
               (loop repeat 20 collect (borba-cad/test-support:prng-next-integer-64 b))))
    (is (not (equal (loop repeat 20
                          collect (borba-cad/test-support:prng-next-integer-64
                                   (borba-cad/test-support:make-prng 42)))
                    (loop repeat 20 collect (borba-cad/test-support:prng-next-integer-64 c)))))))

(test unit-doubles-stay-in-range-and-cover-it
  (let* ((prng (borba-cad/test-support:make-prng 7))
         (values (loop repeat 20000 collect (borba-cad/test-support:prng-next-unit-double prng))))
    (is (every (lambda (value) (and (>= value 0d0) (< value 1d0))) values))
    (is (< (reduce #'min values) 0.001d0))
    (is (> (reduce #'max values) 0.999d0))
    (is (< (abs (- 0.5d0 (/ (reduce #'+ values) (length values)))) 0.01d0))))

(test ranged-draws-respect-their-bounds
  (let ((prng (borba-cad/test-support:make-prng 9)))
    (dotimes (i 5000)
      (let ((value (borba-cad/test-support:prng-next-double prng -5d0 3d0))
            (index (borba-cad/test-support:prng-next-below prng 7)))
        (is (and (>= value -5d0) (< value 3d0)))
        (is (<= 0 index 6))))
    (is (member (borba-cad/test-support:prng-pick prng '(:a :b :c)) '(:a :b :c)))
    (signals type-error (borba-cad/test-support:prng-next-below prng 0))))
