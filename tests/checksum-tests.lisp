(in-package #:borba-cad/tests)

(def-suite checksum
  :description "FNV-1a checksums and workload reproducibility."
  :in all)

(in-suite checksum)

(test fnv1a-matches-the-published-test-vectors
  (is (= #xcbf29ce484222325 (rng:fnv1a-64 "")))
  (is (= #xaf63dc4c8601ec8c (rng:fnv1a-64 "a")))
  (is (= #x85944171f73967e8 (rng:fnv1a-64 "foobar"))))

(test the-checksum-hashes-utf-8-bytes
  (is (/= (rng:fnv1a-64 "e") (rng:fnv1a-64 (string (code-char 233)))))
  (is (= (rng:fnv1a-64 (string (code-char 233))) (rng:fnv1a-64 (string (code-char 233))))))

(test equal-workloads-have-equal-checksums-and-any-difference-changes-it
  (let ((first (rng:drawing-checksum (rng:generate-drawing :seed 2026 :entity-count 500)))
        (again (rng:drawing-checksum (rng:generate-drawing :seed 2026 :entity-count 500)))
        (other-seed (rng:drawing-checksum (rng:generate-drawing :seed 2027 :entity-count 500)))
        (other-size (rng:drawing-checksum (rng:generate-drawing :seed 2026 :entity-count 501))))
    (is (string= first again))
    (is (= 16 (length first)))
    (is (string/= first other-seed))
    (is (string/= first other-size))))
