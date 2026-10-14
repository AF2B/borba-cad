(defpackage #:borba-cad/test-support
  (:use #:cl)
  (:documentation "Seeded PRNG and deterministic workload generation for tests and benchmarks.")
  (:export #:prng
           #:make-prng
           #:prng-next-integer-64
           #:prng-next-unit-double
           #:prng-next-double
           #:prng-next-below
           #:prng-next-boolean
           #:prng-pick
           #:generate-drawing
           #:random-entity
           #:random-kind
           #:fnv1a-64
           #:drawing-checksum
           #:make-temporary-directory
           #:with-temporary-directory))
