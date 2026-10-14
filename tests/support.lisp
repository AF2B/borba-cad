(in-package #:borba-cad/tests)

;;;; Helpers shared by the test files.

(defconstant +comparison-tolerance+ 1d-9
  "Relative tolerance (absolute for values below 1) used by APPROX= in tests. Chosen well
above double-float rounding noise of the short computations tested (about 1e-15) and well
below any difference a test is meant to detect.")

(defun approx= (expected
                actual
                &optional (tolerance +comparison-tolerance+))
  "Return true when ACTUAL equals EXPECTED within a relative tolerance.

EXPECTED: the reference value.
ACTUAL: the computed value.
TOLERANCE: relative bound; absolute when both values are below 1."
  (<= (abs (- expected actual))
      (* tolerance (max 1d0 (abs expected) (abs actual)))))

(defun occurrences-of (needle
                       haystack)
  "Return how many times NEEDLE occurs in HAYSTACK, without overlaps.

NEEDLE: the string to look for.
HAYSTACK: the string to search."
  (loop with start = 0
        for position = (search needle haystack :start2 start)
        while position
        count t
        do (setf start (+ position (length needle)))))
