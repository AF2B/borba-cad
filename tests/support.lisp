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

(defvar *scratch-drawings-directory* nil
  "The folder, made on first use and private to this process, where the file dialogs of tests
start, so that no test ever reads or writes in the real documents folder.")

(defun scratch-drawings-directory ()
  "Return the folder where the file dialogs of tests start, creating it when needed."
  (unless *scratch-drawings-directory*
    (setf *scratch-drawings-directory*
          (uiop:ensure-directory-pathname
           (merge-pathnames (format nil "borba-cad-test-drawings-~d/" (sbcl:process-id))
                            (uiop:temporary-directory)))))
  (ensure-directories-exist *scratch-drawings-directory*)
  *scratch-drawings-directory*)

(defun quiet-config (&optional (project-root #p"/tmp/"))
  "Return a configuration for tests that leaves the user's files alone: nothing is remembered
between runs, and file dialogs start in a folder private to the test process, so a test that
types a name and saves does not write into the real documents folder.

PROJECT-ROOT: the project root the configuration is made for."
  (let ((config (base:default-config :project-root project-root)))
    (setf (base:config-state-directory config) nil
          (base:config-drawings-directory config) (scratch-drawings-directory))
    config))

