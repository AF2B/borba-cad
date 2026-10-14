;;;; Bootstrap shared by every Makefile target.
;;;; Registers the project with ASDF, keeps all compiled output inside build/fasl/, and
;;;; provides the entry points that the Makefile calls (build, tests, lint, ...).

(require :asdf)

(defpackage #:borba-cad/scripts
  (:use #:cl)
  (:export #:*project-root*
           #:project-path
           #:quickload-quietly
           #:build-strict
           #:run-tests
           #:run-platform-tests
           #:fail
           ;; scripts/lint.lisp
           #:lint
           ;; scripts/tools.lisp
           #:install-dependencies
           #:build-executable
           #:logs-summary
           #:make-example
           #:generate-api-reference
           #:start-repl))

(in-package #:borba-cad/scripts)

(defparameter *project-root*
  (uiop:pathname-parent-directory-pathname (uiop:pathname-directory-pathname *load-truename*))
  "Project root: the parent of the scripts/ directory that holds this file.")

(defparameter *project-systems*
  '("borba-cad"
    "borba-cad/platform"
    "borba-cad/test-support"
    "borba-cad/bench"
    "borba-cad/bench-gui"
    "borba-cad/tests"
    "borba-cad/platform-tests")
  "Every system defined by this project; the strict build compiles all of them.")

(defparameter *lint-documented-systems*
  '("borba-cad" "borba-cad/platform" "borba-cad/test-support" "borba-cad/bench" "borba-cad/bench-gui")
  "Systems whose exported symbols make up the documented API (checked by lint, listed by
the API reference).")

(defparameter *undocumented-package-names*
  '("BORBA-CAD/TESTS" "BORBA-CAD/PLATFORM-TESTS")
  "Packages of test code, whose exports are test entry points rather than API.")

(defun project-path (relative)
  "Return the absolute pathname of RELATIVE inside the project.

RELATIVE: path string relative to the project root."
  (merge-pathnames relative *project-root*))

(defun fail (control
             &rest arguments)
  "Print an error message and exit the process with status 1.

CONTROL: a format control string.
ARGUMENTS: format arguments."
  (format *error-output* "~&ERROR: ~?~%" control arguments)
  (finish-output *error-output*)
  (finish-output *standard-output*)
  (sb-ext:exit :code 1 :abort t))

(defun configure-asdf ()
  "Make ASDF find the project's systems and write every fasl below build/fasl/."
  (pushnew *project-root* asdf:*central-registry* :test #'equal)
  (asdf:initialize-output-translations
   `(:output-translations
     (t (,(namestring (project-path (format nil "build/fasl/~a/" (uiop:implementation-identifier))))
         :**/ :*.*.*))
     :ignore-inherited-configuration)))

(configure-asdf)

(defun quickload-quietly (&rest system-names)
  "Load SYSTEM-NAMES through Quicklisp (installing third-party dependencies when missing).

SYSTEM-NAMES: strings naming ASDF systems."
  (unless (find-package '#:ql)
    (fail "Quicklisp is not loaded; run through the Makefile (see docs/development.md)."))
  (dolist (name system-names)
    (uiop:symbol-call '#:ql '#:quickload name :silent t)))

(defun own-system-p (name)
  "Return true when NAME is one of this project's systems.

NAME: a system name string."
  (and (>= (length name) 9)
       (string-equal "borba-cad" name :end2 9)))

(defun third-party-dependencies (system-name)
  "Return the names of the third-party systems that SYSTEM-NAME needs, transitively through
this project's own systems.

SYSTEM-NAME: a system name string."
  (let ((found '()))
    (labels ((walk (name)
               (dolist (dependency (asdf:system-depends-on (asdf:find-system name)))
                 (when (stringp dependency)
                   (let ((dependency-name (string-downcase dependency)))
                     (if (own-system-p dependency-name)
                         (walk dependency-name)
                         (pushnew dependency-name found :test #'string=)))))))
      (walk system-name))
    (nreverse found)))

(defmacro with-quiet-progress (&body body)
  "Run BODY with standard output discarded, so compiler progress lines do not bury the
warnings (which go to the error stream).

BODY: forms to run."
  `(let ((*standard-output* (make-broadcast-stream)))
     ,@body))

(defun delete-own-fasls ()
  "Delete the compiled files of this project so the next load recompiles everything."
  (let ((directory (asdf:apply-output-translations *project-root*)))
    (uiop:delete-directory-tree directory :validate t :if-does-not-exist :ignore)))

(defun build-strict (&key (systems *project-systems*))
  "Recompile this project's SYSTEMS from scratch and fail when anything signals a WARNING or
STYLE-WARNING (the project's zero-warning gate).

SYSTEMS: names of the systems to build."
  (dolist (system systems)
    (with-quiet-progress
      (apply #'quickload-quietly (third-party-dependencies system))))
  (delete-own-fasls)
  (let ((warnings '()))
    ;; Redefinitions that SBCL itself treats as uninteresting (a macro defined at compile time
    ;; and again when its fasl loads) are not project warnings.
    (handler-bind ((warning (lambda (condition)
                              (unless (typep condition sb-ext:*muffled-warnings*)
                                (push condition warnings)))))
      (with-quiet-progress
        (dolist (system systems)
          (asdf:load-system system))))
    (cond (warnings
           (format *error-output* "~&~%BUILD FAILED: ~d warning(s):~%~{  - ~a~%~}"
                   (length warnings)
                   (mapcar (lambda (condition) (remove #\Newline (princ-to-string condition)))
                           (reverse warnings)))
           (fail "the build must be warning-free"))
          (t (format t "~&Build OK: ~{~a~^, ~} compiled with zero warnings.~%" systems)))))

(defun run-test-system (system
                        package
                        suite)
  "Load a test system and run one of its suites; exit with status 1 when any check fails.

SYSTEM: name of the ASDF test system.
PACKAGE: name of the package that defines RUN-ALL and the suites.
SUITE: name of the FiveAM suite to run."
  (with-quiet-progress
    (apply #'quickload-quietly (third-party-dependencies system))
    (asdf:load-system system))
  (let ((suite-symbol (find-symbol (string-upcase suite) package)))
    (unless suite-symbol
      (fail "unknown test suite ~s" suite))
    (unless (uiop:symbol-call package '#:run-all :suite suite-symbol)
      (fail "tests failed"))
    (format t "~&All checks passed.~%")))

(defun run-tests (&key (suite "ALL"))
  "Run the headless correctness tests.

SUITE: name of the FiveAM suite to run."
  (run-test-system "borba-cad/tests"
                   '#:borba-cad/tests
                   suite))

(defun run-platform-tests (&key (suite "ALL"))
  "Run the window-program tests (the renderer tests need a graphical session).

SUITE: name of the FiveAM suite to run."
  (run-test-system "borba-cad/platform-tests"
                   '#:borba-cad/platform-tests
                   suite))
