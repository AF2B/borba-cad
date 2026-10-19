(in-package #:borba-cad/tests)

(def-suite packaging
  :description "The scripts that package the program, as far as they can be tried on this machine."
  :in all)

(in-suite packaging)

(defconstant +pe-offset-position+ 60
  "Position, in a Windows program, of the 4-byte offset of the PE header.")

(defconstant +test-pe-offset+ 128
  "Where the fabricated program's PE header begins.")

(defconstant +optional-header-distance+ 24
  "Bytes from the start of the PE signature to the start of the optional header.")

(defconstant +subsystem-distance+ 68
  "Bytes from the start of the optional header to its subsystem field.")

(defconstant +console-subsystem+ 3
  "The subsystem number of a program that runs in a console window.")

(defconstant +graphical-subsystem+ 2
  "The subsystem number of a program that opens no console window.")

(defun project-file (relative)
  "Return the pathname of RELATIVE inside the project."
  (merge-pathnames relative (asdf:system-source-directory "borba-cad")))

(defun fabricated-program-bytes ()
  "Return the bytes of a minimal Windows program: the MZ mark, the offset of the PE header, the PE
signature, an optional header that says PE32+ and console, and a payload after it that stands for
the Lisp core SBCL appends."
  (let ((bytes (make-array 1024 :element-type '(unsigned-byte 8) :initial-element 0))
        (subsystem-position (+ +test-pe-offset+ +optional-header-distance+ +subsystem-distance+)))
    (setf (aref bytes 0) (char-code #\M)
          (aref bytes 1) (char-code #\Z)
          (aref bytes +pe-offset-position+) +test-pe-offset+
          (aref bytes +test-pe-offset+) (char-code #\P)
          (aref bytes (+ +test-pe-offset+ 1)) (char-code #\E)
          (aref bytes (+ +test-pe-offset+ +optional-header-distance+)) #x0b
          (aref bytes (+ +test-pe-offset+ +optional-header-distance+ 1)) #x02
          (aref bytes subsystem-position) +console-subsystem+)
    (loop for index from 256 below 512
          do (setf (aref bytes index) (mod index 251)))
    bytes))

(defun bash-available-p ()
  "Return true when bash can be run, which the script tests need. Any failure to start it means
it cannot be: this is a probe for a capability, not error handling."
  (handler-case (progn (uiop:run-program (list "bash" "-c" "exit 0"))
                       t)
    (error ()
      nil)))

(defun write-octets (path octets)
  "Write OCTETS to the file PATH."
  (with-open-file (out path :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
    (write-sequence octets out)))

(defun read-octets-of (path)
  "Return the octets of the file PATH."
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((octets (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence octets in)
      octets)))

(defun run-windows-script-function (function path)
  "Run FUNCTION of scripts/dist-windows.sh on the file PATH, reading the script without running
its build. Returns the exit status and what the function wrote on its error output."
  (multiple-value-bind (output error status)
      (uiop:run-program (list "bash" "-c"
                              "BORBA_CAD_SOURCE_ONLY=1 source \"$0\" x y z; \"$1\" \"$2\""
                              (uiop:native-namestring (project-file "scripts/dist-windows.sh"))
                              function
                              (uiop:native-namestring path))
                        :output nil
                        :error-output :string
                        :ignore-error-status t)
    (declare (ignore output))
    (values status error)))

(test the-windows-script-turns-a-console-program-into-a-graphical-one-and-touches-nothing-else
  (when (bash-available-p)
    (with-temporary-directory (directory)
      (let ((path (merge-pathnames "program.exe" directory))
            (original (fabricated-program-bytes)))
        (write-octets path original)
        (is (zerop (run-windows-script-function "set_gui_subsystem" path)))
        (let ((changed (read-octets-of path))
              (subsystem-position (+ +test-pe-offset+ +optional-header-distance+ +subsystem-distance+)))
          (is (= (length original) (length changed)))
          (is (= +graphical-subsystem+ (aref changed subsystem-position)))
          (is (= 0 (aref changed (1+ subsystem-position))))
          (is (equal (loop for index below (length original)
                           unless (<= subsystem-position index (1+ subsystem-position))
                             collect (aref original index))
                     (loop for index below (length changed)
                           unless (<= subsystem-position index (1+ subsystem-position))
                             collect (aref changed index)))
              "only the two bytes of the subsystem field changed"))))))

(test the-windows-script-refuses-a-file-that-is-not-a-windows-program
  (when (bash-available-p)
    (with-temporary-directory (directory)
      (let ((path (merge-pathnames "notes.txt" directory)))
        (write-octets path (map '(vector (unsigned-byte 8)) #'char-code "just some text, not a program"))
        (multiple-value-bind (status error) (run-windows-script-function "set_gui_subsystem" path)
          (is (plusp status))
          (is (search "not a Windows program" error)))
        (let ((bad-pe (fabricated-program-bytes)))
          (setf (aref bad-pe +test-pe-offset+) 0)           ; the PE signature is gone
          (write-octets path bad-pe)
          (multiple-value-bind (status error) (run-windows-script-function "set_gui_subsystem" path)
            (is (plusp status))
            (is (search "no PE header" error))))))))

(test every-packaging-file-is-where-the-scripts-expect-it
  (dolist (file '("packaging/borba-cad.svg" "packaging/linux/borba-cad" "packaging/linux/install.sh"
                  "packaging/linux/uninstall.sh" "packaging/linux/README.txt" "packaging/linux/Borba CAD.desktop"
                  "packaging/windows/README.txt" "scripts/dist-linux.sh" "scripts/dist-windows.sh"
                  "scripts/verify-dist.sh" "scripts/version.sh" "scripts/release-notes.sh"
                  ".github/workflows/ci.yml" ".github/actions/quicklisp/action.yml"))
    (is (probe-file (project-file file)) "~a is missing" file))
  (dolist (script '("packaging/linux/borba-cad" "packaging/linux/install.sh" "packaging/linux/uninstall.sh"
                    "scripts/dist-linux.sh" "scripts/dist-windows.sh" "scripts/verify-dist.sh" "scripts/version.sh"
                    "scripts/release-notes.sh"))
    (is (logtest #o100 (sb-posix:stat-mode (sb-posix:stat (uiop:native-namestring (project-file script)))))
        "~a is not executable" script)))

;;; The version is a tag, and a release has the notes of its version

(defun run-project-script (script
                           arguments
                           &key environment)
  "Run a script of the project and return what it printed, what it wrote on its error output and its exit status.

SCRIPT: the path of the script, relative to the project.
ARGUMENTS: the arguments it is given.
ENVIRONMENT: a list of strings such as \"NAME=value\", added to the environment the script runs in."
  (multiple-value-bind (output error status)
      (uiop:run-program (append (list "env")
                                environment
                                (list (uiop:native-namestring (project-file script)))
                                arguments)
                        :output :string
                        :error-output :string
                        :ignore-error-status t)
    (values (string-right-trim '(#\Newline) output) error status)))

(defun version-of-tag (tag)
  "Return the version that a run started by the tag TAG would build, its error output and its exit status.

TAG: the name of a tag."
  (run-project-script "scripts/version.sh"
                      '()
                      :environment (list "GITHUB_REF_TYPE=tag"
                                         (format nil "GITHUB_REF_NAME=~a" tag))))

(test a-version-is-a-tag-that-reads-vmajor-minor-patch
  (when (bash-available-p)
    (dolist (case '(("v1.3.0" "1.3.0") ("v10.20.30" "10.20.30") ("v1.3.0-rc.1" "1.3.0-rc.1")))
      (multiple-value-bind (version error status) (version-of-tag (first case))
        (declare (ignore error))
        (is (zerop status) "~a" (first case))
        (is (string= (second case) version))))))

(test a-tag-that-is-not-a-version-stops-the-pipeline-and-says-why
  (when (bash-available-p)
    (dolist (tag '("build-4" "v1.3" "1.3.0" "v1.3.0.1" "vnext" "v1.3.0-"))
      (multiple-value-bind (version error status) (version-of-tag tag)
        (declare (ignore version))
        (is (plusp status) "~a is not a version" tag)
        (is (search "is not a version" error) "~a: ~a" tag error)))))

(test a-build-that-is-not-a-release-never-carries-the-name-of-one
  (when (bash-available-p)
    (multiple-value-bind (version error status) (run-project-script "scripts/version.sh" '())
      (declare (ignore error))
      (is (zerop status))
      (is (every #'digit-char-p (subseq version 0 1)) "it starts with a version: ~a" version)
      (is (= 2 (count #\. version :end (position #\- version :start (min 1 (length version)))))
          "MAJOR.MINOR.PATCH, then what says how far past it the build is: ~a" version))))

(defparameter *sample-changelog*
  "# Changelog

## [Unreleased]

### Added

- Something not yet released.

## [1.3.0] - 2026-11-01

### Added

- The new thing.

### Fixed

- The old bug.

## [1.2.0] - 2026-10-15

- The previous release.
"
  "A changelog with an unreleased section and two releases.")

(defun release-notes-of (version
                         changelog)
  "Return the notes of VERSION in a changelog with the text CHANGELOG, as scripts/release-notes.sh prints them, its
error output and its exit status.

VERSION: a version.
CHANGELOG: the text of a changelog."
  (with-temporary-directory (directory)
    (let ((path (merge-pathnames "CHANGELOG.md" directory)))
      (with-open-file (out path :direction :output :if-exists :supersede)
        (write-string changelog out))
      (run-project-script "scripts/release-notes.sh"
                          (list version)
                          :environment (list (format nil "CHANGELOG=~a" (uiop:native-namestring path)))))))

(test the-notes-of-a-release-are-its-section-of-the-changelog
  (when (bash-available-p)
    (multiple-value-bind (notes error status) (release-notes-of "1.3.0" *sample-changelog*)
      (declare (ignore error))
      (is (zerop status))
      (is (eql 0 (search "### Added" notes)) "the heading of the version is left out, and so is the blank after it")
      (is (search "- The old bug." notes))
      (is (null (search "The previous release" notes)) "it stops where the next release begins")
      (is (null (search "Something not yet released" notes))))))

(test a-pre-release-uses-the-notes-of-the-version-it-leads-to-and-else-the-unreleased-ones
  (when (bash-available-p)
    (is (search "The new thing" (release-notes-of "1.3.0-rc.1" *sample-changelog*)))
    (is (search "Something not yet released" (release-notes-of "1.4.0-rc.1" *sample-changelog*)))))

(test a-release-without-notes-is-refused
  (when (bash-available-p)
    (multiple-value-bind (notes error status) (release-notes-of "1.4.0" *sample-changelog*)
      (declare (ignore notes))
      (is (plusp status))
      (is (search "no notes for version 1.4.0" error)))
    (multiple-value-bind (notes error status) (release-notes-of "1.3.0" "## [1.3.0] - 2026-11-01

## [1.2.0] - 2026-10-15

- Old.
")
      (declare (ignore notes))
      (is (plusp status) "an empty section is not notes")
      (is (search "no notes" error)))))

(test the-pipeline-makes-a-release-only-from-a-version-tag
  (let ((workflow (uiop:read-file-string (project-file ".github/workflows/ci.yml"))))
    (is (search "tags: [\"v*\"]" workflow))
    (is (search "if: needs.version.outputs.release == 'true'" workflow) "the release job waits for a version tag")
    (is (search "--verify-tag" workflow) "a release is made for a tag that exists")
    (is (null (search "build-$" workflow)) "no tag is made for a run that is not a version")
    (is (search "runs-on: windows-latest" workflow))
    (is (search "make dist dist-check" workflow))
    (is (search "make dist-windows" workflow))
    (dolist (stage '("make lint" "make compile test test-gui"))
      (is (search stage workflow) "the pipeline runs ~a" stage))))
