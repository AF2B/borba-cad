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
                  "scripts/verify-dist.sh" ".github/workflows/dist.yml"))
    (is (probe-file (project-file file)) "~a is missing" file))
  (dolist (script '("packaging/linux/borba-cad" "packaging/linux/install.sh" "packaging/linux/uninstall.sh"
                    "scripts/dist-linux.sh" "scripts/dist-windows.sh" "scripts/verify-dist.sh"))
    (is (logtest #o100 (sb-posix:stat-mode (sb-posix:stat (uiop:native-namestring (project-file script)))))
        "~a is not executable" script)))
