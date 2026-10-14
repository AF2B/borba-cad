(in-package #:borba-cad/sbcl-support)

;;;; File-system questions and small operations that the portable code asks through this package:
;;;; what is at a path, who may use it, and what the operating system calls a failure.

(defun file-kind (path)
  "Return :file, :directory or :other for what PATH names, following symbolic links, or NIL when
nothing can be found there.

PATH: a pathname."
  (handler-case
      (let ((mode (sb-posix:stat-mode (sb-posix:stat (sb-ext:native-namestring path)))))
        (cond ((sb-posix:s-isdir mode) :directory)
              ((sb-posix:s-isreg mode) :file)
              (t :other)))
    (sb-posix:syscall-error ()
      nil)))

(defun true-path (path)
  "Return the pathname of the file that PATH names after following symbolic links, or PATH itself
when nothing exists there.

PATH: a pathname."
  (or (probe-file path)
      path))

(defun path-accessible-p (path
                          access)
  "Return true when the current user has the given access to PATH.

PATH: a pathname.
ACCESS: :read, :write or :execute."
  (handler-case
      (progn (sb-posix:access (sb-ext:native-namestring path)
                              (ecase access
                                (:read sb-posix:r-ok)
                                (:write sb-posix:w-ok)
                                (:execute sb-posix:x-ok)))
             t)
    (sb-posix:syscall-error ()
      nil)))

(defun file-permissions (path)
  "Return the permission bits of the file at PATH as an integer, or NIL when it cannot be read.

PATH: a pathname."
  (handler-case
      (logand #o7777 (sb-posix:stat-mode (sb-posix:stat (sb-ext:native-namestring path))))
    (sb-posix:syscall-error ()
      nil)))

(defun set-file-permissions (path
                             permissions)
  "Give the file at PATH the permission bits PERMISSIONS. Returns true on success and NIL when
the system refuses; callers use it to keep a replaced file's permissions, which is best effort.

PATH: a pathname.
PERMISSIONS: permission bits as returned by FILE-PERMISSIONS."
  (handler-case
      (progn (sb-posix:chmod (sb-ext:native-namestring path) permissions)
             t)
    (sb-posix:syscall-error ()
      nil)))

(defun describe-os-error (condition)
  "Return the operating system's wording of the problem behind CONDITION, for example
\"Permission denied\" or \"No space left on device\", or the condition's own one-line text when
it carries no error number.

CONDITION: an error condition."
  (let ((text (substitute #\Space #\Newline (princ-to-string condition))))
    (cond ((typep condition 'sb-posix:syscall-error)
           (sb-int:strerror (sb-posix:syscall-errno condition)))
          ;; SBCL words file errors as \"error opening #P...: Permission denied\".
          ((and (typep condition 'file-error)
                (search ": " text))
           (string-trim " " (subseq text (+ 2 (search ": " text :from-end t)))))
          (t text))))

(defconstant +unicode-replacement-code+ #xFFFD
  "Code point that stands for an invalid byte sequence when decoding.")

(defun utf-8-octets->string (octets)
  "Decode UTF-8 octets into a string. Returns NIL when they are not valid UTF-8.

OCTETS: a vector of integers 0 to 255."
  (let* ((replacement (code-char +unicode-replacement-code+))
         (text (sb-ext:octets-to-string (coerce octets '(simple-array (unsigned-byte 8) (*)))
                                        :external-format (list :utf-8 :replacement replacement))))
    (and (not (find replacement text))
         text)))
