(in-package #:borba-cad/sbcl-support)

;;;; File-system questions and small operations that the portable code asks through this package:
;;;; what is at a path, who may use it, and what the operating system calls a failure.

#-win32
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

#+win32
(defun file-kind (path)
  "Return :file or :directory for what PATH names, or NIL when nothing can be found there.

PATH: a pathname."
  (portable-file-kind path))

(defun true-path (path)
  "Return the pathname of the file that PATH names after following symbolic links, or PATH itself
when nothing exists there.

PATH: a pathname."
  (or (probe-file path)
      path))

#-win32
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

#+win32
(defun path-accessible-p (path
                          access)
  "Return true when something exists at PATH. Windows is not asked about permissions, so every
kind of access is answered the same.

PATH: a pathname.
ACCESS: :read, :write or :execute."
  (declare (ignore access))
  (and (portable-file-kind path) t))

#-win32
(defun file-permissions (path)
  "Return the permission bits of the file at PATH as an integer, or NIL when it cannot be read.

PATH: a pathname."
  (handler-case
      (logand #o7777 (sb-posix:stat-mode (sb-posix:stat (sb-ext:native-namestring path))))
    (sb-posix:syscall-error ()
      nil)))

#+win32
(defun file-permissions (path)
  "Return NIL: Windows has no permission bits to read.

PATH: a pathname."
  (declare (ignore path))
  nil)

#-win32
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

#+win32
(defun set-file-permissions (path
                             permissions)
  "Return NIL: Windows has no permission bits to set, and callers treat that as best effort.

PATH: a pathname.
PERMISSIONS: permission bits as returned by FILE-PERMISSIONS."
  (declare (ignore path permissions))
  nil)

(defun describe-os-error (condition)
  "Return the operating system's wording of the problem behind CONDITION, for example
\"Permission denied\" or \"No space left on device\", or the condition's own one-line text when
it carries no error number.

CONDITION: an error condition."
  (let ((text (substitute #\Space #\Newline (princ-to-string condition))))
    (cond #-win32
          ((typep condition 'sb-posix:syscall-error)
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

(defparameter *current-and-parent-names* (list "." "..")
  "The entries every folder lists that are not files.")

#-win32
(defun describe-entry (directory
                       name)
  "Return what a folder entry is as (name kind size modified), or NIL when the entry points
nowhere (a broken symbolic link) or cannot be examined. A symbolic link shows what it points to.

DIRECTORY: the folder that holds the entry, as a native name ending in a slash.
NAME: the entry's name."
  (handler-case
      (let* ((stat (sb-posix:stat (concatenate 'string directory name)))
             (mode (sb-posix:stat-mode stat))
             (kind (cond ((sb-posix:s-isdir mode) :directory)
                         ((sb-posix:s-isreg mode) :file)
                         (t :other))))
        (list name
              kind
              (and (eq kind :file) (sb-posix:stat-size stat))
              (+ +unix-epoch-universal-time+ (sb-posix:stat-mtime stat))))
    (sb-posix:syscall-error ()
      nil)))

#-win32
(defun directory-listing (directory)
  "Return the entries of the folder DIRECTORY, in no particular order, as a list of
(name kind size modified), and a second value: NIL, or a sentence that says why the folder
cannot be read. KIND is :directory, :file or :other (devices, sockets); SIZE is the byte count of
a file and NIL otherwise; MODIFIED is a universal time.

DIRECTORY: a pathname."
  (let ((native (sb-ext:native-namestring (uiop:ensure-directory-pathname directory)))
        (handle nil)
        (entries '()))
    (handler-case
        (progn
          (setf handle (sb-posix:opendir (sb-ext:native-namestring directory)))
          (unwind-protect
               (loop for entry = (sb-posix:readdir handle)
                     until (sb-alien:null-alien entry)
                     do (let ((name (sb-posix:dirent-name entry)))
                          (unless (member name *current-and-parent-names* :test #'string=)
                            (let ((description (describe-entry native name)))
                              (when description
                                (push description entries))))))
            (sb-posix:closedir handle))
          (values (nreverse entries) nil))
      (sb-posix:syscall-error (condition)
        (values nil (format nil "This folder cannot be read: ~a." (describe-os-error condition)))))))

#+win32
(defun directory-listing (directory)
  "Return the entries of the folder DIRECTORY as a list of (name kind size modified), and a second
value: NIL, or a sentence that says why the folder cannot be read. KIND is :directory or :file;
SIZE is the byte count of a file and NIL otherwise; MODIFIED is a universal time.

DIRECTORY: a pathname."
  (portable-directory-listing directory))

#-win32
(defun make-directory (path)
  "Create the folder PATH, which must not exist yet and whose parent must. Returns NIL on
success, and otherwise a sentence that says why it could not be made.

PATH: a pathname."
  (handler-case
      (progn (sb-posix:mkdir (sb-ext:native-namestring path) #o755)
             nil)
    (sb-posix:syscall-error (condition)
      (format nil "The folder cannot be created: ~a." (describe-os-error condition)))))

#+win32
(defun make-directory (path)
  "Create the folder PATH, which must not exist yet and whose parent must. Returns NIL on
success, and otherwise a sentence that says why it could not be made.

PATH: a pathname."
  (portable-make-directory path))

