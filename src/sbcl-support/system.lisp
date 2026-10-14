(in-package #:borba-cad/sbcl-support)

(defparameter *end-of-toplevel-options* "--end-toplevel-options"
  "Marker after which SBCL leaves the remaining arguments to the application.")

(defun environment-variable (name)
  "Return the value of the environment variable NAME as a string, or NIL when unset.

NAME: the variable name."
  (sb-ext:posix-getenv name))

(defun command-line-arguments ()
  "Return the application's command-line arguments as a list of strings.

When started through `sbcl ... --end-toplevel-options ARGS` the arguments are those after the
marker; when started as a saved executable they are everything after the program name."
  (let* ((argv sb-ext:*posix-argv*)
         (marker (member *end-of-toplevel-options* argv :test #'string=)))
    (if marker
        (rest marker)
        (rest argv))))

(defun process-id ()
  "Return the operating-system process id."
  (sb-posix:getpid))

(defun lisp-version-string ()
  "Return the Lisp implementation name and version, for example \"SBCL 2.6.9\"."
  (format nil "~a ~a" (lisp-implementation-type) (lisp-implementation-version)))

(defun read-first-line (path)
  "Return the first line of the file at PATH, or NIL when it cannot be read.

PATH: file to read."
  (with-open-file (stream path :if-does-not-exist nil)
    (and stream (read-line stream nil nil))))

(defun kernel-release-string ()
  "Return the kernel release (for example \"7.2.9-arch1-1\"), or \"unknown\"."
  (or (read-first-line "/proc/sys/kernel/osrelease") "unknown"))

(defun host-name-string ()
  "Return the host name, or \"unknown\"."
  (or (read-first-line "/proc/sys/kernel/hostname") "unknown"))

(defun cpu-model-string ()
  "Return the CPU model name from /proc/cpuinfo, or \"unknown\"."
  (with-open-file (stream "/proc/cpuinfo" :if-does-not-exist nil)
    (or (and stream
             (loop for line = (read-line stream nil nil)
                   while line
                   when (and (> (length line) 10)
                             (string= "model name" line :end2 10))
                     return (string-trim " " (subseq line (1+ (position #\: line))))))
        "unknown")))

(defun sync-output-stream (stream)
  "Flush STREAM and force its data to stable storage (fsync).

STREAM: an open file output stream backed by a file descriptor."
  (finish-output stream)
  (sb-posix:fsync (sb-sys:fd-stream-fd stream))
  t)

(defun rename-over (from
                    to)
  "Atomically rename FROM to TO, replacing TO if it exists (same file system).

FROM: pathname of an existing file.
TO: destination pathname."
  (sb-posix:rename (sb-ext:native-namestring from)
                   (sb-ext:native-namestring to)))

(defun delete-file-if-exists (path)
  "Delete the file at PATH if it exists; return true when a file was deleted.

PATH: file to delete."
  (when (probe-file path)
    (delete-file path)
    t))

(defun file-size-bytes (path)
  "Return the size in bytes of the file at PATH.

PATH: an existing file."
  (with-open-file (stream path :element-type '(unsigned-byte 8))
    (file-length stream)))
