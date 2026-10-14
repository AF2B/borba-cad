(in-package #:borba-cad/bench)

;;;; The environment fingerprint stored with every report, so a result is never separated from
;;;; the machine and software that produced it.

(defun run-command-line (&rest command)
  "Run a command and return its trimmed standard output, or NIL when it fails or is missing.

COMMAND: program name followed by its arguments."
  (handler-case
      (let ((output (uiop:run-program command :output :string :error-output nil :ignore-error-status t)))
        (and output (string-trim '(#\Newline #\Space) output)))
    (error () nil)))

(defun git-revision (directory)
  "Return the short git revision of DIRECTORY, with a dirty marker, or a statement that none exists.

DIRECTORY: the project directory."
  (let* ((root (uiop:native-namestring directory))
         (revision (run-command-line "git"
                                     "-C"
                                     root
                                     "rev-parse"
                                     "--short"
                                     "HEAD")))
    (cond ((or (null revision) (zerop (length revision))) "no commit yet")
          ((plusp (length (or (run-command-line "git"
                                                "-C"
                                                root
                                                "status"
                                                "--porcelain") "")))
           (format nil "~a (uncommitted changes)" revision))
          (t revision))))

(defun environment-fingerprint (config)
  "Return the environment fingerprint as a list of (label . value) pairs.

CONFIG: the application configuration, which supplies the project root."
  (list (cons "date (UTC)" (sbcl:current-utc-timestamp))
        (cons "host" (sbcl:host-name-string))
        (cons "lisp" (sbcl:lisp-version-string))
        (cons "cpu" (sbcl:cpu-model-string))
        (cons "kernel" (sbcl:kernel-release-string))
        (cons "heap capacity (MiB)" (floor (sbcl:heap-capacity-bytes) +bytes-per-mebibyte+))
        (cons "bytes consed between GCs (MiB)" (floor (sb-ext:bytes-consed-between-gcs) +bytes-per-mebibyte+))
        (cons "git revision" (git-revision (base:config-project-root config)))
        (cons "benchmark seed" +benchmark-seed+)))
