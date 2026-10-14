(in-package #:borba-cad/base)

(defconstant +default-log-max-bytes+ (* 5 1024 1024)
  "Default size at which a log file is rotated: 5 MiB keeps hours of info-level output.")

(defconstant +default-log-max-files+ 5
  "Default number of log files kept (current plus rotated); with 5 MiB each, disk use is
bounded at 25 MiB.")

(defconstant +default-window-width+ 1440
  "Default window width in logical pixels.")

(defconstant +default-window-height+ 900
  "Default window height in logical pixels.")

(defconstant +default-frame-summary-interval-seconds+ 1
  "Seconds between aggregated frame/input summary log entries.")

(defconstant +default-memory-sample-interval-seconds+ 5
  "Seconds between memory.sample log entries.")

(defconstant +minimum-log-max-bytes+ 1024
  "Smallest accepted rotation size, so a misconfiguration cannot rotate on every line.")

(defparameter *log-levels* '(:error :warn :info :debug :trace)
  "Log severities from most to least severe.")

(defun log-levels ()
  "Return the list of log level keywords, from most to least severe."
  (copy-list *log-levels*))

(defstruct (config (:copier copy-config))
  "Central application configuration; every default lives here.

PROJECT-ROOT: directory containing the project (logs and examples are relative to it).
LOG-DIRECTORY: directory receiving the rotating log file.
STATE-DIRECTORY: directory for what the program remembers between runs (the recent files), or NIL
to remember nothing.
DRAWINGS-DIRECTORY: folder where the file dialogs start when no drawing is being edited, or NIL for
the user's documents folder.
LOG-LEVEL: least severe level that is written.
LOG-MAX-BYTES: size at which the log file is rotated.
LOG-MAX-FILES: number of log files kept, including the current one.
LOG-TO-STDERR: also echo log lines to standard error.
LOG-SAMPLING: alist of event name and N; only one in N entries of that event is written.
FRAME-SUMMARY-INTERVAL-SECONDS: period of aggregated summary entries.
MEMORY-SAMPLE-INTERVAL-SECONDS: period of memory samples.
FONT-PATH: monospace TrueType font file, or NIL to search the usual locations.
WINDOW-WIDTH: initial window width in logical pixels.
WINDOW-HEIGHT: initial window height in logical pixels.
INITIAL-FILE: drawing file to open at startup, or NIL.
LANGUAGE: the language keyword the user asked for with an option or the environment, or NIL to use the
one saved last time and, failing that, English (US)."
  (project-root nil :type (or null pathname))
  (log-directory nil :type (or null pathname))
  (state-directory nil :type (or null pathname))
  (drawings-directory nil :type (or null pathname))
  (log-level :info :type keyword)
  (log-max-bytes +default-log-max-bytes+ :type integer)
  (log-max-files +default-log-max-files+ :type integer)
  (log-to-stderr nil :type boolean)
  (log-sampling nil :type list)
  (frame-summary-interval-seconds +default-frame-summary-interval-seconds+ :type real)
  (memory-sample-interval-seconds +default-memory-sample-interval-seconds+ :type real)
  (font-path nil :type (or null string))
  (window-width +default-window-width+ :type integer)
  (window-height +default-window-height+ :type integer)
  (initial-file nil :type (or null string))
  (language nil :type (or null keyword)))

(defun configuration-failure (control
                              &rest arguments)
  "Signal a CONFIGURATION-ERROR whose reason is built from CONTROL and ARGUMENTS.

CONTROL: a format control string.
ARGUMENTS: format arguments."
  (error 'configuration-error
         :reason (apply #'format nil control arguments)))

(defun parse-log-level (text)
  "Return the log level keyword named by TEXT, signaling CONFIGURATION-ERROR when unknown.

TEXT: a level name such as \"debug\" (case-insensitive)."
  (or (find text *log-levels* :test #'string-equal)
      (configuration-failure "unknown log level ~s; use one of ~{~(~a~)~^, ~}"
                             text
                             *log-levels*)))

(defun parse-language-option (text)
  "Return the language keyword named by TEXT, signaling CONFIGURATION-ERROR when the program does
not speak it.

TEXT: a language tag such as \"pt-BR\" (any case; a hyphen or an underscore)."
  (or (i18n:parse-language text)
      (configuration-failure "unknown language ~s; use one of ~{~a~^, ~}"
                             text
                             (mapcar #'i18n:language-tag (i18n:supported-languages)))))

(defun parse-positive-integer-option (text
                                      option-name)
  "Parse TEXT as a positive integer, signaling CONFIGURATION-ERROR otherwise.

TEXT: the option value.
OPTION-NAME: option name used in the error message."
  (let ((value (handler-case (parse-integer text)
                 (parse-error ()
                   (configuration-failure "~a expects a positive integer, got ~s"
                                          option-name
                                          text)))))
    (unless (plusp value)
      (configuration-failure "~a expects a positive integer, got ~s"
                             option-name
                             text))
    value))

(defun parse-sampling-option (text)
  "Parse \"event=N\" into a cons of event name and positive integer N.

TEXT: a sampling rule such as \"input.event=10\"."
  (let ((separator (position #\= text)))
    (unless separator
      (configuration-failure "sampling rule ~s must look like event.name=N" text))
    (cons (subseq text 0 separator)
          (parse-positive-integer-option (subseq text (1+ separator)) "log sampling"))))

(defun parse-boolean-option (text)
  "Parse a boolean option value (1, true, yes, on / 0, false, no, off).

TEXT: the option value."
  (cond ((member text '("1" "true" "yes" "on") :test #'string-equal) t)
        ((member text '("0" "false" "no" "off") :test #'string-equal) nil)
        (t (configuration-failure "expected 1/0, true/false, yes/no or on/off, got ~s" text))))

(defun directory-pathname (text)
  "Return TEXT as a directory pathname.

TEXT: a directory name with or without a trailing slash."
  (uiop:ensure-directory-pathname text))

(defun detect-project-root ()
  "Return the project root directory.
Order: BORBA_CAD_ROOT, the ASDF source directory of the borba-cad system, the current directory."
  (let ((from-environment (environment-variable "BORBA_CAD_ROOT")))
    (cond ((and from-environment (plusp (length from-environment)))
           (directory-pathname from-environment))
          (t (or (handler-case (asdf:system-source-directory "borba-cad")
                   (asdf:missing-component () nil))
                 (uiop:getcwd))))))

(defun default-config (&key (project-root (detect-project-root)))
  "Return the default configuration.

PROJECT-ROOT: the project directory; logs default to var/log/ and remembered state to var/state/
below it."
  (make-config :project-root project-root
               :log-directory (merge-pathnames "var/log/" project-root)
               :state-directory (merge-pathnames "var/state/" project-root)))

(defparameter *windows-state-folder-name* "BorbaCAD"
  "Name of the folder, in the user's local application data, where the installed program keeps its
log and what it remembers on Windows.")

(defparameter *unix-state-folder-name* "borba-cad"
  "Name of the folder, in the user's state directory, where the installed program keeps its log and
what it remembers on Linux.")

(defun user-state-directory (&optional (reader #'environment-variable))
  "Return the folder where an installed program keeps its log and what it remembers between
runs: %LOCALAPPDATA%\\BorbaCAD on Windows, $XDG_STATE_HOME/borba-cad (by default
~/.local/state/borba-cad) elsewhere, and a folder in the home directory when the system does not
say where.

READER: function from variable name to string or NIL; defaults to the process environment."
  (flet ((value-of (name)
           (let ((value (funcall reader name)))
             (and value (plusp (length value)) value))))
    (if (uiop:os-windows-p)
        (merge-pathnames (concatenate 'string *windows-state-folder-name* "/")
                         (let ((local (value-of "LOCALAPPDATA")))
                           (if local
                               (directory-pathname local)
                               (user-homedir-pathname))))
        (merge-pathnames (concatenate 'string *unix-state-folder-name* "/")
                         (let ((state (value-of "XDG_STATE_HOME")))
                           (if state
                               (directory-pathname state)
                               (merge-pathnames ".local/state/" (user-homedir-pathname))))))))

(defun installed-config (application-directory
                         &optional (reader #'environment-variable))
  "Return the configuration of the program running from an installed folder: the folder holds the
program, its examples and its fonts, and the log and what the program remembers go to the user's
own state directory, because the installed folder may not be writable.

APPLICATION-DIRECTORY: the folder that holds the executable.
READER: function from variable name to string or NIL; defaults to the process environment."
  (let ((state (user-state-directory reader)))
    (make-config :project-root application-directory
                 :log-directory (merge-pathnames "log/" state)
                 :state-directory state)))

(defun config-with-environment (config
                                &optional (reader #'environment-variable))
  "Return a copy of CONFIG with BORBA_CAD_* environment overrides applied.
Signals CONFIGURATION-ERROR for invalid values.

CONFIG: the configuration to start from.
READER: function from variable name to string or NIL; defaults to the process environment."
  (let ((result (copy-config config)))
    (flet ((value-of (name)
             (let ((value (funcall reader name)))
               (and value (plusp (length value)) value))))
      (let ((log-dir (value-of "BORBA_CAD_LOG_DIR"))
            (state-dir (value-of "BORBA_CAD_STATE_DIR"))
            (drawings-dir (value-of "BORBA_CAD_DRAWINGS_DIR"))
            (log-level (value-of "BORBA_CAD_LOG_LEVEL"))
            (max-bytes (value-of "BORBA_CAD_LOG_MAX_BYTES"))
            (max-files (value-of "BORBA_CAD_LOG_MAX_FILES"))
            (to-stderr (value-of "BORBA_CAD_LOG_STDERR"))
            (sampling (value-of "BORBA_CAD_LOG_SAMPLING"))
            (font (value-of "BORBA_CAD_FONT"))
            (language (value-of "BORBA_CAD_LANGUAGE")))
        (when log-dir
          (setf (config-log-directory result) (directory-pathname log-dir)))
        (when state-dir
          (setf (config-state-directory result) (directory-pathname state-dir)))
        (when drawings-dir
          (setf (config-drawings-directory result) (directory-pathname drawings-dir)))
        (when log-level
          (setf (config-log-level result) (parse-log-level log-level)))
        (when max-bytes
          (setf (config-log-max-bytes result)
                (max +minimum-log-max-bytes+
                     (parse-positive-integer-option max-bytes "BORBA_CAD_LOG_MAX_BYTES"))))
        (when max-files
          (setf (config-log-max-files result)
                (parse-positive-integer-option max-files "BORBA_CAD_LOG_MAX_FILES")))
        (when to-stderr
          (setf (config-log-to-stderr result) (parse-boolean-option to-stderr)))
        (when sampling
          (setf (config-log-sampling result)
                (mapcar #'parse-sampling-option (uiop:split-string sampling :separator ","))))
        (when font
          (setf (config-font-path result) font))
        (when language
          (setf (config-language result) (parse-language-option language)))))
    result))

(defun config-with-arguments (config
                              arguments)
  "Apply core command-line options to a copy of CONFIG.
Returns two values: the new configuration and the arguments that were not recognized, in order.
Recognized: --log-dir DIR, --state-dir DIR, --drawings-dir DIR, --log-level LEVEL, --log-stderr,
--log-sample EVENT=N, --font PATH, --language TAG, --file PATH, --width N, --height N.

CONFIG: the configuration to start from.
ARGUMENTS: list of argument strings."
  (let ((result (copy-config config))
        (remaining '())
        (queue arguments))
    (flet ((next-value (option)
             (when (null (rest queue))
               (configuration-failure "~a needs a value" option))
             (pop queue)
             (first queue)))
      (loop while queue
            do (let ((option (first queue)))
                 (cond ((string= option "--log-dir")
                        (setf (config-log-directory result) (directory-pathname (next-value option))))
                       ((string= option "--state-dir")
                        (setf (config-state-directory result) (directory-pathname (next-value option))))
                       ((string= option "--drawings-dir")
                        (setf (config-drawings-directory result) (directory-pathname (next-value option))))
                       ((string= option "--log-level")
                        (setf (config-log-level result) (parse-log-level (next-value option))))
                       ((string= option "--log-stderr")
                        (setf (config-log-to-stderr result) t))
                       ((string= option "--log-sample")
                        (push (parse-sampling-option (next-value option))
                              (config-log-sampling result)))
                       ((string= option "--font")
                        (setf (config-font-path result) (next-value option)))
                       ((string= option "--language")
                        (setf (config-language result) (parse-language-option (next-value option))))
                       ((string= option "--file")
                        (setf (config-initial-file result) (next-value option)))
                       ((string= option "--width")
                        (setf (config-window-width result)
                              (parse-positive-integer-option (next-value option) option)))
                       ((string= option "--height")
                        (setf (config-window-height result)
                              (parse-positive-integer-option (next-value option) option)))
                       (t (push option remaining)))
                 (pop queue))))
    (values result (nreverse remaining))))
