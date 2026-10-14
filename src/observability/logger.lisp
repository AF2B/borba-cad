(in-package #:borba-cad/observability)

;;;; Structured, human-readable log lines:
;;;;   2026-10-13T21:00:00.123Z INFO geometry.pick duration_ms=0.412 entity_count=1500 result=success
;;;; The logger is used from the main thread only. Each entry is flushed immediately.

(defparameter *log-file-name* "borba-cad.log"
  "Name of the current log file; rotated files append .1, .2, ...")

(defstruct (logger (:constructor %make-logger))
  "A rotating log file sink.

DIRECTORY: directory holding the log files.
LEVEL: least severe level written.
MAX-BYTES: size at which the file is rotated.
MAX-FILES: number of files kept, including the current one.
TO-STDERR: also echo each line to standard error.
SAMPLING: alist of event name and N; one in N entries of that event is written.
STREAM: the open file stream, or NIL when file logging is unavailable.
PATH: pathname of the current log file.
COUNTERS: per-event entry counters used for sampling.
FAILURE: description of why file logging stopped, or NIL."
  (directory nil)
  (level :info :type keyword)
  (max-bytes base:+default-log-max-bytes+ :type integer)
  (max-files base:+default-log-max-files+ :type integer)
  (to-stderr nil)
  (sampling nil :type list)
  (stream nil)
  (path nil)
  (counters (make-hash-table :test 'equal) :type hash-table)
  (failure nil))

(defun severity-rank (severity)
  "Return the rank of SEVERITY: 0 for the most severe.

SEVERITY: a log level keyword."
  (or (position severity (base:log-levels))
      (error "Unknown log severity ~s" severity)))

(defun log-enabled-p (logger
                      severity)
  "Return true when entries of SEVERITY are written.

LOGGER: the logger, or NIL.
SEVERITY: a log level keyword."
  (and logger
       (<= (severity-rank severity) (severity-rank (logger-level logger)))))

(defun log-file-names (logger)
  "Return the pathnames of the current log file and every rotated file that exists, newest
first.

LOGGER: the logger."
  (let ((base-name (uiop:native-namestring (logger-path logger))))
    (cons (logger-path logger)
          (loop for index from 1 below (logger-max-files logger)
                for path = (uiop:parse-native-namestring (format nil "~a.~d" base-name index))
                when (probe-file path)
                  collect path))))

(defun disable-file-logging (logger
                             control
                             &rest arguments)
  "Stop writing the log file after a failure and report it once on standard error.

LOGGER: the logger.
CONTROL: format control string describing the failure.
ARGUMENTS: format arguments."
  (let ((reason (apply #'format nil control arguments)))
    (setf (logger-failure logger) reason)
    (when (logger-stream logger)
      (handler-case (close (logger-stream logger) :abort t)
        (error (condition)
          (format *error-output* "~&WARNING: the log file could not be closed cleanly: ~a~%" condition))))
    (setf (logger-stream logger) nil)
    (format *error-output* "~&WARNING: file logging disabled: ~a~%" reason)
    (finish-output *error-output*)))

(defun open-log-stream (logger)
  "Open the current log file for appending and record its size.

LOGGER: the logger."
  (setf (logger-stream logger)
        (open (logger-path logger)
              :direction :output
              :if-exists :append
              :if-does-not-exist :create
              :external-format :utf-8)))

(defun open-logger (config
                    &key (file-name *log-file-name*))
  "Create a logger from CONFIG and open its file. When the directory or file cannot be used,
a warning is printed on standard error and the logger runs without a file.

CONFIG: the application configuration.
FILE-NAME: name of the current log file inside the log directory."
  (let* ((directory (base:config-log-directory config))
         (logger (%make-logger :directory directory
                               :level (base:config-log-level config)
                               :max-bytes (base:config-log-max-bytes config)
                               :max-files (base:config-log-max-files config)
                               :to-stderr (base:config-log-to-stderr config)
                               :sampling (base:config-log-sampling config)
                               :path (merge-pathnames file-name directory))))
    (handler-case (progn (ensure-directories-exist directory)
                         (open-log-stream logger))
      (error (condition)
        (disable-file-logging logger
                              "cannot open ~a: ~a"
                              (uiop:native-namestring (logger-path logger))
                              condition)))
    logger))

(defun close-logger (logger)
  "Flush and close the log file.

LOGGER: the logger."
  (when (logger-stream logger)
    (finish-output (logger-stream logger))
    (close (logger-stream logger))
    (setf (logger-stream logger) nil))
  logger)

(defun rotated-path (logger
                     index)
  "Return the path of the INDEX-th rotated file.

LOGGER: the logger.
INDEX: 1 for the newest rotated file."
  (uiop:parse-native-namestring
   (format nil "~a.~d" (uiop:native-namestring (logger-path logger)) index)))

(defun rotate-log (logger)
  "Rotate the files: the current file becomes .1, .1 becomes .2, and so on; the oldest is
deleted so at most MAX-FILES remain.

LOGGER: the logger."
  (close (logger-stream logger))
  (setf (logger-stream logger) nil)
  (let ((oldest (1- (logger-max-files logger))))
    (when (plusp oldest)
      (sbcl:delete-file-if-exists (rotated-path logger oldest))
      (loop for index from (1- oldest) downto 1
            when (probe-file (rotated-path logger index))
              do (sbcl:rename-over (rotated-path logger index) (rotated-path logger (1+ index))))
      (sbcl:rename-over (logger-path logger) (rotated-path logger 1)))
    (when (zerop oldest)
      (sbcl:delete-file-if-exists (logger-path logger))))
  (open-log-stream logger))

(defun write-log-line (logger
                       line)
  "Append LINE to the log file, rotating first when it would exceed the size limit, and echo
it to standard error when configured. A write failure disables file logging.

LOGGER: the logger.
LINE: the complete line without a newline."
  (when (logger-to-stderr logger)
    (write-line line *error-output*))
  (when (logger-stream logger)
    (handler-case
        (let ((stream (logger-stream logger)))
          (when (and (plusp (file-position stream))
                     (> (+ (file-position stream) (length line) 1) (logger-max-bytes logger)))
            (rotate-log logger)
            (setf stream (logger-stream logger)))
          (write-line line stream)
          (finish-output stream))
      (error (condition)
        (disable-file-logging logger
                              "cannot write ~a: ~a"
                              (uiop:native-namestring (logger-path logger))
                              condition)))))

(defun log-key-name (key)
  "Return the text of a field key: lower case with underscores, for example duration_ms.

KEY: a keyword or string."
  (substitute #\_ #\- (string-downcase (string key))))

(defun plain-log-value-p (text)
  "Return true when TEXT can be printed without quotes.

TEXT: a string."
  (and (plusp (length text))
       (every (lambda (character)
                (or (alphanumericp character)
                    (find character "._:/+-%")))
              text)
       (every (lambda (character) (< (char-code character) 128)) text)))

(defun format-log-value (value)
  "Return the text of a field value: numbers as digits (doubles with three decimals), keywords
in lower case, booleans as true/false, other strings quoted when needed.

VALUE: an integer, double-float, keyword, boolean or string."
  (typecase value
    (integer (format nil "~d" value))
    (float (base:format-decimal value 3))
    (null "false")
    ((eql t) "true")
    (symbol (string-downcase (symbol-name value)))
    (string (if (plain-log-value-p value)
                value
                (with-output-to-string (out)
                  (write-char #\" out)
                  (loop for character across value
                        do (cond ((member character '(#\" #\\))
                                  (write-char #\\ out)
                                  (write-char character out))
                                 ((char= character #\Newline) (write-string "\\n" out))
                                 ((< (char-code character) 32) (write-char #\Space out))
                                 (t (write-char character out))))
                  (write-char #\" out))))
    (t (format-log-value (princ-to-string value)))))

(defun format-log-line (timestamp
                        severity
                        event
                        fields)
  "Build one log line.

TIMESTAMP: ISO-8601 UTC text.
SEVERITY: a log level keyword.
EVENT: dotted event name such as geometry.pick.
FIELDS: property list of keys and values."
  (with-output-to-string (out)
    (format out "~a ~a ~a" timestamp (symbol-name severity) event)
    (loop for (key value) on fields by #'cddr
          do (format out " ~a=~a" (log-key-name key) (format-log-value value)))))

(defun sampling-allows-p (logger
                          event)
  "Return the sampling rate N when this entry of EVENT should be written (N is 1 for events
that are not sampled), or NIL when it should be skipped.

LOGGER: the logger.
EVENT: event name."
  (let ((rate (cdr (assoc event (logger-sampling logger) :test #'string=))))
    (if (or (null rate) (<= rate 1))
        1
        (let ((count (incf (gethash event (logger-counters logger) 0))))
          (and (= 1 (mod count rate)) rate)))))

(defun log-event (logger
                  severity
                  event
                  &rest fields)
  "Write one entry when SEVERITY is enabled and sampling allows it. A NIL logger discards.

LOGGER: the logger, or NIL.
SEVERITY: :error, :warn, :info, :debug or :trace.
EVENT: dotted event name.
FIELDS: alternating keys and values."
  (when (log-enabled-p logger severity)
    (let ((rate (sampling-allows-p logger event)))
      (when rate
        (write-log-line logger
                        (format-log-line (sbcl:current-utc-timestamp)
                                         severity
                                         event
                                         (if (> rate 1)
                                             (append fields (list :sampled-one-in rate))
                                             fields)))))))

(defun log-condition (logger
                      severity
                      event
                      condition
                      &rest fields)
  "Write an entry describing a condition: its type and message become fields.

LOGGER: the logger, or NIL.
SEVERITY: log level keyword.
EVENT: dotted event name.
CONDITION: the condition object.
FIELDS: extra alternating keys and values."
  (apply #'log-event logger severity event
         (append fields
                 (list :result :error
                       :error (string-downcase (symbol-name (type-of condition)))
                       :message (remove #\Newline (princ-to-string condition))))))

;;; Parsing, used by the log summarizer and by tests.

(defstruct (log-entry (:constructor make-log-entry (timestamp
                                                    severity
                                                    event
                                                    fields)))
  "One parsed log line.

TIMESTAMP: the timestamp text.
SEVERITY: severity name text.
EVENT: event name.
FIELDS: alist of field name and value text."
  timestamp
  severity
  event
  fields)

(defun log-entry-field (entry
                        name)
  "Return the text of field NAME of ENTRY, or NIL.

ENTRY: a log entry.
NAME: field name such as duration_ms."
  (cdr (assoc name (log-entry-fields entry) :test #'string=)))

(defun split-log-fields (text)
  "Split the key=value part of a log line into an alist, honoring quoted values.

TEXT: the text after the event name."
  (let ((fields '())
        (position 0)
        (length (length text)))
    (loop
      (loop while (and (< position length) (char= (char text position) #\Space))
            do (incf position))
      (when (>= position length)
        (return))
      (let ((equals (position #\= text :start position)))
        (unless equals
          (return))
        (let ((key (subseq text position equals))
              (value-start (1+ equals))
              value)
          (if (and (< value-start length) (char= (char text value-start) #\"))
              (let ((out (make-string-output-stream))
                    (index (1+ value-start)))
                (loop while (and (< index length) (char/= (char text index) #\"))
                      do (when (and (char= (char text index) #\\) (< (1+ index) length))
                           (incf index))
                         (write-char (char text index) out)
                         (incf index))
                (setf value (get-output-stream-string out)
                      position (min length (1+ index))))
              (let ((end (or (position #\Space text :start value-start) length)))
                (setf value (subseq text value-start end)
                      position end)))
          (push (cons key value) fields))))
    (nreverse fields)))

(defun parse-log-line (line)
  "Parse a log line into a LOG-ENTRY, or return NIL when it is not a log line.

LINE: one line of a log file."
  (let* ((first-space (position #\Space line))
         (second-space (and first-space (position #\Space line :start (1+ first-space))))
         (third-space (and second-space (position #\Space line :start (1+ second-space)))))
    (when (and second-space (>= first-space 20))
      (make-log-entry (subseq line 0 first-space)
                      (subseq line (1+ first-space) second-space)
                      (subseq line (1+ second-space) (or third-space (length line)))
                      (and third-space (split-log-fields (subseq line (1+ third-space))))))))

(defun discover-log-files (directory
                           &key (file-name *log-file-name*))
  "Return the log files found in DIRECTORY, oldest first: the highest rotation number first,
the current file last.

DIRECTORY: directory to look in.
FILE-NAME: name of the current log file."
  (let* ((base-name (uiop:native-namestring (merge-pathnames file-name directory)))
         (rotated (loop for index from 1 to 99
                        for path = (uiop:parse-native-namestring (format nil "~a.~d" base-name index))
                        while (probe-file path)
                        collect path))
         (current (uiop:parse-native-namestring base-name)))
    (append (reverse rotated)
            (and (probe-file current) (list current)))))
