(in-package #:borba-cad/bench)

;;;; The benchmark runner: builds each workload, applies every scenario, writes the report.

(defparameter *workload-sizes*
  '((:w0 . 0)
    (:w1 . 1000)
    (:w2 . 10000)
    (:w3 . 100000))
  "Named workloads and their entity counts.")

(defconstant +quick-repetition-factor+ 1/20
  "Fraction of the normal repetitions that the quick mode runs.")

(defconstant +quick-minimum-repetitions+ 2
  "Fewest repetitions of any operation in the quick mode.")

(defparameter *benchmark-log-file-name* "benchmark.log"
  "Log file of the benchmark runs, next to the program's own log.")

(defun workload-sizes ()
  "Return a fresh copy of the named workloads and their entity counts."
  (copy-alist *workload-sizes*))

(defun filename-timestamp ()
  "Return the current UTC time as text usable in a file name, like 20261013T231500Z."
  (remove-if (lambda (character) (find character "-:."))
             (sbcl:current-utc-timestamp)))

(defun sanitize-file-name-part (text)
  "Return TEXT with every character outside letters, digits and hyphens replaced by a hyphen.

TEXT: a host name or label."
  (substitute-if #\- (lambda (character) (not (or (alphanumericp character) (char= character #\-)))) text))

(defun progress (stream
                 control
                 &rest arguments)
  "Print one progress line and flush it, so long runs stay observable.

STREAM: where progress goes, or NIL for silence.
CONTROL: a format control string.
ARGUMENTS: format arguments."
  (when stream
    (apply #'format stream control arguments)
    (terpri stream)
    (force-output stream)))

(defun open-benchmark-logger (config
                              log-directory)
  "Open the benchmark log.

CONFIG: the application configuration.
LOG-DIRECTORY: directory of the log, or NIL for the configured one."
  (let ((log-config (base:copy-config config)))
    (when log-directory
      (setf (base:config-log-directory log-config) log-directory))
    (obs:open-logger log-config :file-name *benchmark-log-file-name*)))

(defun log-scenario (logger
                     scenario
                     workload
                     entity-count
                     started)
  "Write the bench.run log entry of one finished scenario.

LOGGER: the benchmark logger, or NIL.
SCENARIO: label of the scenario.
WORKLOAD: keyword naming the workload, or NIL for a scenario of its own.
ENTITY-COUNT: entities in the workload, or NIL.
STARTED: monotonic time at which the scenario began, in nanoseconds."
  (obs:log-event logger
                 :info
                 "bench.run"
                 :scenario scenario
                 :workload (if workload (string-downcase workload) "none")
                 :entity-count (or entity-count 0)
                 :duration-ms (obs:nanoseconds->milliseconds (- (sbcl:monotonic-nanoseconds) started))
                 :result :success))

(defun run-workload (workload
                     entity-count
                     directory
                     logger
                     progress-stream)
  "Generate one workload, apply every scenario to it and return its workload result.

WORKLOAD: keyword naming it.
ENTITY-COUNT: entities to generate.
DIRECTORY: scratch directory for files and logs.
LOGGER: the benchmark logger, or NIL.
PROGRESS-STREAM: where progress lines go, or NIL."
  (progress progress-stream
            "~(~a~): generating ~:d entities"
            workload
            entity-count)
  (let* ((heap-before (heap-after-full-gc-bytes))
         (started (sbcl:monotonic-nanoseconds))
         (drawing (rng:generate-drawing :seed +benchmark-seed+ :entity-count entity-count))
         (generated (sbcl:monotonic-nanoseconds))
         (heap-after (heap-after-full-gc-bytes))
         (result (make-workload-result workload
                                       entity-count
                                       (rng:drawing-checksum drawing)
                                       (/ (- generated started) sbcl:+nanoseconds-per-millisecond+ 1d0)
                                       (- heap-after heap-before))))
    (flet ((apply-scenario (label function)
             (progress progress-stream
                       "~(~a~): ~a"
                       workload
                       label)
             (let* ((began (sbcl:monotonic-nanoseconds))
                    (measurements (funcall function)))
               (dolist (measurement measurements)
                 (setf (measurement-workload measurement) workload))
               (setf (workload-result-measurements result)
                     (append (workload-result-measurements result) measurements))
               (log-scenario logger
                             label
                             workload
                             entity-count
                             began))))
      (apply-scenario "scene, all in view"
                      (lambda () (measure-scene-all-in-view drawing entity-count)))
      (apply-scenario "scene, pan sweep"
                      (lambda () (measure-scene-pan drawing entity-count)))
      (apply-scenario "scene, zoom sweep"
                      (lambda () (measure-scene-zoom drawing entity-count)))
      (apply-scenario "picking"
                      (lambda () (measure-picking drawing entity-count)))
      (apply-scenario "window selection"
                      (lambda () (measure-window-selection drawing entity-count)))
      (apply-scenario "snapping"
                      (lambda () (measure-snapping drawing entity-count)))
      (apply-scenario "persistence"
                      (lambda () (measure-persistence drawing
                                                      entity-count
                                                      directory)))
      (apply-scenario "editing"
                      (lambda () (measure-editing drawing entity-count))))
    (progress progress-stream
              "~(~a~): memory soak"
              workload)
    (let ((began (sbcl:monotonic-nanoseconds)))
      (setf (workload-result-soak result) (run-memory-soak drawing))
      (log-scenario logger
                    "memory soak"
                    workload
                    entity-count
                    began))
    result))

(defun call-with-scratch-directory (function)
  "Call FUNCTION with a fresh scratch directory, which is deleted afterwards.

FUNCTION: function of one argument, the directory pathname."
  (let ((directory (uiop:ensure-directory-pathname
                    (merge-pathnames (format nil "borba-cad-bench-~a-~d/"
                                             (filename-timestamp)
                                             (sbcl:process-id))
                                     (uiop:temporary-directory)))))
    (ensure-directories-exist directory)
    (unwind-protect (funcall function directory)
      (uiop:delete-directory-tree directory :validate t :if-does-not-exist :ignore))))

(defun run-benchmarks (&key (workloads (mapcar #'car *workload-sizes*))
                            (sizes *workload-sizes*)
                            (quick nil)
                            (overhead t)
                            output-directory
                            log-directory
                            (progress-stream *standard-output*)
                            (config (base:default-config)))
  "Run the benchmarks and write the report. Returns the report pathname. Every scenario also
leaves a bench.run entry in the benchmark log.

WORKLOADS: keywords of the workloads to run, a subset of the keys of SIZES.
SIZES: alist of workload keyword and entity count.
QUICK: true for a short run with few repetitions, meant to prove that the harness works.
OVERHEAD: true to also measure the instrumentation overhead.
OUTPUT-DIRECTORY: where the report is written; defaults to bench/reports/ of the project.
LOG-DIRECTORY: where the benchmark log is written; defaults to the configured log directory.
PROGRESS-STREAM: where progress lines go, or NIL for silence.
CONFIG: the application configuration, which supplies the project root."
  (let* ((*repetition-factor* (if quick +quick-repetition-factor+ *repetition-factor*))
         (*minimum-repetitions* (if quick +quick-minimum-repetitions+ *minimum-repetitions*))
         (fingerprint (environment-fingerprint config))
         (directory (or output-directory
                        (merge-pathnames "bench/reports/" (base:config-project-root config))))
         (logger (open-benchmark-logger config log-directory)))
    (ensure-directories-exist directory)
    (unwind-protect
         (call-with-scratch-directory
          (lambda (scratch)
            (benchmark-session workloads
                               sizes
                               overhead
                               scratch
                               directory
                               logger
                               fingerprint
                               quick
                               progress-stream)))
      (obs:close-logger logger))))

(defun benchmark-session (workloads
                          sizes
                          overhead
                          scratch
                          directory
                          logger
                          fingerprint
                          quick
                          progress-stream)
  "Run the workloads and the overhead measurement, write the report and return its path.

WORKLOADS: keywords of the workloads to run.
SIZES: alist of workload keyword and entity count.
OVERHEAD: true to measure the instrumentation overhead too.
SCRATCH: scratch directory for files and logs.
DIRECTORY: where the report goes.
LOGGER: the benchmark logger.
FINGERPRINT: the environment fingerprint.
QUICK: true when this is a quick run, which the report file name records.
PROGRESS-STREAM: where progress lines go, or NIL."
  (let* ((results (mapcar (lambda (workload)
                            (let ((entry (assoc workload sizes)))
                              (unless entry
                                (error 'base:configuration-error
                                       :reason (format nil "unknown workload ~s; use one of ~{~(~a~)~^, ~}"
                                                       workload
                                                       (mapcar #'car sizes))))
                              (run-workload workload
                                            (cdr entry)
                                            scratch
                                            logger
                                            progress-stream)))
                          workloads))
         (overhead-measurements (when overhead
                                  (progress progress-stream "instrumentation overhead")
                                  (let ((began (sbcl:monotonic-nanoseconds)))
                                    (prog1 (measure-instrumentation-overhead scratch)
                                      (log-scenario logger
                                                    "instrumentation overhead"
                                                    nil
                                                    nil
                                                    began)))))
         (path (merge-pathnames (format nil "~a-~a~:[~;-quick~].txt"
                                        (filename-timestamp)
                                        (sanitize-file-name-part (sbcl:host-name-string))
                                        quick)
                                directory)))
    (with-open-file (stream path
                            :direction :output
                            :if-exists :supersede
                            :external-format :utf-8)
      (write-report stream
                    fingerprint
                    results
                    overhead-measurements))
    (progress progress-stream
              "report written to ~a"
              (uiop:native-namestring path))
    path))

;;; Command line

(defparameter *usage*
  "Usage: make bench [BENCH_ARGS=\"...\"]

  --workloads w0,w1,w2,w3   workloads to run (default: all)
  --quick                   short run with few repetitions; proves the harness works
  --no-overhead             skip the instrumentation overhead measurement
  --output DIRECTORY        where to write the report (default: bench/reports/)
  --help                    show this text
"
  "Help text of the benchmark command.")

(defun parse-workload-list (text)
  "Parse a comma separated list of workload names into keywords.

TEXT: for example \"w0,w2\"."
  (mapcar (lambda (name)
            (intern (string-upcase (string-trim " " name)) :keyword))
          (uiop:split-string text :separator ",")))

(defun parse-benchmark-arguments (arguments)
  "Parse the command line into a plist of RUN-BENCHMARKS keywords plus :help.

ARGUMENTS: list of strings."
  (let ((options '()))
    (loop while arguments
          do (let ((argument (pop arguments)))
               (flet ((value-of ()
                        (or (pop arguments)
                            (error 'base:configuration-error
                                   :reason (format nil "~a expects a value" argument)))))
                 (cond ((string= argument "--workloads")
                        (setf (getf options :workloads) (parse-workload-list (value-of))))
                       ((string= argument "--quick")
                        (setf (getf options :quick) t))
                       ((string= argument "--no-overhead")
                        (setf (getf options :overhead) nil))
                       ((string= argument "--output")
                        (setf (getf options :output-directory)
                              (uiop:ensure-directory-pathname (value-of))))
                       ((string= argument "--help")
                        (setf (getf options :help) t))
                       (t (error 'base:configuration-error
                                 :reason (format nil "unknown benchmark option ~s" argument)))))))
    options))

(defun main ()
  "Command-line entry point of the benchmarks. Exit status: 0 on success, 2 for a bad option,
1 for any other failure."
  (handler-case
      (let ((options (parse-benchmark-arguments (sbcl:command-line-arguments))))
        (if (getf options :help)
            (write-string *usage*)
            (apply #'run-benchmarks (loop for (key value) on options by #'cddr
                                          unless (eq key :help)
                                            append (list key value)))))
    (base:configuration-error (condition)
      (format *error-output* "bench: ~a~%~%~a" condition *usage*)
      (finish-output *error-output*)
      (sb-ext:exit :code 2 :abort t))
    (error (condition)
      (format *error-output* "bench: ~a~%" condition)
      (finish-output *error-output*)
      (sb-ext:exit :code 1 :abort t)))
  (finish-output)
  (sb-ext:exit :code 0))
