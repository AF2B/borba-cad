(in-package #:borba-cad/bench)

;;;; The benchmark report: a plain text file a person can read and diff. Every number in it
;;;; was measured by this run; sections that could not be measured say so.

(defstruct (workload-result (:constructor make-workload-result (workload
                                                                entity-count
                                                                checksum
                                                                generate-milliseconds
                                                                retained-bytes)))
  "What is known about one workload besides its timings.

WORKLOAD: keyword naming it (for example :w2).
ENTITY-COUNT: entities in the drawing.
CHECKSUM: checksum of the drawing text, proving that two runs measured the same input.
GENERATE-MILLISECONDS: time to generate the drawing.
RETAINED-BYTES: heap retained by the drawing (after a full GC, minus the heap before it existed).
MEASUREMENTS: the measurements taken on it.
SOAK: the memory-soak series, a list of (label . bytes)."
  workload
  entity-count
  checksum
  generate-milliseconds
  retained-bytes
  (measurements nil)
  (soak nil))

(defconstant +report-rule-width+ 100
  "Width of the horizontal rules in the report.")

(defparameter *not-measured-statements*
  '(("GPU time and overdraw: no vendor-neutral timer is used; the windowed benchmark"
     "(make bench-gui) reports CPU build/submit time, the time glFinish waits and swap time instead.")
    ("Wall-clock GC pauses: SBCL exposes GC CPU time (*gc-run-time*); with the application's"
     "single working thread it approximates the pause.")
    ("Energy and power use."))
  "What this report does not measure, one statement per entry, as a list of lines. Stated so
that nobody reads the report as covering more.")

(defun report-rule (stream
                    character)
  "Write a horizontal rule.

STREAM: the output stream.
CHARACTER: the character it is drawn with."
  (format stream "~v,,,vA~%" +report-rule-width+ character ""))

(defun format-bytes-mib (bytes)
  "Return BYTES as a string in MiB with one decimal.

BYTES: a number of bytes."
  (format nil "~a MiB" (base:format-decimal (/ bytes +bytes-per-mebibyte+) 1)))

(defun format-sample (value)
  "Format a timing value with a precision proportional to its magnitude.

VALUE: a non-negative real number."
  (base:format-decimal value (cond ((< value 10) 3)
                                   ((< value 1000) 2)
                                   (t 1))))

(defun write-environment (fingerprint
                          stream)
  "Write the environment section.

FINGERPRINT: alist of labels and values.
STREAM: the output stream."
  (format stream "ENVIRONMENT~%")
  (report-rule stream #\-)
  (loop for (label . value) in fingerprint
        do (format stream "  ~30a ~a~%" label value))
  (terpri stream))

(defun write-workloads (results
                        stream)
  "Write the workload table.

RESULTS: list of workload results.
STREAM: the output stream."
  (format stream "WORKLOADS (generated with the seed above; equal checksums mean identical drawings)~%")
  (report-rule stream #\-)
  (format stream "  ~8a ~10@a  ~16a ~14@a ~16@a ~18@a~%"
          "name" "entities" "checksum" "generate (ms)" "retained heap" "heap per entity")
  (dolist (result results)
    (let ((count (workload-result-entity-count result)))
      (format stream "  ~8a ~10:d  ~16a ~14a ~16a ~18a~%"
              (workload-result-workload result)
              count
              (workload-result-checksum result)
              (base:format-decimal (workload-result-generate-milliseconds result) 1)
              (format-bytes-mib (workload-result-retained-bytes result))
              (if (plusp count)
                  (format nil "~:d B" (round (workload-result-retained-bytes result) count))
                  "n/a"))))
  (terpri stream))

(defun write-measurement (measurement
                          stream)
  "Write one measurement row and its notes.

MEASUREMENT: the measurement.
STREAM: the output stream."
  (let ((summary (measurement-summary measurement)))
    (if summary
        (format stream "  ~48a ~7:d ~10@a ~10@a ~10@a ~10@a  ~(~a~)~%"
                (measurement-name measurement)
                (obs:sample-summary-count summary)
                (format-sample (obs:sample-summary-mean summary))
                (format-sample (obs:sample-summary-p50 summary))
                (format-sample (obs:sample-summary-p95 summary))
                (format-sample (obs:sample-summary-max summary))
                (measurement-unit measurement))
        (format stream "  ~48a no samples~%" (measurement-name measurement))))
  (dolist (note (reverse (measurement-notes measurement)))
    (format stream "      ~a~%" note)))

(defun write-measurements-header (stream)
  "Write the column headings of the timing tables.

STREAM: the output stream."
  (format stream "  ~48a ~7@a ~10@a ~10@a ~10@a ~10@a  ~a~%"
          "operation" "samples" "mean" "p50" "p95" "max" "unit"))

(defun write-soak (series
                   stream)
  "Write the memory-soak series and the verdict that follows from it.

SERIES: list of (label . bytes).
STREAM: the output stream."
  (format stream "  memory soak: ~d cycles of creating and deleting ~d entities through the history~%"
          +soak-cycles+
          +soak-entities-per-cycle+)
  (format stream "  heap in use after a full GC:~%")
  (loop for (label . bytes) in series
        do (format stream "      ~24a ~a~%" label (format-bytes-mib bytes)))
  (let* ((checkpoints (remove-if-not (lambda (entry) (search "cycles" (car entry))) series))
         (last-two (last checkpoints 2)))
    (when (= 2 (length last-two))
      (format stream "  growth between the last two checkpoints (history already full): ~a~%"
              (format-bytes-mib (- (cdr (second last-two)) (cdr (first last-two))))))))

(defun write-workload-section (result
                               stream)
  "Write the timing table and memory facts of one workload.

RESULT: the workload result.
STREAM: the output stream."
  (format stream "~a  (~:d entities)~%"
          (workload-result-workload result)
          (workload-result-entity-count result))
  (report-rule stream #\-)
  (write-measurements-header stream)
  (dolist (measurement (workload-result-measurements result))
    (write-measurement measurement stream))
  (when (workload-result-soak result)
    (terpri stream)
    (write-soak (workload-result-soak result) stream))
  (terpri stream))

(defun write-overhead (measurements
                       stream)
  "Write the instrumentation-overhead comparison.

MEASUREMENTS: one measurement per instrumentation mode, the first being the baseline.
STREAM: the output stream."
  (format stream "INSTRUMENTATION OVERHEAD (~d entities, ~d interleaved rounds of ~d frames per mode)~%"
          +overhead-entities+
          +overhead-rounds+
          +overhead-frames-per-round+)
  (format stream "  overlay = info-level logging plus the live performance overlay drawn on every frame~%")
  (report-rule stream #\-)
  (write-measurements-header stream)
  (dolist (measurement measurements)
    (write-measurement measurement stream))
  (let* ((baseline (obs:sample-summary-mean (measurement-summary (first measurements)))))
    (format stream "~%  difference from the first row, by mean frame time (the off-control row is the~%")
    (format stream "  same uninstrumented setup again: its difference is the noise floor of this run):~%")
    (dolist (measurement (rest measurements))
      (let ((mean (obs:sample-summary-mean (measurement-summary measurement))))
        (format stream "      ~48a ~a us per frame (~a%)~%"
                (measurement-name measurement)
                (base:format-decimal (- mean baseline) 2)
                (base:format-decimal (* 100 (/ (- mean baseline) baseline)) 1)))))
  (let ((info (find "frame, instrumentation info" measurements
                    :key #'measurement-name
                    :test #'string=))
        (overlay (find "frame, instrumentation overlay" measurements
                       :key #'measurement-name
                       :test #'string=)))
    (when (and info overlay)
      (let ((difference (- (obs:sample-summary-mean (measurement-summary overlay))
                           (obs:sample-summary-mean (measurement-summary info)))))
        (format stream "~%  the overlay itself, over info-level logging: ~a us per frame~%"
                (base:format-decimal difference 2)))))
  (format stream "  A difference below the run-to-run noise (compare p50 and p95) is not a cost.~%")
  (terpri stream))

(defun write-report (stream
                     fingerprint
                     results
                     overhead)
  "Write the whole report.

STREAM: the output stream.
FINGERPRINT: alist describing the environment.
RESULTS: list of workload results.
OVERHEAD: list of measurements of the instrumentation modes, or NIL when not measured."
  (report-rule stream #\=)
  (format stream "BORBA CAD BENCHMARK REPORT~%")
  (report-rule stream #\=)
  (terpri stream)
  (write-environment fingerprint stream)
  (write-workloads results stream)
  (format stream "TIMINGS (per call; lower is better; every sample comes from the monotonic clock)~%")
  (report-rule stream #\=)
  (terpri stream)
  (dolist (result results)
    (write-workload-section result stream))
  (when overhead
    (write-overhead overhead stream))
  (format stream "NOT MEASURED~%")
  (report-rule stream #\-)
  (dolist (statement *not-measured-statements*)
    (format stream "  - ~a~%" (first statement))
    (dolist (continuation (rest statement))
      (format stream "    ~a~%" continuation)))
  (terpri stream)
  (format stream "No performance target is asserted by this report; targets are proposed in docs/performance.md.~%"))
