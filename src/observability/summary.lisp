(in-package #:borba-cad/observability)

;;;; The log summarizer: reads the rotating log files and aggregates them, with no GUI. Timed
;;;; entries (duration_ms) are summarized exactly; interval summaries written by the telemetry
;;;; (count plus mean/p95/max) are combined as count-weighted means and worst-case p95/max,
;;;; because percentiles of percentiles are not meaningful.

(defstruct (event-summary (:constructor make-event-summary (event
                                                            count
                                                            total-ms
                                                            mean-ms
                                                            p50-ms
                                                            p95-ms
                                                            max-ms
                                                            sample-rate)))
  "Statistics of the duration_ms values of one event.

EVENT: event name.
COUNT: number of entries.
TOTAL-MS: sum of durations.
MEAN-MS: mean duration.
P50-MS: median.
P95-MS: 95th percentile.
MAX-MS: longest duration.
SAMPLE-RATE: N when the event was logged one in N, else 1."
  event count total-ms mean-ms p50-ms p95-ms max-ms sample-rate)

(defstruct (interval-row (:constructor make-interval-row (key
                                                          intervals
                                                          samples
                                                          weighted-mean-ms
                                                          worst-p95-ms
                                                          max-ms)))
  "Combined figures of one aggregated metric from interval summary entries.

KEY: event, optional type and metric, for example render.frame cpu.
INTERVALS: number of summary entries combined.
SAMPLES: total number of measurements they covered.
WEIGHTED-MEAN-MS: mean weighted by each interval's sample count.
WORST-P95-MS: the largest interval p95.
MAX-MS: the largest value seen."
  key intervals samples weighted-mean-ms worst-p95-ms max-ms)

(defstruct (log-summary (:constructor make-log-summary (entries
                                                        unparsable
                                                        first-timestamp
                                                        last-timestamp
                                                        severities
                                                        timed-events
                                                        interval-rows)))
  "The aggregated content of a set of log files.

ENTRIES: number of parsed entries.
UNPARSABLE: number of lines that were not log entries.
FIRST-TIMESTAMP: timestamp of the first entry, or NIL.
LAST-TIMESTAMP: timestamp of the last entry, or NIL.
SEVERITIES: alist of severity name and count.
TIMED-EVENTS: list of EVENT-SUMMARY, longest total time first.
INTERVAL-ROWS: list of INTERVAL-ROW, sorted by key."
  entries unparsable first-timestamp last-timestamp severities timed-events interval-rows)

(defun entry-number (entry
                     name)
  "Return field NAME of ENTRY as a double-float, or NIL when absent or not a number.

ENTRY: a log entry.
NAME: field name."
  (let ((text (log-entry-field entry name)))
    (and text
         (handler-case (base:parse-decimal text)
           (base:invalid-input () nil)))))

(defun interval-metric-prefixes (entry)
  "Return the metric prefixes of an interval summary entry, for example (\"cpu\" \"interval\")
for an entry with cpu_ms_mean and interval_ms_mean fields.

ENTRY: a log entry."
  (loop for (name) in (log-entry-fields entry)
        when (and (> (length name) 8)
                  (string= "_ms_mean" name :start2 (- (length name) 8)))
          collect (subseq name 0 (- (length name) 8))))

(defun accumulate-interval (table
                            entry)
  "Fold one interval summary entry into the per-key accumulators.

TABLE: hash table from row key to accumulator list (intervals samples weighted-sum worst-p95 max).
ENTRY: a log entry with count and *_ms_mean fields."
  (let ((count (entry-number entry "count"))
        (type (log-entry-field entry "type")))
    (when (and count (plusp count))
      (dolist (prefix (interval-metric-prefixes entry))
        (let ((mean (entry-number entry (format nil "~a_ms_mean" prefix)))
              (p95 (entry-number entry (format nil "~a_ms_p95" prefix)))
              (maximum (entry-number entry (format nil "~a_ms_max" prefix)))
              (key (format nil "~a~@[[~a]~] ~a" (log-entry-event entry) type prefix)))
          (when mean
            (let ((cell (or (gethash key table)
                            (setf (gethash key table) (list 0 0 0d0 0d0 0d0)))))
              (incf (first cell))
              (incf (second cell) count)
              (incf (third cell) (* mean count))
              (setf (fourth cell) (max (fourth cell) (or p95 0d0))
                    (fifth cell) (max (fifth cell) (or maximum 0d0))))))))))

(defun summarize-log-files (paths)
  "Read and aggregate log files, given oldest first.

PATHS: list of log file pathnames."
  (let ((entries 0)
        (unparsable 0)
        (first-timestamp nil)
        (last-timestamp nil)
        (severities (make-hash-table :test 'equal))
        (durations (make-hash-table :test 'equal))
        (rates (make-hash-table :test 'equal))
        (intervals (make-hash-table :test 'equal)))
    (dolist (path paths)
      (with-open-file (stream path :external-format '(:utf-8 :replacement #\?))
        (loop for line = (read-line stream nil nil)
              while line
              do (let ((entry (parse-log-line line)))
                   (cond ((null entry) (incf unparsable))
                         (t (incf entries)
                            (unless first-timestamp (setf first-timestamp (log-entry-timestamp entry)))
                            (setf last-timestamp (log-entry-timestamp entry))
                            (incf (gethash (log-entry-severity entry) severities 0))
                            (let ((duration (entry-number entry "duration_ms"))
                                  (rate (entry-number entry "sampled_one_in")))
                              (when duration
                                (sample-set-add (or (gethash (log-entry-event entry) durations)
                                                    (setf (gethash (log-entry-event entry) durations)
                                                          (make-sample-set)))
                                                duration))
                              (when rate
                                (setf (gethash (log-entry-event entry) rates) (round rate))))
                            (when (and (entry-number entry "count") (interval-metric-prefixes entry))
                              (accumulate-interval intervals entry))))))))
    (make-log-summary
     entries
     unparsable
     first-timestamp
     last-timestamp
     (sort (loop for severity being the hash-keys of severities using (hash-value count)
                 collect (cons severity count))
           #'> :key #'cdr)
     (sort (loop for event being the hash-keys of durations using (hash-value samples)
                 collect (let ((summary (summarize-samples samples)))
                           (make-event-summary event
                                               (sample-summary-count summary)
                                               (reduce #'+ (sample-set-samples samples))
                                               (sample-summary-mean summary)
                                               (sample-summary-p50 summary)
                                               (sample-summary-p95 summary)
                                               (sample-summary-max summary)
                                               (gethash event rates 1))))
           #'> :key #'event-summary-total-ms)
     (sort (loop for key being the hash-keys of intervals using (hash-value cell)
                 collect (destructuring-bind (count samples weighted worst-p95 maximum) cell
                           (make-interval-row key
                                              count
                                              samples
                                              (/ weighted samples)
                                              worst-p95
                                              maximum)))
           #'string< :key #'interval-row-key))))

(defun format-log-summary (summary
                           stream)
  "Print SUMMARY as plain-text tables.

SUMMARY: a LOG-SUMMARY.
STREAM: output stream."
  (format stream "Log summary: ~d entries (~d unparsable lines)~%"
          (log-summary-entries summary) (log-summary-unparsable summary))
  (when (log-summary-first-timestamp summary)
    (format stream "Period: ~a to ~a~%"
            (log-summary-first-timestamp summary) (log-summary-last-timestamp summary)))
  (format stream "Severities: ~:[none~;~:*~{~a ~d~^, ~}~]~%"
          (loop for (severity . count) in (log-summary-severities summary)
                append (list severity count)))
  (when (log-summary-timed-events summary)
    (format stream "~%Timed events (duration_ms of each entry)~%")
    (format stream "~34a ~8@a ~12@a ~10@a ~10@a ~10@a ~10@a~%"
            "event" "count" "total_ms" "mean_ms" "p50_ms" "p95_ms" "max_ms")
    (dolist (row (log-summary-timed-events summary))
      (format stream "~34a ~8d ~12@a ~10@a ~10@a ~10@a ~10@a~:[~; (logged 1 in ~d)~]~%"
              (event-summary-event row)
              (event-summary-count row)
              (base:format-decimal (event-summary-total-ms row) 3)
              (base:format-decimal (event-summary-mean-ms row) 3)
              (base:format-decimal (event-summary-p50-ms row) 3)
              (base:format-decimal (event-summary-p95-ms row) 3)
              (base:format-decimal (event-summary-max-ms row) 3)
              (> (event-summary-sample-rate row) 1)
              (event-summary-sample-rate row))))
  (when (log-summary-interval-rows summary)
    (format stream "~%Interval summaries (as logged; means are count-weighted, p95 is the worst interval)~%")
    (format stream "~34a ~10@a ~10@a ~12@a ~12@a ~10@a~%"
            "metric" "intervals" "samples" "mean_ms" "worst_p95_ms" "max_ms")
    (dolist (row (log-summary-interval-rows summary))
      (format stream "~34a ~10d ~10d ~12@a ~12@a ~10@a~%"
              (interval-row-key row)
              (interval-row-intervals row)
              (round (interval-row-samples row))
              (base:format-decimal (interval-row-weighted-mean-ms row) 3)
              (base:format-decimal (interval-row-worst-p95-ms row) 3)
              (base:format-decimal (interval-row-max-ms row) 3))))
  summary)
