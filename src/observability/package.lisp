(defpackage #:borba-cad/observability
  (:use #:cl)
  (:local-nicknames (#:base #:borba-cad/base)
                    (#:sbcl #:borba-cad/sbcl-support))
  (:documentation "Structured logging with rotation, operation timing, interval aggregation,
GC and memory sampling, the live performance snapshot and the log summarizer.")
  (:export
   ;; Statistics
   #:percentile
   #:sample-set
   #:make-sample-set
   #:sample-set-add
   #:sample-set-samples
   #:sample-set-clear
   #:sample-set-count
   #:summarize-samples
   #:sample-summary
   #:sample-summary-count
   #:sample-summary-mean
   #:sample-summary-p50
   #:sample-summary-p95
   #:sample-summary-max
   #:nanoseconds->milliseconds
   ;; Logger
   #:logger
   #:open-logger
   #:close-logger
   #:logger-path
   #:logger-failure
   #:log-enabled-p
   #:log-event
   #:log-condition
   #:format-log-line
   #:parse-log-line
   #:log-entry
   #:log-entry-timestamp
   #:log-entry-severity
   #:log-entry-event
   #:log-entry-fields
   #:log-entry-field
   #:log-file-names
   #:discover-log-files
   ;; Metrics and telemetry
   #:metrics
   #:make-metrics
   #:record-operation
   #:operation-stats
   #:operation-stats-count
   #:operation-stats-total-nanoseconds
   #:operation-stats-max-nanoseconds
   #:operation-stats-last-nanoseconds
   #:find-operation-stats
   #:metrics-entity-count
   #:metrics-visible-entity-count
   #:telemetry
   #:make-telemetry
   #:telemetry-logger
   #:telemetry-metrics
   #:telemetry-record-frame
   #:telemetry-record-input
   #:telemetry-record-viewport-update
   #:telemetry-set-entity-counts
   #:telemetry-tick
   #:close-telemetry
   #:telemetry-overlay-lines
   #:+gc-pause-warning-threshold-microseconds+
   ;; Timing
   #:call-with-operation
   #:with-operation
   #:record-operation-result
   ;; Log summary
   #:summarize-log-files
   #:format-log-summary
   #:log-summary
   #:log-summary-entries
   #:log-summary-unparsable
   #:log-summary-first-timestamp
   #:log-summary-last-timestamp
   #:log-summary-severities
   #:log-summary-timed-events
   #:log-summary-interval-rows
   #:event-summary
   #:event-summary-event
   #:event-summary-count
   #:event-summary-total-ms
   #:event-summary-mean-ms
   #:event-summary-p50-ms
   #:event-summary-p95-ms
   #:event-summary-max-ms
   #:event-summary-sample-rate
   #:interval-row
   #:interval-row-key
   #:interval-row-intervals
   #:interval-row-samples
   #:interval-row-weighted-mean-ms
   #:interval-row-worst-p95-ms
   #:interval-row-max-ms))
