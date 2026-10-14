(in-package #:borba-cad/observability)

(defconstant +gc-pause-warning-threshold-microseconds+ 5000
  "A collection pausing at least this long (5 ms, a third of a 60 Hz frame) is logged at
info level; shorter ones at debug level.")

(defconstant +frame-idle-gap-nanoseconds+ 250000000
  "A gap this long between two presented frames means the application was idle, not slow;
such gaps are left out of the frame-interval statistics.")

(defconstant +bytes-per-mebibyte+ 1048576d0
  "Bytes in one mebibyte, as a double for conversions.")

(defstruct (telemetry (:constructor %make-telemetry (config
                                                     logger
                                                     metrics
                                                     summary-interval-nanoseconds
                                                     memory-interval-nanoseconds)))
  "The application's observability hub: logger, live metrics and the interval aggregators.

CONFIG: the application configuration.
LOGGER: the rotating log sink.
METRICS: live figures for the overlay.
SUMMARY-INTERVAL-NANOSECONDS: period of the aggregated summary entries.
MEMORY-INTERVAL-NANOSECONDS: period of memory samples.
FRAME-CPU: per-frame CPU time samples (ms) of the current interval.
FRAME-SWAP: per-frame buffer-swap time samples (ms).
FRAME-INTERVAL: time between consecutive presented frames (ms).
INPUT-SAMPLES: event type to handling-time samples (ms).
VIEWPORT-SAMPLES: viewport update time samples (ms).
LAST-PRESENT-NANOSECONDS: monotonic time of the previous presented frame.
NEXT-SUMMARY-NANOSECONDS: monotonic time of the next summary flush.
NEXT-MEMORY-NANOSECONDS: monotonic time of the next memory sample."
  config
  logger
  metrics
  (summary-interval-nanoseconds 0 :type integer)
  (memory-interval-nanoseconds 0 :type integer)
  (frame-cpu (make-sample-set))
  (frame-swap (make-sample-set))
  (frame-interval (make-sample-set))
  (input-samples (make-hash-table :test 'equal) :type hash-table)
  (viewport-samples (make-sample-set))
  (last-present-nanoseconds nil)
  (next-summary-nanoseconds 0 :type integer)
  (next-memory-nanoseconds 0 :type integer))

(defun seconds->nanoseconds (seconds)
  "Convert seconds to integer nanoseconds.

SECONDS: a real number."
  (round (* seconds sbcl:+nanoseconds-per-second+)))

(defun make-telemetry (config
                       &key (logger (open-logger config))
                            (now (sbcl:monotonic-nanoseconds)))
  "Create the telemetry and start recording GC events.

CONFIG: the application configuration.
LOGGER: the log sink; defaults to a logger opened from CONFIG.
NOW: current monotonic time, injectable for tests."
  (let* ((summary-interval (seconds->nanoseconds (base:config-frame-summary-interval-seconds config)))
         (memory-interval (seconds->nanoseconds (base:config-memory-sample-interval-seconds config)))
         (telemetry (%make-telemetry config
                                     logger
                                     (make-metrics)
                                     summary-interval
                                     memory-interval)))
    (setf (telemetry-next-summary-nanoseconds telemetry) (+ now summary-interval)
          (telemetry-next-memory-nanoseconds telemetry) (+ now memory-interval))
    (sbcl:install-gc-event-hook)
    (update-memory-metrics telemetry)
    telemetry))

(defun telemetry-record-frame (telemetry
                               cpu-nanoseconds
                               swap-nanoseconds
                               present-nanoseconds)
  "Record one presented frame.

TELEMETRY: the telemetry.
CPU-NANOSECONDS: CPU time spent building and submitting the frame.
SWAP-NANOSECONDS: time spent in the buffer swap (includes waiting for vertical sync).
PRESENT-NANOSECONDS: monotonic time at which the frame was presented."
  (sample-set-add (telemetry-frame-cpu telemetry) (nanoseconds->milliseconds cpu-nanoseconds))
  (sample-set-add (telemetry-frame-swap telemetry) (nanoseconds->milliseconds swap-nanoseconds))
  (let ((previous (telemetry-last-present-nanoseconds telemetry)))
    (when (and previous
               (< (- present-nanoseconds previous) +frame-idle-gap-nanoseconds+))
      (sample-set-add (telemetry-frame-interval telemetry)
                      (nanoseconds->milliseconds (- present-nanoseconds previous)))))
  (setf (telemetry-last-present-nanoseconds telemetry) present-nanoseconds))

(defun telemetry-record-input (telemetry
                               event-type
                               duration-nanoseconds)
  "Record the handling time of one input event; mouse moves are only ever aggregated.

TELEMETRY: the telemetry.
EVENT-TYPE: string naming the kind of event, for example \"pointer-move\".
DURATION-NANOSECONDS: handling time."
  (let ((samples (or (gethash event-type (telemetry-input-samples telemetry))
                     (setf (gethash event-type (telemetry-input-samples telemetry))
                           (make-sample-set)))))
    (sample-set-add samples (nanoseconds->milliseconds duration-nanoseconds))))

(defun telemetry-record-viewport-update (telemetry
                                         duration-nanoseconds)
  "Record the time of one pan or zoom update.

TELEMETRY: the telemetry.
DURATION-NANOSECONDS: update time."
  (sample-set-add (telemetry-viewport-samples telemetry)
                  (nanoseconds->milliseconds duration-nanoseconds)))

(defun telemetry-set-entity-counts (telemetry
                                    total
                                    visible)
  "Publish the entity counts shown in the overlay.

TELEMETRY: the telemetry.
TOTAL: entities in the drawing.
VISIBLE: entities drawn in the last frame."
  (let ((metrics (telemetry-metrics telemetry)))
    (setf (metrics-entity-count metrics) total
          (metrics-visible-entity-count metrics) visible)))

(defun poll-gc-events (telemetry)
  "Move the GC events recorded since the last call into the metrics and the log.

TELEMETRY: the telemetry."
  (let ((metrics (telemetry-metrics telemetry)))
    (dolist (event (sbcl:drain-gc-events))
      (let ((pause (sbcl:gc-event-pause-microseconds event)))
        (incf (metrics-gc-count metrics))
        (incf (metrics-gc-total-pause-microseconds metrics) pause)
        (setf (metrics-gc-last-pause-microseconds metrics) pause
              (metrics-gc-max-pause-microseconds metrics)
              (max pause (metrics-gc-max-pause-microseconds metrics)))
        (log-event (telemetry-logger telemetry)
                   (if (>= pause +gc-pause-warning-threshold-microseconds+) :info :debug)
                   "gc.event"
                   :pause-ms (/ pause 1000d0)
                   :heap-mib (/ (sbcl:heap-usage-bytes) +bytes-per-mebibyte+))))))

(defun update-memory-metrics (telemetry)
  "Read the memory sources into the metrics. Returns the heap, capacity, resident set and total
allocation in bytes as four values (the resident set is NIL when unavailable).

TELEMETRY: the telemetry."
  (let ((metrics (telemetry-metrics telemetry))
        (heap (sbcl:heap-usage-bytes))
        (capacity (sbcl:heap-capacity-bytes))
        (rss (sbcl:resident-set-bytes)))
    (setf (metrics-heap-bytes metrics) heap
          (metrics-heap-capacity-bytes metrics) capacity
          (metrics-rss-bytes metrics) rss)
    (values heap capacity rss (sbcl:allocated-bytes-total))))

(defun sample-memory (telemetry)
  "Read the memory sources, update the metrics and write a memory.sample entry.

TELEMETRY: the telemetry."
  (multiple-value-bind (heap capacity rss allocated)
      (update-memory-metrics telemetry)
    (log-event (telemetry-logger telemetry)
               :info
               "memory.sample"
               :heap-mib (/ heap +bytes-per-mebibyte+)
               :heap-capacity-mib (/ capacity +bytes-per-mebibyte+)
               :rss-mib (if rss (/ rss +bytes-per-mebibyte+) "unavailable")
               :allocated-total-mib (/ allocated +bytes-per-mebibyte+))))

(defun summary-fields (prefix
                       summary)
  "Return log fields for a SAMPLE-SUMMARY, with names starting with PREFIX.

PREFIX: field name prefix such as :cpu.
SUMMARY: a sample summary."
  (flet ((key (suffix)
           (intern (format nil "~a-~a" prefix suffix) :keyword)))
    (list (key "MS-MEAN") (sample-summary-mean summary)
          (key "MS-P95") (sample-summary-p95 summary)
          (key "MS-MAX") (sample-summary-max summary))))

(defun flush-frame-summary (telemetry)
  "Write the aggregated render.frame entry for the closed interval and reset it.

TELEMETRY: the telemetry."
  (let ((cpu (summarize-samples (telemetry-frame-cpu telemetry)))
        (swap (summarize-samples (telemetry-frame-swap telemetry)))
        (interval (summarize-samples (telemetry-frame-interval telemetry))))
    (when cpu
      (let ((fps (and interval (plusp (sample-summary-mean interval))
                      (/ 1000d0 (sample-summary-mean interval)))))
        (setf (metrics-last-frame-summary (telemetry-metrics telemetry))
              (list :frames (sample-summary-count cpu)
                    :fps fps
                    :cpu-mean-ms (sample-summary-mean cpu)
                    :cpu-p95-ms (sample-summary-p95 cpu)
                    :cpu-max-ms (sample-summary-max cpu)
                    :interval-p95-ms (and interval (sample-summary-p95 interval))
                    :interval-max-ms (and interval (sample-summary-max interval))))
        (apply #'log-event (telemetry-logger telemetry) :info "render.frame"
               :count (sample-summary-count cpu)
               (append (and fps (list :fps fps))
                       (summary-fields :cpu cpu)
                       (and interval (summary-fields :interval interval))
                       (summary-fields :swap swap)))))
    (sample-set-clear (telemetry-frame-cpu telemetry))
    (sample-set-clear (telemetry-frame-swap telemetry))
    (sample-set-clear (telemetry-frame-interval telemetry))))

(defun flush-input-summaries (telemetry)
  "Write one aggregated input.event entry per event type seen in the closed interval.

TELEMETRY: the telemetry."
  (let ((types (sort (loop for type being the hash-keys of (telemetry-input-samples telemetry)
                           collect type)
                     #'string<)))
    (dolist (type types)
      (let* ((samples (gethash type (telemetry-input-samples telemetry)))
             (summary (summarize-samples samples)))
        (when summary
          (apply #'log-event (telemetry-logger telemetry) :info "input.event"
                 :type type
                 :count (sample-summary-count summary)
                 (summary-fields :handle summary)))
        (sample-set-clear samples)))))

(defun flush-viewport-summary (telemetry)
  "Write the aggregated viewport.update entry for the closed interval.

TELEMETRY: the telemetry."
  (let ((summary (summarize-samples (telemetry-viewport-samples telemetry))))
    (when summary
      (apply #'log-event (telemetry-logger telemetry) :info "viewport.update"
             :count (sample-summary-count summary)
             (summary-fields :update summary)))
    (sample-set-clear (telemetry-viewport-samples telemetry))))

(defun flush-summaries (telemetry)
  "Write every aggregated summary entry and reset the aggregators.

TELEMETRY: the telemetry."
  (flush-frame-summary telemetry)
  (flush-input-summaries telemetry)
  (flush-viewport-summary telemetry))

(defun telemetry-tick (telemetry
                       &optional (now (sbcl:monotonic-nanoseconds)))
  "Do the periodic work: drain GC events, write the summaries and memory samples that are due.
Call it once per main-loop iteration; it is cheap when nothing is due.

TELEMETRY: the telemetry.
NOW: current monotonic time, injectable for tests."
  (poll-gc-events telemetry)
  (when (>= now (telemetry-next-summary-nanoseconds telemetry))
    (flush-summaries telemetry)
    (setf (telemetry-next-summary-nanoseconds telemetry)
          (+ now (telemetry-summary-interval-nanoseconds telemetry))))
  (when (>= now (telemetry-next-memory-nanoseconds telemetry))
    (sample-memory telemetry)
    (setf (telemetry-next-memory-nanoseconds telemetry)
          (+ now (telemetry-memory-interval-nanoseconds telemetry)))))

(defun close-telemetry (telemetry)
  "Write the final summaries, stop recording GC events and close the log.

TELEMETRY: the telemetry."
  (poll-gc-events telemetry)
  (flush-summaries telemetry)
  (sbcl:remove-gc-event-hook)
  (close-logger (telemetry-logger telemetry))
  telemetry)

(defun telemetry-overlay-lines (telemetry)
  "Return the lines of the live performance overlay as strings. Values are measured; anything
not yet measured is shown as n/a.

TELEMETRY: the telemetry."
  (let* ((metrics (telemetry-metrics telemetry))
         (frame (metrics-last-frame-summary metrics))
         (rss (metrics-rss-bytes metrics)))
    (flet ((ms (value)
             (if value (base:format-decimal value 2) "n/a")))
      (append
       (list (format nil "Frames: ~a fps | cpu ~a ms (p95 ~a, max ~a)"
                     (if (getf frame :fps) (base:format-decimal (getf frame :fps) 1) "n/a")
                     (ms (getf frame :cpu-mean-ms))
                     (ms (getf frame :cpu-p95-ms))
                     (ms (getf frame :cpu-max-ms)))
             (format nil "Interval p95 ~a ms, max ~a ms"
                     (ms (getf frame :interval-p95-ms))
                     (ms (getf frame :interval-max-ms)))
             (format nil "Entities: ~d (drawn ~d)"
                     (metrics-entity-count metrics)
                     (metrics-visible-entity-count metrics))
             (format nil "GC: ~d runs | last ~a ms | max ~a ms | total ~a ms"
                     (metrics-gc-count metrics)
                     (ms (/ (metrics-gc-last-pause-microseconds metrics) 1000d0))
                     (ms (/ (metrics-gc-max-pause-microseconds metrics) 1000d0))
                     (ms (/ (metrics-gc-total-pause-microseconds metrics) 1000d0)))
             (format nil "Heap: ~a / ~a MiB | RSS: ~a"
                     (base:format-decimal (/ (metrics-heap-bytes metrics) +bytes-per-mebibyte+) 0)
                     (base:format-decimal (/ (metrics-heap-capacity-bytes metrics) +bytes-per-mebibyte+) 0)
                     (if rss
                         (format nil "~a MiB" (base:format-decimal (/ rss +bytes-per-mebibyte+) 0))
                         "n/a")))
       (loop for event in (metrics-recent-events metrics)
             for stats = (find-operation-stats metrics event)
             collect (format nil "~a: last ~a ms | max ~a ms | n=~d"
                             event
                             (ms (nanoseconds->milliseconds (operation-stats-last-nanoseconds stats)))
                             (ms (nanoseconds->milliseconds (operation-stats-max-nanoseconds stats)))
                             (operation-stats-count stats)))))))
