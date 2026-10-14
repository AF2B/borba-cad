(in-package #:borba-cad/observability)

(defconstant +recent-operation-count+ 6
  "Number of most recently recorded operations kept for the live overlay.")

(defstruct (operation-stats (:constructor make-operation-stats ()))
  "Running statistics of one timed operation.

COUNT: how many times it ran.
TOTAL-NANOSECONDS: sum of all durations.
MAX-NANOSECONDS: longest duration.
LAST-NANOSECONDS: duration of the most recent run."
  (count 0 :type integer)
  (total-nanoseconds 0 :type integer)
  (max-nanoseconds 0 :type integer)
  (last-nanoseconds 0 :type integer))

(defstruct (metrics (:constructor make-metrics ()))
  "The live figures behind the performance overlay.

OPERATIONS: event name to OPERATION-STATS.
RECENT-EVENTS: event names, most recently recorded first.
ENTITY-COUNT: entities in the drawing.
VISIBLE-ENTITY-COUNT: entities drawn in the last frame.
GC-COUNT: collections observed.
GC-TOTAL-PAUSE-MICROSECONDS: sum of pause times.
GC-LAST-PAUSE-MICROSECONDS: latest pause.
GC-MAX-PAUSE-MICROSECONDS: longest pause.
HEAP-BYTES: Lisp heap in use at the last sample.
HEAP-CAPACITY-BYTES: Lisp heap size.
RSS-BYTES: resident set size at the last sample, or NIL.
LAST-FRAME-SUMMARY: property list describing the last completed frame interval, or NIL."
  (operations (make-hash-table :test 'equal) :type hash-table)
  (recent-events nil :type list)
  (entity-count 0 :type integer)
  (visible-entity-count 0 :type integer)
  (gc-count 0 :type integer)
  (gc-total-pause-microseconds 0 :type integer)
  (gc-last-pause-microseconds 0 :type integer)
  (gc-max-pause-microseconds 0 :type integer)
  (heap-bytes 0 :type integer)
  (heap-capacity-bytes 0 :type integer)
  (rss-bytes nil)
  (last-frame-summary nil :type list))

(defun find-operation-stats (metrics
                             event)
  "Return the OPERATION-STATS of EVENT, or NIL when it never ran.

METRICS: the metrics.
EVENT: event name."
  (values (gethash event (metrics-operations metrics))))

(defun record-operation (metrics
                         event
                         duration-nanoseconds)
  "Add one run of EVENT to the statistics.

METRICS: the metrics.
EVENT: event name.
DURATION-NANOSECONDS: how long it took."
  (let ((stats (or (find-operation-stats metrics event)
                   (setf (gethash event (metrics-operations metrics)) (make-operation-stats)))))
    (incf (operation-stats-count stats))
    (incf (operation-stats-total-nanoseconds stats) duration-nanoseconds)
    (setf (operation-stats-max-nanoseconds stats)
          (max (operation-stats-max-nanoseconds stats) duration-nanoseconds)
          (operation-stats-last-nanoseconds stats) duration-nanoseconds))
  (let ((recent (metrics-recent-events metrics)))
    (unless (and recent (string= event (first recent)))
      (setf (metrics-recent-events metrics)
            (let ((updated (cons event (remove event recent :test #'string=))))
              (if (> (length updated) +recent-operation-count+)
                  (subseq updated 0 +recent-operation-count+)
                  updated)))))
  metrics)
