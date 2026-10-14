(in-package #:borba-cad/bench)

;;;; Measurement primitives. Durations come from the monotonic clock; every measurement keeps
;;;; its raw samples so that any statistic can be recomputed.

(defconstant +benchmark-seed+ 20261013
  "Seed of every generated workload; fixed so that runs are comparable.")

(defconstant +bytes-per-mebibyte+ 1048576
  "Bytes in one MiB.")

(defconstant +microseconds-per-millisecond+ 1000d0
  "Microseconds in one millisecond, as a double for conversions.")

(defconstant +gc-settle-seconds+ 0.05
  "Time given to SBCL's finalizer thread to deliver the last GC events of a measured section.")

(defparameter *repetition-factor* 1
  "Multiplier applied to every base repetition count; the quick mode lowers it.")

(defparameter *minimum-repetitions* 10
  "Fewest timed repetitions of any operation, so that percentiles stay meaningful.")

(defconstant +repetition-reference-entities+ 1000
  "Entity count up to which an operation uses its full base repetition count; above it the
count shrinks in proportion so that large workloads finish in bounded time.")

(defstruct (measurement (:constructor make-measurement (name
                                                        unit
                                                        samples
                                                        summary)))
  "The outcome of timing one operation repeatedly.

NAME: what was measured.
UNIT: unit of the samples, :microseconds or :milliseconds.
SAMPLES: the raw samples, as a vector of doubles.
SUMMARY: the SAMPLE-SUMMARY of the samples.
WORKLOAD: keyword of the workload it was measured on, filled in by the runner.
NOTES: free-form facts observed during the measurement, as a list of strings."
  name
  unit
  samples
  summary
  (workload nil)
  (notes nil))

(defun scaled-repetitions (base
                           entity-count)
  "Return how many timed repetitions to run for an operation whose cost grows with the
drawing size.

BASE: repetitions wanted for a drawing of up to +REPETITION-REFERENCE-ENTITIES+ entities.
ENTITY-COUNT: number of entities in the drawing."
  (let* ((wanted (max 1 (round (* base *repetition-factor*))))
         (shrunk (round (* wanted +repetition-reference-entities+)
                        (max entity-count +repetition-reference-entities+))))
    (max (min wanted *minimum-repetitions*) shrunk)))

(defun measure-operation (name
                          repetitions
                          function
                          &key (warmup 3)
                               (unit :microseconds))
  "Call FUNCTION REPETITIONS times after WARMUP untimed calls, timing each timed call.
FUNCTION receives the repetition index (the warm-up calls receive negative indices), so a
sweep can visit a different input at each step and a counter can ignore the warm-up.

NAME: label of the measurement.
REPETITIONS: number of timed calls.
FUNCTION: function of one argument, the repetition index (negative while warming up).
WARMUP: untimed calls first, so that code and caches are warm.
UNIT: :microseconds or :milliseconds."
  (declare (type function function))
  (dotimes (index warmup)
    (funcall function (- index warmup)))
  (let ((set (obs:make-sample-set))
        (divisor (ecase unit
                   (:microseconds sbcl:+nanoseconds-per-microsecond+)
                   (:milliseconds sbcl:+nanoseconds-per-millisecond+))))
    (dotimes (index repetitions)
      (let ((started (sbcl:monotonic-nanoseconds)))
        (funcall function index)
        (obs:sample-set-add set (/ (- (sbcl:monotonic-nanoseconds) started) divisor))))
    (make-measurement name
                      unit
                      (copy-seq (obs:sample-set-samples set))
                      (obs:summarize-samples set))))

(defun call-collecting-gc (function)
  "Call FUNCTION while recording GC events. Returns FUNCTION's value, then the number of
collections, the total GC CPU time and the longest single collection, both in milliseconds.

FUNCTION: function of no arguments."
  (sbcl:install-gc-event-hook)
  (sbcl:drain-gc-events)
  (let ((value (funcall function)))
    ;; The hook runs in SBCL's finalizer thread; give it a moment to deliver the last events.
    (sleep +gc-settle-seconds+)
    (let* ((events (sbcl:drain-gc-events))
           (pauses (mapcar #'sbcl:gc-event-pause-microseconds events)))
      (sbcl:remove-gc-event-hook)
      (values value
              (length events)
              (/ (reduce #'+ pauses) +microseconds-per-millisecond+)
              (/ (reduce #'max pauses :initial-value 0) +microseconds-per-millisecond+)))))

(defun gc-note (collections
                total-milliseconds
                maximum-milliseconds)
  "Return the note describing the garbage collections seen during a measurement.

COLLECTIONS: number of collections.
TOTAL-MILLISECONDS: GC CPU time in total.
MAXIMUM-MILLISECONDS: longest single collection."
  (format nil "GC during run: ~d collections, ~a ms CPU in total, longest ~a ms"
          collections
          (base:format-decimal total-milliseconds 2)
          (base:format-decimal maximum-milliseconds 2)))

(defun heap-after-full-gc-bytes ()
  "Collect everything and return the Lisp heap in use, in bytes."
  (sb-ext:gc :full t)
  (sbcl:heap-usage-bytes))
