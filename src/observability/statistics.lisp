(in-package #:borba-cad/observability)

(defconstant +nanoseconds-per-millisecond+ 1000000d0
  "Nanoseconds in one millisecond, as a double for conversions.")

(declaim (inline nanoseconds->milliseconds))
(defun nanoseconds->milliseconds (nanoseconds)
  "Convert a duration in nanoseconds to milliseconds (double-float).

NANOSECONDS: a duration in nanoseconds."
  (/ (float nanoseconds 1d0) +nanoseconds-per-millisecond+))

(defun percentile (sorted-values
                   fraction)
  "Return the nearest-rank percentile of an ascending vector: the smallest value such that at
least FRACTION of the values are less than or equal to it. NIL for an empty vector.

SORTED-VALUES: vector of real numbers in ascending order.
FRACTION: a number in (0, 1], for example 0.95."
  (let ((count (length sorted-values)))
    (and (plusp count)
         (aref sorted-values (max 0 (1- (ceiling (* fraction count))))))))

(defstruct (sample-set (:constructor make-sample-set ()))
  "A growable collection of numeric samples, reused between reporting intervals.

SAMPLES: adjustable vector of double-floats."
  (samples (make-array 256 :element-type 'double-float :adjustable t :fill-pointer 0)
   :type (and (vector double-float) (not simple-array))
   :read-only t))

(defun sample-set-add (set
                       value)
  "Add one sample.

SET: a sample set.
VALUE: a real number."
  (vector-push-extend (float value 1d0) (sample-set-samples set)))

(defun sample-set-clear (set)
  "Remove every sample, keeping the storage.

SET: a sample set."
  (setf (fill-pointer (sample-set-samples set)) 0))

(defun sample-set-count (set)
  "Return the number of samples.

SET: a sample set."
  (length (sample-set-samples set)))

(defstruct (sample-summary (:constructor make-sample-summary (count
                                                              mean
                                                              p50
                                                              p95
                                                              max)))
  "Descriptive statistics of a sample set.

COUNT: number of samples.
MEAN: arithmetic mean.
P50: median (nearest rank).
P95: 95th percentile (nearest rank).
MAX: largest sample."
  (count 0 :type integer :read-only t)
  (mean 0d0 :type double-float :read-only t)
  (p50 0d0 :type double-float :read-only t)
  (p95 0d0 :type double-float :read-only t)
  (max 0d0 :type double-float :read-only t))

(defun summarize-samples (set)
  "Return the SAMPLE-SUMMARY of SET, or NIL when it is empty.

SET: a sample set."
  (let ((count (sample-set-count set)))
    (when (plusp count)
      (let ((sorted (sort (copy-seq (coerce (sample-set-samples set) '(simple-array double-float (*))))
                          #'<)))
        (make-sample-summary count
                             (/ (reduce #'+ sorted) count)
                             (percentile sorted 0.5d0)
                             (percentile sorted 0.95d0)
                             (aref sorted (1- count)))))))
