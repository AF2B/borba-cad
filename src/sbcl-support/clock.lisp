(in-package #:borba-cad/sbcl-support)

(defconstant +nanoseconds-per-microsecond+ 1000
  "Nanoseconds in one microsecond.")

(defconstant +nanoseconds-per-millisecond+ 1000000
  "Nanoseconds in one millisecond.")

(defconstant +nanoseconds-per-second+ 1000000000
  "Nanoseconds in one second.")

(defconstant +microseconds-per-millisecond+ 1000
  "Microseconds in one millisecond.")

(defconstant +microseconds-per-second+ 1000000
  "Microseconds in one second.")

(defconstant +unix-epoch-universal-time+ 2208988800
  "Common Lisp universal time of 1970-01-01T00:00:00Z.")

(declaim (inline monotonic-nanoseconds))
(defun monotonic-nanoseconds ()
  "Return a reading of the monotonic clock in nanoseconds.

The origin is arbitrary (system boot), so only differences are meaningful.
CL:GET-INTERNAL-REAL-TIME is not used: it advances in ~1 ms ticks on SBCL 2.6.9."
  (multiple-value-bind (seconds nanoseconds)
      (sb-unix:clock-gettime sb-unix:clock-monotonic)
    (+ (* seconds +nanoseconds-per-second+)
       nanoseconds)))

(defun format-utc-timestamp (unix-seconds
                             microseconds)
  "Format a wall-clock instant as an ISO-8601 UTC timestamp with millisecond precision.

UNIX-SECONDS: seconds since 1970-01-01T00:00:00Z.
MICROSECONDS: microseconds within the second, 0 to 999999."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (+ unix-seconds +unix-epoch-universal-time+) 0)
    (format nil
            "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d.~3,'0dZ"
            year
            month
            day
            hour
            minute
            second
            (floor microseconds +microseconds-per-millisecond+))))

(defun current-utc-timestamp ()
  "Return the current wall-clock time as an ISO-8601 UTC timestamp with millisecond precision."
  (multiple-value-bind (unix-seconds microseconds)
      (sb-ext:get-time-of-day)
    (format-utc-timestamp unix-seconds microseconds)))
