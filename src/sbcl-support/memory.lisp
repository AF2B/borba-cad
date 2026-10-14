(in-package #:borba-cad/sbcl-support)

(defstruct (gc-event (:constructor make-gc-event (timestamp-nanoseconds
                                                  pause-microseconds)))
  "One completed garbage collection.

TIMESTAMP-NANOSECONDS: monotonic time at which the after-GC hook ran.
PAUSE-MICROSECONDS: CPU time SBCL spent in this collection (delta of *GC-RUN-TIME*)."
  (timestamp-nanoseconds 0 :type integer :read-only t)
  (pause-microseconds 0 :type integer :read-only t))

;;; The after-GC hook runs in SBCL's finalizer thread (verified in spike S2), possibly while
;;; the main thread holds any lock, so it only does a lock-free push onto a global list.
(sb-ext:defglobal **pending-gc-events** nil)
(sb-ext:defglobal **last-gc-run-time** 0)

(defun record-gc-event ()
  "After-GC hook: push one GC-EVENT onto the pending list without taking any lock."
  (let* ((run-time sb-ext:*gc-run-time*)
         (pause (- run-time **last-gc-run-time**)))
    (setf **last-gc-run-time** run-time)
    (sb-ext:atomic-push (make-gc-event (monotonic-nanoseconds) pause)
                        (symbol-value '**pending-gc-events**))))

(defun install-gc-event-hook ()
  "Start recording GC events; idempotent. Events accumulate until DRAIN-GC-EVENTS is called."
  (setf **last-gc-run-time** sb-ext:*gc-run-time*)
  (pushnew 'record-gc-event sb-ext:*after-gc-hooks*)
  t)

(defun remove-gc-event-hook ()
  "Stop recording GC events and discard any pending ones."
  (setf sb-ext:*after-gc-hooks* (remove 'record-gc-event sb-ext:*after-gc-hooks*))
  (drain-gc-events)
  t)

(defun drain-gc-events ()
  "Return the GC events recorded since the last call, oldest first, and clear the queue."
  (loop
    (let ((pending **pending-gc-events**))
      (when (eq (sb-ext:compare-and-swap (symbol-value '**pending-gc-events**) pending nil)
                pending)
        (return (nreverse pending))))))

(defun gc-total-pause-microseconds ()
  "Return the total CPU time spent in garbage collection since process start, in microseconds."
  sb-ext:*gc-run-time*)

(defun heap-usage-bytes ()
  "Return the number of bytes currently in use in the Lisp heap (dynamic space)."
  (sb-kernel:dynamic-usage))

(defun heap-capacity-bytes ()
  "Return the size of the Lisp heap (dynamic space) in bytes."
  (sb-ext:dynamic-space-size))

(defun allocated-bytes-total ()
  "Return the total number of bytes allocated by Lisp code since process start."
  (sb-ext:get-bytes-consed))

(defun page-size-bytes ()
  "Return the operating-system memory page size in bytes."
  (sb-alien:alien-funcall
   (sb-alien:extern-alien "getpagesize" (function sb-alien:int))))

(defun resident-set-bytes ()
  "Return the resident set size of the process in bytes, or NIL when /proc is unavailable."
  (with-open-file (stream "/proc/self/statm" :if-does-not-exist nil)
    (when stream
      (let* ((line (read-line stream nil ""))
             (first-space (position #\Space line))
             (resident-pages (and first-space
                                  (parse-integer line :start (1+ first-space) :junk-allowed t))))
        (and resident-pages
             (* resident-pages (page-size-bytes)))))))
