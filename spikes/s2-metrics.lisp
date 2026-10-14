;;;; Spike S2 (throwaway): validate SBCL clock, GC-event and memory measurement sources.
;;;; Run: sbcl --noinform --non-interactive --no-sysinit --no-userinit --load spikes/s2-metrics.lisp

(defun mono-ns ()
  (multiple-value-bind (sec nsec) (sb-unix:clock-gettime sb-unix:clock-monotonic)
    (+ (* sec 1000000000) nsec)))

(sb-ext:defglobal **gc-events** nil)
(sb-ext:defglobal **last-real** 0)
(sb-ext:defglobal **last-run** 0)

(defun gc-hook ()
  ;; Lock-free: hooks may run in any thread, possibly while another lock is held.
  (let ((real sb-ext:*gc-real-time*)
        (run sb-ext:*gc-run-time*)
        (now (mono-ns)))
    (sb-ext:atomic-push (list now
                              (- real **last-real**)
                              (- run **last-run**)
                              (sb-thread:thread-name sb-thread:*current-thread*))
                        (symbol-value '**gc-events**))
    (setf **last-real** real
          **last-run** run)))

(push #'gc-hook sb-ext:*after-gc-hooks*)

(defstruct blob (a 0d0 :type double-float) (b 0d0 :type double-float) (c nil))

(defun run-stall-detector (seconds live-count)
  "Allocate garbage for SECONDS while a tight loop records gaps between monotonic reads."
  (let* ((live (make-array live-count))
         (end (+ (mono-ns) (* seconds 1000000000)))
         (prev (mono-ns))
         (stalls '())
         (garbage nil))
    (dotimes (i live-count)
      (setf (aref live i) (make-blob :a (float i 1d0) :c (list i))))
    (loop for now = (mono-ns)
          while (< now end)
          do (let ((gap (- now prev)))
               (when (> gap 500000)     ; > 0.5 ms
                 (push (cons now gap) stalls)))
             (setf prev now
                   garbage (make-array 20000 :element-type 'double-float)))
    (values (nreverse stalls) (length live) garbage)))

(defun report-run (title seconds live-count)
  (setf **gc-events** nil **last-real** sb-ext:*gc-real-time* **last-run** sb-ext:*gc-run-time*)
  (multiple-value-bind (stalls) (run-stall-detector seconds live-count)
    (let* ((events (reverse **gc-events**))
           (n (length events))
           (sum-real (reduce #'+ events :key #'second))
           (max-real (if events (reduce #'max events :key #'second) 0))
           (max-run (if events (reduce #'max events :key #'third) 0))
           (max-stall (if stalls (reduce #'max stalls :key #'cdr) 0))
           (threads (remove-duplicates (mapcar #'fourth events) :test #'equal)))
      (format t "~%== ~a (live objects: ~d, ~d s) ==~%" title live-count seconds)
      (format t "GC hook events: ~d | threads running hook: ~s~%" n threads)
      (format t "sum(*gc-real-time* delta) = ~,2f ms | max single = ~,3f ms | max(*gc-run-time* delta) = ~,3f ms~%"
              (/ sum-real 1000.0) (/ max-real 1000.0) (/ max-run 1000.0))
      (format t "stall detector (gaps > 0.5 ms between monotonic reads): ~d stalls, max = ~,3f ms~%"
              (length stalls) (/ max-stall 1e6))
      ;; Correlate: for each of the 5 longest stalls, find the nearest GC event end time.
      (dolist (stall (subseq (sort (copy-list stalls) #'> :key #'cdr) 0 (min 5 (length stalls))))
        (let* ((nearest (and events
                             (reduce (lambda (a b)
                                       (if (< (abs (- (first a) (car stall)))
                                              (abs (- (first b) (car stall))))
                                           a b))
                                     events)))
               (dist-ms (and nearest (/ (abs (- (first nearest) (car stall))) 1e6))))
          (format t "  stall ~,3f ms | nearest GC event: ~a ms away, real delta ~a ms~%"
                  (/ (cdr stall) 1e6)
                  (and dist-ms (format nil "~,2f" dist-ms))
                  (and nearest (format nil "~,3f" (/ (second nearest) 1000.0)))))))))

(defmacro bench-op (name &body body)
  `(let ((t0 (mono-ns)))
     (dotimes (i 100000) ,@body)
     (format t "~28a ~,1f ns/call~%" ,name (/ (- (mono-ns) t0) 100000.0))))

(format t "SBCL ~a | dynamic-space ~d MiB | bytes-consed-between-gcs ~d MiB~%"
        (lisp-implementation-version)
        (floor (sb-ext:dynamic-space-size) (* 1024 1024))
        (floor (sb-ext:bytes-consed-between-gcs) (* 1024 1024)))

(format t "~%== cost of measurement sources ==~%")
(bench-op "clock-gettime monotonic" (mono-ns))
(bench-op "sb-kernel:dynamic-usage" (sb-kernel:dynamic-usage))
(bench-op "sb-ext:get-bytes-consed" (sb-ext:get-bytes-consed))
(let ((t0 (mono-ns)))
  (dotimes (i 20000)
    (with-open-file (s "/proc/self/statm") (read-line s)))
  (format t "~28a ~,1f ns/call~%" "/proc/self/statm read" (/ (- (mono-ns) t0) 20000.0)))

(report-run "small live set" 3 1000)
(report-run "large live set" 4 1500000)
(format t "~%final dynamic-usage: ~,1f MiB~%" (/ (sb-kernel:dynamic-usage) 1048576.0))
