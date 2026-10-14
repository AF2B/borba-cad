(in-package #:borba-cad/tests)

(def-suite sbcl-support
  :description "SBCL-specific clocks, GC events, memory and file primitives."
  :in all)

(in-suite sbcl-support)

(test monotonic-clock-never-goes-backwards
  (let ((readings (loop repeat 1000 collect (borba-cad/sbcl-support:monotonic-nanoseconds))))
    (is (every #'<= readings (rest readings)))))

(test monotonic-clock-resolves-sub-microsecond-intervals
  ;; Spike S2 measured a 16 ns step; allow 1 ms to stay machine-independent. The property that
  ;; matters is that it is far finer than CL:GET-INTERNAL-REAL-TIME (~1 ms ticks).
  (let ((distinct (remove-duplicates
                   (loop repeat 100000 collect (borba-cad/sbcl-support:monotonic-nanoseconds)))))
    (is (> (length distinct) 1000))))

(test utc-timestamp-formats-known-instants
  (is (string= "1970-01-01T00:00:00.000Z"
               (borba-cad/sbcl-support:format-utc-timestamp 0 0)))
  (is (string= "2026-10-13T21:00:00.123Z"
               (borba-cad/sbcl-support:format-utc-timestamp 1791925200 123456)))
  (is (string= "2000-02-29T23:59:59.999Z"
               (borba-cad/sbcl-support:format-utc-timestamp 951868799 999999))))

(test current-utc-timestamp-has-iso-shape
  (let ((stamp (borba-cad/sbcl-support:current-utc-timestamp)))
    (is (= 24 (length stamp)))
    (is (char= #\T (char stamp 10)))
    (is (char= #\Z (char stamp 23)))))

(test gc-events-are-recorded-and-drained
  (borba-cad/sbcl-support:install-gc-event-hook)
  (unwind-protect
       (let ((events '()))
         (sb-ext:gc :full t)
         ;; The hook runs in SBCL's finalizer thread, so poll for up to two seconds.
         (loop repeat 40
               until events
               do (sleep 0.05)
                  (setf events (borba-cad/sbcl-support:drain-gc-events)))
         (is (plusp (length events)))
         (is (every (lambda (event)
                      (and (typep event 'borba-cad/sbcl-support:gc-event)
                           (>= (borba-cad/sbcl-support:gc-event-pause-microseconds event) 0)))
                    events)))
    (borba-cad/sbcl-support:remove-gc-event-hook)))

(test memory-sources-report-plausible-values
  (is (plusp (borba-cad/sbcl-support:heap-usage-bytes)))
  (is (>= (borba-cad/sbcl-support:heap-capacity-bytes)
          (borba-cad/sbcl-support:heap-usage-bytes)))
  (is (plusp (borba-cad/sbcl-support:allocated-bytes-total)))
  (let ((resident (borba-cad/sbcl-support:resident-set-bytes)))
    (is (or (null resident) (plusp resident)))))

(test file-primitives-rename-and-delete
  (let* ((directory (uiop:ensure-directory-pathname
                     (format nil "/tmp/borba-cad-test-~d-~d/"
                             (borba-cad/sbcl-support:process-id)
                             (random 1000000 (make-random-state t)))))
         (source (merge-pathnames "a.txt" directory))
         (target (merge-pathnames "b.txt" directory)))
    (ensure-directories-exist directory)
    (unwind-protect
         (progn
           (with-open-file (out source :direction :output)
             (write-string "one" out)
             (borba-cad/sbcl-support:sync-output-stream out))
           (with-open-file (out target :direction :output)
             (write-string "old contents" out))
           (borba-cad/sbcl-support:rename-over source target)
           (is (not (probe-file source)))
           (is (= 3 (borba-cad/sbcl-support:file-size-bytes target)))
           (is (borba-cad/sbcl-support:delete-file-if-exists target))
           (is (not (borba-cad/sbcl-support:delete-file-if-exists target))))
      (uiop:delete-directory-tree directory :validate t :if-does-not-exist :ignore))))
