(defpackage #:borba-cad/sbcl-support
  (:use #:cl)
  (:documentation "All SBCL-specific code: clocks, memory and GC sources, environment and file-system primitives.")
  (:export
   ;; Clocks
   #:+nanoseconds-per-microsecond+
   #:+nanoseconds-per-millisecond+
   #:+nanoseconds-per-second+
   #:monotonic-nanoseconds
   #:format-utc-timestamp
   #:current-utc-timestamp
   ;; GC and memory
   #:gc-event
   #:gc-event-timestamp-nanoseconds
   #:gc-event-pause-microseconds
   #:install-gc-event-hook
   #:remove-gc-event-hook
   #:drain-gc-events
   #:gc-total-pause-microseconds
   #:heap-usage-bytes
   #:heap-capacity-bytes
   #:allocated-bytes-total
   #:resident-set-bytes
   ;; Process and environment
   #:environment-variable
   #:command-line-arguments
   #:process-id
   #:lisp-version-string
   #:kernel-release-string
   #:cpu-model-string
   #:host-name-string
   ;; File system
   #:sync-output-stream
   #:rename-over
   #:delete-file-if-exists
   #:file-size-bytes))
