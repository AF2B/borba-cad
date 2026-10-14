(defpackage #:borba-cad/bench
  (:use #:cl)
  (:local-nicknames (#:base #:borba-cad/base)
                    (#:sbcl #:borba-cad/sbcl-support)
                    (#:geo #:borba-cad/geometry)
                    (#:dom #:borba-cad/domain)
                    (#:view #:borba-cad/viewport)
                    (#:persist #:borba-cad/persistence)
                    (#:obs #:borba-cad/observability)
                    (#:scene #:borba-cad/scene)
                    (#:app #:borba-cad/app)
                    (#:rng #:borba-cad/test-support))
  (:documentation "Benchmarks: deterministic workloads W0 to W3, per-operation measurements,
GC and memory observation, instrumentation overhead, and the report writer.")
  (:export
   #:main
   #:run-benchmarks
   #:parse-benchmark-arguments
   #:scaled-repetitions
   #:rotate-list
   #:open-benchmark-logger
   #:log-scenario
   #:parse-workload-list
   #:report-rule
   #:format-sample
   #:write-environment
   #:filename-timestamp
   #:sanitize-file-name-part
   #:*not-measured-statements*
   #:environment-fingerprint
   #:+benchmark-seed+
   #:workload-sizes
   #:measure-operation
   #:measurement
   #:measurement-name
   #:measurement-samples
   #:measurement-summary))
