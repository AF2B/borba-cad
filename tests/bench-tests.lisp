(in-package #:borba-cad/tests)

(def-suite bench
  :description "The benchmark harness itself (not the performance of the application)."
  :in all)

(in-suite bench)

(test measure-operation-times-each-call-and-passes-negative-warm-up-indices
  (let ((indices '()))
    (let ((measurement (bench:measure-operation
                                         "probe"
                                         5
                                         (lambda (index) (push index indices))
                                         :warmup 2)))
      (is (equal '(-2 -1 0 1 2 3 4) (reverse indices)))
      (is (= 5 (length (bench:measurement-samples measurement))))
      (is (= 5 (obs:sample-summary-count (bench:measurement-summary measurement)))))))

(test repetitions-shrink-with-the-drawing-size-but-keep-a-floor
  (flet ((repetitions (base count)
           (bench:scaled-repetitions base count)))
    (is (= 2000 (repetitions 2000 0)))
    (is (= 2000 (repetitions 2000 1000)))
    (is (= 200 (repetitions 2000 10000)))
    (is (= 20 (repetitions 2000 100000)))
    (is (= 10 (repetitions 2000 10000000)))
    (is (= 5 (repetitions 5 100000)))))

(test the-command-line-is-parsed-and-bad-options-are-rejected
  (flet ((parse (&rest arguments)
           (bench:parse-benchmark-arguments arguments)))
    (let ((options (parse "--workloads" "w0,w2" "--quick")))
      (is (equal '(:w0 :w2) (getf options :workloads)))
      (is (eq t (getf options :quick))))
    (let ((options (parse "--no-overhead")))
      (is (null (getf options :overhead)))
      (is (member :overhead options)))
    (is (getf (parse "--help") :help))
    (signals base:configuration-error (parse "--bogus"))
    (signals base:configuration-error (parse "--workloads"))))

(test the-overhead-order-rotates-without-losing-modes
  (flet ((rotate (list places)
           (bench:rotate-list list places)))
    (is (equal '(1 2 3 4) (rotate '(1 2 3 4) 0)))
    (is (equal '(2 3 4 1) (rotate '(1 2 3 4) 1)))
    (is (equal '(1 2 3 4) (rotate '(1 2 3 4) 4)))
    (is (equal '() (rotate '() 3)))))

(test a-quick-run-writes-a-complete-report-and-verifies-the-round-trip
  (with-temporary-directory (directory)
    (let* ((path (bench:run-benchmarks
                                   :workloads '(:tiny)
                                   :sizes '((:tiny . 40))
                                   :quick t
                                   :overhead nil
                                   :output-directory directory
                                   :log-directory directory
                                   :progress-stream nil))
           (text (uiop:read-file-string path)))
      (is (probe-file path))
      (dolist (expected '("BORBA CAD BENCHMARK REPORT"
                          "ENVIRONMENT"
                          "SBCL"
                          "TINY"
                          "scene.build, all entities in view"
                          "pick.entity"
                          "round trip verified"
                          "memory soak"
                          "NOT MEASURED"))
        (is (search expected text) "the report lacks ~s" expected))
      (is (null (search "MISMATCH" text)))
      (let* ((log-text (uiop:read-file-string (merge-pathnames "benchmark.log" directory))))
        (is (search " INFO bench.run scenario=" log-text))
        (is (search "workload=tiny" log-text))
        (is (search "result=success" log-text))))))

(test an-unknown-workload-is-a-configuration-error
  (with-temporary-directory (directory)
    (signals base:configuration-error
      (bench:run-benchmarks
                        :workloads '(:nonexistent)
                        :overhead nil
                        :output-directory directory
                        :log-directory directory
                        :progress-stream nil))))
