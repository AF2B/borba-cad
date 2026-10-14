(in-package #:borba-cad/tests)

(def-suite observability
  :description "Statistics, structured logging, rotation, timing, telemetry and the log summarizer."
  :in all)

(in-suite observability)

;;; Helpers

(defun test-config (directory &key (level :info) (max-bytes 1048576) (max-files 3) sampling
                                 (summary-seconds 1) (memory-seconds 5))
  "Return a configuration whose logs go to DIRECTORY."
  (let ((config (base:default-config :project-root directory)))
    (setf (base:config-log-directory config) directory
          (base:config-log-level config) level
          (base:config-log-max-bytes config) max-bytes
          (base:config-log-max-files config) max-files
          (base:config-log-sampling config) sampling
          (base:config-frame-summary-interval-seconds config) summary-seconds
          (base:config-memory-sample-interval-seconds config) memory-seconds)
    config))

(defun log-lines (path)
  "Return the lines of the file at PATH."
  (with-open-file (stream path :external-format :utf-8)
    (loop for line = (read-line stream nil nil) while line collect line)))

(defun entries-in (directory)
  "Return the parsed entries of every log file in DIRECTORY, oldest first."
  (loop for path in (obs:discover-log-files directory)
        append (remove nil (mapcar #'obs:parse-log-line (log-lines path)))))

(defun entry-count-of (directory event)
  "Return how many entries of EVENT the log files in DIRECTORY hold."
  (count event (entries-in directory) :key #'obs:log-entry-event :test #'string=))

;;; Statistics

(test percentile-uses-the-nearest-rank-definition
  (let ((values (coerce (loop for i from 1 to 100 collect (float i 1d0)) 'vector)))
    (is (= 50d0 (obs:percentile values 0.5d0)))
    (is (= 95d0 (obs:percentile values 0.95d0)))
    (is (= 100d0 (obs:percentile values 1d0)))
    (is (= 1d0 (obs:percentile values 0.001d0))))
  (is (= 7d0 (obs:percentile (vector 7d0) 0.95d0)))
  (is (= 19d0 (obs:percentile (coerce (loop for i from 1 to 20 collect (float i 1d0)) 'vector) 0.95d0)))
  (is (null (obs:percentile (vector) 0.5d0))))

(test sample-sets-summarize-and-reset
  (let ((set (obs:make-sample-set)))
    (is (null (obs:summarize-samples set)))
    (loop for i from 100 downto 1 do (obs:sample-set-add set i))      ; unsorted input
    (let ((summary (obs:summarize-samples set)))
      (is (= 100 (obs:sample-summary-count summary)))
      (is (= 50.5d0 (obs:sample-summary-mean summary)))
      (is (= 50d0 (obs:sample-summary-p50 summary)))
      (is (= 95d0 (obs:sample-summary-p95 summary)))
      (is (= 100d0 (obs:sample-summary-max summary))))
    (obs:sample-set-clear set)
    (is (= 0 (obs:sample-set-count set)))
    (is (null (obs:summarize-samples set)))))

(test nanosecond-conversion
  (is (= 1d0 (obs:nanoseconds->milliseconds 1000000)))
  (is (= 0.412d0 (obs:nanoseconds->milliseconds 412000))))

;;; Line format

(test log-lines-follow-the-documented-format
  (is (string= "2026-10-13T21:00:00.123Z INFO geometry.pick duration_ms=0.412 entity_count=1500 result=success"
               (obs:format-log-line "2026-10-13T21:00:00.123Z" :info "geometry.pick"
                                    '(:duration-ms 0.412d0 :entity-count 1500 :result :success))))
  (is (string= "T WARN x.y"
               (obs:format-log-line "T" :warn "x.y" '()))))

(test log-values-are-quoted-only-when-needed
  (flet ((line (value) (obs:format-log-line "T" :info "e" (list :v value))))
    (is (string= "T INFO e v=plain-text_1.5" (line "plain-text_1.5")))
    (is (string= "T INFO e v=\"two words\"" (line "two words")))
    (is (string= "T INFO e v=\"a=b\"" (line "a=b")))
    (is (string= "T INFO e v=\"say \\\"hi\\\"\"" (line "say \"hi\"")))
    (is (string= "T INFO e v=\"back\\\\slash\"" (line "back\\slash")))
    (is (string= "T INFO e v=\"\"" (line "")))
    (is (string= "T INFO e v=true" (line t)))
    (is (string= "T INFO e v=false" (line nil)))
    (is (string= "T INFO e v=error" (line :error)))
    (is (string= "T INFO e v=-12" (line -12)))
    (is (string= "T INFO e v=2.500" (line 2.5d0)))
    (is (search "\"" (line (coerce (list (code-char 233) #\a) 'string))))   ; non-ASCII is quoted
    (is (not (find #\Newline (line (format nil "two~%lines")))))))

(test log-lines-parse-back-into-their-fields
  (let* ((line (obs:format-log-line "2026-10-13T21:00:00.123Z" :warn "file.save"
                                    '(:duration-ms 12.5d0 :path "my drawing.bcad" :note "say \"hi\" \\ bye"
                                      :ok t)))
         (entry (obs:parse-log-line line)))
    (is (string= "2026-10-13T21:00:00.123Z" (obs:log-entry-timestamp entry)))
    (is (string= "WARN" (obs:log-entry-severity entry)))
    (is (string= "file.save" (obs:log-entry-event entry)))
    (is (string= "12.500" (obs:log-entry-field entry "duration_ms")))
    (is (string= "my drawing.bcad" (obs:log-entry-field entry "path")))
    (is (string= "say \"hi\" \\ bye" (obs:log-entry-field entry "note")))
    (is (string= "true" (obs:log-entry-field entry "ok")))
    (is (null (obs:log-entry-field entry "missing")))))

(test non-log-lines-do-not-parse
  (is (null (obs:parse-log-line "")))
  (is (null (obs:parse-log-line "hello world")))
  (is (null (obs:parse-log-line "short line")))
  (is (obs:parse-log-line "2026-10-13T21:00:00.123Z INFO app.start")))

(test random-field-sets-round-trip-through-format-and-parse
  (let ((prng (rng:make-prng 31)))
    (dotimes (i 300)
      (let* ((fields (loop for k from 0 below (rng:prng-next-below prng 6)
                           append (list (intern (format nil "FIELD-~d" k) :keyword)
                                        (case (rng:prng-next-below prng 4)
                                          (0 (rng:prng-next-below prng 100000))
                                          (1 (rng:prng-next-double prng 0d0 500d0))
                                          (2 (rng:prng-pick prng '("a b" "x=y" "q\"q" "plain" "")))
                                          (t (rng:prng-pick prng '(:success :error t nil)))))))
             (entry (obs:parse-log-line (obs:format-log-line "2026-10-13T21:00:00.123Z" :info "evt" fields))))
        (is (not (null entry)))
        (is (= (floor (length fields) 2) (length (obs:log-entry-fields entry))))))))

;;; Logger

(test the-logger-respects-severity-levels
  (with-temporary-directory (directory)
    (let ((logger (obs:open-logger (test-config directory :level :info))))
      (is (obs:log-enabled-p logger :error))
      (is (obs:log-enabled-p logger :info))
      (is (not (obs:log-enabled-p logger :debug)))
      (is (not (obs:log-enabled-p nil :error)))
      (obs:log-event logger :error "a.error")
      (obs:log-event logger :debug "a.debug")
      (obs:log-event logger :trace "a.trace")
      (obs:log-event nil :error "ignored")
      (obs:close-logger logger)
      (is (equal '("a.error") (mapcar #'obs:log-entry-event (entries-in directory)))))))

(test every-entry-is-flushed-immediately-and-timestamped
  (with-temporary-directory (directory)
    (let ((logger (obs:open-logger (test-config directory))))
      (obs:log-event logger :info "app.start" :version "1")
      (let ((lines (log-lines (obs:logger-path logger))))      ; read before closing: written continuously
        (is (= 1 (length lines)))
        (let ((entry (obs:parse-log-line (first lines))))
          (is (= 24 (length (obs:log-entry-timestamp entry))))
          (is (string= "app.start" (obs:log-entry-event entry)))))
      (obs:close-logger logger))))

(test reopening-appends-to-the-existing-file
  (with-temporary-directory (directory)
    (dotimes (i 2)
      (let ((logger (obs:open-logger (test-config directory))))
        (obs:log-event logger :info "run" :n i)
        (obs:close-logger logger)))
    (is (= 2 (entry-count-of directory "run")))))

(test rotation-keeps-a-bounded-set-of-files-holding-the-newest-entries
  (with-temporary-directory (directory)
    (let ((logger (obs:open-logger (test-config directory :max-bytes 400 :max-files 3))))
      (dotimes (i 100)
        (obs:log-event logger :info "tick" :n i :padding "xxxxxxxxxx"))
      (obs:close-logger logger)
      (let* ((files (obs:discover-log-files directory))
             (names (mapcar #'file-namestring files))
             (numbers (loop for entry in (entries-in directory)
                            collect (parse-integer (obs:log-entry-field entry "n")))))
        (is (equal '("borba-cad.log.2" "borba-cad.log.1" "borba-cad.log") names))
        (is (null (probe-file (merge-pathnames "borba-cad.log.3" directory))))
        ;; No file exceeds the limit (a single line is far shorter than it).
        (dolist (file files)
          (is (<= (sbcl:file-size-bytes file) 400)))
        ;; What remains is a contiguous run of the newest entries, ending at the last one.
        (is (= 99 (car (last numbers))))
        (is (equal numbers (loop for n from (first numbers) to 99 collect n)))
        (is (> (length numbers) 10))
        (is (< (length numbers) 100))))))

(test rotation-with-a-single-file-truncates
  (with-temporary-directory (directory)
    (let ((logger (obs:open-logger (test-config directory :max-bytes 300 :max-files 1))))
      (dotimes (i 60)
        (obs:log-event logger :info "tick" :n i :padding "xxxxxxxxxx"))
      (obs:close-logger logger)
      (is (= 1 (length (obs:discover-log-files directory))))
      (is (<= (sbcl:file-size-bytes (merge-pathnames "borba-cad.log" directory)) 300))
      (is (= 59 (parse-integer (obs:log-entry-field (car (last (entries-in directory))) "n")))))))

(test sampling-writes-one-in-n-and-says-so
  (with-temporary-directory (directory)
    (let ((logger (obs:open-logger (test-config directory :sampling '(("geometry.pick" . 5))))))
      (dotimes (i 20)
        (obs:log-event logger :info "geometry.pick" :n i)
        (obs:log-event logger :info "other.event" :n i))
      (obs:close-logger logger)
      (let ((picks (remove-if-not (lambda (entry) (string= "geometry.pick" (obs:log-entry-event entry)))
                                  (entries-in directory))))
        (is (equal '(0 5 10 15) (mapcar (lambda (entry) (parse-integer (obs:log-entry-field entry "n"))) picks)))
        (is (every (lambda (entry) (equal "5" (obs:log-entry-field entry "sampled_one_in"))) picks)))
      (is (= 20 (entry-count-of directory "other.event"))))))

(test conditions-are-logged-with-their-type-and-message
  (with-temporary-directory (directory)
    (let ((logger (obs:open-logger (test-config directory))))
      (obs:log-condition logger :error "file.load"
                         (make-condition 'base:drawing-file-io-error :path "x.bcad" :reason "does not exist")
                         :path "x.bcad")
      (obs:close-logger logger)
      (let ((entry (first (entries-in directory))))
        (is (string= "error" (obs:log-entry-field entry "result")))
        (is (string= "drawing-file-io-error" (obs:log-entry-field entry "error")))
        (is (search "does not exist" (obs:log-entry-field entry "message")))))))

(test an-unusable-log-directory-degrades-visibly-instead-of-crashing
  (with-temporary-directory (directory)
    (let* ((blocker (merge-pathnames "blocker" directory))
           (stderr (make-string-output-stream)))
      (with-open-file (out blocker :direction :output) (write-string "x" out))
      ;; A log "directory" that is really a file cannot be created.
      (let* ((*error-output* stderr)
             (logger (obs:open-logger (test-config (uiop:ensure-directory-pathname blocker)))))
        (is (obs:logger-failure logger))
        (obs:log-event logger :error "still.fine")                ; must not signal
        (obs:close-logger logger))
      (is (search "file logging disabled" (get-output-stream-string stderr))))))

;;; Operation timing

(defun make-test-telemetry (directory &rest options)
  "Create a telemetry logging into DIRECTORY."
  (obs:make-telemetry (apply #'test-config directory options) :now 0))

(test with-operation-logs-duration-and-outcome-and-feeds-the-metrics
  (with-temporary-directory (directory)
    (let ((telemetry (make-test-telemetry directory)))
      (multiple-value-bind (first second)
          (obs:with-operation (telemetry "geometry.pick" :fields (list :entity-count 1500))
            (sleep 0.01)
            (values :a :b))
        (is (eq :a first))
        (is (eq :b second)))
      (obs:close-telemetry telemetry)
      (let* ((entry (find "geometry.pick" (entries-in directory) :key #'obs:log-entry-event :test #'string=))
             (stats (obs:find-operation-stats (obs:telemetry-metrics telemetry) "geometry.pick")))
        (is (string= "INFO" (obs:log-entry-severity entry)))
        (is (string= "1500" (obs:log-entry-field entry "entity_count")))
        (is (string= "success" (obs:log-entry-field entry "result")))
        (is (>= (base:parse-decimal (obs:log-entry-field entry "duration_ms")) 9d0))
        (is (= 1 (obs:operation-stats-count stats)))
        (is (>= (obs:operation-stats-last-nanoseconds stats) 9000000))))))

(test with-operation-reports-errors-and-lets-them-propagate
  (with-temporary-directory (directory)
    (let ((telemetry (make-test-telemetry directory)))
      (signals base:drawing-file-io-error
        (obs:with-operation (telemetry "file.load" :fields (list :bytes 10))
          (error 'base:drawing-file-io-error :path "x" :reason "boom")))
      (obs:close-telemetry telemetry)
      (let ((entry (find "file.load" (entries-in directory) :key #'obs:log-entry-event :test #'string=)))
        (is (string= "WARN" (obs:log-entry-severity entry)))
        (is (string= "error" (obs:log-entry-field entry "result")))
        (is (string= "drawing-file-io-error" (obs:log-entry-field entry "error")))
        (is (string= "10" (obs:log-entry-field entry "bytes")))))))

(test with-operation-reports-non-local-exits-as-aborted
  (with-temporary-directory (directory)
    (let ((telemetry (make-test-telemetry directory)))
      (block escape
        (obs:with-operation (telemetry "entity.create")
          (return-from escape :left-early)))
      (obs:close-telemetry telemetry)
      (is (string= "aborted"
                   (obs:log-entry-field
                    (find "entity.create" (entries-in directory) :key #'obs:log-entry-event :test #'string=)
                    "result"))))))

(test with-operation-without-telemetry-just-runs-the-body
  (is (= 3 (obs:with-operation (nil "anything") (+ 1 2)))))

(test the-recent-operations-list-is-bounded-and-most-recent-first
  (let ((metrics (obs:make-metrics)))
    (dotimes (i 10)
      (obs:record-operation metrics (format nil "op.~d" i) 1000))
    (obs:record-operation metrics "op.3" 2000)
    (let ((recent (obs::metrics-recent-events metrics)))
      (is (= 6 (length recent)))
      (is (string= "op.3" (first recent)))
      (is (string= "op.9" (second recent))))
    (is (= 2 (obs:operation-stats-count (obs:find-operation-stats metrics "op.3"))))
    (is (= 3000 (obs:operation-stats-total-nanoseconds (obs:find-operation-stats metrics "op.3"))))
    (is (= 2000 (obs:operation-stats-max-nanoseconds (obs:find-operation-stats metrics "op.3"))))
    (is (null (obs:find-operation-stats metrics "never")))))

;;; Telemetry

(defconstant +test-second+ 1000000000
  "One second in nanoseconds, for the injected clock of the telemetry tests.")

(test frame-summaries-are-written-once-per-interval
  (with-temporary-directory (directory)
    (let ((telemetry (make-test-telemetry directory)))
      ;; Ten frames, 16 ms apart, 2 ms of CPU each, then an idle gap of 2 s and one more frame.
      (dotimes (i 10)
        (obs:telemetry-record-frame telemetry 2000000 14000000 (* i 16000000)))
      (obs:telemetry-record-frame telemetry 2000000 14000000 (+ (* 9 16000000) (* 2 +test-second+)))
      (obs:telemetry-tick telemetry (- +test-second+ 1))               ; before the interval ends
      (is (= 0 (entry-count-of directory "render.frame")))
      (obs:telemetry-tick telemetry +test-second+)
      (obs:close-telemetry telemetry)
      (let ((entry (find "render.frame" (entries-in directory) :key #'obs:log-entry-event :test #'string=)))
        (is (not (null entry)))
        (is (string= "11" (obs:log-entry-field entry "count")))
        (is (string= "2.000" (obs:log-entry-field entry "cpu_ms_mean")))
        (is (string= "2.000" (obs:log-entry-field entry "cpu_ms_max")))
        ;; The 2 s idle gap is excluded: every counted interval is 16 ms, so fps = 62.5.
        (is (string= "16.000" (obs:log-entry-field entry "interval_ms_max")))
        (is (string= "62.500" (obs:log-entry-field entry "fps")))
        (is (string= "14.000" (obs:log-entry-field entry "swap_ms_mean"))))
      (is (= 1 (entry-count-of directory "render.frame"))))))                ; the final flush had no frames

(test input-and-viewport-summaries-are-aggregated-per-type
  (with-temporary-directory (directory)
    (let ((telemetry (make-test-telemetry directory)))
      (dotimes (i 100)
        (obs:telemetry-record-input telemetry "pointer-move" (* (1+ i) 1000)))      ; 1..100 microseconds
      (obs:telemetry-record-input telemetry "key" 5000000)
      (obs:telemetry-record-viewport-update telemetry 300000)
      (obs:telemetry-record-viewport-update telemetry 500000)
      (obs:telemetry-tick telemetry +test-second+)
      (obs:close-telemetry telemetry)
      (let* ((inputs (remove-if-not (lambda (entry) (string= "input.event" (obs:log-entry-event entry)))
                                    (entries-in directory)))
             (moves (find "pointer-move"
                          inputs
                          :key (lambda (entry) (obs:log-entry-field entry "type"))
                          :test #'string=)))
        (is (= 2 (length inputs)))
        (is (string= "100" (obs:log-entry-field moves "count")))
        (is (string= "0.095" (obs:log-entry-field moves "handle_ms_p95")))
        (is (string= "0.100" (obs:log-entry-field moves "handle_ms_max"))))
      (let ((viewport (find "viewport.update" (entries-in directory) :key #'obs:log-entry-event :test #'string=)))
        (is (string= "2" (obs:log-entry-field viewport "count")))
        (is (string= "0.400" (obs:log-entry-field viewport "update_ms_mean")))))))

(test nothing-is-logged-for-an-idle-interval
  (with-temporary-directory (directory)
    (let ((telemetry (make-test-telemetry directory :memory-seconds 1000)))
      (obs:telemetry-tick telemetry +test-second+)
      (obs:telemetry-tick telemetry (* 2 +test-second+))
      (obs:close-telemetry telemetry)
      (is (null (entries-in directory))))))

(test memory-samples-follow-their-own-interval
  (with-temporary-directory (directory)
    (let ((telemetry (make-test-telemetry directory :memory-seconds 5)))
      (obs:telemetry-tick telemetry (* 4 +test-second+))
      (is (= 0 (entry-count-of directory "memory.sample")))
      (obs:telemetry-tick telemetry (* 5 +test-second+))
      (is (= 1 (entry-count-of directory "memory.sample")))
      (obs:telemetry-tick telemetry (* 9 +test-second+))
      (is (= 1 (entry-count-of directory "memory.sample")))
      (obs:telemetry-tick telemetry (* 10 +test-second+))
      (is (= 2 (entry-count-of directory "memory.sample")))
      (obs:close-telemetry telemetry)
      (let ((entry (find "memory.sample" (entries-in directory) :key #'obs:log-entry-event :test #'string=)))
        (is (plusp (base:parse-decimal (obs:log-entry-field entry "heap_mib"))))
        (is (plusp (base:parse-decimal (obs:log-entry-field entry "allocated_total_mib"))))
        (is (obs:log-entry-field entry "rss_mib"))))))

(test garbage-collections-reach-the-metrics-and-the-log
  (with-temporary-directory (directory)
    (let ((telemetry (make-test-telemetry directory :level :debug)))
      (sb-ext:gc :full t)
      (let ((metrics (obs:telemetry-metrics telemetry)))
        (loop repeat 40
              until (plusp (obs::metrics-gc-count metrics))
              do (sleep 0.05)
                 (obs:telemetry-tick telemetry 0))               ; the hook runs in SBCL's finalizer thread
        (is (plusp (obs::metrics-gc-count metrics)))
        (is (>= (obs::metrics-gc-max-pause-microseconds metrics)
                (obs::metrics-gc-last-pause-microseconds metrics))))
      (obs:close-telemetry telemetry)
      (let ((entry (find "gc.event" (entries-in directory) :key #'obs:log-entry-event :test #'string=)))
        (is (not (null entry)))
        (is (>= (base:parse-decimal (obs:log-entry-field entry "pause_ms")) 0d0))))))

(test the-overlay-shows-measured-values-and-marks-the-rest-as-unavailable
  (with-temporary-directory (directory)
    (let ((telemetry (make-test-telemetry directory)))
      (let ((before (obs:telemetry-overlay-lines telemetry)))
        (is (search "n/a" (first before)))                         ; nothing measured yet
        (is (search "Entities: 0" (third before))))
      (obs:telemetry-set-entity-counts telemetry 1500 812)
      (dotimes (i 5)
        (obs:telemetry-record-frame telemetry 2000000 14000000 (* i 16000000)))
      (obs:with-operation (telemetry "file.save") (sleep 0.002))
      (obs:telemetry-tick telemetry +test-second+)
      (let ((lines (obs:telemetry-overlay-lines telemetry)))
        (is (search "62.5 fps" (first lines)))
        (is (search "cpu 2.00 ms" (first lines)))
        (is (search "Entities: 1500 (drawn 812)" (third lines)))
        (is (find-if (lambda (line) (search "file.save: last" line)) lines))
        (is (find-if (lambda (line) (search "GC:" line)) lines))
        (is (find-if (lambda (line) (search "Heap:" line)) lines)))
      (obs:close-telemetry telemetry))))

;;; Log summary

(defun write-log-fixture (path lines)
  "Write LINES to PATH."
  (with-open-file (out path :direction :output :if-exists :supersede)
    (dolist (line lines) (write-line line out))))

(test the-summarizer-aggregates-timed-and-interval-entries-exactly
  (with-temporary-directory (directory)
    (let ((older (merge-pathnames "borba-cad.log.1" directory))
          (current (merge-pathnames "borba-cad.log" directory)))
      (write-log-fixture
       older
       (append (loop for i from 1 to 10
                     collect (format nil "2026-10-13T21:00:0~d.000Z INFO geometry.pick ~
                                          duration_ms=~d.000 entity_count=100 result=success"
                                     (mod i 10)
                                     i))
               (list "not a log line")))
      (write-log-fixture
       current
       (list "2026-10-13T21:01:00.000Z INFO file.save duration_ms=40.000 bytes=100 result=success"
             "2026-10-13T21:01:01.000Z WARN file.load duration_ms=5.000 result=error"
             (concatenate 'string
                          "2026-10-13T21:01:02.000Z INFO render.frame count=60 fps=60.000 "
                          "cpu_ms_mean=2.000 cpu_ms_p95=3.000 cpu_ms_max=9.000 "
                          "interval_ms_mean=16.000 interval_ms_p95=17.000 interval_ms_max=20.000")
             (concatenate 'string
                          "2026-10-13T21:01:03.000Z INFO render.frame count=40 fps=50.000 "
                          "cpu_ms_mean=4.000 cpu_ms_p95=6.000 cpu_ms_max=7.000 "
                          "interval_ms_mean=20.000 interval_ms_p95=25.000 interval_ms_max=30.000")
             (concatenate 'string
                          "2026-10-13T21:01:04.000Z INFO input.event type=pointer-move count=10 "
                          "handle_ms_mean=0.100 handle_ms_p95=0.200 handle_ms_max=0.300")
             "2026-10-13T21:01:05.000Z ERROR app.crash message=\"boom\"")
       )
      (let* ((summary (obs:summarize-log-files (obs:discover-log-files directory)))
             (picks (find "geometry.pick" (obs:log-summary-timed-events summary)
                          :key #'obs:event-summary-event :test #'string=)))
        (is (= 16 (obs:log-summary-entries summary)))
        (is (= 1 (obs:log-summary-unparsable summary)))
        (is (string= "2026-10-13T21:00:01.000Z" (obs:log-summary-first-timestamp summary)))
        (is (string= "2026-10-13T21:01:05.000Z" (obs:log-summary-last-timestamp summary)))
        (is (equal '(("INFO" . 14) ("WARN" . 1) ("ERROR" . 1)) (obs:log-summary-severities summary)))
        ;; geometry.pick: durations 1..10 ms.
        (is (= 10 (obs:event-summary-count picks)))
        (is (= 55d0 (obs:event-summary-total-ms picks)))
        (is (= 5.5d0 (obs:event-summary-mean-ms picks)))
        (is (= 5d0 (obs:event-summary-p50-ms picks)))
        (is (= 10d0 (obs:event-summary-p95-ms picks)))
        (is (= 10d0 (obs:event-summary-max-ms picks)))
        ;; Sorted by total time: file.save (40) ahead of geometry.pick (55)? No: 55 > 40.
        (is (equal '("geometry.pick" "file.save" "file.load")
                   (mapcar #'obs:event-summary-event (obs:log-summary-timed-events summary))))
        ;; Interval rows: render.frame cpu = (2*60 + 4*40) / 100 = 2.8 ms, worst p95 6, max 9.
        (let ((cpu (find "render.frame cpu" (obs:log-summary-interval-rows summary)
                         :key #'obs:interval-row-key :test #'string=)))
          (is (= 2 (obs:interval-row-intervals cpu)))
          (is (= 100 (obs:interval-row-samples cpu)))
          (is (approx= 2.8d0 (obs:interval-row-weighted-mean-ms cpu)))
          (is (= 6d0 (obs:interval-row-worst-p95-ms cpu)))
          (is (= 9d0 (obs:interval-row-max-ms cpu))))
        (is (find "input.event[pointer-move] handle" (obs:log-summary-interval-rows summary)
                  :key #'obs:interval-row-key :test #'string=))
        (is (find "render.frame interval" (obs:log-summary-interval-rows summary)
                  :key #'obs:interval-row-key :test #'string=))))))

(test the-summary-text-names-its-tables-and-numbers
  (with-temporary-directory (directory)
    (write-log-fixture (merge-pathnames "borba-cad.log" directory)
                       (list "2026-10-13T21:01:00.000Z INFO file.save duration_ms=40.000 bytes=100 result=success"
                             (concatenate 'string
                                          "2026-10-13T21:01:02.000Z INFO render.frame count=60 "
                                          "fps=60.000 cpu_ms_mean=2.000 cpu_ms_p95=3.000 "
                                          "cpu_ms_max=9.000")))
    (let ((text (with-output-to-string (out)
                  (obs:format-log-summary (obs:summarize-log-files (obs:discover-log-files directory)) out))))
      (is (search "Log summary: 2 entries (0 unparsable lines)" text))
      (is (search "Timed events" text))
      (is (search "file.save" text))
      (is (search "40.000" text))
      (is (search "Interval summaries" text))
      (is (search "render.frame cpu" text)))))

(test summarizing-a-real-session-log-works-end-to-end
  (with-temporary-directory (directory)
    (let ((telemetry (make-test-telemetry directory)))
      (dotimes (i 5)
        (obs:with-operation (telemetry "geometry.pick" :fields (list :entity-count 100))
          (sleep 0.002)))
      (obs:telemetry-record-frame telemetry 1000000 2000000 0)
      (obs:telemetry-record-frame telemetry 1000000 2000000 16000000)
      (obs:telemetry-tick telemetry +test-second+)
      (obs:close-telemetry telemetry)
      (let* ((summary (obs:summarize-log-files (obs:discover-log-files directory)))
             (picks (find "geometry.pick" (obs:log-summary-timed-events summary)
                          :key #'obs:event-summary-event :test #'string=)))
        (is (= 5 (obs:event-summary-count picks)))
        (is (>= (obs:event-summary-p50-ms picks) 1.5d0))
        (is (zerop (obs:log-summary-unparsable summary)))
        (is (find "render.frame cpu" (obs:log-summary-interval-rows summary)
                  :key #'obs:interval-row-key :test #'string=))))))
