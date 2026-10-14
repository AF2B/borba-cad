(defpackage #:borba-cad/bench-gui
  (:use #:cl)
  (:local-nicknames (#:base #:borba-cad/base)
                    (#:sbcl #:borba-cad/sbcl-support)
                    (#:obs #:borba-cad/observability)
                    (#:rng #:borba-cad/test-support)
                    (#:platform #:borba-cad/platform)
                    (#:bench #:borba-cad/bench))
  (:documentation "Frame benchmark with the real window, OpenGL renderer and display. It needs
a graphical session, so it is separate from the headless benchmarks.")
  (:export
   #:main
   #:run-gui-benchmarks))

(in-package #:borba-cad/bench-gui)

(defparameter *default-gui-workloads* '(:w1 :w2 :w3)
  "Workloads rendered in a window by default; W0 is an empty canvas and adds nothing.")

(defconstant +default-frames+ 600
  "Frames measured per workload and swap interval.")

(defparameter *swap-intervals* '(1 0)
  "Swap intervals compared: 1 waits for the display refresh; 0 swaps at once, which shows the
rate the program could sustain (some compositors ignore it, and the report then shows the
interval that was really in effect).")

(defparameter *gui-notes*
  '("  - cpu is the time to apply input, build the draw list and submit it to OpenGL; it excludes"
    "    the swap."
    "  - gpu wait is the time glFinish blocks after the submit: GPU work not already overlapped"
    "    with the CPU. Draining the pipeline every frame serializes CPU and GPU, so cpu + gpu wait"
    "    is an upper bound of a real frame's cost, not a vendor GPU-timer measurement."
    "  - A swap time near zero with swap interval 1 means the swap did not wait for the display;"
    "    the interval is then the rate at which the program submits frames, not a rate the"
    "    display shows.")
  "Lines of the notes that close the windowed benchmark report.")

(defparameter *gui-usage*
  "Usage: make bench-gui [BENCH_ARGS=\"...\"]

  --workloads w1,w2,w3   workloads to render in a window (default: w1,w2,w3)
  --frames N             frames measured per run (default: 600)
  --output DIRECTORY     where to write the report (default: bench/reports/)
  --help                 show this text
"
  "Help text of the windowed benchmark command.")

(defun format-summary-columns (summary)
  "Format a summary as mean, p50, p95 and max columns.

SUMMARY: a sample summary."
  (format nil "~10@a ~10@a ~10@a ~10@a"
          (bench:format-sample (obs:sample-summary-mean summary))
          (bench:format-sample (obs:sample-summary-p50 summary))
          (bench:format-sample (obs:sample-summary-p95 summary))
          (bench:format-sample (obs:sample-summary-max summary))))

(defun write-gui-row (workload
                      entity-count
                      requested-interval
                      result
                      stream)
  "Write the three timing rows of one windowed run.

WORKLOAD: keyword naming the workload.
ENTITY-COUNT: entities drawn.
REQUESTED-INTERVAL: the swap interval that was asked for.
RESULT: the frame benchmark result.
STREAM: the output stream."
  (format stream "~(~a~) (~:d entities), swap interval requested ~d, in effect ~a, ~d frames~%"
          workload
          entity-count
          requested-interval
          (platform:frame-benchmark-result-swap-interval result)
          (platform:frame-benchmark-result-frames result))
  (loop for (label summary) in (list (list "cpu: input, build, submit"
                                           (platform:frame-benchmark-result-cpu-ms result))
                                     (list "gpu wait: glFinish after submit"
                                           (platform:frame-benchmark-result-gpu-wait-ms result))
                                     (list "swap buffers"
                                           (platform:frame-benchmark-result-swap-ms result))
                                     (list "interval between presented frames"
                                           (platform:frame-benchmark-result-interval-ms result)))
        do (format stream "  ~36a ~a  ms~%" label (format-summary-columns summary)))
  (let ((cpu (platform:frame-benchmark-result-cpu-ms result))
        (gpu-wait (platform:frame-benchmark-result-gpu-wait-ms result))
        (interval (platform:frame-benchmark-result-interval-ms result)))
    (format stream "  serialized frame cost (cpu + gpu wait, mean): ~a ms~%"
            (base:format-decimal (+ (obs:sample-summary-mean cpu) (obs:sample-summary-mean gpu-wait)) 3))
    (format stream "  presented rate: ~a frames per second (from the mean interval)~%~%"
            (base:format-decimal (/ 1000 (obs:sample-summary-mean interval)) 1))))

(defun run-gui-benchmarks (&key (workloads *default-gui-workloads*)
                                (frames +default-frames+)
                                output-directory
                                (progress-stream *standard-output*)
                                (config (base:default-config)))
  "Render each workload in a real window and write the report. Returns the report pathname.

WORKLOADS: keywords of the workloads to render.
FRAMES: frames measured per run.
OUTPUT-DIRECTORY: where the report goes; defaults to bench/reports/ of the project.
PROGRESS-STREAM: where progress lines go, or NIL.
CONFIG: the application configuration."
  (unless (platform:display-available-p)
    (error 'base:configuration-error
           :reason "no graphical session found (WAYLAND_DISPLAY and DISPLAY are unset)"))
  (let* ((directory (or output-directory
                        (merge-pathnames "bench/reports/" (base:config-project-root config))))
         (path (merge-pathnames (format nil "~a-~a-gui.txt"
                                        (bench:filename-timestamp)
                                        (bench:sanitize-file-name-part (sbcl:host-name-string)))
                                directory))
         (rows (make-string-output-stream))
         (logger (bench:open-benchmark-logger config nil))
         (gl-renderer nil))
    (ensure-directories-exist directory)
    (dolist (workload workloads)
      (let* ((entry (assoc workload (bench:workload-sizes)))
             (count (or (cdr entry)
                        (error 'base:configuration-error
                               :reason (format nil "unknown workload ~s" workload))))
             (drawing (rng:generate-drawing :seed bench:+benchmark-seed+ :entity-count count)))
        (dolist (interval *swap-intervals*)
          (when progress-stream
            (format progress-stream "~(~a~): ~d frames, swap interval ~d~%" workload frames interval)
            (force-output progress-stream))
          (let* ((began (sbcl:monotonic-nanoseconds))
                 (result (platform:run-frame-benchmark drawing
                                                       :frames frames
                                                       :swap-interval interval)))
            (bench:log-scenario logger
                                (format nil "windowed frames, swap interval ~d" interval)
                                workload
                                count
                                began)
            (setf gl-renderer (platform:frame-benchmark-result-gl-renderer result))
            (write-gui-row workload
                           count
                           interval
                           result
                           rows)))))
    (with-open-file (stream path :direction :output :if-exists :supersede :external-format :utf-8)
      (bench:report-rule stream #\=)
      (format stream "BORBA CAD WINDOWED FRAME BENCHMARK~%")
      (bench:report-rule stream #\=)
      (terpri stream)
      (bench:write-environment (append (bench:environment-fingerprint config)
                                       (list (cons "OpenGL renderer" gl-renderer)
                                             (cons "session"
                                                   (format nil "XDG_SESSION_TYPE=~a WAYLAND_DISPLAY=~a DISPLAY=~a"
                                                           (or (sbcl:environment-variable "XDG_SESSION_TYPE") "-")
                                                           (or (sbcl:environment-variable "WAYLAND_DISPLAY") "-")
                                                           (or (sbcl:environment-variable "DISPLAY") "-")))))
                               stream)
      (format stream "FRAMES (window 1440x900 logical pixels; times in milliseconds; columns: mean p50 p95 max)~%")
      (bench:report-rule stream #\-)
      (write-string (get-output-stream-string rows) stream)
      (format stream "Notes~%")
      (bench:report-rule stream #\-)
      (dolist (line *gui-notes*)
        (format stream "~a~%" line)))
    (obs:close-logger logger)
    (when progress-stream
      (format progress-stream "report written to ~a~%" (uiop:native-namestring path)))
    path))

(defun parse-gui-arguments (arguments)
  "Parse the command line into keyword arguments of RUN-GUI-BENCHMARKS plus :help.

ARGUMENTS: list of strings."
  (let ((options '()))
    (loop while arguments
          do (let ((argument (pop arguments)))
               (flet ((value-of ()
                        (or (pop arguments)
                            (error 'base:configuration-error
                                   :reason (format nil "~a expects a value" argument)))))
                 (cond ((string= argument "--workloads")
                        (setf (getf options :workloads) (bench:parse-workload-list (value-of))))
                       ((string= argument "--frames")
                        (setf (getf options :frames)
                              (base:parse-positive-integer-option (value-of) argument)))
                       ((string= argument "--output")
                        (setf (getf options :output-directory)
                              (uiop:ensure-directory-pathname (value-of))))
                       ((string= argument "--help")
                        (setf (getf options :help) t))
                       (t (error 'base:configuration-error
                                 :reason (format nil "unknown option ~s" argument)))))))
    options))

(defun main ()
  "Command-line entry point of the windowed benchmark. Exit status: 0 on success, 2 for a bad
option or a missing display, 1 for any other failure."
  (handler-case
      (let ((options (parse-gui-arguments (sbcl:command-line-arguments))))
        (if (getf options :help)
            (write-string *gui-usage*)
            (apply #'run-gui-benchmarks (loop for (key value) on options by #'cddr
                                              unless (eq key :help)
                                                append (list key value)))))
    (base:configuration-error (condition)
      (format *error-output* "bench-gui: ~a~%~%~a" condition *gui-usage*)
      (finish-output *error-output*)
      (sb-ext:exit :code 2 :abort t))
    (error (condition)
      (format *error-output* "bench-gui: ~a~%" condition)
      (finish-output *error-output*)
      (sb-ext:exit :code 1 :abort t)))
  (finish-output)
  (sb-ext:exit :code 0))
