(in-package #:borba-cad/observability)

(defun call-with-operation (telemetry
                            event
                            fields
                            thunk
                            &key (level :info))
  "Run THUNK, measure it with the monotonic clock, record it in the metrics and write one
log entry with its duration and outcome (success, error with the condition, or aborted by a
non-local exit). Errors propagate unchanged. A NIL TELEMETRY just runs THUNK.

TELEMETRY: the telemetry, or NIL.
EVENT: dotted event name.
FIELDS: alternating keys and values describing the workload, for example :entity-count 1500.
THUNK: function of no arguments doing the work; its values are returned.
LEVEL: severity of the entry on success."
  (declare (type function thunk))
  (if (null telemetry)
      (funcall thunk)
      (let ((started (sbcl:monotonic-nanoseconds))
            (failure nil)
            (completed nil))
        (unwind-protect
             (handler-bind ((error (lambda (condition) (setf failure condition))))
               (multiple-value-prog1 (funcall thunk)
                 (setf completed t)))
          (let* ((duration (- (sbcl:monotonic-nanoseconds) started))
                 (milliseconds (nanoseconds->milliseconds duration))
                 (logger (telemetry-logger telemetry)))
            (record-operation (telemetry-metrics telemetry)
                              event
                              duration)
            (cond (completed
                   (apply #'log-event logger level event
                          :duration-ms milliseconds
                          (append fields (list :result :success))))
                  (failure
                   (apply #'log-condition logger :warn event failure
                          :duration-ms milliseconds
                          fields))
                  (t
                   (apply #'log-event logger :warn event
                          :duration-ms milliseconds
                          (append fields (list :result :aborted))))))))))

(defmacro with-operation ((telemetry event &key fields (level :info)) &body body)
  "Time BODY as the operation EVENT; see CALL-WITH-OPERATION. A macro because this wrapper
appears around every measured operation.

TELEMETRY: expression giving the telemetry, or NIL.
EVENT: dotted event name.
FIELDS: expression giving the alternating keys and values logged with the entry.
LEVEL: severity of the success entry.
BODY: forms to run."
  `(call-with-operation ,telemetry ,event ,fields (lambda () ,@body) :level ,level))

(defun record-operation-result (telemetry
                                event
                                duration-nanoseconds
                                level
                                &rest fields)
  "Record a finished operation whose descriptive fields were only known afterwards, and log
it. A NIL TELEMETRY does nothing.

TELEMETRY: the telemetry, or NIL.
EVENT: dotted event name.
DURATION-NANOSECONDS: how long the operation took.
LEVEL: severity of the entry.
FIELDS: alternating keys and values, for example :bytes 1200 :result :success."
  (when telemetry
    (record-operation (telemetry-metrics telemetry)
                      event
                      duration-nanoseconds)
    (apply #'log-event (telemetry-logger telemetry) level event
           :duration-ms (nanoseconds->milliseconds duration-nanoseconds)
           fields)))
