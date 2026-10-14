(defpackage #:borba-cad/base
  (:use #:cl)
  (:documentation "Units, tolerances, conditions and configuration shared by every module.")
  (:import-from #:borba-cad/sbcl-support
                #:environment-variable)
  (:export
   ;; Tolerances
   #:+maximum-coordinate-magnitude+
   #:+maximum-length+
   #:+drawing-resolution+
   #:+length-tolerance+
   #:coordinate-p
   #:positive-length-p
   ;; Units
   #:length-unit
   #:supported-length-units
   #:length-unit-p
   #:unit-abbreviation
   #:unit-area-abbreviation
   #:unit-from-abbreviation
   #:millimeters->unit
   #:unit->millimeters
   #:square-millimeters->unit
   #:default-display-precision
   #:format-decimal
   #:format-length
   #:format-area
   #:format-byte-size
   #:parse-length
   #:parse-decimal
   #:radians->degrees
   #:degrees->radians
   ;; Conditions
   #:borba-cad-error
   #:invalid-entity
   #:invalid-entity-kind
   #:invalid-entity-reason
   #:invalid-input
   #:invalid-input-text
   #:invalid-input-reason
   #:layer-error
   #:layer-error-reason
   #:drawing-file-error
   #:drawing-file-error-path
   #:drawing-file-error-reason
   #:drawing-file-io-error
   #:drawing-file-syntax-error
   #:drawing-file-content-error
   #:drawing-file-version-error
   #:drawing-file-error-line
   #:drawing-file-error-column
   #:configuration-error
   #:configuration-error-reason
   ;; Configuration
   #:+default-log-max-bytes+
   #:+default-log-max-files+
   #:+default-window-width+
   #:+default-window-height+
   #:+default-frame-summary-interval-seconds+
   #:+default-memory-sample-interval-seconds+
   #:log-levels
   #:config
   #:make-config
   #:copy-config
   #:config-project-root
   #:config-log-directory
   #:config-state-directory
   #:config-drawings-directory
   #:config-log-level
   #:config-log-max-bytes
   #:config-log-max-files
   #:config-log-to-stderr
   #:config-log-sampling
   #:config-frame-summary-interval-seconds
   #:config-memory-sample-interval-seconds
   #:config-font-path
   #:config-window-width
   #:config-window-height
   #:config-initial-file
   #:parse-positive-integer-option
   #:detect-project-root
   #:default-config
   #:config-with-environment
   #:user-state-directory
   #:installed-config
   #:config-with-arguments
   #:parse-log-level))
