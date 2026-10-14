(in-package #:borba-cad/tests)

(def-suite base
  :description "Units, tolerances, conditions and configuration."
  :in all)

(in-suite base)

;;; Tolerances

(test coordinate-predicate-covers-boundaries
  (is (borba-cad/base:coordinate-p 0d0))
  (is (borba-cad/base:coordinate-p borba-cad/base:+maximum-coordinate-magnitude+))
  (is (borba-cad/base:coordinate-p (- borba-cad/base:+maximum-coordinate-magnitude+)))
  (is (not (borba-cad/base:coordinate-p (* 1.0000001d0 borba-cad/base:+maximum-coordinate-magnitude+))))
  (is (not (borba-cad/base:coordinate-p 1)))
  (is (not (borba-cad/base:coordinate-p 1.0)))
  (is (not (borba-cad/base:coordinate-p "1"))))

(test positive-length-predicate-uses-the-tolerance-as-an-exclusive-bound
  (is (not (borba-cad/base:positive-length-p 0d0)))
  (is (not (borba-cad/base:positive-length-p borba-cad/base:+length-tolerance+)))
  (is (borba-cad/base:positive-length-p (* 2 borba-cad/base:+length-tolerance+)))
  (is (not (borba-cad/base:positive-length-p -5d0))))

(test tolerance-sits-between-representation-noise-and-drawing-resolution
  ;; The rationale documented in tolerances.lisp, checked numerically.
  (let* ((magnitude borba-cad/base:+maximum-coordinate-magnitude+)
         (ulp-at-maximum (- (sb-kernel:make-double-float
                             (sb-kernel:double-float-high-bits magnitude)
                             (1+ (sb-kernel:double-float-low-bits magnitude)))
                            magnitude)))
    (is (< ulp-at-maximum 2d-9))
    (is (> borba-cad/base:+length-tolerance+ (* 100 ulp-at-maximum)))
    (is (< borba-cad/base:+length-tolerance+
           (/ borba-cad/base:+drawing-resolution+ 100)))))

;;; Units

(test unit-conversions-round-trip
  (dolist (unit (borba-cad/base:supported-length-units))
    (dolist (millimeters '(0d0 1d0 150d0 3500d0 -42.5d0))
      (is (= millimeters
             (borba-cad/base:unit->millimeters
              (borba-cad/base:millimeters->unit millimeters unit)
              unit))))))

(test unit-factors-are-exact-for-common-building-dimensions
  (is (= 150d0 (borba-cad/base:unit->millimeters 15d0 :centimeter)))
  (is (= 3500d0 (borba-cad/base:unit->millimeters 3.5d0 :meter)))
  (is (= 2.4d0 (borba-cad/base:millimeters->unit 2400d0 :meter))))

(test unit-names
  (is (string= "mm" (borba-cad/base:unit-abbreviation :millimeter)))
  (is (eq :centimeter (borba-cad/base:unit-from-abbreviation "CM")))
  (is (null (borba-cad/base:unit-from-abbreviation "km")))
  (is (= 2 (length (borba-cad/base:unit-area-abbreviation :meter))))
  (signals type-error (borba-cad/base:unit-abbreviation :furlong)))

(test format-decimal-rounds-half-away-from-zero-on-the-exact-value
  (is (string= "0.13" (borba-cad/base:format-decimal 0.125d0 2)))
  (is (string= "-0.13" (borba-cad/base:format-decimal -0.125d0 2)))
  (is (string= "3" (borba-cad/base:format-decimal 2.5d0 0)))
  (is (string= "1.00" (borba-cad/base:format-decimal 1.005d0 2))) ; exact value is 1.00499999...
  (is (string= "12345.678" (borba-cad/base:format-decimal 12345.678d0 3))))

(test format-decimal-never-prints-negative-zero
  (is (string= "0.00" (borba-cad/base:format-decimal -0.001d0 2)))
  (is (string= "0" (borba-cad/base:format-decimal -0.4d0 0)))
  (is (string= "0.00" (borba-cad/base:format-decimal 0d0 2))))

(test format-length-and-area
  (is (string= "3.50" (borba-cad/base:format-length 3500d0 :meter)))
  (is (string= "3500" (borba-cad/base:format-length 3500d0 :millimeter)))
  (is (string= "350.0" (borba-cad/base:format-length 3500d0 :centimeter)))
  (is (string= "3.5 m" (borba-cad/base:format-length 3500d0 :meter :precision 1 :with-unit t)))
  (is (string= "12.00" (borba-cad/base:format-area 12000000d0 :meter)))
  (is (search "m" (borba-cad/base:format-area 12000000d0 :meter :with-unit t))))

(test parse-length-accepts-units-signs-and-both-separators
  (is (= 2500d0 (borba-cad/base:parse-length "2.5" :meter)))
  (is (= 2500d0 (borba-cad/base:parse-length "2,5" :meter)))
  (is (= 150d0 (borba-cad/base:parse-length "150 mm" :meter)))
  (is (= 1200d0 (borba-cad/base:parse-length "1,2m" :millimeter)))
  (is (= -30d0 (borba-cad/base:parse-length "-3 CM" :millimeter)))
  (is (= 0.5d0 (borba-cad/base:parse-length ".5" :millimeter)))
  (is (= 7d0 (borba-cad/base:parse-length "  +7 " :millimeter))))

(test parse-length-rejects-malformed-input-with-a-typed-condition
  (dolist (text '("" "abc" "1.2.3" "--1" "1e5" "12 furlongs" "mm" "1 2"
                  "99999999999999999999999999999999999999"))
    (signals borba-cad/base:invalid-input
      (borba-cad/base:parse-length text :millimeter))))

(test parse-length-error-message-is-actionable
  (handler-case (borba-cad/base:parse-length "12 furlongs" :millimeter)
    (borba-cad/base:invalid-input (condition)
      (let ((message (princ-to-string condition)))
        (is (search "furlongs" message))
        (is (search "mm, cm or m" message))))))

(test parse-length-is-exact-for-decimal-text
  ;; The rational intermediate keeps 0.1 m exactly equal to the double nearest 100 mm.
  (is (= 100d0 (borba-cad/base:parse-length "0.1" :meter)))
  (is (= 1234.5678d0 (borba-cad/base:parse-length "1234.5678" :millimeter))))

(test angle-conversion
  (is (< (abs (- 180d0 (borba-cad/base:radians->degrees pi))) 1d-12))
  (is (< (abs (- pi (borba-cad/base:degrees->radians 180d0))) 1d-12))
  (is (< (abs (- 90d0 (borba-cad/base:radians->degrees (/ pi 2)))) 1d-12)))

;;; Conditions

(test conditions-share-one-root-and-read-as-sentences
  (let ((entity (make-condition 'borba-cad/base:invalid-entity :kind :line :reason "it has no length")))
    (is (typep entity 'borba-cad/base:borba-cad-error))
    (is (string= "Invalid line: it has no length" (princ-to-string entity))))
  (let ((syntax (make-condition 'borba-cad/base:drawing-file-syntax-error
                                :path "a.bcad" :reason "unbalanced parenthesis" :line 3 :column 9)))
    (is (typep syntax 'borba-cad/base:drawing-file-error))
    (is (string= "Drawing file a.bcad, line 3, column 9: unbalanced parenthesis"
                 (princ-to-string syntax))))
  (is (subtypep 'borba-cad/base:drawing-file-version-error 'borba-cad/base:drawing-file-content-error))
  (is (subtypep 'borba-cad/base:drawing-file-content-error 'borba-cad/base:drawing-file-syntax-error)))

;;; Configuration

(defun environment-from (alist)
  "Return a reader function that looks names up in ALIST (test double for the environment)."
  (lambda (name)
    (cdr (assoc name alist :test #'string=))))

(test default-config-places-logs-under-the-project-root
  (let ((config (borba-cad/base:default-config :project-root #p"/proj/")))
    (is (equal #p"/proj/var/log/" (borba-cad/base:config-log-directory config)))
    (is (eq :info (borba-cad/base:config-log-level config)))
    (is (= borba-cad/base:+default-log-max-bytes+ (borba-cad/base:config-log-max-bytes config)))))

(test environment-overrides-apply-and-validate
  (let* ((base (borba-cad/base:default-config :project-root #p"/proj/"))
         (config (borba-cad/base:config-with-environment
                  base
                  (environment-from '(("BORBA_CAD_LOG_DIR" . "/tmp/logs")
                                      ("BORBA_CAD_LOG_LEVEL" . "DEBUG")
                                      ("BORBA_CAD_LOG_MAX_BYTES" . "4096")
                                      ("BORBA_CAD_LOG_MAX_FILES" . "2")
                                      ("BORBA_CAD_LOG_STDERR" . "yes")
                                      ("BORBA_CAD_LOG_SAMPLING" . "input.event=10,geometry.pick=2")
                                      ("BORBA_CAD_STATE_DIR" . "/var/lib/borba")
                                      ("BORBA_CAD_DRAWINGS_DIR" . "/home/me/obras")
                                      ("BORBA_CAD_FONT" . "/fonts/mono.ttf"))))))
    (is (equal #p"/tmp/logs/" (borba-cad/base:config-log-directory config)))
    (is (eq :debug (borba-cad/base:config-log-level config)))
    (is (= 4096 (borba-cad/base:config-log-max-bytes config)))
    (is (= 2 (borba-cad/base:config-log-max-files config)))
    (is (borba-cad/base:config-log-to-stderr config))
    (is (equal '(("input.event" . 10) ("geometry.pick" . 2))
               (borba-cad/base:config-log-sampling config)))
    (is (string= "/fonts/mono.ttf" (borba-cad/base:config-font-path config)))
    (is (equal #p"/var/lib/borba/" (borba-cad/base:config-state-directory config)))
    (is (equal #p"/home/me/obras/" (borba-cad/base:config-drawings-directory config)))
    ;; The original is untouched.
    (is (eq :info (borba-cad/base:config-log-level base)))))

(test the-default-configuration-remembers-state-below-the-project-and-leaves-the-drawings-folder-open
  (let ((config (borba-cad/base:default-config :project-root #p"/proj/")))
    (is (equal #p"/proj/var/state/" (borba-cad/base:config-state-directory config)))
    (is (null (borba-cad/base:config-drawings-directory config)))))

(test environment-overrides-reject-bad-values
  (let ((base (borba-cad/base:default-config :project-root #p"/proj/")))
    (dolist (binding '(("BORBA_CAD_LOG_LEVEL" . "loud")
                       ("BORBA_CAD_LOG_MAX_FILES" . "0")
                       ("BORBA_CAD_LOG_MAX_BYTES" . "lots")
                       ("BORBA_CAD_LOG_STDERR" . "maybe")
                       ("BORBA_CAD_LOG_SAMPLING" . "input.event")))
      (signals borba-cad/base:configuration-error
        (borba-cad/base:config-with-environment base (environment-from (list binding)))))))

(test command-line-options-apply-and-leave-unknown-arguments
  (let ((base (borba-cad/base:default-config :project-root #p"/proj/")))
    (multiple-value-bind (config remaining)
        (borba-cad/base:config-with-arguments
         base
         '("--log-level" "trace" "--demo" "--file" "a.bcad" "--width" "800" "--log-stderr" "--extra"
           "--state-dir" "/s" "--drawings-dir" "/d"))
      (is (eq :trace (borba-cad/base:config-log-level config)))
      (is (equal #p"/s/" (borba-cad/base:config-state-directory config)))
      (is (equal #p"/d/" (borba-cad/base:config-drawings-directory config)))
      (is (string= "a.bcad" (borba-cad/base:config-initial-file config)))
      (is (= 800 (borba-cad/base:config-window-width config)))
      (is (borba-cad/base:config-log-to-stderr config))
      (is (equal '("--demo" "--extra") remaining)))))

(test command-line-options-report-missing-and-invalid-values
  (let ((base (borba-cad/base:default-config :project-root #p"/proj/")))
    (signals borba-cad/base:configuration-error
      (borba-cad/base:config-with-arguments base '("--log-level")))
    (signals borba-cad/base:configuration-error
      (borba-cad/base:config-with-arguments base '("--width" "wide")))
    (signals borba-cad/base:configuration-error
      (borba-cad/base:config-with-arguments base '("--log-level" "loud")))))

(test project-root-honors-the-environment-then-falls-back
  (let ((root (borba-cad/base:detect-project-root)))
    (is (typep root 'pathname))
    (is (null (pathname-name root)))))
