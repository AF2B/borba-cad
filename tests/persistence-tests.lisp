(in-package #:borba-cad/tests)

(def-suite persistence
  :description "The .bcad format: writer, reader, validation, round trips and atomic saving."
  :in all)

(in-suite persistence)

;;; Fixtures

(defun golden-drawing ()
  "Return a small drawing with one layer added and one entity of every kind."
  (let ((drawing (dom:make-drawing)))
    (dom:add-layer drawing "walls" (dom:make-rgb 255 120 80))
    (dom:apply-changes
     drawing
     (dom:create-entities-changes
      (list (dom:make-line :id 1 :layer-id 1 :x1 0d0 :y1 0d0 :x2 5000d0 :y2 0d0)
            (dom:make-rectangle :id 2 :layer-id 1 :min-x 100d0 :min-y 200d0 :width 3000d0 :height 2500d0)
            (dom:make-circle :id 3 :layer-id 0 :center-x 500d0 :center-y 600d0 :radius 250d0)
            (dom:make-polyline :id 4 :layer-id 1 :coordinates (doubles 0d0 0d0 100d0 0d0 100d0 100d0)
                               :closed nil)
            (dom:make-text :id 5 :layer-id 0 :x 10d0 :y 20d0 :height 150d0 :string "Hi \"you\"" :angle 0.5d0)
            (dom:make-dimension :id 6 :layer-id 0 :x1 0d0 :y1 0d0 :x2 1000d0 :y2 0d0 :offset 500d0
                                :text-height 100d0 :unit :meter :precision 2))))
    drawing))

(defparameter *golden-text*
  (format nil "~{~a~%~}"
          (list "(drawing"
            " (format-version 5)"
            " (unit millimeter)"
            " (display-unit millimeter)"
            " (grid (spacing 100.0) (visible t) (snap t) (object-snap t))"
            " (default-text-height 250.0)"
            " (current-layer 0)"
            " (next-layer-id 2)"
            " (next-entity-id 7)"
            " (layers"
            "  (layer (id 0) (name \"0\") (color 235 235 235) (visible t) (locked nil))"
            "  (layer (id 1) (name \"walls\") (color 255 120 80) (visible t) (locked nil))"
            " )"
            " (blocks"
            " )"
            " (entities"
            "  (line (id 1) (layer 1) (x1 0.0) (y1 0.0) (x2 5000.0) (y2 0.0))"
            "  (rectangle (id 2) (layer 1) (min-x 100.0) (min-y 200.0) (width 3000.0) (height 2500.0))"
            "  (circle (id 3) (layer 0) (center-x 500.0) (center-y 600.0) (radius 250.0))"
            "  (polyline (id 4) (layer 1) (closed nil) (vertices 0.0 0.0 100.0 0.0 100.0 100.0))"
            "  (text (id 5) (layer 0) (x 10.0) (y 20.0) (height 150.0) (string \"Hi \\\"you\\\"\") (angle 0.5))"
            (concatenate 'string
                         "  (dimension (id 6) (layer 0) (x1 0.0) (y1 0.0) (x2 1000.0) (y2 0.0) "
                         "(offset 500.0) (text-height 100.0) (unit meter) (precision 2))")
            " ))"))
  "The exact text the writer must produce for GOLDEN-DRAWING (documents format version 5).")

(defun replace-once (text old new)
  "Return TEXT with the single occurrence of OLD replaced by NEW; fails the test if OLD is not
present exactly once."
  (let ((start (search old text)))
    (unless (and start (not (search old text :start2 (1+ start))))
      (error "Test setup: ~s must occur exactly once in the fixture" old))
    (concatenate 'string (subseq text 0 start) new (subseq text (+ start (length old))))))

(defmacro signals-file-error ((condition-type &key line column message) text)
  "Check that parsing TEXT signals CONDITION-TYPE, optionally at a position and with a message
fragment."
  `(handler-case (progn (persist:parse-drawing-text ,text)
                        (fail "expected ~s but parsing succeeded" ',condition-type))
     (,condition-type (condition)
       ,@(when line `((is (eql ,line (base:drawing-file-error-line condition)))))
       ,@(when column `((is (eql ,column (base:drawing-file-error-column condition)))))
       ,@(when message `((is (search ,message (princ-to-string condition)))))
       (is (plusp (length (princ-to-string condition)))))))

;;; Numbers

(test format-double-prints-shortest-readable-text
  (is (string= "0.0" (persist:format-double 0d0)))
  (is (string= "-0.0" (persist:format-double -0d0)))
  (is (string= "1.0" (persist:format-double 1d0)))
  (is (string= "150.0" (persist:format-double 150d0)))
  (is (string= "-3500.0" (persist:format-double -3500d0)))
  (is (string= "0.1" (persist:format-double 0.1d0)))
  (is (string= "1.0e-5" (persist:format-double 1d-5)))
  (is (string= "10000000.0" (persist:format-double 1d7)))
  (is (string= "123.456" (persist:format-double 123.456d0))))

(test number-tokens-parse-exactly-and-reject-malformed-text
  (flet ((parse (token) (persist::parse-number-token token)))
    (is (eql 42 (parse "42")))
    (is (eql -7 (parse "-7")))
    (is (eql 150d0 (parse "150.0")))
    (is (eql 0.1d0 (parse "0.1")))
    (is (eql 0.5d0 (parse ".5")))
    (is (eql 1d-5 (parse "1.0e-5")))
    (is (eql 1d-5 (parse "1E-5")))
    (is (eql 123456789012345680d0 (parse "1.2345678901234568e17")))
    (is (eql -0d0 (parse "-0.0")))
    (is (eql 1.7976931348623157d308 (parse "1.7976931348623157e308")))
    (dolist (bad '("" "." "-" "+" "1.2.3" "1e" "1e+" "e5" "12ab" "1e400" "1e999999999" "9e308" "--1"))
      (is (null (parse bad))))))

(test every-double-round-trips-bit-for-bit
  (let ((prng (rng:make-prng 12)))
    (dotimes (i 4000)
      (let* ((magnitude (expt 10d0 (- (rng:prng-next-below prng 14) 6)))   ; 1e-6 .. 1e7
             (value (* (if (rng:prng-next-boolean prng) 1d0 -1d0)
                       magnitude
                       (rng:prng-next-double prng 1d0 10d0)))
             (text (persist:format-double value))
             (parsed (persist::parse-number-token text)))
        (is (and (typep parsed 'double-float)
                 (= (sb-kernel:double-float-bits value) (sb-kernel:double-float-bits parsed))))))))

;;; Writer

(test writer-produces-the-documented-format-exactly
  (is (string= *golden-text* (persist:drawing-to-string (golden-drawing)))))

(test the-golden-text-reads-back-into-an-equal-drawing
  (let ((drawing (persist:parse-drawing-text *golden-text*)))
    (is (string= *golden-text* (persist:drawing-to-string drawing)))
    (is (equalp (drawing-entities (golden-drawing)) (drawing-entities drawing)))))

;;; Round trips

(defun layers-description (drawing)
  "Return the layers of DRAWING as a comparable list."
  (mapcar (lambda (layer)
            (list (dom:layer-id layer) (dom:layer-name layer)
                  (dom:rgb-red (dom:layer-color layer))
                  (dom:rgb-green (dom:layer-color layer))
                  (dom:rgb-blue (dom:layer-color layer))
                  (dom:layer-visible layer) (dom:layer-locked layer)))
          (dom:layers-in-order drawing)))

(defun settings-description (drawing)
  "Return the drawing-wide settings of DRAWING as a comparable list."
  (list (dom:drawing-unit drawing) (dom:drawing-display-unit drawing)
        (dom:drawing-grid-spacing drawing) (dom:drawing-grid-visible drawing)
        (dom:drawing-snap-to-grid drawing) (dom:drawing-object-snap drawing)
        (dom:drawing-default-text-height drawing) (dom:drawing-current-layer-id drawing)
        (dom:drawing-next-layer-id drawing) (dom:drawing-next-entity-id drawing)))

(test generated-drawings-round-trip-exactly
  (dolist (seed '(1 2 3 4 5 6 7 8))
    (let* ((original (rng:generate-drawing :seed seed :entity-count (* 40 seed)))
           (text (persist:drawing-to-string original))
           (loaded (persist:parse-drawing-text text)))
      (is (equalp (drawing-entities original) (drawing-entities loaded)))
      (is (equal (layers-description original) (layers-description loaded)))
      (is (equal (settings-description original) (settings-description loaded)))
      (is (string= text (persist:drawing-to-string loaded))))))      ; writing is idempotent

(test non-default-settings-and-layer-flags-round-trip
  (let ((drawing (golden-drawing)))
    (dom:set-layer-locked drawing 1 t)
    (dom:set-layer-visible drawing 1 nil)
    (dom:set-current-layer drawing 1)
    (dom:set-display-unit drawing :centimeter)
    (dom:set-grid-spacing drawing 37.5d0)
    (dom:set-grid-visible drawing nil)
    (dom:set-snap-to-grid drawing nil)
    (dom:set-object-snap drawing nil)
    (dom:set-default-text-height drawing 123.25d0)
    (dom:rename-layer drawing 1 "Walls \"north\" \\ side")
    (let ((loaded (persist:parse-drawing-text (persist:drawing-to-string drawing))))
      (is (equal (layers-description drawing) (layers-description loaded)))
      (is (equal (settings-description drawing) (settings-description loaded))))))

(test negative-zero-and-extreme-coordinates-survive
  (let* ((limit base:+maximum-coordinate-magnitude+)
         (drawing (dom:make-drawing)))
    (dom:apply-changes drawing
                       (dom:create-entities-changes
                        (list (dom:make-line :id 1 :layer-id 0 :x1 -0d0 :y1 0d0 :x2 limit :y2 (- limit))
                              (dom:make-line :id 2 :layer-id 0 :x1 1d-3 :y1 1d-3 :x2 2.0000000000000004d-3 :y2 1d-3))))
    (let* ((loaded (persist:parse-drawing-text (persist:drawing-to-string drawing)))
           (first (dom:store-find (dom:drawing-store loaded) 1))
           (second (dom:store-find (dom:drawing-store loaded) 2)))
      (is (minusp (float-sign (dom:line-x1 first))))
      (is (= limit (dom:line-x2 first)))
      (is (= 2.0000000000000004d-3 (dom:line-x2 second))))))

(test unicode-text-and-layer-names-round-trip
  (let* ((word (coerce (mapcar #'code-char '(233 231 227 8364 128512)) 'string))
         (drawing (dom:make-drawing)))
    (dom:add-layer drawing word (dom:make-rgb 1 2 3))
    (dom:apply-changes drawing
                       (dom:create-entity-changes
                        (dom:make-text :id 1 :layer-id 0 :x 0d0 :y 0d0 :height 100d0 :string word)))
    (let ((loaded (persist:parse-drawing-text (persist:drawing-to-string drawing))))
      (is (string= word (dom:text-string (dom:store-find (dom:drawing-store loaded) 1))))
      (is (dom:find-layer-by-name loaded word)))))

(test an-empty-drawing-round-trips
  (let* ((drawing (dom:make-drawing))
         (loaded (persist:parse-drawing-text (persist:drawing-to-string drawing))))
    (is (= 0 (dom:drawing-entity-count loaded)))
    (is (equal (settings-description drawing) (settings-description loaded)))))

;;; Lenient where it is harmless

(test comments-crlf-line-endings-and-a-byte-order-mark-are-accepted
  (let* ((with-comments (concatenate 'string "; saved by hand" (string #\Newline) *golden-text*))
         (crlf (with-output-to-string (out)
                 (loop for character across *golden-text*
                       do (when (char= character #\Newline) (write-char #\Return out))
                          (write-char character out))))
         (bom (concatenate 'string (string (code-char #xFEFF)) *golden-text*)))
    (dolist (text (list with-comments crlf bom))
      (is (equalp (drawing-entities (golden-drawing))
                  (drawing-entities (persist:parse-drawing-text text)))))))

(test integers-are-accepted-where-numbers-are-expected
  (let ((text (replace-once *golden-text* "(x1 0.0) (y1 0.0) (x2 5000.0) (y2 0.0)" "(x1 0) (y1 0) (x2 5000) (y2 0)")))
    (is (= 5000d0 (dom:line-x2 (dom:store-find (dom:drawing-store (persist:parse-drawing-text text)) 1))))))

;;; Syntax errors

(test syntax-errors-name-their-position
  (signals-file-error (base:drawing-file-syntax-error :line 1 :column 1 :message "must start with (drawing") "")
  (signals-file-error (base:drawing-file-syntax-error :line 1 :column 1) "hello")
  (signals-file-error (base:drawing-file-syntax-error :message "never closed") "(drawing (format-version 1")
  (signals-file-error (base:drawing-file-syntax-error :message "expected the field (unit")
                      "(drawing (format-version 1)")
  (signals-file-error (base:drawing-file-syntax-error :message "unexpected closing") "()")
  (signals-file-error (base:drawing-file-syntax-error :message "never closed")
                      "(drawing (format-version \"abc")
  (signals-file-error (base:drawing-file-syntax-error :message "control characters")
                      (replace-once *golden-text* "\"walls\"" "\"walls"))
  (signals-file-error (base:drawing-file-syntax-error :message "backslash")
                      (replace-once *golden-text* "\"walls\"" "\"wa\\xlls\""))
  (signals-file-error (base:drawing-file-syntax-error :message "control characters")
                      (replace-once *golden-text* "\"walls\"" (format nil "\"wa~%lls\"")))
  (signals-file-error (base:drawing-file-syntax-error :line 17 :message "unexpected character")
                      (replace-once *golden-text* "(x1 0.0) (y1 0.0) (x2 5000.0)" "(x1 #.(boom)) (y1 0.0) (x2 5000.0)"))
  (signals-file-error (base:drawing-file-syntax-error :message "unexpected content after")
                      (concatenate 'string *golden-text* "(drawing)")))

(test syntax-error-columns-are-one-based
  (signals-file-error (base:drawing-file-content-error :line 2 :column 2 :message "expected the field (format-version")
                      (format nil "(drawing~% (xyz ~%)"))
  (signals-file-error (base:drawing-file-content-error :line 17 :column 4 :message "unknown entity type")
                      (replace-once *golden-text* "(line (id 1)" "(lyne (id 1)")))

(test reader-limits-bound-nesting-token-length-and-exponents
  (signals-file-error (base:drawing-file-syntax-error :message "nested deeper")
                      (format nil
                              "(drawing (format-version ~a~a))"
                              (make-string 12 :initial-element #\()
                              (make-string 12 :initial-element #\))))
  (signals-file-error (base:drawing-file-syntax-error :message "longer than")
                      (format nil "(drawing (format-version ~a))" (make-string 200 :initial-element #\a)))
  (signals-file-error (base:drawing-file-syntax-error :message "out of range")
                      (replace-once *golden-text* "(spacing 100.0)" "(spacing 1e999999999)"))
  (signals-file-error (base:drawing-file-syntax-error :message "too large")
                      (replace-once *golden-text* "(spacing 100.0)" "(spacing 9e308)")))

;;; Content errors

(test version-errors-are-distinct-and-explicit
  (signals-file-error (base:drawing-file-version-error :line 2 :message "newer")
                      (replace-once *golden-text* "(format-version 5)" "(format-version 6)"))
  (signals-file-error (base:drawing-file-version-error :message "not supported")
                      (replace-once *golden-text* "(format-version 5)" "(format-version 0)"))
  (signals-file-error (base:drawing-file-content-error :message "whole number")
                      (replace-once *golden-text* "(format-version 5)" "(format-version 5.0)")))

(test header-fields-are-required-ordered-and-validated
  (signals-file-error (base:drawing-file-content-error :message "expected the field (unit")
                      (replace-once *golden-text* " (unit millimeter)
" ""))
  (signals-file-error (base:drawing-file-content-error :message "only millimeter")
                      (replace-once *golden-text* "(unit millimeter)" "(unit meter)"))
  (signals-file-error (base:drawing-file-content-error :message "unknown unit")
                      (replace-once *golden-text* "(display-unit millimeter)" "(display-unit furlong)"))
  (signals-file-error (base:drawing-file-content-error :message "grid spacing")
                      (replace-once *golden-text* "(spacing 100.0)" "(spacing 0.5)"))
  (signals-file-error (base:drawing-file-content-error :message "missing field \"snap\"")
                      (replace-once *golden-text* " (snap t)" ""))
  (signals-file-error (base:drawing-file-content-error :message "default text height")
                      (replace-once *golden-text* "(default-text-height 250.0)" "(default-text-height -1.0)"))
  (signals-file-error (base:drawing-file-content-error :message "current layer 9")
                      (replace-once *golden-text* "(current-layer 0)" "(current-layer 9)"))
  (signals-file-error (base:drawing-file-content-error :message "next-layer-id")
                      (replace-once *golden-text* "(next-layer-id 2)" "(next-layer-id 1)")))

(test layer-errors
  (signals-file-error (base:drawing-file-content-error :message "default layer")
                      (replace-once *golden-text* "(layer (id 0) (name \"0\")" "(layer (id 3) (name \"ground\")"))
  (signals-file-error (base:drawing-file-content-error :message "appears twice")
                      (replace-once *golden-text* "(layer (id 1) (name \"walls\")" "(layer (id 0) (name \"walls\")"))
  (signals-file-error (base:drawing-file-content-error :message "already exists")
                      (replace-once *golden-text* "(name \"walls\")" "(name \"0\")"))
  (signals-file-error (base:drawing-file-content-error :message "name cannot be empty")
                      (replace-once *golden-text* "(name \"walls\")" "(name \"  \")"))
  (signals-file-error (base:drawing-file-content-error :message "three numbers")
                      (replace-once *golden-text* "(color 255 120 80)" "(color 255 120)"))
  (signals-file-error (base:drawing-file-content-error :message "outside the allowed range")
                      (replace-once *golden-text* "(color 255 120 80)" "(color 255 120 300)"))
  (signals-file-error (base:drawing-file-content-error :message "expected t or nil")
                      (replace-once *golden-text* "(visible t) (locked nil))
  (layer (id 1)" "(visible yes) (locked nil))
  (layer (id 1)")))

(test entity-field-errors-name-the-offender
  (signals-file-error (base:drawing-file-content-error :line 17 :message "unknown field \"z1\"")
                      (replace-once *golden-text*
                                    "(line (id 1) (layer 1) (x1 0.0) (y1 0.0)"
                                    "(line (id 1) (layer 1) (x1 0.0) (z1 0.0)"))
  (signals-file-error (base:drawing-file-content-error :line 17 :message "appears twice")
                      (replace-once *golden-text*
                                    "(line (id 1) (layer 1) (x1 0.0) (y1 0.0)"
                                    "(line (id 1) (layer 1) (x1 0.0) (x1 0.0)"))
  (signals-file-error (base:drawing-file-content-error :line 17 :message "missing field \"y2\"")
                      (replace-once *golden-text* " (y2 0.0))
  (rectangle" ")
  (rectangle"))
  (signals-file-error (base:drawing-file-content-error :line 17 :message "expected a number")
                      (replace-once *golden-text* "(line (id 1) (layer 1) (x1 0.0)" "(line (id 1) (layer 1) (x1 nan)"))
  (signals-file-error (base:drawing-file-content-error :line 17 :message "expected a number")
                      (replace-once *golden-text*
                                    "(line (id 1) (layer 1) (x1 0.0)"
                                    "(line (id 1) (layer 1) (x1 \"0\")"))
  (signals-file-error (base:drawing-file-content-error :line 17 :message "exactly one value")
                      (replace-once *golden-text*
                                    "(line (id 1) (layer 1) (x1 0.0)"
                                    "(line (id 1) (layer 1) (x1 0.0 1.0)"))
  (signals-file-error (base:drawing-file-content-error :line 17 :message "expected a field")
                      (replace-once *golden-text* "(line (id 1) (layer 1) (x1 0.0)" "(line (id 1) (layer 1) x1"))
  (signals-file-error (base:drawing-file-content-error :line 20 :message "expected t or nil")
                      (replace-once *golden-text* "(closed nil)" "(closed maybe)"))
  (signals-file-error (base:drawing-file-content-error :line 21 :message "expected a quoted string")
                      (replace-once *golden-text* "(string \"Hi \\\"you\\\"\")" "(string hello)"))
  (signals-file-error (base:drawing-file-content-error :line 22 :message "unknown unit")
                      (replace-once *golden-text* "(unit meter)" "(unit parsec)"))
  (signals-file-error (base:drawing-file-content-error :line 22 :message "whole number")
                      (replace-once *golden-text* "(precision 2)" "(precision 2.5)")))

(test entity-structure-errors
  (signals-file-error (base:drawing-file-content-error :message "expected an entity")
                      (replace-once *golden-text* "  (circle (id 3)" "  42 (circle (id 3)"))
  (signals-file-error (base:drawing-file-content-error :message "unknown entity type \"spline\"")
                      (replace-once *golden-text* "(circle (id 3)" "(spline (id 3)"))
  (signals-file-error (base:drawing-file-content-error :line 18 :message "entity id 1 appears twice")
                      (replace-once *golden-text* "(rectangle (id 2)" "(rectangle (id 1)"))
  (signals-file-error (base:drawing-file-content-error :line 19 :message "missing layer 9")
                      (replace-once *golden-text* "(circle (id 3) (layer 0)" "(circle (id 3) (layer 9)"))
  (signals-file-error (base:drawing-file-content-error :line 22 :message "not below next-entity-id")
                      (replace-once *golden-text* "(next-entity-id 7)" "(next-entity-id 6)"))
  (signals-file-error (base:drawing-file-content-error :message "outside the allowed range")
                      (replace-once *golden-text* "(id 1) (layer 1) (x1" "(id 0) (layer 1) (x1")))

(test invariant-violations-are-reported-with-the-entity-position
  (signals-file-error (base:drawing-file-content-error :line 17 :column 3 :message "Invalid line")
                      (replace-once *golden-text* "(x2 5000.0) (y2 0.0)" "(x2 0.0) (y2 0.0)"))
  (signals-file-error (base:drawing-file-content-error :line 18 :message "Invalid rectangle")
                      (replace-once *golden-text* "(width 3000.0)" "(width 0.0)"))
  (signals-file-error (base:drawing-file-content-error :line 19 :message "Invalid circle")
                      (replace-once *golden-text* "(radius 250.0)" "(radius -5.0)"))
  (signals-file-error (base:drawing-file-content-error :line 20 :message "Invalid polyline")
                      (replace-once *golden-text*
                                    "(vertices 0.0 0.0 100.0 0.0 100.0 100.0)"
                                    "(vertices 0.0 0.0 0.0 0.0)"))
  (signals-file-error (base:drawing-file-content-error :line 20 :message "pairs")
                      (replace-once *golden-text*
                                    "(vertices 0.0 0.0 100.0 0.0 100.0 100.0)"
                                    "(vertices 0.0 0.0 100.0)"))
  (signals-file-error (base:drawing-file-content-error :line 21 :message "Invalid text")
                      (replace-once *golden-text* "(string \"Hi \\\"you\\\"\")" "(string \"\")"))
  (signals-file-error (base:drawing-file-content-error :line 22 :message "Invalid dimension")
                      (replace-once *golden-text* "(precision 2)" "(precision 99)"))
  (signals-file-error (base:drawing-file-content-error :line 17 :message "Invalid line")
                      (replace-once *golden-text* "(x2 5000.0)" "(x2 5.0e8)")))

;;; Robustness

(test every-truncation-of-a-valid-file-is-rejected-cleanly
  (let* ((text (persist:drawing-to-string (rng:generate-drawing :seed 3 :entity-count 15)))
         (last-paren (position #\) text :from-end t)))
    (loop for cut from 0 below last-paren by 7
          do (handler-case (progn (persist:parse-drawing-text (subseq text 0 cut))
                                  (fail "a file truncated at ~d was accepted" cut))
               (base:drawing-file-error () (pass))))))

(test random-single-character-mutations-never-escape-as-foreign-errors
  ;; Each mutation either still parses (harmless change) or is rejected with a drawing-file
  ;; error. Any other error type would mean the reader leaks an internal failure.
  (let* ((prng (rng:make-prng 2026))
         (text (persist:drawing-to-string (rng:generate-drawing :seed 4 :entity-count 12)))
         (alphabet "()\"\;#.-+eE0123456789 tnilxyz
")
         (accepted 0)
         (rejected 0))
    (dotimes (i 3000)
      (let* ((mutated (copy-seq text))
             (position (rng:prng-next-below prng (length text)))
             (replacement (char alphabet (rng:prng-next-below prng (length alphabet)))))
        (case (rng:prng-next-below prng 3)
          (0 (setf (char mutated position) replacement))
          (1 (setf mutated (concatenate 'string (subseq mutated 0 position) (subseq mutated (1+ position)))))
          (2 (setf mutated (concatenate 'string
                                        (subseq mutated 0 position)
                                        (string replacement)
                                        (subseq mutated position)))))
        (handler-case (progn (persist:parse-drawing-text mutated) (incf accepted))
          (base:drawing-file-error () (incf rejected))
          (error (condition)
            (fail "mutation at ~d produced a foreign error: ~a" position condition)))))
    (is (> rejected 1000))
    (is (plusp accepted))))

(test loading-never-evaluates-anything
  (let ((marker (format nil "borba-cad-eval-marker-~d" (random 1000000 (make-random-state t)))))
    (signals base:drawing-file-error
      (persist:parse-drawing-text
       (format nil "(drawing #.(progn (defparameter cl-user::*pwned* ~s) 1))" marker)))
    (is (not (boundp 'cl-user::*pwned*)))))

;;; Files

(defun file-text (path)
  "Return the contents of PATH as a string."
  (uiop:read-file-string path :external-format :utf-8))

(test native-path-takes-special-characters-literally
  (let ((native (uiop:native-namestring (persist:native-path "a*b[1].bcad"))))
    (is (string= "a*b[1].bcad" (subseq native (- (length native) (length "a*b[1].bcad"))))))
  (is (search (string-right-trim "/" (uiop:native-namestring (user-homedir-pathname)))
              (uiop:native-namestring (persist:native-path "~/x.bcad"))))
  (is (search "drawings" (uiop:native-namestring (persist:native-path "drawings/a.bcad")))))

(test save-and-load-round-trip-through-a-file
  (with-temporary-directory (directory)
    (let ((path (merge-pathnames "plan.bcad" directory))
          (original (golden-drawing)))
      (multiple-value-bind (saved stats) (persist:save-drawing original path)
        (is (equal (uiop:native-namestring path) (uiop:native-namestring saved)))
        (is (= (length (sb-ext:string-to-octets *golden-text* :external-format :utf-8))
               (getf stats :bytes)))
        (is (plusp (getf stats :serialize-nanoseconds))))
      (is (string= *golden-text* (file-text path)))
      (multiple-value-bind (loaded stats) (persist:load-drawing path)
        (is (equalp (drawing-entities original) (drawing-entities loaded)))
        (is (plusp (getf stats :bytes)))
        (is (plusp (getf stats :parse-nanoseconds))))
      (is (equal (list path) (directory (merge-pathnames "*.*" directory)))))))      ; no temporary file left

(test saving-replaces-an-existing-file-completely
  (with-temporary-directory (directory)
    (let ((path (merge-pathnames "plan.bcad" directory)))
      (persist:save-drawing (golden-drawing) path)
      (persist:save-drawing (dom:make-drawing) path)
      (is (= 0 (dom:drawing-entity-count (persist:load-drawing path)))))))

(test a-failed-save-leaves-the-previous-file-intact-and-no-temporary-file
  (with-temporary-directory (directory)
    (let ((path (merge-pathnames "plan.bcad" directory)))
      (persist:save-drawing (golden-drawing) path)
      (let ((before (file-text path)))
        ;; Failure after the temporary file is complete but before the rename.
        (signals base:drawing-file-io-error
          (persist:save-drawing (dom:make-drawing) path
                                :before-commit (lambda (temporary)
                                                 (declare (ignore temporary))
                                                 (error "simulated disk failure"))))
        (is (string= before (file-text path)))
        (is (equal (list path) (directory (merge-pathnames "*.*" directory))))))))

(test saving-reports-missing-and-read-only-directories
  (with-temporary-directory (directory)
    (let ((missing (merge-pathnames "nope/plan.bcad" directory)))
      (handler-case (progn (persist:save-drawing (golden-drawing) missing) (fail "expected an error"))
        (base:drawing-file-io-error (condition)
          (is (search "does not exist" (princ-to-string condition)))))
      (unless (zerop (sb-posix:getuid))       ; permissions do not bind the superuser
        (let ((path (merge-pathnames "plan.bcad" directory)))
          (persist:save-drawing (golden-drawing) path)
          (let ((before (file-text path)))
            (sb-posix:chmod (uiop:native-namestring directory) #o555)
            (unwind-protect
                 (signals base:drawing-file-io-error (persist:save-drawing (dom:make-drawing) path))
              (sb-posix:chmod (uiop:native-namestring directory) #o755))
            (is (string= before (file-text path)))))))))

(test loading-reports-file-level-problems-as-typed-errors
  (with-temporary-directory (directory)
    (signals base:drawing-file-io-error (persist:load-drawing (merge-pathnames "absent.bcad" directory)))
    (signals base:drawing-file-io-error (persist:load-drawing directory))
    (let ((binary (merge-pathnames "binary.bcad" directory))
          (huge (merge-pathnames "huge.bcad" directory)))
      (with-open-file (out binary :direction :output :element-type '(unsigned-byte 8))
        (write-sequence #(40 100 255 254 253 41) out))                       ; invalid UTF-8
      (handler-case (progn (persist:load-drawing binary) (fail "expected an error"))
        (base:drawing-file-io-error (condition)
          (is (search "UTF-8" (princ-to-string condition)))))
      (with-open-file (out huge :direction :output) (write-string "(" out))
      (sb-posix:truncate (uiop:native-namestring huge) (1+ persist:+maximum-file-bytes+))
      (handler-case (progn (persist:load-drawing huge) (fail "expected an error"))
        (base:drawing-file-io-error (condition)
          (is (search "limit" (princ-to-string condition))))))))

(test a-corrupt-file-names-itself-and-the-position
  (with-temporary-directory (directory)
    (let ((path (merge-pathnames "bad.bcad" directory)))
      (with-open-file (out path :direction :output)
        (write-string (replace-once *golden-text* "(radius 250.0)" "(radius -5.0)") out))
      (handler-case (progn (persist:load-drawing path) (fail "expected an error"))
        (base:drawing-file-content-error (condition)
          (is (search "bad.bcad" (princ-to-string condition)))
          (is (= 19 (base:drawing-file-error-line condition))))))))
