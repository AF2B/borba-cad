(in-package #:borba-cad/tests)

(def-suite dxf
  :description "DXF: the reader, the import into a drawing, the export, and what is left out."
  :in all)

(in-suite dxf)

;;; A way to write DXF files in a test

(defun dxf-text (&rest groups)
  "Return the text of a DXF file made of GROUPS, each a list of a code and a value, written as lines."
  (with-output-to-string (stream)
    (dolist (group groups)
      (format stream "~3d~c~a~c" (first group) #\Newline (second group) #\Newline))))

(defun entities-file (&rest records)
  "Return the text of a DXF file that has only an entities section, with RECORDS, each a list of groups."
  (apply #'dxf-text (append '((0 "SECTION") (2 "ENTITIES"))
                            (apply #'append records)
                            '((0 "ENDSEC") (0 "EOF")))))

(defun imported (text &rest options)
  "Import DXF TEXT; return the drawing and the report."
  (dxf:import-document (dxf:parse-dxf-text text) :units (getf options :units)))

(defun entity-of-kind (drawing kind)
  "Return the first entity of KIND in DRAWING."
  (find kind (dom:drawing-entities drawing) :key #'dom:entity-kind))

(defun entities-of-kind (drawing kind)
  "Return the entities of KIND in DRAWING."
  (remove kind (dom:drawing-entities drawing) :key #'dom:entity-kind :test-not #'eq))

(defun about (expected actual &optional (tolerance 1d-6))
  "Return true when two numbers agree to TOLERANCE."
  (< (abs (- expected actual)) tolerance))

;;; Colors

(test the-color-index-has-the-fixed-colors-and-the-ramp-of-hues
  (flet ((rgb (index) (let ((color (dxf:aci->rgb index)))
                        (list (dom:rgb-red color) (dom:rgb-green color) (dom:rgb-blue color)))))
    (is (equal '(255 0 0) (rgb 1)))
    (is (equal '(255 255 0) (rgb 2)))
    (is (equal '(0 255 0) (rgb 3)))
    (is (equal '(0 0 255) (rgb 5)))
    (is (equal '(255 255 255) (rgb 7)))
    (is (equal '(51 51 51) (rgb 250)))
    (is (equal '(255 0 0) (rgb 10)) "the ramp begins at red")
    (is (equal '(255 63 0) (rgb 20)))
    (is (equal '(255 127 0) (rgb 30)))
    (is (equal '(255 191 0) (rgb 40)) "three hues of fifteen degrees on from red")
    (is (equal '(255 127 127) (rgb 11)) "the washed out shade of red")
    (is (equal '(204 0 0) (rgb 12)))
    (is (equal '(204 102 102) (rgb 13)))
    (is (equal '(76 38 38) (rgb 19)))
    (is (equal '(255 255 0) (rgb 50)) "sixty degrees on, yellow")
    (is (equal '(255 0 0) (rgb -1)) "a negative index, a layer that is off, has the color of its absolute value")
    (is (null (dxf:aci->rgb 0)) "by block")
    (is (null (dxf:aci->rgb 256)) "by layer")
    (is (null (dxf:aci->rgb 300)))))

(test a-color-goes-to-the-index-whose-color-is-nearest
  (flet ((index (red green blue) (dxf:rgb->aci (dom:make-rgb red green blue))))
    (is (= 1 (index 255 0 0)))
    (is (= 5 (index 0 0 255)))
    (is (= 7 (index 255 255 255)))
    (is (= 7 (index 250 250 250)) "near white")
    (is (= 1 (index 240 20 20)))
    (loop for color-index from 1 to 255
          do (let ((color (dxf:aci->rgb color-index)))
               (is (equal (list (dom:rgb-red color) (dom:rgb-green color) (dom:rgb-blue color))
                          (let ((back (dxf:aci->rgb (dxf:rgb->aci color))))
                            (list (dom:rgb-red back) (dom:rgb-green back) (dom:rgb-blue back))))
                   "the index ~d comes back to its own color" color-index)))))

;;; Reading

(test the-reader-turns-text-into-numbers-the-way-dxf-files-write-them
  (flet ((number-of (text) (dxf::parse-real text)))
    (is (= 1d0 (number-of "1")))
    (is (= -1.5d0 (number-of "-1.5")))
    (is (= 1500d0 (number-of "1.5E+3")))
    (is (= 0.5d0 (number-of ".5")))
    (is (= 5d0 (number-of "5.")))
    (is (= 0.001d0 (number-of "1e-3")))
    (is (= 12.25d0 (number-of "  12.25  ")))
    (dolist (text '("" "abc" "1.2.3" "--1" "1e" "e5" "." "1e999999" "12abc"))
      (is (null (handler-case (number-of text) (error () nil))) "~s is not a number" text))))

(test the-lines-of-a-dxf-file-may-end-in-either-way
  (dolist (separator (list (string #\Newline) (coerce (list #\Return #\Newline) 'string) (string #\Return)))
    (let* ((text (format nil "  0~aSECTION~a  2~aENTITIES~a  0~aLINE~a  8~aA~a  0~aENDSEC~a  0~aEOF~a"
                         separator separator separator separator separator separator separator separator
                         separator separator separator separator))
           (document (dxf:parse-dxf-text text)))
      (is (= 1 (length (dxf:dxf-document-entities document))) "~s" separator)
      (is (string= "LINE" (dxf:record-type (first (dxf:dxf-document-entities document))))))))

(test a-file-that-is-not-dxf-is-refused-with-the-line-of-the-problem
  (dolist (text (list "hello" (format nil "  0~aSECTION~a  x~aENTITIES~a" #\Newline #\Newline #\Newline #\Newline)
                      "" (format nil "  0~aSECTION~a" #\Newline #\Newline)))
    (signals base:drawing-file-error (dxf:parse-dxf-text text)))
  (handler-case (dxf:parse-dxf-text (format nil "  0~aSECTION~a  2~aENTITIES~a  q~aLINE~a" #\Newline #\Newline #\Newline
                                            #\Newline #\Newline #\Newline))
    (base:drawing-file-error (condition)
      (is (search "line 5" (princ-to-string condition)))
      (is (search "group code" (princ-to-string condition))))))

(test the-reader-reads-the-sections-the-import-needs
  (let ((document (dxf:parse-dxf-text (fixture-text "plan-r12.dxf"))))
    (is (= 3 (length (dxf:dxf-document-layers document))))
    (is (equal '("LINE" "CIRCLE" "ARC" "POLYLINE" "TEXT")
               (mapcar #'dxf:record-type (dxf:dxf-document-entities document))))
    (let ((polyline (fourth (dxf:dxf-document-entities document))))
      (is (= 3 (length (dxf:record-children polyline))) "the vertices belong to the polyline")
      (is (every (lambda (child) (string= "VERTEX" (dxf:record-type child))) (dxf:record-children polyline))))
    (is (eq :millimeter (dxf:document-units document)))))

(test the-units-of-a-file-are-those-of-its-header-or-unknown
  (flet ((units (code)
           (dxf:document-units (dxf:parse-dxf-text
                                (dxf-text '(0 "SECTION") '(2 "HEADER") '(9 "$INSUNITS") (list 70 code)
                                          '(0 "ENDSEC") '(0 "EOF"))))))
    (is (eq :inch (units 1)))
    (is (eq :foot (units 2)))
    (is (eq :millimeter (units 4)))
    (is (eq :centimeter (units 5)))
    (is (eq :meter (units 6)))
    (is (null (units 0)) "unitless")
    (is (null (units 7)) "a unit the program does not take"))
  (is (null (dxf:document-units (dxf:parse-dxf-text (entities-file '((0 "LINE")))))) "no header, no units"))

;;; Bytes

(defun octets (&rest codes)
  "Return a vector of bytes."
  (make-array (length codes) :element-type '(unsigned-byte 8) :initial-contents codes))

(test text-is-decoded-as-utf-8-when-it-is-and-as-windows-western-otherwise
  (is (string= "abc" (dxf:decode-dxf-octets (octets 97 98 99))))
  (is (string= (format nil "caf~a" (code-char 233)) (dxf:decode-dxf-octets (octets 99 97 102 195 169))) "UTF-8")
  (is (string= (format nil "caf~a" (code-char 233)) (dxf:decode-dxf-octets (octets 99 97 102 233))) "one byte, 1252")
  (is (string= (format nil "~a" (code-char 8364)) (dxf:decode-dxf-octets (octets #x80))) "1252 has the euro at 0x80")
  (is (string= "abc" (dxf:decode-dxf-octets (octets #xEF #xBB #xBF 97 98 99))) "a byte order mark is dropped")
  (is (string= "" (dxf:decode-dxf-octets (octets)))))

(test a-binary-dxf-file-is-refused-and-says-how-to-save-one-that-is-not
  (let ((binary (map '(vector (unsigned-byte 8)) #'char-code "AutoCAD Binary DXF")))
    (handler-case (dxf:decode-dxf-octets binary)
      (base:drawing-file-error (condition)
        (is (search "binary DXF" (princ-to-string condition)))
        (is (search "ASCII" (princ-to-string condition)))))))

;;; Importing

(test the-fixture-becomes-a-drawing-with-its-layers-and-entities
  (multiple-value-bind (drawing report) (imported (fixture-text "plan-r12.dxf"))
    (is (equal '("0" "PAREDES" "COTAS") (mapcar #'dom:layer-name (dom:layers-in-order drawing))))
    (let ((walls (dom:find-layer-by-name drawing "PAREDES"))
          (dimensions (dom:find-layer-by-name drawing "COTAS")))
      (is (equal '(255 0 0) (list (dom:rgb-red (dom:layer-color walls)) (dom:rgb-green (dom:layer-color walls))
                                  (dom:rgb-blue (dom:layer-color walls)))))
      (is (dom:layer-visible walls))
      (is (not (dom:layer-visible dimensions)) "a negative color is a layer that is off")
      (is (dom:layer-locked dimensions) "flag 4 is a locked layer"))
    (is (equal '(:line :circle :arc :polyline :text) (mapcar #'dom:entity-kind (dom:drawing-entities drawing))))
    (is (= 5 (dxf:report-imported-total report)))
    (is (zerop (dxf:report-ignored-total report)))
    (is (eq :millimeter (dom:drawing-display-unit drawing)))
    (let ((line (entity-of-kind drawing :line)))
      (is (= 5000d0 (dom:line-x2 line)))
      (is (string= "PAREDES" (dom:layer-name (dom:find-layer drawing (dom:entity-layer-id line))))))
    (let ((arc (entity-of-kind drawing :arc)))
      (is (about (/ pi 2) (dom:arc-start-angle arc)))
      (is (about pi (dom:arc-end-angle arc)))
      (is (= 1000d0 (dom:arc-radius arc))))
    (let ((polyline (entity-of-kind drawing :polyline)))
      (is (dom:polyline-closed polyline))
      (is (= 6 (length (dom:polyline-coordinates polyline)))))
    (let ((text (entity-of-kind drawing :text)))
      (is (about (* 30 (/ pi 180)) (dom:text-angle text)))
      (is (string= (format nil "Sala ~a45 ~a" (code-char 176) (code-char 233)) (dom:text-string text))
          "%%d is a degree sign and \\U+00E9 an e with an acute accent"))))

(test a-line-a-circle-and-an-arc-are-carried-over-with-their-numbers
  (let ((drawing (imported (entities-file '((0 "LINE") (8 "A") (10 "1.0") (20 "2.0") (11 "3.5") (21 "-4.0"))
                                          '((0 "CIRCLE") (8 "A") (10 "10") (20 "20") (40 "5"))
                                          '((0 "ARC") (8 "A") (10 "0") (20 "0") (40 "7") (50 "350") (51 "10"))))))
    (let ((line (entity-of-kind drawing :line)))
      (is (equal '(1d0 2d0 3.5d0 -4d0)
                 (list (dom:line-x1 line) (dom:line-y1 line) (dom:line-x2 line) (dom:line-y2 line)))))
    (is (= 5d0 (dom:circle-radius (entity-of-kind drawing :circle))))
    (let ((arc (entity-of-kind drawing :arc)))
      (is (about (* 350 (/ pi 180)) (dom:arc-start-angle arc)))
      (is (about (* 10 (/ pi 180)) (dom:arc-end-angle arc)))
      (is (about (* 20 (/ pi 180)) (dom:arc-sweep arc)) "across the zero direction"))))

(test an-entity-on-a-layer-the-table-does-not-list-makes-the-layer
  (let ((drawing (imported (entities-file '((0 "LINE") (8 "NEW LAYER") (10 "0") (20 "0") (11 "1") (21 "1"))
                                          '((0 "LINE") (8 "NEW LAYER") (10 "0") (20 "0") (11 "2") (21 "2"))))))
    (is (equal '("0" "NEW LAYER") (mapcar #'dom:layer-name (dom:layers-in-order drawing))))
    (is (= 1 (dom:layer-id (dom:find-layer-by-name drawing "NEW LAYER"))))
    (is (= 2 (dom:drawing-entity-count drawing)))))

(test layer-names-the-program-cannot-keep-are-made-acceptable
  (let* ((long (make-string 100 :initial-element #\a))
         (text (dxf-text '(0 "SECTION") '(2 "TABLES") '(0 "TABLE") '(2 "LAYER")
                         '(0 "LAYER") '(2 "walls") '(62 "1")
                         '(0 "LAYER") '(2 "walls ") '(62 "2")
                         (list 0 "LAYER") (list 2 long) '(62 "3")
                         '(0 "ENDTAB") '(0 "ENDSEC") '(0 "EOF")))
         (drawing (imported text))
         (names (mapcar #'dom:layer-name (dom:layers-in-order drawing))))
    (is (= 4 (length names)) "the default layer and three more")
    (is (= 4 (length (remove-duplicates names :test #'string=))) "no two the same")
    (is (every (lambda (name) (<= (length name) dom:+maximum-layer-name-length+)) names))))

(test a-polyline-is-open-or-closed-and-an-arc-segment-is-cut-into-straight-ones
  (multiple-value-bind (drawing report)
      (imported (entities-file '((0 "LWPOLYLINE") (8 "A") (90 "3") (70 "0")
                                 (10 "0") (20 "0") (10 "100") (20 "0") (42 "1.0") (10 "100") (20 "100"))))
    (let ((polyline (entity-of-kind drawing :polyline)))
      (is (not (dom:polyline-closed polyline)))
      (is (> (length (dom:polyline-coordinates polyline)) 10) "the half circle after the second vertex")
      (is (about 0d0 (aref (dom:polyline-coordinates polyline) 0)))
      (is (about 100d0 (aref (dom:polyline-coordinates polyline)
                             (- (length (dom:polyline-coordinates polyline)) 2)))))
    (is (find-if (lambda (note) (search "Arc segments" note)) (dxf:report-notes report)))
    ;; The bulge of the second vertex makes the segment to the third a half circle of diameter 100,
    ;; centered on (100, 50), to the right of the chord because the bulge is positive.
    (let ((coordinates (dom:polyline-coordinates (entity-of-kind drawing :polyline))))
      (loop for index from 4 below (- (length coordinates) 2) by 2
            do (is (about 50d0 (geo:distance 100d0 50d0 (aref coordinates index) (aref coordinates (1+ index))) 1d-6)
                   "point ~d" (/ index 2))
               (is (>= (aref coordinates index) 100d0) "on the right of the chord, a positive bulge turns left")))))

(test a-closed-lwpolyline-and-a-bulge-on-its-closing-segment
  (let ((drawing (imported (entities-file '((0 "LWPOLYLINE") (8 "A") (90 "3") (70 "1")
                                            (10 "0") (20 "0") (10 "100") (20 "0") (10 "100") (20 "100") (42 "-0.5"))))))
    (let ((polyline (entity-of-kind drawing :polyline)))
      (is (dom:polyline-closed polyline))
      (is (> (length (dom:polyline-coordinates polyline)) 8) "the arc of the closing segment adds vertices"))))

(test the-old-style-polyline-keeps-its-vertices-and-leaves-out-meshes
  (multiple-value-bind (drawing report)
      (imported (entities-file '((0 "POLYLINE") (8 "A") (66 "1") (70 "0"))
                               '((0 "VERTEX") (8 "A") (10 "0") (20 "0"))
                               '((0 "VERTEX") (8 "A") (10 "10") (20 "0"))
                               '((0 "SEQEND") (8 "A"))
                               '((0 "POLYLINE") (8 "A") (66 "1") (70 "16"))
                               '((0 "VERTEX") (8 "A") (10 "0") (20 "0"))
                               '((0 "SEQEND") (8 "A"))))
    (is (= 1 (dom:drawing-entity-count drawing)))
    (is (equal '(("POLYLINE" . 1)) (dxf:report-ignored report)) "a polygon mesh is not a polyline")))

(test a-text-is-placed-where-its-alignment-says
  (let ((drawing (imported (entities-file '((0 "TEXT") (8 "A") (10 "0") (20 "0") (40 "100") (1 "ABCDEF")
                                            (72 "0") (73 "0"))
                                          '((0 "TEXT") (8 "A") (10 "0") (20 "0") (11 "1000") (21 "500") (40 "100")
                                            (1 "ABCDEF") (72 "1") (73 "0"))
                                          '((0 "TEXT") (8 "A") (10 "0") (20 "0") (11 "1000") (21 "500") (40 "100")
                                            (1 "ABCDEF") (72 "2") (73 "0"))))))
    (destructuring-bind (left center right) (entities-of-kind drawing :text)
      (is (= 0d0 (dom:text-x left)))
      (is (about (- 1000d0 (/ (geo:text-width 6 100d0) 2)) (dom:text-x center)) "centered on the alignment point")
      (is (about (- 1000d0 (geo:text-width 6 100d0)) (dom:text-x right)) "ending at it")
      (is (= 500d0 (dom:text-y center))))))

(test text-special-codes-and-escapes-become-the-characters-they-stand-for
  (flet ((cleaned (text) (dxf::clean-text text)))
    (is (string= (format nil "90~a" (code-char 176)) (cleaned "90%%d")))
    (is (string= (format nil "~a5" (code-char 177)) (cleaned "%%p5")))
    (is (string= (format nil "~a20" (code-char 248)) (cleaned "%%c20")))
    (is (string= "AB" (cleaned "%%uA%%oB")) "underline and overline toggles are dropped")
    (is (string= "A" (cleaned "%%065")) "%%nnn is the character with that code")
    (is (string= (format nil "~a" (code-char #x00E7)) (cleaned "\\U+00E7")))
    (is (string= "100%" (cleaned "100%")) "a lone percent sign is a percent sign")
    (is (string= "ab" (cleaned (format nil "a~cb" (code-char 7)))) "control characters go")))

(test a-multiline-text-becomes-one-text-for-each-line
  (multiple-value-bind (drawing report)
      (imported (entities-file '((0 "MTEXT") (8 "A") (10 "0") (20 "0") (40 "100") (71 "1")
                                 (1 "First line\\PSecond {\\fArial;line}\\P\\C1;Third"))))
    (let ((texts (entities-of-kind drawing :text)))
      (is (equal '("First line" "Second line" "Third") (mapcar #'dom:text-string texts)))
      (is (every (lambda (text) (about 100d0 (dom:text-height text))) texts))
      (is (> (dom:text-y (first texts)) (dom:text-y (second texts)) (dom:text-y (third texts))) "each below the last"))
    (is (find-if (lambda (note) (search "Multiline" note)) (dxf:report-notes report)))))

(test a-whole-ellipse-is-an-ellipse-and-a-part-of-one-is-a-polyline
  (multiple-value-bind (drawing report)
      (imported (entities-file '((0 "ELLIPSE") (8 "A") (10 "0") (20 "0") (11 "100") (21 "0") (40 "0.5")
                                 (41 "0") (42 "6.283185307179586"))
                               '((0 "ELLIPSE") (8 "A") (10 "0") (20 "0") (11 "0") (21 "100") (40 "0.25")
                                 (41 "0") (42 "3.141592653589793"))))
    (let ((ellipse (entity-of-kind drawing :ellipse)))
      (is (= 100d0 (dom:ellipse-radius-x ellipse)))
      (is (= 50d0 (dom:ellipse-radius-y ellipse))))
    (is (= 1 (length (entities-of-kind drawing :polyline))) "a half ellipse is an open polyline")
    (is (find-if (lambda (note) (search "Elliptical" note)) (dxf:report-notes report)))))

(test what-the-program-cannot-draw-is-counted-and-said
  (multiple-value-bind (drawing report)
      (imported (entities-file '((0 "SPLINE") (8 "A")) '((0 "HATCH") (8 "A")) '((0 "HATCH") (8 "A"))
                               '((0 "POINT") (8 "A") (10 "1") (20 "1"))
                               '((0 "LINE") (8 "A") (10 "0") (20 "0") (11 "1") (21 "1"))))
    (is (= 1 (dom:drawing-entity-count drawing)))
    (is (equal '(("HATCH" . 2) ("POINT" . 1) ("SPLINE" . 1)) (dxf:report-ignored report)))
    (is (= 4 (dxf:report-ignored-total report)))
    (let ((summary (dxf:report-summary report)))
      (is (search "1 entity imported." summary))
      (is (search "Left out: 2 HATCH, 1 POINT, 1 SPLINE." summary)))))

(test an-entity-that-would-be-invalid-is-left-out-with-the-reason
  (multiple-value-bind (drawing report)
      (imported (entities-file '((0 "LINE") (8 "A") (10 "5") (20 "5") (11 "5") (21 "5"))
                               '((0 "CIRCLE") (8 "A") (10 "0") (20 "0") (40 "0"))
                               '((0 "LINE") (8 "A") (10 "0") (20 "0") (11 "1") (21 "1"))))
    (is (= 1 (dom:drawing-entity-count drawing)))
    (is (= 2 (dxf:report-ignored-total report)))
    (let ((summary (dxf:report-summary report)))
      (is (search "Invalid line" summary))
      (is (search "Invalid circle" summary)))))

(test the-units-of-the-file-or-the-ones-given-set-the-scale
  (let ((text (entities-file '((0 "LINE") (8 "A") (10 "0") (20 "0") (11 "2") (21 "0")))))
    (is (= 2d0 (dom:line-x2 (entity-of-kind (imported text :units :millimeter) :line))))
    (is (= 20d0 (dom:line-x2 (entity-of-kind (imported text :units :centimeter) :line))))
    (is (= 2000d0 (dom:line-x2 (entity-of-kind (imported text :units :meter) :line))))
    (is (= 50.8d0 (dom:line-x2 (entity-of-kind (imported text :units :inch) :line))))
    (is (= 609.6d0 (dom:line-x2 (entity-of-kind (imported text :units :foot) :line))))
    (is (eq :meter (dom:drawing-display-unit (imported text :units :meter)))
        "lengths are shown in the unit of the file")
    (is (eq :millimeter (dom:drawing-display-unit (imported text :units :inch))))
    (is (= 2d0 (dom:line-x2 (entity-of-kind (imported text) :line))) "no unit said, millimetres")))

(test coordinates-far-from-the-origin-move-the-drawing-and-say-so
  (multiple-value-bind (drawing report)
      (imported (entities-file '((0 "LINE") (8 "A") (10 "7000000000") (20 "500000") (11 "7000001000") (21 "500000"))
                               '((0 "CIRCLE") (8 "A") (10 "7000000500") (20 "500500") (40 "200"))))
    (is (= 2 (dom:drawing-entity-count drawing)))
    (let ((line (entity-of-kind drawing :line))
          (circle (entity-of-kind drawing :circle)))
      (is (< (abs (dom:line-x1 line)) 1d7))
      (is (about 1000d0 (- (dom:line-x2 line) (dom:line-x1 line))) "the shape is kept")
      (is (about 500d0 (- (dom:circle-center-x circle) (dom:line-x1 line))) "and so are the distances"))
    (is (find-if (lambda (note) (search "far from the origin" note)) (dxf:report-notes report)))))

(test a-drawing-near-the-origin-is-not-moved
  (multiple-value-bind (drawing report)
      (imported (entities-file '((0 "LINE") (8 "A") (10 "1000") (20 "1000") (11 "2000") (21 "1000"))))
    (is (= 1000d0 (dom:line-x1 (entity-of-kind drawing :line))))
    (is (null (dxf:report-notes report)))))

(test a-mirrored-extrusion-turns-an-arc-and-a-circle-over
  (let ((drawing (imported (entities-file '((0 "CIRCLE") (8 "A") (10 "100") (20 "0") (40 "5") (230 "-1"))
                                          '((0 "ARC") (8 "A") (10 "100") (20 "0") (40 "5") (50 "0") (51 "90")
                                            (230 "-1"))))))
    (is (= -100d0 (dom:circle-center-x (entity-of-kind drawing :circle))))
    (let ((arc (entity-of-kind drawing :arc)))
      (is (= -100d0 (dom:arc-center-x arc)))
      (is (about (/ pi 2) (dom:arc-start-angle arc)) "0 to 90 seen from underneath is 90 to 180")
      (is (about pi (dom:arc-end-angle arc))))))

;;; Blocks

(defun blocks-file (blocks &rest entities)
  "Return the text of a DXF file with a blocks section and an entities section."
  (apply #'dxf-text (append '((0 "SECTION") (2 "BLOCKS"))
                            (apply #'append blocks)
                            '((0 "ENDSEC") (0 "SECTION") (2 "ENTITIES"))
                            (apply #'append entities)
                            '((0 "ENDSEC") (0 "EOF")))))

(defun door-block ()
  "Return the groups of a block called DOOR: a line of 900 along x and a quarter arc."
  '((0 "BLOCK") (2 "DOOR") (10 "0") (20 "0")
    (0 "LINE") (8 "0") (10 "0") (20 "0") (11 "900") (21 "0")
    (0 "ARC") (8 "0") (10 "0") (20 "0") (40 "900") (50 "0") (51 "90")
    (0 "ENDBLK")))

(test an-insert-puts-the-shapes-of-its-block-where-it-says
  (multiple-value-bind (drawing report)
      (imported (blocks-file (list (door-block))
                             '((0 "INSERT") (8 "DOORS") (2 "DOOR") (10 "1000") (20 "2000") (41 "1") (42 "1")
                               (50 "90"))))
    (is (equal '(:line :arc) (mapcar #'dom:entity-kind (dom:drawing-entities drawing))))
    (let ((line (entity-of-kind drawing :line))
          (arc (entity-of-kind drawing :arc)))
      (is (about 1000d0 (dom:line-x1 line)))
      (is (about 2000d0 (dom:line-y1 line)))
      (is (about 1000d0 (dom:line-x2 line)) "turned a quarter turn: the 900 along x now runs along y")
      (is (about 2900d0 (dom:line-y2 line)))
      (is (about 1000d0 (dom:arc-center-x arc)))
      (is (about 900d0 (dom:arc-radius arc)))
      (is (about (/ pi 2) (dom:arc-start-angle arc)) "the arc was turned by the same quarter turn")
      (is (about pi (dom:arc-end-angle arc))))
    (is (equal "DOORS" (dom:layer-name (dom:find-layer drawing (dom:entity-layer-id (entity-of-kind drawing :line)))))
        "what a block draws on layer 0 goes on the layer of the insert")
    (is (null (dxf:report-notes report)))))

(test an-insert-scales-and-repeats-its-block
  (let ((drawing (imported (blocks-file (list (door-block))
                                        '((0 "INSERT") (8 "A") (2 "DOOR") (10 "0") (20 "0") (41 "2") (42 "2")
                                          (70 "3") (71 "2") (44 "5000") (45 "4000"))))))
    (is (= 12 (dom:drawing-entity-count drawing)) "three columns and two rows of a line and an arc")
    (is (= 6 (length (entities-of-kind drawing :line))))
    (is (about 1800d0 (- (dom:line-x2 (first (entities-of-kind drawing :line)))
                         (dom:line-x1 (first (entities-of-kind drawing :line))))))
    (is (about 1800d0 (dom:arc-radius (first (entities-of-kind drawing :arc)))))
    (is (equal '(0d0 5000d0 10000d0 0d0 5000d0 10000d0)
               (mapcar (lambda (line) (dom:line-x1 line)) (entities-of-kind drawing :line)))
        "the columns 5000 apart, then the next row")))

(test a-mirrored-insert-turns-the-arc-the-other-way
  (let ((drawing (imported (blocks-file (list (door-block))
                                        '((0 "INSERT") (8 "A") (2 "DOOR") (10 "0") (20 "0") (41 "-1") (42 "1"))))))
    (let ((line (entity-of-kind drawing :line))
          (arc (entity-of-kind drawing :arc)))
      (is (about -900d0 (dom:line-x2 line)))
      (is (about (/ pi 2) (dom:arc-start-angle arc)) "a quarter arc from 0 to 90, mirrored across y, is 90 to 180")
      (is (about pi (dom:arc-end-angle arc))))))

(test a-stretched-block-turns-its-circles-into-polylines-and-says-so
  (multiple-value-bind (drawing report)
      (imported (blocks-file (list '((0 "BLOCK") (2 "ROUND") (10 "0") (20 "0")
                                     (0 "CIRCLE") (8 "0") (10 "0") (20 "0") (40 "100")
                                     (0 "ENDBLK")))
                             '((0 "INSERT") (8 "A") (2 "ROUND") (10 "0") (20 "0") (41 "2") (42 "1"))))
    (is (null (entities-of-kind drawing :circle)))
    (let ((polyline (entity-of-kind drawing :polyline)))
      (is (dom:polyline-closed polyline))
      (is (about 400d0 (- (reduce #'max (loop for index from 0 below (length (dom:polyline-coordinates polyline)) by 2
                                              collect (aref (dom:polyline-coordinates polyline) index)))
                          (reduce #'min (loop for index from 0 below (length (dom:polyline-coordinates polyline)) by 2
                                              collect (aref (dom:polyline-coordinates polyline) index))))
                 1d0)
          "twice as wide as the circle: a diameter of 200 doubled"))
    (is (find-if (lambda (note) (search "stretched" note)) (dxf:report-notes report)))))

(test blocks-inside-blocks-are-followed-and-a-block-that-contains-itself-stops
  (multiple-value-bind (drawing report)
      (imported (blocks-file (list '((0 "BLOCK") (2 "INNER") (10 "0") (20 "0")
                                     (0 "LINE") (8 "0") (10 "0") (20 "0") (11 "100") (21 "0")
                                     (0 "ENDBLK"))
                                   '((0 "BLOCK") (2 "OUTER") (10 "0") (20 "0")
                                     (0 "INSERT") (8 "0") (2 "INNER") (10 "0") (20 "500")
                                     (0 "INSERT") (8 "0") (2 "OUTER") (10 "0") (20 "0")
                                     (0 "ENDBLK")))
                             '((0 "INSERT") (8 "A") (2 "OUTER") (10 "1000") (20 "0"))))
    (is (plusp (dom:drawing-entity-count drawing)))
    (is (< (dom:drawing-entity-count drawing) 20) "the recursion was cut, not followed for ever")
    (is (find-if (lambda (note) (search "Blocks inside blocks" note)) (dxf:report-notes report)))
    (is (about 500d0 (dom:line-y1 (first (dom:drawing-entities drawing)))) "and the first level was placed right")))

(test an-insert-of-a-block-that-does-not-exist-is-left-out
  (multiple-value-bind (drawing report)
      (imported (entities-file '((0 "INSERT") (8 "A") (2 "NOWHERE") (10 "0") (20 "0"))))
    (is (= 0 (dom:drawing-entity-count drawing)))
    (is (equal '(("INSERT" . 1)) (dxf:report-ignored report)))))

(test a-dimension-is-carried-over-as-what-its-block-draws
  (multiple-value-bind (drawing report)
      (imported (blocks-file (list '((0 "BLOCK") (2 "*D1") (10 "0") (20 "0")
                                     (0 "LINE") (8 "0") (10 "0") (20 "0") (11 "1000") (21 "0")
                                     (0 "SOLID") (8 "0") (10 "0") (20 "0") (11 "100") (21 "30") (12 "100") (22 "-30")
                                     (13 "100") (23 "-30")
                                     (0 "MTEXT") (8 "0") (10 "500") (20 "50") (40 "100") (1 "1000")
                                     (0 "ENDBLK")))
                             '((0 "DIMENSION") (8 "COTAS") (2 "*D1") (70 "1"))))
    (is (equal '(:line :polyline :text) (mapcar #'dom:entity-kind (dom:drawing-entities drawing))))
    (is (string= "COTAS" (dom:layer-name (dom:find-layer drawing
                                                         (dom:entity-layer-id (entity-of-kind drawing :line))))))
    (is (find-if (lambda (note) (search "Dimensions" note)) (dxf:report-notes report)))
    (is (find-if (lambda (note) (search "Filled shapes" note)) (dxf:report-notes report)))))

;;; Exporting

(defun lines-of (text)
  "Return the lines of a DXF text."
  (with-input-from-string (stream text)
    (loop for line = (read-line stream nil nil)
          while line
          collect (string-right-trim '(#\Return) line))))

(defun groups-of (text)
  "Return the groups of a DXF text as (code value) lists."
  (loop for (code value) on (lines-of text) by #'cddr
        collect (list (parse-integer code) value)))

(defparameter *line-end-for-tests* (coerce (list #\Return #\Newline) 'string)
  "How a line of a DXF file written by the program ends.")

(defun export-sample-drawing ()
  "Return a drawing with every kind of entity, on two layers, one hidden and locked."
  (let ((drawing (dom:make-drawing)))
    (dom:add-layer drawing "Walls: 1/2" (dom:make-rgb 255 0 0))
    (let ((hidden (dom:add-layer drawing "hidden" (dom:make-rgb 0 0 255))))
      (dom:set-layer-visible drawing (dom:layer-id hidden) nil)
      (dom:set-layer-locked drawing (dom:layer-id hidden) t))
    (dom:apply-changes
     drawing
     (dom:create-entities-changes
      (list (dom:make-line :id 1 :layer-id 1 :x1 0d0 :y1 0d0 :x2 5000d0 :y2 2500d0)
            (dom:make-rectangle :id 2 :layer-id 1 :min-x 100d0 :min-y 200d0 :width 3000d0 :height 2500d0)
            (dom:make-circle :id 3 :layer-id 0 :center-x 500d0 :center-y 600d0 :radius 250d0)
            (dom:make-polyline :id 4 :layer-id 1 :coordinates (doubles 0d0 0d0 100d0 0d0 100d0 100d0) :closed nil)
            (dom:make-text :id 5 :layer-id 0 :x 10d0 :y 20d0 :height 150d0
                           :string (format nil "Cozinha ~a 90~a" (code-char 233) (code-char 176)) :angle 0.5d0)
            (dom:make-dimension :id 6 :layer-id 0 :x1 0d0 :y1 0d0 :x2 1000d0 :y2 0d0 :offset 500d0
                                :text-height 100d0 :unit :millimeter :precision 0)
            (dom:make-arc :id 7 :layer-id 2 :center-x 0d0 :center-y 0d0 :radius 900d0 :start-angle 0.5d0
                          :end-angle 2d0)
            (dom:make-ellipse :id 8 :layer-id 2 :center-x 100d0 :center-y 100d0 :radius-x 800d0 :radius-y 300d0)
            (dom:make-polygon :id 9 :layer-id 2 :center-x 0d0 :center-y 0d0 :radius 600d0 :sides 5))))
    drawing))

(test an-exported-file-is-a-well-formed-r12-dxf
  (multiple-value-bind (text report) (dxf:export-drawing-text (export-sample-drawing))
    (let ((groups (groups-of text)))
      (is (equal '(0 "SECTION") (first groups)))
      (is (equal '(0 "EOF") (car (last groups))))
      (is (member '(1 "AC1009") groups :test #'equal) "the version")
      (is (member '(70 "4") (loop for (code value) in groups collect (list code (string-trim " " value))) :test #'equal)
          "units: millimetres")
      (is (equal '("HEADER" "TABLES" "BLOCKS" "ENTITIES")
                 (loop for (first second) on groups
                       when (and (equal first '(0 "SECTION")) second)
                         collect (second second)))
          "the four sections, in the order readers expect")
      (is (every (lambda (group) (and (integerp (first group)) (stringp (second group)))) groups))
      (is (every (lambda (line) (every (lambda (character) (<= 32 (char-code character) 126)) line)) (lines-of text))
          "ASCII only")
      (is (search *line-end-for-tests* (subseq text 0 10)) "lines end as AutoCAD ends them"))
    (is (= 9 (dxf:report-imported-total report)))))

(defun exported-layers (drawing)
  "Return the layers of the layer table of the DXF text of DRAWING, each as an alist from group code to value."
  (let ((groups (groups-of (dxf:export-drawing-text drawing))))
    (loop for tail on groups
          when (equal '(0 "LAYER") (first tail))
            collect (loop for group in (rest tail)
                          until (= 0 (first group))
                          collect (cons (first group) (string-trim " " (second group)))))))

(test the-layer-table-has-the-names-colors-and-states-of-the-layers
  (let ((layers (exported-layers (export-sample-drawing))))
    (is (equal '("0" "Walls_ 1_2" "hidden") (mapcar (lambda (layer) (cdr (assoc 2 layer))) layers))
        "a name loses the characters that DXF forbids")
    (is (equal '("7" "1" "-5") (mapcar (lambda (layer) (cdr (assoc 62 layer))) layers))
        "white, red, and blue that is switched off by the sign")
    (is (equal '("0" "0" "4") (mapcar (lambda (layer) (cdr (assoc 70 layer))) layers)) "only the last is locked")
    (is (every (lambda (layer) (equal "CONTINUOUS" (cdr (assoc 6 layer)))) layers))))

(test two-layers-whose-names-differ-only-by-case-or-forbidden-characters-get-different-names
  (let ((drawing (dom:make-drawing)))
    (dom:add-layer drawing "a/b" (dom:make-rgb 255 0 0))
    (dom:add-layer drawing "a:b" (dom:make-rgb 255 0 0))
    (dom:add-layer drawing "a?b" (dom:make-rgb 255 0 0))
    (let ((names (mapcar (lambda (layer) (cdr (assoc 2 layer))) (exported-layers drawing))))
      (is (= 4 (length (remove-duplicates names :test #'string-equal))) "~s" names)
      (is (equal '("0" "a_b" "a_b_2" "a_b_3") names)))))

(test the-new-layers-keep-their-colors-visibility-and-locks-through-a-round-trip
  (let* ((drawing (export-sample-drawing))
         (back (imported (dxf:export-drawing-text drawing))))
    (is (equal '("0" "Walls_ 1_2" "hidden") (mapcar #'dom:layer-name (dom:layers-in-order back))))
    (let ((hidden (dom:find-layer-by-name back "hidden")))
      (is (not (dom:layer-visible hidden)))
      (is (dom:layer-locked hidden))
      (is (equal '(0 0 255) (list (dom:rgb-red (dom:layer-color hidden)) (dom:rgb-green (dom:layer-color hidden))
                                  (dom:rgb-blue (dom:layer-color hidden))))))
    (is (equal '(255 0 0) (let ((color (dom:layer-color (dom:find-layer-by-name back "Walls_ 1_2"))))
                            (list (dom:rgb-red color) (dom:rgb-green color) (dom:rgb-blue color)))))))

(defun entity-groups (text type)
  "Return the groups of the first entity of TYPE in the entities section of the DXF text, as (code value) lists."
  (let* ((groups (groups-of text))
         (start (position-if (lambda (group) (equal group (list 0 type))) groups
                             :start (position '(2 "ENTITIES") groups :test #'equal))))
    (when start
      (loop for group in (nthcdr (1+ start) groups)
            until (= 0 (first group))
            collect (list (first group) (string-trim " " (second group)))))))

(test every-kind-of-entity-is-written-as-the-entity-dxf-has-for-it
  (let ((text (dxf:export-drawing-text (export-sample-drawing))))
    (let ((line (entity-groups text "LINE")))
      (is (equal "Walls_ 1_2" (second (assoc 8 line))) "on its layer")
      (is (equal '(10 "0.0") (assoc 10 line)))
      (is (equal '(11 "5000.0") (assoc 11 line)))
      (is (equal '(21 "2500.0") (assoc 21 line))))
    (let ((circle (entity-groups text "CIRCLE")))
      (is (equal '(40 "250.0") (assoc 40 circle)))
      (is (equal '(10 "500.0") (assoc 10 circle))))
    (let ((arc (entity-groups text "ARC")))
      (is (equal '(40 "900.0") (assoc 40 arc)))
      (is (about 28.6478897565d0 (parse-real-for-test (second (assoc 50 arc))) 1d-6) "0.5 radian in degrees")
      (is (about 114.591559026d0 (parse-real-for-test (second (assoc 51 arc))) 1d-6)))
    (let ((text-groups (entity-groups text "TEXT")))
      (is (equal (list 1 (format nil "Cozinha \\U+00E9 90\\U+00B0")) (assoc 1 text-groups))
          "characters beyond ASCII are written as \\U+hhhh")
      (is (equal '(40 "150.0") (assoc 40 text-groups)))
      (is (about 28.6478897565d0 (parse-real-for-test (second (assoc 50 text-groups))) 1d-6)))))

(defun parse-real-for-test (text)
  "Return the number written in TEXT."
  (dxf::parse-real text))

(test a-rectangle-a-polygon-and-an-ellipse-are-closed-polylines-and-a-polyline-is-itself
  (multiple-value-bind (text report) (dxf:export-drawing-text (export-sample-drawing))
    (let* ((groups (groups-of text))
           (polylines (loop for tail on groups
                            when (equal '(0 "POLYLINE") (first tail))
                              collect (loop for group in (rest tail)
                                            until (equal group '(0 "SEQEND"))
                                            collect group))))
      (is (= 4 (length polylines)) "the rectangle, the polyline, the ellipse and the polygon")
      (flet ((vertices (polyline) (count '(0 "VERTEX") polyline :test #'equal))
             (closed (polyline) (string= "1" (string-trim " " (second (assoc 70 polyline))))))
        (is (equal '(4 3 128 5) (mapcar #'vertices polylines)))
        (is (equal '(t nil t t) (mapcar #'closed polylines)))))
    (is (find-if (lambda (note) (search "Ellipses" note)) (dxf:report-notes report)))
    (is (find-if (lambda (note) (search "Regular polygons" note)) (dxf:report-notes report)))
    (is (null (find-if (lambda (note) (search "Rectangles" note)) (dxf:report-notes report))))))

(test a-dimension-is-written-as-lines-arrowheads-and-text
  (let* ((drawing (dom:make-drawing)))
    (dom:apply-changes drawing (dom:create-entities-changes
                                (list (dom:make-dimension :id 1 :layer-id 0 :x1 0d0 :y1 0d0 :x2 1000d0 :y2 0d0
                                                          :offset 500d0 :text-height 100d0 :unit :millimeter
                                                          :precision 0))))
    (multiple-value-bind (text report) (dxf:export-drawing-text drawing)
      (let ((types (loop for (code value) in (groups-of text)
                         when (= 0 code) collect value)))
        (is (= 3 (count "LINE" types :test #'string=)) "two extension lines and the dimension line")
        (is (= 2 (count "SOLID" types :test #'string=)) "two arrowheads")
        (is (= 1 (count "TEXT" types :test #'string=)))
        (is (null (member "DIMENSION" types :test #'string=)) "no DIMENSION entity"))
      (is (equal '(1 "1000") (assoc 1 (entity-groups text "TEXT"))) "the text of the dimension")
      (is (find-if (lambda (note) (search "Dimensions" note)) (dxf:report-notes report))))))

(test a-written-dimension-is-read-back-as-lines-text-and-outlines
  (let* ((drawing (dom:make-drawing)))
    (dom:apply-changes drawing (dom:create-entities-changes
                                (list (dom:make-dimension :id 1 :layer-id 0 :x1 0d0 :y1 0d0 :x2 1000d0 :y2 0d0
                                                          :offset 500d0 :text-height 100d0 :unit :millimeter
                                                          :precision 0))))
    (let ((back (imported (dxf:export-drawing-text drawing))))
      (is (= 3 (length (entities-of-kind back :line))))
      (is (= 2 (length (entities-of-kind back :polyline))) "the arrowheads, as triangles")
      (is (string= "1000" (dom:text-string (entity-of-kind back :text)))))))

(test an-empty-drawing-is-a-valid-dxf-file-that-reads-back-empty
  (multiple-value-bind (text report) (dxf:export-drawing-text (dom:make-drawing))
    (is (= 0 (dxf:report-imported-total report)))
    (let ((back (imported text)))
      (is (= 0 (dom:drawing-entity-count back)))
      (is (equal '("0") (mapcar #'dom:layer-name (dom:layers-in-order back)))))))

;;; Round trips

(test lines-circles-arcs-polylines-and-texts-survive-an-export-and-an-import
  (let* ((drawing (export-sample-drawing))
         (back (imported (dxf:export-drawing-text drawing))))
    (let ((line (entity-of-kind back :line)))
      (is (equal '(0d0 0d0 5000d0 2500d0) (list (dom:line-x1 line) (dom:line-y1 line) (dom:line-x2 line)
                                                (dom:line-y2 line)))))
    (let ((circle (entity-of-kind back :circle)))
      (is (about 250d0 (dom:circle-radius circle)))
      (is (about 600d0 (dom:circle-center-y circle))))
    (let ((arc (entity-of-kind back :arc)))
      (is (about 0.5d0 (dom:arc-start-angle arc) 1d-8))
      (is (about 2d0 (dom:arc-end-angle arc) 1d-8))
      (is (about 900d0 (dom:arc-radius arc))))
    (let ((text (entity-of-kind back :text)))
      (is (string= (format nil "Cozinha ~a 90~a" (code-char 233) (code-char 176)) (dom:text-string text)))
      (is (about 0.5d0 (dom:text-angle text) 1d-8))
      (is (about 150d0 (dom:text-height text))))
    (is (= 6 (length (entities-of-kind back :polyline)))
        "the rectangle, the polyline, the ellipse and the polygon, and the two arrowheads of the dimension")))

(test coordinates-survive-an-export-and-an-import-exactly-enough
  (let* ((prng (rng:make-prng 91))
         (drawing (dom:make-drawing)))
    (dom:apply-changes drawing
                       (dom:create-entities-changes
                        (loop for id from 1 to 200
                              collect (dom:make-line :id id :layer-id 0
                                                     :x1 (rng:prng-next-double prng -1d6 1d6)
                                                     :y1 (rng:prng-next-double prng -1d6 1d6)
                                                     :x2 (rng:prng-next-double prng 2d6 3d6)
                                                     :y2 (rng:prng-next-double prng -1d6 1d6)))))
    (let ((back (imported (dxf:export-drawing-text drawing))))
      (is (= 200 (dom:drawing-entity-count back)))
      (loop for original in (dom:drawing-entities drawing)
            for copy in (dom:drawing-entities back)
            do (is (about (dom:line-x1 original) (dom:line-x1 copy) 1d-9))
               (is (about (dom:line-y2 original) (dom:line-y2 copy) 1d-9))))))

;;; A file as AutoCAD writes it

(defun autocad-style-file ()
  "Return the text of a DXF file written the way AutoCAD writes them (version 2000): a header of many
variables, classes, tables with handles and subclass markers, blocks for the model and paper space
and an anonymous dimension block, entities with handles, a paper space entity, and an objects
section."
  (dxf-text '(0 "SECTION") '(2 "HEADER")
            '(9 "$ACADVER") '(1 "AC1015")
            '(9 "$INSBASE") '(10 "0.0") '(20 "0.0") '(30 "0.0")
            '(9 "$EXTMIN") '(10 "-100.0") '(20 "-200.0") '(30 "0.0")
            '(9 "$INSUNITS") '(70 "6")
            '(9 "$DWGCODEPAGE") '(3 "ANSI_1252")
            '(0 "ENDSEC")
            '(0 "SECTION") '(2 "CLASSES")
            '(0 "CLASS") '(1 "ACDBDICTIONARYWDFLT") '(2 "AcDbDictionaryWithDefault") '(3 "ObjectDBX Classes")
            '(0 "ENDSEC")
            '(0 "SECTION") '(2 "TABLES")
            '(0 "TABLE") '(2 "VPORT") '(5 "8") '(330 "0") '(100 "AcDbSymbolTable") '(70 "1")
            '(0 "VPORT") '(5 "29") '(330 "8") '(100 "AcDbSymbolTableRecord") '(100 "AcDbViewportTableRecord")
            '(2 "*Active") '(70 "0")
            '(0 "ENDTAB")
            '(0 "TABLE") '(2 "LAYER") '(5 "2") '(330 "0") '(100 "AcDbSymbolTable") '(70 "2")
            '(0 "LAYER") '(5 "10") '(330 "2") '(100 "AcDbSymbolTableRecord") '(100 "AcDbLayerTableRecord")
            '(2 "0") '(70 "0") '(62 "7") '(420 "16777215") '(6 "Continuous") '(370 "-3") '(390 "F")
            '(0 "LAYER") '(5 "11") '(330 "2") '(100 "AcDbSymbolTableRecord") '(100 "AcDbLayerTableRecord")
            '(2 "A-WALL") '(70 "0") '(62 "30") '(6 "Continuous")
            '(0 "ENDTAB")
            '(0 "ENDSEC")
            '(0 "SECTION") '(2 "BLOCKS")
            '(0 "BLOCK") '(5 "20") '(100 "AcDbEntity") '(8 "0") '(100 "AcDbBlockBegin") '(2 "*Model_Space")
            '(70 "0") '(10 "0.0") '(20 "0.0") '(30 "0.0") '(3 "*Model_Space") '(1 "")
            '(0 "ENDBLK") '(5 "21") '(100 "AcDbEntity") '(8 "0") '(100 "AcDbBlockEnd")
            '(0 "BLOCK") '(5 "30") '(100 "AcDbEntity") '(8 "0") '(100 "AcDbBlockBegin") '(2 "*D1")
            '(70 "1") '(10 "0.0") '(20 "0.0") '(30 "0.0") '(3 "*D1") '(1 "")
            '(0 "LINE") '(5 "31") '(100 "AcDbEntity") '(8 "A-WALL") '(100 "AcDbLine")
            '(10 "0.0") '(20 "0.0") '(30 "0.0") '(11 "5.0") '(21 "0.0") '(31 "0.0")
            '(0 "ENDBLK") '(5 "32") '(100 "AcDbEntity") '(8 "0") '(100 "AcDbBlockEnd")
            '(0 "ENDSEC")
            '(0 "SECTION") '(2 "ENTITIES")
            '(0 "LWPOLYLINE") '(5 "40") '(100 "AcDbEntity") '(8 "A-WALL") '(100 "AcDbPolyline")
            '(90 "4") '(70 "1") '(43 "0.0") '(38 "0.0") '(39 "0.0")
            '(10 "0.0") '(20 "0.0") '(10 "6.0") '(20 "0.0") '(10 "6.0") '(20 "4.0") '(10 "0.0") '(20 "4.0")
            '(0 "MTEXT") '(5 "41") '(100 "AcDbEntity") '(8 "0") '(100 "AcDbMText")
            '(10 "1.0") '(20 "1.0") '(30 "0.0") '(40 "0.25") '(41 "3.0") '(71 "1") '(72 "5") '(1 "Sala\\Pde estar")
            '(0 "DIMENSION") '(5 "42") '(100 "AcDbEntity") '(8 "0") '(100 "AcDbDimension") '(2 "*D1") '(70 "1")
            '(0 "LINE") '(5 "43") '(67 "1") '(100 "AcDbEntity") '(8 "0") '(100 "AcDbLine")
            '(10 "0.0") '(20 "0.0") '(30 "0.0") '(11 "1.0") '(21 "1.0") '(31 "0.0")
            '(0 "ENDSEC")
            '(0 "SECTION") '(2 "OBJECTS")
            '(0 "DICTIONARY") '(5 "C") '(330 "0") '(100 "AcDbDictionary") '(281 "1")
            '(0 "ENDSEC")
            '(0 "EOF")))

(test a-file-as-autocad-writes-it-is-read-with-its-handles-classes-and-objects-skipped
  (multiple-value-bind (drawing report) (imported (autocad-style-file))
    (is (eq :meter (dom:drawing-display-unit drawing)))
    (is (equal '("0" "A-WALL") (mapcar #'dom:layer-name (dom:layers-in-order drawing))))
    (let ((color (dom:layer-color (dom:find-layer-by-name drawing "A-WALL"))))
      (is (equal '(255 127 0) (list (dom:rgb-red color) (dom:rgb-green color) (dom:rgb-blue color)))))
    (is (equal '(:polyline :text :text :line) (mapcar #'dom:entity-kind (dom:drawing-entities drawing)))
        "the room, the two lines of the text, and the line the dimension block draws; not the paper space line")
    (let ((room (entity-of-kind drawing :polyline)))
      (is (dom:polyline-closed room))
      (is (= 6000d0 (aref (dom:polyline-coordinates room) 2)) "meters, so 6.0 is 6000 millimetres"))
    (let ((texts (entities-of-kind drawing :text)))
      (is (equal '("Sala" "de estar") (mapcar #'dom:text-string texts)))
      (is (about 250d0 (dom:text-height (first texts)))))
    (is (find-if (lambda (note) (search "paper space" note)) (dxf:report-notes report)))
    (is (null (dxf:report-ignored report)) "everything in the model was carried over")))

(test a-dxf-without-an-entities-section-still-gives-its-layers
  (let ((drawing (imported (dxf-text '(0 "SECTION") '(2 "TABLES") '(0 "TABLE") '(2 "LAYER")
                                     '(0 "LAYER") '(2 "ONLY") '(62 "3")
                                     '(0 "ENDTAB") '(0 "ENDSEC") '(0 "EOF")))))
    (is (equal '("0" "ONLY") (mapcar #'dom:layer-name (dom:layers-in-order drawing))))
    (is (= 0 (dom:drawing-entity-count drawing)))))

(test importing-a-large-dxf-file-is-fast-enough
  (let* ((count 30000)
         (text (with-output-to-string (stream)
                 (format stream "  0~%SECTION~%  2~%ENTITIES~%")
                 (dotimes (index count)
                   (format stream "  0~%LINE~%  8~%A~%  10~%~d.0~%  20~%0.0~%  11~%~d.5~%  21~%100.0~%"
                           index index))
                 (format stream "  0~%ENDSEC~%  0~%EOF~%")))
         (started (sbcl:monotonic-nanoseconds)))
    (is (= count (dom:drawing-entity-count (imported text))))
    (is (< (/ (- (sbcl:monotonic-nanoseconds) started) 1d9) 10d0) "thirty thousand lines in seconds, not minutes")))
