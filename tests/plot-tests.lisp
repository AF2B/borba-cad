(in-package #:borba-cad/tests)

(def-suite plotting
  :description "Putting a drawing on a sheet: the scale, the sheet, the PDF and SVG files, and the questions."
  :in all)

(in-suite plotting)

;;; Helpers

(defun plot-drawing (&rest entities)
  "Return a drawing holding ENTITIES, on layer 0."
  (let ((drawing (dom:make-drawing)))
    (dom:apply-changes drawing (dom:create-entities-changes entities))
    drawing))

(defun plot-line (id x1 y1 x2 y2)
  "Return a line entity on layer 0."
  (dom:make-line :id id :layer-id 0 :x1 (float x1 1d0) :y1 (float y1 1d0) :x2 (float x2 1d0) :y2 (float y2 1d0)))

(defun plan-of (drawing &rest settings)
  "Return the plot plan of DRAWING with the plot settings made from SETTINGS."
  (plot:plan-plot drawing (apply #'plot:make-plot-settings settings)))

(defun stroke-points (plan)
  "Return the points of the first stroke of PLAN as a list of (x . y) conses."
  (plot:plot-stroke-points (find-if (lambda (item) (typep item 'plot:plot-stroke)) (plot:plot-plan-items plan))))

(defun octets-text (octets)
  "Return OCTETS as a string with a character for every byte."
  (map 'string #'code-char octets))

(defun near-p (expected actual &optional (tolerance 1d-6))
  "Return true when two numbers agree to TOLERANCE."
  (< (abs (- expected actual)) tolerance))

;;; Sheets and scales

(test the-sheets-are-measured-standing-up-in-millimetres
  (multiple-value-bind (width height) (plot:paper-size :a4)
    (is (= 210d0 width))
    (is (= 297d0 height)))
  (multiple-value-bind (width height) (plot:paper-size :a0)
    (is (= 841d0 width))
    (is (= 1189d0 height)))
  (is (string= "A3" (plot:paper-name :a3)))
  (signals base:invalid-input (plot:paper-size :a9)))

(test a-scale-is-typed-as-a-ratio-a-fraction-or-a-number-and-fit-is-nil
  (is (near-p 0.02d0 (plot:parse-plot-scale "1:50")))
  (is (near-p 0.02d0 (plot:parse-plot-scale "1/50")))
  (is (near-p 0.02d0 (plot:parse-plot-scale "50")))
  (is (near-p 0.01d0 (plot:parse-plot-scale " 1 : 100 ")))
  (is (near-p 2d0 (plot:parse-plot-scale "2:1")))
  (is (near-p 1d0 (plot:parse-plot-scale "1:1")))
  (is (near-p 0.04d0 (plot:parse-plot-scale "1:25")))
  (is (null (plot:parse-plot-scale "fit")))
  (is (null (plot:parse-plot-scale " FIT ")))
  (is (null (plot:parse-plot-scale ""))))

(test a-text-that-is-not-a-scale-is-refused-with-a-reason
  (dolist (text '("abc" "0" "1:0" "-5" "1:" ":5" "1:2:3x"))
    (handler-case (progn (plot:parse-plot-scale text)
                         (fail "~s was accepted as a scale" text))
      (base:invalid-input (condition)
        (is (plusp (length (base:invalid-input-reason condition))))))))

(test a-scale-is-said-as-a-ratio
  (is (string= "1:50" (plot:scale-text 0.02d0)))
  (is (string= "1:2" (plot:scale-text 0.5d0)))
  (is (string= "1:1" (plot:scale-text 1d0)))
  (is (string= "2:1" (plot:scale-text 2d0)))
  (is (string= "1:36.6" (plot:scale-text 0.027338129496402876d0)))
  (is (string= "1:100" (plot:scale-text 0.01d0))))

(test settings-that-make-no-sense-are-refused
  (signals base:invalid-input (plot:make-plot-settings :paper :a9))
  (signals base:invalid-input (plot:make-plot-settings :ratio 1d-7))
  (signals base:invalid-input (plot:make-plot-settings :ratio 1d7))
  (signals base:invalid-input (plot:make-plot-settings :line-width 0d0))
  (signals base:invalid-input (plot:make-plot-settings :margin 150d0))
  (signals base:invalid-input (plot:make-plot-settings :margin -1d0))
  (let ((settings (plot:make-plot-settings)))
    (is (eq :a4 (plot:plot-settings-paper settings)))
    (is (null (plot:plot-settings-ratio settings)))
    (is (plot:plot-settings-monochrome settings))))

;;; Where the drawing goes

(test a-drawing-with-nothing-visible-is-refused
  (signals base:export-refused (plan-of (dom:make-drawing)))
  (let ((drawing (plot-drawing (plot-line 1 0 0 100 0))))
    (dom:set-layer-visible drawing 0 nil)
    (handler-case (plan-of drawing)
      (base:export-refused (condition)
        (is (search "no visible entities" (princ-to-string condition)))))))

(test fitting-makes-the-drawing-as-large-as-the-sheet-allows-and-centers-it
  (let* ((plan (plan-of (plot-drawing (plot-line 1 0 0 1000 0) (plot-line 2 0 0 0 100))))
         (points (stroke-points plan)))
    (is (= 297d0 (plot:plot-plan-width plan)) "wider than high: the sheet lies down")
    (is (= 210d0 (plot:plot-plan-height plan)))
    (is (near-p 0.277d0 (plot:plot-plan-ratio plan)) "the width less two margins of 10, over 1000")
    (is (plot:plot-plan-fits plan))
    (is (near-p 10d0 (car (first points))))
    (is (near-p 287d0 (car (second points))))
    (is (near-p (/ 210d0 2) (+ (cdr (first points)) (/ (* 0.277d0 100) 2))) "centered up and down")))

(test a-tall-drawing-stands-the-sheet-up
  (let ((plan (plan-of (plot-drawing (plot-line 1 0 0 0 1000)))))
    (is (= 210d0 (plot:plot-plan-width plan)))
    (is (= 297d0 (plot:plot-plan-height plan)))
    (is (near-p 0.277d0 (plot:plot-plan-ratio plan) 1d-3))))

(test a-given-scale-puts-a-millimetre-of-paper-for-so-many-of-the-drawing
  (let* ((plan (plan-of (plot-drawing (plot-line 1 0 0 5000 0)) :paper :a3 :ratio 0.02d0))
         (points (stroke-points plan)))
    (is (= 0.02d0 (plot:plot-plan-ratio plan)))
    (is (near-p 100d0 (- (car (second points)) (car (first points)))) "5000 mm at 1:50 are 100 mm")
    (is (plot:plot-plan-fits plan))
    (is (near-p (/ 420d0 2) (/ (+ (car (first points)) (car (second points))) 2)) "in the middle of the sheet")))

(test a-drawing-too-big-for-the-sheet-says-so-and-is-still-centered
  (let ((plan (plan-of (plot-drawing (plot-line 1 0 0 20000 0)) :ratio 0.02d0)))
    (is (not (plot:plot-plan-fits plan)))
    (is (= 297d0 (plot:plot-plan-width plan)) "it lies down, which is the way it reaches out the least")))

(test the-sheet-turns-to-let-a-drawing-fit-when-the-scale-is-given
  (let ((plan (plan-of (plot-drawing (plot-line 1 0 0 0 5000)) :ratio 0.04d0)))
    (is (plot:plot-plan-fits plan) "200 mm tall fits the standing sheet")
    (is (= 297d0 (plot:plot-plan-height plan)))))

(test only-visible-layers-are-drawn
  (let* ((drawing (plot-drawing (plot-line 1 0 0 100 0)))
         (hidden (dom:add-layer drawing "hidden" (dom:make-rgb 1 2 3))))
    (dom:apply-changes drawing (dom:create-entities-changes
                                (list (dom:entity-with-layer (plot-line 2 0 50 100 50) (dom:layer-id hidden)))))
    (dom:set-layer-visible drawing (dom:layer-id hidden) nil)
    (let ((plan (plan-of drawing)))
      (is (= 1 (plot:plot-plan-entity-count plan)))
      (is (= 1 (length (plot:plot-plan-items plan)))))))

;;; What is drawn

(test every-kind-of-entity-becomes-strokes-fills-and-text
  (let* ((entities (list (plot-line 1 0 0 1000 0)
                         (dom:make-rectangle :id 2 :layer-id 0 :min-x 0d0 :min-y 0d0 :width 500d0 :height 300d0)
                         (dom:make-circle :id 3 :layer-id 0 :center-x 0d0 :center-y 0d0 :radius 400d0)
                         (dom:make-polyline :id 4 :layer-id 0 :closed t
                                            :coordinates (doubles 0d0 0d0 100d0 0d0 0d0 100d0))
                         (dom:make-text :id 5 :layer-id 0 :x 10d0 :y 10d0 :height 50d0 :string "Hi" :angle 0.3d0)
                         (dom:make-dimension :id 6 :layer-id 0 :x1 0d0 :y1 0d0 :x2 1000d0 :y2 0d0 :offset 200d0
                                             :text-height 80d0 :unit :millimeter :precision 0)
                         (dom:make-arc :id 7 :layer-id 0 :center-x 0d0 :center-y 0d0 :radius 300d0 :start-angle 0d0
                                       :end-angle 1d0)
                         (dom:make-ellipse :id 8 :layer-id 0 :center-x 0d0 :center-y 0d0 :radius-x 300d0
                                           :radius-y 100d0)
                         (dom:make-polygon :id 9 :layer-id 0 :center-x 0d0 :center-y 0d0 :radius 100d0 :sides 5)
                         (dom:make-radial-dimension :id 10 :layer-id 0 :center-x 0d0 :center-y 0d0 :radius 400d0
                                                    :angle 0.5d0 :diameter nil :text-height 50d0 :unit :millimeter
                                                    :precision 0)
                         (dom:make-angular-dimension :id 11 :layer-id 0 :vertex-x 0d0 :vertex-y 0d0 :start-angle 0d0
                                                     :end-angle 1d0 :radius 200d0 :text-height 50d0 :precision 1)))
         (plan (plan-of (apply #'plot-drawing entities))))
    (is (= 11 (plot:plot-plan-entity-count plan)))
    (let ((items (plot:plot-plan-items plan)))
      (is (every (lambda (item) (typep item '(or plot:plot-stroke plot:plot-fill plot:plot-text))) items))
      (is (= 4 (count-if (lambda (item) (typep item 'plot:plot-text)) items))
          "the text and the text of three dimensions")
      (is (<= 2 (count-if (lambda (item) (typep item 'plot:plot-fill)) items)))
      (is (every (lambda (item)
                   (every (lambda (point) (and (<= -1d-6 (car point) (+ 1d-6 (plot:plot-plan-width plan)))
                                                (<= -1d-6 (cdr point) (+ 1d-6 (plot:plot-plan-height plan)))))
                          (typecase item
                            (plot:plot-stroke (plot:plot-stroke-points item))
                            (plot:plot-fill (plot:plot-fill-points item)))))
                 items)
          "everything is on the sheet"))))

(test a-circle-is-a-closed-run-of-segments-that-is-never-far-from-it
  (let* ((plan (plan-of (plot-drawing (dom:make-circle :id 1 :layer-id 0 :center-x 0d0 :center-y 0d0 :radius 400d0))
                        :ratio 0.1d0))
         (stroke (first (plot:plot-plan-items plan)))
         (points (plot:plot-stroke-points stroke))
         (center-x (/ (plot:plot-plan-width plan) 2))
         (center-y (/ (plot:plot-plan-height plan) 2)))
    (is (plot:plot-stroke-closed stroke))
    (is (<= 16 (length points)))
    (dolist (point points)
      (is (near-p 40d0 (geo:distance center-x center-y (car point) (cdr point)) 1d-9) "on the circle of 40 mm"))
    (let ((chord (geo:distance (car (first points)) (cdr (first points)) (car (second points)) (cdr (second points)))))
      (is (<= (- 40d0 (sqrt (- (* 40d0 40d0) (* (/ chord 2) (/ chord 2))))) 0.0201d0)
          "a chord is at most 0.02 mm from the arc"))))

(test an-arc-keeps-its-two-ends
  (let* ((plan (plan-of (plot-drawing (dom:make-arc :id 1 :layer-id 0 :center-x 0d0 :center-y 0d0 :radius 500d0
                                                    :start-angle 0d0 :end-angle (/ pi 2)))
                        :ratio 0.1d0))
         (points (stroke-points plan))
         (first (first points))
         (last (car (last points))))
    (is (near-p 50d0 (- (car first) (car last))) "the start is 50 mm to the right of the end")
    (is (near-p 50d0 (- (cdr last) (cdr first))) "and below it")))

(test text-and-dimensions-are-sized-by-the-scale
  (let* ((plan (plan-of (plot-drawing (dom:make-text :id 1 :layer-id 0 :x 0d0 :y 0d0 :height 250d0 :string "Hall"
                                                     :angle 0.5d0)
                                      (plot-line 2 0 0 5000 0))
                        :ratio 0.02d0))
         (text (find-if (lambda (item) (typep item 'plot:plot-text)) (plot:plot-plan-items plan))))
    (is (string= "Hall" (plot:plot-text-string text)))
    (is (near-p 5d0 (plot:plot-text-height text)) "250 mm at 1:50")
    (is (near-p 0.5d0 (plot:plot-text-angle text)))))

(test the-ink-is-black-or-the-color-of-the-layer-unless-that-is-too-light
  (let ((drawing (plot-drawing (plot-line 1 0 0 100 0)))
        (orange (dom:add-layer (dom:make-drawing) "x" (dom:make-rgb 255 120 80))))
    (declare (ignore orange))
    (let ((layer (dom:add-layer drawing "orange" (dom:make-rgb 220 90 30))))
      (dom:apply-changes drawing (dom:create-entities-changes
                                  (list (dom:entity-with-layer (plot-line 2 0 10 100 10) (dom:layer-id layer)))))
      (flet ((colors (&rest settings)
               (mapcar #'plot:plot-stroke-color (plot:plot-plan-items (apply #'plan-of drawing settings)))))
        (is (equal '((0d0 0d0 0d0) (0d0 0d0 0d0)) (colors :monochrome t)))
        (let ((colored (colors :monochrome nil)))
          (is (equal '(0d0 0d0 0d0) (first colored)) "the default layer is light gray, which would not show")
          (is (near-p (/ 220 255d0) (first (second colored))))
          (is (near-p (/ 30 255d0) (third (second colored)))))))))

;;; PDF

(defun xref-offsets (text)
  "Return the byte offsets in the cross-reference table of the PDF TEXT, for the objects, in order."
  (let* ((start (1+ (search (format nil "~%xref~%") text :from-end t)))
         (offsets '()))
    (with-input-from-string (stream text :start start)
      (read-line stream)
      (let ((count (parse-integer (read-line stream) :start 2)))
        (read-line stream)
        (dotimes (index (1- count))
          (push (parse-integer (read-line stream) :end 10) offsets))))
    (nreverse offsets)))

(defparameter *plot-sample*
  (list (plot-line 1 0 0 2000 0)
        (dom:make-circle :id 2 :layer-id 0 :center-x 1000d0 :center-y 500d0 :radius 300d0)
        (dom:make-text :id 3 :layer-id 0 :x 100d0 :y 100d0 :height 100d0 :string "Hall (1) \\ 20" :angle 0d0))
  "Entities for the PDF tests.")

(test a-pdf-is-a-header-objects-a-cross-reference-table-and-a-trailer
  (multiple-value-bind (octets plan replaced) (plot:render-pdf (apply #'plot-drawing *plot-sample*)
                                                              (plot:make-plot-settings))
    (declare (ignore plan))
    (let ((text (octets-text octets)))
      (is (zerop replaced))
      (is (eql 0 (search "%PDF-1.4" text)))
      (is (string= (format nil "%%EOF~%") (subseq text (- (length text) 6))))
      (is (search "/Type /Catalog" text))
      (is (search "/BaseFont /Courier" text))
      (is (search "/MediaBox [0 0 841.89 595.28]" text) "A4 lying down, in points")
      (let ((offsets (xref-offsets text)))
        (is (= 6 (length offsets)))
        (loop for offset in offsets
              for number from 1
              do (is (eql offset (search (format nil "~d 0 obj" number) text :start2 offset))
                     "object ~d is where the table says" number)))
      (let ((start (parse-integer text :start (+ (search "startxref" text :from-end t) 10) :junk-allowed t)))
        (is (eql start (search (format nil "xref~%0 7") text :start2 start)) "startxref points at the table")))))

(test the-length-of-the-content-is-the-length-of-the-content
  (let* ((text (octets-text (plot:render-pdf (apply #'plot-drawing *plot-sample*) (plot:make-plot-settings))))
         (declared (parse-integer text :start (+ (search "/Length " text) 8) :junk-allowed t))
         (begin (+ (search (format nil "stream~%") text) 7))
         (end (search (format nil "endstream") text)))
    (is (= declared (- end begin)))))

(test the-content-draws-in-points-with-round-joins-and-the-line-width
  (let* ((text (octets-text (plot:render-pdf (apply #'plot-drawing *plot-sample*)
                                             (plot:make-plot-settings :ratio 0.1d0 :line-width 0.5d0)))))
    (is (search "1 J" text))
    (is (search "1 j" text))
    (is (search "1.417 w" text) "0.5 mm is 1.417 points")
    (is (search " m" text))
    (is (search " l" text))
    (is (search "S" text))
    (is (search "BT" text))
    (is (search "/F1 28.346 Tf" text) "100 mm of text at 1:10 is 10 mm, 28.346 points")))

(test strings-are-escaped-and-the-characters-the-font-lacks-become-question-marks
  (multiple-value-bind (octets plan replaced)
      (plot:render-pdf (plot-drawing (plot-line 1 0 0 100 0)
                                     (dom:make-text :id 2 :layer-id 0 :x 0d0 :y 0d0 :height 10d0
                                                    :string (format nil "A(B)\\~aC~a~aD" (code-char 176)
                                                                    (code-char #x41F) (code-char #x20AC))))
                       (plot:make-plot-settings))
    (declare (ignore plan))
    (let ((text (octets-text octets)))
      (is (search "(A\\(B\\)\\\\\\260C??D) Tj" text)
          "the parentheses and the backslash are escaped, the degree sign is 260 octal")
      (is (= 2 replaced))
      (is (every (lambda (byte) (< byte 256)) octets))
      (is (null (find-if (lambda (character) (> (char-code character) 127)) (subseq text 20)))
          "after the header comment the file is plain ASCII"))))

(test the-same-drawing-always-makes-the-same-file
  (let ((a (plot:render-pdf (apply #'plot-drawing *plot-sample*) (plot:make-plot-settings :paper :a3 :ratio 0.05d0)))
        (b (plot:render-pdf (apply #'plot-drawing *plot-sample*) (plot:make-plot-settings :paper :a3 :ratio 0.05d0))))
    (is (equalp a b))))

(test a-bigger-sheet-has-a-bigger-media-box
  (let ((text (octets-text (plot:render-pdf (apply #'plot-drawing *plot-sample*)
                                            (plot:make-plot-settings :paper :a0)))))
    (is (search "/MediaBox [0 0 3370.39 2383.94]" text))))

(test colors-are-set-for-strokes-and-fills
  (let ((mono (octets-text (plot:render-pdf (apply #'plot-drawing *plot-sample*) (plot:make-plot-settings)))))
    (is (search "0 0 0 RG" mono))
    (is (search "0 0 0 rg" mono) "the text and the arrowheads are black"))
  (let* ((drawing (apply #'plot-drawing *plot-sample*))
         (layer (dom:add-layer drawing "red" (dom:make-rgb 200 0 0))))
    (dom:apply-changes drawing (dom:create-entities-changes
                                (list (dom:entity-with-layer (plot-line 9 0 900 2000 900) (dom:layer-id layer)))))
    (is (search "0.784 0 0 RG" (octets-text (plot:render-pdf drawing (plot:make-plot-settings :monochrome nil)))))))

;;; SVG

(test an-svg-is-the-sheet-in-millimetres-with-y-turned-over
  (multiple-value-bind (text plan) (plot:render-svg (plot-drawing (plot-line 1 0 0 1000 0)
                                                                 (plot-line 2 0 0 0 100))
                                                   (plot:make-plot-settings))
    (declare (ignore plan))
    (is (search "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"297mm\" height=\"210mm\" viewBox=\"0 0 297 210\">"
                text))
    (is (search "<rect width=\"297\" height=\"210\" fill=\"white\"/>" text))
    (is (search "stroke-width=\"0.25\"" text))
    (let ((first (search "<polyline points=\"" text)))
      (is (numberp first))
      (is (search "10,118.85 287,118.85" text :start2 first) "the long line, below the middle of the height")
      (is (search "10,118.85 10,91.15" text) "the short one goes up, so its y gets smaller"))))

(test svg-text-is-escaped-turned-and-sized
  (let ((text (plot:render-svg (plot-drawing (plot-line 1 0 0 1000 0)
                                             (dom:make-text :id 2 :layer-id 0 :x 100d0 :y 100d0 :height 100d0
                                                            :string "A & B < C \"D\"" :angle (/ pi 2)))
                               (plot:make-plot-settings :ratio 0.1d0))))
    (is (search "A &amp; B &lt; C &quot;D&quot;" text))
    (is (search "font-size=\"10\"" text))
    (is (search "rotate(-90 " text) "counter-clockwise in a sheet that goes up is minus in one that goes down")
    (is (search "font-family=\"Courier New, Courier, monospace\"" text))
    (is (search "xml:space=\"preserve\"" text))))

(test svg-fills-and-closed-shapes-are-polygons
  (let ((text (plot:render-svg (plot-drawing (dom:make-rectangle :id 1 :layer-id 0 :min-x 0d0 :min-y 0d0 :width 100d0
                                                                 :height 50d0)
                                             (dom:make-dimension :id 2 :layer-id 0 :x1 0d0 :y1 0d0 :x2 100d0
                                                                 :y2 0d0 :offset 30d0 :text-height 10d0
                                                                 :unit :millimeter :precision 0))
                               (plot:make-plot-settings))))
    (is (search "<polygon points=" text))
    (is (search "stroke=\"none\"/>" text) "an arrowhead")
    (is (search "fill=\"#000000\"" text))))

(test svg-colors-are-hexadecimal
  (let* ((drawing (plot-drawing (plot-line 1 0 0 100 0)))
         (layer (dom:add-layer drawing "blue" (dom:make-rgb 0 128 255))))
    (dom:apply-changes drawing (dom:create-entities-changes
                                (list (dom:entity-with-layer (plot-line 2 0 10 100 10) (dom:layer-id layer)))))
    (is (search "stroke=\"#0080ff\"" (plot:render-svg drawing (plot:make-plot-settings :monochrome nil))))))

;;; In the application

(defun plot-app (directory &rest entities)
  "Return an application with a file in DIRECTORY and a drawing of ENTITIES, or of a line and a text."
  (let ((application (app:make-app (quiet-config)
                                   :drawing (apply #'plot-drawing
                                                   (or entities
                                                       (list (plot-line 1 0 0 5000 0)
                                                             (dom:make-text :id 2 :layer-id 0 :x 100d0 :y 100d0
                                                                            :height 250d0 :string "Hall"))))
                                   :width 1440d0
                                   :height 900d0)))
    (setf (app:app-file-path application) (merge-pathnames "current.bcad" directory))
    (set-test-view application)
    application))

(test the-more-menu-offers-the-two-exports
  (with-temporary-directory (directory)
    (let ((application (plot-app directory)))
      (click-widget application :more)
      (let ((labels (mapcar #'ui:widget-label (menu-rows application))))
        (is (member "Export as PDF..." labels :test #'string=))
        (is (member "Export as SVG..." labels :test #'string=))))))

(test exporting-a-pdf-asks-the-sheet-then-the-scale-then-the-colors-then-the-name
  (with-temporary-directory (directory)
    (let ((application (plot-app directory)))
      (app:operation-export-pdf application)
      (is (string= "Export as PDF" (prompt-title-of application)))
      (is (equal '("A4" "A3" "A2" "A1" "Cancel") (prompt-labels application)))
      (choose-in-prompt application 1)
      (is (string= "Export as PDF" (prompt-title-of application)))
      (is (eq :text (app:prompt-kind (app:app-prompt application))) "the scale is typed")
      (type-text application "1:100")
      (press application :enter)
      (is (equal '("Black" "Layer colors" "Cancel") (prompt-labels application)))
      (choose-in-prompt application 0)
      (is (string= "Export as a PDF file" (dialog-title-text application)))
      (is (string= "current.pdf" (ui:edit-state-text (app:file-dialog-name-edit (dialog-of application)))))
      (is (string= "Export" (ui:widget-label (accept-button application))))
      (press application :enter)
      (is (null (app:app-dialog application)))
      (let ((path (merge-pathnames "current.pdf" directory)))
        (is (probe-file path))
        (is (search "%PDF-1.4" (fixture-like-text path)))
        (is (search "/MediaBox [0 0 1190.55 841.89]" (fixture-like-text path)) "A3 lying down"))
      (is (string= "PDF exported" (prompt-title-of application)))
      (is (search "current.pdf was saved" (app:prompt-message (app:app-prompt application))))
      (is (search "Sheet A3, scale 1:100" (app:prompt-message (app:app-prompt application)))))))

(test exporting-an-svg-goes-through-the-same-questions
  (with-temporary-directory (directory)
    (let ((application (plot-app directory)))
      (app:operation-export-svg application)
      (is (string= "Export as SVG" (prompt-title-of application)))
      (choose-in-prompt application 0)
      (press application :enter)
      (choose-in-prompt application 1)
      (is (string= "Export as an SVG file" (dialog-title-text application)))
      (is (string= "current.svg" (ui:edit-state-text (app:file-dialog-name-edit (dialog-of application)))))
      (press application :enter)
      (let ((text (fixture-like-text (merge-pathnames "current.svg" directory))))
        (is (search "<svg xmlns" text))
        (is (search "stroke=\"#" text))
        (is (search "Hall" text)))
      (is (string= "SVG exported" (prompt-title-of application))))))

(test the-scale-is-asked-again-until-it-is-one
  (with-temporary-directory (directory)
    (let ((application (plot-app directory)))
      (app:operation-export-pdf application)
      (choose-in-prompt application 0)
      (type-text application "big")
      (press application :enter)
      (is (eq :text (app:prompt-kind (app:app-prompt application))) "asked again")
      (is (search "type a scale" (app:prompt-message (app:app-prompt application))))
      (is (search "Type the scale" (app:prompt-message (app:app-prompt application))) "and how")
      (type-text application "0")
      (press application :enter)
      (is (search "above zero" (app:prompt-message (app:app-prompt application))))
      (type-text application "1:200")
      (press application :enter)
      (is (equal '("Black" "Layer colors" "Cancel") (prompt-labels application))))))

(test what-was-chosen-is-remembered-for-the-next-export
  (with-temporary-directory (directory)
    (let ((application (plot-app directory)))
      (app:operation-export-pdf application)
      (choose-in-prompt application 2)
      (type-text application "1:50")
      (press application :enter)
      (choose-in-prompt application 1)
      (press application :enter)
      (let ((settings (app:app-plot-settings application)))
        (is (eq :a2 (plot:plot-settings-paper settings)))
        (is (near-p 0.02d0 (plot:plot-settings-ratio settings)))
        (is (not (plot:plot-settings-monochrome settings))))
      (choose-in-prompt application 0)
      (app:operation-export-pdf application)
      (choose-in-prompt application 0)
      (is (string= "1:50" (ui:edit-state-text (app:prompt-edit (app:app-prompt application))))
          "the scale question starts with the last answer"))))

(test cancel-at-any-question-ends-the-export
  (with-temporary-directory (directory)
    (let ((application (plot-app directory)))
      (app:operation-export-pdf application)
      (choose-in-prompt application 4)
      (is (null (app:app-prompt application)))
      (is (null (app:app-dialog application)))
      (app:operation-export-pdf application)
      (choose-in-prompt application 0)
      (press application :escape)
      (is (null (app:app-prompt application)))
      (is (null (app:app-dialog application)))
      (app:operation-export-pdf application)
      (choose-in-prompt application 0)
      (press application :enter)
      (choose-in-prompt application 2)
      (is (null (app:app-prompt application)))
      (is (null (app:app-dialog application)))
      (is (null (probe-file (merge-pathnames "current.pdf" directory)))))))

(test an-empty-drawing-says-so-without-asking
  (with-temporary-directory (directory)
    (let ((application (app:make-app (quiet-config) :drawing (dom:make-drawing) :width 1440d0 :height 900d0)))
      (app:operation-export-pdf application)
      (is (null (app:app-prompt application)))
      (is (eq :error (cdr (app:app-message application))))
      (is (search "nothing to export" (message-text application))))))

(test a-drawing-with-nothing-visible-says-so-in-the-notice
  (with-temporary-directory (directory)
    (let ((application (plot-app directory)))
      (dom:set-layer-visible (app:app-drawing application) 0 nil)
      (app:operation-export-pdf application)
      (choose-in-prompt application 0)
      (press application :enter)
      (choose-in-prompt application 0)
      (press application :enter)
      (is (string= "Could not export" (prompt-title-of application)))
      (is (search "no visible entities" (app:prompt-message (app:app-prompt application))))
      (is (null (probe-file (merge-pathnames "current.pdf" directory)))))))

(test the-notice-warns-when-the-drawing-does-not-fit-and-when-characters-were-replaced
  (with-temporary-directory (directory)
    (let ((application (plot-app directory
                                 (plot-line 1 0 0 90000 0)
                                 (dom:make-text :id 2 :layer-id 0 :x 0d0 :y 100d0 :height 250d0
                                                :string (format nil "~a~a" (code-char #x41F) (code-char #x440))))))
      (app:operation-export-pdf application)
      (choose-in-prompt application 0)
      (type-text application "1:50")
      (press application :enter)
      (choose-in-prompt application 0)
      (press application :enter)
      (let ((message (app:prompt-message (app:app-prompt application))))
        (is (search "larger than the sheet" message))
        (is (search "2 characters could not be written" message))))))

(test the-questions-and-the-notice-speak-every-language
  (dolist (language '(:pt-br :ru-ru))
    (i18n:with-language (language)
      (with-temporary-directory (directory)
        (let ((application (plot-app directory)))
          (app:operation-export-pdf application)
          (is (null (untranslated-texts (list (prompt-title-of application)
                                              (app:prompt-message (app:app-prompt application)))
                                        language))
              "the sheet question in ~a" language)
          (choose-in-prompt application 0)
          (is (null (untranslated-texts (list (app:prompt-message (app:app-prompt application))) language))
              "the scale question in ~a" language)
          (type-text application "1:100")
          (press application :enter)
          (is (null (untranslated-texts (cons (app:prompt-message (app:app-prompt application))
                                              (prompt-labels application))
                                        language))
              "the colors question in ~a" language)
          (choose-in-prompt application 0)
          (is (null (untranslated-texts (list (dialog-title-text application)
                                              (ui:widget-label (accept-button application)))
                                        language)))
          (press application :enter)
          (is (null (untranslated-texts (list (prompt-title-of application)) language))
              "the notice title in ~a" language)
          (is (not (search "was saved" (app:prompt-message (app:app-prompt application))))
              "the notice in ~a" language))))))
