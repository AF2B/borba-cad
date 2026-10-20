(in-package #:borba-cad/tests)

(def-suite sheets
  :description "The sheets that are printed: viewports, title blocks, how they are saved, cut and written as a PDF."
  :in all)

(in-suite sheets)

;;; Helpers

(defun a-sheet-viewport (&key (x 20d0)
                        (y 40d0)
                        (width 100d0)
                        (height 50d0)
                        (center-x 500d0)
                        (center-y 0d0)
                        (scale 0.1d0))
  "Return a viewport, 100 by 50 millimetres on the sheet, with a window of one metre across on the drawing.

X: where on the sheet.
Y: where on the sheet.
WIDTH: its width.
HEIGHT: its height.
CENTER-X: x of the drawing at its center.
CENTER-Y: y of the drawing at its center.
SCALE: its scale."
  (dom:make-viewport :x x
                     :y y
                     :width width
                     :height height
                     :center-x center-x
                     :center-y center-y
                     :scale scale))

(defun long-text (length)
  "Return a text of that many letters.

LENGTH: how many."
  (make-string length :initial-element #\a))

(defun refusal-of (thunk)
  "Return the sentence that a thunk is refused with, or NIL when it is not refused.

THUNK: function of no arguments."
  (handler-case (progn (funcall thunk) nil)
    (base:edit-refused (condition)
      (princ-to-string condition))))

(defun sheet-drawing (&rest entities)
  "Return a drawing that holds ENTITIES and no sheet.

ENTITIES: the entities."
  (apply #'plot-drawing entities))

(defun stroke-items (plan)
  "Return the strokes of a plan.

PLAN: a plot plan."
  (remove-if-not (lambda (item) (typep item 'plot:plot-stroke)) (plot:plot-plan-items plan)))

(defun text-items (plan)
  "Return the texts of a plan.

PLAN: a plot plan."
  (remove-if-not (lambda (item) (typep item 'plot:plot-text)) (plot:plot-plan-items plan)))

(defun point-list (stroke)
  "Return the points of a stroke as a list of lists of two numbers.

STROKE: a plot stroke."
  (mapcar (lambda (point) (list (car point) (cdr point))) (plot:plot-stroke-points stroke)))

;;; Viewports and sheets

(test a-sheet-viewport-says-where-it-is-and-what-it-shows
  (let ((viewport (a-sheet-viewport)))
    (is (= 100d0 (dom:viewport-width viewport)))
    (is (= 0.1d0 (dom:viewport-scale viewport)))
    (let ((window (dom:viewport-window viewport)))
      (is (approx= 0d0 (geo:bounds-min-x window)))
      (is (approx= 1000d0 (geo:bounds-max-x window)))
      (is (approx= -250d0 (geo:bounds-min-y window)))
      (is (approx= 250d0 (geo:bounds-max-y window))))
    (is (= 25d0 (dom:viewport-x (dom:viewport-moved viewport 5d0 0d0))))
    (is (= 0.5d0 (dom:viewport-scale (dom:viewport-with viewport :scale 0.5d0))))
    (is (= 100d0 (dom:viewport-width (dom:viewport-with viewport :scale 0.5d0))) "the rest stays")))

(test a-sheet-viewport-that-makes-no-sense-is-refused-and-says-why
  (is (search "width and a height above zero" (refusal-of (lambda () (a-sheet-viewport :width 0d0)))))
  (is (search "out of range" (refusal-of (lambda () (a-sheet-viewport :scale 0d0)))))
  (is (search "out of range" (refusal-of (lambda () (a-sheet-viewport :scale 2d6)))))
  (is (search "too far from the sheet" (refusal-of (lambda () (a-sheet-viewport :x 200000d0)))))
  (is (search "numbers of a reasonable size" (refusal-of (lambda () (a-sheet-viewport :center-x 1d9))))))

(test a-sheet-has-a-name-a-paper-a-title-block-and-viewports
  (let ((sheet (dom:make-sheet :name "  Plan  "
                               :paper :a2
                               :landscape nil
                               :fields '((:title . "Ground floor") (:project . "House"))
                               :viewports (list (a-sheet-viewport)))))
    (is (equal "Plan" (dom:sheet-name sheet)))
    (is (eq :a2 (dom:sheet-paper sheet)))
    (is (not (dom:sheet-landscape sheet)))
    (is (equal '(:project :title :author :date :revision) (mapcar #'car (dom:sheet-fields sheet)))
        "every field, in the same order")
    (is (equal "House" (dom:sheet-field sheet :project)))
    (is (equal "" (dom:sheet-field sheet :author)))
    (is (= 1 (length (dom:sheet-viewports sheet))))
    (let ((changed (dom:sheet-with sheet :fields '((:author . "Ana")) :name "Plan B")))
      (is (equal "Ana" (dom:sheet-field changed :author)))
      (is (equal "House" (dom:sheet-field changed :project)) "a field that was not named stays")
      (is (equal "Plan B" (dom:sheet-name changed))))))

(test a-sheet-that-makes-no-sense-is-refused-and-says-why
  (is (search "needs a name" (refusal-of (lambda () (dom:make-sheet :name "  ")))))
  (is (search "not a paper size" (refusal-of (lambda () (dom:make-sheet :paper :a9)))))
  (is (search "at most" (refusal-of (lambda ()
                                      (dom:make-sheet :fields (list (cons :title (long-text 200))))))))
  (is (search "line break" (refusal-of (lambda () (dom:make-sheet :fields (list (cons :title (format nil "a~%b"))))))))
  (is (search "at most" (refusal-of (lambda () (dom:make-sheet :name (make-string 100 :initial-element #\a))))))
  (is (search "are viewports" (refusal-of (lambda () (dom:make-sheet :viewports '(1 2)))))))

(test a-drawing-keeps-its-sheets-in-order-and-a-change-counts-as-a-change
  (let ((drawing (dom:make-drawing)))
    (is (zerop (dom:sheet-count drawing)))
    (let ((revision (dom:drawing-revision drawing)))
      (is (= 0 (dom:add-sheet drawing (dom:make-sheet :name "A"))))
      (is (= 1 (dom:add-sheet drawing (dom:make-sheet :name "B"))))
      (is (< revision (dom:drawing-revision drawing))))
    (is (equal '("A" "B") (mapcar #'dom:sheet-name (dom:drawing-sheets drawing))))
    (dom:replace-sheet drawing 0 (dom:make-sheet :name "A2"))
    (is (equal '("A2" "B") (mapcar #'dom:sheet-name (dom:drawing-sheets drawing))))
    (dom:remove-sheet drawing 0)
    (is (equal '("B") (mapcar #'dom:sheet-name (dom:drawing-sheets drawing))))
    (is (search "no sheet 3" (refusal-of (lambda () (dom:remove-sheet drawing 2)))))
    (is (search "no sheet 5" (refusal-of (lambda () (dom:replace-sheet drawing 4 (dom:make-sheet))))))
    (is (null (dom:sheet-at drawing 3)))))

(test viewports-are-put-on-a-sheet-changed-and-taken-off
  (let ((drawing (dom:make-drawing)))
    (dom:add-sheet drawing (dom:make-sheet))
    (is (= 0 (dom:add-viewport drawing 0 (a-sheet-viewport))))
    (is (= 1 (dom:add-viewport drawing 0 (a-sheet-viewport :x 150d0))))
    (dom:replace-viewport drawing 0 1 (a-sheet-viewport :x 160d0))
    (is (equal '(20d0 160d0) (mapcar #'dom:viewport-x (dom:sheet-viewports (dom:sheet-at drawing 0)))))
    (dom:remove-viewport drawing 0 0)
    (is (equal '(160d0) (mapcar #'dom:viewport-x (dom:sheet-viewports (dom:sheet-at drawing 0)))))
    (is (search "no such viewport" (refusal-of (lambda () (dom:remove-viewport drawing 0 5)))))
    (is (search "no sheet 2" (refusal-of (lambda () (dom:add-viewport drawing 1 (a-sheet-viewport))))))))

(test a-drawing-has-at-most-so-many-sheets-and-a-sheet-so-many-viewports
  (let ((drawing (dom:make-drawing)))
    (is (search "at most" (refusal-of (lambda ()
                                        (dom:set-sheets drawing
                                                        (loop repeat (1+ dom:+maximum-sheets+)
                                                              collect (dom:make-sheet)))))))
    (is (search "at most" (refusal-of (lambda ()
                                        (dom:make-sheet :viewports (loop repeat (1+ dom:+maximum-viewports+)
                                                                         collect (a-sheet-viewport)))))))))

;;; Saved with the drawing

(defun drawing-with-sheets ()
  "Return a drawing with a line and two sheets."
  (let ((drawing (sheet-drawing (plot-line 1 0 0 1000 0))))
    (dom:add-sheet drawing (dom:make-sheet :name "Plan"
                                           :paper :a3
                                           :landscape t
                                           :fields '((:project . "House \"Alfa\"") (:title . "Ground floor")
                                                     (:author . "Ana") (:date . "2026-10-19") (:revision . "B"))
                                           :viewports (list (a-sheet-viewport)
                                                            (a-sheet-viewport :x 140d0 :scale 0.02d0 :center-x 0d0))))
    (dom:add-sheet drawing (dom:make-sheet :name "Detail" :paper :a4 :landscape nil))
    drawing))

(test the-sheets-are-written-in-the-drawing-file
  (let ((text (persist:drawing-to-string (drawing-with-sheets))))
    (is (search (format nil "(format-version ~d)" persist:+supported-format-version+) text))
    (is (search (format nil " (sheets~%  (sheet (name \"Plan\") (paper a3) (landscape t)~%") text))
    (is (search (concatenate 'string
                             "   (fields (project \"House \\\"Alfa\\\"\") (title \"Ground floor\") (author \"Ana\") "
                             "(date \"2026-10-19\") (revision \"B\"))")
                text))
    (is (search (concatenate 'string
                             "    (viewport (x 20.0) (y 40.0) (width 100.0) (height 50.0) "
                             "(center-x 500.0) (center-y 0.0) (scale 0.1))")
                text))
    (is (search "(sheet (name \"Detail\") (paper a4) (landscape nil)" text))))

(test the-sheets-come-back-as-they-were-and-the-text-is-stable
  (let* ((drawing (drawing-with-sheets))
         (text (persist:drawing-to-string drawing))
         (again (persist:parse-drawing-text text)))
    (is (= 2 (dom:sheet-count again)))
    (let ((plan (dom:sheet-at again 0)))
      (is (equal "Plan" (dom:sheet-name plan)))
      (is (eq :a3 (dom:sheet-paper plan)))
      (is (dom:sheet-landscape plan))
      (is (equal "House \"Alfa\"" (dom:sheet-field plan :project)))
      (is (equal "B" (dom:sheet-field plan :revision)))
      (is (equal '(20d0 140d0) (mapcar #'dom:viewport-x (dom:sheet-viewports plan))))
      (is (equal '(0.1d0 0.02d0) (mapcar #'dom:viewport-scale (dom:sheet-viewports plan)))))
    (is (not (dom:sheet-landscape (dom:sheet-at again 1))))
    (is (string= text (persist:drawing-to-string again)))))

(test a-drawing-without-sheets-writes-an-empty-section
  (let ((text (persist:drawing-to-string (sheet-drawing))))
    (is (search (format nil " (sheets~% )~%") text))
    (is (null (dom:drawing-sheets (persist:parse-drawing-text text))))))

(test a-sheet-that-is-wrong-is-said-where-it-is
  (flet ((problem (old new)
           (handler-case (progn (persist:parse-drawing-text
                                 (replace-once (persist:drawing-to-string (drawing-with-sheets)) old new))
                                nil)
             (base:drawing-file-content-error (condition)
               (princ-to-string condition)))))
    (is (search "unknown paper" (problem "(paper a3)" "(paper a9)")))
    (is (search "unknown field of the title block" (problem "(revision \"B\")" "(colour \"B\")")))
    (is (search "appears twice" (problem "(revision \"B\")" "(author \"B\")")))
    (is (search "expected t or nil" (problem "(landscape t)" "(landscape maybe)")))
    (is (search "out of range" (problem "(scale 0.1)" "(scale 0.0)")))
    (is (search "width and a height above zero" (problem "(width 100.0) (height 50.0) (center-x 500.0)"
                                                         "(width 0.0) (height 50.0) (center-x 500.0)")))
    (is (search "missing field" (problem " (center-y 0.0) (scale 0.1))" " (scale 0.1))")))
    (is (search "unknown field in a sheet" (problem "(landscape t)" "(orientation t)")))))

(test the-sheets-section-is-required-from-version-seven
  (let ((without (replace-once (persist:drawing-to-string (sheet-drawing))
                               (format nil " (sheets~% )~%")
                               "")))
    (handler-case (progn (persist:parse-drawing-text without)
                         (fail "a version 7 file without its sheets was accepted"))
      (base:drawing-file-content-error (condition)
        (is (search "expected the field (sheets" (princ-to-string condition)))))))

;;; The paper

(test a-sheet-is-as-large-as-its-paper-and-the-way-it-lies
  (flet ((size (&rest options)
           (multiple-value-list (plot:sheet-size (apply #'dom:make-sheet options)))))
    (is (equal '(420d0 297d0) (size :paper :a3 :landscape t)))
    (is (equal '(297d0 420d0) (size :paper :a3 :landscape nil)))
    (is (equal '(279.4d0 215.9d0) (size :paper :letter :landscape t)))))

(test the-frame-is-ten-millimetres-in-and-the-title-block-is-along-the-bottom-of-it
  (let* ((sheet (dom:make-sheet :paper :a3 :landscape t))
         (frame (plot:sheet-frame sheet))
         (body (plot:sheet-body sheet)))
    (is (approx= 10d0 (geo:bounds-min-x frame)))
    (is (approx= 410d0 (geo:bounds-max-x frame)))
    (is (approx= 287d0 (geo:bounds-max-y frame)))
    (is (approx= 37d0 (geo:bounds-min-y body)) "above the 27 millimetres of the title block")
    (is (approx= 287d0 (geo:bounds-max-y body)))))

(test the-title-block-has-eight-cells-that-say-what-the-sheet-says
  (let* ((sheet (dom:make-sheet :paper :a3
                                :fields '((:project . "House") (:title . "Plan") (:author . "Ana")
                                          (:date . "today") (:revision . "C"))
                                :viewports (list (a-sheet-viewport))))
         (cells (plot:title-cells sheet 1 3))
         (by-label (lambda (label) (sixth (find label cells :key #'fifth :test #'string=)))))
    (is (= 8 (length cells)))
    (is (equal '("Project" "Title" "Author" "Date" "Revision" "Scale" "Sheet" "Paper") (mapcar #'fifth cells)))
    (is (equal "House" (funcall by-label "Project")))
    (is (equal "Plan" (funcall by-label "Title")))
    (is (equal "C" (funcall by-label "Revision")))
    (is (equal "1:10" (funcall by-label "Scale")))
    (is (equal "2/3" (funcall by-label "Sheet")))
    (is (equal "A3" (funcall by-label "Paper")))
    (let ((frame (plot:sheet-frame sheet)))
      (dolist (cell cells)
        (destructuring-bind (x y width height &rest rest) cell
          (declare (ignore rest))
          (is (<= (geo:bounds-min-x frame) x))
          (is (<= (geo:bounds-min-y frame) y))
          (is (<= (+ x width) (+ (geo:bounds-max-x frame) 1d-9)))
          (is (<= (+ y height) (+ (geo:bounds-min-y frame) plot:+title-block-height+ 1d-9))
              "all of them are in the title block"))))))

(test the-scale-cell-says-the-scale-or-that-it-varies
  (flet ((scale-of (&rest viewports)
           (sixth (find "Scale" (plot:title-cells (dom:make-sheet :viewports viewports) 0 1)
                        :key #'fifth
                        :test #'string=))))
    (is (equal "-" (scale-of)))
    (is (equal "1:50" (scale-of (a-sheet-viewport :scale 0.02d0) (a-sheet-viewport :scale 0.02d0))))
    (is (equal "As shown" (scale-of (a-sheet-viewport :scale 0.02d0) (a-sheet-viewport :scale 0.1d0))))
    (is (equal "2:1" (scale-of (a-sheet-viewport :scale 2d0))))))

(test a-long-text-is-cut-to-fit-its-cell
  (is (equal "House" (plot:fitted-text "House" 50d0 3.5d0)))
  (is (equal "A very lon..." (plot:fitted-text "A very long name indeed" (* 13 3.5d0 0.6d0) 3.5d0)))
  (is (equal "ab" (plot:fitted-text "abcdef" (* 2 3.5d0 0.6d0) 3.5d0)) "too narrow for dots")
  (is (equal "" (plot:fitted-text "abc" 0d0 3.5d0))))

;;; Cut to a window

(defun stroke-of-points (&rest coordinates)
  "Return a stroke through points, in black.

COORDINATES: x and y, point after point."
  (plot:make-plot-stroke (loop for (x y) on coordinates by #'cddr collect (cons (float x 1d0) (float y 1d0)))
                         nil
                         (list 0d0 0d0 0d0)))

(defparameter *window* (geo:make-bounds 0d0 0d0 100d0 100d0)
  "The rectangle that strokes are cut to in the tests.")

(test a-stroke-inside-the-window-is-kept-and-one-outside-is-dropped
  (let ((inside (stroke-of-points 10 10 90 90))
        (outside (stroke-of-points 200 10 300 90)))
    (is (equal (list inside) (plot:clip-stroke inside *window*)))
    (is (null (plot:clip-stroke outside *window*)))))

(test a-stroke-that-leaves-the-window-is-cut-at-its-edge
  (let ((clipped (plot:clip-stroke (stroke-of-points 50 50 150 50) *window*)))
    (is (= 1 (length clipped)))
    (is (equal '((50d0 50d0) (100d0 50d0)) (point-list (first clipped))))))

(test a-stroke-that-comes-in-and-goes-out-again-is-in-pieces
  (let ((clipped (plot:clip-stroke (stroke-of-points -50 50 50 50 50 150 150 150 150 50 50 50 50 -50) *window*)))
    (is (= 2 (length clipped)))
    (is (equal '((0d0 50d0) (50d0 50d0) (50d0 100d0)) (point-list (first clipped))))
    (is (equal '((100d0 50d0) (50d0 50d0) (50d0 0d0)) (point-list (second clipped))) "after the stretch outside")))

(test a-closed-stroke-that-is-partly-outside-becomes-runs-and-one-that-is-inside-stays-closed
  (let ((inside (plot:make-plot-stroke (list (cons 10d0 10d0) (cons 90d0 10d0) (cons 50d0 90d0))
                                       t
                                       (list 0d0 0d0 0d0)))
        (across (plot:make-plot-stroke (list (cons 50d0 50d0) (cons 150d0 50d0) (cons 150d0 150d0) (cons 50d0 150d0))
                                       t
                                       (list 0d0 0d0 0d0))))
    (is (equal (list inside) (plot:clip-stroke inside *window*)))
    (let ((runs (plot:clip-stroke across *window*)))
      (is (plusp (length runs)))
      (is (notany #'plot:plot-stroke-closed runs))
      (dolist (run runs)
        (dolist (point (plot:plot-stroke-points run))
          (is (geo:bounds-contains-point-p *window* (car point) (cdr point))))))))

(test a-stroke-keeps-its-thickness-when-it-is-cut
  (let ((clipped (plot:clip-stroke (plot:make-plot-stroke (list (cons 50d0 50d0) (cons 150d0 50d0))
                                                          nil
                                                          (list 1d0 0d0 0d0)
                                                          0.7d0)
                                   *window*)))
    (is (= 0.7d0 (plot:plot-stroke-width (first clipped))))
    (is (equal '(1d0 0d0 0d0) (plot:plot-stroke-color (first clipped))))))

(defun fill-of-points (&rest coordinates)
  "Return a filled polygon, in black.

COORDINATES: x and y, point after point."
  (plot:make-plot-fill (loop for (x y) on coordinates by #'cddr collect (cons (float x 1d0) (float y 1d0)))
                       (list 0d0 0d0 0d0)))

(defun polygon-area (points)
  "Return the area of a polygon given as (x . y) conses.

POINTS: the vertices."
  (abs (/ (loop for (a b) on (append points (list (first points)))
                while b
                sum (- (* (car a) (cdr b)) (* (car b) (cdr a))))
          2)))

(test a-filled-polygon-is-cut-to-the-part-inside
  (let ((half (plot:clip-fill (fill-of-points 50 0 150 0 150 100 50 100) *window*)))
    (is (approx= 5000d0 (polygon-area (plot:plot-fill-points half))) "half of a 100 by 100 square")
    (dolist (point (plot:plot-fill-points half))
      (is (geo:bounds-contains-point-p *window* (car point) (cdr point)))))
  (is (null (plot:clip-fill (fill-of-points 200 0 300 0 300 100) *window*)))
  (let ((triangle (fill-of-points 10 10 90 10 50 90)))
    (is (approx= (polygon-area (plot:plot-fill-points triangle))
                 (polygon-area (plot:plot-fill-points (plot:clip-fill triangle *window*)))))))

(test a-text-is-kept-when-it-starts-in-the-window
  (let ((inside (plot:make-plot-text "in" 10d0 10d0 3d0 0d0 (list 0d0 0d0 0d0)))
        (outside (plot:make-plot-text "out" 110d0 10d0 3d0 0d0 (list 0d0 0d0 0d0))))
    (is (equal (list inside) (plot:clip-item inside *window*)))
    (is (null (plot:clip-item outside *window*)))))

;;; A viewport at its scale

(test a-sheet-viewport-draws-what-is-in-its-window-at-its-scale
  (let* ((drawing (sheet-drawing (plot-line 1 0 0 1000 0) (plot-line 2 0 5000 1000 5000)))
         (items (plot:viewport-items drawing
                                     (plot:visible-entities drawing)
                                     (a-sheet-viewport)
                                     (plot:make-plot-settings))))
    (let ((strokes (remove-if-not (lambda (item) (typep item 'plot:plot-stroke)) items)))
      (is (= 2 (length strokes)) "the line in the window and the border; the other line is far outside")
      (is (equal '((20d0 65d0) (120d0 65d0)) (point-list (first strokes)))
          "1000 mm at a tenth is 100 mm of paper, 20 from the edge of the sheet and 65 up, the middle of the viewport")
      (is (equal '((20d0 40d0) (120d0 40d0) (120d0 90d0) (20d0 90d0)) (point-list (second strokes)))
          "the border is the rectangle of the viewport"))))

(test what-is-outside-a-sheet-viewport-is-cut-off-at-its-border
  (let* ((drawing (sheet-drawing (plot-line 1 -2000 0 3000 0)))
         (items (plot:viewport-items drawing
                                     (plot:visible-entities drawing)
                                     (a-sheet-viewport)
                                     (plot:make-plot-settings)))
         (line (first (remove-if-not (lambda (item) (typep item 'plot:plot-stroke)) items))))
    (is (equal '((20d0 65d0) (120d0 65d0)) (point-list line)) "the line goes beyond the window on both sides")))

(test two-viewports-show-the-same-drawing-at-two-scales
  (let* ((drawing (sheet-drawing (dom:make-circle :id 1 :layer-id 0 :center-x 500d0 :center-y 0d0 :radius 100d0)))
         (settings (plot:make-plot-settings))
         (coarse (plot:viewport-items drawing
                                      (plot:visible-entities drawing)
                                      (a-sheet-viewport :scale 0.05d0)
                                      settings))
         (fine (plot:viewport-items drawing (plot:visible-entities drawing) (a-sheet-viewport :scale 0.2d0) settings)))
    (flet ((width-of (items)
             (let ((xs (mapcar #'car (plot:plot-stroke-points (first items)))))
               (- (reduce #'max xs) (reduce #'min xs)))))
      (is (approx= 10d0 (width-of coarse) 1d-2) "a circle of 200 mm across, at 1:20")
      (is (approx= 40d0 (width-of fine) 1d-2) "and at 1:5"))))

(test a-hidden-layer-is-not-in-a-sheet-viewport
  (let ((drawing (sheet-drawing (plot-line 1 0 0 1000 0))))
    (dom:set-layer-visible drawing 0 nil)
    (let ((items (plot:viewport-items drawing
                                      (plot:visible-entities drawing)
                                      (a-sheet-viewport)
                                      (plot:make-plot-settings))))
      (is (= 1 (length items)) "only the border"))))

;;; A sheet and all of them

(defun planned (sheet &optional (drawing (sheet-drawing (plot-line 1 0 0 1000 0))))
  "Return the plot plan of a sheet that is the first of one.

SHEET: the sheet.
DRAWING: the drawing it is of."
  (plot:plan-sheet drawing
                   sheet
                   0
                   1
                   (plot:make-plot-settings)))

(test a-sheet-is-planned-with-its-viewports-its-frame-and-its-title-block
  (let ((plan (planned (dom:make-sheet :paper :a3
                                       :fields '((:project . "House"))
                                       :viewports (list (a-sheet-viewport))))))
    (is (= 420d0 (plot:plot-plan-width plan)))
    (is (= 297d0 (plot:plot-plan-height plan)))
    (is (= 1 (plot:plot-plan-entity-count plan)))
    (let ((strokes (stroke-items plan)))
      (is (= (+ 1 1 1 8) (length strokes)) "the line, the border, the frame and one for each cell"))
    (is (find "House" (text-items plan) :key #'plot:plot-text-string :test #'string=))
    (is (find "Project" (text-items plan) :key #'plot:plot-text-string :test #'string=))
    (is (find "1/1" (text-items plan) :key #'plot:plot-text-string :test #'string=))
    (is (find 0.7d0 (stroke-items plan) :key #'plot:plot-stroke-width) "the frame is thicker")))

(test a-sheet-with-no-viewport-is-just-the-frame-and-the-title-block
  (let ((plan (planned (dom:make-sheet))))
    (is (= 0 (plot:plot-plan-entity-count plan)))
    (is (= (+ 1 8) (length (stroke-items plan))))))

(test the-drawing-has-to-have-sheets-to-be-planned-as-sheets
  (handler-case (progn (plot:plan-sheets (sheet-drawing (plot-line 1 0 0 1 1)) (plot:make-plot-settings))
                       (fail "a drawing without sheets was planned"))
    (base:export-refused (condition)
      (is (search "no sheets" (princ-to-string condition))))))

(test every-sheet-is-planned-with-its-place-among-them
  (let ((drawing (drawing-with-sheets)))
    (let ((plans (plot:plan-sheets drawing (plot:make-plot-settings))))
      (is (= 2 (length plans)))
      (is (find "1/2" (text-items (first plans)) :key #'plot:plot-text-string :test #'string=))
      (is (find "2/2" (text-items (second plans)) :key #'plot:plot-text-string :test #'string=))
      (is (= 420d0 (plot:plot-plan-width (first plans))))
      (is (= 210d0 (plot:plot-plan-width (second plans))) "the second is A4 standing up")
      (is (= 297d0 (plot:plot-plan-height (second plans)))))))

;;; The PDF

(test a-pdf-of-the-sheets-has-a-page-for-each-and-says-so
  (multiple-value-bind (octets plans replaced) (plot:render-sheets-pdf (drawing-with-sheets) (plot:make-plot-settings))
    (let ((text (octets-text octets)))
      (is (= 2 (length plans)))
      (is (zerop replaced))
      (is (eql 0 (search "%PDF-1.4" text)))
      (is (search "/Count 2" text))
      (is (search "/Kids [5 0 R 7 0 R]" text))
      (is (search "/MediaBox [0 0 1190.55 841.89]" text) "the first, A3 lying down")
      (is (search "/MediaBox [0 0 595.28 841.89]" text) "the second, A4 standing")
      (is (search "(Ground floor) Tj" text))
      (is (search "(2/2) Tj" text))
      (is (search "1.984 w" text) "the frame, 0.7 millimetres, in points")
      (let ((offsets (xref-offsets text)))
        (is (= (+ 4 (* 2 2)) (length offsets)))
        (loop for offset in offsets
              for number from 1
              do (is (eql offset (search (format nil "~d 0 obj" number) text :start2 offset))
                     "object ~d is where the table says" number)))
      (let ((start (parse-integer text :start (+ (search "startxref" text :from-end t) 10) :junk-allowed t)))
        (is (eql start (search (format nil "xref~%0 9") text :start2 start)))))))

(test the-length-of-each-page-is-the-length-of-its-content
  (let* ((text (octets-text (plot:render-sheets-pdf (drawing-with-sheets) (plot:make-plot-settings))))
         (start 0)
         (checked 0))
    (loop for found = (search "<< /Length " text :start2 start)
          while found
          do (let* ((length-end (position #\Space text :start (+ found 11)))
                    (declared (parse-integer text :start (+ found 11) :end length-end))
                    (stream-start (+ (search (format nil "stream~%") text :start2 found) 7))
                    (stream-end (search (format nil "endstream") text :start2 stream-start)))
               (is (= declared (- stream-end stream-start)))
               (incf checked)
               (setf start stream-end)))
    (is (= 2 checked))))

(test the-colors-of-the-layers-are-used-when-the-plot-is-not-monochrome
  (let ((drawing (drawing-with-sheets)))
    (let ((layer (dom:add-layer drawing "red" (dom:make-rgb 200 0 0))))
      (dom:apply-changes drawing
                         (dom:create-entities-changes
                          (list (dom:make-line :id 5 :layer-id (dom:layer-id layer) :x1 0d0 :y1 100d0 :x2 900d0
                                               :y2 100d0)))))
    (let ((colored (octets-text (plot:render-sheets-pdf drawing (plot:make-plot-settings :monochrome nil))))
          (black (octets-text (plot:render-sheets-pdf drawing (plot:make-plot-settings)))))
      (is (search "0.784 0 0 RG" colored))
      (is (not (search "0.784 0 0 RG" black))))))

(test a-drawing-without-sheets-has-nothing-to-write-as-sheets
  (handler-case (progn (plot:render-sheets-pdf (sheet-drawing) (plot:make-plot-settings))
                       (fail "no sheets and still a file"))
    (base:export-refused ()
      (pass))))

(test the-pdf-of-the-drawing-as-one-page-is-as-it-was
  (multiple-value-bind (octets plan) (plot:render-pdf (sheet-drawing (plot-line 1 0 0 2000 0))
                                                      (plot:make-plot-settings))
    (declare (ignore plan))
    (let ((text (octets-text octets)))
      (is (search "/Count 1" text))
      (is (search "/Kids [5 0 R]" text))
      (is (= 6 (length (xref-offsets text)))))))

;;; In the language

(defun sheets-section (drawing)
  "Return the part of the file of a drawing that holds its sheets.

DRAWING: the drawing."
  (let* ((text (persist:drawing-to-string drawing))
         (start (search " (sheets" text))
         (end (search " (entities" text)))
    (subseq text start end)))

(test the-console-adds-sheets-and-viewports
  (let* ((drawing (sheet-drawing (plot-line 1 0 0 4000 0) (plot-line 2 0 0 0 2000)))
         (context (script-env drawing)))
    (is (equal '("1") (ran-ok context (concatenate 'string
                                                   "(sheet \"Plan\" :paper :a3 :project \"House\" "
                                                   ":title \"Ground floor\" :author \"Ana\")"))))
    (is (equal '("2") (ran-ok context "(sheet \"Detail\" :paper :a4 :landscape nil)")))
    (is (equal '("((1 \"Plan\" :A3 T 0) (2 \"Detail\" :A4 NIL 0))") (ran-ok context "(sheets)")))
    (is (equal "Ground floor" (dom:sheet-field (dom:sheet-at drawing 0) :title)))
    (is (equal '("1") (ran-ok context "(viewport 1 :scale \"1:50\" :center-x 2000 :center-y 1000)")))
    (is (equal '("2") (ran-ok context "(viewport 1 :x 20 :y 50 :width 80 :height 60 :scale 1/20)")))
    (let ((viewports (dom:sheet-viewports (dom:sheet-at drawing 0))))
      (is (approx= 0.02d0 (dom:viewport-scale (first viewports))))
      (is (approx= 2000d0 (dom:viewport-center-x (first viewports))))
      (is (approx= 0.05d0 (dom:viewport-scale (second viewports))) "a ratio such as 1/20 is a scale too")
      (is (approx= 80d0 (dom:viewport-width (second viewports)))))))

(test a-sheet-viewport-that-is-not-told-more-fills-the-sheet-and-shows-all-of-the-drawing
  (let* ((drawing (sheet-drawing (plot-line 1 0 0 4000 0) (plot-line 2 0 0 0 2000)))
         (context (script-env drawing)))
    (ran-ok context "(sheet \"Plan\" :paper :a3)")
    (ran-ok context "(viewport 1)")
    (let* ((viewport (first (dom:sheet-viewports (dom:sheet-at drawing 0))))
           (body (plot:sheet-body (dom:sheet-at drawing 0))))
      (is (approx= 2000d0 (dom:viewport-center-x viewport)))
      (is (approx= 1000d0 (dom:viewport-center-y viewport)))
      (is (approx= (- (geo:bounds-width body) 10d0) (dom:viewport-width viewport)) "five millimetres in on each side")
      (let ((window (dom:viewport-window viewport)))
        (is (<= (geo:bounds-min-x window) 0d0))
        (is (>= (geo:bounds-max-x window) 4000d0))
        (is (<= (geo:bounds-min-y window) 0d0))
        (is (>= (geo:bounds-max-y window) 2000d0)))
      (is (approx= (/ 390d0 4000d0) (dom:viewport-scale viewport)) "the scale at which it just fits across"))))

(test sheets-can-be-edited-and-taken-away-by-their-numbers
  (let* ((drawing (sheet-drawing (plot-line 1 0 0 100 0)))
         (context (script-env drawing)))
    (ran-ok context "(sheet \"One\") (sheet \"Two\") (sheet \"Three\")")
    (ran-ok context "(edit-sheet 2 :name \"Second\" :paper :a1 :landscape nil :revision \"C\")")
    (let ((second (dom:sheet-at drawing 1)))
      (is (equal "Second" (dom:sheet-name second)))
      (is (eq :a1 (dom:sheet-paper second)))
      (is (not (dom:sheet-landscape second)))
      (is (equal "C" (dom:sheet-field second :revision))))
    (ran-ok context "(edit-sheet 2 :title \"Roof\")")
    (is (not (dom:sheet-landscape (dom:sheet-at drawing 1))) "what is not named stays")
    (ran-ok context "(remove-sheet 1)")
    (is (equal '("Second" "Three") (mapcar #'dom:sheet-name (dom:drawing-sheets drawing))))))

(test a-command-about-sheets-says-what-is-wrong
  (let ((context (script-env (sheet-drawing (plot-line 1 0 0 100 0)))))
    (is (search "no sheet 1" (failure-of context "(viewport 1)")))
    (ran-ok context "(sheet \"One\")")
    (is (search "no sheet 3" (failure-of context "(edit-sheet 3 :name \"x\")")))
    (is (search "no sheet 0" (failure-of context "(remove-sheet 0)")))
    (is (search "not a paper size" (failure-of context "(sheet \"A\" :paper :a9)")))
    (is (search "needs a name" (failure-of context "(sheet \"\")")))
    (is (search "scale is above zero" (failure-of context "(viewport 1 :scale 0)")))
    (is (search "type a scale" (failure-of context "(viewport 1 :scale \"big\")")))
    (is (search "number or text" (failure-of context "(viewport 1 :scale :big)")))
    (is (search "width and a height above zero" (failure-of context "(viewport 1 :width 0)")))))

(test the-script-of-a-drawing-makes-its-sheets-again
  (let* ((drawing (drawing-with-sheets))
         (script (script:drawing-script drawing)))
    (is (search "(sheet \"Plan\" :paper :a3 :landscape t :project \"House \\\"Alfa\\\"\"" script))
    (is (search "(viewport 1 :x 20 :y 40 :width 100 :height 50 :center-x 500 :center-y 0 :scale 0.1)" script))
    (is (search "(sheet \"Detail\" :paper :a4 :landscape nil)" script))
    (let ((again (dom:make-drawing)))
      (ran-ok (script-env again) script)
      (is (string= (sheets-section drawing) (sheets-section again))))))

(test the-help-lists-the-sheet-commands
  (let ((overview (script:script-result-output (run-script (script-env) "(help)"))))
    (is (search "Sheets" overview))
    (is (search "viewport" overview))))
