(in-package #:borba-cad/tests)

(def-suite quantities
  :description "The reports of quantities: totals, blocks and the cut list, shown as a table and written as CSV."
  :in all)

(in-suite quantities)

;;; Helpers

(defun steel-drawing ()
  "Return a drawing of a layer called steel with four bars of 1000 mm, two of 500 mm and a frame, and a layer called
glass with a pane."
  (let* ((drawing (dom:make-drawing))
         (steel (dom:add-layer drawing "steel" (dom:make-rgb 200 200 200)))
         (glass (dom:add-layer drawing "glass" (dom:make-rgb 100 200 250)))
         (entities '()))
    (flet ((bar (id y length)
             (push (dom:make-line :id id :layer-id (dom:layer-id steel) :x1 0d0 :y1 (float y 1d0)
                                  :x2 (float length 1d0) :y2 (float y 1d0))
                   entities)))
      (dotimes (index 4)
        (bar (1+ index) (* 100 index) 1000))
      (bar 5 500 500)
      (bar 6 600 500)
      (push (dom:make-polyline :id 7 :layer-id (dom:layer-id steel)
                               :coordinates (path-coordinates 0 0 100 0 100 50 0 50)
                               :closed t)
            entities)
      (push (dom:make-rectangle :id 8 :layer-id (dom:layer-id glass) :min-x 0d0 :min-y 0d0 :width 400d0 :height 300d0)
            entities))
    (dom:apply-changes drawing (dom:create-entities-changes (nreverse entities)))
    drawing))

(defun report-row (report label)
  "Return the row of REPORT whose first value is LABEL.

REPORT: a quantity report.
LABEL: the name in the first column."
  (find label (dom:quantity-report-rows report) :key #'first :test #'equal))

(defun door-drawing ()
  "Return a drawing with a block called door, of a line 10 long and one 20 long, placed three times: at the size it was
drawn, twice that size, and at that size again on another layer."
  (let* ((drawing (dom:make-drawing))
         (other (dom:add-layer drawing "other" (dom:make-rgb 1 2 3)))
         (definition (dom:make-block-definition :name "door"
                                                :entities (list (dom:make-line :id 1 :layer-id 0 :x1 0d0 :y1 0d0
                                                                               :x2 10d0 :y2 0d0)
                                                                (dom:make-line :id 2 :layer-id 0 :x1 0d0 :y1 0d0
                                                                               :x2 20d0 :y2 0d0)))))
    (dom:define-block drawing definition)
    (dom:apply-changes drawing
                       (dom:create-entities-changes
                        (list (dom:make-insert :id 1 :layer-id 0 :block definition :x 0d0 :y 0d0)
                              (dom:make-insert :id 2 :layer-id 0 :block definition :x 100d0 :y 0d0 :scale 2d0)
                              (dom:make-insert :id 3 :layer-id (dom:layer-id other) :block definition :x 200d0
                                                                                    :y 0d0))))
    drawing))

;;; What an entity measures

(test every-shape-has-a-length-and-the-closed-ones-an-area
  (is (approx= 100d0 (dom:entity-length (cut-line 1 0 0 60 80))))
  (is (approx= 0d0 (dom:entity-area (cut-line 1 0 0 60 80))))
  (is (approx= 200d0 (dom:entity-length (spread-rectangle 1 0 0 60 40))))
  (is (approx= 2400d0 (dom:entity-area (spread-rectangle 1 0 0 60 40))))
  (is (approx= (* 2 pi 10) (dom:entity-length (cut-circle 1 0 0 10))))
  (is (approx= (* pi 100) (dom:entity-area (cut-circle 1 0 0 10))))
  (is (approx= (* 5 pi) (dom:entity-length (cut-arc 1 0 0 10 0 (/ pi 2)))))
  (let ((open (dom:make-polyline :id 1 :layer-id 0 :coordinates (path-coordinates 0 0 30 0 30 40) :closed nil))
        (closed (dom:make-polyline :id 1 :layer-id 0 :coordinates (path-coordinates 0 0 30 0 30 40) :closed t)))
    (is (approx= 70d0 (dom:entity-length open)))
    (is (approx= 0d0 (dom:entity-area open)) "an open polyline encloses nothing")
    (is (approx= 120d0 (dom:entity-length closed)))
    (is (approx= 600d0 (dom:entity-area closed)))
    (is (approx= 600d0 (dom:entity-area (dom:make-polyline :id 1
                                                           :layer-id 0
                                                           :coordinates (path-coordinates 0 0 0 40 30 0)
                                                           :closed t)))
        "the area does not depend on which way round the shape goes"))
  (is (approx= 0d0 (dom:entity-length (dom:make-text :id 1 :layer-id 0 :x 0d0 :y 0d0 :string "a" :height 10d0)))))

;;; The reports

(test the-totals-by-layer-count-length-and-area-in-the-order-of-the-layers
  (let ((report (dom:quantity-report (steel-drawing) :layer)))
    (is (equal '("Layer" "Count" "Length" "Area") (mapcar #'car (dom:quantity-report-columns report))))
    (is (equal '(:text :count :length :area) (mapcar #'cdr (dom:quantity-report-columns report))))
    (is (equal '("steel" "glass") (mapcar #'first (dom:quantity-report-rows report))) "layer 0 holds nothing")
    (let ((steel (report-row report "steel")))
      (is (= 7 (second steel)))
      (is (approx= (+ 4000 1000 300) (third steel)))
      (is (approx= 5000d0 (fourth steel)) "the frame, 100 by 50"))
    (let ((glass (report-row report "glass")))
      (is (= 1 (second glass)))
      (is (approx= 1400d0 (third glass)))
      (is (approx= 120000d0 (fourth glass))))
    (is (equal '("Total" 8) (subseq (dom:quantity-report-totals report) 0 2)))
    (is (approx= 6700d0 (third (dom:quantity-report-totals report))))
    (is (approx= 125000d0 (fourth (dom:quantity-report-totals report))))))

(test the-totals-by-kind-use-the-names-of-the-kinds
  (let ((report (dom:quantity-report (steel-drawing) :kind)))
    (is (equal '("Line" "Rectangle" "Polyline") (mapcar #'first (dom:quantity-report-rows report))))
    (is (= 6 (second (report-row report "Line"))))
    (is (approx= 5000d0 (third (report-row report "Line"))))))

(test a-hatch-is-counted-by-kind-but-its-area-is-not-added-twice
  (let ((drawing (steel-drawing)))
    (dom:apply-changes drawing
                       (dom:create-entities-changes
                        (list (dom:make-hatch :id 20
                                              :layer-id (dom:layer-id (dom:find-layer-by-name drawing "glass"))
                                              :coordinates (path-coordinates 0 0 400 0 400 300 0 300)
                                              :spacing 10d0
                                              :angle 0d0))))
    (is (approx= 120000d0 (fourth (report-row (dom:quantity-report drawing :kind) "Hatch"))))
    (is (approx= 120000d0 (fourth (report-row (dom:quantity-report drawing :layer) "glass")))
        "by layer the hatch adds a count but not its area")
    (is (= 2 (second (report-row (dom:quantity-report drawing :layer) "glass"))))))

(test what-is-in-a-block-is-counted-as-placed-and-the-blocks-are-listed
  (let ((drawing (door-drawing)))
    (let ((layers (dom:quantity-report drawing :layer)))
      (is (= 4 (second (report-row layers "0"))) "two entities in each of two placements")
      (is (approx= (+ 30 60) (third (report-row layers "0"))) "the larger placement has lines twice as long")
      (is (= 2 (second (report-row layers "other")))))
    (let ((blocks (dom:quantity-report drawing :block)))
      (is (equal '("door") (mapcar #'first (dom:quantity-report-rows blocks))))
      (is (= 3 (second (report-row blocks "door"))) "placed three times")
      (is (approx= (+ 30 60 30) (third (report-row blocks "door")))))
    (is (equal '("Line") (mapcar #'first (dom:quantity-report-rows (dom:quantity-report drawing :kind))))
        "a block placed is not a kind of its own: its lines are counted")
    (is (= 6 (second (report-row (dom:quantity-report drawing :kind) "Line"))))))

(test the-cut-list-groups-the-bars-of-a-length-on-a-layer
  (let ((report (dom:quantity-report (steel-drawing) :cut)))
    (is (equal '("Layer" "Bar length" "Quantity" "Total length") (mapcar #'car (dom:quantity-report-columns report))))
    (is (equal '(("steel" 1000d0 4 4000d0)
                 ("steel" 500d0 2 1000d0)
                 ("steel" 300d0 1 300d0)
                 ("glass" 1400d0 1 1400d0))
               (dom:quantity-report-rows report))
        "the layers in order, the longest bar first; the frame is one bar, as long as its perimeter")
    (is (equal '("Total" nil 8 6700d0) (dom:quantity-report-totals report)))))

(test bars-that-differ-by-less-than-a-tenth-of-a-millimetre-are-of-the-same-length
  (let ((drawing (dom:make-drawing)))
    (dom:apply-changes drawing
                       (dom:create-entities-changes
                        (list (cut-line 1 0 0 1000 0)
                              (cut-line 2 0 10 1000.04d0 10)
                              (cut-line 3 0 20 1000.5d0 20))))
    (let ((rows (dom:quantity-report-rows (dom:quantity-report drawing :cut))))
      (is (= 2 (length rows)))
      (is (= 2 (third (second rows))) "1000 and 1000.04"))))

(test hidden-layers-are-not-counted-unless-their-entities-are-asked-for
  (let ((drawing (steel-drawing)))
    (dom:set-layer-visible drawing (dom:layer-id (dom:find-layer-by-name drawing "glass")) nil)
    (is (equal '("steel") (mapcar #'first (dom:quantity-report-rows (dom:quantity-report drawing :layer)))))
    (is (= 7 (dom:counted-entity-count drawing nil)))
    (is (equal '("glass") (mapcar #'first (dom:quantity-report-rows (dom:quantity-report drawing :layer :ids '(8))))))))

(test the-selection-is-what-is-counted-when-ids-are-given
  (let ((report (dom:quantity-report (steel-drawing) :cut :ids '(1 2 5))))
    (is (equal '(("steel" 1000d0 2 2000d0) ("steel" 500d0 1 500d0)) (dom:quantity-report-rows report)))))

(test a-report-that-has-nothing-to-show-says-so
  (flet ((refusal (drawing kind &rest options)
           (handler-case (apply #'dom:quantity-report drawing kind options)
             (base:edit-refused (condition) (princ-to-string condition)))))
    (is (search "nothing to count" (refusal (dom:make-drawing) :layer)))
    (is (search "no blocks" (refusal (steel-drawing) :block)))
    (is (search "no bars"
                (refusal (let ((drawing (dom:make-drawing)))
                           (dom:apply-changes drawing
                                              (dom:create-entities-changes
                                               (list (dom:make-text :id 1 :layer-id 0 :x 0d0 :y 0d0 :string "a"
                                                                    :height 10d0))))
                           drawing)
                         :cut)))))

;;; CSV

(test a-report-is-written-as-csv-with-crlf-and-the-unit-in-the-headings
  (let ((text (dom:quantity-report-csv (dom:quantity-report (steel-drawing) :cut) :millimeter)))
    (is (equal (format nil "Layer,Bar length (mm),Quantity,Total length (mm)~c~csteel,1000.0,4,4000.0~c~c"
                       #\Return #\Newline #\Return #\Newline)
               (subseq text 0 (1+ (position #\Newline text :start (1+ (position #\Newline text)))))))
    (is (search (format nil "Total,,8,6700.0~c~c" #\Return #\Newline) text))
    (is (char= #\Newline (char text (1- (length text)))))))

(test lengths-and-areas-are-written-in-the-display-unit
  (let ((text (dom:quantity-report-csv (dom:quantity-report (steel-drawing) :layer) :meter)))
    (is (search "Length (m)" text))
    (is (search (format nil "Area (m~c)" (code-char 178)) text))
    (is (search "steel,7,5.300,0.005" text) "5300 mm and 5000 square millimetres, in metres")))

(test the-excel-format-has-a-semicolon-and-a-decimal-comma
  (let ((text (dom:quantity-report-csv (dom:quantity-report (steel-drawing) :cut)
                                       :millimeter
                                       :separator #\;
                                       :decimal-mark #\,)))
    (is (search "steel;1000,0;4;4000,0" text))
    (is (not (search "1000.0" text)))))

(test a-field-with-a-separator-a-quote-or-a-line-break-is-quoted
  (is (equal "plain" (dom:csv-field "plain" #\,)))
  (is (equal "\"a,b\"" (dom:csv-field "a,b" #\,)))
  (is (equal "a;b" (dom:csv-field "a;b" #\,)))
  (is (equal "\"say \"\"hi\"\"\"" (dom:csv-field "say \"hi\"" #\,)))
  (is (equal (format nil "\"two~clines\"" #\Newline) (dom:csv-field (format nil "two~clines" #\Newline) #\,))))

;;; The form and the table

(defun quantity-app (&rest entities)
  "Return an application with a drawing of ENTITIES (those of the steel drawing when none are given)."
  (let ((application (make-test-app :drawing (if entities
                                                 (let ((drawing (dom:make-drawing)))
                                                   (dom:apply-changes drawing (dom:create-entities-changes entities))
                                                   drawing)
                                                 (steel-drawing)))))
    application))

(defun open-quantities (application)
  "Open the quantities form with the mouse, through the Measure menu."
  (click-widget application :measure)
  (click-widget application '(:menu-item :quantities)))

(defun label-of (application id)
  "Return the text of the widget ID, or NIL when it is not there."
  (let ((widget (find-widget application id)))
    (and widget (ui:widget-label widget))))

(test the-measure-menu-offers-the-quantities
  (let ((application (quantity-app)))
    (click-widget application :measure)
    (is (equal '((:menu-item :measure-distance) (:menu-item :measure-angle) (:menu-item :measure-area)
                 (:menu-item :quantities))
               (mapcar #'ui:widget-id (menu-rows application))))
    (let ((item (find-widget application '(:menu-item :quantities))))
      (is (equal "Quantities..." (ui:widget-label item)))
      (is (equal "Shift+L" (ui:widget-shortcut item)))
      (is (eq :quantities (ui:widget-icon item))))
    (press application :escape)
    (press application :l :shift)
    (is (eq :form (app:prompt-kind (app:app-prompt application))) "the shortcut opens it as well")))

(test the-form-asks-which-report-for-what-and-in-which-csv-format
  (let ((application (quantity-app)))
    (open-quantities application)
    (let ((prompt (app:app-prompt application)))
      (is (equal '(:report :scope :format) (mapcar #'app:form-field-key (app:prompt-fields prompt))))
      (is (equal '((:report . :layer) (:scope . :all) (:format . :standard)) (app:form-values prompt))
          "nothing is selected, and English writes with a comma"))
    (is (search "Counts 8 entities" (label-of application :prompt-info)))
    (press application :down)
    (press application :space)
    (is (search "Only the selection" (label-of application '(:prompt-field 1))))
    (is (search "Nothing is selected" (label-of application :prompt-info)))
    (press application :enter)
    (is (eq :form (app:prompt-kind (app:app-prompt application))) "refused, and still open")
    (is (search "Nothing is selected" (app:prompt-problem (app:app-prompt application))))))

(test the-selection-is-the-default-scope-when-there-is-one
  (let ((application (quantity-app)))
    (app:select-ids application '(1 2 3))
    (open-quantities application)
    (is (eq :selection (cdr (assoc :scope (app:form-values (app:app-prompt application))))))
    (is (search "Counts 3 entities" (label-of application :prompt-info)))
    (press application :enter)
    (is (eq :report (app:prompt-kind (app:app-prompt application))))
    (is (equal '("steel") (mapcar #'first (dom:quantity-report-rows (app:prompt-report (app:app-prompt application))))))
    (is (search "The selection" (app:prompt-message (app:app-prompt application))))))

(test the-report-is-shown-as-a-table-with-its-headings-rows-and-totals
  (let ((application (quantity-app)))
    (open-quantities application)
    (press application :enter)
    (let ((prompt (app:app-prompt application)))
      (is (eq :report (app:prompt-kind prompt)))
      (is (equal "Quantities: totals by layer" (app:prompt-title prompt)))
      (is (equal '("Export CSV..." "Close") (prompt-labels application))))
    (is (equal '("Layer" "Count" "Length (mm)" "Area (mm2)")
               (loop for index below 4
                     collect (substitute #\2 (code-char 178) (label-of application (list :report-heading index))))))
    (is (equal '("steel" "7" "5300.0" "5000.0")
               (loop for column below 4
                     collect (label-of application (list (list :report-row 0) column)))))
    (is (equal "Total" (label-of application '(:report-total 0))))
    (is (equal "8" (label-of application '(:report-total 1))))))

(test the-cut-list-is-a-report-too-and-says-what-a-bar-is
  (let ((application (quantity-app)))
    (open-quantities application)
    (press application :space)
    (press application :space)
    (press application :space)
    (is (search "Cut list" (label-of application '(:prompt-field 0))))
    (press application :enter)
    (is (equal "Quantities: cut list" (app:prompt-title (app:app-prompt application))))
    (is (search "is one bar" (app:prompt-message (app:app-prompt application))))))

(test a-long-table-scrolls-with-the-wheel-and-the-keys
  (let* ((entities (loop for index from 1 to 40
                         collect (cut-line index 0 (* index 10) (+ 100 (* index 25)) (* index 10))))
         (application (apply #'quantity-app entities)))
    (open-quantities application)
    (press application :space)
    (press application :space)
    (press application :space)
    (press application :enter)
    (let ((prompt (app:app-prompt application)))
      (is (= 40 (length (dom:quantity-report-rows (app:prompt-report prompt)))))
      (is (search "Rows 1 to 14 of 40" (label-of application :report-position)))
      (send application (app:make-wheel-event -1d0 600d0 300d0))
      (is (= 1 (app:prompt-scroll prompt)) "the wheel goes down a row")
      (press application :page-down)
      (is (= 15 (app:prompt-scroll prompt)))
      (press application :end)
      (is (= 26 (app:prompt-scroll prompt)) "to the last row")
      (is (search "Rows 27 to 40 of 40" (label-of application :report-position)))
      (press application :down)
      (is (= 26 (app:prompt-scroll prompt)) "not beyond the end")
      (press application :home)
      (is (= 0 (app:prompt-scroll prompt)))
      (press application :up)
      (is (= 0 (app:prompt-scroll prompt)))
      (is (find-widget application '((:report-row 13) 0)))
      (is (null (find-widget application '((:report-row 14) 0))) "only the rows in view are drawn"))))

(test a-small-window-shows-fewer-rows-and-keeps-them-all-reachable
  (let* ((entities (loop for index from 1 to 40
                         collect (cut-line index 0 (* index 10) (+ 100 (* index 25)) (* index 10))))
         (application (make-test-app :drawing (let ((drawing (dom:make-drawing)))
                                                (dom:apply-changes drawing (dom:create-entities-changes entities))
                                                drawing)
                                     :width 900d0
                                     :height 460d0)))
    (open-quantities application)
    (press application :space)
    (press application :space)
    (press application :space)
    (press application :enter)
    (let ((visible (loop for index from 0
                         while (find-widget application (list (list :report-row index) 0))
                         finally (return index))))
      (is (< 2 visible 14) "fewer than the usual, and a few")
      (press application :end)
      (is (find-widget application (list (list :report-row (1- visible)) 0)))
      (is (search "of 40" (label-of application :report-position))))
    (let ((card (find-widget application :prompt)))
      (is (<= 0 (ui:widget-y card)))
      (is (<= (+ (ui:widget-y card) (ui:widget-height card)) 460)))))

(test escape-closes-the-report-and-enter-offers-the-file
  (let ((application (quantity-app)))
    (open-quantities application)
    (press application :enter)
    (press application :escape)
    (is (null (app:app-prompt application)))
    (open-quantities application)
    (press application :enter)
    (press application :enter)
    (is (null (app:app-prompt application)))
    (is (eq :quantities (app:file-dialog-purpose (dialog-of application))))
    (is (equal "Export the quantities as a CSV file" (dialog-title-text application)))
    (is (equal "Export" (ui:widget-label (accept-button application))))))

(defun substitute-bom (text)
  "Return TEXT, read as Latin-1, without the three characters that a UTF-8 byte order mark is read as.

TEXT: the text of a file."
  (if (and (>= (length text) 3) (= #xEF (char-code (char text 0))))
      (subseq text 3)
      text))

(test the-report-is-written-to-the-file-that-is-chosen
  (with-temporary-directory (directory)
    (let ((application (quantity-app)))
      (setf (app:app-file-path application) (merge-pathnames "plan.bcad" directory))
      (open-quantities application)
      (dotimes (index 3)
        (press application :space))
      (press application :tab)
      (press application :tab)
      (press application :space)
      (press application :enter)
      (press application :enter)
      (is (equal "plan.csv" (ui:edit-state-text (app:file-dialog-name-edit (dialog-of application)))))
      (press application :enter)
      (let ((text (fixture-like-text (merge-pathnames "plan.csv" directory))))
        (is (search (format nil "Layer;Bar length (mm);Quantity;Total length (mm)~c~c" #\Return #\Newline)
                    (substitute-bom text))
            "Excel in Portuguese: a semicolon, after the byte order mark")
        (is (= #xEF (char-code (char text 0))))
        (is (search "steel;1000,0;4;4000,0" text)))
      (is (equal "Quantities saved" (prompt-title-of application)))
      (is (search "plan.csv was saved" (app:prompt-message (app:app-prompt application)))))))

(test the-standard-format-has-no-byte-order-mark
  (with-temporary-directory (directory)
    (let ((application (quantity-app)))
      (setf (app:app-file-path application) (merge-pathnames "plan.bcad" directory))
      (open-quantities application)
      (press application :enter)
      (press application :enter)
      (press application :enter)
      (let ((text (fixture-like-text (merge-pathnames "plan.csv" directory))))
        (is (char= #\L (char text 0)))
        (is (search "steel,7,5300.0,5000.0" text))))))

;;; The console

(test the-console-returns-the-quantities-as-rows
  (let* ((drawing (steel-drawing))
         (context (script-env drawing)))
    (is (equal '("3") (ran-ok context "(length (quantities :layer))")) "two layers and the total")
    (is (equal '("\"steel\"") (ran-ok context "(first (first (quantities :layer)))")))
    (is (equal '("\"Total\"") (ran-ok context "(first (car (last (quantities :cut :ids '(1 2 3)))))")))
    (is (equal '("3") (ran-ok context "(third (car (last (quantities :cut :ids '(1 2 3)))))")))
    (is (search "nothing to count" (failure-of (script-env (dom:make-drawing)) "(quantities :layer)")))
    (is (search "quantities" (script:script-result-output (run-script (script-env) "(help)"))))))
