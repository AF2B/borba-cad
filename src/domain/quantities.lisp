(in-package #:borba-cad/domain)

;;;; Quantities: what a drawing holds, counted and measured, for a bill of materials or a cut list. Four reports: the
;;;; totals by layer, the totals by kind of entity, the blocks that are placed, and the bars by length. A report holds
;;;; the numbers in millimetres; showing them in the unit of the drawing and writing them as CSV are done from it. What
;;;; is inside a block is counted as many times as the block is placed, at the size it is placed, and a layer that is
;;;; hidden is not counted unless the entities are asked for by their ids.

(defconstant +cut-length-resolution+ 0.1d0
  "Bars whose lengths differ by less than this, in millimetres, are bars of the same length in the cut list.")

(defstruct (quantity-report (:constructor make-quantity-report (kind
                                                                columns
                                                                rows
                                                                totals)))
  "A table of quantities.

KIND: :layer, :kind, :block or :cut.
COLUMNS: list of (heading . type) with the type :text, :count, :length or :area; the headings are translated.
ROWS: list of lists with one value for each column: a string, a whole number, or a number of millimetres or square
millimetres.
TOTALS: a list like a row, which sums what can be summed, or NIL."
  kind columns rows totals)

(defstruct (counted (:constructor make-counted (entity
                                                block-name
                                                placement)))
  "An entity that is counted.

ENTITY: the entity, as placed when it comes from a block.
BLOCK-NAME: the name of the block it comes from, or NIL.
PLACEMENT: the id of the placement it comes from, or NIL."
  entity block-name placement)

(defun entity-length (entity)
  "Return the length of an entity that is a line, a curve, a polyline or a shape, in millimetres, the perimeter for a
shape that is closed; 0 for any other.

ENTITY: an entity."
  (typecase entity
    (line-entity (geo:segment-length (line-x1 entity)
                                     (line-y1 entity)
                                     (line-x2 entity)
                                     (line-y2 entity)))
    (polyline-entity (geo:polyline-length (polyline-coordinates entity) (polyline-closed entity)))
    (arc-entity (geo:arc-length (arc-radius entity) (arc-sweep entity)))
    (circle-entity (geo:circle-circumference (circle-radius entity)))
    (ellipse-entity (geo:ellipse-perimeter (ellipse-radius-x entity) (ellipse-radius-y entity)))
    (rectangle-entity (geo:rectangle-perimeter (rectangle-width entity) (rectangle-height entity)))
    (polygon-entity (geo:polyline-length (polygon-vertices entity) t))
    (t 0d0)))

(defun entity-area (entity)
  "Return the area that an entity encloses, in square millimetres: that of a closed shape, and what is left of a hatch
once its holes are taken out; 0 for any other.

ENTITY: an entity."
  (typecase entity
    (polyline-entity (if (polyline-closed entity)
                         (abs (geo:contour-area (polyline-coordinates entity)))
                         0d0))
    (circle-entity (geo:circle-area (circle-radius entity)))
    (ellipse-entity (geo:ellipse-area (ellipse-radius-x entity) (ellipse-radius-y entity)))
    (rectangle-entity (geo:rectangle-area (rectangle-width entity) (rectangle-height entity)))
    (polygon-entity (abs (geo:contour-area (polygon-vertices entity))))
    (hatch-entity (- (abs (geo:contour-area (hatch-coordinates entity)))
                     (loop for ring in (hatch-holes entity)
                           sum (abs (geo:contour-area ring)))))
    (t 0d0)))

(defun bar-p (entity)
  "Return true for an entity that is a bar of the cut list: a line, an arc, a circle, an ellipse, a polyline, a
rectangle or a polygon, each of which is one bar, as long as its length or its perimeter.

ENTITY: an entity."
  (typep entity '(or line-entity arc-entity circle-entity ellipse-entity polyline-entity rectangle-entity
                  polygon-entity)))

(defun counted-entities (drawing
                         ids)
  "Return the entities that a report counts, as COUNTED structs in the order they are drawn, the entities of a block
that is placed instead of the placement: those with IDS, or when IDS is NIL every entity on a layer that is not
hidden.

DRAWING: the drawing.
IDS: list of entity ids, or NIL for the whole drawing."
  (let ((counted '()))
    (flet ((add (entity)
             (if (typep entity 'insert-entity)
                 (dolist (placed (insert-entities entity))
                   (push (make-counted placed
                                       (block-definition-name (insert-block entity))
                                       (entity-id entity))
                         counted))
                 (push (make-counted entity
                                     nil
                                     nil)
                       counted))))
      (if ids
          (dolist (id ids)
            (let ((entity (store-find (drawing-store drawing) id)))
              (when entity
                (add entity))))
          (map-entities (lambda (entity)
                          (let ((layer (find-layer drawing (entity-layer-id entity))))
                            (when (and layer (layer-visible layer))
                              (add entity))))
                        (drawing-store drawing))))
    (nreverse counted)))

(defun layer-label (drawing
                    entity)
  "Return the name of the layer of an entity.

DRAWING: the drawing.
ENTITY: the entity."
  (let ((layer (find-layer drawing (entity-layer-id entity))))
    (if layer
        (layer-name layer)
        "")))

(defparameter *kind-labels*
  (list (cons :line (i18n:mark "Line"))
        (cons :rectangle (i18n:mark "Rectangle"))
        (cons :circle (i18n:mark "Circle"))
        (cons :polyline (i18n:mark "Polyline"))
        (cons :text (i18n:mark "Text"))
        (cons :dimension (i18n:mark "Dimension"))
        (cons :radial-dimension (i18n:mark "Radius or diameter dimension"))
        (cons :angular-dimension (i18n:mark "Angular dimension"))
        (cons :arc (i18n:mark "Arc"))
        (cons :ellipse (i18n:mark "Ellipse"))
        (cons :polygon (i18n:mark "Polygon"))
        (cons :hatch (i18n:mark "Hatch")))
  "The name of each kind of entity, in English, in the order the report by kind lists them.")

(defun grouped-rows (items
                     key
                     order
                     include-hatch-area)
  "Return one row for each group of the counted entities ITEMS, as (label count length area), in the order of ORDER.

ITEMS: the COUNTED structs.
KEY: function of a counted struct that returns what it is grouped by.
ORDER: the keys, in the order the rows go; a key that no entity has gives no row.
INCLUDE-HATCH-AREA: true to add the area of hatches, which is false for a total, since a hatch fills a shape that is
already counted."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (item items)
      (let* ((entity (counted-entity item))
             (cell (or (gethash (funcall key item) table)
                       (setf (gethash (funcall key item) table) (list 0 0d0 0d0)))))
        (incf (first cell))
        (incf (second cell) (entity-length entity))
        (when (or include-hatch-area (not (typep entity 'hatch-entity)))
          (incf (third cell) (entity-area entity)))))
    (loop for group in order
          for cell = (gethash group table)
          when cell
            collect (list group (first cell) (second cell) (third cell)))))

(defun sum-rows (rows)
  "Return the totals of rows of (label count length area): the sums of the three numbers.

ROWS: the rows."
  (list (i18n:tr "Total")
        (reduce #'+ rows :key #'second)
        (reduce #'+ rows :key #'third)
        (reduce #'+ rows :key #'fourth)))

(defun grouped-report (kind
                       rows
                       first-heading)
  "Make a report of the kind that has a name, a count, a length and an area in each row.

KIND: the kind of the report.
ROWS: rows of (label count length area).
FIRST-HEADING: the heading of the first column."
  (make-quantity-report kind
                        (list (cons first-heading :text)
                              (cons (i18n:tr "Count") :count)
                              (cons (i18n:tr "Length") :length)
                              (cons (i18n:tr "Area") :area))
                        rows
                        (and rows (sum-rows rows))))

(defun layer-report (drawing
                     items)
  "Return the report of the totals by layer.

DRAWING: the drawing.
ITEMS: the counted entities."
  (let ((rows (grouped-rows items
                            (lambda (item) (layer-label drawing (counted-entity item)))
                            (mapcar #'layer-name (layers-in-order drawing))
                            nil)))
    (grouped-report :layer
                    rows
                    (i18n:tr "Layer"))))

(defun kind-report (items)
  "Return the report of the totals by kind of entity.

ITEMS: the counted entities."
  (let ((rows (grouped-rows items
                            (lambda (item) (entity-kind (counted-entity item)))
                            (mapcar #'car *kind-labels*)
                            t)))
    (grouped-report :kind
                    (loop for (kind . rest) in rows
                          collect (cons (i18n:tr (cdr (assoc kind *kind-labels*))) rest))
                    (i18n:tr "Kind"))))

(defun block-report (items)
  "Return the report of the blocks that are placed: for each, how many times, and the length and area of what is in all
its placements.

ITEMS: the counted entities."
  (let ((placements (make-hash-table :test #'equal))
        (names '()))
    (dolist (item items)
      (let ((name (counted-block-name item)))
        (when name
          (unless (gethash name placements)
            (setf (gethash name placements) (make-hash-table))
            (push name names))
          (setf (gethash (counted-placement item) (gethash name placements)) t))))
    (let ((rows (grouped-rows (remove-if-not #'counted-block-name items)
                              #'counted-block-name
                              (sort names #'string-lessp)
                              nil)))
      (grouped-report :block
                      (loop for (name nil length area) in rows
                            collect (list name
                                          (hash-table-count (gethash name placements))
                                          length
                                          area))
                      (i18n:tr "Block")))))

(defun cut-report (drawing
                   items)
  "Return the cut list: the bars of the drawing, with the number of bars of each length on each layer and their total
length, by layer in the order of the layers and then from the longest bar to the shortest.

DRAWING: the drawing.
ITEMS: the counted entities."
  (let ((table (make-hash-table :test #'equal))
        (layer-order (mapcar #'layer-name (layers-in-order drawing))))
    (dolist (item items)
      (let* ((entity (counted-entity item))
             (length (entity-length entity)))
        (when (and (bar-p entity) (base:positive-length-p length))
          (let ((key (cons (layer-label drawing entity)
                           (* +cut-length-resolution+ (round length +cut-length-resolution+)))))
            (incf (gethash key table 0))))))
    (let ((rows (sort (loop for (layer . length) being the hash-keys of table using (hash-value count)
                            collect (list layer length count (* length count)))
                      (lambda (first second)
                        (let ((first-position (or (position (first first) layer-order :test #'string=) 0))
                              (second-position (or (position (first second) layer-order :test #'string=) 0)))
                          (or (< first-position second-position)
                              (and (= first-position second-position)
                                   (> (second first) (second second)))))))))
      (make-quantity-report :cut
                            (list (cons (i18n:tr "Layer") :text)
                                  (cons (i18n:tr "Bar length") :length)
                                  (cons (i18n:tr "Quantity") :count)
                                  (cons (i18n:tr "Total length") :length))
                            rows
                            (and rows
                                 (list (i18n:tr "Total")
                                       nil
                                       (reduce #'+ rows :key #'third)
                                       (reduce #'+ rows :key #'fourth)))))))

(defun quantity-report (drawing
                        kind
                        &key ids)
  "Return a report of what the drawing holds. Signals EDIT-REFUSED when there is nothing to count.

DRAWING: the drawing.
KIND: :layer for the totals by layer, :kind for those by kind of entity, :block for the blocks that are placed, or
:cut for the cut list.
IDS: the ids of the entities to count, or NIL for everything on the layers that are not hidden."
  (let ((items (counted-entities drawing ids)))
    (when (null items)
      (refuse-edit "There is nothing to count."))
    (let ((report (ecase kind
                    (:layer (layer-report drawing items))
                    (:kind (kind-report items))
                    (:block (block-report items))
                    (:cut (cut-report drawing items)))))
      (when (null (quantity-report-rows report))
        (case kind
          (:block (refuse-edit "There are no blocks placed."))
          (:cut (refuse-edit "There are no bars to cut."))
          (t (refuse-edit "There is nothing to count."))))
      report)))

(defun counted-entity-count (drawing
                             ids)
  "Return how many entities a report would count, those inside the blocks that are placed included.

DRAWING: the drawing.
IDS: the ids of the entities to count, or NIL for everything on the layers that are not hidden."
  (length (counted-entities drawing ids)))

;;; Showing and writing a report

(defun quantity-heading (column
                         unit)
  "Return the heading of a column of a report with the unit of its numbers.

COLUMN: a column of the report, (heading . type).
UNIT: the display unit, :millimeter, :centimeter or :meter."
  (destructuring-bind (heading . type) column
    (case type
      (:length (format nil "~a (~a)" heading (base:unit-abbreviation unit)))
      (:area (format nil "~a (~a)" heading (base:unit-area-abbreviation unit)))
      (t heading))))

(defun quantity-cell-text (value
                           type
                           unit
                           &key (decimal-mark #\.))
  "Return a value of a report as text: a length or an area in the display unit, with one more decimal than is shown for
a length elsewhere.

VALUE: the value, or NIL for a cell that is empty.
TYPE: the type of its column, :text, :count, :length or :area.
UNIT: the display unit.
DECIMAL-MARK: the character between the whole part and the decimals."
  (if (null value)
      ""
      (let ((precision (1+ (base:default-display-precision unit))))
        (substitute decimal-mark
                    #\.
                    (ecase type
                      (:text value)
                      (:count (princ-to-string value))
                      (:length (base:format-length value
                                                   unit
                                                   :precision precision))
                      (:area (base:format-area value
                                               unit
                                               :precision precision)))))))

(defun csv-field (text
                  separator)
  "Return TEXT as a field of a CSV file: in quotes, with the quotes in it doubled, when it holds the separator, a quote
or a line break.

TEXT: the text.
SEPARATOR: the character between fields."
  (if (find-if (lambda (character) (or (char= character separator)
                                       (char= character #\")
                                       (char= character #\Newline)
                                       (char= character #\Return)))
               text)
      (with-output-to-string (stream)
        (write-char #\" stream)
        (loop for character across text
              do (when (char= character #\")
                   (write-char #\" stream))
                 (write-char character stream))
        (write-char #\" stream))
      text))

(defun quantity-report-csv (report
                            unit
                            &key (separator #\,)
                                 (decimal-mark #\.))
  "Return a report as the text of a CSV file: a line of headings, a line for each row and one for the totals, with
lines that end in a carriage return and a line feed, as RFC 4180 has it.

REPORT: the report.
UNIT: the display unit, which the lengths and areas are written in.
SEPARATOR: the character between fields; with a semicolon, and a comma as the decimal mark, Excel in Portuguese
opens it.
DECIMAL-MARK: the character between the whole part and the decimals of a number."
  (let ((columns (quantity-report-columns report)))
    (with-output-to-string (stream)
      (flet ((write-line-of (fields)
               (loop for (field . more) on fields
                     do (write-string (csv-field field separator) stream)
                        (when more
                          (write-char separator stream)))
               (write-char #\Return stream)
               (write-char #\Newline stream)))
        (write-line-of (mapcar (lambda (column) (quantity-heading column unit)) columns))
        (dolist (row (append (quantity-report-rows report)
                             (and (quantity-report-totals report) (list (quantity-report-totals report)))))
          (write-line-of (loop for value in row
                               for (nil . type) in columns
                               collect (quantity-cell-text value
                                                           type
                                                           unit
                                                           :decimal-mark decimal-mark))))))))
