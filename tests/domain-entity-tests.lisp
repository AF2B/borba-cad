(in-package #:borba-cad/tests)

(def-suite domain-entities
  :description "Entity constructors, invariants and the entity protocol."
  :in all)

(in-suite domain-entities)

;;; Helpers

(defun derived-value-of (entity label)
  "Return the derived value of ENTITY whose label is LABEL."
  (dom:derived-value-value
   (find label (dom:entity-derived-values entity) :key #'dom:derived-value-label :test #'string=)))

(defun a-line (&key (id 1) (layer-id 0) (x1 0d0) (y1 0d0) (x2 1000d0) (y2 0d0))
  "Build a valid line, overriding fields as needed."
  (dom:make-line :id id :layer-id layer-id :x1 x1 :y1 y1 :x2 x2 :y2 y2))

(defun a-rectangle (&key (id 1) (layer-id 0) (min-x 0d0) (min-y 0d0) (width 1000d0) (height 500d0))
  "Build a valid rectangle, overriding fields as needed."
  (dom:make-rectangle :id id :layer-id layer-id :min-x min-x :min-y min-y :width width :height height))

(defun a-circle (&key (id 1) (layer-id 0) (center-x 0d0) (center-y 0d0) (radius 100d0))
  "Build a valid circle, overriding fields as needed."
  (dom:make-circle :id id :layer-id layer-id :center-x center-x :center-y center-y :radius radius))

(defun doubles (&rest numbers)
  "Return NUMBERS as a simple double-float array."
  (make-array (length numbers) :element-type 'double-float :initial-contents numbers))

(defun a-polyline (&key (id 1) (layer-id 0) (coordinates (doubles 0d0 0d0 100d0 0d0 100d0 100d0)) closed)
  "Build a valid polyline, overriding fields as needed."
  (dom:make-polyline :id id :layer-id layer-id :coordinates coordinates :closed closed))

(defun a-text (&key (id 1) (layer-id 0) (x 0d0) (y 0d0) (string "Kitchen") (height 100d0))
  "Build a valid text annotation, overriding fields as needed."
  (dom:make-text :id id :layer-id layer-id :x x :y y :string string :height height))

(defun a-dimension (&key (id 1) (layer-id 0) (x1 0d0) (y1 0d0) (x2 1000d0) (y2 0d0)
                         (offset 500d0) (text-height 100d0) (unit :millimeter) (precision 0))
  "Build a valid aligned dimension, overriding fields as needed."
  (dom:make-dimension :id id :layer-id layer-id :x1 x1 :y1 y1 :x2 x2 :y2 y2
                      :offset offset :text-height text-height :unit unit :precision precision))

(defmacro signals-invalid-entity (kind &body body)
  "Check that BODY signals INVALID-ENTITY of entity KIND, and that the message is non-empty."
  `(handler-case (progn ,@body (fail "expected INVALID-ENTITY, but no error was signaled"))
     (base:invalid-entity (condition)
       (is (eq ,kind (base:invalid-entity-kind condition)))
       (is (plusp (length (princ-to-string condition)))))))

;;; Line

(test line-construction-and-accessors
  (let ((line (a-line :id 7 :layer-id 3 :x1 10d0 :y1 20d0 :x2 40d0 :y2 60d0)))
    (is (= 7 (dom:entity-id line)))
    (is (= 3 (dom:entity-layer-id line)))
    (is (= 10d0 (dom:line-x1 line)))
    (is (= 60d0 (dom:line-y2 line)))
    (is (eq :line (dom:entity-kind line)))
    (let ((bounds (dom:entity-bounds line)))
      (is (= 10d0 (geo:bounds-min-x bounds)))
      (is (= 60d0 (geo:bounds-max-y bounds))))))

(test line-rejects-degenerate-and-out-of-range-input
  (signals-invalid-entity :line (a-line :x2 0d0 :y2 0d0))                        ; zero length
  (signals-invalid-entity :line (a-line :x2 base:+length-tolerance+))            ; exactly the tolerance
  (is (a-line :x2 (* 2 base:+length-tolerance+)))                                 ; just above it
  (signals-invalid-entity :line (a-line :x1 (* 2 base:+maximum-coordinate-magnitude+)))
  (signals-invalid-entity :line (a-line :x1 0))                                   ; integer, not a double
  (signals-invalid-entity :line (a-line :y2 1.0))                                 ; single-float
  (signals-invalid-entity :line (a-line :x1 "0"))
  (signals-invalid-entity :line (a-line :id 0))
  (signals-invalid-entity :line (a-line :layer-id -1)))

(test line-accepts-coordinates-at-the-limit
  (let ((limit base:+maximum-coordinate-magnitude+))
    (is (a-line :x1 (- limit) :y1 (- limit) :x2 limit :y2 limit))))

(test line-error-message-names-the-problem
  (handler-case (a-line :x2 0d0 :y2 0d0)
    (base:invalid-entity (condition)
      (let ((message (princ-to-string condition)))
        (is (search "Invalid line" message))
        (is (search "length" message))))))

;;; Rectangle

(test rectangle-construction-and-corner-normalization
  (let ((rectangle (a-rectangle :min-x -10d0 :min-y 5d0 :width 30d0 :height 20d0)))
    (is (eq :rectangle (dom:entity-kind rectangle)))
    (is (= 20d0 (geo:bounds-max-x (dom:entity-bounds rectangle))))
    (is (= 25d0 (geo:bounds-max-y (dom:entity-bounds rectangle)))))
  (let ((normalized (dom:make-rectangle-from-corners :id 1 :layer-id 0 :x1 50d0 :y1 40d0 :x2 10d0 :y2 -20d0)))
    (is (= 10d0 (dom:rectangle-min-x normalized)))
    (is (= -20d0 (dom:rectangle-min-y normalized)))
    (is (= 40d0 (dom:rectangle-width normalized)))
    (is (= 60d0 (dom:rectangle-height normalized)))))

(test rectangle-rejects-degenerate-sizes-and-out-of-range-extent
  (signals-invalid-entity :rectangle (a-rectangle :width 0d0))
  (signals-invalid-entity :rectangle (a-rectangle :height -5d0))
  (signals-invalid-entity :rectangle (a-rectangle :width base:+length-tolerance+))
  (signals-invalid-entity :rectangle (a-rectangle :min-x (- base:+maximum-coordinate-magnitude+ 10d0)
                                                  :width 100d0))
  (signals-invalid-entity :rectangle
    (dom:make-rectangle-from-corners :id 1 :layer-id 0 :x1 5d0 :y1 5d0 :x2 5d0 :y2 50d0)))

;;; Circle

(test circle-construction-and-extent-check
  (let ((circle (a-circle :center-x 10d0 :center-y -10d0 :radius 5d0)))
    (is (eq :circle (dom:entity-kind circle)))
    (is (= 5d0 (geo:bounds-min-x (dom:entity-bounds circle))))
    (is (= -15d0 (geo:bounds-min-y (dom:entity-bounds circle)))))
  (signals-invalid-entity :circle (a-circle :radius 0d0))
  (signals-invalid-entity :circle (a-circle :radius -1d0))
  (signals-invalid-entity :circle (a-circle :center-x (- base:+maximum-coordinate-magnitude+ 10d0)
                                            :radius 100d0)))

;;; Polyline

(test polyline-construction-copies-its-input
  (let* ((input (doubles 0d0 0d0 100d0 0d0 100d0 100d0))
         (polyline (a-polyline :coordinates input)))
    (setf (aref input 0) 999d0)
    (is (= 0d0 (aref (dom:polyline-coordinates polyline) 0)))
    (is (not (dom:polyline-closed polyline)))
    (is (eq :polyline (dom:entity-kind polyline)))
    (is (= 100d0 (geo:bounds-max-x (dom:entity-bounds polyline))))))

(test polyline-accepts-lists-and-vectors-of-doubles
  (is (a-polyline :coordinates (list 0d0 0d0 10d0 10d0)))
  (is (a-polyline :coordinates (vector 0d0 0d0 10d0 10d0))))

(test polyline-rejects-invalid-vertex-sequences
  (signals-invalid-entity :polyline (a-polyline :coordinates (doubles 0d0 0d0)))                  ; one vertex
  (signals-invalid-entity :polyline (a-polyline :coordinates (doubles 0d0 0d0 5d0 5d0) :closed t)) ; closed needs 3
  (signals-invalid-entity :polyline (a-polyline :coordinates (doubles 0d0 0d0 10d0)))             ; odd count
  (signals-invalid-entity :polyline (a-polyline :coordinates (doubles 0d0 0d0 0d0 0d0 5d0 5d0)))  ; coincident neighbors
  (signals-invalid-entity :polyline (a-polyline :coordinates (doubles 0d0 0d0 10d0 0d0 0d0 0d0)
                                                :closed t))                                        ; last = first
  (signals-invalid-entity :polyline (a-polyline :coordinates (list 0d0 0d0 10 10)))               ; integers
  (signals-invalid-entity :polyline (a-polyline :coordinates 42)))

(test polyline-vertex-limit-boundary
  (flet ((path (vertex-count)
           (let ((coordinates (make-array (* 2 vertex-count) :element-type 'double-float :initial-element 0d0)))
             (dotimes (vertex vertex-count coordinates)
               (setf (aref coordinates (* 2 vertex)) (* 10d0 vertex))))))
    (is (a-polyline :coordinates (path dom:+maximum-polyline-vertices+)))
    (signals-invalid-entity :polyline
      (a-polyline :coordinates (path (1+ dom:+maximum-polyline-vertices+))))))

(test closed-polyline-includes-the-closing-segment-in-length-and-picking
  (let ((open (a-polyline :coordinates (doubles 0d0 0d0 10d0 0d0 10d0 10d0)))
        (closed (a-polyline :coordinates (doubles 0d0 0d0 10d0 0d0 10d0 10d0) :closed t)))
    (is (= 20d0 (derived-value-of open "Length")))
    (is (approx= (+ 20d0 (sqrt 200d0)) (derived-value-of closed "Length")))
    (is (= 5d0 (dom:entity-pick-distance open 5d0 5d0)))
    (is (approx= 0d0 (dom:entity-pick-distance closed 5d0 5d0)))))

;;; Text

(test text-construction-copies-the-string-and-computes-bounds
  (let* ((source (copy-seq "Hello"))
         (text (a-text :string source :x 10d0 :y 20d0 :height 100d0)))
    (setf (char source 0) #\J)
    (is (string= "Hello" (dom:text-string text)))
    (is (eq :text (dom:entity-kind text)))
    ;; 5 characters x 0.6 em x 100 = 300 wide; baseline at y = 20; box spans -25..95 around it.
    (let ((bounds (dom:entity-bounds text)))
      (is (approx= 10d0 (geo:bounds-min-x bounds)))
      (is (approx= 310d0 (geo:bounds-max-x bounds)))
      (is (approx= -5d0 (geo:bounds-min-y bounds)))
      (is (approx= 115d0 (geo:bounds-max-y bounds))))))

(test text-accepts-non-ascii-characters
  (let ((string (coerce (list (code-char 233) (code-char 231) (code-char 227) #\x) 'string)))
    (is (= 4 (length (dom:text-string (a-text :string string)))))))

(test text-rejects-invalid-strings-and-heights
  (signals-invalid-entity :text (a-text :string ""))
  (signals-invalid-entity :text (a-text :string (coerce (list #\a (code-char 10) #\b) 'string)))
  (signals-invalid-entity :text (a-text :string (coerce (list #\a (code-char 127)) 'string)))
  (signals-invalid-entity :text (a-text :string 'not-a-string))
  (signals-invalid-entity :text (a-text :string (make-string (1+ dom:+maximum-text-length+)
                                                             :initial-element #\a)))
  (is (a-text :string (make-string dom:+maximum-text-length+ :initial-element #\a)))
  (signals-invalid-entity :text (a-text :height 0d0))
  (signals-invalid-entity :text (a-text :x (* 3 base:+maximum-coordinate-magnitude+))))

;;; Dimension

(test dimension-derives-its-measured-value-text-and-layout
  (let ((dimension (a-dimension :x2 3500d0 :unit :meter :precision 2)))
    (is (eq :dimension (dom:entity-kind dimension)))
    (is (= 3500d0 (dom:dimension-measured-length dimension)))
    (is (string= "3.50" (dom:dimension-text dimension)))
    (is (typep (dom:dimension-layout dimension) 'geo:dimension-layout))
    (is (geo:bounds-contains-bounds-p (dom:entity-bounds dimension)
                                      (geo:dimension-layout-bounds (dom:dimension-layout dimension)))))
  (is (string= "1000" (dom:dimension-text (a-dimension)))))

(test dimension-rejects-invalid-definitions
  (signals-invalid-entity :dimension (a-dimension :x2 0d0 :y2 0d0))
  (signals-invalid-entity :dimension (a-dimension :unit :furlong))
  (signals-invalid-entity :dimension (a-dimension :precision -1))
  (signals-invalid-entity :dimension (a-dimension :precision (1+ dom:+maximum-dimension-precision+)))
  (is (a-dimension :precision dom:+maximum-dimension-precision+))
  (signals-invalid-entity :dimension (a-dimension :text-height 0d0))
  (signals-invalid-entity :dimension (a-dimension :offset (* 3 base:+maximum-coordinate-magnitude+))))

;;; Entity protocol

(defun every-entity-kind ()
  "Return one valid entity of every kind, all with id 1 on layer 0."
  (list (a-line) (a-rectangle) (a-circle) (a-polyline) (a-text) (a-dimension)
        (dom:make-arc :id 1 :layer-id 0 :center-x 0d0 :center-y 0d0 :radius 100d0 :start-angle 0d0
                      :end-angle 1d0)
        (dom:make-ellipse :id 1 :layer-id 0 :center-x 0d0 :center-y 0d0 :radius-x 200d0 :radius-y 100d0)
        (dom:make-polygon :id 1 :layer-id 0 :center-x 0d0 :center-y 0d0 :radius 100d0 :sides 6)
        (dom:make-radial-dimension :id 1 :layer-id 0 :center-x 0d0 :center-y 0d0 :radius 100d0 :angle 0.7d0
                                   :diameter nil :text-height 20d0 :unit :millimeter :precision 1)
        (dom:make-angular-dimension :id 1 :layer-id 0 :vertex-x 0d0 :vertex-y 0d0 :start-angle 0.2d0
                                    :end-angle 1.4d0 :radius 100d0 :text-height 20d0 :precision 1)
        (dom:make-hatch :id 1 :layer-id 0 :coordinates (doubles 0d0 0d0 100d0 0d0 100d0 100d0 0d0 100d0)
                        :pattern :lines :angle 0.5d0 :spacing 10d0)
        (dom:make-insert :id 1 :layer-id 0
                         :block (dom:make-block-definition
                                 :name "pair"
                                 :entities (list (dom:make-line :id 1 :layer-id 0 :x1 0d0 :y1 0d0 :x2 50d0 :y2 20d0)
                                                 (dom:make-circle :id 2 :layer-id 0 :center-x 10d0 :center-y 10d0
                                                                  :radius 5d0)))
                         :x 100d0 :y 100d0 :scale 2d0 :rotation 0.3d0)))

(test every-kind-reports-a-distinct-kind-keyword-and-bounds
  (let ((entities (every-entity-kind)))
    (is (equal (dom:entity-kinds) (mapcar #'dom:entity-kind entities)))
    (dolist (entity entities)
      (is (typep (dom:entity-bounds entity) 'geo:bounds)))))

(test pick-distance-analytic-cases
  (is (= 20d0 (dom:entity-pick-distance (a-line) 500d0 20d0)))               ; perpendicular to the segment
  (is (= 20d0 (dom:entity-pick-distance (a-line) -20d0 0d0)))               ; before the start
  (is (= 100d0 (dom:entity-pick-distance (a-rectangle) 500d0 -100d0)))       ; below the bottom edge
  (is (= 250d0 (dom:entity-pick-distance (a-rectangle) 500d0 250d0)))        ; the middle: 250 from top and bottom
  (is (approx= 0d0 (dom:entity-pick-distance (a-circle) 60d0 80d0)))         ; on the circle
  (is (= 100d0 (dom:entity-pick-distance (a-circle) 0d0 0d0)))               ; the center is a radius away
  (is (= 0d0 (dom:entity-pick-distance (a-text :height 100d0) 50d0 10d0)))   ; inside the filled box
  (is (> (dom:entity-pick-distance (a-text :height 100d0) 50d0 500d0) 0d0))
  (let ((dimension (a-dimension)))
    (is (approx= 0d0 (dom:entity-pick-distance dimension 500d0 500d0)))      ; on the dimension line
    (is (approx= 0d0 (dom:entity-pick-distance dimension 0d0 300d0)))        ; on an extension line
    (is (> (dom:entity-pick-distance dimension 500d0 -400d0) 0d0))))

(test dimension-text-is-pickable
  (let* ((dimension (a-dimension))
         (layout (dom:dimension-layout dimension)))
    (is (approx= 0d0 (dom:entity-pick-distance dimension
                                               (+ (geo:dimension-layout-text-x layout) 10d0)
                                               (+ (geo:dimension-layout-text-y layout) 20d0))))))

(test translation-moves-every-kind-and-keeps-the-id
  (dolist (entity (every-entity-kind))
    (let* ((moved (dom:entity-translated entity 30d0 -40d0))
           (before (dom:entity-bounds entity))
           (after (dom:entity-bounds moved)))
      (is (= (dom:entity-id entity) (dom:entity-id moved)))
      (is (eq (dom:entity-kind entity) (dom:entity-kind moved)))
      (is (approx= (+ 30d0 (geo:bounds-min-x before)) (geo:bounds-min-x after)))
      (is (approx= (- (geo:bounds-max-y before) 40d0) (geo:bounds-max-y after))))))

(test translation-never-mutates-the-original
  (let* ((polyline (a-polyline))
         (original (copy-seq (dom:polyline-coordinates polyline))))
    (dom:entity-translated polyline 500d0 500d0)
    (is (equalp original (dom:polyline-coordinates polyline)))))

(test translation-out-of-range-signals-invalid-entity
  (signals-invalid-entity :line
    (dom:entity-translated (a-line :x1 0d0 :x2 1000d0) base:+maximum-coordinate-magnitude+ 0d0)))

(test snap-points-per-kind
  (flet ((kinds (entity) (mapcar #'third (dom:entity-snap-points entity))))
    (is (equal '(:endpoint :endpoint) (kinds (a-line))))
    (is (= 4 (length (dom:entity-snap-points (a-rectangle)))))
    (is (equal '(:center) (kinds (a-circle))))
    (is (= 3 (length (dom:entity-snap-points (a-polyline)))))
    (is (equal '(:endpoint) (kinds (a-text))))
    (is (equal '(:endpoint :endpoint) (kinds (a-dimension)))))
  (is (equal '(0d0 0d0 :center) (first (dom:entity-snap-points (a-circle))))))

(test derived-values-match-closed-form-geometry
  (is (= 1000d0 (derived-value-of (a-line) "Length")))
  (is (approx= 0d0 (derived-value-of (a-line) "Angle")))
  (is (= 500000d0 (derived-value-of (a-rectangle) "Area")))
  (is (= 3000d0 (derived-value-of (a-rectangle) "Perimeter")))
  (is (= 200d0 (derived-value-of (a-circle) "Diameter")))
  (is (approx= (* 10000d0 (float pi 1d0)) (derived-value-of (a-circle) "Area")))
  (is (= 3 (derived-value-of (a-polyline) "Vertices")))
  (is (= 210d0 (derived-value-of (a-text :string "1234567" :height 50d0) "Width")))
  (is (= 1000d0 (derived-value-of (a-dimension) "Measured length"))))

(test inspector-properties-per-kind
  (flet ((keys (entity) (mapcar #'dom:property-key (dom:entity-properties entity))))
    (is (equal '(:x1 :y1 :x2 :y2) (keys (a-line))))
    (is (equal '(:min-x :min-y :width :height) (keys (a-rectangle))))
    (is (equal '(:center-x :center-y :radius) (keys (a-circle))))
    (is (equal '(:x :y :string :height :angle) (keys (a-text))))
    (is (equal '(:x1 :y1 :x2 :y2 :offset :text-height :unit :precision) (keys (a-dimension)))))
  (is (every #'dom:property-editable (dom:entity-properties (a-line))))
  (is (eq :length (dom:property-kind (third (dom:entity-properties (a-rectangle)))))))

(test polyline-inspector-lists-a-bounded-number-of-vertices
  (let* ((many (let ((coordinates (make-array 40 :element-type 'double-float :initial-element 0d0)))
                 (dotimes (vertex 20 coordinates)
                   (setf (aref coordinates (* 2 vertex)) (* 10d0 vertex)))))
         (rows (dom:entity-properties (a-polyline :coordinates many))))
    (is (eq :closed (dom:property-key (first rows))))
    (is (= (1+ (* 2 dom:+inspector-maximum-vertex-rows+)) (length rows)))))

(test editing-a-property-returns-a-validated-copy
  (let* ((line (a-line))
         (edited (dom:entity-with-property line :x2 2000d0)))
    (is (= 2000d0 (dom:line-x2 edited)))
    (is (= 1000d0 (dom:line-x2 line)))                                  ; the original is untouched
    (is (= (dom:entity-id line) (dom:entity-id edited))))
  (is (= 77d0 (dom:circle-radius (dom:entity-with-property (a-circle) :radius 77d0))))
  (is (string= "Bath" (dom:text-string (dom:entity-with-property (a-text) :string "Bath"))))
  (is (eq :meter (dom:dimension-unit (dom:entity-with-property (a-dimension) :unit :meter))))
  (is (dom:polyline-closed (dom:entity-with-property (a-polyline) :closed t)))
  (is (= 55d0 (aref (dom:polyline-coordinates
                     (dom:entity-with-property (a-polyline) (cons :vertex-x 1) 55d0))
                    2))))

(test editing-a-property-into-an-invalid-state-is-rejected
  (signals-invalid-entity :rectangle (dom:entity-with-property (a-rectangle) :width 0d0))
  (signals-invalid-entity :line (dom:entity-with-property (a-line) :x2 0d0))
  (signals-invalid-entity :dimension (dom:entity-with-property (a-dimension) :precision 99))
  (signals-invalid-entity :polyline
    (dom:entity-with-property (a-polyline) (cons :vertex-x 1) 0d0)))

(test editing-an-unknown-property-is-a-programming-error
  (signals error (dom:entity-with-property (a-line) :radius 5d0))
  (signals error (dom:entity-with-property (a-polyline) (cons :vertex-x 99) 5d0))
  (signals error (dom:entity-with-property (a-polyline) :bogus 5d0)))

(test entity-with-layer-keeps-everything-else
  (dolist (entity (every-entity-kind))
    (let ((moved (dom:entity-with-layer entity 9)))
      (is (= 9 (dom:entity-layer-id moved)))
      (is (= (dom:entity-id entity) (dom:entity-id moved)))
      (is (eq (dom:entity-kind entity) (dom:entity-kind moved)))
      (is (equalp (dom:entity-bounds entity) (dom:entity-bounds moved))))))
