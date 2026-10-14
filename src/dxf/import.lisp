(in-package #:borba-cad/dxf)

;;;; Turning what a DXF file holds into a drawing. The records of the file are first made into
;;;; shapes in the units of the file (a line, a circle, an arc, a polyline with its arc segments cut
;;;; into straight ones, a text, an ellipse), the blocks inserted into the drawing are expanded, and
;;;; only then, when the extent of everything is known, are the entities of the drawing built. What
;;;; cannot be turned into an entity is counted and said in the report, never dropped quietly.

(defconstant +maximum-shapes+ 4000000
  "Most shapes an import makes, blocks expanded: it stops a block that inserts blocks that insert blocks
from asking for more memory than there is.")

(defconstant +maximum-reported-problems+ 5
  "Most distinct reasons that an entity could not be made that the report keeps.")

(defconstant +arc-step-degrees+ 5d0
  "Largest angle, in degrees, that a segment of a polyline made from an arc spans: the arc is cut into
segments of at most this, so a bulge looks round and the file stays small.")

(defconstant +degrees-per-half-turn+ 180d0
  "Degrees in half a turn.")

(defconstant +dxf-text-ascent-ratio+ 0.95d0
  "How far above the baseline, in text heights, the top of a text is taken to be when a DXF text is
anchored by its top.")

(defconstant +mtext-line-spacing-ratio+ 1.667d0
  "Distance between the baselines of two lines of a multiline text, in text heights.")

(defparameter *unit-millimeters*
  '((:millimeter . 1d0) (:centimeter . 10d0) (:meter . 1000d0) (:inch . 25.4d0) (:foot . 304.8d0))
  "How many millimetres each unit of a DXF file is.")

;;; The report

(defstruct (dxf-report (:constructor make-dxf-report ()))
  "What an import did and could not do.

IMPORTED: hash table from the kind of entity to how many were made.
IGNORED: hash table from the DXF type of an entity to how many were left out.
NOTES: the sentences that say what was approximated, moved or refused, newest first.
PROBLEMS: the reasons that entities could not be made, newest first."
  (imported (make-hash-table :test 'eq))
  (ignored (make-hash-table :test 'equal))
  (notes nil)
  (problems nil))

(defun count-up (table
                 key)
  "Add one to the count of KEY in a hash table of counts.

TABLE: a hash table from keys to integers.
KEY: the key."
  (incf (gethash key table 0)))

(defun table-alist (table)
  "Return a hash table of counts as an alist, the largest count first and ties in name order.

TABLE: a hash table from keys to integers."
  (sort (loop for key being the hash-keys of table using (hash-value count)
              collect (cons key count))
        (lambda (first second)
          (or (> (cdr first) (cdr second))
              (and (= (cdr first) (cdr second))
                   (string< (princ-to-string (car first)) (princ-to-string (car second))))))))

(defun report-imported (report)
  "Return how many entities of each kind were made, as an alist from the kind keyword to the count.

REPORT: a DXF-REPORT."
  (table-alist (dxf-report-imported report)))

(defun report-ignored (report)
  "Return how many entities of each DXF type were left out, as an alist from the type name to the
count, the most frequent first.

REPORT: a DXF-REPORT."
  (table-alist (dxf-report-ignored report)))

(defun report-notes (report)
  "Return the sentences that say what was approximated, moved or refused, oldest first.

REPORT: a DXF-REPORT."
  (reverse (dxf-report-notes report)))

(defun report-imported-total (report)
  "Return how many entities were made.

REPORT: a DXF-REPORT."
  (reduce #'+ (mapcar #'cdr (report-imported report))))

(defun report-ignored-total (report)
  "Return how many entities were left out.

REPORT: a DXF-REPORT."
  (reduce #'+ (mapcar #'cdr (report-ignored report))))

(defun note (report
             sentence)
  "Add a sentence to the notes of a report, unless it is there already.

REPORT: a DXF-REPORT.
SENTENCE: the text, already in the language of the interface."
  (pushnew sentence (dxf-report-notes report) :test #'string=))

(defun report-summary (report)
  "Return the report as a few sentences for the user: how many entities were made, which DXF types
were left out and how many of each, and the notes.

REPORT: a DXF-REPORT."
  (with-output-to-string (stream)
    (format stream "~a" (i18n:trf "~d ~:*~[entities~;entity~:;entities~] imported." (report-imported-total report)))
    (let ((ignored (report-ignored report)))
      (when ignored
        (format stream " ~a"
                (i18n:trf "Left out: ~{~a~^, ~}."
                          (loop for (type . count) in ignored
                                collect (format nil "~d ~a" count type))))))
    (dolist (sentence (report-notes report))
      (format stream " ~a" sentence))
    (dolist (problem (reverse (dxf-report-problems report)))
      (format stream " ~a" problem))))

;;; Affine transforms

(defstruct (affine (:constructor make-affine (a
                                              b
                                              c
                                              d
                                              e
                                              f)))
  "The plane transform x' = a x + c y + e, y' = b x + d y + f.

A: x coefficient of x'.
B: x coefficient of y'.
C: y coefficient of x'.
D: y coefficient of y'.
E: translation along x.
F: translation along y."
  (a 1d0 :type double-float :read-only t)
  (b 0d0 :type double-float :read-only t)
  (c 0d0 :type double-float :read-only t)
  (d 1d0 :type double-float :read-only t)
  (e 0d0 :type double-float :read-only t)
  (f 0d0 :type double-float :read-only t))

(defparameter *identity* (make-affine 1d0
                                      0d0
                                      0d0
                                      1d0
                                      0d0
                                      0d0)
  "The transform that leaves every point where it is.")

(defun affine-apply (transform
                     x
                     y)
  "Return, as two values, the point (X, Y) carried by TRANSFORM.

TRANSFORM: an affine.
X: x of the point.
Y: y of the point."
  (values (+ (* (affine-a transform) x) (* (affine-c transform) y) (affine-e transform))
          (+ (* (affine-b transform) x) (* (affine-d transform) y) (affine-f transform))))

(defun affine-compose (outer
                       inner)
  "Return the transform that does INNER first and OUTER after it.

OUTER: the transform done second.
INNER: the transform done first."
  (let ((a1 (affine-a outer)) (b1 (affine-b outer)) (c1 (affine-c outer)) (d1 (affine-d outer))
        (e1 (affine-e outer)) (f1 (affine-f outer))
        (a2 (affine-a inner)) (b2 (affine-b inner)) (c2 (affine-c inner)) (d2 (affine-d inner))
        (e2 (affine-e inner)) (f2 (affine-f inner)))
    (make-affine (+ (* a1 a2) (* c1 b2))
                 (+ (* b1 a2) (* d1 b2))
                 (+ (* a1 c2) (* c1 d2))
                 (+ (* b1 c2) (* d1 d2))
                 (+ (* a1 e2) (* c1 f2) e1)
                 (+ (* b1 e2) (* d1 f2) f1))))

(defun affine-determinant (transform)
  "Return the determinant of the linear part of TRANSFORM: negative when it turns the plane over.

TRANSFORM: an affine."
  (- (* (affine-a transform) (affine-d transform))
     (* (affine-b transform) (affine-c transform))))

(defun affine-similarity-scale (transform)
  "Return the factor by which TRANSFORM scales lengths when it is a rotation or a reflection and a
uniform scaling, and NIL when it stretches one way more than another or shears.

TRANSFORM: an affine."
  (let ((column-1 (sqrt (+ (expt (affine-a transform) 2) (expt (affine-b transform) 2))))
        (column-2 (sqrt (+ (expt (affine-c transform) 2) (expt (affine-d transform) 2))))
        (dot (+ (* (affine-a transform) (affine-c transform)) (* (affine-b transform) (affine-d transform)))))
    (and (> column-1 0d0)
         (< (abs (- column-1 column-2)) (* 1d-9 (max column-1 column-2)))
         (< (abs dot) (* 1d-9 column-1 column-2))
         column-1)))

(defun affine-rotation (transform)
  "Return the angle, in radians, that TRANSFORM turns the direction of the x axis by.

TRANSFORM: an affine."
  (atan (affine-b transform) (affine-a transform)))

(defun insert-transform (x
                         y
                         scale-x
                         scale-y
                         rotation
                         base-x
                         base-y)
  "Return the transform of an INSERT: the block is moved so that its base point is the origin, scaled,
turned by ROTATION in degrees, and put at (X, Y).

X: x of the insertion point.
Y: y of the insertion point.
SCALE-X: scale along x.
SCALE-Y: scale along y.
ROTATION: angle in degrees.
BASE-X: x of the base point of the block.
BASE-Y: y of the base point of the block."
  (let* ((angle (* (/ (float pi 1d0) +degrees-per-half-turn+) rotation))
         (cosine (cos angle))
         (sine (sin angle))
         (a (* cosine scale-x))
         (b (* sine scale-x))
         (c (- (* sine scale-y)))
         (d (* cosine scale-y)))
    (make-affine a
                 b
                 c
                 d
                 (+ x (- (+ (* a base-x) (* c base-y))))
                 (+ y (- (+ (* b base-x) (* d base-y)))))))

;;; Shapes: what the records say, before it is an entity

(defstruct (shape (:constructor make-shape (kind
                                            layer
                                            &rest data)))
  "A shape read from a DXF file, in the units of the file.

KIND: :line, :circle, :arc, :ellipse, :polyline or :text.
LAYER: the name of its layer.
DATA: what the kind needs: a line is x1 y1 x2 y2; a circle is x y radius; an arc is x y radius start end,
in radians; an ellipse is x y radius-x radius-y rotation; a polyline is a flat list of coordinates and
a closed flag; a text is x y height string angle."
  kind
  layer
  data)

(defstruct (importer (:constructor make-importer (document
                                                  report)))
  "The state of an import.

DOCUMENT: the DXF-DOCUMENT being imported.
REPORT: the report being written.
SHAPES: the shapes made so far, newest first.
COUNT: how many.
BLOCK-DEPTH: how deep in blocks the import is now."
  document
  report
  (shapes nil)
  (count 0 :type fixnum)
  (block-depth 0 :type fixnum))

(defun degrees->radians* (degrees)
  "Return DEGREES in radians.

DEGREES: an angle in degrees."
  (* degrees (/ (float pi 1d0) +degrees-per-half-turn+)))

(defun emit-shape (importer
                   shape)
  "Add a shape to the importer, unless it holds too many.

IMPORTER: the importer.
SHAPE: a shape."
  (cond ((>= (importer-count importer) +maximum-shapes+)
         (note (importer-report importer)
               (i18n:trf "The import stopped after ~d shapes: the drawing is too large." +maximum-shapes+)))
        (t
         (push shape (importer-shapes importer))
         (incf (importer-count importer)))))

(defun ignore-record (importer
                      record)
  "Count a record that is not made into an entity.

IMPORTER: the importer.
RECORD: the record."
  (count-up (dxf-report-ignored (importer-report importer)) (record-type record)))

(defun layer-name-of (record
                      inherited)
  "Return the name of the layer of an entity record; entities on layer 0 inside a block take the layer
of the insert that placed it.

RECORD: an entity record.
INHERITED: the layer of the INSERT being expanded, or NIL."
  (let ((name (or (pair-value (record-pairs record) 8) "0")))
    (if (and inherited (string= name "0"))
        inherited
        name)))

;;; Text

(defparameter *degree-sign* (string (code-char 176))
  "The degree sign, which %%d stands for in a DXF text.")

(defparameter *plus-minus-sign* (string (code-char 177))
  "The plus-minus sign, which %%p stands for.")

(defparameter *diameter-sign* (string (code-char 248))
  "The diameter sign, which %%c stands for.")

(defun clean-text (string)
  "Return the text of a DXF TEXT as it is to be shown: the special codes %%d, %%p, %%c and %%nnn turned
into the characters they stand for, the toggles %%u and %%o dropped, and \\U+hhhh turned into the
character with that code. Control characters are dropped.

STRING: the text as the file has it."
  (let ((result (make-string-output-stream))
        (index 0)
        (length (length string)))
    (loop while (< index length)
          do (let ((character (char string index)))
               (cond ((and (char= character #\%) (< (+ index 2) length) (char= #\% (char string (1+ index))))
                      (let ((code (char-downcase (char string (+ index 2)))))
                        (case code
                          (#\d (write-string *degree-sign* result) (incf index 3))
                          (#\p (write-string *plus-minus-sign* result) (incf index 3))
                          (#\c (write-string *diameter-sign* result) (incf index 3))
                          ((#\u #\o) (incf index 3))
                          (t (if (and (<= (+ index 5) length)
                                      (every #'digit-char-p (subseq string (+ index 2) (+ index 5))))
                                 (progn (write-char (code-char (parse-integer string :start (+ index 2)
                                                                                     :end (+ index 5)))
                                                    result)
                                        (incf index 5))
                                 (progn (write-char character result) (incf index)))))))
                     ((and (char= character #\\) (< (+ index 6) length)
                           (char-equal #\U (char string (1+ index)))
                           (char= #\+ (char string (+ index 2)))
                           (every (lambda (digit) (digit-char-p digit 16)) (subseq string (+ index 3) (+ index 7))))
                      (write-char (code-char (parse-integer string :start (+ index 3) :end (+ index 7) :radix 16))
                                  result)
                      (incf index 7))
                     ((or (< (char-code character) 32) (= (char-code character) 127))
                      (incf index))
                     (t (write-char character result)
                        (incf index)))))
    (get-output-stream-string result)))

(defun mtext-lines (string)
  "Return the lines of a multiline text, with its formatting codes taken out: braces, font, height and
color changes, underlines, and stacked fractions written as a slash.

STRING: the text as the file has it."
  (let ((result (make-string-output-stream))
        (lines '())
        (index 0)
        (length (length string)))
    (flet ((end-line ()
             (push (get-output-stream-string result) lines)))
      (loop while (< index length)
            do (let ((character (char string index)))
                 (cond ((and (char= character #\\) (< (1+ index) length))
                        (let ((code (char string (1+ index))))
                          (case code
                            ((#\P #\p) (end-line) (incf index 2))
                            (#\~ (write-char #\Space result) (incf index 2))
                            ((#\\ #\{ #\}) (write-char code result) (incf index 2))
                            ((#\L #\l #\O #\o #\K #\k) (incf index 2))
                            (#\S (let ((end (position #\; string :start index)))
                                   (let ((body (subseq string (+ index 2) (or end length))))
                                     (write-string (substitute #\/ #\^ (substitute #\/ #\# body)) result))
                                   (setf index (if end (1+ end) length))))
                            (#\U (write-char #\\ result) (incf index))
                            ((#\f #\F #\H #\h #\C #\c #\T #\t #\Q #\q #\W #\w #\A #\a)
                             (let ((end (position #\; string :start index)))
                               (setf index (if end (1+ end) length))))
                            (t (incf index 2)))))
                       ((member character '(#\{ #\})) (incf index))
                       (t (write-char character result) (incf index)))))
      (end-line))
    (nreverse lines)))

;;; Numbers out of the groups

(defun real-of (record
                code
                &optional (default 0d0))
  "Return the number in a group of RECORD.

RECORD: a record.
CODE: the group code.
DEFAULT: the value when the record has no such group."
  (pair-real (record-pairs record)
             code
             default))

(defun mirrored-extrusion-p (record)
  "Return true when the entity record is drawn on a plane turned to face the other way, which a
drawing made on the underside of a plane has: its extrusion direction points down.

RECORD: an entity record."
  (let ((z (pair-real (record-pairs record)
                      230
                      1d0)))
    (< z 0d0)))

;;; Entities

(defun bulge-points (x1
                     y1
                     x2
                     y2
                     bulge)
  "Return the points strictly between the two ends of a polyline segment that is an arc, as a flat list
of x and y values: the segment from (X1, Y1) to (X2, Y2) with BULGE, which is the tangent of a quarter
of the angle the arc turns through (positive counter-clockwise), cut into segments of at most
+ARC-STEP-DEGREES+.

X1: x of the start.
Y1: y of the start.
X2: x of the end.
Y2: y of the end.
BULGE: the bulge of the segment."
  (let* ((chord (geo:distance x1
                              y1
                              x2
                              y2))
         (included (* 4 (atan bulge))))
    (when (and (> chord 0d0) (/= 0d0 included))
      (let* ((radius (/ chord (* 2 (sin (/ (abs included) 2)))))
             (apothem (* radius (cos (/ included 2))))
             (middle-x (/ (+ x1 x2) 2))
             (middle-y (/ (+ y1 y2) 2))
             (direction-x (/ (- x2 x1) chord))
             (direction-y (/ (- y2 y1) chord))
             (center-x (- middle-x (* apothem (- direction-y))))
             (center-y (- middle-y (* apothem direction-x)))
             (start-angle (atan (- y1 center-y) (- x1 center-x)))
             (steps (max 1 (ceiling (abs included) (degrees->radians* +arc-step-degrees+))))
             (points '()))
        (loop for step from 1 below steps
              do (let ((angle (+ start-angle (* included (/ step steps)))))
                   (push (+ center-x (* (abs radius) (cos angle))) points)
                   (push (+ center-y (* (abs radius) (sin angle))) points)))
        (nreverse points)))))

(defun polyline-coordinates (vertices
                             closed
                             report)
  "Return the flat list of coordinates of a polyline given as vertices with bulges, the arc segments
cut into straight ones.

VERTICES: a list of (x y bulge) lists.
CLOSED: true when the last vertex joins the first.
REPORT: the report, in which a polyline that had arcs is noted."
  (let ((coordinates '())
        (had-arcs nil)
        (count (length vertices)))
    (loop for index from 0 below count
          for (x y bulge) = (nth index vertices)
          do (push x coordinates)
             (push y coordinates)
             (let ((next (cond ((< (1+ index) count) (nth (1+ index) vertices))
                               (closed (first vertices)))))
               (when (and next bulge (/= 0d0 bulge))
                 (let ((inner (bulge-points x
                                            y
                                            (first next)
                                            (second next)
                                            bulge)))
                   (when inner
                     (setf had-arcs t)
                     (dolist (value inner)
                       (push value coordinates)))))))
    (when had-arcs
      (note report (i18n:tr "Arc segments of polylines were replaced by short straight segments.")))
    (nreverse coordinates)))

(defun lwpolyline-vertices (record)
  "Return the vertices of an LWPOLYLINE record as (x y bulge) lists.

RECORD: an LWPOLYLINE record."
  (let ((vertices '())
        (mirror (mirrored-extrusion-p record)))
    (dolist (pair (record-pairs record))
      (case (car pair)
        (10 (let ((x (parse-real (cdr pair))))
              (when x
                (push (list (if mirror (- x) x) 0d0 0d0) vertices))))
        (20 (let ((y (parse-real (cdr pair))))
              (when (and y vertices)
                (setf (second (first vertices)) y))))
        (42 (let ((bulge (parse-real (cdr pair))))
              (when (and bulge vertices)
                (setf (third (first vertices)) (if mirror (- bulge) bulge)))))))
    (nreverse vertices)))

(defun polyline-vertices (record)
  "Return the vertices of an old-style POLYLINE record as (x y bulge) lists, leaving out the points that
only control a fitted curve.

RECORD: a POLYLINE record with its VERTEX records as children."
  (let ((vertices '())
        (mirror (mirrored-extrusion-p record)))
    (dolist (child (record-children record))
      (when (and (string= "VERTEX" (record-type child))
                 (zerop (logand 16 (pair-integer (record-pairs child)
                                                 70
                                                 0))))
        (let ((x (real-of child 10))
              (y (real-of child 20))
              (bulge (real-of child 42)))
          (push (list (if mirror (- x) x) y (if mirror (- bulge) bulge)) vertices))))
    (nreverse vertices)))

(defun make-polyline-shape (importer
                            layer
                            vertices
                            closed)
  "Emit the polyline of VERTICES, or note that it is not a polyline.

IMPORTER: the importer.
LAYER: the layer name.
VERTICES: a list of (x y bulge) lists.
CLOSED: true when it is closed."
  (when (>= (length vertices) 2)
    (emit-shape importer
                (make-shape :polyline
                            layer
                            (polyline-coordinates vertices
                                                  closed
                                                  (importer-report importer))
                            closed))))

(defun text-origin (x
                    y
                    ax
                    ay
                    height
                    string
                    angle
                    horizontal
                    vertical)
  "Return, as two values, the baseline start of a text that a DXF file gives by an alignment point: the
point is the left, the center or the right of the text, and its baseline, its bottom, its middle or its
top.

X: x of the first point, the insertion point.
Y: y of the first point.
AX: x of the alignment point.
AY: y of the alignment point.
HEIGHT: the text height.
STRING: the text.
ANGLE: the direction of the text in radians.
HORIZONTAL: group 72: 0 left, 1 center, 2 right, 3 aligned, 4 middle, 5 fit.
VERTICAL: group 73: 0 baseline, 1 bottom, 2 middle, 3 top."
  (if (and (zerop horizontal) (zerop vertical))
      (values x y)
      (let* ((width (geo:text-width (length string) height))
             (back (case horizontal
                     ((1 4) (/ width 2))
                     (2 width)
                     (t 0d0)))
             (down (case vertical
                     (1 (* geo:+text-descent-ratio+ height))
                     (2 (* 0.35d0 height))
                     (3 (* +dxf-text-ascent-ratio+ height))
                     (t 0d0)))
             (cosine (cos angle))
             (sine (sin angle)))
        (values (- ax (* back cosine) (- (* (- down) sine)))
                (- ay (* back sine) (* down cosine))))))

(defun import-text (importer
                    record
                    layer)
  "Emit the shape of a TEXT record.

IMPORTER: the importer.
RECORD: a TEXT record.
LAYER: the layer name."
  (let* ((pairs (record-pairs record))
         (string (clean-text (or (pair-value pairs 1) "")))
         (height (real-of record 40))
         (angle (degrees->radians* (real-of record 50)))
         (horizontal (pair-integer pairs
                                   72
                                   0))
         (vertical (pair-integer pairs
                                 73
                                 0))
         (aligned (and (pair-value pairs 11) (or (plusp horizontal) (plusp vertical)))))
    (when (and (plusp (length string)) (plusp height))
      (multiple-value-bind (x y)
          (if aligned
              (text-origin (real-of record 10)
                           (real-of record 20)
                           (real-of record 11)
                           (real-of record 21)
                           height
                           string
                           angle
                           horizontal
                           vertical)
              (values (real-of record 10) (real-of record 20)))
        (emit-shape importer
                    (make-shape :text
                                layer
                                x
                                y
                                height
                                (if (> (length string) dom:+maximum-text-length+)
                                    (subseq string 0 dom:+maximum-text-length+)
                                    string)
                                angle))))))

(defun import-mtext (importer
                     record
                     layer)
  "Emit the shapes of an MTEXT record: one text for each of its lines.

IMPORTER: the importer.
RECORD: an MTEXT record.
LAYER: the layer name."
  (let* ((pairs (record-pairs record))
         (chunks (pair-values pairs 3))
         (string (apply #'concatenate 'string (append chunks (list (or (pair-value pairs 1) "")))))
         (height (real-of record 40))
         (angle (if (pair-value pairs 11)
                    (atan (real-of record 21) (real-of record 11))
                    (degrees->radians* (real-of record 50))))
         (attachment (pair-integer pairs
                                   71
                                   1))
         (lines (remove "" (mapcar #'clean-text (mtext-lines string)) :test #'string=))
         (cosine (cos angle))
         (sine (sin angle))
         (horizontal (mod (1- attachment) 3))
         (top (real-of record 20))
         (left (real-of record 10)))
    (note (importer-report importer) (i18n:tr "Multiline texts were imported as one text for each line."))
    (when (and lines (plusp height))
      (loop for line in lines
            for index from 0
            do (let* ((width (geo:text-width (length line) height))
                      (back (case horizontal (1 (/ width 2)) (2 width) (t 0d0)))
                      (down (+ (* +dxf-text-ascent-ratio+ height) (* index +mtext-line-spacing-ratio+ height))))
                 (emit-shape importer
                             (make-shape :text
                                         layer
                                         (- left (* back cosine) (* (- down) sine))
                                         (- top (* back sine) (* down cosine))
                                         height
                                         (if (> (length line) dom:+maximum-text-length+)
                                             (subseq line 0 dom:+maximum-text-length+)
                                             line)
                                         angle)))))))

(defun ellipse-parameter-points (center-x
                                 center-y
                                 radius-x
                                 radius-y
                                 rotation
                                 start
                                 end)
  "Return the flat coordinates of the points of an elliptical arc, from parameter START to END.

CENTER-X: x of the center.
CENTER-Y: y of the center.
RADIUS-X: the semi-axis along ROTATION.
RADIUS-Y: the other semi-axis.
ROTATION: direction of the first axis in radians.
START: the parameter the arc starts at, in radians.
END: the parameter it ends at."
  (let* ((sweep (let ((turn (- end start)))
                  (if (<= turn 0d0) (+ turn geo:+two-pi+) turn)))
         (steps (max 2 (ceiling sweep (degrees->radians* +arc-step-degrees+))))
         (cosine (cos rotation))
         (sine (sin rotation))
         (points '()))
    (loop for step from 0 to steps
          do (let* ((parameter (+ start (* sweep (/ step steps))))
                    (u (* radius-x (cos parameter)))
                    (v (* radius-y (sin parameter))))
               (push (+ center-x (* u cosine) (- (* v sine))) points)
               (push (+ center-y (* u sine) (* v cosine)) points)))
    (nreverse points)))

(defun import-ellipse (importer
                       record
                       layer)
  "Emit the shape of an ELLIPSE record: an ellipse when it is whole, and a polyline along it otherwise.

IMPORTER: the importer.
RECORD: an ELLIPSE record.
LAYER: the layer name."
  (let* ((major-x (real-of record 11))
         (major-y (real-of record 21))
         (radius-x (sqrt (+ (* major-x major-x) (* major-y major-y))))
         (radius-y (* radius-x (real-of record
                                        40
                                        1d0)))
         (rotation (atan major-y major-x))
         (start (real-of record
                         41
                         0d0))
         (end (real-of record
                       42
                       geo:+two-pi+)))
    (cond ((or (<= radius-x 0d0) (<= radius-y 0d0))
           (ignore-record importer record))
          ((< (abs (- (- end start) geo:+two-pi+)) 1d-9)
           (emit-shape importer
                       (make-shape :ellipse
                                   layer
                                   (real-of record 10)
                                   (real-of record 20)
                                   radius-x
                                   radius-y
                                   rotation)))
          (t
           (note (importer-report importer) (i18n:tr "Elliptical arcs were imported as polylines."))
           (emit-shape importer
                       (make-shape :polyline
                                   layer
                                   (ellipse-parameter-points (real-of record 10)
                                                             (real-of record 20)
                                                             radius-x
                                                             radius-y
                                                             rotation
                                                             start
                                                             end)
                                   nil))))))

(defun import-record (importer
                      record
                      layer-inherited)
  "Make the shapes of one entity record, or count it as left out.

IMPORTER: the importer.
RECORD: an entity record.
LAYER-INHERITED: the layer of the INSERT being expanded, or NIL."
  (let ((type (record-type record))
        (layer (layer-name-of record layer-inherited))
        (pairs (record-pairs record)))
    (cond ((string= type "LINE")
           (emit-shape importer
                       (make-shape :line
                                   layer
                                   (real-of record 10)
                                   (real-of record 20)
                                   (real-of record 11)
                                   (real-of record 21))))
          ((string= type "CIRCLE")
           (let ((x (real-of record 10)))
             (emit-shape importer
                         (make-shape :circle
                                     layer
                                     (if (mirrored-extrusion-p record) (- x) x)
                                     (real-of record 20)
                                     (real-of record 40)))))
          ((string= type "ARC")
           (let* ((mirror (mirrored-extrusion-p record))
                  (start (degrees->radians* (real-of record 50)))
                  (end (degrees->radians* (real-of record 51)))
                  (x (real-of record 10)))
             (emit-shape importer
                         (make-shape :arc
                                     layer
                                     (if mirror (- x) x)
                                     (real-of record 20)
                                     (real-of record 40)
                                     (if mirror (- (float pi 1d0) end) start)
                                     (if mirror (- (float pi 1d0) start) end)))))
          ((string= type "LWPOLYLINE")
           (make-polyline-shape importer
                                layer
                                (lwpolyline-vertices record)
                                (logtest 1 (pair-integer pairs
                                                         70
                                                         0))))
          ((string= type "POLYLINE")
           (if (zerop (logand (+ 16 64) (pair-integer pairs
                                                      70
                                                      0)))
               (make-polyline-shape importer
                                    layer
                                    (polyline-vertices record)
                                    (logtest 1 (pair-integer pairs
                                                             70
                                                             0)))
               (ignore-record importer record)))
          ((string= type "TEXT")
           (import-text importer
                        record
                        layer))
          ((string= type "MTEXT")
           (import-mtext importer
                         record
                         layer))
          ((string= type "ELLIPSE")
           (import-ellipse importer
                           record
                           layer))
          ((string= type "INSERT")
           (import-insert importer
                          record
                          layer))
          ((string= type "DIMENSION")
           (import-dimension importer
                             record
                             layer))
          ((string= type "SOLID")
           (import-solid importer
                         record
                         layer))
          (t
           (ignore-record importer record)))))

;;; Blocks

(defun find-block (document
                   name)
  "Return the block with a name, or NIL.

DOCUMENT: the DXF-DOCUMENT.
NAME: the name of the block."
  (find name (dxf-document-blocks document) :key #'dxf-block-name :test #'string-equal))

(defun insert-array-size (record)
  "Return, as two values, how many columns and rows an INSERT is repeated in.

RECORD: an INSERT record."
  (values (max 1 (pair-integer (record-pairs record)
                               70
                               1))
          (max 1 (pair-integer (record-pairs record)
                               71
                               1))))

(defun insert-scales (record)
  "Return, as two values, the scale of an INSERT along x and along y; a missing y scale is the x scale.

RECORD: an INSERT record."
  (let ((scale-x (real-of record
                          41
                          1d0)))
    (values scale-x
            (real-of record
                     42
                     scale-x))))

(defun import-insert (importer
                      record
                      layer)
  "Expand an INSERT record: the shapes of its block, in as many places as it is repeated.

IMPORTER: the importer.
RECORD: an INSERT record.
LAYER: the layer of the insert."
  (let* ((pairs (record-pairs record))
         (block (find-block (importer-document importer) (or (pair-value pairs 2) ""))))
    (cond ((null block)
           (ignore-record importer record))
          ((>= (importer-block-depth importer) +maximum-block-depth+)
           (note (importer-report importer) (i18n:tr "Blocks inside blocks were followed only so far."))
           (ignore-record importer record))
          (t
           (multiple-value-bind (columns rows)
               (insert-array-size record)
           (multiple-value-bind (scale-x scale-y)
               (insert-scales record)
             (incf (importer-block-depth importer))
             (dotimes (row rows)
               (dotimes (column columns)
                 (let ((transform (insert-transform (+ (real-of record 10) (* column (real-of record 44)))
                                                    (+ (real-of record 20) (* row (real-of record 45)))
                                                    scale-x
                                                    scale-y
                                                    (real-of record 50)
                                                    (dxf-block-base-x block)
                                                    (dxf-block-base-y block)))
                       (before (importer-shapes importer)))
                   (dolist (member (dxf-block-records block))
                     (import-record importer
                                    member
                                    layer))
                   ;; The shapes the block made are in its own coordinates: carry them to where it sits.
                   (loop for cell on (importer-shapes importer)
                         until (eq cell before)
                         do (setf (car cell) (transform-shape (car cell)
                                                              transform
                                                              (importer-report importer)))))))
             (decf (importer-block-depth importer))))))))

(defun import-dimension (importer
                         record
                         layer)
  "Expand a DIMENSION record into the drawing that its anonymous block holds: its lines, its arrowheads
and its text, which is how a DXF file stores what a dimension looks like.

IMPORTER: the importer.
RECORD: a DIMENSION record.
LAYER: the layer name."
  (let ((block (find-block (importer-document importer) (or (pair-value (record-pairs record) 2) ""))))
    (cond ((null block)
           (ignore-record importer record))
          (t
           (note (importer-report importer) (i18n:tr "Dimensions were imported as their lines, arrowheads and text."))
           (dolist (member (dxf-block-records block))
             (import-record importer
                            member
                            layer))))))

(defun import-solid (importer
                     record
                     layer)
  "Emit the outline of a SOLID record, a filled triangle or quadrilateral, as a closed polyline: the
program has no filled shapes.

IMPORTER: the importer.
RECORD: a SOLID record.
LAYER: the layer name."
  (let* ((corners (loop for (x-code y-code) in '((10 20) (11 21) (12 22) (13 23))
                        collect (list (real-of record x-code) (real-of record y-code) 0d0)))
         ;; The fourth corner comes before the third in a quadrilateral.
         (ordered (list (first corners) (second corners) (fourth corners) (third corners)))
         (distinct (remove-duplicates ordered :test #'equal :from-end t)))
    (note (importer-report importer) (i18n:tr "Filled shapes were imported as their outlines."))
    (make-polyline-shape importer
                         layer
                         distinct
                         t)))

(defun transform-points (transform
                         coordinates)
  "Return a flat list of x and y values carried by a transform.

TRANSFORM: an affine.
COORDINATES: a flat list of x and y values."
  (loop for (x y) on coordinates by #'cddr
        append (multiple-value-list (affine-apply transform
                                                  x
                                                  y))))

(defun sampled-circle (center-x
                       center-y
                       radius
                       start
                       sweep
                       closed)
  "Return the flat coordinates of points along a circle or an arc of it, close enough that no segment
spans more than +ARC-STEP-DEGREES+, for the shapes that a stretching transform has spoilt.

CENTER-X: x of the center.
CENTER-Y: y of the center.
RADIUS: the radius.
START: the start angle in radians.
SWEEP: how far it turns.
CLOSED: true for a whole circle, whose last point is not repeated."
  (let* ((steps (max 3 (ceiling sweep (degrees->radians* +arc-step-degrees+))))
         (points '()))
    (loop for step from 0 to (if closed (1- steps) steps)
          do (let ((angle (+ start (* sweep (/ step steps)))))
               (push (+ center-x (* radius (cos angle))) points)
               (push (+ center-y (* radius (sin angle))) points)))
    (nreverse points)))

(defun transform-shape (shape
                        transform
                        report)
  "Return a shape carried by a transform: a circle, an arc or a text stays what it is when the transform
is a rotation or a reflection and a uniform scaling, and a circle or an arc becomes a polyline when it
is not.

SHAPE: a shape in the coordinates of a block.
TRANSFORM: the transform of the INSERT.
REPORT: the report, in which a spoilt shape is noted."
  (let ((data (shape-data shape))
        (layer (shape-layer shape))
        (scale (affine-similarity-scale transform))
        (flipped (< (affine-determinant transform) 0d0)))
    (ecase (shape-kind shape)
      (:line
       (destructuring-bind (x1 y1 x2 y2) data
         (multiple-value-bind (tx1 ty1) (affine-apply transform
                                                      x1
                                                      y1)
           (multiple-value-bind (tx2 ty2) (affine-apply transform
                                                        x2
                                                        y2)
             (make-shape :line
                         layer
                         tx1
                         ty1
                         tx2
                         ty2)))))
      (:polyline
       (make-shape :polyline
                   layer
                   (transform-points transform (first data))
                   (second data)))
      (:circle
       (destructuring-bind (x y radius) data
         (multiple-value-bind (cx cy) (affine-apply transform
                                                    x
                                                    y)
           (if scale
               (make-shape :circle
                           layer
                           cx
                           cy
                           (* radius scale))
               (progn (note report (i18n:tr "Circles and arcs in stretched blocks were replaced by polylines."))
                      (make-shape :polyline
                                  layer
                                  (transform-points transform (sampled-circle x
                                                                              y
                                                                              radius
                                                                              0d0
                                                                              geo:+two-pi+
                                                                              t))
                                  t))))))
      (:arc
       (destructuring-bind (x y radius start end) data
         (multiple-value-bind (cx cy) (affine-apply transform
                                                    x
                                                    y)
           (if scale
               (let ((turn (affine-rotation transform)))
                 (if flipped
                     (make-shape :arc
                                 layer
                                 cx
                                 cy
                                 (* radius scale)
                                 (- turn end)
                                 (- turn start))
                     (make-shape :arc
                                 layer
                                 cx
                                 cy
                                 (* radius scale)
                                 (+ start turn)
                                 (+ end turn))))
               (progn (note report (i18n:tr "Circles and arcs in stretched blocks were replaced by polylines."))
                      (make-shape :polyline
                                  layer
                                  (transform-points transform
                                                    (sampled-circle x
                                                                    y
                                                                    radius
                                                                    start
                                                                    (let ((sweep (- end start)))
                                                                      (if (<= sweep 0d0) (+ sweep geo:+two-pi+) sweep))
                                                                    nil))
                                  nil))))))
      (:ellipse
       (destructuring-bind (x y radius-x radius-y rotation) data
         (multiple-value-bind (cx cy) (affine-apply transform
                                                    x
                                                    y)
           (if scale
               (make-shape :ellipse
                           layer
                           cx
                           cy
                           (* radius-x scale)
                           (* radius-y scale)
                           (if flipped
                               (- (affine-rotation transform) rotation)
                               (+ rotation (affine-rotation transform))))
               (make-shape :polyline
                           layer
                           (transform-points transform
                                             (ellipse-parameter-points x
                                                                       y
                                                                       radius-x
                                                                       radius-y
                                                                       rotation
                                                                       0d0
                                                                       geo:+two-pi+))
                           nil)))))
      (:text
       (destructuring-bind (x y height string angle) data
         (multiple-value-bind (tx ty) (affine-apply transform
                                                    x
                                                    y)
           (make-shape :text
                       layer
                       tx
                       ty
                       (* height (or scale (sqrt (abs (affine-determinant transform)))))
                       string
                       (+ angle (affine-rotation transform)))))))))

;;; Layers

(defparameter *layer-name-replacement* #\_
  "What replaces a character that a layer name may not have.")

(defun clean-layer-name (name)
  "Return a name as the program accepts it for a layer: without control characters or the blanks at its
ends, and not longer than a layer name may be.

NAME: the name of a layer in a DXF file."
  (let ((cleaned (string-trim '(#\Space #\Tab)
                              (substitute-if *layer-name-replacement*
                                             (lambda (character) (< (char-code character) 32))
                                             name))))
    (cond ((zerop (length cleaned)) "0")
          ((> (length cleaned) dom:+maximum-layer-name-length+) (subseq cleaned 0 dom:+maximum-layer-name-length+))
          (t cleaned))))

(defun unique-layer-name (drawing
                          name)
  "Return NAME, or NAME with a number after it, so that no layer of the drawing has it.

DRAWING: the drawing.
NAME: a layer name."
  (if (null (dom:find-layer-by-name drawing name))
      name
      (loop for number from 2
            for candidate = (format nil "~a (~d)" name number)
            when (null (dom:find-layer-by-name drawing candidate))
              return candidate)))

(defun layer-id-for (drawing
                     names
                     name)
  "Return the id of the layer called NAME in the drawing, making the layer when there is none.

DRAWING: the drawing.
NAMES: hash table from the names in the file to the ids of the layers made for them.
NAME: the name in the file."
  (or (gethash name names)
      (setf (gethash name names)
            (let ((cleaned (clean-layer-name name)))
              (if (string= cleaned "0")
                  dom:+default-layer-id+
                  (dom:layer-id (dom:add-layer drawing
                                               (unique-layer-name drawing cleaned)
                                               (dom:default-layer-color))))))))

(defun import-layers (drawing
                      document
                      names)
  "Make the layers of the layer table of DOCUMENT in the drawing: their names, their colors, whether
they are shown and whether they are locked.

DRAWING: the drawing.
DOCUMENT: the DXF-DOCUMENT.
NAMES: hash table from the names in the file to the ids of the layers, filled in."
  (dolist (record (dxf-document-layers document))
    (let* ((pairs (record-pairs record))
           (name (or (pair-value pairs 2) "0"))
           (id (layer-id-for drawing
                             names
                             name))
           (index (pair-integer pairs
                                62
                                7))
           (flags (pair-integer pairs
                                70
                                0))
           (color (aci->rgb index)))
      (when color
        (dom:recolor-layer drawing
                           id
                           color))
      (when (or (minusp index) (logtest 1 flags))
        (dom:set-layer-visible drawing
                               id
                               nil))
      (when (logtest 4 flags)
        (dom:set-layer-locked drawing
                              id
                              t)))))

;;; Extents

(defun shape-points (shape)
  "Return a few points that bound a shape: (x . y) conses, enough to know how far from the origin it
reaches.

SHAPE: a shape."
  (let ((data (shape-data shape)))
    (ecase (shape-kind shape)
      (:line (destructuring-bind (x1 y1 x2 y2) data
               (list (cons x1 y1) (cons x2 y2))))
      ((:circle :arc) (destructuring-bind (x y radius &rest ignored) data
                        (declare (ignore ignored))
                        (list (cons (- x radius) (- y radius)) (cons (+ x radius) (+ y radius)))))
      (:ellipse (destructuring-bind (x y radius-x radius-y rotation) data
                  (declare (ignore rotation))
                  (let ((reach (max radius-x radius-y)))
                    (list (cons (- x reach) (- y reach)) (cons (+ x reach) (+ y reach))))))
      (:polyline (loop for (x y) on (first data) by #'cddr
                       collect (cons x y)))
      (:text (destructuring-bind (x y &rest ignored) data
               (declare (ignore ignored))
               (list (cons x y)))))))

(defun extent-shift (shapes
                     scale)
  "Return, as two values, how far to move every shape, in millimetres, so that the drawing lies within
the coordinates the program accepts: nothing when it already does, and otherwise enough to put the
middle of its extent at the origin.

SHAPES: the shapes, in the units of the file.
SCALE: how many millimetres a unit of the file is."
  (let ((min-x most-positive-double-float)
        (min-y most-positive-double-float)
        (max-x most-negative-double-float)
        (max-y most-negative-double-float))
    (dolist (shape shapes)
      (dolist (point (shape-points shape))
        (let ((x (* scale (car point)))
              (y (* scale (cdr point))))
          (setf min-x (min min-x x)
                min-y (min min-y y)
                max-x (max max-x x)
                max-y (max max-y y)))))
    (if (and (<= min-x max-x)
             (> (max (abs min-x) (abs max-x) (abs min-y) (abs max-y)) base:+maximum-coordinate-magnitude+))
        (values (- (/ (+ min-x max-x) 2)) (- (/ (+ min-y max-y) 2)))
        (values 0d0 0d0))))

;;; Building the entities

(defun build-entity (shape
                     id
                     layer-id
                     scale
                     shift-x
                     shift-y)
  "Make the entity of a shape, in millimetres.

SHAPE: the shape.
ID: the id the entity gets.
LAYER-ID: the id of its layer.
SCALE: how many millimetres a unit of the file is.
SHIFT-X: millimetres to move it along x.
SHIFT-Y: millimetres to move it along y."
  (let ((data (shape-data shape)))
    (flet ((x (value) (+ shift-x (* scale value)))
           (y (value) (+ shift-y (* scale value)))
           (scaled (value) (* scale value)))
      (ecase (shape-kind shape)
        (:line
         (destructuring-bind (x1 y1 x2 y2) data
           (dom:make-line :id id
                          :layer-id layer-id
                          :x1 (x x1)
                          :y1 (y y1)
                          :x2 (x x2)
                          :y2 (y y2))))
        (:circle
         (destructuring-bind (cx cy radius) data
           (dom:make-circle :id id
                            :layer-id layer-id
                            :center-x (x cx)
                            :center-y (y cy)
                            :radius (scaled radius))))
        (:arc
         (destructuring-bind (cx cy radius start end) data
           (dom:make-arc :id id
                         :layer-id layer-id
                         :center-x (x cx)
                         :center-y (y cy)
                         :radius (scaled radius)
                         :start-angle start
                         :end-angle end)))
        (:ellipse
         (destructuring-bind (cx cy radius-x radius-y rotation) data
           (dom:make-ellipse :id id
                             :layer-id layer-id
                             :center-x (x cx)
                             :center-y (y cy)
                             :radius-x (scaled radius-x)
                             :radius-y (scaled radius-y)
                             :rotation rotation)))
        (:polyline
         (let ((coordinates (make-array (length (first data)) :element-type 'double-float)))
           (loop for (px py) on (first data) by #'cddr
                 for index from 0 by 2
                 do (setf (aref coordinates index) (x px)
                          (aref coordinates (1+ index)) (y py)))
           (dom:make-polyline :id id
                              :layer-id layer-id
                              :coordinates (remove-consecutive-duplicates coordinates)
                              :closed (second data))))
        (:text
         (destructuring-bind (tx ty height string angle) data
           (dom:make-text :id id
                          :layer-id layer-id
                          :x (x tx)
                          :y (y ty)
                          :string string
                          :height (scaled height)
                          :angle angle)))))))

(defun remove-consecutive-duplicates (coordinates)
  "Return the coordinates of a polyline without the vertices that repeat the one before them, which the
program does not accept in a polyline.

COORDINATES: a vector of alternating x and y values."
  (let ((kept '()))
    (loop for index from 0 below (length coordinates) by 2
          for x = (aref coordinates index)
          for y = (aref coordinates (1+ index))
          do (unless (and kept
                          (not (base:positive-length-p (geo:distance (second kept)
                                                                     (first kept)
                                                                     x
                                                                     y))))
               (push x kept)
               (push y kept)))
    (let ((result (make-array (length kept) :element-type 'double-float)))
      (loop for value in (reverse kept)
            for index from 0
            do (setf (aref result index) value))
      result)))

(defun resolved-units (document
                       units)
  "Return the unit to read the file in: UNITS when it is given, the unit the file says otherwise, and
millimetres when it says nothing.

DOCUMENT: the DXF-DOCUMENT.
UNITS: a unit keyword, or NIL to take the file's own."
  (or units (document-units document) :millimeter))

(defun display-unit-for (units)
  "Return the unit the imported drawing shows lengths in: the unit of the file when the program has it, and
millimetres otherwise.

UNITS: the unit the file is read in."
  (if (member units '(:millimeter :centimeter :meter))
      units
      :millimeter))

(defun import-model-space (importer)
  "Import the entities of the entities section that are in model space, which is where a drawing is;
those in a layout, which is paper space and has the same drawing again in a frame, are counted and left out.

IMPORTER: the importer."
  (let ((paper-space 0))
    (dolist (record (dxf-document-entities (importer-document importer)))
      (if (= 1 (pair-integer (record-pairs record)
                             67
                             0))
          (incf paper-space)
          (import-record importer
                         record
                         nil)))
    (when (plusp paper-space)
      (note (importer-report importer)
            (i18n:trf "~d ~:*~[entities~;entity~:;entities~] in paper space layouts were left out." paper-space)))))

(defun import-document (document
                        &key units)
  "Make a drawing from a DXF-DOCUMENT. Returns two values: the drawing and the DXF-REPORT that says how
many entities of each kind were made, which DXF types were left out, and what was approximated.

DOCUMENT: the DXF-DOCUMENT.
UNITS: the unit of the numbers in the file as :millimeter, :centimeter, :meter, :inch or :foot, or NIL to
take the one the file says, and millimetres when it says none."
  (let* ((report (make-dxf-report))
         (drawing (dom:make-drawing))
         (names (make-hash-table :test 'equal))
         (importer (make-importer document report))
         (unit (resolved-units document units))
         (scale (cdr (assoc unit *unit-millimeters*))))
    (import-layers drawing
                   document
                   names)
    (import-model-space importer)
    (let ((shapes (reverse (importer-shapes importer))))
      (multiple-value-bind (shift-x shift-y)
          (extent-shift shapes scale)
        (when (or (/= 0d0 shift-x) (/= 0d0 shift-y))
          (note report
                (i18n:trf "The drawing was far from the origin, so it was moved by ~a mm along x and ~a mm along y."
                          (base:format-decimal shift-x 0)
                          (base:format-decimal shift-y 0))))
        (let ((entities '())
              (next-id 1))
          (dolist (shape shapes)
            (handler-case
                (let ((entity (build-entity shape
                                            next-id
                                            (layer-id-for drawing
                                                          names
                                                          (shape-layer shape))
                                            scale
                                            shift-x
                                            shift-y)))
                  (push entity entities)
                  (incf next-id)
                  (count-up (dxf-report-imported report) (dom:entity-kind entity)))
              (base:invalid-entity (condition)
                (count-up (dxf-report-ignored report) (string-upcase (symbol-name (shape-kind shape))))
                (let ((message (princ-to-string condition)))
                  (when (and (< (length (dxf-report-problems report)) +maximum-reported-problems+)
                             (not (member message (dxf-report-problems report) :test #'string=)))
                    (push message (dxf-report-problems report)))))))
          (dom:apply-changes drawing (dom:create-entities-changes (nreverse entities)))
          (dom:restore-drawing-state drawing
                                     :current-layer-id (dom:drawing-current-layer-id drawing)
                                     :next-entity-id next-id
                                     :next-layer-id (dom:drawing-next-layer-id drawing)
                                     :display-unit (display-unit-for unit)
                                     :grid-spacing (dom:drawing-grid-spacing drawing)
                                     :grid-visible (dom:drawing-grid-visible drawing)
                                     :snap-to-grid (dom:drawing-snap-to-grid drawing)
                                     :object-snap (dom:drawing-object-snap drawing)
                                     :default-text-height (dom:drawing-default-text-height drawing)))))
    (values drawing report)))
