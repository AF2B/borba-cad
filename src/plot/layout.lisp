(in-package #:borba-cad/plot)

;;;; Putting a drawing on a sheet. The sheet is measured in millimetres with its origin at the bottom left and y
;;;; going up, like the drawing; the scale is the millimetres of paper for a millimetre of drawing. The drawing is
;;;; flattened into strokes, filled triangles and text whose coordinates are already those of the sheet.

(defparameter *paper-sizes*
  '((:a4 "A4" 210d0 297d0)
    (:a3 "A3" 297d0 420d0)
    (:a2 "A2" 420d0 594d0)
    (:a1 "A1" 594d0 841d0)
    (:a0 "A0" 841d0 1189d0)
    (:letter "Letter" 215.9d0 279.4d0))
  "The sheets, as (keyword name width height) with the width and the height in millimetres, the sheet standing up.")

(defconstant +default-line-width+ 0.25d0
  "How thick lines are drawn, in millimetres of paper.")

(defconstant +default-margin+ 10d0
  "How much of the edge of the sheet is left empty, in millimetres.")

(defconstant +chord-error+ 0.02d0
  "How far a curve may be from the segments that stand for it, in millimetres of paper.")

(defconstant +minimum-circle-segments+ 16
  "Fewest segments a whole circle is drawn with, however small.")

(defconstant +maximum-circle-segments+ 720
  "Most segments a whole circle is drawn with, however large.")

(defconstant +light-color-luminance+ 0.7d0
  "How light a layer color must be to be printed in black, which is to say invisible on white paper.")

(defconstant +maximum-scale-ratio+ 1d6
  "Largest scale, in paper per drawing, and the reciprocal of the smallest, that a typed scale may have.")

(defun paper-size (paper)
  "Return the width and the height of a sheet standing up, in millimetres, as two values.

PAPER: a keyword of *PAPER-SIZES*."
  (let ((entry (assoc paper *paper-sizes*)))
    (unless entry
      (error 'base:invalid-input
             :text (string paper)
             :reason (i18n:tr "that is not a sheet size")))
    (values (third entry) (fourth entry))))

(defun paper-name (paper)
  "Return the name of a sheet, such as \"A4\".

PAPER: a keyword of *PAPER-SIZES*."
  (second (assoc paper *paper-sizes*)))

;;; Settings

(defstruct (plot-settings (:constructor %make-plot-settings (paper
                                                              ratio
                                                              monochrome
                                                              line-width
                                                              margin)))
  "How a drawing is put on a sheet.

PAPER: a keyword of *PAPER-SIZES*.
RATIO: the millimetres of paper for a millimetre of the drawing, or NIL to make it as large as the sheet allows.
MONOCHROME: true to draw everything in black, false to use the colors of the layers.
LINE-WIDTH: the thickness of lines, in millimetres of paper.
MARGIN: the empty edge of the sheet, in millimetres."
  (paper :a4 :type keyword :read-only t)
  (ratio nil :type (or null double-float) :read-only t)
  (monochrome t :read-only t)
  (line-width +default-line-width+ :type double-float :read-only t)
  (margin +default-margin+ :type double-float :read-only t))

(defun make-plot-settings (&key (paper :a4)
                                ratio
                                (monochrome t)
                                (line-width +default-line-width+)
                                (margin +default-margin+))
  "Create validated plot settings. Signals INVALID-INPUT when one makes no sense.

PAPER: a keyword of *PAPER-SIZES*.
RATIO: millimetres of paper for a millimetre of the drawing, above zero, or NIL to fit the drawing to the sheet.
MONOCHROME: true to draw everything in black.
LINE-WIDTH: thickness of lines in millimetres, above zero.
MARGIN: empty edge of the sheet in millimetres, zero or more and less than half of the sheet."
  (multiple-value-bind (width height) (paper-size paper)
    (when (and ratio (not (< (/ +maximum-scale-ratio+) ratio +maximum-scale-ratio+)))
      (error 'base:invalid-input
             :text (princ-to-string ratio)
             :reason (i18n:tr "the scale is out of range")))
    (unless (plusp line-width)
      (error 'base:invalid-input
             :text (princ-to-string line-width)
             :reason (i18n:tr "the line width must be above zero")))
    (unless (and (>= margin 0) (< (* 2 margin) (min width height)))
      (error 'base:invalid-input
             :text (princ-to-string margin)
             :reason (i18n:tr "the margin leaves no room on the sheet"))))
  (%make-plot-settings paper
                       (and ratio (float ratio 1d0))
                       (and monochrome t)
                       (float line-width 1d0)
                       (float margin 1d0)))

(defun parse-plot-scale (text)
  "Return the scale that TEXT names: NIL for \"fit\", or the millimetres of paper for a millimetre of the drawing.
\"1:50\" and \"1/50\" and a plain \"50\" are a fiftieth, \"2:1\" is twice life size. Signals INVALID-INPUT when
TEXT names none.

TEXT: what was typed."
  (let* ((trimmed (string-downcase (string-trim " " text)))
         (separator (position-if (lambda (character) (find character ":/")) trimmed)))
    (flet ((number-of (piece)
             (let ((value (handler-case (base:parse-decimal (string-trim " " piece))
                            (base:invalid-input ()
                              (error 'base:invalid-input
                                     :text text
                                     :reason (i18n:tr "type a scale such as 1:50, or fit"))))))
               (unless (plusp value)
                 (error 'base:invalid-input
                        :text text
                        :reason (i18n:tr "the scale must be above zero")))
               (float value 1d0))))
      (cond ((member trimmed '("fit" "") :test #'string=) nil)
            (separator
             (/ (number-of (subseq trimmed 0 separator))
                (number-of (subseq trimmed (1+ separator)))))
            (t (/ 1d0 (number-of trimmed)))))))

(defun scale-text (ratio)
  "Return a scale as a ratio, such as 1:50, or 2:1 when it enlarges.

RATIO: millimetres of paper for a millimetre of the drawing."
  (flet ((tidy (value)
           (string-right-trim "." (string-right-trim "0" (base:format-decimal value 1)))))
    (if (>= ratio 1d0)
        (format nil "~a:1" (tidy ratio))
        (format nil "1:~a" (tidy (/ 1d0 ratio))))))

;;; Plans

(defstruct (plot-plan (:constructor make-plot-plan (settings
                                                     width
                                                     height
                                                     ratio
                                                     fits
                                                     offset-x
                                                     offset-y
                                                     entity-count)))
  "Where a drawing goes on a sheet and what is drawn there.

SETTINGS: the plot settings.
WIDTH: width of the sheet in millimetres, in the orientation chosen.
HEIGHT: height of the sheet in millimetres.
RATIO: the millimetres of paper for a millimetre of the drawing, as worked out for a fit.
FITS: true when the drawing lies within the margins.
OFFSET-X: sheet x of the origin of the drawing.
OFFSET-Y: sheet y of the origin of the drawing.
ENTITY-COUNT: how many entities are drawn.
ITEMS: the strokes, fills and texts, in drawing order, in sheet coordinates."
  settings
  width
  height
  ratio
  fits
  offset-x
  offset-y
  entity-count
  (items nil))

(defun visible-entities (drawing)
  "Return the entities of the drawing that are on visible layers, in stacking order.

DRAWING: the drawing."
  (let ((entities '()))
    (dom:map-entities (lambda (entity)
                        (let ((layer (dom:find-layer drawing (dom:entity-layer-id entity))))
                          (when (and layer (dom:layer-visible layer))
                            (push entity entities))))
                      (dom:drawing-store drawing))
    (nreverse entities)))

(defun entities-bounds (entities)
  "Return the bounds that hold every entity.

ENTITIES: a non-empty list of entities."
  (reduce #'geo:bounds-union (mapcar #'dom:entity-bounds entities)))

(defun fit-ratio (bounds
                  width
                  height
                  margin)
  "Return the scale at which the drawing just fills the sheet inside the margins.

BOUNDS: the bounds of the drawing.
WIDTH: width of the sheet.
HEIGHT: height of the sheet.
MARGIN: the empty edge."
  (let ((available-x (- width (* 2 margin)))
        (available-y (- height (* 2 margin)))
        (extent-x (geo:bounds-width bounds))
        (extent-y (geo:bounds-height bounds)))
    (cond ((and (plusp extent-x) (plusp extent-y)) (min (/ available-x extent-x) (/ available-y extent-y)))
          ((plusp extent-x) (/ available-x extent-x))
          ((plusp extent-y) (/ available-y extent-y))
          (t 1d0))))

(defun overflow (bounds
                 width
                 height
                 margin
                 ratio)
  "Return how far, in millimetres, the drawing at a scale reaches out of the margins of a sheet.

BOUNDS: the bounds of the drawing.
WIDTH: width of the sheet.
HEIGHT: height of the sheet.
MARGIN: the empty edge.
RATIO: the scale."
  (+ (max 0d0 (- (* ratio (geo:bounds-width bounds)) (- width (* 2 margin))))
     (max 0d0 (- (* ratio (geo:bounds-height bounds)) (- height (* 2 margin))))))

(defun choose-orientation (bounds
                           settings)
  "Return the width, the height and the scale of the sheet turned the way that suits the drawing: the way that
lets it be larger when it is fitted, or that lets it fit, or reach out the least, when the scale is given.

BOUNDS: the bounds of the drawing.
SETTINGS: the plot settings."
  (multiple-value-bind (short long) (paper-size (plot-settings-paper settings))
    (let ((margin (plot-settings-margin settings))
          (given (plot-settings-ratio settings))
          (candidates '()))
      (dolist (orientation (list (cons long short) (cons short long)))
        (let* ((ratio (or given (fit-ratio bounds
                                           (car orientation)
                                           (cdr orientation)
                                           margin)))
               (cost (overflow bounds
                               (car orientation)
                               (cdr orientation)
                               margin
                               ratio)))
          (push (list (car orientation) (cdr orientation) ratio cost) candidates)))
      (setf candidates (nreverse candidates))
      (first (stable-sort candidates
                          (if given
                              #'<
                              #'>)
                          :key (if given
                                   #'fourth
                                   #'third))))))

(defun plan-plot (drawing
                  settings)
  "Work out where the drawing goes on the sheet and flatten what is visible into plot items. Signals EXPORT-REFUSED
when nothing is visible.

DRAWING: the drawing.
SETTINGS: the plot settings."
  (let ((entities (visible-entities drawing)))
    (when (null entities)
      (error 'base:export-refused
             :reason (i18n:tr "There is nothing to export: the drawing has no visible entities.")))
    (let ((bounds (entities-bounds entities)))
      (destructuring-bind (width height ratio cost) (choose-orientation bounds settings)
        (let* ((offset-x (- (/ (- width (* ratio (geo:bounds-width bounds))) 2)
                            (* ratio (geo:bounds-min-x bounds))))
               (offset-y (- (/ (- height (* ratio (geo:bounds-height bounds))) 2)
                            (* ratio (geo:bounds-min-y bounds))))
               (plan (make-plot-plan settings
                                     width
                                     height
                                     ratio
                                     (< cost 1d-6)
                                     offset-x
                                     offset-y
                                     (length entities))))
          (setf (plot-plan-items plan)
                (loop for entity in entities
                      append (entity-items drawing
                                           entity
                                           plan)))
          plan)))))

;;; Flattening

(defun sheet-x (plan
                x)
  "Return the sheet x of a drawing x.

PLAN: the plot plan.
X: x in the drawing."
  (+ (plot-plan-offset-x plan) (* (plot-plan-ratio plan) x)))

(defun sheet-y (plan
                y)
  "Return the sheet y of a drawing y.

PLAN: the plot plan.
Y: y in the drawing."
  (+ (plot-plan-offset-y plan) (* (plot-plan-ratio plan) y)))

(defstruct (plot-stroke (:constructor make-plot-stroke (points
                                                         closed
                                                         color)))
  "A run of segments.

POINTS: the vertices as a list of (x . y) conses in sheet millimetres.
CLOSED: true when the last joins the first.
COLOR: a list of red, green and blue, each from 0 to 1."
  points
  closed
  color)

(defstruct (plot-fill (:constructor make-plot-fill (points
                                                     color)))
  "A filled polygon, in practice an arrowhead.

POINTS: the vertices as a list of (x . y) conses in sheet millimetres.
COLOR: a list of red, green and blue, each from 0 to 1."
  points
  color)

(defstruct (plot-text (:constructor make-plot-text (string
                                                     x
                                                     y
                                                     height
                                                     angle
                                                     color)))
  "A line of text.

STRING: the characters.
X: sheet x of the start of the baseline.
Y: sheet y of the start of the baseline.
HEIGHT: the size of the characters in millimetres.
ANGLE: direction of the baseline, in radians counter-clockwise from the sheet's x.
COLOR: a list of red, green and blue, each from 0 to 1."
  string
  x
  y
  height
  angle
  color)

(defun layer-ink (drawing
                  entity
                  settings)
  "Return the color an entity is drawn in: black, or the color of its layer, unless that is so light that it would
not show on white.

DRAWING: the drawing.
ENTITY: the entity.
SETTINGS: the plot settings."
  (if (plot-settings-monochrome settings)
      (list 0d0 0d0 0d0)
      (let* ((color (dom:layer-color (dom:find-layer drawing (dom:entity-layer-id entity))))
             (red (/ (dom:rgb-red color) 255d0))
             (green (/ (dom:rgb-green color) 255d0))
             (blue (/ (dom:rgb-blue color) 255d0)))
        (if (> (+ (* 0.2126d0 red) (* 0.7152d0 green) (* 0.0722d0 blue)) +light-color-luminance+)
            (list 0d0 0d0 0d0)
            (list red green blue)))))

(defun segments-for (radius
                     sweep)
  "Return how many segments make a curve of a radius, in millimetres of paper, and a sweep look round.

RADIUS: radius on the sheet.
SWEEP: how far it turns, in radians."
  (let* ((whole (if (<= radius +chord-error+)
                    +minimum-circle-segments+
                    (max +minimum-circle-segments+
                         (min +maximum-circle-segments+
                              (ceiling geo:+two-pi+ (* 2 (acos (- 1d0 (/ +chord-error+ radius)))))))))
         (segments (ceiling (* whole sweep) geo:+two-pi+)))
    (max 1 segments)))

(defun arc-points (plan
                   center-x
                   center-y
                   radius
                   start
                   sweep)
  "Return the points of an arc as a list of (x . y) conses in sheet millimetres, enough of them to look round.

PLAN: the plot plan.
CENTER-X: x of the center in the drawing.
CENTER-Y: y of the center.
RADIUS: radius in the drawing.
START: direction of the start, in radians.
SWEEP: how far it turns counter-clockwise."
  (let ((segments (segments-for (* radius (plot-plan-ratio plan)) sweep)))
    (loop for index from 0 to segments
          for angle = (+ start (* sweep (/ index (float segments 1d0))))
          collect (cons (sheet-x plan (+ center-x (* radius (cos angle))))
                        (sheet-y plan (+ center-y (* radius (sin angle))))))))

(defun coordinate-points (plan
                          coordinates)
  "Return a flat array of drawing coordinates as a list of (x . y) conses in sheet millimetres.

PLAN: the plot plan.
COORDINATES: simple-array of double-float holding x0 y0 x1 y1 ..."
  (loop for index from 0 below (length coordinates) by 2
        collect (cons (sheet-x plan (aref coordinates index))
                      (sheet-y plan (aref coordinates (1+ index))))))

(defgeneric entity-items (drawing entity plan)
  (:documentation "Return the plot items that draw ENTITY, in sheet coordinates.

DRAWING: the drawing.
ENTITY: an entity.
PLAN: the plot plan."))

(defun stroke-of (drawing
                  entity
                  plan
                  points
                  closed)
  "Return a one-element list holding the stroke through POINTS in the ink of ENTITY.

DRAWING: the drawing.
ENTITY: the entity.
PLAN: the plot plan.
POINTS: list of (x . y) conses in sheet millimetres.
CLOSED: true when the last joins the first."
  (list (make-plot-stroke points
                          closed
                          (layer-ink drawing
                                     entity
                                     (plot-plan-settings plan)))))

(defmethod entity-items (drawing (line dom:line-entity) plan)
  (stroke-of drawing
             line
             plan
             (list (cons (sheet-x plan (dom:line-x1 line)) (sheet-y plan (dom:line-y1 line)))
                   (cons (sheet-x plan (dom:line-x2 line)) (sheet-y plan (dom:line-y2 line))))
             nil))

(defmethod entity-items (drawing (rectangle dom:rectangle-entity) plan)
  (let* ((x1 (dom:rectangle-min-x rectangle))
         (y1 (dom:rectangle-min-y rectangle))
         (x2 (+ x1 (dom:rectangle-width rectangle)))
         (y2 (+ y1 (dom:rectangle-height rectangle))))
    (stroke-of drawing
               rectangle
               plan
               (list (cons (sheet-x plan x1) (sheet-y plan y1))
                     (cons (sheet-x plan x2) (sheet-y plan y1))
                     (cons (sheet-x plan x2) (sheet-y plan y2))
                     (cons (sheet-x plan x1) (sheet-y plan y2)))
               t)))

(defmethod entity-items (drawing (circle dom:circle-entity) plan)
  (let ((points (arc-points plan
                            (dom:circle-center-x circle)
                            (dom:circle-center-y circle)
                            (dom:circle-radius circle)
                            0d0
                            geo:+two-pi+)))
    (stroke-of drawing
               circle
               plan
               (butlast points)
               t)))

(defmethod entity-items (drawing (arc dom:arc-entity) plan)
  (stroke-of drawing
             arc
             plan
             (arc-points plan
                         (dom:arc-center-x arc)
                         (dom:arc-center-y arc)
                         (dom:arc-radius arc)
                         (dom:arc-start-angle arc)
                         (dom:arc-sweep arc))
             nil))

(defmethod entity-items (drawing (ellipse dom:ellipse-entity) plan)
  (let* ((largest (* (plot-plan-ratio plan) (max (dom:ellipse-radius-x ellipse) (dom:ellipse-radius-y ellipse))))
         (count (max 3 (segments-for largest geo:+two-pi+))))
    (stroke-of drawing
               ellipse
               plan
               (coordinate-points plan
                                  (geo:ellipse-points (dom:ellipse-center-x ellipse)
                                                      (dom:ellipse-center-y ellipse)
                                                      (dom:ellipse-radius-x ellipse)
                                                      (dom:ellipse-radius-y ellipse)
                                                      (dom:ellipse-rotation ellipse)
                                                      count))
               t)))

(defmethod entity-items (drawing (polygon dom:polygon-entity) plan)
  (stroke-of drawing
             polygon
             plan
             (coordinate-points plan (dom:polygon-vertices polygon))
             t))

(defmethod entity-items (drawing (polyline dom:polyline-entity) plan)
  (stroke-of drawing
             polyline
             plan
             (coordinate-points plan (dom:polyline-coordinates polyline))
             (dom:polyline-closed polyline)))

(defmethod entity-items (drawing (text dom:text-entity) plan)
  (list (make-plot-text (dom:text-string text)
                        (sheet-x plan (dom:text-x text))
                        (sheet-y plan (dom:text-y text))
                        (* (plot-plan-ratio plan) (dom:text-height text))
                        (dom:text-angle text)
                        (layer-ink drawing
                                   text
                                   (plot-plan-settings plan)))))

(defmethod entity-items (drawing (hatch dom:hatch-entity) plan)
  (let ((triangles (dom:hatch-triangles hatch))
        (color (layer-ink drawing
                          hatch
                          (plot-plan-settings plan))))
    (append (segment-strokes drawing
                             hatch
                             plan
                             (dom:hatch-segments hatch))
            (loop for index from 0 below (length triangles) by 6
                  collect (make-plot-fill (list (cons (sheet-x plan (aref triangles index))
                                                      (sheet-y plan (aref triangles (+ index 1))))
                                                (cons (sheet-x plan (aref triangles (+ index 2)))
                                                      (sheet-y plan (aref triangles (+ index 3))))
                                                (cons (sheet-x plan (aref triangles (+ index 4)))
                                                      (sheet-y plan (aref triangles (+ index 5)))))
                                          color)))))

(defun segment-strokes (drawing
                        entity
                        plan
                        segments)
  "Return a stroke for each segment of a flat array of x1 y1 x2 y2 groups.

DRAWING: the drawing.
ENTITY: the entity.
PLAN: the plot plan.
SEGMENTS: simple-array of double-float, four numbers for each segment, in drawing coordinates."
  (loop for index from 0 below (length segments) by 4
        append (stroke-of drawing
                          entity
                          plan
                          (list (cons (sheet-x plan (aref segments index)) (sheet-y plan (aref segments (+ index 1))))
                                (cons (sheet-x plan (aref segments (+ index 2)))
                                      (sheet-y plan (aref segments (+ index 3)))))
                          nil)))

(defun arrow-fills (drawing
                    entity
                    plan
                    arrowheads)
  "Return a filled triangle for each three points of a flat array of arrowheads.

DRAWING: the drawing.
ENTITY: the entity.
PLAN: the plot plan.
ARROWHEADS: simple-array of double-float, six numbers for each triangle, in drawing coordinates."
  (loop for index from 0 below (length arrowheads) by 6
        collect (make-plot-fill (list (cons (sheet-x plan (aref arrowheads index))
                                            (sheet-y plan (aref arrowheads (+ index 1))))
                                      (cons (sheet-x plan (aref arrowheads (+ index 2)))
                                            (sheet-y plan (aref arrowheads (+ index 3))))
                                      (cons (sheet-x plan (aref arrowheads (+ index 4)))
                                            (sheet-y plan (aref arrowheads (+ index 5)))))
                                (layer-ink drawing
                                           entity
                                           (plot-plan-settings plan)))))

(defmethod entity-items (drawing (dimension dom:dimension-entity) plan)
  (let* ((layout (dom:dimension-layout dimension))
         (ratio (plot-plan-ratio plan)))
    (append (segment-strokes drawing
                             dimension
                             plan
                             (geo:dimension-layout-extension-lines layout))
            (segment-strokes drawing
                             dimension
                             plan
                             (geo:dimension-layout-dimension-line layout))
            (arrow-fills drawing
                         dimension
                         plan
                         (geo:dimension-layout-arrowheads layout))
            (list (make-plot-text (dom:dimension-text dimension)
                                  (sheet-x plan (geo:dimension-layout-text-x layout))
                                  (sheet-y plan (geo:dimension-layout-text-y layout))
                                  (* ratio (dom:dimension-text-height dimension))
                                  (geo:dimension-layout-text-angle layout)
                                  (layer-ink drawing
                                             dimension
                                             (plot-plan-settings plan)))))))

(defun annotation-items (drawing
                         entity
                         plan
                         layout
                         text)
  "Return the plot items of a radius, diameter or angle dimension.

DRAWING: the drawing.
ENTITY: the dimension entity.
PLAN: the plot plan.
LAYOUT: its annotation layout.
TEXT: the text it shows."
  (append (segment-strokes drawing
                           entity
                           plan
                           (geo:annotation-layout-segments layout))
          (arrow-fills drawing
                       entity
                       plan
                       (geo:annotation-layout-arrowheads layout))
          (list (make-plot-text text
                                (sheet-x plan (geo:annotation-layout-text-x layout))
                                (sheet-y plan (geo:annotation-layout-text-y layout))
                                (* (plot-plan-ratio plan) (geo:annotation-layout-text-height layout))
                                (geo:annotation-layout-text-angle layout)
                                (layer-ink drawing
                                           entity
                                           (plot-plan-settings plan))))))

(defmethod entity-items (drawing (dimension dom:radial-dimension-entity) plan)
  (annotation-items drawing
                    dimension
                    plan
                    (dom:radial-dimension-layout dimension)
                    (dom:radial-dimension-text dimension)))

(defmethod entity-items (drawing (dimension dom:angular-dimension-entity) plan)
  (annotation-items drawing
                    dimension
                    plan
                    (dom:angular-dimension-layout dimension)
                    (dom:angular-dimension-text dimension)))
