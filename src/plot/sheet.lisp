(in-package #:borba-cad/plot)

;;;; Putting the sheets of a drawing on paper: each sheet is a page, with a frame, a title block along the bottom of it,
;;;; and its viewports, each a window on the drawing at a scale of its own, cut to its rectangle. What comes out is a
;;;; plot plan for each page, in the sheet's own millimetres, which the PDF writer and the screen draw alike.

(defconstant +frame-margin+ 10d0
  "How far the frame of a sheet is from the edge of the paper, in millimetres.")

(defconstant +title-block-height+ 27d0
  "How tall the title block along the bottom of the frame is, in millimetres.")

(defconstant +frame-line-width+ 0.7d0
  "How thick the frame of a sheet is drawn, in millimetres.")

(defconstant +title-line-width+ 0.35d0
  "How thick the lines of the title block and the borders of the viewports are drawn, in millimetres.")

(defconstant +title-label-height+ 2d0
  "The height of the small name that each cell of the title block has, in millimetres.")

(defconstant +title-text-inset+ 1.5d0
  "How far the text of a cell of the title block is from its borders, in millimetres.")

(defconstant +character-width-ratio+ 0.6d0
  "How wide a character of the plot font is, in the height of the text.")

(defconstant +project-column-share+ 0.36d0
  "The share of the width of the frame that the project cell of the title block takes.")

(defconstant +title-column-share+ 0.34d0
  "The share of the width of the frame that the title cell of the title block takes; what is left goes to the cells of
the author, the date, the revision, the scale, the sheet and the paper.")

(defconstant +project-text-height+ 4d0
  "The height of the text of the project, in millimetres.")

(defconstant +title-text-height+ 5d0
  "The height of the text of the title of the sheet, in millimetres.")

(defconstant +small-text-height+ 3.5d0
  "The height of the text of the other cells of the title block, in millimetres.")

(defconstant +default-viewport-inset+ 5d0
  "How far inside the area for viewports a viewport that is made to fill it is kept from its edges, in millimetres.")

(defun sheet-size (sheet)
  "Return the width and the height of a sheet as it lies on the paper, in millimetres, as two values.

SHEET: the sheet."
  (multiple-value-bind (short long) (paper-size (dom:sheet-paper sheet))
    (if (dom:sheet-landscape sheet)
        (values long short)
        (values short long))))

(defun sheet-frame (sheet)
  "Return the rectangle inside the margin of a sheet, where its frame is drawn.

SHEET: the sheet."
  (multiple-value-bind (width height) (sheet-size sheet)
    (geo:make-bounds +frame-margin+
                     +frame-margin+
                     (- width +frame-margin+)
                     (- height +frame-margin+))))

(defun sheet-body (sheet)
  "Return the rectangle inside the frame and above the title block, where the viewports go.

SHEET: the sheet."
  (let ((frame (sheet-frame sheet)))
    (geo:make-bounds (geo:bounds-min-x frame)
                     (+ (geo:bounds-min-y frame) +title-block-height+)
                     (geo:bounds-max-x frame)
                     (geo:bounds-max-y frame))))

(defun rectangle-points (bounds)
  "Return the corners of a rectangle as a list of (x . y) conses, going round counter-clockwise.

BOUNDS: the rectangle."
  (list (cons (geo:bounds-min-x bounds) (geo:bounds-min-y bounds))
        (cons (geo:bounds-max-x bounds) (geo:bounds-min-y bounds))
        (cons (geo:bounds-max-x bounds) (geo:bounds-max-y bounds))
        (cons (geo:bounds-min-x bounds) (geo:bounds-max-y bounds))))

(defparameter *black* (list 0d0 0d0 0d0)
  "The color of everything that is not the drawing.")

(defun fitted-text (string
                    width
                    height)
  "Return a text cut to fit a width in the plot font, with three dots at the end when something was cut.

STRING: the text.
WIDTH: the room, in millimetres.
HEIGHT: the height of the text."
  (let ((room (max 0 (floor width (* +character-width-ratio+ height)))))
    (cond ((<= (length string) room) string)
          ((<= room 3) (subseq string 0 room))
          (t (concatenate 'string (subseq string 0 (- room 3)) "...")))))

(defun scale-cell-text (sheet)
  "Return what the scale cell of a title block says: the scale when all the viewports have one, and a word when they
do not, or when there are none.

SHEET: the sheet."
  (let ((scales (remove-duplicates (mapcar #'dom:viewport-scale (dom:sheet-viewports sheet))
                                   :test (lambda (first second) (< (abs (- first second)) 1d-12)))))
    (cond ((null scales) "-")
          ((null (cdr scales)) (scale-text (first scales)))
          (t (i18n:tr "As shown")))))

(defun title-cells (sheet
                    index
                    count)
  "Return the cells of the title block of a sheet, each as a list of x, y, width and height on the sheet, the small
name, what it says and the height of that: the cells of the project and the title of the sheet, and six small ones.

SHEET: the sheet.
INDEX: the position of the sheet, from 0.
COUNT: how many sheets there are."
  (let* ((frame (sheet-frame sheet))
         (x (geo:bounds-min-x frame))
         (y (geo:bounds-min-y frame))
         (width (geo:bounds-width frame))
         (project-width (* width +project-column-share+))
         (title-width (* width +title-column-share+))
         (info-x (+ x project-width title-width))
         (info-width (- width project-width title-width))
         (cell-width (/ info-width 2))
         (row-height (/ +title-block-height+ 3)))
    (flet ((small (column row label text)
             (list (+ info-x (* column cell-width))
                   (+ y (* (- 2 row) row-height))
                   cell-width
                   row-height
                   label
                   text
                   +small-text-height+)))
      (list (list x y project-width +title-block-height+
                  (i18n:tr "Project") (dom:sheet-field sheet :project) +project-text-height+)
            (list (+ x project-width) y title-width +title-block-height+
                  (i18n:tr "Title") (dom:sheet-field sheet :title) +title-text-height+)
            (small 0 0 (i18n:tr "Author") (dom:sheet-field sheet :author))
            (small 1 0 (i18n:tr "Date") (dom:sheet-field sheet :date))
            (small 0 1 (i18n:tr "Revision") (dom:sheet-field sheet :revision))
            (small 1 1 (i18n:tr "Scale") (scale-cell-text sheet))
            (small 0 2 (i18n:tr "Sheet") (format nil "~d/~d" (1+ index) count))
            (small 1 2 (i18n:tr "Paper") (paper-name (dom:sheet-paper sheet)))))))

(defun title-block-items (sheet
                          index
                          count)
  "Return the strokes and texts that draw the frame and the title block of a sheet.

SHEET: the sheet.
INDEX: the position of the sheet, from 0.
COUNT: how many sheets there are."
  (append (list (make-plot-stroke (rectangle-points (sheet-frame sheet))
                                  t
                                  *black*
                                  +frame-line-width+))
          (loop for (x y width height label text text-height) in (title-cells sheet
                                                                              index
                                                                              count)
                append (list (make-plot-stroke (rectangle-points (geo:make-bounds x
                                                                                  y
                                                                                  (+ x width)
                                                                                  (+ y height)))
                                               t
                                               *black*
                                               +title-line-width+)
                             (make-plot-text (fitted-text label
                                                          (- width (* 2 +title-text-inset+))
                                                          +title-label-height+)
                                             (+ x +title-text-inset+)
                                             (- (+ y height) +title-text-inset+ +title-label-height+)
                                             +title-label-height+
                                             0d0
                                             *black*)
                             (make-plot-text (fitted-text text
                                                          (- width (* 2 +title-text-inset+))
                                                          text-height)
                                             (+ x +title-text-inset+)
                                             (+ y +title-text-inset+)
                                             text-height
                                             0d0
                                             *black*)))))

(defun viewport-rectangle (viewport)
  "Return where a viewport is on its sheet, as bounds.

VIEWPORT: the viewport."
  (geo:make-bounds (dom:viewport-x viewport)
                   (dom:viewport-y viewport)
                   (+ (dom:viewport-x viewport) (dom:viewport-width viewport))
                   (+ (dom:viewport-y viewport) (dom:viewport-height viewport))))

(defun viewport-items (drawing
                       entities
                       viewport
                       settings)
  "Return the items that draw a viewport: what of the drawing shows in its window, at its scale and cut to its
rectangle, and its border. Also returns how many entities were drawn.

DRAWING: the drawing.
ENTITIES: the entities that are on layers that are not hidden.
VIEWPORT: the viewport.
SETTINGS: the plot settings."
  (let* ((ratio (dom:viewport-scale viewport))
         (rectangle (viewport-rectangle viewport))
         (window (dom:viewport-window viewport))
         (plan (make-plot-plan settings
                               0d0
                               0d0
                               ratio
                               t
                               (- (+ (dom:viewport-x viewport) (/ (dom:viewport-width viewport) 2))
                                  (* ratio (dom:viewport-center-x viewport)))
                               (- (+ (dom:viewport-y viewport) (/ (dom:viewport-height viewport) 2))
                                  (* ratio (dom:viewport-center-y viewport)))
                               0))
         (shown (remove-if-not (lambda (entity) (geo:bounds-intersect-p (dom:entity-bounds entity) window))
                               entities)))
    (values (append (loop for entity in shown
                          append (loop for item in (entity-items drawing
                                                                 entity
                                                                 plan)
                                       append (clip-item item rectangle)))
                    (list (make-plot-stroke (rectangle-points rectangle)
                                            t
                                            *black*
                                            +title-line-width+)))
            (length shown))))

(defun plan-sheet (drawing
                   sheet
                   index
                   count
                   settings
                   &optional (entities (visible-entities drawing)))
  "Work out what is printed on a sheet: the drawing through each of its viewports, then the frame and the title block.
Returns a plot plan in the millimetres of the sheet.

DRAWING: the drawing.
SHEET: the sheet.
INDEX: the position of the sheet, from 0.
COUNT: how many sheets there are.
SETTINGS: the plot settings, of which the colors and the thickness of lines are used.
ENTITIES: the entities that are on layers that are not hidden."
  (multiple-value-bind (width height) (sheet-size sheet)
    (let ((items '())
          (drawn 0))
      (dolist (viewport (dom:sheet-viewports sheet))
        (multiple-value-bind (viewport-list count-drawn) (viewport-items drawing
                                                                         entities
                                                                         viewport
                                                                         settings)
          (setf items (append items viewport-list))
          (incf drawn count-drawn)))
      (let ((plan (make-plot-plan settings
                                  width
                                  height
                                  1d0
                                  t
                                  0d0
                                  0d0
                                  drawn)))
        (setf (plot-plan-items plan)
              (append items
                      (title-block-items sheet
                                         index
                                         count)))
        plan))))

(defun plan-sheets (drawing
                    settings)
  "Work out every sheet of the drawing, as a list of plot plans in the order of the sheets. Signals EXPORT-REFUSED
when the drawing has no sheets.

DRAWING: the drawing.
SETTINGS: the plot settings."
  (let ((sheets (dom:drawing-sheets drawing)))
    (when (null sheets)
      (error 'base:export-refused
             :reason (i18n:tr "There is nothing to export: the drawing has no sheets.")))
    (let ((entities (visible-entities drawing))
          (count (length sheets)))
      (loop for sheet in sheets
            for index from 0
            collect (plan-sheet drawing
                                sheet
                                index
                                count
                                settings
                                entities)))))

(defun default-viewport (sheet
                         center-x
                         center-y
                         scale)
  "Return a viewport that fills the area of a sheet for viewports, to a margin, showing a point of the drawing at its
center at a scale.

SHEET: the sheet.
CENTER-X: x of the point of the drawing at the center.
CENTER-Y: y of that point.
SCALE: millimetres of paper for a millimetre of the drawing."
  (let ((body (sheet-body sheet)))
    (dom:make-viewport :x (+ (geo:bounds-min-x body) +default-viewport-inset+)
                       :y (+ (geo:bounds-min-y body) +default-viewport-inset+)
                       :width (- (geo:bounds-width body) (* 2 +default-viewport-inset+))
                       :height (- (geo:bounds-height body) (* 2 +default-viewport-inset+))
                       :center-x center-x
                       :center-y center-y
                       :scale scale)))
