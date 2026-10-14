(in-package #:borba-cad/ui)

(defconstant +font-pixels+ 13f0
  "Em size of the user-interface text, in logical pixels.")

(defconstant +small-font-pixels+ 12f0
  "Em size of secondary text, in logical pixels.")

(defconstant +heading-font-pixels+ 15f0
  "Em size of headings, in logical pixels.")

(defconstant +row-height+ 28f0
  "Height of one list row or field, in logical pixels.")

(defconstant +padding+ 12f0
  "Standard gap between a container's edge and its content, in logical pixels.")

(defconstant +gap+ 6f0
  "Standard gap between neighbouring user-interface elements, in logical pixels.")

(defconstant +radius+ 8f0
  "Corner radius of buttons and rows, in logical pixels.")

(defconstant +small-radius+ 6f0
  "Corner radius of fields and chips' inner parts, in logical pixels.")

(defconstant +card-radius+ 12f0
  "Corner radius of cards and dialogs, in logical pixels.")

(defconstant +toolbar-height+ 48f0
  "Height of the title bar, in logical pixels.")

(defconstant +tool-palette-width+ 60f0
  "Width of the column of drawing tools, in logical pixels.")

(defconstant +status-height+ 30f0
  "Height of the status bar, in logical pixels.")

(defconstant +side-panel-width+ 332f0
  "Width of the right-hand panel, in logical pixels.")

(defconstant +panel-margin+ 8f0
  "Gap between the side panel's edge and its cards, and between the cards, in logical pixels.")

(defconstant +icon-size+ 22f0
  "Size of an icon inside a title bar button, in logical pixels.")

(defconstant +small-icon-size+ 16f0
  "Size of an icon that stands beside a label, in logical pixels.")

(defconstant +tool-icon-size+ 24f0
  "Size of an icon inside a button of the tool palette, in logical pixels.")

(defconstant +icon-button-size+ 36f0
  "Side of a square icon button, in logical pixels.")

(defconstant +layers-panel-fraction+ 0.38f0
  "Share of the side panel's height given to the layers panel; the inspector gets the rest.")

(defconstant +chip-height+ 22f0
  "Height of a pill-shaped toggle, in logical pixels.")

(defconstant +chip-padding+ 10f0
  "Space between a chip's edge and its content, in logical pixels.")

(defconstant +tooltip-height+ 28f0
  "Height of a tooltip, in logical pixels.")

(defconstant +fit-tolerance-pixels+ 0.01f0
  "Slack added to the available width when fitting text, so a label sized exactly to its text
is never truncated by floating-point rounding.")

(defparameter *ellipsis* "..."
  "Suffix that marks a truncated label.")

(defun fixed-advance-width (font
                            string
                            size)
  "Measure STRING on the fixed 0.6 em grid, whatever the font. It is the default text measurer,
which keeps layout deterministic where no renderer exists, as in tests.

FONT: the font keyword; ignored.
STRING: the text.
SIZE: em size in pixels."
  (declare (ignore font))
  (* (length string) geo:+text-advance-ratio+ size))

(defvar *text-measurer* #'fixed-advance-width
  "Function of a font keyword (:ui, :ui-strong or :mono), a string and an em size in pixels that
returns the width of the text in pixels. The window program installs one that reads the real
glyph advances; the default measures a fixed 0.6 em per character. A global because layout is
computed far from any renderer and every widget must measure the same way.")

(defstruct (rect (:constructor make-rect (x
                                          y
                                          width
                                          height)))
  "An axis-aligned screen rectangle in logical pixels.

X: left edge.
Y: top edge.
WIDTH: width.
HEIGHT: height."
  (x 0f0 :type single-float :read-only t)
  (y 0f0 :type single-float :read-only t)
  (width 0f0 :type single-float :read-only t)
  (height 0f0 :type single-float :read-only t))

(defun rect-contains-p (rect
                        x
                        y)
  "Return true when the point lies inside RECT.

RECT: a rectangle.
X: point x in pixels.
Y: point y in pixels."
  (and (<= (rect-x rect) x (+ (rect-x rect) (rect-width rect)))
       (<= (rect-y rect) y (+ (rect-y rect) (rect-height rect)))))

(defun text-width-pixels (string
                          &optional (size +font-pixels+)
                                    (font :ui))
  "Return the width of STRING at SIZE in FONT, as a single-float, as measured by the installed
text measurer.

STRING: the text.
SIZE: em size in pixels.
FONT: :ui, :ui-strong or :mono."
  (float (funcall *text-measurer* font string size) 1f0))

(defun fits-p (string
               maximum-width
               size
               font)
  "Return true when STRING is no wider than MAXIMUM-WIDTH, within the fit tolerance.

STRING: the text.
MAXIMUM-WIDTH: available width in pixels.
SIZE: em size in pixels.
FONT: the font keyword."
  (let ((width (text-width-pixels string
                                  size
                                  font)))
    (<= width (+ maximum-width +fit-tolerance-pixels+))))

(defun fitting-prefix-length (string
                              maximum-width
                              size
                              font)
  "Return the number of characters of STRING, from the start, that fit in MAXIMUM-WIDTH.

STRING: the text.
MAXIMUM-WIDTH: available width in pixels.
SIZE: em size in pixels.
FONT: the font keyword."
  (let ((low 0)
        (high (length string)))
    (loop while (< low high)
          do (let ((middle (ceiling (+ low high) 2)))
               (if (fits-p (subseq string 0 middle)
                           maximum-width
                           size
                           font)
                   (setf low middle)
                   (setf high (1- middle)))))
    low))

(defun fit-text (string
                 maximum-width
                 &optional (size +font-pixels+)
                           (font :ui))
  "Return STRING, shortened with an ellipsis when it would be wider than MAXIMUM-WIDTH. When not
even one character and the ellipsis fit, the longest prefix that fits is returned.

STRING: the text.
MAXIMUM-WIDTH: available width in pixels.
SIZE: em size in pixels.
FONT: :ui, :ui-strong or :mono."
  (if (fits-p string
              maximum-width
              size
              font)
      string
      (let* ((ellipsis-width (text-width-pixels *ellipsis*
                                                size
                                                font))
             (kept (fitting-prefix-length string
                                          (- maximum-width ellipsis-width)
                                          size
                                          font)))
        (if (plusp kept)
            (concatenate 'string
                         (subseq string 0 kept)
                         *ellipsis*)
            (subseq string 0 (fitting-prefix-length string
                                                    maximum-width
                                                    size
                                                    font))))))

(defun line-break-position (text
                            maximum-width
                            size
                            font)
  "Return where the first line of TEXT, which is wider than MAXIMUM-WIDTH, ends: at the last
space that fits, or where the first word overflows.

TEXT: the text.
MAXIMUM-WIDTH: width of a line in pixels.
SIZE: em size in pixels.
FONT: the font keyword."
  (let* ((fitting (max 1 (fitting-prefix-length text
                                                maximum-width
                                                size
                                                font)))
         (space (position #\Space text
                          :end (min (length text) (1+ fitting))
                          :from-end t)))
    (if (and space (plusp space))
        space
        fitting)))

(defun wrap-text (string
                  maximum-width
                  maximum-lines
                  &optional (size +font-pixels+)
                            (font :ui))
  "Break STRING into lines no wider than MAXIMUM-WIDTH, at spaces where there are any. A word
wider than a line is cut where it overflows. When the text needs more than MAXIMUM-LINES lines,
the last one is truncated with an ellipsis.

STRING: the text.
MAXIMUM-WIDTH: width of a line in pixels.
MAXIMUM-LINES: most lines returned, at least 1.
SIZE: em size in pixels.
FONT: :ui, :ui-strong or :mono."
  (let ((limit (max 1 maximum-lines))
        (lines '())
        (remaining string))
    (loop while (and (plusp (length remaining)) (< (length lines) limit))
          do (cond ((fits-p remaining
                            maximum-width
                            size
                            font)
                    (push remaining lines)
                    (setf remaining ""))
                   ((= (length lines) (1- limit))
                    (push (fit-text remaining
                                    maximum-width
                                    size
                                    font)
                          lines)
                    (setf remaining ""))
                   (t
                    (let ((break-at (line-break-position remaining
                                                         maximum-width
                                                         size
                                                         font)))
                      (push (string-right-trim " " (subseq remaining 0 break-at)) lines)
                      (setf remaining (string-left-trim " " (subseq remaining break-at)))))))
    (or (nreverse lines)
        (list ""))))

(defun visible-text-window (string
                            caret
                            maximum-width
                            size
                            font)
  "Return the start and end of the part of STRING to show in a field MAXIMUM-WIDTH wide so that
the caret is visible: the window ends at the caret when the text before it is too long to fit,
and otherwise starts at the beginning.

STRING: the text.
CARET: caret position, a count of characters from the start.
MAXIMUM-WIDTH: width of the text area in pixels.
SIZE: em size in pixels.
FONT: :ui, :ui-strong or :mono."
  (let ((start 0)
        (end caret))
    ;; Move the start right until the text from there to the caret fits.
    (loop while (and (< start caret)
                     (not (fits-p (subseq string start caret)
                                  maximum-width
                                  size
                                  font)))
          do (incf start))
    (loop while (and (< end (length string))
                     (fits-p (subseq string start (1+ end))
                             maximum-width
                             size
                             font))
          do (incf end))
    (values start end)))

(defstruct (layout (:constructor make-layout (toolbar
                                              tool-palette
                                              canvas
                                              side-panel
                                              layers-panel
                                              inspector-panel
                                              status-bar)))
  "The regions of the window. They tile it without gaps.

TOOLBAR: the title bar along the top.
TOOL-PALETTE: the column of drawing tools along the left.
CANVAS: the drawing area.
SIDE-PANEL: right-hand column.
LAYERS-PANEL: upper part of the side panel.
INSPECTOR-PANEL: lower part of the side panel.
STATUS-BAR: strip along the bottom."
  toolbar tool-palette canvas side-panel layers-panel inspector-panel status-bar)

(defun compute-layout (width
                       height)
  "Divide a window of WIDTH x HEIGHT logical pixels into regions.

WIDTH: window width in pixels.
HEIGHT: window height in pixels."
  (let* ((width (float width 1f0))
         (height (float height 1f0))
         (middle-height (max 0f0 (- height +toolbar-height+ +status-height+)))
         (side-x (max +tool-palette-width+ (- width +side-panel-width+)))
         (side-width (max 0f0 (- width side-x)))
         (layers-height (* middle-height +layers-panel-fraction+)))
    (make-layout (make-rect 0f0
                            0f0
                            width
                            +toolbar-height+)
                 (make-rect 0f0
                            +toolbar-height+
                            (min width +tool-palette-width+)
                            middle-height)
                 (make-rect +tool-palette-width+
                            +toolbar-height+
                            (max 0f0 (- side-x +tool-palette-width+))
                            middle-height)
                 (make-rect side-x
                            +toolbar-height+
                            side-width
                            middle-height)
                 (make-rect side-x
                            +toolbar-height+
                            side-width
                            layers-height)
                 (make-rect side-x
                            (+ +toolbar-height+ layers-height)
                            side-width
                            (- middle-height layers-height))
                 (make-rect 0f0
                            (- height +status-height+)
                            width
                            +status-height+))))
