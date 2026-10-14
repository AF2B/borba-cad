(in-package #:borba-cad/ui)

(defconstant +font-pixels+ 14f0
  "Em size of the user-interface text, in logical pixels.")

(defconstant +small-font-pixels+ 12f0
  "Em size of secondary text, in logical pixels.")

(defconstant +row-height+ 24f0
  "Height of one list row or field, in logical pixels.")

(defconstant +padding+ 8f0
  "Standard gap between user-interface elements, in logical pixels.")

(defconstant +toolbar-height+ 40f0
  "Height of the toolbar, in logical pixels.")

(defconstant +status-height+ 26f0
  "Height of the status bar, in logical pixels.")

(defconstant +side-panel-width+ 330f0
  "Width of the right-hand panel, in logical pixels.")

(defconstant +layers-panel-fraction+ 0.38f0
  "Share of the side panel's height given to the layers panel; the inspector gets the rest.")

(defconstant +fit-tolerance-pixels+ 0.01f0
  "Slack added to the available width when fitting text, so a label sized exactly to its text
is never truncated by floating-point rounding.")

(defparameter *ellipsis* "..."
  "Suffix that marks a truncated label.")

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
                          &optional (size +font-pixels+))
  "Return the width of STRING at SIZE on the fixed 0.6 em grid, as a single-float.

STRING: the text.
SIZE: em size in pixels."
  (float (* (length string) geo:+text-advance-ratio+ size) 1f0))

(defun fit-text (string
                 maximum-width
                 &optional (size +font-pixels+))
  "Return STRING, shortened with an ellipsis when it would be wider than MAXIMUM-WIDTH.

STRING: the text.
MAXIMUM-WIDTH: available width in pixels.
SIZE: em size in pixels."
  (let ((capacity (floor (+ maximum-width +fit-tolerance-pixels+) (* geo:+text-advance-ratio+ size))))
    (cond ((<= (length string) capacity) string)
          ((<= capacity (length *ellipsis*)) (subseq string 0 (max 0 capacity)))
          (t (concatenate 'string
                          (subseq string 0 (- capacity (length *ellipsis*)))
                          *ellipsis*)))))

(defstruct (layout (:constructor make-layout (toolbar
                                              canvas
                                              side-panel
                                              layers-panel
                                              inspector-panel
                                              status-bar)))
  "The regions of the window.

TOOLBAR: strip along the top.
CANVAS: the drawing area.
SIDE-PANEL: right-hand column.
LAYERS-PANEL: upper part of the side panel.
INSPECTOR-PANEL: lower part of the side panel.
STATUS-BAR: strip along the bottom."
  toolbar canvas side-panel layers-panel inspector-panel status-bar)

(defun compute-layout (width
                       height)
  "Divide a window of WIDTH x HEIGHT logical pixels into regions.

WIDTH: window width in pixels.
HEIGHT: window height in pixels."
  (let* ((width (float width 1f0))
         (height (float height 1f0))
         (middle-height (max 0f0 (- height +toolbar-height+ +status-height+)))
         (side-x (max 0f0 (- width +side-panel-width+)))
         (layers-height (* middle-height +layers-panel-fraction+)))
    (make-layout (make-rect 0f0
                            0f0
                            width
                            +toolbar-height+)
                 (make-rect 0f0
                            +toolbar-height+
                            side-x
                            middle-height)
                 (make-rect side-x
                            +toolbar-height+
                            (- width side-x)
                            middle-height)
                 (make-rect side-x
                            +toolbar-height+
                            (- width side-x)
                            layers-height)
                 (make-rect side-x
                            (+ +toolbar-height+ layers-height)
                            (- width side-x)
                            (- middle-height layers-height))
                 (make-rect 0f0
                            (- height +status-height+)
                            width
                            +status-height+))))
