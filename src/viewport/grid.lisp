(in-package #:borba-cad/viewport)

;;;; Adaptive grid. The drawing's grid spacing is the finest level. When zooming out would
;;;; pack lines closer than +MINIMUM-GRID-SPACING-PIXELS+, the displayed spacing is multiplied
;;;; along the ladder 1, 2, 5, 10, 20, 50, ... until it is wide enough, so the grid thins out
;;;; instead of turning into a solid fill. Snapping always uses the drawing's own spacing.

(defconstant +minimum-grid-spacing-pixels+ 8d0
  "Closest the displayed grid lines may be, in pixels. Below it a grid reads as noise.")

(defconstant +grid-major-interval+ 5
  "Every this many displayed grid lines, one is drawn as a major line.")

(defconstant +maximum-grid-lines-per-axis+ 4000
  "Safety bound on the lines drawn per axis; reachable only with a degenerate viewport.")

(defstruct (grid-level (:constructor make-grid-level (spacing
                                                      major-interval)))
  "The grid as displayed at the current zoom.

SPACING: distance between displayed minor lines, in millimetres.
MAJOR-INTERVAL: every this many minor lines is a major line."
  (spacing 100d0 :type double-float :read-only t)
  (major-interval +grid-major-interval+ :type (integer 1) :read-only t))

(defun ladder-multiplier (index)
  "Return the INDEX-th multiplier of the ladder 1, 2, 5, 10, 20, 50, 100, ...

INDEX: position in the ladder, from 0."
  (multiple-value-bind (decade step)
      (floor index 3)
    (* (expt 10 decade) (aref #(1 2 5) step))))

(defun compute-grid-level (viewport
                           base-spacing)
  "Return the grid level to display: BASE-SPACING times the smallest ladder multiplier that
keeps the lines at least +MINIMUM-GRID-SPACING-PIXELS+ apart.

VIEWPORT: the viewport.
BASE-SPACING: the drawing's grid spacing in millimetres."
  (declare (type double-float base-spacing))
  (let ((scale (viewport-scale viewport)))
    (loop for index from 0
          for spacing of-type double-float = (* base-spacing (ladder-multiplier index))
          when (>= (* spacing scale) +minimum-grid-spacing-pixels+)
            return (make-grid-level spacing +grid-major-interval+))))

(defun visible-grid-range (viewport
                           spacing)
  "Return the integer line indices visible on the canvas as four values: first and last index
along x, first and last along y. Line I lies at I times SPACING. The range is clipped to
+MAXIMUM-GRID-LINES-PER-AXIS+ lines per axis.

VIEWPORT: the viewport.
SPACING: displayed grid spacing in millimetres."
  (declare (type double-float spacing))
  (let* ((bounds (viewport-visible-bounds viewport))
         (first-x (ceiling (geo:bounds-min-x bounds) spacing))
         (last-x (floor (geo:bounds-max-x bounds) spacing))
         (first-y (ceiling (geo:bounds-min-y bounds) spacing))
         (last-y (floor (geo:bounds-max-y bounds) spacing)))
    (flet ((limit-first (first last)
             (if (> (- last first) +maximum-grid-lines-per-axis+)
                 (- last +maximum-grid-lines-per-axis+)
                 first)))
      (values (limit-first first-x last-x) last-x
              (limit-first first-y last-y) last-y))))
