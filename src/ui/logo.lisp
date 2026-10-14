(in-package #:borba-cad/ui)

;;;; The brand mark: a rounded tile in the accent color holding a "B" drawn with the strokes of a
;;;; technical pen, straight bars and two round bowls. It is data on the same 24 by 24 grid as the
;;;; icons, so it scales to any size.

(defconstant +logo-grid+ 24f0
  "Side of the grid the mark is drawn on.")

(defconstant +logo-tile-radius+ 6f0
  "Corner radius of the tile, in grid units.")

(defconstant +logo-stroke+ 2.5f0
  "Width of the strokes of the letter, in grid units.")

(defconstant +logo-bowl-radius+ 3.25f0
  "Radius of the two bowls of the letter, in grid units.")

(defconstant +logo-bowl-start+ (* -0.5f0 (float pi 1f0))
  "Angle where a bowl begins, at its top, in radians.")

(defconstant +logo-bowl-end+ (* 0.5f0 (float pi 1f0))
  "Angle where a bowl ends, at its bottom, in radians.")

(defparameter *logo-strokes*
  '((:bar 8.5 5.5 8.5 18.5)
    (:bar 8.5 5.5 12.5 5.5)
    (:bowl 12.5 8.75)
    (:bar 8.5 12.0 13.2 12.0)
    (:bowl 13.2 15.25)
    (:bar 8.5 18.5 13.2 18.5))
  "The strokes of the letter: a bar from one point to another, or a bowl around a center.")

(defun draw-logo (list
                  x
                  y
                  size
                  theme)
  "Draw the brand mark: the tile and the letter in the colors of the theme.

LIST: the draw list.
X: left edge in pixels.
Y: top edge in pixels.
SIZE: side of the tile in pixels.
THEME: the theme."
  (let ((ink (scene:theme-accent-contrast theme))
        (unit (/ size +logo-grid+))
        (width (* +logo-stroke+ (/ size +logo-grid+))))
    (flet ((px (grid-x)
             (+ x (* grid-x unit)))
           (py (grid-y)
             (+ y (* grid-y unit))))
      (scene:add-rounded-rectangle list
                                   x
                                   y
                                   size
                                   size
                                   (* +logo-tile-radius+ unit)
                                   (scene:theme-accent theme))
      (dolist (stroke *logo-strokes*)
        (ecase (first stroke)
          (:bar
           (destructuring-bind (from-x from-y to-x to-y) (rest stroke)
             (scene:add-thick-line list
                                   (px from-x)
                                   (py from-y)
                                   (px to-x)
                                   (py to-y)
                                   width
                                   ink)
             ;; A disc at each end rounds the corners where bars meet.
             (scene:add-disc list
                             (px from-x)
                             (py from-y)
                             (/ width 2)
                             ink)
             (scene:add-disc list
                             (px to-x)
                             (py to-y)
                             (/ width 2)
                             ink)))
          (:bowl
           (destructuring-bind (center-x center-y) (rest stroke)
             (scene:add-arc-stroke list
                                   (px center-x)
                                   (py center-y)
                                   (* +logo-bowl-radius+ unit)
                                   +logo-bowl-start+
                                   +logo-bowl-end+
                                   width
                                   ink))))))))
