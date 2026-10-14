(in-package #:borba-cad/geometry)

;;;; Aligned dimension layout. All sizes scale with the text height so a dimension keeps its
;;;; proportions at any drawing scale.

(defconstant +dimension-arrow-length-ratio+ 1d0
  "Arrowhead length as a fraction of the text height.")

(defconstant +dimension-arrow-width-ratio+ 0.3d0
  "Arrowhead width as a fraction of the arrowhead length.")

(defconstant +dimension-extension-gap-ratio+ 0.4d0
  "Gap between a definition point and the start of its extension line, as a fraction of the
text height.")

(defconstant +dimension-extension-overshoot-ratio+ 0.6d0
  "How far an extension line runs past the dimension line, as a fraction of the text height.")

(defconstant +dimension-text-gap-ratio+ 0.35d0
  "Gap between the dimension line and the text baseline, as a fraction of the text height.")

(defstruct (dimension-layout (:constructor make-dimension-layout (extension-lines
                                                                 dimension-line
                                                                 arrowheads
                                                                 text-x
                                                                 text-y
                                                                 text-angle
                                                                 text-width
                                                                 text-height
                                                                 bounds)))
  "Everything needed to draw one aligned dimension, in world coordinates.

EXTENSION-LINES: 8 doubles, two segments (x1 y1 x2 y2 each).
DIMENSION-LINE: 4 doubles, one segment.
ARROWHEADS: 12 doubles, two filled triangles (tip, base corner, base corner).
TEXT-X: x of the text baseline start.
TEXT-Y: y of the text baseline start.
TEXT-ANGLE: text direction in radians, chosen so the text reads left to right.
TEXT-WIDTH: width of the text line.
TEXT-HEIGHT: text height (em size).
BOUNDS: bounds of every part."
  (extension-lines nil :type (simple-array double-float (8)) :read-only t)
  (dimension-line nil :type (simple-array double-float (4)) :read-only t)
  (arrowheads nil :type (simple-array double-float (12)) :read-only t)
  (text-x 0d0 :type double-float :read-only t)
  (text-y 0d0 :type double-float :read-only t)
  (text-angle 0d0 :type double-float :read-only t)
  (text-width 0d0
              :type double-float
              :read-only t)
  (text-height 0d0 :type double-float :read-only t)
  (bounds nil :type bounds :read-only t))

(defun arrowhead-triangle (tip-x
                           tip-y
                           axis-x
                           axis-y
                           length
                           width)
  "Return the six coordinates of an arrowhead triangle whose base lies along AXIS from the tip.

TIP-X: x of the arrow tip.
TIP-Y: y of the arrow tip.
AXIS-X: x of the unit vector from the tip toward the base.
AXIS-Y: y of the unit vector from the tip toward the base.
LENGTH: tip-to-base distance.
WIDTH: base width."
  (declare (type double-float tip-x tip-y axis-x axis-y length width))
  (let* ((base-x (+ tip-x (* axis-x length)))
         (base-y (+ tip-y (* axis-y length)))
         (half-width (* 0.5d0 width))
         (normal-x (- axis-y))
         (normal-y axis-x))
    (list tip-x
          tip-y
          (+ base-x (* normal-x half-width))
          (+ base-y (* normal-y half-width))
          (- base-x (* normal-x half-width))
          (- base-y (* normal-y half-width)))))

(defun compute-dimension-layout (x1
                                 y1
                                 x2
                                 y2
                                 offset
                                 text-height
                                 character-count)
  "Compute the layout of an aligned dimension between two points.
The dimension line is parallel to the segment P1-P2 and displaced by OFFSET along its left
normal (negative OFFSET displaces to the right). P1 and P2 must be farther apart than the
length tolerance.

X1: x of the first definition point.
Y1: y of the first definition point.
X2: x of the second definition point.
Y2: y of the second definition point.
OFFSET: signed distance of the dimension line from the measured segment.
TEXT-HEIGHT: text height (em size) that sizes every part.
CHARACTER-COUNT: number of characters in the dimension text."
  (declare (type double-float x1 y1 x2 y2 offset text-height))
  (let* ((measured (distance x1
                             y1
                             x2
                             y2))
         (ux (/ (- x2 x1) measured))
         (uy (/ (- y2 y1) measured))
         (nx (- uy))
         (ny ux)
         (side (if (minusp offset) -1d0 1d0))
         (gap (min (* +dimension-extension-gap-ratio+ text-height) (abs offset)))
         (overshoot (* +dimension-extension-overshoot-ratio+ text-height))
         (a1x (+ x1 (* nx offset)))
         (a1y (+ y1 (* ny offset)))
         (a2x (+ x2 (* nx offset)))
         (a2y (+ y2 (* ny offset)))
         (arrow-length (* +dimension-arrow-length-ratio+ text-height))
         (arrow-width (* +dimension-arrow-width-ratio+ arrow-length))
         ;; Arrows normally point outward from inside the line; when the line is too short
         ;; to hold them, they sit outside it pointing inward.
         (inside-sign (if (< measured (* 2 arrow-length)) -1d0 1d0))
         (extension-lines (make-array 8 :element-type 'double-float
                                        :initial-contents
                                        (list (+ x1 (* nx side gap))
                                              (+ y1 (* ny side gap))
                                              (+ a1x (* nx side overshoot))
                                              (+ a1y (* ny side overshoot))
                                              (+ x2 (* nx side gap))
                                              (+ y2 (* ny side gap))
                                              (+ a2x (* nx side overshoot))
                                              (+ a2y (* ny side overshoot)))))
         (dimension-line (make-array 4 :element-type 'double-float
                                       :initial-contents (list a1x a1y a2x a2y)))
         (arrowheads (make-array 12 :element-type 'double-float
                                    :initial-contents
                                    (append (arrowhead-triangle a1x
                                                                a1y
                                                                (* ux inside-sign)
                                                                (* uy inside-sign)
                                                                arrow-length
                                                                arrow-width)
                                            (arrowhead-triangle a2x
                                                                a2y
                                                                (- (* ux inside-sign))
                                                                (- (* uy inside-sign))
                                                                arrow-length
                                                                arrow-width))))
         ;; Text reads left to right: flip the direction when the line points leftward.
         (reversed (or (< ux 0d0) (and (= ux 0d0) (< uy 0d0))))
         (text-angle (atan (if reversed (- uy) uy) (if reversed (- ux) ux)))
         (text-direction-x (cos text-angle))
         (text-direction-y (sin text-angle))
         (text-normal-x (- text-direction-y))
         (text-normal-y text-direction-x)
         (width (text-width character-count text-height))
         (middle-x (* 0.5d0 (+ a1x a2x)))
         (middle-y (* 0.5d0 (+ a1y a2y)))
         (text-gap (* +dimension-text-gap-ratio+ text-height))
         (text-x (+ (- middle-x (* text-direction-x 0.5d0 width)) (* text-normal-x text-gap)))
         (text-y (+ (- middle-y (* text-direction-y 0.5d0 width)) (* text-normal-y text-gap)))
         (text-bounds (oriented-box-bounds text-x
                                           text-y
                                           width
                                           (* +text-ascent-ratio+ text-height)
                                           (* +text-descent-ratio+ text-height)
                                           text-angle))
         (min-x (bounds-min-x text-bounds))
         (min-y (bounds-min-y text-bounds))
         (max-x (bounds-max-x text-bounds))
         (max-y (bounds-max-y text-bounds)))
    (declare (type double-float measured ux uy nx ny gap a1x a1y a2x a2y min-x min-y max-x max-y))
    (flet ((include-points (points)
             (loop for (x y) on (coerce points 'list) by #'cddr
                   do (setf min-x (min min-x x)
                            max-x (max max-x x)
                            min-y (min min-y y)
                            max-y (max max-y y)))))
      (include-points extension-lines)
      (include-points dimension-line)
      (include-points arrowheads))
    (make-dimension-layout extension-lines
                           dimension-line
                           arrowheads
                           text-x
                           text-y
                           text-angle
                           width
                           text-height
                           (make-bounds min-x
                                        min-y
                                        max-x
                                        max-y))))
