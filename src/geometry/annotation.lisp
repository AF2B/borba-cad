(in-package #:borba-cad/geometry)

;;;; Layouts of the radius, diameter and angle dimensions. Each is a few segments, arrowheads and a line of text,
;;;; all sized by the text height, so that a dimension keeps its proportions at any drawing scale.

(defconstant +radial-leader-gap-ratio+ 0.5d0
  "Space around the text of a radius dimension on its leader, as a fraction of the text height.")

(defconstant +angular-gap-ratio+ 0.4d0
  "How far an extension line of an angle dimension stays from the vertex, and how far the text stays from the
arc, as a fraction of the text height.")

(defconstant +angular-overshoot-ratio+ 0.6d0
  "How far an extension line of an angle dimension runs past the arc, as a fraction of the text height.")

(defconstant +angular-segment-step+ (/ (float pi 1d0) 45)
  "The most an angle dimension turns in one segment of its arc: four degrees.")

(defconstant +angular-minimum-segments+ 2
  "Fewest segments the arc of an angle dimension is drawn with.")

(defconstant +minimum-arrow-room-ratio+ 2
  "A dimension line shorter than this many arrow lengths cannot hold its arrows, which then stand outside it.")

(defstruct (annotation-layout (:constructor make-annotation-layout (segments
                                                                    arrowheads
                                                                    text-x
                                                                    text-y
                                                                    text-angle
                                                                    text-width
                                                                    text-height
                                                                    bounds)))
  "Everything needed to draw a radius, diameter or angle dimension, in world coordinates.

SEGMENTS: doubles, x1 y1 x2 y2 for each stroked segment.
ARROWHEADS: doubles, six for each filled triangle (tip, base corner, base corner).
TEXT-X: x of the text baseline start.
TEXT-Y: y of the text baseline start.
TEXT-ANGLE: text direction in radians.
TEXT-WIDTH: width of the text line.
TEXT-HEIGHT: text height (em size).
BOUNDS: bounds of every part."
  (segments nil :type (simple-array double-float (*)) :read-only t)
  (arrowheads nil :type (simple-array double-float (*)) :read-only t)
  (text-x 0d0 :type double-float :read-only t)
  (text-y 0d0 :type double-float :read-only t)
  (text-angle 0d0 :type double-float :read-only t)
  (text-width 0d0 :type double-float :read-only t)
  (text-height 0d0 :type double-float :read-only t)
  (bounds nil :type bounds :read-only t))

(defun doubles-array (numbers)
  "Return NUMBERS, a list of doubles, as a simple array of double-float.

NUMBERS: a list of double-floats."
  (make-array (length numbers) :element-type 'double-float :initial-contents numbers))

(defun finish-annotation-layout (segments
                                 arrowheads
                                 text-x
                                 text-y
                                 text-angle
                                 character-count
                                 text-height)
  "Build the layout of an annotation from its parts, working out the width of the text and the bounds of
everything.

SEGMENTS: list of doubles, x1 y1 x2 y2 for each segment.
ARROWHEADS: list of doubles, six for each triangle.
TEXT-X: x of the text baseline start.
TEXT-Y: y of the text baseline start.
TEXT-ANGLE: text direction in radians.
CHARACTER-COUNT: characters in the text.
TEXT-HEIGHT: text height (em size)."
  (let* ((width (text-width character-count text-height))
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
    (dolist (points (list segments arrowheads))
      (loop for (x y) on points by #'cddr
            do (setf min-x (min min-x x)
                     max-x (max max-x x)
                     min-y (min min-y y)
                     max-y (max max-y y))))
    (make-annotation-layout (doubles-array segments)
                            (doubles-array arrowheads)
                            text-x
                            text-y
                            text-angle
                            width
                            text-height
                            (make-bounds min-x
                                         min-y
                                         max-x
                                         max-y))))

(defun reads-backwards-p (direction-x
                          direction-y)
  "Return true when a line in this direction would have its text upside down, so the text runs the other way.

DIRECTION-X: x of the direction.
DIRECTION-Y: y of the direction."
  (or (< direction-x 0d0)
      (and (= direction-x 0d0) (< direction-y 0d0))))

(defun compute-radial-dimension-layout (center-x
                                        center-y
                                        radius
                                        angle
                                        diameter
                                        text-height
                                        character-count)
  "Compute the layout of a radius or diameter dimension of a circle. A radius dimension is a leader from the center
through the circle at ANGLE, with an arrowhead on the circle and the text on a tail beyond it. A diameter
dimension is the line through the center at ANGLE, with an arrowhead at each end and the text above it.

CENTER-X: x of the center.
CENTER-Y: y of the center.
RADIUS: radius of the circle.
ANGLE: direction of the leader from the center, in radians.
DIAMETER: true for a diameter dimension.
TEXT-HEIGHT: text height (em size) that sizes every part.
CHARACTER-COUNT: characters in the dimension text."
  (declare (type double-float center-x center-y radius angle text-height))
  (let* ((ux (cos angle))
         (uy (sin angle))
         (reversed (reads-backwards-p ux uy))
         (text-angle (atan (if reversed (- uy) uy) (if reversed (- ux) ux)))
         (text-direction-x (cos text-angle))
         (text-direction-y (sin text-angle))
         (text-normal-x (- text-direction-y))
         (text-normal-y text-direction-x)
         (width (text-width character-count text-height))
         (gap (* +radial-leader-gap-ratio+ text-height))
         (text-gap (* +dimension-text-gap-ratio+ text-height))
         (arrow-length (* +dimension-arrow-length-ratio+ text-height))
         (arrow-width (* +dimension-arrow-width-ratio+ arrow-length))
         (edge-x (+ center-x (* ux radius)))
         (edge-y (+ center-y (* uy radius)))
         (opposite-x (- center-x (* ux radius)))
         (opposite-y (- center-y (* uy radius)))
         (room-for-arrows (>= (if diameter (* 2 radius) radius)
                              (* +minimum-arrow-room-ratio+ arrow-length)))
         (inside-sign (if room-for-arrows 1d0 -1d0)))
    (if diameter
        (finish-annotation-layout (list opposite-x opposite-y edge-x edge-y)
                                  (append (arrowhead-triangle edge-x
                                                              edge-y
                                                              (* (- ux) inside-sign)
                                                              (* (- uy) inside-sign)
                                                              arrow-length
                                                              arrow-width)
                                          (arrowhead-triangle opposite-x
                                                              opposite-y
                                                              (* ux inside-sign)
                                                              (* uy inside-sign)
                                                              arrow-length
                                                              arrow-width))
                                  (+ (- center-x (* text-direction-x 0.5d0 width)) (* text-normal-x text-gap))
                                  (+ (- center-y (* text-direction-y 0.5d0 width)) (* text-normal-y text-gap))
                                  text-angle
                                  character-count
                                  text-height)
        (let* ((tail (+ width (* 2 gap)))
               (start-distance (if reversed (+ gap width) gap)))
          (finish-annotation-layout (list center-x
                                          center-y
                                          (+ edge-x (* ux tail))
                                          (+ edge-y (* uy tail)))
                                    (arrowhead-triangle edge-x
                                                        edge-y
                                                        (* (- ux) inside-sign)
                                                        (* (- uy) inside-sign)
                                                        arrow-length
                                                        arrow-width)
                                    (+ edge-x (* ux start-distance) (* text-normal-x text-gap))
                                    (+ edge-y (* uy start-distance) (* text-normal-y text-gap))
                                    text-angle
                                    character-count
                                    text-height)))))

(defun angular-arc-segments (vertex-x
                             vertex-y
                             radius
                             start-angle
                             sweep)
  "Return the segments, as a list of doubles, that approximate an arc of a circle about a vertex.

VERTEX-X: x of the center.
VERTEX-Y: y of the center.
RADIUS: radius of the arc.
START-ANGLE: direction of its start, in radians.
SWEEP: how far it turns counter-clockwise, in radians."
  (let* ((count (max +angular-minimum-segments+ (ceiling sweep +angular-segment-step+)))
         (points (loop for index from 0 to count
                       for angle = (+ start-angle (* sweep (/ index (float count 1d0))))
                       collect (cons (+ vertex-x (* radius (cos angle)))
                                     (+ vertex-y (* radius (sin angle)))))))
    (loop for (first second) on points
          while second
          append (list (car first) (cdr first) (car second) (cdr second)))))

(defun compute-angular-dimension-layout (vertex-x
                                         vertex-y
                                         start-angle
                                         end-angle
                                         radius
                                         text-height
                                         character-count)
  "Compute the layout of an angle dimension: the two arms as extension lines, an arc of the given radius from
the first arm to the second, counter-clockwise, an arrowhead at each end of the arc, and the text, level, beyond
the middle of the arc.

VERTEX-X: x of the vertex.
VERTEX-Y: y of the vertex.
START-ANGLE: direction of the first arm, in radians.
END-ANGLE: direction of the second arm; the dimension turns counter-clockwise from the first to this one.
RADIUS: radius of the dimension arc.
TEXT-HEIGHT: text height (em size) that sizes every part.
CHARACTER-COUNT: characters in the dimension text."
  (declare (type double-float vertex-x vertex-y start-angle end-angle radius text-height))
  (let* ((sweep (normalize-angle (- end-angle start-angle)))
         (gap (* +angular-gap-ratio+ text-height))
         (inner (min gap (* 0.5d0 radius)))
         (outer (+ radius (* +angular-overshoot-ratio+ text-height)))
         (arrow-length (* +dimension-arrow-length-ratio+ text-height))
         (arrow-width (* +dimension-arrow-width-ratio+ arrow-length))
         (inside-sign (if (>= (* radius sweep) (* +minimum-arrow-room-ratio+ arrow-length)) 1d0 -1d0))
         (start-x (+ vertex-x (* radius (cos start-angle))))
         (start-y (+ vertex-y (* radius (sin start-angle))))
         (end-x (+ vertex-x (* radius (cos end-angle))))
         (end-y (+ vertex-y (* radius (sin end-angle))))
         (middle (+ start-angle (* 0.5d0 sweep)))
         (width (text-width character-count text-height))
         (push (+ gap
                  (* 0.5d0 width (abs (cos middle)))
                  (* 0.5d0 text-height (abs (sin middle)))))
         (text-center-x (+ vertex-x (* (+ radius push) (cos middle))))
         (text-center-y (+ vertex-y (* (+ radius push) (sin middle)))))
    (finish-annotation-layout (append (list (+ vertex-x (* inner (cos start-angle)))
                                            (+ vertex-y (* inner (sin start-angle)))
                                            (+ vertex-x (* outer (cos start-angle)))
                                            (+ vertex-y (* outer (sin start-angle)))
                                            (+ vertex-x (* inner (cos end-angle)))
                                            (+ vertex-y (* inner (sin end-angle)))
                                            (+ vertex-x (* outer (cos end-angle)))
                                            (+ vertex-y (* outer (sin end-angle))))
                                      (angular-arc-segments vertex-x
                                                            vertex-y
                                                            radius
                                                            start-angle
                                                            sweep))
                              (append (arrowhead-triangle start-x
                                                          start-y
                                                          (* (- (sin start-angle)) inside-sign)
                                                          (* (cos start-angle) inside-sign)
                                                          arrow-length
                                                          arrow-width)
                                      (arrowhead-triangle end-x
                                                          end-y
                                                          (* (sin end-angle) inside-sign)
                                                          (* (- (cos end-angle)) inside-sign)
                                                          arrow-length
                                                          arrow-width))
                              (- text-center-x (* 0.5d0 width))
                              (- text-center-y (* 0.5d0 text-height (- +text-ascent-ratio+ +text-descent-ratio+)))
                              0d0
                              character-count
                              text-height)))

(defun annotation-pick-distance (layout
                                 x
                                 y)
  "Return the distance from a point to the nearest stroke or the text of an annotation.

LAYOUT: an annotation layout.
X: point x.
Y: point y."
  (let ((segments (annotation-layout-segments layout))
        (text-height (annotation-layout-text-height layout)))
    (min (loop for index from 0 below (length segments) by 4
               minimize (point-segment-distance x
                                                y
                                                (aref segments index)
                                                (aref segments (+ index 1))
                                                (aref segments (+ index 2))
                                                (aref segments (+ index 3))))
         (point-oriented-box-distance x
                                      y
                                      (annotation-layout-text-x layout)
                                      (annotation-layout-text-y layout)
                                      (annotation-layout-text-width layout)
                                      (* +text-ascent-ratio+ text-height)
                                      (* +text-descent-ratio+ text-height)
                                      (annotation-layout-text-angle layout)))))

(defun annotation-touches-bounds-p (layout
                                    bounds)
  "Return true when a stroke or the text of an annotation has a point inside BOUNDS or on its edge.

LAYOUT: an annotation layout.
BOUNDS: the window."
  (let ((segments (annotation-layout-segments layout))
        (text-height (annotation-layout-text-height layout)))
    (or (loop for index from 0 below (length segments) by 4
              thereis (segment-touches-bounds-p (aref segments index)
                                                (aref segments (+ index 1))
                                                (aref segments (+ index 2))
                                                (aref segments (+ index 3))
                                                bounds))
        (oriented-box-touches-bounds-p (annotation-layout-text-x layout)
                                       (annotation-layout-text-y layout)
                                       (annotation-layout-text-width layout)
                                       (* +text-ascent-ratio+ text-height)
                                       (* +text-descent-ratio+ text-height)
                                       (annotation-layout-text-angle layout)
                                       bounds))))
