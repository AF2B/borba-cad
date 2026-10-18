(in-package #:borba-cad/geometry)

;;;; Where shapes cross, and the points of a shape that are found from a point outside it: the points that the
;;;; object snaps of intersection, perpendicular and tangent give. Everything is double-float millimetres.

(defun segment-segment-intersection (x1
                                     y1
                                     x2
                                     y2
                                     x3
                                     y3
                                     x4
                                     y4)
  "Return, as two values, the point where the segment (X1, Y1)-(X2, Y2) crosses the segment (X3, Y3)-(X4, Y4),
or NIL when they do not cross: parallel ones, and ones that end before they meet, do not.

X1: x of the start of the first segment.
Y1: y of the start of the first segment.
X2: x of its end.
Y2: y of its end.
X3: x of the start of the second segment.
Y3: y of the start of the second segment.
X4: x of its end.
Y4: y of its end."
  (declare (type double-float x1 y1 x2 y2 x3 y3 x4 y4))
  (let* ((dx1 (- x2 x1))
         (dy1 (- y2 y1))
         (dx2 (- x4 x3))
         (dy2 (- y4 y3))
         (denominator (- (* dx1 dy2) (* dy1 dx2))))
    (unless (< (abs denominator) 1d-12)
      (let ((along-first (/ (- (* (- x3 x1) dy2) (* (- y3 y1) dx2)) denominator))
            (along-second (/ (- (* (- x3 x1) dy1) (* (- y3 y1) dx1)) denominator))
            (margin 1d-9))
        (when (and (<= (- margin) along-first (+ 1d0 margin))
                   (<= (- margin) along-second (+ 1d0 margin)))
          (values (+ x1 (* along-first dx1))
                  (+ y1 (* along-first dy1))))))))

(defun segment-circle-intersections (x1
                                     y1
                                     x2
                                     y2
                                     center-x
                                     center-y
                                     radius)
  "Return the points where a segment crosses a circle, as a list of (x . y) conses: none, one when it touches or
enters and stops inside, or two.

X1: x of the start of the segment.
Y1: y of the start of the segment.
X2: x of its end.
Y2: y of its end.
CENTER-X: x of the center of the circle.
CENTER-Y: y of the center.
RADIUS: its radius."
  (declare (type double-float x1 y1 x2 y2 center-x center-y radius))
  (let* ((dx (- x2 x1))
         (dy (- y2 y1))
         (fx (- x1 center-x))
         (fy (- y1 center-y))
         (a (+ (* dx dx) (* dy dy)))
         (b (+ (* fx dx) (* fy dy)))
         (c (- (+ (* fx fx) (* fy fy)) (* radius radius)))
         (discriminant (- (* b b) (* a c))))
    (when (and (> a 1d-12) (>= discriminant 0d0))
      (let ((root (sqrt discriminant))
            (points '()))
        (dolist (along (if (< root 1d-9)
                           (list (/ (- b) a))
                           (list (/ (- (- b) root) a) (/ (+ (- b) root) a))))
          (when (<= -1d-9 along (+ 1d0 1d-9))
            (push (cons (+ x1 (* along dx)) (+ y1 (* along dy))) points)))
        (nreverse points)))))

(defun circle-circle-intersections (x1
                                    y1
                                    radius1
                                    x2
                                    y2
                                    radius2)
  "Return the points where two circles cross, as a list of (x . y) conses: none, one when they touch, or two.

X1: x of the center of the first circle.
Y1: y of its center.
RADIUS1: its radius.
X2: x of the center of the second circle.
Y2: y of its center.
RADIUS2: its radius."
  (declare (type double-float x1 y1 radius1 x2 y2 radius2))
  (let ((distance (distance x1
                            y1
                            x2
                            y2)))
    (when (and (> distance 1d-9)
               (<= distance (+ radius1 radius2 1d-9))
               (>= distance (- (abs (- radius1 radius2)) 1d-9)))
      (let* ((along (/ (+ (- (* radius1 radius1) (* radius2 radius2)) (* distance distance)) (* 2 distance)))
             (height (sqrt (max 0d0 (- (* radius1 radius1) (* along along)))))
             (ux (/ (- x2 x1) distance))
             (uy (/ (- y2 y1) distance))
             (middle-x (+ x1 (* along ux)))
             (middle-y (+ y1 (* along uy))))
        (if (< height 1d-9)
            (list (cons middle-x middle-y))
            (list (cons (- middle-x (* height uy)) (+ middle-y (* height ux)))
                  (cons (+ middle-x (* height uy)) (- middle-y (* height ux)))))))))

(defun point-segment-foot (px
                           py
                           x1
                           y1
                           x2
                           y2)
  "Return, as two values, the foot of the perpendicular from a point to a segment, or NIL when the foot is
beyond an end of the segment.

PX: x of the point.
PY: y of the point.
X1: x of the start of the segment.
Y1: y of the start of the segment.
X2: x of its end.
Y2: y of its end."
  (declare (type double-float px py x1 y1 x2 y2))
  (let* ((dx (- x2 x1))
         (dy (- y2 y1))
         (length-squared (+ (* dx dx) (* dy dy))))
    (when (> length-squared 1d-12)
      (let ((along (/ (+ (* (- px x1) dx) (* (- py y1) dy)) length-squared)))
        (when (<= -1d-9 along (+ 1d0 1d-9))
          (values (+ x1 (* along dx)) (+ y1 (* along dy))))))))

(defun circle-perpendicular-feet (px
                                  py
                                  center-x
                                  center-y
                                  radius)
  "Return the points of a circle where the line through them and the center passes through a point, as a list of
two (x . y) conses; none when the point is the center.

PX: x of the point.
PY: y of the point.
CENTER-X: x of the center of the circle.
CENTER-Y: y of the center.
RADIUS: its radius."
  (declare (type double-float px py center-x center-y radius))
  (let ((distance (distance px
                            py
                            center-x
                            center-y)))
    (when (> distance 1d-9)
      (let ((ux (/ (- px center-x) distance))
            (uy (/ (- py center-y) distance)))
        (list (cons (+ center-x (* radius ux)) (+ center-y (* radius uy)))
              (cons (- center-x (* radius ux)) (- center-y (* radius uy))))))))

(defun circle-tangent-points (px
                              py
                              center-x
                              center-y
                              radius)
  "Return the points of a circle where a line from a point outside it touches it, as a list of two (x . y)
conses; none when the point is on the circle or inside it.

PX: x of the point.
PY: y of the point.
CENTER-X: x of the center of the circle.
CENTER-Y: y of the center.
RADIUS: its radius."
  (declare (type double-float px py center-x center-y radius))
  (let ((distance (distance px
                            py
                            center-x
                            center-y)))
    (when (> distance (+ radius 1d-9))
      (let* ((toward (atan (- center-y py) (- center-x px)))
             (opening (asin (/ radius distance)))
             (length (sqrt (- (* distance distance) (* radius radius)))))
        (loop for sign in '(-1d0 1d0)
              collect (let ((angle (+ toward (* sign opening))))
                        (cons (+ px (* length (cos angle)))
                              (+ py (* length (sin angle))))))))))
