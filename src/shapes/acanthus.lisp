(in-package #:borba-cad/shapes)

;;;; Acanthus leaves, as ironwork draws them: a midrib with lobes that rise like flames on either side, a notch cut
;;;; deep between each two, a pointed tip, and, in some, a stem that ends in a volute. They are made by a procedure
;;;; with a few numbers (how many lobes, how wide, how deep the notches, which way the tips curl), so that a family
;;;; of them is a list of numbers, and so that every curve is exact at any size.

(defconstant +leaf-length+ 100d0
  "How tall a leaf is, from the base to the tip: a shape is 100 units high.")

;;; A pen that draws a polyline from lines and curves

(defstruct (pen (:constructor make-pen (start
                                         &aux (points (list start)))))
  "What draws a contour one stroke after another.

START: the point it begins at, a cons of x and y.
POINTS: the points so far, last first."
  (points '() :type list)
  start)

(defun pen-here (pen)
  "Return the point that a pen is at.

PEN: a pen."
  (first (pen-points pen)))

(defun pen-line-to (pen
                    point)
  "Draw a straight line from where a pen is to a point.

PEN: a pen.
POINT: a cons of x and y."
  (push point (pen-points pen))
  pen)

(defun pen-curve-to (pen
                     control-1
                     control-2
                     point)
  "Draw a cubic Bezier curve from where a pen is to a point.

PEN: a pen.
CONTROL-1: the first control point, a cons of x and y.
CONTROL-2: the second control point.
POINT: where the curve ends."
  (dolist (flat (flatten-cubic (pen-here pen)
                               control-1
                               control-2
                               point))
    (push flat (pen-points pen)))
  pen)

(defun pen-contour (pen
                    &optional closed)
  "Return what a pen has drawn as a contour.

PEN: a pen.
CLOSED: true when the last point joins the first."
  (make-contour (reverse (pen-points pen))
                closed))

(defun point+ (point
               dx
               dy)
  "Return a point moved by a distance along x and a distance along y.

POINT: a cons of x and y.
DX: the distance along x.
DY: the distance along y."
  (cons (+ (car point) dx)
        (+ (cdr point) dy)))

;;; One side of a leaf

(defstruct (side (:constructor make-side (&key lobes
                                               width
                                               depth
                                               lowest
                                               highest
                                               belly
                                               scoop)))
  "What one edge of a leaf is like.

LOBES: how many lobes, not counting the tip.
WIDTH: how far from the midrib the lobes reach at the widest, in units.
DEPTH: how far out the notches are, as a fraction of the lobes: 0 cuts to the midrib, 1 leaves no notch.
LOWEST: the angle, in degrees above the horizontal, that the lowest lobe points at.
HIGHEST: the angle that the highest lobe points at; the lobes between fan out from one to the other.
BELLY: how far the lower edge of a lobe bulges outward, as a fraction of its length.
SCOOP: how far the upper edge of a lobe hollows, as a fraction of its length."
  (lobes 4 :type (integer 1 12))
  (width 36d0 :type double-float)
  (depth 0.30d0 :type double-float)
  (lowest 18d0 :type double-float)
  (highest 70d0 :type double-float)
  (belly 0.22d0 :type double-float)
  (scoop 0.12d0 :type double-float))

(defun half-width-at (fraction
                      width)
  "Return how far from the midrib a leaf reaches at a fraction of its length: nothing at the base and at the tip, the
most at about a third of the way up.

FRACTION: 0 at the base, 1 at the tip.
WIDTH: the most it reaches."
  (* width
     (expt (sin (* pi (expt fraction 0.72d0))) 0.85d0)
     1.0d0))

(defun lobe-base-height (index
                         lobes
                         length)
  "Return how high up the midrib the notch before a lobe is: the lobes are shorter toward the tip, and the first is
above a short stem.

INDEX: which lobe, counting from the base from 0; LOBES counts the tip.
LOBES: how many lobes there are in all.
LENGTH: the length of the leaf."
  (* length (+ 0.12d0 (* 0.82d0 (- 1d0 (expt (- 1d0 (/ index (float lobes 1d0))) 1.22d0))))))

(defun offset-point (point
                     from
                     to
                     along
                     across)
  "Return a point that is a fraction ALONG the way from one point to another, and moved sideways by a fraction ACROSS
of the distance between them, to the right of the way from FROM to TO.

POINT: unused; the point is worked out from FROM and TO.
FROM: a cons of x and y.
TO: a cons of x and y.
ALONG: the fraction of the way, 0 at FROM and 1 at TO.
ACROSS: the fraction of the distance to move to the right of the way."
  (declare (ignore point))
  (let* ((dx (- (car to) (car from)))
         (dy (- (cdr to) (cdr from))))
    (cons (+ (car from) (* along dx) (* across dy))
          (+ (cdr from) (* along dy) (* (- across) dx)))))

(defun draw-side (pen
                  side
                  length
                  direction)
  "Draw one edge of a leaf from its base up to its tip, with the pen. DIRECTION is 1 for the right edge and -1 for the
left, which is drawn with the points mirrored across the midrib. The lobes fan out: the lowest points nearly sideways,
the highest nearly up, so that each lobe clears the one above it.

PEN: a pen at the base of the leaf.
SIDE: what the edge is like.
LENGTH: the length of the leaf.
DIRECTION: 1 or -1."
  (let* ((lobes (side-lobes side))
         (count (1+ lobes))
         (last-notch nil))
    (flet ((mirror (point)
             (cons (* direction (car point)) (cdr point))))
      (dotimes (index lobes)
        (let* ((below (lobe-base-height index
                                        count
                                        length))
               (above (lobe-base-height (1+ index)
                                        count
                                        length))
               (middle (/ (+ below above) 2))
               (reach (half-width-at (/ middle length) (side-width side)))
               (angle (* (/ pi 180)
                         (+ (side-lowest side)
                            (* (- (side-highest side) (side-lowest side))
                               (if (> lobes 1)
                                   (/ index (float (1- lobes) 1d0))
                                   0d0)))))
               (notch (cons (* reach (side-depth side)) below))
               (run (- reach (car notch)))
               (tip (cons reach (min (+ below (* run (tan angle)))
                                     (* 0.95d0 length))))
               (next-notch (cons (* (half-width-at (/ above length) (side-width side)) (side-depth side))
                                 above)))
          (when (zerop index)
            (pen-line-to pen (mirror notch)))
          ;; the lower edge: out from the notch, bellying outward and downward, then up to the tip
          (pen-curve-to pen
                        (mirror (offset-point nil
                                              notch
                                              tip
                                              0.30d0
                                              (* (side-belly side) (if (zerop index) 0.3d0 1d0))))
                        (mirror (offset-point nil
                                              notch
                                              tip
                                              0.72d0
                                              (* 0.6d0 (side-belly side))))
                        (mirror tip))
          ;; the upper edge: back from the tip into the notch of the next lobe, hollowed
          (pen-curve-to pen
                        (mirror (offset-point nil
                                              tip
                                              next-notch
                                              0.30d0
                                              (side-scoop side)))
                        (mirror (offset-point nil
                                              tip
                                              next-notch
                                              0.70d0
                                              (* 0.5d0 (side-scoop side))))
                        (mirror next-notch))
          (setf last-notch next-notch)))
      ;; the tip of the leaf: from the last notch up to a point on the midrib
      (let ((gap (- length (cdr last-notch))))
        (pen-curve-to pen
                      (mirror (point+ last-notch
                                      (* -0.05d0 (car last-notch))
                                      (* 0.45d0 gap)))
                      (mirror (cons (* 0.35d0 (car last-notch)) (- length (* 0.25d0 gap))))
                      (cons 0d0 length))))))

(defun leaf-outline (right
                     left
                     &optional (length +leaf-length+))
  "Return the closed contour that is the edge of a leaf: up the right side to the tip and down the left.

RIGHT: what the right edge is like.
LEFT: what the left edge is like.
LENGTH: the length of the leaf."
  (let ((up (make-pen (cons 0d0 0d0)))
        (down (make-pen (cons 0d0 0d0))))
    (draw-side up
               right
               length
               1)
    (draw-side down
               left
               length
               -1)
    (make-contour (append (reverse (pen-points up))
                          (butlast (rest (pen-points down))))
                  t)))

(defun midrib (length
               bend)
  "Return the contour of the midrib of a leaf: the line from its base to its tip, curved a little toward one side.

LENGTH: the length of the leaf.
BEND: how far the middle of the midrib is from the straight line, in units; negative bends it the other way."
  (let ((pen (make-pen (cons 0d0 0d0))))
    (pen-curve-to pen
                  (cons (* 0.9d0 bend) (* 0.30d0 length))
                  (cons (* 0.9d0 bend) (* 0.70d0 length))
                  (cons 0d0 (* 0.985d0 length)))
    (pen-contour pen)))

(defun leaf-veins (side
                   length
                   direction)
  "Return the contours of the veins of one edge of a leaf: from the midrib, in each lobe, up toward its tip.

SIDE: what the edge is like.
LENGTH: the length of the leaf.
DIRECTION: 1 for the right edge and -1 for the left."
  (let* ((lobes (side-lobes side))
         (count (1+ lobes)))
    (loop for index from 0 below lobes
          collect (let* ((below (lobe-base-height index
                                                  count
                                                  length))
                         (above (lobe-base-height (1+ index)
                                                  count
                                                  length))
                         (middle (/ (+ below above) 2))
                         (reach (half-width-at (/ middle length) (side-width side)))
                         (angle (* (/ pi 180)
                                   (+ (side-lowest side)
                                      (* (- (side-highest side) (side-lowest side))
                                         (if (> lobes 1)
                                             (/ index (float (1- lobes) 1d0))
                                             0d0)))))
                         (start (cons 0d0 (+ below (* 0.20d0 (- above below)))))
                         (end (cons (* direction 0.62d0 reach)
                                    (min (+ (cdr start) (* 0.62d0 reach (tan angle))) (* 0.9d0 length))))
                         (pen (make-pen start)))
                    (pen-curve-to pen
                                  (offset-point nil
                                                start
                                                end
                                                0.35d0
                                                (* direction -0.05d0))
                                  (offset-point nil
                                                start
                                                end
                                                0.70d0
                                                (* direction -0.03d0))
                                  end)
                    (pen-contour pen)))))

;;; Moving a shape about

(defun map-points (function
                   contours)
  "Return contours with every point replaced by what FUNCTION makes of it.

FUNCTION: function of a cons of x and y that returns a cons.
CONTOURS: a list of contours."
  (mapcar (lambda (contour)
            (make-contour (mapcar function (contour-points contour))
                          (contour-closed contour)))
          contours))

(defun sweep-contours (contours
                       sweep
                       length)
  "Return contours bent to one side the way a leaf curls: every point moves sideways by a distance that grows with
the square of its height, up to SWEEP at the top.

CONTOURS: a list of contours.
SWEEP: how far the top moves sideways; negative bends the other way.
LENGTH: the height of the leaf."
  (map-points (lambda (point)
                (let ((fraction (/ (cdr point) length)))
                  (cons (+ (car point) (* sweep fraction fraction))
                        (cdr point))))
              contours))

(defun turn-contours (contours
                      degrees
                      about)
  "Return contours turned counter-clockwise about a point.

CONTOURS: a list of contours.
DEGREES: the angle.
ABOUT: the point, a cons of x and y."
  (let ((cosine (cos (* (/ pi 180) degrees)))
        (sine (sin (* (/ pi 180) degrees))))
    (map-points (lambda (point)
                  (let ((dx (- (car point) (car about)))
                        (dy (- (cdr point) (cdr about))))
                    (cons (+ (car about) (- (* dx cosine) (* dy sine)))
                          (+ (cdr about) (* dx sine) (* dy cosine)))))
                contours)))

(defun shift-contours (contours
                       dx
                       dy)
  "Return contours moved by a distance along x and a distance along y.

CONTOURS: a list of contours.
DX: the distance along x.
DY: the distance along y."
  (map-points (lambda (point)
                (point+ point
                        dx
                        dy))
              contours))

(defun mirror-contours (contours)
  "Return contours reflected across the vertical axis.

CONTOURS: a list of contours."
  (mapcar (lambda (contour)
            (make-contour (reverse (mapcar (lambda (point)
                                             (cons (- (car point)) (cdr point)))
                                           (contour-points contour)))
                          (contour-closed contour)))
          contours))

(defun fit-contours (contours)
  "Return contours scaled so that they are 100 units high, and moved so that their lowest point is on the axis of x:
the base of a shape is where it is placed.

CONTOURS: a list of contours."
  (let* ((points (loop for contour in contours
                       append (contour-points contour)))
         (low (loop for point in points minimize (cdr point)))
         (high (loop for point in points maximize (cdr point)))
         (factor (/ +leaf-length+ (- high low))))
    (map-points (lambda (point)
                  (cons (* factor (car point))
                        (* factor (- (cdr point) low))))
                contours)))

;;; Whole leaves

(defun leaf-contours (right
                      left
                      &key veins
                           (sweep 0d0)
                           (length +leaf-length+))
  "Return the contours of a leaf: its edge, its midrib and, when asked for, the veins of its lobes.

RIGHT: what the right edge is like.
LEFT: what the left edge is like.
VEINS: true to draw the veins.
SWEEP: how far the tip of the leaf curls to the side, in units.
LENGTH: the length of the leaf."
  (sweep-contours (append (list (leaf-outline right
                                              left
                                              length)
                                (midrib length 0d0))
                          (and veins (leaf-veins right
                                                 length
                                                 1))
                          (and veins (leaf-veins left
                                                 length
                                                 -1)))
                  sweep
                  length))

(defun volute-contour (center-x
                       center-y
                       outer
                       inner
                       turns
                       start-degrees)
  "Return the contour of a spiral that winds in from a radius to a smaller one, clockwise.

CENTER-X: x of the middle of the spiral.
CENTER-Y: y of the middle.
OUTER: the radius it begins at.
INNER: the radius it ends at.
TURNS: how many times it goes round.
START-DEGREES: the direction of its first point from the middle."
  (let ((steps (ceiling (* turns 36))))
    (make-contour (loop for step from 0 to steps
                        collect (let* ((fraction (/ step (float steps 1d0)))
                                       (radius (- outer (* (- outer inner) fraction)))
                                       (angle (* (/ pi 180) (- start-degrees (* fraction turns 360)))))
                                  (cons (+ center-x (* radius (cos angle)))
                                        (+ center-y (* radius (sin angle)))))))))

(defun scroll-leaf-contours (right
                             left
                             &key veins
                                  (side 1))
  "Return the contours of a leaf on a stem that ends in a volute: the way ironwork ends a scroll. The leaf rises from
the end of the stem, and the stem curves out and winds down and in to the side SIDE.

RIGHT: what the right edge of the leaf is like.
LEFT: what the left edge is like.
VEINS: true to draw the veins.
SIDE: 1 for a volute on the right, -1 on the left."
  (let* ((leaf (shift-contours (leaf-contours right
                                              left
                                              :veins
                                              veins
                                              :length
                                              60d0)
                               0d0
                               40d0))
         (spiral (volute-contour 15d0
                                 20d0
                                 15d0
                                 2.6d0
                                 1.75d0
                                 90d0))
         (stem (let ((pen (make-pen (cons 0d0 40d0))))
                 (pen-curve-to pen
                               (cons 3d0 40d0)
                               (cons 9d0 35.5d0)
                               (first (contour-points spiral)))
                 (pen-contour pen)))
         (contours (append leaf (list stem spiral))))
    (if (plusp side)
        contours
        (mirror-contours contours))))

(defun leaf-pair-contours (right
                           left
                           &key veins
                                (spread 30d0))
  "Return the contours of two leaves that rise from the same base and lean away from each other.

RIGHT: what the right edge of a leaf is like.
LEFT: what the left edge is like.
VEINS: true to draw the veins.
SPREAD: how far each leaf leans from the vertical, in degrees."
  (let ((origin (cons 0d0 0d0))
        (leaf (leaf-contours right
                             left
                             :veins
                             veins
                             :length
                             78d0)))
    (append (turn-contours leaf
                           (- spread)
                           origin)
            (turn-contours leaf
                           spread
                           origin))))
