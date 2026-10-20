(in-package #:borba-cad/plot)

;;;; Cutting the items of a plot to a rectangle, which is how a viewport of a sheet shows only its window of the
;;;; drawing: a stroke is cut into the runs that lie inside, a filled polygon into the polygon that lies inside, and a
;;;; text is kept when it starts inside.

(defconstant +join-tolerance+ 1d-9
  "How near, in millimetres, the end of one clipped segment and the start of the next must be to be one run.")

(defun near-point-p (point
                     x
                     y)
  "Return true when a point is at (X, Y), to the tolerance of joining runs.

POINT: an (x . y) cons.
X: x of the place.
Y: y of the place."
  (and (< (abs (- (car point) x)) +join-tolerance+)
       (< (abs (- (cdr point) y)) +join-tolerance+)))

(defun clip-stroke (stroke
                    bounds)
  "Return the strokes that are what lies of a stroke inside a rectangle: the stroke itself when all of it does, and
otherwise the runs of it that do, each its own stroke, which are none when none of it does.

STROKE: a plot stroke.
BOUNDS: the rectangle, as bounds in sheet millimetres."
  (let* ((points (plot-stroke-points stroke))
         (closed (plot-stroke-closed stroke)))
    (cond ((null (cdr points))
           '())
          ((every (lambda (point)
                    (geo:bounds-contains-point-p bounds
                                                 (car point)
                                                 (cdr point)))
                  points)
           (list stroke))
          (t
           (let ((runs '())
                 (current '())
                 (path (if closed
                           (append points (list (first points)))
                           points)))
             (flet ((flush ()
                      (when (cdr current)
                        (push (make-plot-stroke (reverse current)
                                                nil
                                                (plot-stroke-color stroke)
                                                (plot-stroke-width stroke))
                              runs))
                      (setf current '())))
               (loop for (from to) on path
                     while to
                     do (multiple-value-bind (x1 y1 x2 y2) (geo:clip-segment-to-bounds (float (car from) 1d0)
                                                                                       (float (cdr from) 1d0)
                                                                                       (float (car to) 1d0)
                                                                                       (float (cdr to) 1d0)
                                                                                       bounds)
                          (cond ((null x1)
                                 (flush))
                                (t
                                 (unless (and current (near-point-p (first current)
                                                                    x1
                                                                    y1))
                                   (flush)
                                   (push (cons x1 y1) current))
                                 (push (cons x2 y2) current)
                                 (unless (near-point-p to
                                                       x2
                                                       y2)
                                   (flush))))))
               (flush))
             (nreverse runs))))))

(defun clip-edge-inside-p (edge
                           bounds
                           x
                           y)
  "Return true when a point is on the inner side of an edge of the rectangle.

EDGE: :left, :right, :bottom or :top.
BOUNDS: the rectangle.
X: x of the point.
Y: y of the point."
  (ecase edge
    (:left (>= x (geo:bounds-min-x bounds)))
    (:right (<= x (geo:bounds-max-x bounds)))
    (:bottom (>= y (geo:bounds-min-y bounds)))
    (:top (<= y (geo:bounds-max-y bounds)))))

(defun clip-edge-crossing (edge
                           bounds
                           from
                           to)
  "Return where the segment between two points crosses the line of an edge, as a cons.

EDGE: :left, :right, :bottom or :top.
BOUNDS: the rectangle.
FROM: an (x . y) cons.
TO: an (x . y) cons."
  (let ((x1 (car from))
        (y1 (cdr from))
        (x2 (car to))
        (y2 (cdr to)))
    (ecase edge
      ((:left :right)
       (let* ((x (if (eq edge :left) (geo:bounds-min-x bounds) (geo:bounds-max-x bounds)))
              (fraction (/ (- x x1) (- x2 x1))))
         (cons x (+ y1 (* fraction (- y2 y1))))))
      ((:bottom :top)
       (let* ((y (if (eq edge :bottom) (geo:bounds-min-y bounds) (geo:bounds-max-y bounds)))
              (fraction (/ (- y y1) (- y2 y1))))
         (cons (+ x1 (* fraction (- x2 x1))) y))))))

(defun clip-polygon-edge (points
                          edge
                          bounds)
  "Cut a polygon to one side of the rectangle (Sutherland and Hodgman).

POINTS: the vertices, as (x . y) conses.
EDGE: :left, :right, :bottom or :top.
BOUNDS: the rectangle."
  (let ((result '()))
    (loop for current in points
          for previous = (car (last points)) then previous-point
          for previous-point = current
          do (let ((current-inside (clip-edge-inside-p edge
                                                       bounds
                                                       (car current)
                                                       (cdr current)))
                   (previous-inside (clip-edge-inside-p edge
                                                        bounds
                                                        (car previous)
                                                        (cdr previous))))
               (cond ((and current-inside previous-inside)
                      (push current result))
                     ((and current-inside (not previous-inside))
                      (push (clip-edge-crossing edge
                                                bounds
                                                previous
                                                current) result)
                      (push current result))
                     ((and (not current-inside) previous-inside)
                      (push (clip-edge-crossing edge
                                                bounds
                                                previous
                                                current) result)))))
    (nreverse result)))

(defun clip-fill (fill
                  bounds)
  "Return the filled polygon that lies inside a rectangle, or NIL when none of it does.

FILL: a plot fill.
BOUNDS: the rectangle."
  (let ((points (plot-fill-points fill)))
    (dolist (edge '(:left :right :bottom :top))
      (when points
        (setf points (clip-polygon-edge points
                                        edge
                                        bounds))))
    (and (>= (length points) 3)
         (make-plot-fill points (plot-fill-color fill)))))

(defun clip-item (item
                  bounds)
  "Return what lies inside a rectangle of an item of a plot, as a list.

ITEM: a plot stroke, fill or text.
BOUNDS: the rectangle, as bounds in sheet millimetres."
  (etypecase item
    (plot-stroke (clip-stroke item bounds))
    (plot-fill (let ((clipped (clip-fill item bounds)))
                 (and clipped (list clipped))))
    (plot-text (and (geo:bounds-contains-point-p bounds
                                                 (float (plot-text-x item) 1d0)
                                                 (float (plot-text-y item) 1d0))
                    (list item)))))
