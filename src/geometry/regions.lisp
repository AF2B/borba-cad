(in-package #:borba-cad/geometry)

;;;; Regions: shapes made of closed loops, and what joins, cuts and overlaps them. A region is a list of loops, each a
;;;; flat array x0 y0 x1 y1 ... of the vertices of a simple polygon, in the direction that has the inside on the
;;;; left: an outer boundary goes counter-clockwise and a hole goes clockwise. The three operations work on the
;;;; edges: every edge is cut where the other region's edges cross it or touch it, each piece is judged inside, outside
;;;; or on the boundary of the other region, the pieces that the operation keeps are chosen, and they are joined end to
;;;; end into the loops of the result.

(defconstant +vertex-cell-factor+ 8d0
  "How many times the length tolerance the cells are wide that vertices are looked up in: points closer than the
tolerance are the same vertex.")

(defstruct (region-edge (:constructor make-region-edge (x1
                                                        y1
                                                        x2
                                                        y2)))
  "A straight piece of the boundary of a region, from its start to its end, with the inside on its left.

X1: x of the start.
Y1: y of the start.
X2: x of the end.
Y2: y of the end.
SPLITS: where other edges cross or touch it, as a list of (parameter x . y), the parameter running from 0 at the start
to 1 at the end."
  (x1 0d0 :type double-float)
  (y1 0d0 :type double-float)
  (x2 0d0 :type double-float)
  (y2 0d0 :type double-float)
  (splits '() :type list))

(defun region-edges (loops)
  "Return the edges of the loops of a region, leaving out those of no length.

LOOPS: a list of flat arrays of vertices."
  (let ((edges '()))
    (dolist (loop-coordinates loops)
      (let ((count (floor (length loop-coordinates) 2)))
        (dotimes (index count)
          (let* ((next (mod (1+ index) count))
                 (x1 (aref loop-coordinates (* 2 index)))
                 (y1 (aref loop-coordinates (1+ (* 2 index))))
                 (x2 (aref loop-coordinates (* 2 next)))
                 (y2 (aref loop-coordinates (1+ (* 2 next)))))
            (when (> (+ (abs (- x2 x1)) (abs (- y2 y1))) +length-tolerance+)
              (push (make-region-edge x1
                                      y1
                                      x2
                                      y2) edges))))))
    (nreverse edges)))

(defun region-winding-number (x
                              y
                              loops)
  "Return how many times the loops of a region go around a point, counter-clockwise ones counting plus one: the point is
inside the region when this is not zero.

X: x of the point.
Y: y of the point.
LOOPS: a list of flat arrays of vertices."
  (let ((winding 0))
    (dolist (loop-coordinates loops winding)
      (let ((count (floor (length loop-coordinates) 2)))
        (dotimes (index count)
          (let* ((next (mod (1+ index) count))
                 (x1 (aref loop-coordinates (* 2 index)))
                 (y1 (aref loop-coordinates (1+ (* 2 index))))
                 (x2 (aref loop-coordinates (* 2 next)))
                 (y2 (aref loop-coordinates (1+ (* 2 next))))
                 (side (- (* (- x2 x1) (- y y1)) (* (- x x1) (- y2 y1)))))
            (cond ((<= y1 y)
                   (when (and (> y2 y) (> side 0d0))
                     (incf winding)))
                  (t
                   (when (and (<= y2 y) (< side 0d0))
                     (decf winding))))))))))

(defun add-split (edge
                  parameter
                  x
                  y)
  "Record that something crosses or touches an edge at a parameter, at a point.

EDGE: a region edge.
PARAMETER: where along it, from 0 at its start to 1 at its end.
X: x of the point.
Y: y of the point."
  (push (list parameter x y) (region-edge-splits edge)))

(defun split-edge-pair (a
                        b)
  "Record where the edge A and the edge B cross, touch or overlap, as splits of each: only points that are inside an
edge are splits of it, because its end is already a vertex. A point that is where an edge ends is the very point of that
edge, so that both get the same coordinates.

A: a region edge.
B: a region edge."
  (let* ((x1 (region-edge-x1 a))
         (y1 (region-edge-y1 a))
         (x2 (region-edge-x2 a))
         (y2 (region-edge-y2 a))
         (x3 (region-edge-x1 b))
         (y3 (region-edge-y1 b))
         (x4 (region-edge-x2 b))
         (y4 (region-edge-y2 b))
         (dx1 (- x2 x1))
         (dy1 (- y2 y1))
         (dx2 (- x4 x3))
         (dy2 (- y4 y3))
         (length-a (sqrt (+ (* dx1 dx1) (* dy1 dy1))))
         (length-b (sqrt (+ (* dx2 dx2) (* dy2 dy2))))
         (epsilon-a (/ +length-tolerance+ length-a))
         (epsilon-b (/ +length-tolerance+ length-b))
         (cross (- (* dx1 dy2) (* dy1 dx2))))
    (if (> (abs cross) (* 1d-12 length-a length-b))
        (let* ((ex (- x3 x1))
               (ey (- y3 y1))
               (ta (/ (- (* ex dy2) (* ey dx2)) cross))
               (tb (/ (- (* ex dy1) (* ey dx1)) cross)))
          (when (and (<= (- epsilon-a) ta (+ 1d0 epsilon-a))
                     (<= (- epsilon-b) tb (+ 1d0 epsilon-b)))
            (let ((inside-a (< epsilon-a ta (- 1d0 epsilon-a)))
                  (inside-b (< epsilon-b tb (- 1d0 epsilon-b))))
              (cond ((and inside-a inside-b)
                     (let ((x (+ x1 (* ta dx1)))
                           (y (+ y1 (* ta dy1))))
                       (add-split a
                                  ta
                                  x
                                  y)
                       (add-split b
                                  tb
                                  x
                                  y)))
                    (inside-b
                     (add-split b
                                tb
                                (if (< ta 0.5d0) x1 x2)
                                (if (< ta 0.5d0) y1 y2)))
                    (inside-a
                     (add-split a
                                ta
                                (if (< tb 0.5d0) x3 x4)
                                (if (< tb 0.5d0) y3 y4)))))))
        (when (< (/ (abs (- (* (- x3 x1) dy1) (* (- y3 y1) dx1))) length-a) +length-tolerance+)
          (flet ((along-a (x y)
                   (/ (+ (* (- x x1) dx1) (* (- y y1) dy1)) (* length-a length-a)))
                 (along-b (x y)
                   (/ (+ (* (- x x3) dx2) (* (- y y3) dy2)) (* length-b length-b))))
            (let ((s3 (along-a x3 y3))
                  (s4 (along-a x4 y4))
                  (r1 (along-b x1 y1))
                  (r2 (along-b x2 y2)))
              (when (< epsilon-a s3 (- 1d0 epsilon-a))
                (add-split a
                           s3
                           x3
                           y3))
              (when (< epsilon-a s4 (- 1d0 epsilon-a))
                (add-split a
                           s4
                           x4
                           y4))
              (when (< epsilon-b r1 (- 1d0 epsilon-b))
                (add-split b
                           r1
                           x1
                           y1))
              (when (< epsilon-b r2 (- 1d0 epsilon-b))
                (add-split b
                           r2
                           x2
                           y2))))))))

(defun edge-pieces (edge)
  "Return the pieces that an edge is cut into by its splits, as a list of edges in order along it.

EDGE: a region edge."
  (let ((points (list (list 0d0 (region-edge-x1 edge) (region-edge-y1 edge))))
        (pieces '()))
    (dolist (split (sort (copy-list (region-edge-splits edge)) #'< :key #'first))
      (push split points))
    (push (list 1d0 (region-edge-x2 edge) (region-edge-y2 edge)) points)
    (setf points (nreverse points))
    (loop for (from to) on points
          while to
          do (when (> (+ (abs (- (second to) (second from))) (abs (- (third to) (third from)))) +length-tolerance+)
               (push (make-region-edge (second from)
                                       (third from)
                                       (second to)
                                       (third to)) pieces)))
    (nreverse pieces)))

(defun distance-to-edge (x
                         y
                         edge)
  "Return how far a point is from an edge, and the way the edge runs as a second and third value, a unit vector.

X: x of the point.
Y: y of the point.
EDGE: a region edge."
  (let* ((x1 (region-edge-x1 edge))
         (y1 (region-edge-y1 edge))
         (dx (- (region-edge-x2 edge) x1))
         (dy (- (region-edge-y2 edge) y1))
         (length-squared (+ (* dx dx) (* dy dy)))
         (along (max 0d0 (min 1d0 (/ (+ (* (- x x1) dx) (* (- y y1) dy)) length-squared))))
         (nearest-x (+ x1 (* along dx)))
         (nearest-y (+ y1 (* along dy)))
         (length (sqrt length-squared)))
    (values (sqrt (+ (expt (- x nearest-x) 2) (expt (- y nearest-y) 2)))
            (/ dx length)
            (/ dy length))))

(defun classify-piece (piece
                       other-pieces
                       other-loops)
  "Say where a piece of an edge is with respect to another region: :inside, :outside, :same when it lies along an edge
of the other region that runs the same way, and :opposite when it runs the other way.

PIECE: a region edge.
OTHER-PIECES: the pieces of the edges of the other region.
OTHER-LOOPS: the loops of the other region."
  (let* ((x (/ (+ (region-edge-x1 piece) (region-edge-x2 piece)) 2))
         (y (/ (+ (region-edge-y1 piece) (region-edge-y2 piece)) 2))
         (dx (- (region-edge-x2 piece) (region-edge-x1 piece)))
         (dy (- (region-edge-y2 piece) (region-edge-y1 piece)))
         (length (sqrt (+ (* dx dx) (* dy dy)))))
    (dolist (other other-pieces)
      (multiple-value-bind (distance direction-x direction-y) (distance-to-edge x
                                                                                y
                                                                                other)
        (when (< distance +length-tolerance+)
          (return-from classify-piece
            (if (plusp (+ (* (/ dx length) direction-x) (* (/ dy length) direction-y)))
                :same
                :opposite)))))
    (if (zerop (region-winding-number x
                                      y
                                      other-loops))
        :outside
        :inside)))

(defun reversed-edge (edge)
  "Return an edge that runs the other way.

EDGE: a region edge."
  (make-region-edge (region-edge-x2 edge)
                    (region-edge-y2 edge)
                    (region-edge-x1 edge)
                    (region-edge-y1 edge)))

(defun kept-edges (operation
                   pieces-a
                   pieces-b
                   loops-a
                   loops-b)
  "Return the pieces that make the boundary of the result of an operation, each running with the inside on its left.

OPERATION: :union, :intersection or :difference (the first region without the second).
PIECES-A: the pieces of the edges of the first region.
PIECES-B: the pieces of the edges of the second.
LOOPS-A: the loops of the first.
LOOPS-B: the loops of the second."
  (let ((kept '()))
    (dolist (piece pieces-a)
      (let ((where (classify-piece piece
                                   pieces-b
                                   loops-b)))
        (when (ecase operation
                (:union (member where '(:outside :same)))
                (:intersection (member where '(:inside :same)))
                (:difference (member where '(:outside :opposite))))
          (push piece kept))))
    (dolist (piece pieces-b)
      (let ((where (classify-piece piece
                                   pieces-a
                                   loops-a)))
        (ecase operation
          (:union (when (eq where :outside)
                    (push piece kept)))
          (:intersection (when (eq where :inside)
                           (push piece kept)))
          (:difference (when (eq where :inside)
                         (push (reversed-edge piece) kept))))))
    (nreverse kept)))

;;; Joining the pieces into loops

(defstruct (vertex-table (:constructor make-vertex-table ()))
  "The vertices of the result, with the points that are as good as one found as one.

POINTS: the vertices, as a vector of conses of x and y.
CELLS: a hash table from a cell of the plane to the vertices in it."
  (points (make-array 0 :adjustable t :fill-pointer t))
  (cells (make-hash-table :test 'equal)))

(defun vertex-id (table
                  x
                  y)
  "Return the number of the vertex at a point, making one when none is within the tolerance.

TABLE: a vertex table.
X: x of the point.
Y: y of the point."
  (let* ((size (* +vertex-cell-factor+ +length-tolerance+))
         (cell-x (floor x size))
         (cell-y (floor y size)))
    (loop for around-x from (1- cell-x) to (1+ cell-x)
          do (loop for around-y from (1- cell-y) to (1+ cell-y)
                   do (dolist (id (gethash (cons around-x around-y) (vertex-table-cells table)))
                        (let ((point (aref (vertex-table-points table) id)))
                          (when (and (< (abs (- (car point) x)) (* 2 +length-tolerance+))
                                     (< (abs (- (cdr point) y)) (* 2 +length-tolerance+)))
                            (return-from vertex-id id))))))
    (let ((id (vector-push-extend (cons x y) (vertex-table-points table))))
      (push id (gethash (cons cell-x cell-y) (vertex-table-cells table)))
      id)))

(defun turn-angle (from-x
                   from-y
                   to-x
                   to-y)
  "Return the angle that a way of going turns through to another, from minus pi to pi, counter-clockwise positive.

FROM-X: x of the first direction.
FROM-Y: y of the first direction.
TO-X: x of the second direction.
TO-Y: y of the second direction."
  (atan (- (* from-x to-y) (* from-y to-x))
        (+ (* from-x to-x) (* from-y to-y))))

(defun join-edges (edges)
  "Join directed edges end to end into loops, as flat arrays of vertices. Where several edges leave a vertex the one
that turns the most clockwise is taken, which keeps loops that touch at a point apart. An edge that leads nowhere is
left out.

EDGES: a list of region edges."
  (let* ((table (make-vertex-table))
         (starts (make-hash-table))
         (links (loop for edge in edges
                      collect (list (vertex-id table
                                               (region-edge-x1 edge)
                                               (region-edge-y1 edge))
                                    (vertex-id table
                                               (region-edge-x2 edge)
                                               (region-edge-y2 edge))
                                    nil)))
         (loops '()))
    (dolist (link links)
      (push link (gethash (first link) starts)))
    (dolist (first links)
      (unless (or (third first) (= (first first) (second first)))
        (let ((chain (list first))
              (current first)
              (closed nil))
          (setf (third first) t)
          (loop
            (let ((vertex (second current)))
              (when (= vertex (first first))
                (setf closed t)
                (return))
              (let ((candidates (remove-if #'third (gethash vertex starts))))
                (when (null candidates)
                  (return))
                (let* ((incoming-from (aref (vertex-table-points table) (first current)))
                       (incoming-to (aref (vertex-table-points table) vertex))
                       (in-x (- (car incoming-to) (car incoming-from)))
                       (in-y (- (cdr incoming-to) (cdr incoming-from)))
                       (best nil)
                       (best-angle nil))
                  (dolist (candidate candidates)
                    (let* ((target (aref (vertex-table-points table) (second candidate)))
                           (angle (turn-angle in-x
                                              in-y
                                              (- (car target) (car incoming-to))
                                              (- (cdr target) (cdr incoming-to)))))
                      (when (or (null best-angle) (< angle best-angle))
                        (setf best candidate
                              best-angle angle))))
                  (setf (third best) t)
                  (push best chain)
                  (setf current best)))))
          (when closed
            (let ((points (loop for link in (reverse chain)
                                collect (aref (vertex-table-points table) (first link)))))
              (when (>= (length points) 3)
                (push (make-array (* 2 (length points))
                                  :element-type 'double-float
                                  :initial-contents (loop for point in points
                                                          append (list (car point) (cdr point))))
                      loops)))))))
    (nreverse loops)))

(defun clean-loop (loop-coordinates)
  "Return a loop without the vertices that are on a straight line between their neighbors, or NIL when what is left is
not a polygon with an area.

LOOP-COORDINATES: a flat array of vertices."
  (let ((points (loop for index from 0 below (floor (length loop-coordinates) 2)
                      collect (cons (aref loop-coordinates (* 2 index)) (aref loop-coordinates (1+ (* 2 index)))))))
    (loop
      (let ((count (length points))
            (removed nil))
        (when (< count 3)
          (return-from clean-loop nil))
        (dotimes (index count)
          (let* ((before (nth (mod (1- index) count) points))
                 (here (nth index points))
                 (after (nth (mod (1+ index) count) points))
                 (dx1 (- (car here) (car before)))
                 (dy1 (- (cdr here) (cdr before)))
                 (dx2 (- (car after) (car here)))
                 (dy2 (- (cdr after) (cdr here)))
                 (cross (- (* dx1 dy2) (* dy1 dx2)))
                 (scale (* (sqrt (+ (* dx1 dx1) (* dy1 dy1))) (sqrt (+ (* dx2 dx2) (* dy2 dy2))))))
            (when (or (< scale +length-tolerance+)
                      (< (abs cross) (* 1d-9 scale)))
              (setf points (append (subseq points 0 index) (subseq points (1+ index)))
                    removed t)
              (return))))
        (unless removed
          (return))))
    (let ((cleaned (make-array (* 2 (length points))
                               :element-type 'double-float
                               :initial-contents (loop for point in points
                                                       append (list (car point) (cdr point))))))
      (and (> (abs (contour-signed-area cleaned)) (* 100 +length-tolerance+ +length-tolerance+))
           cleaned))))

(defun region-boolean (operation
                       a
                       b)
  "Return the region that results from joining two regions, from what they have in common, or from taking the second
from the first, as a list of loops: the outer boundaries go counter-clockwise and the holes clockwise. The result is
empty, NIL, when nothing is left.

OPERATION: :union, :intersection or :difference.
A: the first region, a list of loops with the inside on their left.
B: the second region."
  (let ((edges-a (region-edges a))
        (edges-b (region-edges b)))
    (dolist (edge-a edges-a)
      (dolist (edge-b edges-b)
        (split-edge-pair edge-a edge-b)))
    (let* ((pieces-a (loop for edge in edges-a append (edge-pieces edge)))
           (pieces-b (loop for edge in edges-b append (edge-pieces edge)))
           (kept (kept-edges operation
                             pieces-a
                             pieces-b
                             a
                             b)))
      (loop for joined in (join-edges kept)
            for cleaned = (clean-loop joined)
            when cleaned
              collect cleaned))))

(defun polygon-region (coordinates)
  "Return the region that a polygon makes: its loop going counter-clockwise, whichever way the polygon was given.

COORDINATES: a flat array of vertices."
  (let ((copy (copy-seq coordinates)))
    (if (minusp (contour-signed-area copy))
        (let* ((count (floor (length copy) 2))
               (reversed (make-array (length copy) :element-type 'double-float)))
          (dotimes (index count)
            (let ((source (- count 1 index)))
              (setf (aref reversed (* 2 index)) (aref copy (* 2 source))
                    (aref reversed (1+ (* 2 index))) (aref copy (1+ (* 2 source))))))
          (list reversed))
        (list copy))))

(defun region-area (loops)
  "Return the area of a region: its outer boundaries minus its holes.

LOOPS: a list of flat arrays of vertices, with the inside on their left."
  (loop for loop-coordinates in loops
        sum (contour-signed-area loop-coordinates)))
