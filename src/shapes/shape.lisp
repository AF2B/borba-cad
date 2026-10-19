(in-package #:borba-cad/shapes)

;;;; A shape: a list of contours, each a polyline, open or closed, in the units of the shape, which is 100 units from
;;;; its base to its top, with its base at the origin and its middle on the vertical axis. It is turned into a block of
;;;; polylines when it is placed, and a block is one placement in the drawing that the user can scale, turn, flip and
;;;; explode.

(defstruct (contour (:constructor make-contour (points
                                                &optional closed)))
  "One polyline of a shape.

POINTS: a list of conses of x and y.
CLOSED: true when the last point joins the first."
  (points '() :type list)
  (closed nil :type boolean))

(defstruct (shape (:constructor make-shape (id
                                            name
                                            contours
                                            &optional name-argument)))
  "A shape of the library.

ID: a string that identifies the shape inside its group.
NAME: the English name of the shape, translated when shown; it may have a ~d for the NAME-ARGUMENT.
CONTOURS: a list of contours.
NAME-ARGUMENT: the number that the name asks for, or NIL.
PREVIEW: the contours cut down to the few points that a small picture needs, made the first time they are asked for."
  id
  name
  (contours '() :type list)
  name-argument
  (preview nil :type list))

(defstruct (group (:constructor make-group (id
                                            title
                                            shapes)))
  "Shapes that belong together, such as the acanthus leaves.

ID: a string that identifies the group inside its area.
TITLE: the English title of the group, translated when shown.
SHAPES: the shapes, in the order they are shown."
  id
  title
  (shapes '() :type list))

(defstruct (area (:constructor make-area (id
                                          title
                                          groups)))
  "A trade or a field that has shapes, such as ironwork.

ID: a string that identifies the area.
TITLE: the English title of the area, translated when shown.
GROUPS: the groups, in the order they are shown."
  id
  title
  (groups '() :type list))

(defun shape-bounds (shape)
  "Return the least and the greatest x and y of the points of a shape, as four values, or NIL when it has none.

SHAPE: a shape."
  (let ((points (loop for contour in (shape-contours shape)
                      append (contour-points contour))))
    (when points
      (values (loop for point in points minimize (car point))
              (loop for point in points minimize (cdr point))
              (loop for point in points maximize (car point))
              (loop for point in points maximize (cdr point))))))

(defun shape-height (shape)
  "Return the height of a shape, from its lowest point to its highest.

SHAPE: a shape."
  (multiple-value-bind (min-x min-y max-x max-y) (shape-bounds shape)
    (declare (ignore min-x max-x))
    (if min-y
        (- max-y min-y)
        0d0)))

(defun shape-width (shape)
  "Return the width of a shape, from its leftmost point to its rightmost.

SHAPE: a shape."
  (multiple-value-bind (min-x min-y max-x max-y) (shape-bounds shape)
    (declare (ignore min-y max-y))
    (if min-x
        (- max-x min-x)
        0d0)))

(defun shape-title (shape)
  "Return the name of a shape as it is shown, in the language of the interface.

SHAPE: a shape."
  (if (shape-name-argument shape)
      (i18n:trf (shape-name shape) (shape-name-argument shape))
      (i18n:tr (shape-name shape))))

;;; The picture of a shape

(defconstant +preview-tolerance+ 0.4d0
  "How far the points of a preview may stray from the contour, in the units of the shape: a picture a few dozen pixels
across cannot show less.")

(defun distance-to-segment (point
                            from
                            to)
  "Return how far a point is from the segment between two others.

POINT: a cons of x and y.
FROM: the start of the segment.
TO: its end."
  (let* ((dx (- (car to) (car from)))
         (dy (- (cdr to) (cdr from)))
         (length-squared (+ (* dx dx) (* dy dy))))
    (if (zerop length-squared)
        (sqrt (+ (expt (- (car point) (car from)) 2) (expt (- (cdr point) (cdr from)) 2)))
        (let* ((along (max 0d0 (min 1d0 (/ (+ (* (- (car point) (car from)) dx)
                                               (* (- (cdr point) (cdr from)) dy))
                                            length-squared))))
               (x (+ (car from) (* along dx)))
               (y (+ (cdr from) (* along dy))))
          (sqrt (+ (expt (- (car point) x) 2) (expt (- (cdr point) y) 2)))))))

(defun simplified-points (points
                          tolerance)
  "Return POINTS with those left out that lie within a tolerance of the line between the points kept on either side:
the Douglas-Peucker method.

POINTS: a list of conses of x and y.
TOLERANCE: how far the result may stray."
  (let* ((vector (coerce points 'vector))
         (count (length vector)))
    (if (< count 3)
        points
        (let ((keep (make-array count :element-type 'bit :initial-element 0)))
          (setf (aref keep 0) 1
                (aref keep (1- count)) 1)
          (labels ((visit (first last)
                     (let ((worst 0d0)
                           (index nil))
                       (loop for candidate from (1+ first) below last
                             do (let ((distance (distance-to-segment (aref vector candidate)
                                                                     (aref vector first)
                                                                     (aref vector last))))
                                  (when (> distance worst)
                                    (setf worst distance
                                          index candidate))))
                       (when (and index (> worst tolerance))
                         (setf (aref keep index) 1)
                         (visit first index)
                         (visit index last)))))
            (visit 0 (1- count)))
          (loop for index from 0 below count
                when (= 1 (aref keep index))
                  collect (aref vector index))))))

(defun shape-preview-polylines (shape)
  "Return the picture of a shape as a list of polylines, each a cons of a vector of single-floats (x and y, point after
point) and true when the polyline is closed.

SHAPE: a shape."
  (or (shape-preview shape)
      (setf (shape-preview shape)
            (loop for contour in (shape-contours shape)
                  for points = (simplified-points (contour-points contour) +preview-tolerance+)
                  when (rest points)
                    collect (cons (coerce (loop for point in points
                                                append (list (float (car point) 1f0) (float (cdr point) 1f0)))
                                          '(simple-array single-float (*)))
                                  (contour-closed contour))))))
