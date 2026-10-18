(in-package #:borba-cad/domain)

;;;; The points an object snap can give besides the ends and centers that each entity lists: the middle of a
;;;; segment or an arc, the quadrants of a circle, where two entities cross, and, from the last point drawn,
;;;; the foot of a perpendicular and the points of tangency. Each entity is first reduced to its primitives,
;;;; the segments, circles and arcs it is made of, and the points are found from those.

(defconstant +maximum-intersection-entities+ 40
  "Most entities near the pointer whose crossings are looked for, which keeps the work to a few hundred pairs
however crowded the drawing is.")

(defparameter *object-snap-priority* '(:endpoint :intersection :center :midpoint :quadrant :perpendicular :tangent)
  "The kinds of object snap, the one that wins when two are at the same distance first.")

(defun entity-primitives (entity)
  "Return the primitives ENTITY is made of, as a list: (:segment x1 y1 x2 y2), (:circle x y radius) and
(:arc x y radius start sweep). An ellipse, a text and a dimension have none that are looked at.

ENTITY: an entity."
  (flet ((closed-segments (coordinates closed)
           (let ((count (floor (length coordinates) 2))
                 (segments '()))
             (loop for index from 0 below (1- count)
                   do (push (list :segment
                                  (aref coordinates (* 2 index))
                                  (aref coordinates (1+ (* 2 index)))
                                  (aref coordinates (* 2 (1+ index)))
                                  (aref coordinates (1+ (* 2 (1+ index)))))
                            segments))
             (when (and closed (> count 2))
               (push (list :segment
                           (aref coordinates (* 2 (1- count)))
                           (aref coordinates (1+ (* 2 (1- count))))
                           (aref coordinates 0)
                           (aref coordinates 1))
                     segments))
             (nreverse segments))))
    (typecase entity
      (line-entity
       (list (list :segment (line-x1 entity) (line-y1 entity) (line-x2 entity) (line-y2 entity))))
      (rectangle-entity
       (let* ((x1 (rectangle-min-x entity))
              (y1 (rectangle-min-y entity))
              (x2 (+ x1 (rectangle-width entity)))
              (y2 (+ y1 (rectangle-height entity))))
         (list (list :segment x1 y1 x2 y1)
               (list :segment x2 y1 x2 y2)
               (list :segment x2 y2 x1 y2)
               (list :segment x1 y2 x1 y1))))
      (polyline-entity
       (closed-segments (polyline-coordinates entity) (polyline-closed entity)))
      (polygon-entity
       (closed-segments (polygon-vertices entity) t))
      (circle-entity
       (list (list :circle (circle-center-x entity) (circle-center-y entity) (circle-radius entity))))
      (arc-entity
       (list (list :arc
                   (arc-center-x entity)
                   (arc-center-y entity)
                   (arc-radius entity)
                   (arc-start-angle entity)
                   (arc-sweep entity))))
      (t nil))))

(defun point-on-arc-primitive-p (primitive
                                 x
                                 y)
  "Return true when a point of the circle of an arc primitive lies on the arc itself.

PRIMITIVE: an (:arc x y radius start sweep) list.
X: x of the point.
Y: y of the point."
  (destructuring-bind (center-x center-y radius start sweep) (rest primitive)
    (declare (ignore radius))
    (geo:angle-within-sweep-p (atan (- y center-y) (- x center-x))
                              start
                              sweep)))

(defun primitive-circle (primitive)
  "Return the circle a circle or an arc primitive lies on, as the list (x y radius), or NIL for a segment.

PRIMITIVE: a primitive."
  (and (member (first primitive) '(:circle :arc))
       (subseq primitive 1 4)))

(defun primitive-keeps-point-p (primitive
                                point)
  "Return true when a point that is known to lie on the circle of PRIMITIVE lies on the primitive: always for a
circle, and for an arc when it is within its sweep.

PRIMITIVE: a circle or an arc primitive.
POINT: an (x . y) cons."
  (or (eq :circle (first primitive))
      (point-on-arc-primitive-p primitive
                                (car point)
                                (cdr point))))

(defun primitive-extra-snaps (primitive)
  "Return the midpoints and quadrants of a primitive as a list of (x y kind): the middle of a segment or an arc,
and the four quadrants of a circle, or those that are on an arc.

PRIMITIVE: a primitive."
  (ecase (first primitive)
    (:segment
     (destructuring-bind (x1 y1 x2 y2) (rest primitive)
       (list (list (/ (+ x1 x2) 2) (/ (+ y1 y2) 2) :midpoint))))
    ((:circle :arc)
     (destructuring-bind (center-x center-y radius) (primitive-circle primitive)
       (let ((snaps '()))
         (dolist (angle (list 0d0 (/ pi 2) (float pi 1d0) (* 3 (/ pi 2))))
           (let ((x (+ center-x (* radius (cos angle))))
                 (y (+ center-y (* radius (sin angle)))))
             (when (primitive-keeps-point-p primitive (cons x y))
               (push (list x y :quadrant) snaps))))
         (when (eq :arc (first primitive))
           (let ((angle (+ (fourth (rest primitive)) (/ (fifth (rest primitive)) 2))))
             (push (list (+ center-x (* radius (cos angle))) (+ center-y (* radius (sin angle))) :midpoint)
                   snaps)))
         (nreverse snaps))))))

(defun primitive-intersections (first
                                second)
  "Return the points where two primitives cross, as a list of (x . y) conses.

FIRST: a primitive.
SECOND: a primitive."
  (let ((circle-a (primitive-circle first))
        (circle-b (primitive-circle second)))
    (remove-if-not
     (lambda (point)
       (and (or (null circle-a) (primitive-keeps-point-p first point))
            (or (null circle-b) (primitive-keeps-point-p second point))))
     (cond ((and (null circle-a) (null circle-b))
            (multiple-value-bind (x y) (apply #'geo:segment-segment-intersection (append (rest first) (rest second)))
              (and x (list (cons x y)))))
           ((null circle-a)
            (apply #'geo:segment-circle-intersections (append (rest first) circle-b)))
           ((null circle-b)
            (apply #'geo:segment-circle-intersections (append (rest second) circle-a)))
           (t
            (apply #'geo:circle-circle-intersections (append circle-a circle-b)))))))

(defun primitive-anchor-snaps (primitive
                               anchor)
  "Return the perpendicular and tangent points of a primitive from the last point drawn, as a list of (x y kind):
the foot of the perpendicular on a segment, the points of a circle or an arc that are in line with its center
and the anchor, and the points where a line from the anchor touches it.

PRIMITIVE: a primitive.
ANCHOR: an (x . y) cons."
  (let ((px (car anchor))
        (py (cdr anchor)))
    (ecase (first primitive)
      (:segment
       (destructuring-bind (x1 y1 x2 y2) (rest primitive)
         (multiple-value-bind (x y) (geo:point-segment-foot px
                                                            py
                                                            x1
                                                            y1
                                                            x2
                                                            y2)
           (and x (list (list x y :perpendicular))))))
      ((:circle :arc)
       (destructuring-bind (center-x center-y radius) (primitive-circle primitive)
         (append (loop for point in (geo:circle-perpendicular-feet px
                                                                   py
                                                                   center-x
                                                                   center-y
                                                                   radius)
                       when (primitive-keeps-point-p primitive point)
                         collect (list (car point) (cdr point) :perpendicular))
                 (loop for point in (geo:circle-tangent-points px
                                                               py
                                                               center-x
                                                               center-y
                                                               radius)
                       when (primitive-keeps-point-p primitive point)
                         collect (list (car point) (cdr point) :tangent))))))))

(defun nearby-snap-candidates (entities
                               anchor)
  "Return every point an object snap can give on a list of entities, as a list of (x y kind): the ends and centers
the entities list, the midpoints and quadrants, where the entities cross each other, and, when there is an
anchor, the perpendicular and tangent points.

ENTITIES: the entities near the pointer.
ANCHOR: an (x . y) cons, the last point drawn, or NIL."
  (let ((candidates '())
        (primitives '()))
    (dolist (entity entities)
      (dolist (candidate (entity-snap-points entity))
        (push candidate candidates))
      (dolist (primitive (entity-primitives entity))
        (push primitive primitives)
        (dolist (candidate (primitive-extra-snaps primitive))
          (push candidate candidates))
        (when anchor
          (dolist (candidate (primitive-anchor-snaps primitive anchor))
            (push candidate candidates)))))
    (let ((group (subseq (reverse entities) 0 (min +maximum-intersection-entities+ (length entities))))
          (crossing '()))
      (loop for (first . later) on group
            do (dolist (second later)
                 (dolist (a (entity-primitives first))
                   (dolist (b (entity-primitives second))
                     (dolist (point (primitive-intersections a b))
                       (push (list (car point) (cdr point) :intersection) crossing))))))
      (append crossing candidates))))

(defun better-snap-p (distance
                      kind
                      best-distance
                      best-kind)
  "Return true when a candidate at DISTANCE of KIND is better than the best so far: nearer, or as near but of a
kind that comes first in the priority.

DISTANCE: how far the candidate is from the pointer.
KIND: its kind.
BEST-DISTANCE: how far the best so far is, or NIL when there is none.
BEST-KIND: its kind."
  (or (null best-distance)
      (< distance (- best-distance 1d-9))
      (and (<= distance (+ best-distance 1d-9))
           (< (or (position kind *object-snap-priority*) most-positive-fixnum)
              (or (position best-kind *object-snap-priority*) most-positive-fixnum)))))
