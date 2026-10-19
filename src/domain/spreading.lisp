(in-package #:borba-cad/domain)

;;;; Taking a shape apart: exploding it, which leaves the pieces where they were, and spreading it, which also moves
;;;; each a little way from the others, as in the exploded drawing of an assembly. A rectangle, a polygon and a
;;;; polyline come apart into their sides, a block into the entities it stands for, and a circle, when spread, into
;;;; arcs. Every function here works out a change set and changes nothing; a shape that cannot be spread signals
;;;; EDIT-REFUSED with the reason, in a sentence.

(defconstant +circle-spread-parts+ 4
  "How many arcs a circle comes apart into when it is spread.")

(defparameter *spreadable-kinds* '(:rectangle :polygon :polyline :circle :insert)
  "The kinds of entity that can be spread.")

(defun unit-direction (dx
                       dy)
  "Return the unit vector that points the way of a vector, as two values, or zeros when it has no length.

DX: the x of the vector.
DY: the y of the vector."
  (let ((length (sqrt (+ (* dx dx) (* dy dy)))))
    (if (base:positive-length-p length)
        (values (/ dx length) (/ dy length))
        (values 0d0 0d0))))

(defun contour-spread-parts (entity
                             coordinates
                             closed)
  "Return the sides of a contour, each as a list of a line and the direction that it moves in: away from the
inside of a closed contour, and away from the center of an open one. A side with no length is left out.

ENTITY: the entity the contour belongs to, whose id the lines are given until they are added.
COORDINATES: flat array of the vertices.
CLOSED: true when the last vertex joins the first."
  (let* ((count (floor (length coordinates) 2))
         (sides (if closed count (1- count)))
         (counter-clockwise (or (not closed)
                                (>= (geo:contour-signed-area coordinates) 0d0)))
         (center-x (loop for index from 0 below count
                         sum (aref coordinates (* 2 index)) into total
                         finally (return (/ total count))))
         (center-y (loop for index from 0 below count
                         sum (aref coordinates (1+ (* 2 index))) into total
                         finally (return (/ total count))))
         (parts '()))
    (dotimes (index sides)
      (let* ((next (mod (1+ index) count))
             (x1 (aref coordinates (* 2 index)))
             (y1 (aref coordinates (1+ (* 2 index))))
             (x2 (aref coordinates (* 2 next)))
             (y2 (aref coordinates (1+ (* 2 next)))))
        (when (base:positive-length-p (sqrt (+ (expt (- x2 x1) 2) (expt (- y2 y1) 2))))
          (multiple-value-bind (direction-x direction-y)
              (cond ((not closed)
                     (unit-direction (- (/ (+ x1 x2) 2) center-x)
                                     (- (/ (+ y1 y2) 2) center-y)))
                    (counter-clockwise
                     (unit-direction (- y2 y1)
                                     (- x1 x2)))
                    (t
                     (unit-direction (- y1 y2)
                                     (- x2 x1))))
            (push (list (make-line :id (entity-id entity)
                                   :layer-id (entity-layer-id entity)
                                   :x1 x1
                                   :y1 y1
                                   :x2 x2
                                   :y2 y2)
                        direction-x
                        direction-y)
                  parts)))))
    (nreverse parts)))

(defun circle-spread-parts (circle)
  "Return the arcs that a circle comes apart into, each as a list of an arc and the direction that it moves in:
straight out from the center.

CIRCLE: a circle."
  (loop for index from 0 below +circle-spread-parts+
        collect (let* ((start (* geo:+two-pi+ (/ index (float +circle-spread-parts+ 1d0))))
                       (end (* geo:+two-pi+ (/ (1+ index) (float +circle-spread-parts+ 1d0))))
                       (middle (/ (+ start end) 2)))
                  (list (make-arc :id (entity-id circle)
                                  :layer-id (entity-layer-id circle)
                                  :center-x (circle-center-x circle)
                                  :center-y (circle-center-y circle)
                                  :radius (circle-radius circle)
                                  :start-angle start
                                  :end-angle end)
                        (cos middle)
                        (sin middle)))))

(defun insert-spread-parts (insert)
  "Return the entities that a placement of a block stands for, each as a list of the entity and the direction that
it moves in: away from the center of the placement.

INSERT: a placement of a block."
  (let* ((bounds (entity-bounds insert))
         (center-x (geo:bounds-center-x bounds))
         (center-y (geo:bounds-center-y bounds)))
    (loop for member in (insert-entities insert)
          collect (let ((member-bounds (entity-bounds member)))
                    (multiple-value-bind (direction-x direction-y)
                        (unit-direction (- (geo:bounds-center-x member-bounds) center-x)
                                        (- (geo:bounds-center-y member-bounds) center-y))
                      (list member direction-x direction-y))))))

(defun entity-spread-parts (entity)
  "Return the pieces that ENTITY comes apart into, each as a list of the entity that is the piece and the unit
vector of the direction it moves in, or NIL when this kind of entity cannot be spread.

ENTITY: an entity."
  (typecase entity
    (rectangle-entity (contour-spread-parts entity
                                            (entity-region-boundary entity)
                                            t))
    (polygon-entity (contour-spread-parts entity
                                          (polygon-vertices entity)
                                          t))
    (polyline-entity (contour-spread-parts entity
                                           (polyline-coordinates entity)
                                           (polyline-closed entity)))
    (circle-entity (circle-spread-parts entity))
    (insert-entity (insert-spread-parts entity))
    (t nil)))

(defun entity-explode-pieces (entity)
  "Return the entities that ENTITY comes apart into when it is exploded, where they were, or NIL when this kind of
entity cannot be exploded: a block, a rectangle, a polygon and a polyline can.

ENTITY: an entity."
  (typecase entity
    (insert-entity (insert-entities entity))
    ((or rectangle-entity polygon-entity polyline-entity)
     (mapcar #'first (entity-spread-parts entity)))
    (t nil)))

(defun spread-target (drawing
                      x
                      y
                      radius)
  "Return the entity that a click spreads, or NIL: the smallest closed shape, or block, that holds the point, and
when none does, the entity that can be spread nearest to it within RADIUS.

DRAWING: the drawing.
X: world x of the click.
Y: world y of the click.
RADIUS: how far from the point, in millimetres, an entity may be and still be picked."
  (let ((best nil)
        (best-area nil))
    (map-entities (lambda (entity)
                    (let* ((layer (find-layer drawing (entity-layer-id entity)))
                           (boundary (entity-region-boundary entity))
                           (bounds (entity-bounds entity)))
                      (when (and layer
                                 (layer-visible layer)
                                 (member (entity-kind entity) *spreadable-kinds*)
                                 (geo:bounds-contains-point-p bounds
                                                              x
                                                              y)
                                 (or (and boundary
                                          (geo:point-in-polygon-p x
                                                                  y
                                                                  boundary))
                                     (and (null boundary)
                                          (eq :insert (entity-kind entity)))))
                        (let ((area (* (geo:bounds-width bounds) (geo:bounds-height bounds))))
                          (when (or (null best-area) (< area best-area))
                            (setf best entity
                                  best-area area))))))
                  (drawing-store drawing))
    (or best
        (pick-entity drawing
                     x
                     y
                     radius
                     *spreadable-kinds*))))

(defun spread-pieces-changes (entity
                              parts
                              distance
                              next-id)
  "Return the change set that replaces ENTITY by its PARTS, each moved DISTANCE along its direction, and the first id
that the change set did not use.

ENTITY: the entity that comes apart.
PARTS: what ENTITY-SPREAD-PARTS returned for it.
DISTANCE: millimetres.
NEXT-ID: the id that the first piece is given."
  (let ((changes (list (make-change (entity-id entity)
                                    entity
                                    nil))))
    (dolist (part parts)
      (destructuring-bind (piece direction-x direction-y) part
        (push (make-change next-id
                           nil
                           (entity-with-identity (entity-translated piece
                                                                    (* distance direction-x)
                                                                    (* distance direction-y))
                                                 next-id
                                                 (entity-layer-id entity)))
              changes)
        (incf next-id)))
    (values (nreverse changes)
            next-id)))

(defun spread-changes (drawing
                       id
                       distance)
  "Return the change set that takes the entity with ID apart and moves each piece DISTANCE away from the others,
the pieces on its layer. Signals EDIT-REFUSED when the entity is missing, on a locked layer or of a kind that
cannot be spread, when it is a single piece, or when the distance is not above zero, and INVALID-ENTITY when a
piece would not be valid.

DRAWING: the drawing.
ID: id of the entity to spread.
DISTANCE: millimetres."
  (let ((entity (store-find (drawing-store drawing) id)))
    (unless entity
      (refuse-edit "That entity is no longer there."))
    (require-editable drawing entity)
    (unless (base:positive-length-p distance)
      (refuse-edit "The distance must be above zero."))
    (let ((parts (entity-spread-parts entity)))
      (when (null parts)
        (refuse-edit "That cannot be spread: use a rectangle, polygon, polyline, circle or block."))
      (when (null (rest parts))
        (refuse-edit "That shape is a single piece, so there is nothing to spread."))
      (values (spread-pieces-changes entity
                                     parts
                                     distance
                                     (drawing-next-entity-id drawing))))))

(defun spread-selection-changes (drawing
                                 ids
                                 distance)
  "Return the change set that takes apart every entity among IDS that can be spread and moves its pieces DISTANCE from
each other, how many entities were spread, and how many of IDS were skipped (missing, on a locked layer, or of a
kind with no pieces). Signals EDIT-REFUSED when the distance is not above zero.

DRAWING: the drawing.
IDS: list of entity ids.
DISTANCE: millimetres."
  (unless (base:positive-length-p distance)
    (refuse-edit "The distance must be above zero."))
  (multiple-value-bind (entities missing) (existing-editable-entities drawing ids)
    (let ((next-id (drawing-next-entity-id drawing))
          (changes '())
          (spread 0)
          (skipped missing))
      (dolist (entity entities)
        (let ((parts (entity-spread-parts entity)))
          (if (rest parts)
              (multiple-value-bind (piece-changes following-id) (spread-pieces-changes entity
                                                                                       parts
                                                                                       distance
                                                                                       next-id)
                (setf changes (append changes piece-changes)
                      next-id following-id)
                (incf spread))
              (incf skipped))))
      (values changes
              spread
              skipped))))
