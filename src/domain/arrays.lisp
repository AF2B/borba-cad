(in-package #:borba-cad/domain)

;;;; Arrays: copies of what is selected laid out in a grid, round a point or along a path. The original is the first
;;;; item, so an array of N items adds N - 1 copies of each entity. Like the other commands these only work out change
;;;; sets; what cannot be done signals EDIT-REFUSED with the reason, in a sentence.

(defconstant +maximum-array-entities+ 20000
  "Most entities one array may add.")

(defconstant +path-vertices-per-turn+ 720
  "How many vertices stand for a full turn of a circle, an ellipse or an arc when it is a path that things follow.")

(defconstant +full-turn-tolerance+ 1d-9
  "How far, in radians, a polar array may fall short of a full turn and still be one.")

(defun entity-path (entity)
  "Return the path that an entity makes, as two values: a flat array of its vertices, and true when the path is
closed. A curve is cut into many short segments. Returns NIL for an entity that is not a line, a polyline, an arc, a
circle, an ellipse, a rectangle or a polygon.

ENTITY: an entity."
  (typecase entity
    (line-entity
     (values (make-array 4 :element-type 'double-float
                           :initial-contents (list (line-x1 entity)
                                                   (line-y1 entity)
                                                   (line-x2 entity)
                                                   (line-y2 entity)))
             nil))
    (polyline-entity
     (values (polyline-coordinates entity) (polyline-closed entity)))
    (arc-entity
     (let* ((segments (max 1 (ceiling (* +path-vertices-per-turn+ (arc-sweep entity))
                                      (* 2 (float pi 1d0)))))
            (points (make-array (* 2 (1+ segments)) :element-type 'double-float)))
       (dotimes (index (1+ segments))
         (let ((angle (+ (arc-start-angle entity) (* (arc-sweep entity) (/ index (float segments 1d0))))))
           (setf (aref points (* 2 index)) (+ (arc-center-x entity) (* (arc-radius entity) (cos angle)))
                 (aref points (1+ (* 2 index))) (+ (arc-center-y entity) (* (arc-radius entity) (sin angle))))))
       (values points nil)))
    (circle-entity
     (values (geo:ellipse-points (circle-center-x entity)
                                 (circle-center-y entity)
                                 (circle-radius entity)
                                 (circle-radius entity)
                                 0d0
                                 +path-vertices-per-turn+)
             t))
    (ellipse-entity
     (values (geo:ellipse-points (ellipse-center-x entity)
                                 (ellipse-center-y entity)
                                 (ellipse-radius-x entity)
                                 (ellipse-radius-y entity)
                                 (ellipse-rotation entity)
                                 +path-vertices-per-turn+)
             t))
    ((or rectangle-entity polygon-entity)
     (values (entity-region-boundary entity) t))
    (t nil)))

(defun array-changes (drawing
                      ids
                      transforms)
  "Return the change set that adds, for each of TRANSFORMS, a copy of every entity with IDS made by it, as new entities
on the layer of the original, and how many were skipped because they are missing or on a locked layer. Signals
EDIT-REFUSED when nothing can be copied or the array would add too many entities, and INVALID-ENTITY when a copy would
leave the drawing limits.

DRAWING: the drawing.
IDS: list of entity ids.
TRANSFORMS: list of functions of an entity that return the copy, one for each item but the first."
  (multiple-value-bind (entities skipped) (existing-editable-entities drawing ids)
    (when (null entities)
      (refuse-edit "Select what to repeat first, on a layer that is not locked."))
    (when (> (* (length entities) (length transforms)) +maximum-array-entities+)
      (refuse-edit "An array may add at most ~d entities." +maximum-array-entities+))
    (let ((next-id (drawing-next-entity-id drawing))
          (changes '()))
      (dolist (transform transforms)
        (dolist (entity entities)
          (push (make-change next-id
                             nil
                             (entity-with-identity (funcall transform entity)
                                                   next-id
                                                   (entity-layer-id entity)))
                changes)
          (incf next-id)))
      (values (nreverse changes) skipped))))

(defun rectangular-array-transforms (rows
                                     columns
                                     row-spacing
                                     column-spacing
                                     angle)
  "Return the transforms that put copies in a grid of ROWS by COLUMNS, every item but the first, row by row.

ROWS: how many rows, at least 1.
COLUMNS: how many columns, at least 1.
ROW-SPACING: distance from one row to the next, in the direction 90 degrees from the columns; negative goes the other
way.
COLUMN-SPACING: distance from one column to the next, in the direction ANGLE; negative goes the other way.
ANGLE: direction of the columns, in radians."
  (loop for row below rows
        append (loop for column below columns
                     unless (and (zerop row) (zerop column))
                       collect (let ((dx (- (* column column-spacing (cos angle)) (* row row-spacing (sin angle))))
                                     (dy (+ (* column column-spacing (sin angle)) (* row row-spacing (cos angle)))))
                                 (lambda (entity)
                                   (entity-translated entity
                                                      dx
                                                      dy))))))

(defun rectangular-array-changes (drawing
                                  ids
                                  rows
                                  columns
                                  row-spacing
                                  column-spacing
                                  &key (angle 0d0))
  "Return the change set that lays copies of the entities with IDS in a grid, the originals being its first item, and
how many were skipped. Signals EDIT-REFUSED when the grid has one item or the spacing of a direction that has more is
zero.

DRAWING: the drawing.
IDS: list of entity ids.
ROWS: how many rows, at least 1.
COLUMNS: how many columns, at least 1.
ROW-SPACING: distance from one row to the next, in millimetres.
COLUMN-SPACING: distance from one column to the next, in millimetres.
ANGLE: direction of the columns, in radians; the rows are 90 degrees from it."
  (when (< (* rows columns) 2)
    (refuse-edit "An array needs at least two items."))
  (when (and (> columns 1) (zerop column-spacing))
    (refuse-edit "The spacing of the columns must not be zero."))
  (when (and (> rows 1) (zerop row-spacing))
    (refuse-edit "The spacing of the rows must not be zero."))
  (array-changes drawing
                 ids
                 (rectangular-array-transforms rows
                                               columns
                                               row-spacing
                                               column-spacing
                                               angle)))

(defun entities-center (entities)
  "Return the center of the box around some entities, as two values.

ENTITIES: a non-empty list of entities."
  (let ((bounds (reduce #'geo:bounds-union (mapcar #'entity-bounds entities))))
    (values (geo:bounds-center-x bounds)
            (geo:bounds-center-y bounds))))

(defun polar-array-changes (drawing
                            ids
                            center-x
                            center-y
                            count
                            total-angle
                            &key (rotate-items t))
  "Return the change set that lays copies of the entities with IDS round a point, the originals being the first of COUNT
items, and how many were skipped. A total angle of a full turn or more puts the items at equal steps round the whole
circle; a smaller one puts the last item at the end of the angle. Signals EDIT-REFUSED when there are fewer than two
items or the angle is zero.

DRAWING: the drawing.
IDS: list of entity ids.
CENTER-X: x of the center.
CENTER-Y: y of the center.
COUNT: how many items in all, the originals included, at least 2.
TOTAL-ANGLE: the angle the items are spread over, in radians, counter-clockwise when positive.
ROTATE-ITEMS: true to turn each copy as it goes round; false to keep it as it was drawn, only moved."
  (when (< count 2)
    (refuse-edit "An array needs at least two items."))
  (when (< (abs total-angle) +full-turn-tolerance+)
    (refuse-edit "The angle of a polar array must not be zero."))
  (let* ((full-turn (>= (abs total-angle) (- (* 2 (float pi 1d0)) +full-turn-tolerance+)))
         (step (/ total-angle (if full-turn count (1- count))))
         (entities (existing-editable-entities drawing ids)))
    (multiple-value-bind (reference-x reference-y) (if entities
                                                       (entities-center entities)
                                                       (values center-x center-y))
      (array-changes drawing
                     ids
                     (loop for index from 1 below count
                           collect (let ((angle (* index step)))
                                     (if rotate-items
                                         (lambda (entity)
                                           (entity-rotated entity
                                                           center-x
                                                           center-y
                                                           angle))
                                         (multiple-value-bind (moved-x moved-y) (rotate-xy reference-x
                                                                                           reference-y
                                                                                           center-x
                                                                                           center-y
                                                                                           angle)
                                           (let ((dx (- moved-x reference-x))
                                                 (dy (- moved-y reference-y)))
                                             (lambda (entity)
                                               (entity-translated entity
                                                                  dx
                                                                  dy)))))))))))

(defun path-array-transforms (points
                              reference-x
                              reference-y
                              align)
  "Return the transforms that move copies to the points of a path, every item but the first, the first point being where
the originals are.

POINTS: list of (x y angle) lists, the first for the originals.
REFERENCE-X: x of the center of the originals.
REFERENCE-Y: y of the center of the originals.
ALIGN: true to turn each copy by how far the path has turned since its start."
  (destructuring-bind (start-x start-y start-angle) (first points)
    (loop for (x y angle) in (rest points)
          collect (let ((dx (- x start-x))
                        (dy (- y start-y))
                        (turn (- angle start-angle)))
                    (lambda (entity)
                      (let ((moved (entity-translated entity
                                                      dx
                                                      dy)))
                        (if align
                            (entity-rotated moved
                                            (+ reference-x dx)
                                            (+ reference-y dy)
                                            turn)
                            moved)))))))

(defun path-array-changes (drawing
                           ids
                           path-id
                           &key count
                                spacing
                                align)
  "Return the change set that lays copies of the entities with IDS along the path that another entity makes, the
originals being the item at the start of the path, and how many were skipped. With COUNT the items are at equal steps
from the start to the end of the path (round a closed one); with SPACING there is one every that distance, as many as
fit. Signals EDIT-REFUSED when the entity is missing or not a path, or when there are fewer than two items.

DRAWING: the drawing.
IDS: list of entity ids; the path itself is left out of them.
PATH-ID: id of the line, polyline, arc, circle, ellipse, rectangle or polygon the items follow.
COUNT: how many items in all, the originals included, or NIL to use SPACING.
SPACING: distance between one item and the next along the path, in millimetres, or NIL to use COUNT.
ALIGN: true to turn each copy to follow the direction of the path."
  (let ((path (store-find (drawing-store drawing) path-id)))
    (unless path
      (refuse-edit "The path is not in the drawing."))
    (multiple-value-bind (coordinates closed) (entity-path path)
      (unless coordinates
        (refuse-edit "A path is a line, polyline, arc, circle, ellipse, rectangle or polygon."))
      (let ((length (geo:path-length coordinates closed))
            (entities (existing-editable-entities drawing (remove path-id ids))))
        (unless (base:positive-length-p length)
          (refuse-edit "The path has no length."))
        (when (and (null count) (not (and spacing (base:positive-length-p spacing))))
          (refuse-edit "Give a number of items or a spacing above zero."))
        (let ((points (geo:path-points-at coordinates
                                          closed
                                          (if count
                                              (geo:path-even-distances length
                                                                       closed
                                                                       count)
                                              (geo:path-spaced-distances length
                                                                         closed
                                                                         spacing)))))
          (when (< (length points) 2)
            (refuse-edit "An array needs at least two items."))
          (multiple-value-bind (reference-x reference-y) (if entities
                                                             (entities-center entities)
                                                             (values 0d0 0d0))
            (array-changes drawing
                           (remove path-id ids)
                           (path-array-transforms points
                                                  reference-x
                                                  reference-y
                                                  align))))))))
