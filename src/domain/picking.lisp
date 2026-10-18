(in-package #:borba-cad/domain)

;;;; Picking, window selection and snapping. All distances and radii here are world lengths in
;;;; millimetres; the application converts its pixel-based radii before calling.

(defun pickable-p (drawing
                   entity)
  "Return true when ENTITY may be selected: its layer exists, is visible and is not locked.

DRAWING: the drawing.
ENTITY: an entity."
  (let ((layer (find-layer drawing (entity-layer-id entity))))
    (and layer
         (layer-visible layer)
         (not (layer-locked layer)))))

(defun pick-entity (drawing
                    x
                    y
                    radius)
  "Return the pickable entity nearest to the point within RADIUS, or NIL. When several are at
the same distance the one created last (topmost) wins.

DRAWING: the drawing.
X: point x in millimetres.
Y: point y in millimetres.
RADIUS: pick radius in millimetres."
  (declare (type double-float x y radius))
  (let ((store (drawing-store drawing))
        (best nil)
        (best-distance radius))
    (declare (type double-float best-distance))
    (loop for index from (1- (store-count store)) downto 0
          do (let ((entity (store-entity-at store index)))
               (when (geo:bounds-near-point-p (entity-bounds entity)
                                              x
                                              y
                                              radius)
                 (let ((distance (entity-pick-distance entity
                                                       x
                                                       y)))
                   (declare (type double-float distance))
                   (when (and (if best
                                  (< distance best-distance)
                                  (<= distance radius))
                              (pickable-p drawing entity))
                     (setf best entity
                           best-distance distance))))))
    best))

(defun entities-in-window (drawing
                           window)
  "Return the pickable entities that lie entirely inside WINDOW, in stacking order.

DRAWING: the drawing.
WINDOW: a bounds value in world coordinates."
  (let ((found '()))
    (map-entities (lambda (entity)
                    (when (and (geo:bounds-contains-bounds-p window (entity-bounds entity))
                               (pickable-p drawing entity))
                      (push entity found)))
                  (drawing-store drawing))
    (nreverse found)))

(defun entities-crossing-window (drawing
                                 window)
  "Return the pickable entities that lie inside WINDOW or touch it, in stacking order: what a
crossing selection takes.

DRAWING: the drawing.
WINDOW: a bounds value in world coordinates."
  (let ((found '()))
    (map-entities (lambda (entity)
                    (when (and (geo:bounds-intersect-p (entity-bounds entity) window)
                               (pickable-p drawing entity)
                               (entity-touches-bounds-p entity window))
                      (push entity found)))
                  (drawing-store drawing))
    (nreverse found)))

(defun pickable-entities (drawing)
  "Return every pickable entity, in stacking order.

DRAWING: the drawing."
  (let ((found '()))
    (map-entities (lambda (entity)
                    (when (pickable-p drawing entity)
                      (push entity found)))
                  (drawing-store drawing))
    (nreverse found)))

(defun snap-to-grid (drawing
                     x
                     y)
  "Return the grid point nearest to (X, Y) as two values.

DRAWING: the drawing, which supplies the grid spacing.
X: point x in millimetres.
Y: point y in millimetres."
  (let ((spacing (drawing-grid-spacing drawing)))
    (values (geo:snap-coordinate x spacing)
            (geo:snap-coordinate y spacing))))

(defun find-object-snap (drawing
                         x
                         y
                         radius
                         &optional anchor)
  "Return the object-snap point nearest to (X, Y) within RADIUS as three values (x, y, kind), or NIL. The
candidates, on the pickable entities near the pointer, are their endpoints and centers, the midpoints of their
segments and arcs, the quadrants of circles and arcs, the points where they cross, and, when there is an
anchor, the feet of perpendiculars to them from it and the points where a line from it touches a circle or an
arc. At equal distances the endpoint wins, then the intersection, the center, the midpoint, the quadrant, the
perpendicular and the tangent.

DRAWING: the drawing.
X: point x in millimetres.
Y: point y in millimetres.
RADIUS: snap radius in millimetres.
ANCHOR: an (x . y) cons, the last point drawn, or NIL."
  (declare (type double-float x y radius))
  (let ((nearby '())
        (best nil)
        (best-distance nil))
    (map-entities (lambda (entity)
                    (when (and (geo:bounds-near-point-p (entity-bounds entity)
                                                        x
                                                        y
                                                        radius)
                               (pickable-p drawing entity))
                      (push entity nearby)))
                  (drawing-store drawing))
    (dolist (candidate (nearby-snap-candidates (nreverse nearby) anchor))
      (destructuring-bind (candidate-x candidate-y kind) candidate
        (let ((distance (geo:distance x
                                      y
                                      candidate-x
                                      candidate-y)))
          (when (and (<= distance radius)
                     (better-snap-p distance
                                    kind
                                    best-distance
                                    (third best)))
            (setf best candidate
                  best-distance distance)))))
    (and best
         (values (first best) (second best) (third best)))))

(defun snap-point (drawing
                   x
                   y
                   object-radius
                   &optional anchor)
  "Return the snapped position for a pointer at (X, Y) as three values (x, y, kind). The kind
is one of the kinds of object snap (see FIND-OBJECT-SNAP), :grid for grid snap, NIL when nothing
snapped. Object snap wins over grid snap; either applies only when enabled in the drawing.

DRAWING: the drawing.
X: pointer x in millimetres.
Y: pointer y in millimetres.
OBJECT-RADIUS: object-snap radius in millimetres.
ANCHOR: an (x . y) cons, the last point drawn, or NIL."
  (multiple-value-bind (object-x object-y kind)
      (if (drawing-object-snap drawing)
          (find-object-snap drawing
                            x
                            y
                            object-radius
                            anchor)
          nil)
    (cond (kind (values object-x object-y kind))
          ((drawing-snap-to-grid drawing)
           (multiple-value-bind (grid-x grid-y)
               (snap-to-grid drawing
                             x
                             y)
             (values grid-x grid-y :grid)))
          (t (values x y nil)))))
