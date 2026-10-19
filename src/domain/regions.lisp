(in-package #:borba-cad/domain)

;;;; Joining, cutting and overlapping closed shapes: a rectangle, a circle, an ellipse, a polygon or a closed polyline
;;;; is a region, and what two or more of them make together is one or more closed polylines, with a polyline for each
;;;; hole. The shapes that were used are replaced by the result. Like the other commands these work out change sets and
;;;; change nothing; what cannot be done signals EDIT-REFUSED with the reason, in a sentence.

(defun region-entities (drawing
                        ids)
  "Return the entities among IDS that can be joined, cut or overlapped, which is to say that exist, are on a layer
that is not locked, and are closed shapes, and how many of IDS were skipped as a second value.

DRAWING: the drawing.
IDS: list of entity ids."
  (multiple-value-bind (entities skipped) (existing-editable-entities drawing ids)
    (let ((shapes (remove-if-not #'entity-region-boundary entities)))
      (values shapes
              (+ skipped (- (length entities) (length shapes)))))))

(defun region-of (entity)
  "Return the region that a closed shape makes.

ENTITY: a rectangle, circle, ellipse, polygon or closed polyline."
  (geo:polygon-region (entity-region-boundary entity)))

(defun loops-polylines (loops
                        layer-id
                        first-id)
  "Return the closed polylines that are the loops of a region, on a layer, with ids from FIRST-ID on.

LOOPS: a list of flat arrays of vertices.
LAYER-ID: the layer they go on.
FIRST-ID: the id of the first."
  (loop for loop-coordinates in loops
        for id from first-id
        collect (make-polyline :id id
                               :layer-id layer-id
                               :coordinates loop-coordinates
                               :closed t)))

(defun region-result-changes (drawing
                              used
                              loops
                              layer-id)
  "Return the change set that replaces the shapes that were used by the polylines of the loops of the result.

DRAWING: the drawing.
USED: the entities that were used.
LOOPS: the loops of the result.
LAYER-ID: the layer the polylines go on."
  (append (mapcar (lambda (entity)
                    (make-change (entity-id entity)
                                 entity
                                 nil))
                  used)
          (create-entities-changes (loops-polylines loops
                                                    layer-id
                                                    (drawing-next-entity-id drawing)))))

(defun region-changes (drawing
                       ids
                       operation
                       &key base-id)
  "Return the change set that joins, overlaps or cuts the closed shapes with IDS, replacing them by the polylines of
what results, on the layer of the first, and how many shapes were used and how many of IDS were skipped. Signals
EDIT-REFUSED when fewer than two shapes can be used, or when nothing is left of the result.

DRAWING: the drawing.
IDS: list of entity ids.
OPERATION: :union, :intersection or :difference.
BASE-ID: for :difference, the id of the shape the others are cut from, or NIL for the first."
  (multiple-value-bind (shapes skipped) (region-entities drawing ids)
    (when (< (length shapes) 2)
      (refuse-edit "Choose at least two closed shapes: a rectangle, circle, ellipse, polygon or closed polyline."))
    (let* ((base (or (and base-id (find base-id shapes :key #'entity-id))
                     (first shapes)))
           (others (remove base shapes))
           (result (region-of base)))
      (dolist (other others)
        (setf result (geo:region-boolean operation
                                         result
                                         (region-of other))))
      (when (null result)
        (if (eq operation :difference)
            (refuse-edit "Nothing is left of the first shape.")
            (refuse-edit "The shapes do not overlap.")))
      (values (region-result-changes drawing
                                     shapes
                                     result
                                     (entity-layer-id base))
              (length shapes)
              skipped))))

(defun region-target (drawing
                      x
                      y
                      radius)
  "Return the closed shape that a click picks: the smallest that holds the point, and when none does, the nearest within
RADIUS of it, or NIL.

DRAWING: the drawing.
X: world x of the click.
Y: world y of the click.
RADIUS: how far from the point, in millimetres, a shape may be and still be picked."
  (let ((best nil)
        (best-area nil))
    (map-entities (lambda (entity)
                    (let ((layer (find-layer drawing (entity-layer-id entity)))
                          (boundary (entity-region-boundary entity)))
                      (when (and boundary
                                 layer
                                 (layer-visible layer)
                                 (geo:bounds-contains-point-p (entity-bounds entity)
                                                              x
                                                              y)
                                 (geo:point-in-polygon-p x
                                                         y
                                                         boundary))
                        (let ((area (geo:contour-area boundary)))
                          (when (or (null best-area) (< area best-area))
                            (setf best entity
                                  best-area area))))))
                  (drawing-store drawing))
    (or best
        (pick-entity drawing
                     x
                     y
                     radius
                     '(:rectangle :polygon :polyline :circle :ellipse)))))
