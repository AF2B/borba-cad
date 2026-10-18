(in-package #:borba-cad/domain)

;;;; Moving entities from one drawing to another, which is what copy and paste do. A copy is a
;;;; small drawing that holds the entities and the layers they use, so that it can be written
;;;; and read as any drawing is; a paste turns the entities of such a drawing into new entities
;;;; of the drawing it goes into.

(defun drawing-entities (drawing)
  "Return every entity of DRAWING in stacking order.

DRAWING: the drawing."
  (let ((found '()))
    (map-entities (lambda (entity) (push entity found))
                  (drawing-store drawing))
    (nreverse found)))

(defun extract-entities (drawing
                         entities)
  "Return a new drawing that holds ENTITIES, with the ids and layer ids they have, and the layers
they are on, with the settings of DRAWING that give its numbers meaning.

DRAWING: the drawing the entities come from.
ENTITIES: a non-empty list of entities of DRAWING."
  (let ((snippet (make-drawing)))
    (dolist (layer-id (drawing-layer-order drawing))
      (when (find layer-id entities :key #'entity-layer-id)
        (let ((layer (find-layer drawing layer-id)))
          (restore-layer snippet
                         layer-id
                         (layer-name layer)
                         (layer-color layer)
                         (layer-visible layer)
                         (layer-locked layer)))))
    (dolist (entity entities)
      (when (eq :insert (entity-kind entity))
        (define-block snippet (insert-block entity))))
    (apply-changes snippet (create-entities-changes entities))
    (restore-drawing-state snippet
                           :current-layer-id (entity-layer-id (first entities))
                           :next-entity-id (1+ (reduce #'max entities :key #'entity-id))
                           :next-layer-id (drawing-next-layer-id drawing)
                           :display-unit (drawing-display-unit drawing)
                           :grid-spacing (drawing-grid-spacing drawing)
                           :grid-visible (drawing-grid-visible drawing)
                           :snap-to-grid (drawing-snap-to-grid drawing)
                           :object-snap (drawing-object-snap drawing)
                           :default-text-height (drawing-default-text-height drawing))
    snippet))

(defun entities-bounds (entities)
  "Return the bounds that contain every one of ENTITIES.

ENTITIES: a non-empty list of entities."
  (reduce #'geo:bounds-union (mapcar #'entity-bounds entities)))

(defun paste-layer-id (drawing
                       snippet
                       entity)
  "Return the id of the layer of DRAWING that a pasted ENTITY goes onto: the layer that has the
name of the layer it was copied from, when there is one that is visible and unlocked, and the
current layer otherwise.

DRAWING: the drawing the entity is pasted into.
SNIPPET: the copied drawing the entity comes from.
ENTITY: the entity, with its layer id in SNIPPET."
  (let* ((origin (find-layer snippet (entity-layer-id entity)))
         (same-name (and origin (find-layer-by-name drawing (layer-name origin)))))
    (if (and same-name (layer-visible same-name) (not (layer-locked same-name)))
        (layer-id same-name)
        (drawing-current-layer-id drawing))))

(defun placed-in-drawing (drawing
                          entity)
  "Return ENTITY, with a placement of a block made to place the block of that name that DRAWING has.

DRAWING: the drawing the entity goes into.
ENTITY: an entity of a copied drawing."
  (if (eq :insert (entity-kind entity))
      (update-insert entity
                     :block (find-block drawing (block-definition-name (insert-block entity))))
      entity))

(defun pasted-entities (drawing
                        snippet
                        dx
                        dy)
  "Return the entities of SNIPPET as new entities of DRAWING: with fresh ids, on the layers that
PASTE-LAYER-ID picks, and moved by (DX, DY). The blocks of SNIPPET that DRAWING does not have are added to it, and a
placement of a block that it has uses its block, whatever the copy said.

DRAWING: the drawing they are pasted into.
SNIPPET: the copied drawing.
DX: displacement along x.
DY: displacement along y."
  (dolist (definition (block-definitions snippet))
    (unless (find-block drawing (block-definition-name definition))
      (define-block drawing definition)))
  (loop for entity in (drawing-entities snippet)
        for id from (drawing-next-entity-id drawing)
        collect (entity-translated (entity-with-identity (placed-in-drawing drawing entity)
                                                         id
                                                         (paste-layer-id drawing
                                                                         snippet
                                                                         entity))
                                   dx
                                   dy)))
