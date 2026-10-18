(in-package #:borba-cad/domain)

;;;; Builders of change sets for the user-level operations. They only compute changes; applying
;;;; them (and recording undo) is HISTORY-COMMIT's job.

(defun editable-entity-p (drawing
                          entity)
  "Return true when ENTITY may be changed: its layer exists and is not locked.

DRAWING: the drawing.
ENTITY: an entity."
  (let ((layer (find-layer drawing (entity-layer-id entity))))
    (and layer (not (layer-locked layer)))))

(defun create-entity-changes (entity)
  "Return the change set that adds ENTITY.

ENTITY: the entity to add; its id must be unused."
  (list (make-change (entity-id entity)
                     nil
                     entity)))

(defun create-entities-changes (entities)
  "Return the change set that adds every entity in ENTITIES.

ENTITIES: list of entities with unused, distinct ids."
  (mapcar (lambda (entity) (make-change (entity-id entity)
                                        nil
                                        entity)) entities))

(defun existing-editable-entities (drawing
                                   ids)
  "Return two values: the entities among IDS that exist and are editable, and how many were
skipped because they are missing or on a locked layer.

DRAWING: the drawing.
IDS: list of entity ids."
  (let ((entities '())
        (skipped 0))
    (dolist (id ids)
      (let ((entity (store-find (drawing-store drawing) id)))
        (if (and entity (editable-entity-p drawing entity))
            (push entity entities)
            (incf skipped))))
    (values (nreverse entities) skipped)))

(defun delete-entities-changes (drawing
                                ids)
  "Return the change set that deletes the entities with IDS, and the number skipped because
they are missing or on a locked layer.

DRAWING: the drawing.
IDS: list of entity ids."
  (multiple-value-bind (entities skipped)
      (existing-editable-entities drawing ids)
    (values (mapcar (lambda (entity) (make-change (entity-id entity)
                                                  entity
                                                  nil)) entities)
            skipped)))

(defun translate-entities-changes (drawing
                                   ids
                                   dx
                                   dy)
  "Return the change set that moves the entities with IDS by (DX, DY), and the number skipped.
Signals INVALID-ENTITY when a moved entity would leave the drawing limits.

DRAWING: the drawing.
IDS: list of entity ids.
DX: displacement along x in millimetres.
DY: displacement along y in millimetres."
  (multiple-value-bind (entities skipped)
      (existing-editable-entities drawing ids)
    (values (mapcar (lambda (entity)
                      (make-change (entity-id entity)
                                   entity
                                   (entity-translated entity
                                                      dx
                                                      dy)))
                    entities)
            skipped)))

(defun edit-property-changes (drawing
                              id
                              key
                              value)
  "Return the change set that sets one property of one entity, or NIL when the value is
already current. Signals LAYER-ERROR when the entity is missing or locked, and INVALID-ENTITY
when the new value violates an invariant.

DRAWING: the drawing.
ID: entity id.
KEY: property key from ENTITY-PROPERTIES.
VALUE: new value."
  (let ((entity (store-find (drawing-store drawing) id)))
    (unless entity
      (error 'base:layer-error :reason (i18n:trf "entity ~d does not exist" id)))
    (unless (editable-entity-p drawing entity)
      (error 'base:layer-error :reason (i18n:trf "entity ~d is on a locked layer" id)))
    (let ((updated (entity-with-property entity
                                         key
                                         value)))
      (unless (equalp entity updated)
        (list (make-change id
                           entity
                           updated))))))

(defun grip-changes (drawing
                     id
                     grip
                     x
                     y)
  "Return the change set that drags a grip of one entity to a point, or NIL when that leaves the
entity as it is. Signals LAYER-ERROR when the entity is missing or locked, and INVALID-ENTITY when
the result would be invalid.

DRAWING: the drawing.
ID: entity id.
GRIP: one of the grips of the entity, from ENTITY-GRIPS.
X: world x the grip is dragged to.
Y: world y the grip is dragged to."
  (let ((entity (store-find (drawing-store drawing) id)))
    (unless entity
      (error 'base:layer-error :reason (i18n:trf "entity ~d does not exist" id)))
    (unless (editable-entity-p drawing entity)
      (error 'base:layer-error :reason (i18n:trf "entity ~d is on a locked layer" id)))
    (let ((updated (entity-with-grip entity
                                     grip
                                     x
                                     y)))
      (unless (equalp entity updated)
        (list (make-change id
                           entity
                           updated))))))

(defun assign-layer-changes (drawing
                             ids
                             layer-id)
  "Return the change set that moves the entities with IDS to another layer, and the number
skipped. Signals LAYER-ERROR when the layer does not exist.

DRAWING: the drawing.
IDS: list of entity ids.
LAYER-ID: id of the destination layer."
  (require-layer drawing layer-id)
  (multiple-value-bind (entities skipped)
      (existing-editable-entities drawing ids)
    (values (loop for entity in entities
                  unless (= layer-id (entity-layer-id entity))
                    collect (make-change (entity-id entity)
                                         entity
                                         (entity-with-layer entity layer-id)))
            skipped)))

(defun transform-entities-changes (drawing
                                   ids
                                   transform
                                   copy)
  "Return the change set that replaces the entities with IDS by what TRANSFORM makes of them, or, when COPY
is true, adds what it makes of them as new entities and leaves them as they are, and the number skipped
because they are missing or on a locked layer. Signals INVALID-ENTITY when a result would not be valid.

DRAWING: the drawing.
IDS: list of entity ids.
TRANSFORM: function of an entity that returns the transformed entity, with the same id and layer.
COPY: true to keep the originals and add the results."
  (multiple-value-bind (entities skipped)
      (existing-editable-entities drawing ids)
    (let ((next-id (drawing-next-entity-id drawing)))
      (values (mapcar (lambda (entity)
                        (let ((result (funcall transform entity)))
                          (if copy
                              (prog1 (make-change next-id
                                                  nil
                                                  (entity-with-identity result
                                                                        next-id
                                                                        (entity-layer-id entity)))
                                (incf next-id))
                              (make-change (entity-id entity)
                                           entity
                                           result))))
                      entities)
              skipped))))

(defun rotate-entities-changes (drawing
                                ids
                                center-x
                                center-y
                                angle
                                &key copy)
  "Return the change set that turns the entities with IDS about a point (or adds turned copies), and the number
skipped.

DRAWING: the drawing.
IDS: list of entity ids.
CENTER-X: x of the center of the turn.
CENTER-Y: y of the center.
ANGLE: radians, counter-clockwise.
COPY: true to keep the originals and add the turned copies."
  (transform-entities-changes drawing
                              ids
                              (lambda (entity) (entity-rotated entity
                                                               center-x
                                                               center-y
                                                               angle))
                              copy))

(defun mirror-entities-changes (drawing
                                ids
                                line-x1
                                line-y1
                                line-x2
                                line-y2
                                &key copy)
  "Return the change set that reflects the entities with IDS in the line through two points (or adds reflected
copies), and the number skipped.

DRAWING: the drawing.
IDS: list of entity ids.
LINE-X1: x of the first point of the line.
LINE-Y1: y of the first point.
LINE-X2: x of the second point.
LINE-Y2: y of the second point.
COPY: true to keep the originals and add the reflected copies."
  (transform-entities-changes drawing
                              ids
                              (lambda (entity) (entity-mirrored entity
                                                                line-x1
                                                                line-y1
                                                                line-x2
                                                                line-y2))
                              copy))

(defun scale-entities-changes (drawing
                               ids
                               center-x
                               center-y
                               factor
                               &key copy)
  "Return the change set that scales the entities with IDS about a point (or adds scaled copies), and the
number skipped.

DRAWING: the drawing.
IDS: list of entity ids.
CENTER-X: x of the center of the scaling.
CENTER-Y: y of the center.
FACTOR: a positive number.
COPY: true to keep the originals and add the scaled copies."
  (transform-entities-changes drawing
                              ids
                              (lambda (entity) (entity-scaled entity
                                                              center-x
                                                              center-y
                                                              factor))
                              copy))
