(in-package #:borba-cad/domain)

;;;; Working with blocks: making one of entities, placing one, and taking a placement apart into the entities it stands
;;;; for. Like the other commands these work out change sets and change nothing; the one thing they leave to the
;;;; caller is to add a new definition to the drawing with DEFINE-BLOCK before the change set is applied.

(defun make-block-changes (drawing
                           ids
                           name
                           base-x
                           base-y)
  "Return the definition of a block made of the entities with IDS, and the change set that replaces them with a
placement of it at the base point, on the layer of the first of them, and how many of IDS were skipped because they are
missing or on a locked layer. The caller adds the definition to the drawing before applying the changes. Signals
EDIT-REFUSED when there is nothing to make it of, the name is no good or is that of a block in use, or one of the
entities is a placement; and INVALID-ENTITY when the placement would not be valid.

DRAWING: the drawing.
IDS: list of entity ids, in the order they are to be drawn in the block.
NAME: what the block is called.
BASE-X: x of the base point of the block.
BASE-Y: y of the base point."
  (multiple-value-bind (entities skipped) (existing-editable-entities drawing ids)
    (let ((definition (make-block-definition :name name
                                             :entities entities
                                             :base-x base-x
                                             :base-y base-y)))
      (when (and (find-block drawing name) (block-in-use-p drawing name))
        (error 'base:edit-refused
               :reason (i18n:trf "A block called ~a is already in use in this drawing."
                                 (block-definition-name definition))))
      (values definition
              (append (mapcar (lambda (entity) (make-change (entity-id entity)
                                                            entity
                                                            nil))
                              entities)
                      (create-entity-changes (make-insert :id (drawing-next-entity-id drawing)
                                                          :layer-id (entity-layer-id (first entities))
                                                          :block definition
                                                          :x base-x
                                                          :y base-y)))
              skipped))))

(defun insert-changes (drawing
                       name
                       x
                       y
                       &key (scale 1d0)
                            (rotation 0d0)
                            flipped)
  "Return the change set that places the block called NAME on the current layer. Signals EDIT-REFUSED when there is no
such block, and INVALID-ENTITY when the placement would not be valid.

DRAWING: the drawing.
NAME: the name of the block.
X: x of the point the base point of the block goes to.
Y: y of that point.
SCALE: how many times larger than the definition.
ROTATION: the turn, in radians.
FLIPPED: true to reflect the block across its vertical axis first."
  (let ((definition (find-block drawing name)))
    (unless definition
      (error 'base:edit-refused
             :reason (i18n:trf "There is no block called ~a." name)))
    (create-entity-changes (make-insert :id (drawing-next-entity-id drawing)
                                        :layer-id (drawing-current-layer-id drawing)
                                        :block definition
                                        :x x
                                        :y y
                                        :scale scale
                                        :rotation rotation
                                        :flipped flipped))))

(defun explode-changes (drawing
                        ids)
  "Return the change set that replaces each placement of a block among the entities with IDS by the entities it
stands for, on its layer, and how many placements were exploded and how many of IDS were skipped (missing, on a
locked layer, or not placements).

DRAWING: the drawing.
IDS: list of entity ids."
  (multiple-value-bind (entities missing) (existing-editable-entities drawing ids)
    (let ((next-id (drawing-next-entity-id drawing))
          (changes '())
          (exploded 0)
          (skipped missing))
      (dolist (entity entities)
        (cond ((eq :insert (entity-kind entity))
               (push (make-change (entity-id entity)
                                  entity
                                  nil)
                     changes)
               (dolist (piece (insert-entities entity))
                 (push (make-change next-id
                                    nil
                                    (entity-with-identity piece
                                                          next-id
                                                          (entity-layer-id entity)))
                       changes)
                 (incf next-id))
               (incf exploded))
              (t (incf skipped))))
      (values (nreverse changes)
              exploded
              skipped))))
