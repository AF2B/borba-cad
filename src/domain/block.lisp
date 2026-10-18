(in-package #:borba-cad/domain)

;;;; Blocks: a group of entities that is defined once, under a name, and placed in a drawing as often as wanted. A
;;;; definition never changes once it is made, so a placement can hold the definition itself. The entities of a
;;;; definition are kept as if its base point were the origin, with ids from 1 and on the default layer; where a block
;;;; is placed, they take the layer of the placement.

(defconstant +maximum-block-name-length+ 64
  "Longest accepted block name.")

(defconstant +maximum-block-entities+ 20000
  "Most entities one block may hold.")

(defstruct (block-definition (:constructor %make-block-definition (name
                                                                     entities
                                                                     bounds))
                             (:copier nil))
  "A named group of entities.

NAME: what the block is called, with the case the user gave it.
ENTITIES: the entities, in the order they were drawn, with the base point of the block at the origin.
BOUNDS: the bounds of the entities."
  (name "" :type string :read-only t)
  (entities nil :type list :read-only t)
  (bounds nil :type (or null geo:bounds) :read-only t))

(defun block-name-key (name)
  "Return what blocks are compared by: the name without case, so that Door and door are one block.

NAME: a block name."
  (string-downcase name))

(defun clean-block-name (name)
  "Return a block name trimmed, or signal EDIT-REFUSED when it is empty, too long or holds a control character.

NAME: what the user typed."
  (let ((trimmed (string-trim " " name)))
    (when (zerop (length trimmed))
      (error 'base:edit-refused :reason (i18n:tr "A block needs a name.")))
    (when (> (length trimmed) +maximum-block-name-length+)
      (error 'base:edit-refused
             :reason (i18n:trf "A block name has at most ~d characters." +maximum-block-name-length+)))
    (when (find-if (lambda (character) (< (char-code character) 32)) trimmed)
      (error 'base:edit-refused :reason (i18n:tr "A block name cannot hold control characters.")))
    trimmed))

(defun make-block-definition (&key name
                                   entities
                                   (base-x 0d0)
                                   (base-y 0d0))
  "Create a block from entities that are in a drawing, moved so that the base point is the origin and given the ids 1
and on and the default layer. Signals EDIT-REFUSED when the name is no good, there are no entities, there are too many
of them, or one of them is a placement of a block, which a block cannot hold; and INVALID-ENTITY when moving one makes
it invalid.

NAME: what the block is called.
ENTITIES: the entities, in the order they are to be drawn.
BASE-X: x of the point that is the origin of the block.
BASE-Y: y of that point."
  (let ((clean (clean-block-name name)))
    (when (null entities)
      (error 'base:edit-refused :reason (i18n:tr "A block needs at least one entity.")))
    (when (> (length entities) +maximum-block-entities+)
      (error 'base:edit-refused
             :reason (i18n:trf "A block holds at most ~d entities." +maximum-block-entities+)))
    (when (find :insert entities :key #'entity-kind)
      (error 'base:edit-refused :reason (i18n:tr "A block cannot hold a block: explode it first.")))
    (let ((placed (loop for entity in entities
                        for id from 1
                        collect (entity-with-identity (entity-translated entity
                                                                         (- base-x)
                                                                         (- base-y))
                                                      id
                                                      +default-layer-id+))))
      (%make-block-definition clean
                              placed
                              (entities-bounds placed)))))
