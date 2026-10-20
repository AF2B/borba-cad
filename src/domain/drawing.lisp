(in-package #:borba-cad/domain)

(defconstant +default-grid-spacing+ 100d0
  "Default grid spacing in millimetres (10 cm): a convenient step for building plans.")

(defconstant +minimum-grid-spacing+ 1d0
  "Smallest accepted grid spacing in millimetres. Finer grids add nothing the user can click
at any practical zoom and would be hidden by the adaptive grid display.")

(defconstant +default-text-height+ 250d0
  "Default text height in millimetres: 2.5 mm printed at a 1:100 plan scale.")

(defconstant +maximum-layer-name-length+ 64
  "Longest accepted layer name.")

(defstruct (drawing (:constructor %make-drawing ())
                    (:copier nil))
  "The drawing aggregate: layers, entities and drawing-wide settings.

STORE: the entities.
LAYERS: id to layer.
LAYER-ORDER: layer ids in display order.
BLOCKS: block name, without case, to the block definition.
CURRENT-LAYER-ID: layer that receives new entities.
UNIT: unit in which coordinates are stored (always :millimeter in format version 1).
DISPLAY-UNIT: unit in which the user sees and types lengths.
GRID-SPACING: grid and snap spacing in millimetres.
GRID-VISIBLE: whether the grid is drawn.
SNAP-TO-GRID: whether pointer positions snap to the grid.
OBJECT-SNAP: whether pointer positions snap to entity endpoints and centers.
DEFAULT-TEXT-HEIGHT: text height used for new text and dimensions.
NEXT-ENTITY-ID: id the next new entity receives; never decreases.
NEXT-LAYER-ID: id the next new layer receives; never decreases.
SHEETS: the sheets that are printed, in order; see sheets.lisp.
UNDERLAYS: the pictures under the drawing, from the lowest; see underlays.lisp.
NEXT-UNDERLAY-ID: number the next underlay receives; never decreases.
REVISION: counter bumped by every change, for dirty tracking and cache invalidation."
  (store (make-entity-store) :type entity-store :read-only t)
  (layers (make-hash-table) :type hash-table :read-only t)
  (layer-order nil :type list)
  (blocks (make-hash-table :test 'equal) :type hash-table :read-only t)
  (sheets nil :type list)
  (underlays nil :type list)
  (next-underlay-id 1 :type (integer 1))
  (current-layer-id +default-layer-id+ :type (integer 0))
  (unit :millimeter :type keyword)
  (display-unit :millimeter :type keyword)
  (grid-spacing +default-grid-spacing+ :type double-float)
  (grid-visible t :type boolean)
  (snap-to-grid t :type boolean)
  (object-snap t :type boolean)
  (default-text-height +default-text-height+ :type double-float)
  (next-entity-id 1 :type (integer 1))
  (next-layer-id 1 :type (integer 1))
  (revision 0 :type fixnum))

(defun touch-drawing (drawing)
  "Record that DRAWING changed.

DRAWING: the drawing."
  (incf (drawing-revision drawing)))

(defun make-drawing ()
  "Create an empty drawing with the default layer, millimetre units and default settings."
  (let ((drawing (%make-drawing)))
    (setf (gethash +default-layer-id+ (drawing-layers drawing))
          (%make-layer +default-layer-id+
                       (default-layer-name)
                       (default-layer-color)
                       t
                       nil))
    (setf (drawing-layer-order drawing) (list +default-layer-id+))
    drawing))

(defun drawing-entity-count (drawing)
  "Return the number of entities in DRAWING.

DRAWING: the drawing."
  (store-count (drawing-store drawing)))

;;; Blocks

(defun find-block (drawing
                   name)
  "Return the block called NAME, compared without regard to case, or NIL.

DRAWING: the drawing.
NAME: a block name."
  (values (gethash (block-name-key name) (drawing-blocks drawing))))

(defun block-definitions (drawing)
  "Return the blocks of the drawing, sorted by name.

DRAWING: the drawing."
  (sort (loop for definition being the hash-values of (drawing-blocks drawing)
              collect definition)
        #'string-lessp
        :key #'block-definition-name))

(defun block-names (drawing)
  "Return the names of the blocks of the drawing, sorted.

DRAWING: the drawing."
  (mapcar #'block-definition-name (block-definitions drawing)))

(defun block-in-use-p (drawing
                       name)
  "Return true when an entity of the drawing is a placement of the block called NAME.

DRAWING: the drawing.
NAME: a block name."
  (let ((key (block-name-key name))
        (used nil))
    (map-entities (lambda (entity)
                    (when (and (eq :insert (entity-kind entity))
                               (string= key (block-name-key (block-definition-name (insert-block entity)))))
                      (setf used t)))
                  (drawing-store drawing))
    used))

(defun define-block (drawing
                     definition)
  "Add a block to the drawing, or replace the block of the same name when no entity places it. Signals EDIT-REFUSED when
a block of that name is placed in the drawing.

DRAWING: the drawing.
DEFINITION: the block definition."
  (let ((name (block-definition-name definition)))
    (when (and (find-block drawing name) (block-in-use-p drawing name))
      (error 'base:edit-refused
             :reason (i18n:trf "A block called ~a is already in use in this drawing." name)))
    (setf (gethash (block-name-key name) (drawing-blocks drawing)) definition)
    (touch-drawing drawing)
    definition))

(defun remove-block (drawing
                     name)
  "Take the block called NAME out of the drawing. Signals EDIT-REFUSED when it is placed.

DRAWING: the drawing.
NAME: a block name."
  (when (block-in-use-p drawing name)
    (error 'base:edit-refused
           :reason (i18n:trf "A block called ~a is in use and cannot be removed." name)))
  (remhash (block-name-key name) (drawing-blocks drawing))
  (touch-drawing drawing))

;;; Layers

(defun find-layer (drawing
                   id)
  "Return the layer with ID, or NIL.

DRAWING: the drawing.
ID: layer id."
  (values (gethash id (drawing-layers drawing))))

(defun layers-in-order (drawing)
  "Return the layers of DRAWING in display order.

DRAWING: the drawing."
  (mapcar (lambda (id) (find-layer drawing id)) (drawing-layer-order drawing)))

(defun find-layer-by-name (drawing
                           name)
  "Return the layer called NAME (compared case-insensitively), or NIL.

DRAWING: the drawing.
NAME: layer name."
  (find name (layers-in-order drawing) :key #'layer-name :test #'string-equal))

(defun layer-failure (control
                      &rest arguments)
  "Signal a LAYER-ERROR whose reason is built from CONTROL and ARGUMENTS.

CONTROL: format control string.
ARGUMENTS: format arguments."
  (error 'base:layer-error
         :reason (apply #'i18n:trf control arguments)))

(defun require-layer (drawing
                      id)
  "Return the layer with ID or signal LAYER-ERROR.

DRAWING: the drawing.
ID: layer id."
  (or (find-layer drawing id)
      (layer-failure "there is no layer with id ~d" id)))

(defun require-layer-name (drawing
                           name
                           &optional ignored-id)
  "Return NAME trimmed, or signal LAYER-ERROR when it is empty, too long or already used.

DRAWING: the drawing.
NAME: proposed layer name.
IGNORED-ID: id of a layer that may keep this name (when renaming it)."
  (let ((trimmed (string-trim " " (or name ""))))
    (when (zerop (length trimmed))
      (layer-failure "the layer name cannot be empty"))
    (when (> (length trimmed) +maximum-layer-name-length+)
      (layer-failure "the layer name is longer than ~d characters" +maximum-layer-name-length+))
    (let ((existing (find-layer-by-name drawing trimmed)))
      (when (and existing (not (eql (layer-id existing) ignored-id)))
        (layer-failure "a layer named ~s already exists" trimmed)))
    trimmed))

(defun store-layer (drawing
                    layer)
  "Put LAYER into DRAWING, replacing any layer with the same id, and record the change.

DRAWING: the drawing.
LAYER: the layer value."
  (setf (gethash (layer-id layer) (drawing-layers drawing)) layer)
  (touch-drawing drawing)
  layer)

(defun add-layer (drawing
                  name
                  color)
  "Create a layer with the next layer id and return it. Signals LAYER-ERROR when NAME is empty,
too long or already used.

DRAWING: the drawing.
NAME: layer name.
COLOR: an RGB color."
  (let* ((trimmed (require-layer-name drawing name))
         (id (drawing-next-layer-id drawing))
         (layer (%make-layer id
                             trimmed
                             color
                             t
                             nil)))
    (setf (drawing-next-layer-id drawing) (1+ id))
    (setf (drawing-layer-order drawing) (append (drawing-layer-order drawing) (list id)))
    (store-layer drawing layer)))

(defun restore-layer (drawing
                      id
                      name
                      color
                      visible
                      locked)
  "Add a layer with an explicit id, as when loading a file. Signals LAYER-ERROR on a duplicate
id or name. Layer 0 is the default layer and replaces the one made by MAKE-DRAWING.

DRAWING: the drawing.
ID: layer id.
NAME: layer name.
COLOR: an RGB color.
VISIBLE: visibility flag.
LOCKED: lock flag."
  (let ((existing (find-layer drawing id)))
    (when (and existing (/= id +default-layer-id+))
      (layer-failure "layer id ~d is used twice" id))
    (let* ((trimmed (require-layer-name drawing
                                        name
                                        id))
           (layer (%make-layer id
                               trimmed
                               color
                               visible
                               locked)))
      (unless existing
        (setf (drawing-layer-order drawing) (append (drawing-layer-order drawing) (list id))))
      (setf (drawing-next-layer-id drawing) (max (drawing-next-layer-id drawing) (1+ id)))
      (store-layer drawing layer))))

(defun replace-layer (drawing
                      id
                      &key (name nil name-supplied)
                           (color nil color-supplied)
                           (visible nil visible-supplied)
                           (locked nil locked-supplied))
  "Replace the layer ID with a copy having the supplied fields changed.

DRAWING: the drawing.
ID: layer id.
NAME: new name, when supplied.
COLOR: new color, when supplied.
VISIBLE: new visibility, when supplied.
LOCKED: new lock flag, when supplied."
  (let ((layer (require-layer drawing id)))
    (store-layer drawing
                 (%make-layer id
                              (if name-supplied (require-layer-name drawing
                                                                    name
                                                                    id) (layer-name layer))
                              (if color-supplied color (layer-color layer))
                              (if visible-supplied visible (layer-visible layer))
                              (if locked-supplied locked (layer-locked layer))))))

(defun rename-layer (drawing
                     id
                     name)
  "Rename a layer. Signals LAYER-ERROR when NAME is empty, too long or used by another layer.

DRAWING: the drawing.
ID: layer id.
NAME: the new name."
  (replace-layer drawing
                 id
                 :name name))

(defun recolor-layer (drawing
                      id
                      color)
  "Change a layer's color.

DRAWING: the drawing.
ID: layer id.
COLOR: an RGB color."
  (replace-layer drawing
                 id
                 :color color))

(defun set-layer-visible (drawing
                          id
                          visible)
  "Show or hide a layer.

DRAWING: the drawing.
ID: layer id.
VISIBLE: true to show."
  (replace-layer drawing
                 id
                 :visible (and visible t)))

(defun set-layer-locked (drawing
                         id
                         locked)
  "Lock or unlock a layer.

DRAWING: the drawing.
ID: layer id.
LOCKED: true to lock."
  (replace-layer drawing
                 id
                 :locked (and locked t)))

(defun set-current-layer (drawing
                          id)
  "Make a layer the one that receives new entities. Signals LAYER-ERROR when it is missing.

DRAWING: the drawing.
ID: layer id."
  (require-layer drawing id)
  (setf (drawing-current-layer-id drawing) id)
  (touch-drawing drawing)
  id)

(defun layer-entity-count (drawing
                           layer-id)
  "Return how many entities are on a layer.

DRAWING: the drawing.
LAYER-ID: layer id."
  (let ((count 0))
    (map-entities (lambda (entity)
                    (when (= layer-id (entity-layer-id entity))
                      (incf count)))
                  (drawing-store drawing))
    count))

(defun remove-layer (drawing
                     id)
  "Delete an empty layer. Signals LAYER-ERROR for the default layer or a layer that still
holds entities. The caller must clear the undo history, which may refer to the layer.

DRAWING: the drawing.
ID: layer id."
  (let ((layer (require-layer drawing id)))
    (when (= id +default-layer-id+)
      (layer-failure "the default layer cannot be deleted"))
    (let ((count (layer-entity-count drawing id)))
      (when (plusp count)
        (layer-failure "layer ~s still holds ~d ~:*~[entities~;entity~:;entities~]; move or delete them first"
                       (layer-name layer)
                       count)))
    (remhash id (drawing-layers drawing))
    (setf (drawing-layer-order drawing) (remove id (drawing-layer-order drawing)))
    (when (= id (drawing-current-layer-id drawing))
      (setf (drawing-current-layer-id drawing) +default-layer-id+))
    (touch-drawing drawing)
    id))

;;; Settings

(defun set-display-unit (drawing
                         unit)
  "Choose the unit in which lengths are shown and typed.

DRAWING: the drawing.
UNIT: a supported length unit keyword."
  (check-type unit base:length-unit)
  (setf (drawing-display-unit drawing) unit)
  (touch-drawing drawing)
  unit)

(defun valid-grid-spacing-p (value)
  "Return true when VALUE is an acceptable grid spacing in millimetres.

VALUE: any object."
  (and (typep value 'double-float)
       (>= value +minimum-grid-spacing+)
       (<= value base:+maximum-coordinate-magnitude+)))

(defun set-grid-spacing (drawing
                         spacing)
  "Set the grid and snap spacing. Signals INVALID-INPUT when it is below the minimum.

DRAWING: the drawing.
SPACING: spacing in millimetres, at least +MINIMUM-GRID-SPACING+."
  (unless (valid-grid-spacing-p spacing)
    (error 'base:invalid-input
           :text (princ-to-string spacing)
           :reason (i18n:trf "the grid spacing must be between ~f and ~f mm"
                           +minimum-grid-spacing+
                           base:+maximum-coordinate-magnitude+)))
  (setf (drawing-grid-spacing drawing) spacing)
  (touch-drawing drawing)
  spacing)

(defun set-grid-visible (drawing
                         visible)
  "Show or hide the grid.

DRAWING: the drawing.
VISIBLE: true to show."
  (setf (drawing-grid-visible drawing) (and visible t))
  (touch-drawing drawing)
  visible)

(defun set-snap-to-grid (drawing
                         enabled)
  "Enable or disable snapping to the grid.

DRAWING: the drawing.
ENABLED: true to snap."
  (setf (drawing-snap-to-grid drawing) (and enabled t))
  (touch-drawing drawing)
  enabled)

(defun set-object-snap (drawing
                        enabled)
  "Enable or disable snapping to entity endpoints and centers.

DRAWING: the drawing.
ENABLED: true to snap."
  (setf (drawing-object-snap drawing) (and enabled t))
  (touch-drawing drawing)
  enabled)

(defun set-default-text-height (drawing
                                height)
  "Set the text height used for new text and dimensions.

DRAWING: the drawing.
HEIGHT: height in millimetres, above the length tolerance."
  (unless (base:positive-length-p height)
    (error 'base:invalid-input
           :text (princ-to-string height)
           :reason (i18n:tr "the text height must be a positive length")))
  (setf (drawing-default-text-height drawing) height)
  (touch-drawing drawing)
  height)

(defun restore-drawing-state (drawing
                              &key current-layer-id
                                   next-entity-id
                                   next-layer-id
                                   display-unit
                                   grid-spacing
                                   grid-visible
                                   snap-to-grid
                                   object-snap
                                   default-text-height)
  "Set the drawing-wide state read from a file. Validation errors use the same conditions as
the individual setters.

DRAWING: the drawing, after its layers were restored.
CURRENT-LAYER-ID: id of the current layer.
NEXT-ENTITY-ID: next entity id, above every existing id.
NEXT-LAYER-ID: next layer id, above every existing id.
DISPLAY-UNIT: display unit keyword.
GRID-SPACING: grid spacing in millimetres.
GRID-VISIBLE: grid visibility.
SNAP-TO-GRID: grid snap flag.
OBJECT-SNAP: object snap flag.
DEFAULT-TEXT-HEIGHT: default text height."
  (set-current-layer drawing current-layer-id)
  (set-display-unit drawing display-unit)
  (set-grid-spacing drawing grid-spacing)
  (set-grid-visible drawing grid-visible)
  (set-snap-to-grid drawing snap-to-grid)
  (set-object-snap drawing object-snap)
  (set-default-text-height drawing default-text-height)
  (setf (drawing-next-entity-id drawing) (max next-entity-id (drawing-next-entity-id drawing))
        (drawing-next-layer-id drawing) (max next-layer-id (drawing-next-layer-id drawing)))
  drawing)
