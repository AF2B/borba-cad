(in-package #:borba-cad/script)

;;;; The actions of a session as the commands that make them. A session is the steps that undo keeps, each a
;;;; list of changes, so a recording is read from them: a new entity is the command that draws it, an erased one
;;;; is ERASE, a moved one is MOVE, one whose properties changed is EDIT, and any other change is REDRAW, which
;;;; puts the new entity in the place of the old. An entity is named by its number when it was in the drawing
;;;; before the recording, and by a variable when the recording made it, because the number it will have when the
;;;; script is run is not the number it had.

(defparameter *recording-header*
  (list ";;;; Borba CAD script: the actions that were recorded in the editor, as the commands that make them."
        ";;;; Run it on the drawing as it was when the recording began: it names the entities it changes by number."
        ";;;; Lengths are millimetres and angles are degrees, unless written (rad x).")
  "The comment lines a recorded script begins with.")

(defconstant +translation-scale+ 1d9
  "What a distance is multiplied by to round it to a billionth of a millimetre.")

(defstruct (recorder (:constructor make-recorder (drawing
                                                  referenced)))
  "What is known while the steps of a session are turned into commands.

DRAWING: the drawing the session ended in, which says what the layers are called and what color they are.
REFERENCED: hash table of the ids of the entities that a step changes or erases, which are the ones that need a
name when the recording makes them.
HANDLES: hash table from the id of an entity to the variable that holds its number in the script.
DECLARED-LAYERS: hash table of the names of the layers that the script has made or used.
CURRENT-LAYER: the name of the layer that the commands written so far have made the one new entities go on.
BLOCKS: the definitions of the blocks that the script places, newest first."
  drawing
  referenced
  (handles (make-hash-table))
  (declared-layers (make-hash-table :test 'equal))
  (current-layer nil)
  (blocks '()))

;;; Naming the entities

(defun referenced-ids (steps)
  "Return a hash table with the id of every entity that a step changes or erases.

STEPS: lists of changes, oldest first."
  (let ((table (make-hash-table)))
    (dolist (step steps table)
      (dolist (change step)
        (when (dom:change-before change)
          (setf (gethash (dom:change-entity-id change) table) t))))))

(defun entity-reference (recorder
                         id)
  "Return what names an entity in the script: the variable that holds its number when the recording made it,
and the number it had otherwise.

RECORDER: the recorder.
ID: the id of the entity."
  (or (gethash id (recorder-handles recorder))
      id))

(defun reference-text (reference)
  "Return a reference to an entity as source.

REFERENCE: a number or the name of a variable."
  (if (integerp reference)
      (format nil "~d" reference)
      reference))

(defun references-text (references)
  "Return references to entities as the argument that the commands take: the one reference, or the list.

REFERENCES: a list of numbers and names of variables."
  (cond ((null (rest references))
         (reference-text (first references)))
        ((every #'integerp references)
         (format nil "'(~{~d~^ ~})" references))
        (t
         (format nil "(list~{ ~a~})" (mapcar #'reference-text references)))))

(defun handle-name (entity)
  "Return the name of the variable that holds the number of an entity that the script makes.

ENTITY: the entity."
  (format nil "*~(~a~)-~d*" (dom:entity-kind entity) (dom:entity-id entity)))

;;; Making entities

(defun layer-lines (recorder
                    layer-id)
  "Return the lines that make a layer the one new entities go on: the command that makes it, with its color, the
first time it is used, the command that selects it when another is in use, and none when it already is.

RECORDER: the recorder.
LAYER-ID: the id of the layer."
  (let ((layer (dom:find-layer (recorder-drawing recorder) layer-id)))
    (cond ((null layer)
           '())
          ((not (gethash (dom:layer-name layer) (recorder-declared-layers recorder)))
           (setf (gethash (dom:layer-name layer) (recorder-declared-layers recorder)) t
                 (recorder-current-layer recorder) (dom:layer-name layer))
           (list (layer-command layer)))
          ((not (equal (dom:layer-name layer) (recorder-current-layer recorder)))
           (setf (recorder-current-layer recorder) (dom:layer-name layer))
           (list (format nil "(layer ~a)" (string-text (dom:layer-name layer)))))
          (t
           '()))))

(defun note-block (recorder
                   entity)
  "Remember the block that an entity places, so that the script makes the block before it places it.

RECORDER: the recorder.
ENTITY: the entity."
  (when (typep entity 'dom:insert-entity)
    (let ((definition (dom:insert-block entity)))
      (unless (find (dom:block-definition-name definition)
                    (recorder-blocks recorder)
                    :key #'dom:block-definition-name
                    :test #'string=)
        (push definition (recorder-blocks recorder))))))

(defun creation-operations (recorder
                            entity)
  "Return the operations that draw an entity on its layer, naming the number it will have when the entity is
changed again later in the recording.

RECORDER: the recorder.
ENTITY: the entity."
  (note-block recorder entity)
  (let* ((id (dom:entity-id entity))
         (command (entity-command entity))
         (line (cond ((gethash id (recorder-referenced recorder))
                      (let ((name (handle-name entity)))
                        (setf (gethash id (recorder-handles recorder)) name)
                        (format nil "(defparameter ~a ~a)" name command)))
                     (t
                      command))))
    (append (mapcar (lambda (text) (list :text text))
                    (layer-lines recorder (dom:entity-layer-id entity)))
            (list (list :text line)))))

;;; Changing entities

(defun same-command-p (entity
                       other)
  "Return true when two entities are drawn by the same command, which is to say they are the same but for their
number and their layer.

ENTITY: an entity.
OTHER: an entity."
  (string= (entity-command entity)
           (entity-command other)))

(defun rounded-distance (distance)
  "Return a distance rounded to a billionth of a millimetre, which is what the float error of a move leaves.

DISTANCE: a distance in millimetres."
  (/ (fround (* distance +translation-scale+)) +translation-scale+))

(defun translation-between (entity
                            moved)
  "Return the distance along x and the distance along y that move ENTITY to where MOVED is, as a cons, or NIL when
MOVED is not ENTITY moved.

ENTITY: an entity.
MOVED: the entity as it is after the change."
  (when (eq (dom:entity-kind entity) (dom:entity-kind moved))
    (let* ((before (dom:entity-bounds entity))
           (after (dom:entity-bounds moved))
           (dx (- (geo:bounds-min-x after) (geo:bounds-min-x before)))
           (dy (- (geo:bounds-min-y after) (geo:bounds-min-y before))))
      (unless (and (zerop dx) (zerop dy))
        (dolist (candidate (list (cons (rounded-distance dx) (rounded-distance dy))
                                 (cons dx dy)))
          (when (same-command-p (dom:entity-translated entity
                                                       (car candidate)
                                                       (cdr candidate))
                                moved)
            (return candidate)))))))

(defun property-value-text (property
                            value)
  "Return the value of a property as the argument of EDIT.

PROPERTY: the property.
VALUE: its value, in the units the program counts in."
  (ecase (dom:property-kind property)
    ((:coordinate :length :factor) (number-text (float value 1d0)))
    (:angle (angle-text value))
    (:text (string-text value))
    (:integer (format nil "~d" value))
    (:boolean (boolean-text value))
    ((:unit :pattern) (keyword-text value))))

(defun edit-line (reference
                  entity
                  changed)
  "Return the EDIT command that turns ENTITY into CHANGED, or NIL when its properties do not say all that changed.

REFERENCE: what names the entity in the script.
ENTITY: an entity.
CHANGED: the entity as it is after the change."
  (let ((current entity)
        (arguments '()))
    (when (eq (dom:entity-kind entity) (dom:entity-kind changed))
      (dolist (property (dom:entity-properties entity))
        (let ((other (find (dom:property-key property)
                           (dom:entity-properties changed)
                           :key #'dom:property-key)))
          (when (and other
                     (dom:property-editable property)
                     (not (equalp (dom:property-value property) (dom:property-value other))))
            (setf current (handler-case (dom:entity-with-property current
                                                                 (dom:property-key property)
                                                                 (dom:property-value other))
                            (base:invalid-entity ()
                              (return-from edit-line nil))))
            (push (format nil "~a ~a"
                          (keyword-text (dom:property-key property))
                          (property-value-text property (dom:property-value other)))
                  arguments))))
      (and arguments
           (same-command-p current changed)
           (format nil "(edit ~a~{ ~a~})" (reference-text reference) (reverse arguments))))))

(defun change-operation (reference
                         entity
                         changed)
  "Return the operation that turns ENTITY into CHANGED, which differs from it in more than its layer: a move, an
edit, or the new entity drawn in the place of the old. Returns NIL for a placement of a block, which no command can
draw in place of another.

REFERENCE: what names the entity in the script.
ENTITY: an entity.
CHANGED: the entity as it became."
  (let ((translation (translation-between entity changed))
        (edit (edit-line reference
                         entity
                         changed)))
    (cond (translation
           (list :move
                 (list reference)
                 (car translation)
                 (cdr translation)))
          (edit
           (list :text edit))
          ((not (typep changed 'dom:insert-entity))
           (list :text (format nil "(redraw ~a ~a)"
                               (reference-text reference)
                               (entity-command changed)))))))

(defun replacement-operations (recorder
                               entity
                               changed)
  "Return the operations that turn an entity into what it became: a move to another layer, a move, an edit, the
new entity drawn in the place of the old, or, for a placement of a block, the old one erased and the new one drawn.

RECORDER: the recorder.
ENTITY: the entity as it was.
CHANGED: the entity as it became."
  (let* ((reference (entity-reference recorder (dom:entity-id entity)))
         (layer-id (dom:entity-layer-id changed))
         (layer (dom:find-layer (recorder-drawing recorder) layer-id))
         (moved-layer (and layer
                           (/= (dom:entity-layer-id entity) layer-id)))
         (current (if moved-layer
                      (dom:entity-with-layer entity layer-id)
                      entity)))
    (flet ((layer-operations ()
             (and moved-layer
                  (append (mapcar (lambda (text) (list :text text))
                                  (layer-lines recorder layer-id))
                          (list (list :layer
                                      (list reference)
                                      (dom:layer-name layer)))))))
      (if (same-command-p current changed)
          (layer-operations)
          (let ((operation (change-operation reference
                                             current
                                             changed)))
            (if operation
                (append (layer-operations)
                        (list operation))
                (cons (list :erase (list reference))
                      (creation-operations recorder changed))))))))

(defun step-operations (recorder
                        step)
  "Return the operations that make one step: for each of its changes, what draws, erases or changes the entity.

RECORDER: the recorder.
STEP: a list of changes."
  (loop for change in step
        append (let ((before (dom:change-before change))
                     (after (dom:change-after change)))
                 (cond ((null before)
                        (creation-operations recorder after))
                       ((null after)
                        (list (list :erase
                                    (list (entity-reference recorder (dom:change-entity-id change))))))
                       (t
                        (replacement-operations recorder
                                                before
                                                after))))))

;;; Writing

(defun mergeable-p (previous
                    operation)
  "Return true when two operations in a row do the same to different entities, so that one command can do both.

PREVIOUS: an operation, or NIL.
OPERATION: the operation that follows it."
  (and previous
       (eq (first previous) (first operation))
       (case (first operation)
         (:move (and (= (third previous) (third operation))
                     (= (fourth previous) (fourth operation))))
         (:layer (string= (third previous) (third operation)))
         (:erase t)
         (t nil))))

(defun merged-operations (operations)
  "Return OPERATIONS with the moves, the erasures and the changes of layer that follow each other and are the same
made into one operation on the entities of all of them.

OPERATIONS: a list of operations."
  (let ((merged '()))
    (dolist (operation operations (nreverse merged))
      (if (mergeable-p (first merged) operation)
          (setf (second (first merged)) (append (second (first merged)) (second operation)))
          (push (copy-list operation) merged)))))

(defun operation-line (operation)
  "Return the command that does an operation.

OPERATION: a list that begins with :text, :move, :erase or :layer."
  (ecase (first operation)
    (:text (second operation))
    (:move (format nil "(move ~a ~a ~a)"
                   (references-text (second operation))
                   (number-text (third operation))
                   (number-text (fourth operation))))
    (:erase (format nil "(erase ~a)" (references-text (second operation))))
    (:layer (format nil "(move-to-layer ~a ~a)"
                    (references-text (second operation))
                    (string-text (third operation))))))

(defun steps-script (steps
                     drawing)
  "Return the text of a script that makes STEPS again: the commands of each step, a blank line between steps,
after the blocks that the steps place.

STEPS: lists of changes, oldest first, as undo keeps them.
DRAWING: the drawing the steps ended in."
  (let* ((recorder (make-recorder drawing (referenced-ids steps)))
         (groups (mapcar (lambda (step)
                           (mapcar #'operation-line
                                   (merged-operations (step-operations recorder step))))
                         steps)))
    (with-output-to-string (stream)
      (dolist (line *recording-header*)
        (write-line line stream))
      (terpri stream)
      (when (recorder-blocks recorder)
        (dolist (definition (reverse (recorder-blocks recorder)))
          (write-block-definition definition stream))
        (terpri stream))
      (dolist (lines groups)
        (when lines
          (dolist (line lines)
            (write-line line stream))
          (terpri stream))))))
