(in-package #:borba-cad/domain)

;;;; Change sets. Every mutation of the entities of a drawing is a list of CHANGE records. A
;;;; change set is applied atomically and can be inverted, which is all that undo needs.

(defconstant +history-depth-limit+ 500
  "Most change sets kept for undo. Bounds the memory used by history (NFR-6).")

(defstruct (change (:constructor make-change (entity-id
                                              before
                                              after)))
  "One entity's transition inside a change set.

ENTITY-ID: id of the entity.
BEFORE: the entity as it was, or NIL when the change creates it.
AFTER: the entity as it becomes, or NIL when the change deletes it."
  (entity-id 1 :type (integer 1) :read-only t)
  (before nil :type (or null entity) :read-only t)
  (after nil :type (or null entity) :read-only t))

(defun state-mismatch (id)
  "Signal an error stating that a change set does not match the drawing state.

ID: id of the entity whose state differs."
  (error "The change set does not match the drawing: entity ~d is not in the expected state" id))

(defun classify-changes (drawing
                         changes)
  "Validate CHANGES against DRAWING without modifying it, and split them into removals (ids),
additions and replacements (entities). Signals an error when a change does not fit.

DRAWING: the drawing.
CHANGES: list of CHANGE records."
  (let ((store (drawing-store drawing))
        (seen (make-hash-table))
        (removals '())
        (additions '())
        (replacements '()))
    (dolist (change changes)
      (let* ((id (change-entity-id change))
             (before (change-before change))
             (after (change-after change))
             (current (store-find store id)))
        (when (gethash id seen)
          (error "The change set touches entity ~d more than once" id))
        (setf (gethash id seen) t)
        (when (or (and before (/= id (entity-id before)))
                  (and after (/= id (entity-id after))))
          (error "A change for entity ~d carries an entity with another id" id))
        (cond ((and before after)
               (unless (eq current before) (state-mismatch id))
               (push after replacements))
              (before
               (unless (eq current before) (state-mismatch id))
               (push id removals))
              (after
               (when current (state-mismatch id))
               (push after additions))
              (t (error "The change for entity ~d is empty" id)))
        (when after
          (unless (find-layer drawing (entity-layer-id after))
            (error 'base:layer-error
                   :reason (format nil "entity ~d refers to missing layer ~d"
                                   id
                                   (entity-layer-id after)))))))
    (values removals additions replacements)))

(defun apply-changes (drawing
                      changes)
  "Apply a change set atomically: it is fully validated before the first modification, so a
failure leaves DRAWING untouched. Bumps the revision.

DRAWING: the drawing.
CHANGES: list of CHANGE records, each entity appearing at most once."
  (multiple-value-bind (removals additions replacements)
      (classify-changes drawing changes)
    (let ((store (drawing-store drawing)))
      (when removals
        (store-remove-many store removals))
      (when additions
        (store-add-many store additions))
      (dolist (entity replacements)
        (store-replace store entity))
      (dolist (entity additions)
        (setf (drawing-next-entity-id drawing)
              (max (drawing-next-entity-id drawing) (1+ (entity-id entity))))))
    (touch-drawing drawing)
    drawing))

(defun invert-changes (changes)
  "Return the change set that undoes CHANGES.

CHANGES: list of CHANGE records."
  (mapcar (lambda (change)
            (make-change (change-entity-id change)
                         (change-after change)
                         (change-before change)))
          (reverse changes)))

(defstruct (history (:constructor make-history (&key (limit +history-depth-limit+))))
  "Undo and redo stacks of change sets.

UNDO-STACK: applied change sets, most recent first.
REDO-STACK: undone change sets, most recently undone first.
LIMIT: most change sets kept in the undo stack."
  (undo-stack nil :type list)
  (redo-stack nil :type list)
  (limit +history-depth-limit+ :type (integer 1)))

(defun history-can-undo-p (history)
  "Return true when there is something to undo.

HISTORY: a history."
  (and (history-undo-stack history) t))

(defun history-can-redo-p (history)
  "Return true when there is something to redo.

HISTORY: a history."
  (and (history-redo-stack history) t))

(defun history-undo-depth (history)
  "Return the number of change sets that can be undone.

HISTORY: a history."
  (length (history-undo-stack history)))

(defun history-redo-depth (history)
  "Return the number of change sets that can be redone.

HISTORY: a history."
  (length (history-redo-stack history)))

(defun history-commit (history
                       drawing
                       changes)
  "Apply CHANGES to DRAWING and record them for undo, discarding the redo stack. An empty
change set is ignored and returns NIL.

HISTORY: the history to record into.
DRAWING: the drawing to change.
CHANGES: list of CHANGE records."
  (when changes
    (apply-changes drawing changes)
    (push changes (history-undo-stack history))
    (setf (history-redo-stack history) nil)
    (let ((limit (history-limit history)))
      (when (> (length (history-undo-stack history)) limit)
        (setf (cdr (nthcdr (1- limit) (history-undo-stack history))) nil)))
    t))

(defun history-record (history
                       changes)
  "Record CHANGES, which have already been applied to the drawing, for undo, discarding the redo
stack: what a command that made its changes one by one, to see them as it went, leaves as one
step. An empty change set is ignored and returns NIL.

HISTORY: the history to record into.
CHANGES: list of CHANGE records, each entity appearing at most once, that take the drawing from
how it was before the command to how it is now."
  (when changes
    (push changes (history-undo-stack history))
    (setf (history-redo-stack history) nil)
    (let ((limit (history-limit history)))
      (when (> (length (history-undo-stack history)) limit)
        (setf (cdr (nthcdr (1- limit) (history-undo-stack history))) nil)))
    t))

(defun history-undo (history
                     drawing)
  "Undo the most recent change set; return true, or NIL when there is nothing to undo.

HISTORY: the history.
DRAWING: the drawing to change."
  (let ((changes (first (history-undo-stack history))))
    (when changes
      (apply-changes drawing (invert-changes changes))
      (pop (history-undo-stack history))
      (push changes (history-redo-stack history))
      t)))

(defun history-redo (history
                     drawing)
  "Redo the most recently undone change set; return true, or NIL when there is nothing to redo.

HISTORY: the history.
DRAWING: the drawing to change."
  (let ((changes (first (history-redo-stack history))))
    (when changes
      (apply-changes drawing changes)
      (pop (history-redo-stack history))
      (push changes (history-undo-stack history))
      t)))

(defun history-clear (history)
  "Forget every undoable and redoable change set.

HISTORY: the history."
  (setf (history-undo-stack history) nil
        (history-redo-stack history) nil)
  history)
