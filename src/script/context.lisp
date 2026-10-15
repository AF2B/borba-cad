(in-package #:borba-cad/script)

;;;; The context a command runs in, and the transaction that goes with it. A command acts on the drawing
;;;; at once, so that the next step of the same command sees what the last one did; what it did is kept as
;;;; the net change of each entity, so that the whole command can be undone as one step, or taken back
;;;; at once when it fails half way.

(defparameter *maximum-changes* 2000000
  "Most entities a single command may create, change or erase: well beyond what a drawing is meant to hold,
and a bound on the memory a runaway loop can ask for.")

(defstruct (script-context (:constructor make-script-context (drawing
                                                              &key selection-reader
                                                                   selection-writer
                                                                   view-fitter)))
  "What a command runs against.

DRAWING: the drawing it draws on.
SELECTION-READER: function of no arguments that returns the ids of the selected entities, or NIL.
SELECTION-WRITER: function of a list of ids that selects them, or NIL.
VIEW-FITTER: function of no arguments that zooms to the whole drawing, or NIL.
NET: hash table from entity id to a cons of how the entity was when the command began (NIL when it did not
exist) and how it is now (NIL when it is gone).
CHANGE-COUNT: how many changes the command has made."
  drawing
  selection-reader
  selection-writer
  view-fitter
  (net (make-hash-table))
  (change-count 0 :type fixnum))

(defvar *context* nil
  "The context of the command that is running.")

(defun current-context ()
  "Return the context of the command that is running, or signal an error when there is none."
  (or *context*
      (error "This command can only be used where a drawing is being edited.")))

(defun context-changes (context)
  "Return the net changes of the command as a list of CHANGE records, in id order: what takes the drawing
from how it was when the command began to how it is. An entity that was created and erased again is not
in it, and one that was changed back is not either.

CONTEXT: a script context."
  (let ((changes '()))
    (maphash (lambda (id entry)
               (unless (equalp (car entry) (cdr entry))
                 (push (dom:make-change id
                                        (car entry)
                                        (cdr entry)) changes)))
             (script-context-net context))
    (sort changes #'< :key #'dom:change-entity-id)))

(defun context-reset (context)
  "Forget what the command did, which is to say that it is done and recorded: the next command begins
from the drawing as it is now.

CONTEXT: a script context."
  (clrhash (script-context-net context))
  (setf (script-context-change-count context) 0))

(defun context-rollback (context)
  "Undo everything the command did, so that the drawing is as it was when the command began.

CONTEXT: a script context."
  (let ((changes (context-changes context)))
    (when changes
      (sbcl:with-uninterrupted
        (dom:apply-changes (script-context-drawing context)
                           (dom:invert-changes changes))))
    (context-reset context)))

(defun emit-change (before
                    after)
  "Make one change to the drawing: the entity BEFORE becomes AFTER, a creation when there is no BEFORE and
an erasure when there is no AFTER. It is done whole or not at all, even when the command times out.

BEFORE: the entity as it is, or NIL.
AFTER: the entity as it becomes, or NIL."
  (let* ((context (current-context))
         (id (dom:entity-id (or before after))))
    (when (>= (script-context-change-count context) *maximum-changes*)
      (error "A command may make at most ~d changes." *maximum-changes*))
    (sbcl:with-uninterrupted
      (dom:apply-changes (script-context-drawing context)
                         (list (dom:make-change id
                                                before
                                                after)))
      (let ((entry (gethash id (script-context-net context))))
        (if entry
            (setf (cdr entry) after)
            (setf (gethash id (script-context-net context)) (cons before after))))
      (incf (script-context-change-count context)))
    id))
