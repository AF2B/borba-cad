(in-package #:borba-cad/app)

;;;; Grips: the handles on the one entity that is selected. Dragging a grip changes the entity: an end
;;;; of a line, a corner of a rectangle, the radius of a circle, a vertex of a polyline. While the drag
;;;; goes on the entity as it would become is drawn as a preview; releasing commits one change, and
;;;; Escape abandons it.

(defconstant +grip-pick-pixels+ 9d0
  "How close, in pixels, the pointer must be to a grip to take it.")

(defstruct (grip-drag (:constructor make-grip-drag (entity
                                                    grip)))
  "A grip being dragged.

ENTITY: the entity as it was when the drag began.
GRIP: the grip taken.
PREVIEW: the entity as the drag would leave it so far, or NIL when that is not valid."
  entity
  grip
  (preview nil))

(defun selected-grip-entity (app)
  "Return the entity that has grips: the selected one, when exactly one is selected, the select tool
is the active one, and no grip is being dragged; otherwise NIL.

APP: the application."
  (and (eq :select (tool-kind (app-tool app)))
       (null (app-grip-drag app))
       (let ((ids (selection-ids app)))
         (and ids
              (null (rest ids))
              (dom:store-find (dom:drawing-store (app-drawing app)) (first ids))))))

(defun selection-grips (app)
  "Return the grips to show: those of the selected entity, or none.

APP: the application."
  (let ((entity (selected-grip-entity app)))
    (and entity (dom:entity-grips entity))))

(defun pointer-distance-to-grip (app
                                 grip)
  "Return how far, in pixels, the pointer is from GRIP.

APP: the application.
GRIP: a grip."
  (multiple-value-bind (x y)
      (view:world->screen (app-viewport app)
                          (dom:grip-x grip)
                          (dom:grip-y grip))
    (geo:distance x
                  y
                  (app-pointer-x app)
                  (app-pointer-y app))))

(defun grip-under-pointer (app)
  "Return the grip of the selected entity that the pointer is on, the nearest when several are, or
NIL.

APP: the application."
  (let ((best nil)
        (best-distance +grip-pick-pixels+))
    (dolist (grip (selection-grips app))
      (let ((distance (pointer-distance-to-grip app grip)))
        (when (<= distance best-distance)
          (setf best grip
                best-distance distance))))
    best))

(defun start-grip-drag (app
                        grip)
  "Begin dragging GRIP of the selected entity.

APP: the application.
GRIP: a grip of the selected entity."
  (setf (app-grip-drag app) (make-grip-drag (selected-grip-entity app) grip)))

(defun update-grip-drag (app
                         x
                         y)
  "Compute what the dragged entity becomes with the grip at (X, Y), for the preview.

APP: the application.
X: world x.
Y: world y."
  (let ((drag (app-grip-drag app)))
    (when drag
      (setf (grip-drag-preview drag)
            (handler-case (dom:entity-with-grip (grip-drag-entity drag)
                                                (grip-drag-grip drag)
                                                x
                                                y)
              (base:invalid-entity ()
                nil)))
      (app-request-redraw app))))

(defun finish-grip-drag (app
                         x
                         y)
  "End the drag of a grip at (X, Y), committing the change as one step of undo.

APP: the application.
X: world x the grip was released at.
Y: world y."
  (let ((drag (app-grip-drag app)))
    (setf (app-grip-drag app) nil)
    (when drag
      (handler-case
          (commit-changes app
                          (dom:grip-changes (app-drawing app)
                                            (dom:entity-id (grip-drag-entity drag))
                                            (grip-drag-grip drag)
                                            x
                                            y)
                          "entity.edit"
                          :context "grip")
        (base:borba-cad-error (condition)
          (report-error app
                        condition
                        "grip")))
      (app-request-redraw app))))

(defun cancel-grip-drag (app)
  "Abandon the drag of a grip, if there is one.

APP: the application."
  (when (app-grip-drag app)
    (setf (app-grip-drag app) nil)
    (app-request-redraw app)))
