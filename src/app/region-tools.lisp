(in-package #:borba-cad/app)

;;;; Joining, cutting and overlapping closed shapes in the application. Union and intersect work on what is selected;
;;;; subtract is a tool, because it needs to know which shape is cut and which are cut away: a click on the first,
;;;; then a click on each of the others, and Enter. Each is one step of undo, selects the polylines it makes, and says
;;;; in a sentence why it could not do what was asked.

(defun result-polyline-ids (changes)
  "Return the ids of the entities that a change set adds.

CHANGES: a change set."
  (loop for change in changes
        when (null (dom:change-before change))
          collect (dom:entity-id (dom:change-after change))))

(defun operation-region (app
                         operation)
  "Join the selected closed shapes into one outline, or keep only where they all overlap, and select the polylines that
result.

APP: the application.
OPERATION: :union or :intersection."
  (handler-case
      (multiple-value-bind (changes used) (dom:region-changes (app-drawing app)
                                                              (selection-ids app)
                                                              operation)
        (when (commit-changes app
                              changes
                              "entity.region")
          (select-ids app (result-polyline-ids changes))
          (set-message app
                       (ecase operation
                         (:union (i18n:trf "Joined ~d shapes." used))
                         (:intersection (i18n:trf "Kept where ~d shapes overlap." used))))))
    (base:borba-cad-error (condition)
      (report-error app
                    condition
                    "region"))))

;;; Subtract

(defclass subtract-tool (tool)
  ((base :initform nil :accessor subtract-base)
   (cutters :initform '() :accessor subtract-cutters))
  (:documentation "Cuts closed shapes away from another: a click on the shape to cut, then a click on each shape to
cut away from it, and Enter."))

(defmethod tool-kind ((tool subtract-tool)) :subtract)

(defmethod tool-prompt ((tool subtract-tool) app)
  (declare (ignore app))
  (if (subtract-base tool)
      (i18n:trf "Subtract, ~d ~:*~[shapes~;shape~:;shapes~] to cut away: click another, or Enter to cut (Esc cancels)"
                (length (subtract-cutters tool)))
      (i18n:tr "Subtract: click the shape to cut from")))

(defun subtract-changes (tool
                         app
                         &optional extra)
  "Return the change set that cuts the shapes of a subtract tool away from its first shape, with another shape cut away
as well when EXTRA is given, or signal EDIT-REFUSED.

TOOL: the subtract tool, with its first shape.
APP: the application.
EXTRA: another shape to cut away, or NIL."
  (let ((base (subtract-base tool)))
    (values (dom:region-changes (app-drawing app)
                                (mapcar #'dom:entity-id
                                        (append (list base)
                                                (subtract-cutters tool)
                                                (and extra (list extra))))
                                :difference
                                :base-id (dom:entity-id base)))))

(defun subtract-target-under (app
                              cursor)
  "Return the closed shape that a click at the pointer would pick, or NIL.

APP: the application.
CURSOR: the pointer position."
  (dom:region-target (app-drawing app)
                     (cursor-raw-x cursor)
                     (cursor-raw-y cursor)
                     (pick-radius-in-world app)))

(defmethod tool-pointer-down ((tool subtract-tool) app cursor)
  (let ((target (subtract-target-under app cursor)))
    (cond ((null target)
           (set-message app
                        (i18n:tr "Click a rectangle, circle, ellipse, polygon or closed polyline.")
                        :error))
          ((null (subtract-base tool))
           (setf (subtract-base tool) target)
           (select-ids app (list (dom:entity-id target))))
          ((or (eql (dom:entity-id target) (dom:entity-id (subtract-base tool)))
               (member (dom:entity-id target) (subtract-cutters tool) :key #'dom:entity-id))
           (set-message app
                        (i18n:tr "That shape is already part of the cut.")
                        :error))
          (t
           (push target (subtract-cutters tool)))))
  (app-request-redraw app))

(defun finish-subtract (tool
                        app)
  "Cut the shapes away from the first one and go back to the select tool.

TOOL: the subtract tool.
APP: the application."
  (handler-case
      (let ((changes (subtract-changes tool app)))
        (when (commit-changes app
                              changes
                              "entity.region")
          (select-ids app (result-polyline-ids changes))
          (operation-set-tool app :select)
          (set-message app
                       (i18n:tr "Cut done."))))
    (base:borba-cad-error (condition)
      (report-error app
                    condition
                    "region"))))

(defmethod tool-key ((tool subtract-tool) app key modifiers)
  (declare (ignore modifiers))
  (when (eq key :enter)
    (if (subtract-cutters tool)
        (finish-subtract tool app)
        (set-message app
                     (i18n:tr "Click the shapes to cut away first.")
                     :error))
    t))

(defmethod tool-preview-entities ((tool subtract-tool) app)
  (when (subtract-base tool)
    (let* ((cursor (app-cursor app))
           (hovered (and cursor (subtract-target-under app cursor)))
           (extra (and hovered
                       (not (eql (dom:entity-id hovered) (dom:entity-id (subtract-base tool))))
                       (not (member (dom:entity-id hovered) (subtract-cutters tool) :key #'dom:entity-id))
                       hovered)))
      (when (or extra (subtract-cutters tool))
        (handler-case (loop for change in (subtract-changes tool
                                                            app
                                                            extra)
                            when (and (null (dom:change-before change)) (dom:change-after change))
                              collect (dom:change-after change))
          (base:borba-cad-error ()
            nil))))))

(defmethod tool-cancel ((tool subtract-tool) app)
  (setf (subtract-base tool) nil
        (subtract-cutters tool) '())
  (app-request-redraw app))

(defmethod tool-in-progress-p ((tool subtract-tool))
  (and (subtract-base tool) t))
