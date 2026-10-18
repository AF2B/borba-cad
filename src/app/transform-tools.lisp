(in-package #:borba-cad/app)

;;;; The tools that change what is selected as a whole: rotate, mirror and scale. Each takes its points by clicks
;;;; (or typed, see constraints.lisp), shows the result as a preview while the pointer moves, and commits it as
;;;; one step of undo. Holding Control at the last click of rotate and scale keeps the originals and adds the
;;;; changed copies; mirror asks.

(defparameter *selection-tool-kinds* '(:rotate :mirror :scale)
  "The tools that change what is selected as a whole, and so need a selection.")

(defparameter *modify-tool-kinds* '(:rotate :mirror :scale :trim :extend :offset :fillet :make-block)
  "The tools that change what is drawn, which the Modify button opens, as opposed to those that draw: the ones that
need a selection, and the ones that work on what is clicked.")

(defun selection-tool-kind-p (kind)
  "Return true when KIND is a tool that changes the selection as a whole.

KIND: a tool kind keyword."
  (and (member kind *selection-tool-kinds*) t))

(defun modify-tool-kind-p (kind)
  "Return true when KIND is a tool that changes the selection.

KIND: a tool kind keyword."
  (and (member kind *modify-tool-kinds*) t))

(defun selected-ids-to-transform (app)
  "Return the ids of the selected entities, in order.

APP: the application."
  (selection-ids app))

(defun transformed-preview (app
                            transform)
  "Return what the selection would become under TRANSFORM, as entities for the preview. An entity that cannot be
transformed is left out.

APP: the application.
TRANSFORM: function of an entity that returns the transformed entity."
  (loop for entity in (selected-entities app)
        for result = (and (dom:editable-entity-p (app-drawing app) entity)
                          (try-entity (lambda () (funcall transform entity))))
        when result
          collect result))

(defun copy-held-p (cursor)
  "Return true when Control is held at the click that gave CURSOR.

CURSOR: a cursor."
  (and (modifier-p (cursor-modifiers cursor) :control) t))

(defun transformation-message (app
                               verb
                               count
                               skipped)
  "Tell what a transformation did.

APP: the application.
VERB: :rotated, :mirrored or :scaled.
COUNT: how many entities.
SKIPPED: how many were left because their layers are locked."
  (set-message app
               (ecase verb
                 (:rotated (i18n:trf "Rotated ~d ~:*~[entities~;entity~:;entities~].~a"
                                     count
                                     (skipped-note skipped)))
                 (:mirrored (i18n:trf "Mirrored ~d ~:*~[entities~;entity~:;entities~].~a"
                                      count
                                      (skipped-note skipped)))
                 (:scaled (i18n:trf "Scaled ~d ~:*~[entities~;entity~:;entities~].~a"
                                    count
                                    (skipped-note skipped))))))

(defun commit-transformation (app
                              verb
                              transform
                              copy)
  "Change the selection with TRANSFORM, as one step of undo, and go back to the select tool. With COPY the
originals stay and the changed copies are added and become the selection. A result that would not be valid is
said and nothing changes.

APP: the application.
VERB: :rotated, :mirrored or :scaled.
TRANSFORM: function of an entity that returns the transformed entity.
COPY: true to add copies instead of changing the originals."
  (handler-case
      (multiple-value-bind (changes skipped)
          (dom:transform-entities-changes (app-drawing app)
                                          (selected-ids-to-transform app)
                                          transform
                                          copy)
        (cond ((null changes)
               (set-message app
                            (i18n:tr "The selected entities are on locked layers.")
                            :error))
              ((commit-changes app
                               changes
                               (format nil "entity.~(~a~)" verb))
               (when copy
                 (select-ids app (mapcar #'dom:change-entity-id changes)))
               (transformation-message app
                                       verb
                                       (length changes)
                                       skipped)
               (operation-set-tool app :select))))
    (base:invalid-entity (condition)
      (report-error app
                    condition
                    "transform"))))

;;; Rotate

(defclass rotate-tool (tool)
  ((pivot :initform nil :accessor rotate-pivot))
  (:documentation "Turns the selection about a point: a click for the center, then a click in the direction of the
new angle, or the angle typed in degrees."))

(defmethod tool-kind ((tool rotate-tool)) :rotate)

(defmethod tool-prompt ((tool rotate-tool) app)
  (declare (ignore app))
  (if (rotate-pivot tool)
      (i18n:tr "Rotate: click in the direction of the new angle, or type it in degrees (Ctrl keeps the originals)")
      (i18n:tr "Rotate: click the center of the turn")))

(defmethod tool-anchor ((tool rotate-tool))
  (rotate-pivot tool))

(defun angle-from-pivot (pivot
                         x
                         y)
  "Return the direction from the pivot to a point, in radians, or NIL when they are at the same place.

PIVOT: an (x . y) cons.
X: world x of the point.
Y: world y of the point."
  (and (base:positive-length-p (geo:distance (car pivot)
                                             (cdr pivot)
                                             x
                                             y))
       (atan (- y (cdr pivot)) (- x (car pivot)))))

(defmethod tool-pointer-down ((tool rotate-tool) app cursor)
  (let ((pivot (rotate-pivot tool)))
    (if (null pivot)
        (setf (rotate-pivot tool) (point-key (cursor-x cursor) (cursor-y cursor)))
        (let ((angle (angle-from-pivot pivot
                                       (cursor-x cursor)
                                       (cursor-y cursor))))
          (if angle
              (commit-transformation app
                                     :rotated
                                     (lambda (entity)
                                       (dom:entity-rotated entity
                                                           (car pivot)
                                                           (cdr pivot)
                                                           angle))
                                     (copy-held-p cursor))
              (set-message app
                           (i18n:tr "The two points must be apart.")
                           :error))))
    (app-request-redraw app)))

(defmethod tool-pointer-move ((tool rotate-tool) app cursor)
  (declare (ignore cursor))
  (when (rotate-pivot tool)
    (app-request-redraw app)))

(defmethod tool-preview-entities ((tool rotate-tool) app)
  (let ((pivot (rotate-pivot tool))
        (cursor (app-cursor app)))
    (when (and pivot cursor)
      (let ((angle (angle-from-pivot pivot
                                     (cursor-x cursor)
                                     (cursor-y cursor))))
        (when angle
          (transformed-preview app
                               (lambda (entity)
                                 (dom:entity-rotated entity
                                                     (car pivot)
                                                     (cdr pivot)
                                                     angle))))))))

(defmethod tool-cancel ((tool rotate-tool) app)
  (setf (rotate-pivot tool) nil)
  (app-request-redraw app))

(defmethod tool-in-progress-p ((tool rotate-tool))
  (and (rotate-pivot tool) t))

(defmethod tool-typed-number ((tool rotate-tool) app number)
  (let ((pivot (rotate-pivot tool)))
    (when pivot
      (commit-transformation app
                             :rotated
                             (lambda (entity)
                               (dom:entity-rotated entity
                                                   (car pivot)
                                                   (cdr pivot)
                                                   (base:degrees->radians number)))
                             nil)
      t)))

;;; Mirror

(defclass mirror-tool (tool)
  ((first-point :initform nil :accessor mirror-first-point))
  (:documentation "Reflects the selection in a line given by two points, and asks whether to keep the originals."))

(defmethod tool-kind ((tool mirror-tool)) :mirror)

(defmethod tool-prompt ((tool mirror-tool) app)
  (declare (ignore app))
  (if (mirror-first-point tool)
      (i18n:tr "Mirror: click the second point of the line (Esc cancels)")
      (i18n:tr "Mirror: click the first point of the line")))

(defmethod tool-anchor ((tool mirror-tool))
  (mirror-first-point tool))

(defun mirror-with (app
                    first
                    x
                    y
                    copy)
  "Reflect the selection in the line from FIRST to (X, Y).

APP: the application.
FIRST: an (x . y) cons, the first point of the line.
X: world x of the second point.
Y: world y of the second point.
COPY: true to keep the originals."
  (commit-transformation app
                         :mirrored
                         (lambda (entity)
                           (dom:entity-mirrored entity
                                                (car first)
                                                (cdr first)
                                                x
                                                y))
                         copy))

(defmethod tool-pointer-down ((tool mirror-tool) app cursor)
  (let ((first (mirror-first-point tool))
        (x (cursor-x cursor))
        (y (cursor-y cursor)))
    (cond ((null first)
           (setf (mirror-first-point tool) (point-key x y)))
          ((not (base:positive-length-p (geo:distance (car first)
                                                      (cdr first)
                                                      x
                                                      y)))
           (set-message app
                        (i18n:tr "The two points must be apart.")
                        :error))
          (t
           (open-choice-prompt app
                               (i18n:tr "Mirror")
                               (i18n:tr "Keep the original entities?")
                               (list (cons (i18n:tr "Keep")
                                           (lambda () (mirror-with app
                                                                   first
                                                                   x
                                                                   y
                                                                   t)))
                                     (cons (i18n:tr "Delete")
                                           (lambda () (mirror-with app
                                                                   first
                                                                   x
                                                                   y
                                                                   nil)))
                                     (cons (i18n:tr "Cancel") nil)))))
    (app-request-redraw app)))

(defmethod tool-pointer-move ((tool mirror-tool) app cursor)
  (declare (ignore cursor))
  (when (mirror-first-point tool)
    (app-request-redraw app)))

(defmethod tool-preview-entities ((tool mirror-tool) app)
  (let ((first (mirror-first-point tool))
        (cursor (app-cursor app)))
    (when (and first
               cursor
               (base:positive-length-p (geo:distance (car first)
                                                     (cdr first)
                                                     (cursor-x cursor)
                                                     (cursor-y cursor))))
      (transformed-preview app
                           (lambda (entity)
                             (dom:entity-mirrored entity
                                                  (car first)
                                                  (cdr first)
                                                  (cursor-x cursor)
                                                  (cursor-y cursor)))))))

(defmethod tool-cancel ((tool mirror-tool) app)
  (setf (mirror-first-point tool) nil)
  (app-request-redraw app))

(defmethod tool-in-progress-p ((tool mirror-tool))
  (and (mirror-first-point tool) t))

;;; Scale

(defclass scale-tool (tool)
  ((pivot :initform nil :accessor scale-pivot)
   (reference :initform nil :accessor scale-reference))
  (:documentation "Scales the selection about a point: a click for the base point, a click that says how long
something is now, and a click that says how long it should be; or the factor typed."))

(defmethod tool-kind ((tool scale-tool)) :scale)

(defmethod tool-prompt ((tool scale-tool) app)
  (declare (ignore app))
  (cond ((null (scale-pivot tool))
         (i18n:tr "Scale: click the base point"))
        ((null (scale-reference tool))
         (i18n:tr "Scale: click a point to say how long it is now, or type the factor"))
        (t
         (i18n:tr "Scale: click where that point should be, or type the factor (Ctrl keeps the originals)"))))

(defmethod tool-anchor ((tool scale-tool))
  (scale-pivot tool))

(defun scale-factor-at (tool
                        x
                        y)
  "Return the factor the selection would be scaled by if the target point were (X, Y), or NIL when it is not
yet known or is not a scale.

TOOL: the scale tool.
X: world x of the target point.
Y: world y of the target point."
  (let ((pivot (scale-pivot tool))
        (reference (scale-reference tool)))
    (when (and pivot reference)
      (let ((factor (/ (geo:distance (car pivot)
                                     (cdr pivot)
                                     x
                                     y) reference)))
        (and (base:positive-length-p factor) factor)))))

(defun scale-with (app
                   pivot
                   factor
                   copy)
  "Scale the selection about PIVOT by FACTOR.

APP: the application.
PIVOT: an (x . y) cons.
FACTOR: a positive number.
COPY: true to keep the originals."
  (commit-transformation app
                         :scaled
                         (lambda (entity)
                           (dom:entity-scaled entity
                                              (car pivot)
                                              (cdr pivot)
                                              factor))
                         copy))

(defmethod tool-pointer-down ((tool scale-tool) app cursor)
  (let ((pivot (scale-pivot tool))
        (x (cursor-x cursor))
        (y (cursor-y cursor)))
    (cond ((null pivot)
           (setf (scale-pivot tool) (point-key x y)))
          ((null (scale-reference tool))
           (let ((distance (geo:distance (car pivot)
                                         (cdr pivot)
                                         x
                                         y)))
             (if (base:positive-length-p distance)
                 (setf (scale-reference tool) distance)
                 (set-message app
                              (i18n:tr "The two points must be apart.")
                              :error))))
          (t
           (let ((factor (scale-factor-at tool
                                          x
                                          y)))
             (if factor
                 (scale-with app
                             pivot
                             factor
                             (copy-held-p cursor))
                 (set-message app
                              (i18n:tr "The two points must be apart.")
                              :error)))))
    (app-request-redraw app)))

(defmethod tool-pointer-move ((tool scale-tool) app cursor)
  (declare (ignore cursor))
  (when (scale-reference tool)
    (app-request-redraw app)))

(defmethod tool-preview-entities ((tool scale-tool) app)
  (let* ((cursor (app-cursor app))
         (pivot (scale-pivot tool))
         (factor (and cursor (scale-factor-at tool
                                              (cursor-x cursor)
                                              (cursor-y cursor)))))
    (when factor
      (transformed-preview app
                           (lambda (entity)
                             (dom:entity-scaled entity
                                                (car pivot)
                                                (cdr pivot)
                                                factor))))))

(defmethod tool-cancel ((tool scale-tool) app)
  (setf (scale-pivot tool) nil
        (scale-reference tool) nil)
  (app-request-redraw app))

(defmethod tool-in-progress-p ((tool scale-tool))
  (and (scale-pivot tool) t))

(defmethod tool-typed-number ((tool scale-tool) app number)
  (let ((pivot (scale-pivot tool)))
    (when pivot
      (if (base:positive-length-p (float number 1d0))
          (scale-with app
                      pivot
                      (float number 1d0)
                      nil)
          (set-message app
                       (i18n:tr "The factor must be above zero.")
                       :error))
      t)))

;;; The Modify menu

(defparameter *modify-menu-tools*
  (list (list :rotate :rotate (i18n:mark "Rotate") "Shift+R")
        (list :mirror :mirror (i18n:mark "Mirror") "Shift+M")
        (list :scale :scale (i18n:mark "Scale") "Shift+S")
        nil
        (list :trim :trim (i18n:mark "Trim") "Shift+T")
        (list :extend :extend (i18n:mark "Extend") "Shift+E")
        (list :offset :offset (i18n:mark "Offset") "Shift+O")
        (list :fillet :fillet (i18n:mark "Fillet") "Shift+F")
        nil
        (list :make-block :make-block (i18n:mark "Make block...") "Shift+B")
        (list :explode :explode (i18n:mark "Explode") "Shift+X"))
  "The choices of the modify menu, top to bottom, as (tool-kind icon label shortcut), with NIL between groups. The
labels are in English and are translated where the menu is built.")

(defparameter *modify-shortcuts*
  '((:r . :rotate) (:m . :mirror) (:s . :scale) (:t . :trim) (:e . :extend) (:o . :offset) (:f . :fillet)
    (:b . :make-block) (:x . :explode))
  "Letter keys that activate the modify tools when Shift is held.")
