(in-package #:borba-cad/app)

;;;; The tools that change what exists by what is around it, and need no selection: trim and extend, which are
;;;; given the part of a line, an arc or a circle to work on by a click, offset, which copies an entity at a
;;;; distance, fillet, which rounds the corner of two lines, and spread, which takes a shape apart and moves its
;;;; pieces a little from each other. Each commits one change set, so each is one step of undo, and says in a
;;;; sentence why it could not do what was asked.

(defun reshape-pick (app
                     cursor
                     &optional kinds)
  "Return the entity under the pointer, or NIL.

APP: the application.
CURSOR: the pointer position.
KINDS: a list of entity kinds, when only those may be picked."
  (dom:pick-entity (app-drawing app)
                   (cursor-raw-x cursor)
                   (cursor-raw-y cursor)
                   (pick-radius-in-world app)
                   kinds))

(defun commit-reshaping (app
                         changes-thunk
                         label)
  "Commit the change set that CHANGES-THUNK works out, as one step of undo. What cannot be done is said in the
status bar and nothing changes.

APP: the application.
CHANGES-THUNK: function of no arguments that returns a change set or signals an error.
LABEL: the name of the operation, for the history and the log."
  (handler-case (commit-changes app
                                (funcall changes-thunk)
                                label)
    (base:borba-cad-error (condition)
      (report-error app
                    condition
                    "reshape")
      nil)))

;;; Trim and extend

(defclass trim-tool (tool) ()
  (:documentation "Cuts away the part of a line, an arc or a circle that is clicked, as far as the nearest crossings."))

(defmethod tool-kind ((tool trim-tool)) :trim)

(defmethod tool-prompt ((tool trim-tool) app)
  (declare (ignore app))
  (i18n:tr "Trim: click the part of a line, an arc or a circle to cut away"))

(defmethod tool-pointer-down ((tool trim-tool) app cursor)
  (let ((entity (reshape-pick app cursor)))
    (if entity
        (commit-reshaping app
                          (lambda ()
                            (dom:trim-changes (app-drawing app)
                                              (dom:entity-id entity)
                                              (cursor-raw-x cursor)
                                              (cursor-raw-y cursor)))
                          "entity.trim")
        (set-message app
                     (i18n:tr "Click a line, an arc or a circle.")
                     :error))
    (app-request-redraw app)))

(defclass extend-tool (tool) ()
  (:documentation "Lengthens the end of a line or an arc, the one nearer to the click, up to the next entity."))

(defmethod tool-kind ((tool extend-tool)) :extend)

(defmethod tool-prompt ((tool extend-tool) app)
  (declare (ignore app))
  (i18n:tr "Extend: click near the end of a line or an arc to lengthen it up to the next entity"))

(defmethod tool-pointer-down ((tool extend-tool) app cursor)
  (let ((entity (reshape-pick app cursor)))
    (if entity
        (commit-reshaping app
                          (lambda ()
                            (dom:extend-changes (app-drawing app)
                                                (dom:entity-id entity)
                                                (cursor-raw-x cursor)
                                                (cursor-raw-y cursor)))
                          "entity.extend")
        (set-message app
                     (i18n:tr "Click a line or an arc.")
                     :error))
    (app-request-redraw app)))

;;; Offset

(defclass offset-tool (tool)
  ((target :initform nil :accessor offset-target))
  (:documentation "Copies an entity at a distance: a click on it, then a click on the side and at the distance of
the copy, or the distance typed."))

(defmethod tool-kind ((tool offset-tool)) :offset)

(defmethod tool-prompt ((tool offset-tool) app)
  (declare (ignore app))
  (if (offset-target tool)
      (i18n:tr "Offset: click on the side and at the distance of the copy, or type the distance (Esc cancels)")
      (i18n:tr "Offset: click the entity to copy")))

(defun offset-through-pointer (tool
                               cursor)
  "Return the signed distance that makes the copy of the entity of an offset tool pass through the pointer.

TOOL: the offset tool with its target set.
CURSOR: the pointer position."
  (dom:entity-offset-through (offset-target tool)
                             (cursor-x cursor)
                             (cursor-y cursor)))

(defun commit-offset (tool
                      app
                      distance)
  "Add the copy of the entity of an offset tool at a signed distance, and ready the tool for another entity.

TOOL: the offset tool with its target set.
APP: the application.
DISTANCE: millimetres, as for ENTITY-OFFSET."
  (when (commit-reshaping app
                          (lambda ()
                            (dom:offset-changes (app-drawing app)
                                                (dom:entity-id (offset-target tool))
                                                distance))
                          "entity.offset")
    (setf (offset-target tool) nil)))

(defun say-cannot-offset (app)
  "Tell the user that the entity they clicked is of a kind that cannot be offset.

APP: the application."
  (set-message app
               (i18n:tr "That cannot be offset: use a line, circle, arc, rectangle, polygon or polyline.")
               :error))

(defmethod tool-pointer-down ((tool offset-tool) app cursor)
  (cond ((null (offset-target tool))
         (let ((entity (reshape-pick app cursor)))
           (cond ((null entity)
                  (set-message app
                               (i18n:tr "Click the entity to copy.")
                               :error))
                 ((null (dom:entity-offset-through entity
                                                   (cursor-x cursor)
                                                   (cursor-y cursor)))
                  (say-cannot-offset app))
                 (t (setf (offset-target tool) entity)))))
        (t
         (let ((distance (offset-through-pointer tool cursor)))
           (if (base:positive-length-p (abs distance))
               (commit-offset tool
                              app
                              distance)
               (set-message app
                            (i18n:tr "Click away from the entity, on the side where the copy should be.")
                            :error)))))
  (app-request-redraw app))

(defmethod tool-pointer-move ((tool offset-tool) app cursor)
  (declare (ignore cursor))
  (when (offset-target tool)
    (app-request-redraw app)))

(defmethod tool-typed-number ((tool offset-tool) app number)
  (let ((target (offset-target tool))
        (cursor (app-cursor app)))
    (when target
      (if (and cursor (plusp number))
          (commit-offset tool
                         app
                         (* (if (minusp (offset-through-pointer tool cursor)) -1 1)
                            (base:unit->millimeters (float number 1d0) (display-unit-of app))))
          (set-message app
                       (i18n:tr "The distance must be above zero.")
                       :error))
      t)))

(defmethod tool-preview-entities ((tool offset-tool) app)
  (let ((cursor (app-cursor app))
        (target (offset-target tool)))
    (when (and target cursor)
      (let* ((distance (offset-through-pointer tool cursor))
             (entity (and (base:positive-length-p (abs distance))
                          (try-entity (lambda ()
                                        (dom:entity-offset target distance))))))
        (and entity (list entity))))))

(defmethod tool-cancel ((tool offset-tool) app)
  (setf (offset-target tool) nil)
  (app-request-redraw app))

(defmethod tool-in-progress-p ((tool offset-tool))
  (and (offset-target tool) t))

;;; Fillet

(defclass fillet-tool (tool)
  ((first-line :initform nil :accessor fillet-first-line)
   (first-x :initform 0d0 :accessor fillet-first-x)
   (first-y :initform 0d0 :accessor fillet-first-y))
  (:documentation "Rounds the corner of two lines with an arc of the radius the application remembers: a click on
each line, on the part of it that stays. A number typed sets the radius."))

(defmethod tool-kind ((tool fillet-tool)) :fillet)

(defmethod tool-prompt ((tool fillet-tool) app)
  (if (fillet-first-line tool)
      (i18n:trf "Fillet, radius ~a: click the second line (Esc cancels)"
                (length-text app (app-fillet-radius app)))
      (i18n:trf "Fillet, radius ~a: click the first line, or type another radius"
                (length-text app (app-fillet-radius app)))))

(defmethod tool-pointer-down ((tool fillet-tool) app cursor)
  (let ((line (reshape-pick app
                            cursor
                            '(:line))))
    (cond ((null line)
           (set-message app
                        (i18n:tr "Click a line.")
                        :error))
          ((null (fillet-first-line tool))
           (setf (fillet-first-line tool) line
                 (fillet-first-x tool) (cursor-raw-x cursor)
                 (fillet-first-y tool) (cursor-raw-y cursor)))
          (t
           (when (commit-reshaping app
                                   (lambda ()
                                     (dom:fillet-changes (app-drawing app)
                                                         (dom:entity-id (fillet-first-line tool))
                                                         (fillet-first-x tool)
                                                         (fillet-first-y tool)
                                                         (dom:entity-id line)
                                                         (cursor-raw-x cursor)
                                                         (cursor-raw-y cursor)
                                                         (app-fillet-radius app)))
                                   "entity.fillet")
             (setf (fillet-first-line tool) nil)))))
  (app-request-redraw app))

(defmethod tool-typed-number ((tool fillet-tool) app number)
  (if (>= number 0)
      (progn (setf (app-fillet-radius app) (base:unit->millimeters (float number 1d0) (display-unit-of app)))
             (set-message app
                          (i18n:trf "Fillet radius: ~a"
                                    (length-text app (app-fillet-radius app)))))
      (set-message app
                   (i18n:tr "The radius cannot be negative.")
                   :error))
  t)

(defmethod tool-cancel ((tool fillet-tool) app)
  (setf (fillet-first-line tool) nil)
  (app-request-redraw app))

(defmethod tool-in-progress-p ((tool fillet-tool))
  (and (fillet-first-line tool) t))

;;; Spread

(defclass spread-tool (tool) ()
  (:documentation "Takes a shape apart and moves each piece the distance the application remembers from the others:
a click inside a rectangle, a polygon, a closed polyline, a circle or a block. A number typed sets the distance."))

(defmethod tool-kind ((tool spread-tool)) :spread)

(defmethod tool-prompt ((tool spread-tool) app)
  (i18n:trf "Spread, distance ~a: click inside the shape to take apart, or type another distance"
            (length-text app (app-spread-distance app))))

(defun spread-target-under (app
                            cursor)
  "Return the entity that a click at the pointer would spread, or NIL.

APP: the application.
CURSOR: the pointer position."
  (dom:spread-target (app-drawing app)
                     (cursor-raw-x cursor)
                     (cursor-raw-y cursor)
                     (pick-radius-in-world app)))

(defmethod tool-pointer-down ((tool spread-tool) app cursor)
  (let ((target (or (spread-target-under app cursor)
                    (reshape-pick app cursor))))
    (if target
        (commit-reshaping app
                          (lambda ()
                            (dom:spread-changes (app-drawing app)
                                                (dom:entity-id target)
                                                (app-spread-distance app)))
                          "entity.spread")
        (set-message app
                     (i18n:tr "Click inside a rectangle, polygon, polyline, circle or block.")
                     :error))
    (app-request-redraw app)))

(defmethod tool-pointer-move ((tool spread-tool) app cursor)
  (declare (ignore cursor))
  (app-request-redraw app))

(defmethod tool-typed-number ((tool spread-tool) app number)
  (if (plusp number)
      (progn (setf (app-spread-distance app) (base:unit->millimeters (float number 1d0) (display-unit-of app)))
             (set-message app
                          (i18n:trf "Spread distance: ~a"
                                    (length-text app (app-spread-distance app)))))
      (set-message app
                   (i18n:tr "The distance must be above zero.")
                   :error))
  t)

(defmethod tool-preview-entities ((tool spread-tool) app)
  (let* ((cursor (app-cursor app))
         (target (and cursor (spread-target-under app cursor))))
    (when target
      (handler-case (loop for change in (dom:spread-changes (app-drawing app)
                                                            (dom:entity-id target)
                                                            (app-spread-distance app))
                          when (dom:change-after change)
                            collect (dom:change-after change))
        (base:borba-cad-error ()
          nil)))))
