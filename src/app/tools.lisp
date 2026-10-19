(in-package #:borba-cad/app)

;;;; Tools. A tool interprets pointer positions (already snapped by the application) and
;;;; proposes changes; it never touches the drawing except through COMMIT-CHANGES. CLOS is used
;;;; because the seven tools share one protocol and differ only in behavior.

(defclass tool () ()
  (:documentation "Base of every tool."))

(defgeneric tool-kind (tool)
  (:documentation "Return the keyword naming TOOL.

TOOL: a tool."))

(defgeneric tool-prompt (tool app)
  (:documentation "Return the hint shown in the status bar while TOOL is active.

TOOL: a tool.
APP: the application."))

(defgeneric tool-pointer-down (tool app cursor)
  (:documentation "The primary button was pressed over the canvas.

TOOL: a tool.
APP: the application.
CURSOR: the pointer position, raw and snapped, in world coordinates.")
  (:method ((tool tool) app cursor)
    (declare (ignore app cursor))))

(defgeneric tool-pointer-move (tool app cursor)
  (:documentation "The pointer moved over the canvas.

TOOL: a tool.
APP: the application.
CURSOR: the pointer position, raw and snapped, in world coordinates.")
  (:method ((tool tool) app cursor)
    (declare (ignore app cursor))))

(defgeneric tool-pointer-up (tool app cursor)
  (:documentation "The primary button was released.

TOOL: a tool.
APP: the application.
CURSOR: the pointer position, raw and snapped, in world coordinates.")
  (:method ((tool tool) app cursor)
    (declare (ignore app cursor))))

(defgeneric tool-key (tool app key modifiers)
  (:documentation "A key was pressed while the canvas has the focus. Returns true when handled.

TOOL: a tool.
APP: the application.
KEY: key keyword.
MODIFIERS: modifier keys held.")
  (:method ((tool tool) app key modifiers)
    (declare (ignore app key modifiers))
    nil))

(defgeneric tool-cancel (tool app)
  (:documentation "Abandon whatever the tool is doing.

TOOL: a tool.
APP: the application.")
  (:method ((tool tool) app)
    (declare (ignore app))))

(defgeneric tool-in-progress-p (tool)
  (:documentation "Return true when TOOL is in the middle of an operation that Escape cancels.

TOOL: a tool.")
  (:method ((tool tool))
    nil))

(defgeneric tool-typed-number (tool app number)
  (:documentation "Take a plain number that was typed while TOOL waits for a point: an angle in degrees for rotate,
a factor for scale. Return true when the tool used it, and NIL when it has no use for one now, so that the text
is read as a point.

TOOL: a tool.
APP: the application.
NUMBER: the number typed.")
  (:method ((tool tool) app number)
    (declare (ignore app number))
    nil))

(defgeneric tool-anchor (tool)
  (:documentation "Return the last point TOOL has been given, as an (x . y) cons, which ortho and polar hold the
next one to and a typed length is measured from; NIL when it has none.

TOOL: a tool.")
  (:method ((tool tool))
    nil))

(defgeneric tool-preview-entities (tool app)
  (:documentation "Return the entities TOOL would create now, drawn as a preview.

TOOL: a tool.
APP: the application.")
  (:method ((tool tool) app)
    (declare (ignore app))
    nil))

;;; Helpers shared by the creation tools

(defun try-entity (thunk)
  "Call THUNK, which builds an entity for a preview, and return it; an invalid in-progress
shape (for example a line of zero length) is simply not previewed.

THUNK: function of no arguments."
  (handler-case (funcall thunk)
    (base:invalid-entity () nil)))

(defun creation-id (app)
  "Return the id the next created entity receives.

APP: the application."
  (dom:drawing-next-entity-id (app-drawing app)))

(defun creation-layer-id (app)
  "Return the id of the layer that receives new entities.

APP: the application."
  (dom:drawing-current-layer-id (app-drawing app)))

(defun check-creation-layer (app)
  "Signal LAYER-ERROR when new entities cannot be drawn on the current layer.

APP: the application."
  (let ((layer (dom:find-layer (app-drawing app) (creation-layer-id app))))
    (when (dom:layer-locked layer)
      (error 'base:layer-error
             :reason (i18n:trf "layer ~s is locked; unlock it or choose another layer" (dom:layer-name layer))))
    (unless (dom:layer-visible layer)
      (error 'base:layer-error
             :reason (i18n:trf "layer ~s is hidden; show it or choose another layer" (dom:layer-name layer))))))

(defun create-entity (app
                      thunk)
  "Build an entity with THUNK and add it to the drawing. Validation errors and layer errors are
shown to the user. Returns true when the entity was created.

APP: the application.
THUNK: function of no arguments returning an entity."
  (handler-case
      (progn (check-creation-layer app)
             (commit-changes app
                             (dom:create-entity-changes (funcall thunk))
                             "entity.create"
                             :context "creation"))
    (base:borba-cad-error (condition)
      (report-error app
                    condition
                    "creation")
      nil)))

(defun point-key (x
                  y)
  "Return a cons for a world point.

X: world x.
Y: world y."
  (cons x y))

;;; Select

(defclass select-tool (tool)
  ((mode :initform nil :accessor select-mode)
   (start-x :initform 0d0 :accessor select-start-x)
   (start-y :initform 0d0 :accessor select-start-y)
   (press-pointer-x :initform 0d0 :accessor select-press-pointer-x)
   (press-pointer-y :initform 0d0 :accessor select-press-pointer-y)
   (moved :initform nil :accessor select-moved)
   (hit-id :initform nil :accessor select-hit-id)
   (additive :initform nil :accessor select-additive))
  (:documentation "Selects by click, by window (left to right: what lies inside) or by crossing (right to
left: what it touches), and moves the selection by dragging."))

(defmethod tool-kind ((tool select-tool))
  :select)

(defmethod tool-prompt ((tool select-tool) app)
  (if (selection-grips app)
      (i18n:tr "Drag a handle to change the shape, or the shape itself to move it; Esc deselects.")
      (i18n:tr
       "Select: click or drag; left to right takes what is inside, right to left what it touches (Shift adds).")))

(defun pick-radius-in-world (app)
  "Return the pick radius in millimetres at the current zoom.

APP: the application."
  (view:pixels->world-length (app-viewport app) +pick-radius-pixels+))

(defmethod tool-pointer-down ((tool select-tool) app cursor)
  (setf (select-moved tool) nil
        (select-press-pointer-x tool) (app-pointer-x app)
        (select-press-pointer-y tool) (app-pointer-y app)
        (select-additive tool) (modifier-p (cursor-modifiers cursor) :shift))
  (let ((grip (grip-under-pointer app)))
    (if grip
        (progn (start-grip-drag app grip)
               (setf (select-mode tool) :grip))
        (select-by-pointer-down tool
                                app
                                cursor))))

(defun select-by-pointer-down (tool
                               app
                               cursor)
  "Handle a press of the select tool that is not on a grip: it picks, or begins a window.

TOOL: the select tool.
APP: the application.
CURSOR: the pointer position."
  (let ((entity (obs:with-operation ((app-telemetry app) "geometry.pick"
                                     :fields (list :entity-count (dom:drawing-entity-count (app-drawing app)))
                                     :level :debug)
                  (dom:pick-entity (app-drawing app)
                                   (cursor-raw-x cursor)
                                   (cursor-raw-y cursor)
                                   (pick-radius-in-world app)))))
    (cond ((and entity (select-additive tool))
           (toggle-selected app (dom:entity-id entity))
           (setf (select-mode tool) nil))
          (entity
           (unless (gethash (dom:entity-id entity) (app-selection app))
             (select-ids app (list (dom:entity-id entity))))
           (setf (select-mode tool) :move
                 (select-hit-id tool) (dom:entity-id entity)
                 (select-start-x tool) (cursor-x cursor)
                 (select-start-y tool) (cursor-y cursor)))
          (t (setf (select-mode tool) :window
                   (select-hit-id tool) nil
                   (select-start-x tool) (cursor-raw-x cursor)
                   (select-start-y tool) (cursor-raw-y cursor))))))

(defun pointer-travel (tool
                       app)
  "Return how far the pointer moved since the press, in pixels.

TOOL: the select tool.
APP: the application."
  (geo:distance (select-press-pointer-x tool)
                (select-press-pointer-y tool)
                (app-pointer-x app)
                (app-pointer-y app)))

(defmethod tool-pointer-move ((tool select-tool) app cursor)
  (when (and (select-mode tool)
             (not (select-moved tool))
             (>= (pointer-travel tool app) +drag-threshold-pixels+))
    (setf (select-moved tool) t))
  (when (select-moved tool)
    (ecase (select-mode tool)
      (:move (setf (app-translation app)
                   (cons (- (cursor-x cursor) (select-start-x tool))
                         (- (cursor-y cursor) (select-start-y tool)))))
      (:grip (update-grip-drag app
                               (cursor-x cursor)
                               (cursor-y cursor)))
      (:window (setf (app-window app)
                     (geo:bounds-from-corners (select-start-x tool)
                                              (select-start-y tool)
                                              (cursor-raw-x cursor)
                                              (cursor-raw-y cursor))
                     (app-window-crossing app)
                     (< (cursor-raw-x cursor) (select-start-x tool)))))
    (app-request-redraw app)))

(defmethod tool-pointer-up ((tool select-tool) app cursor)
  (case (select-mode tool)
    (:grip
     (if (select-moved tool)
         (finish-grip-drag app
                           (cursor-x cursor)
                           (cursor-y cursor))
         (cancel-grip-drag app)))
    (:move
     (let ((translation (app-translation app)))
       (setf (app-translation app) nil)
       (cond ((and (select-moved tool) translation
                   (or (/= 0d0 (car translation)) (/= 0d0 (cdr translation))))
              (handler-case
                  (multiple-value-bind (changes skipped)
                      (dom:translate-entities-changes (app-drawing app)
                                                      (selection-ids app)
                                                      (car translation)
                                                      (cdr translation))
                    (when (commit-changes app
                                          changes
                                          "entity.edit"
                                          :context "move")
                      (set-message app (i18n:trf "Moved ~d.~a"
                                                 (length changes)
                                                 (skipped-note skipped)))))
                (base:borba-cad-error (condition) (report-error app
                                                                condition
                                                                "move"))))
             ((not (select-moved tool))
              (select-ids app (list (select-hit-id tool)))))))
    (:window
     (cond ((select-moved tool)
            (let ((window (app-window app))
                  (crossing (app-window-crossing app)))
              (setf (app-window app) nil
                    (app-window-crossing app) nil)
              (when window
                (select-ids app
                            (mapcar #'dom:entity-id
                                    (obs:with-operation ((app-telemetry app) "geometry.window-select"
                                                         :fields (list :entity-count
                                                                       (dom:drawing-entity-count
                                                                        (app-drawing app))
                                                                       :crossing
                                                                       crossing)
                                                         :level :debug)
                                      (if crossing
                                          (dom:entities-crossing-window (app-drawing app) window)
                                          (dom:entities-in-window (app-drawing app) window))))
                            :add (select-additive tool)))))
           ((not (select-additive tool))
            (clear-selection app)))))
  (setf (select-mode tool) nil
        (select-moved tool) nil)
  (app-request-redraw app))

(defmethod tool-preview-entities ((tool select-tool) app)
  (let ((drag (app-grip-drag app)))
    (and drag (grip-drag-preview drag) (list (grip-drag-preview drag)))))

(defmethod tool-cancel ((tool select-tool) app)
  (cancel-grip-drag app)
  (setf (select-mode tool) nil
        (select-moved tool) nil
        (app-translation app) nil
        (app-window app) nil
        (app-window-crossing app) nil)
  (app-request-redraw app))

(defmethod tool-in-progress-p ((tool select-tool))
  (and (select-mode tool) (select-moved tool) t))

;;; Two-click shapes: line, rectangle, circle

(defclass two-click-tool (tool)
  ((first-point :initform nil :accessor first-point))
  (:documentation "A tool that creates its entity from two clicks."))

(defgeneric build-two-click-entity (tool app x1 y1 x2 y2)
  (:documentation "Build the entity from the two clicked points.

TOOL: the tool.
APP: the application.
X1: world x of the first click.
Y1: world y of the first click.
X2: world x of the second point.
Y2: world y of the second point."))

(defgeneric two-click-prompts (tool)
  (:documentation "Return the status hints for the first and the second click as two values.

TOOL: the tool."))

(defmethod tool-prompt ((tool two-click-tool) app)
  (declare (ignore app))
  (multiple-value-bind (first-hint second-hint) (two-click-prompts tool)
    (if (first-point tool) second-hint first-hint)))

(defmethod tool-pointer-down ((tool two-click-tool) app cursor)
  (cond ((null (first-point tool))
         (setf (first-point tool) (point-key (cursor-x cursor) (cursor-y cursor)))
         (app-request-redraw app))
        (t (let ((first (first-point tool)))
             (when (create-entity app (lambda ()
                                        (build-two-click-entity tool
                                                                app
                                                                (car first)
                                                                (cdr first)
                                                                (cursor-x cursor)
                                                                (cursor-y cursor))))
               (setf (first-point tool) nil))))))

(defmethod tool-pointer-move ((tool two-click-tool) app cursor)
  (declare (ignore cursor))
  (when (first-point tool)
    (app-request-redraw app)))

(defmethod tool-preview-entities ((tool two-click-tool) app)
  (let ((first (first-point tool))
        (cursor (app-cursor app)))
    (when (and first cursor)
      (let ((entity (try-entity (lambda ()
                                  (build-two-click-entity tool
                                                          app
                                                          (car first)
                                                          (cdr first)
                                                          (cursor-x cursor)
                                                          (cursor-y cursor))))))
        (and entity (list entity))))))

(defmethod tool-cancel ((tool two-click-tool) app)
  (setf (first-point tool) nil)
  (app-request-redraw app))

(defmethod tool-in-progress-p ((tool two-click-tool))
  (and (first-point tool) t))

(defmethod tool-anchor ((tool two-click-tool))
  (first-point tool))

(defclass line-tool (two-click-tool) ()
  (:documentation "Creates a line segment from two clicks."))
(defmethod tool-kind ((tool line-tool)) :line)
(defmethod two-click-prompts ((tool line-tool))
  (values (i18n:tr "Line: click the start point") (i18n:tr "Line: click the end point (Esc cancels)")))
(defmethod build-two-click-entity ((tool line-tool) app x1 y1 x2 y2)
  (dom:make-line :id (creation-id app)
                 :layer-id (creation-layer-id app)
                 :x1 x1
                 :y1 y1
                 :x2 x2
                 :y2 y2))

(defclass rectangle-tool (two-click-tool) ()
  (:documentation "Creates an axis-aligned rectangle from two opposite corners."))
(defmethod tool-kind ((tool rectangle-tool)) :rectangle)
(defmethod two-click-prompts ((tool rectangle-tool))
  (values (i18n:tr "Rectangle: click the first corner") (i18n:tr "Rectangle: click the opposite corner (Esc cancels)")))
(defmethod build-two-click-entity ((tool rectangle-tool) app x1 y1 x2 y2)
  (dom:make-rectangle-from-corners :id (creation-id app)
                                   :layer-id (creation-layer-id app)
                                   :x1 x1
                                   :y1 y1
                                   :x2 x2
                                   :y2 y2))

(defclass circle-tool (two-click-tool) ()
  (:documentation "Creates a circle from its center and a point on its outline."))
(defmethod tool-kind ((tool circle-tool)) :circle)
(defmethod two-click-prompts ((tool circle-tool))
  (values (i18n:tr "Circle: click the center") (i18n:tr "Circle: click a point on the circle (Esc cancels)")))
(defmethod build-two-click-entity ((tool circle-tool) app x1 y1 x2 y2)
  (dom:make-circle :id (creation-id app)
                   :layer-id (creation-layer-id app)
                   :center-x x1
                   :center-y y1
                   :radius (geo:distance x1
                                         y1
                                         x2
                                         y2)))

;;; Arc, ellipse and regular polygon: a fixed number of clicks

(defclass point-sequence-tool (tool)
  ((points :initform nil :accessor sequence-points))
  (:documentation "A tool that creates its entity from a fixed number of clicks. The points clicked so far
are kept, newest first, until the last click, which builds the entity."))

(defgeneric points-needed (tool)
  (:documentation "Return how many clicks the entity of TOOL takes.

TOOL: the tool."))

(defgeneric sequence-prompts (tool app)
  (:documentation "Return the status hints, one for each click, as a list.

TOOL: the tool.
APP: the application."))

(defgeneric build-from-points (tool app points x y)
  (:documentation "Build the entity from the clicks made before the last one and the last point.

TOOL: the tool.
APP: the application.
POINTS: the earlier clicks as (x . y) conses, oldest first.
X: world x of the last point.
Y: world y of the last point."))

(defgeneric point-problem (tool points x y)
  (:documentation "Return a message when the point (X, Y) is not acceptable as the next click, or NIL.

TOOL: the tool.
POINTS: the earlier clicks, oldest first.
X: world x of the point.
Y: world y of the point.")
  (:method ((tool point-sequence-tool) points x y)
    (let ((previous (car (last points))))
      (when (and previous
                 (not (base:positive-length-p (geo:distance (car previous)
                                                            (cdr previous)
                                                            x
                                                            y))))
        (i18n:tr "The two points must be apart.")))))

(defmethod tool-prompt ((tool point-sequence-tool) app)
  (nth (length (sequence-points tool)) (sequence-prompts tool app)))

(defmethod tool-pointer-down ((tool point-sequence-tool) app cursor)
  (let* ((x (cursor-x cursor))
         (y (cursor-y cursor))
         (earlier (reverse (sequence-points tool)))
         (problem (point-problem tool
                                 earlier
                                 x
                                 y)))
    (cond (problem
           (set-message app
                        problem
                        :error))
          ((< (length earlier) (1- (points-needed tool)))
           (push (point-key x y) (sequence-points tool)))
          ((create-entity app (lambda () (build-from-points tool
                                                            app
                                                            earlier
                                                            x
                                                            y)))
           (setf (sequence-points tool) nil)))
    (app-request-redraw app)))

(defmethod tool-pointer-move ((tool point-sequence-tool) app cursor)
  (declare (ignore cursor))
  (when (sequence-points tool)
    (app-request-redraw app)))

(defmethod tool-preview-entities ((tool point-sequence-tool) app)
  (let ((cursor (app-cursor app))
        (earlier (reverse (sequence-points tool))))
    (when (and earlier cursor)
      (let ((entity (try-entity
                     (lambda ()
                       (if (= (length earlier) (1- (points-needed tool)))
                           (build-from-points tool
                                              app
                                              earlier
                                              (cursor-x cursor)
                                              (cursor-y cursor))
                           (dom:make-line :id (creation-id app)
                                          :layer-id (creation-layer-id app)
                                          :x1 (car (first earlier))
                                          :y1 (cdr (first earlier))
                                          :x2 (cursor-x cursor)
                                          :y2 (cursor-y cursor)))))))
        (and entity (list entity))))))

(defmethod tool-cancel ((tool point-sequence-tool) app)
  (setf (sequence-points tool) nil)
  (app-request-redraw app))

(defmethod tool-in-progress-p ((tool point-sequence-tool))
  (and (sequence-points tool) t))

(defmethod tool-anchor ((tool point-sequence-tool))
  (first (sequence-points tool)))

(defun direction-from (center
                       x
                       y)
  "Return the direction in radians from the point CENTER, a cons, to (X, Y).

CENTER: an (x . y) cons.
X: world x.
Y: world y."
  (atan (- y (cdr center)) (- x (car center))))

(defclass arc-tool (point-sequence-tool) ()
  (:documentation "Creates an arc from its center, the point where it starts (which gives the radius) and
a point in the direction where it ends, counter-clockwise."))
(defmethod tool-kind ((tool arc-tool)) :arc)
(defmethod points-needed ((tool arc-tool)) 3)
(defmethod sequence-prompts ((tool arc-tool) app)
  (declare (ignore app))
  (list (i18n:tr "Arc: click the center")
        (i18n:tr "Arc: click where the arc starts (Esc cancels)")
        (i18n:tr "Arc: click in the direction where it ends, counter-clockwise (Esc cancels)")))
(defmethod build-from-points ((tool arc-tool) app points x y)
  (destructuring-bind (center start) points
    (dom:make-arc :id (creation-id app)
                  :layer-id (creation-layer-id app)
                  :center-x (car center)
                  :center-y (cdr center)
                  :radius (geo:distance (car center)
                                        (cdr center)
                                        (car start)
                                        (cdr start))
                  :start-angle (direction-from center
                                               (car start)
                                               (cdr start))
                  :end-angle (direction-from center
                                             x
                                             y))))

(defclass ellipse-tool (point-sequence-tool) ()
  (:documentation "Creates an ellipse from its center, the end of its first axis, and a point whose distance
from that axis is the other half-axis."))
(defmethod tool-kind ((tool ellipse-tool)) :ellipse)
(defmethod points-needed ((tool ellipse-tool)) 3)
(defmethod sequence-prompts ((tool ellipse-tool) app)
  (declare (ignore app))
  (list (i18n:tr "Ellipse: click the center")
        (i18n:tr "Ellipse: click the end of the first axis (Esc cancels)")
        (i18n:tr "Ellipse: click to set the other half-axis (Esc cancels)")))
(defmethod point-problem ((tool ellipse-tool) points x y)
  (if (= (length points) 2)
      nil
      (call-next-method)))
(defmethod build-from-points ((tool ellipse-tool) app points x y)
  (destructuring-bind (center axis-end) points
    (let* ((rotation (direction-from center
                                     (car axis-end)
                                     (cdr axis-end)))
           (radius-x (geo:distance (car center)
                                   (cdr center)
                                   (car axis-end)
                                   (cdr axis-end)))
           (across (abs (- (* (- (car center) x) (sin rotation)) (* (- y (cdr center)) (- (cos rotation)))))))
      (dom:make-ellipse :id (creation-id app)
                        :layer-id (creation-layer-id app)
                        :center-x (car center)
                        :center-y (cdr center)
                        :radius-x radius-x
                        :radius-y across
                        :rotation rotation))))

(defconstant +minimum-polygon-sides+ 3
  "Fewest sides of a regular polygon.")

(defclass polygon-tool (point-sequence-tool) ()
  (:documentation "Creates a regular polygon from its center and one of its vertices; Up and Down change
the number of sides, which the application keeps for the next polygon."))
(defmethod tool-kind ((tool polygon-tool)) :polygon)
(defmethod points-needed ((tool polygon-tool)) 2)
(defmethod sequence-prompts ((tool polygon-tool) app)
  (list (i18n:trf "Polygon, ~d sides (Up and Down change it): click the center" (app-polygon-sides app))
        (i18n:trf "Polygon, ~d sides: click a vertex (Esc cancels)" (app-polygon-sides app))))
(defmethod build-from-points ((tool polygon-tool) app points x y)
  (let ((center (first points)))
    (dom:make-polygon :id (creation-id app)
                      :layer-id (creation-layer-id app)
                      :center-x (car center)
                      :center-y (cdr center)
                      :radius (geo:distance (car center)
                                            (cdr center)
                                            x
                                            y)
                      :sides (app-polygon-sides app)
                      :rotation (direction-from center
                                                x
                                                y))))
(defmethod tool-key ((tool polygon-tool) app key modifiers)
  (declare (ignore modifiers))
  (case key
    ((:up :down)
     (setf (app-polygon-sides app)
           (max +minimum-polygon-sides+
                (min dom:+maximum-polygon-sides+
                     (+ (app-polygon-sides app) (if (eq key :up) 1 -1)))))
     (app-request-redraw app)
     t)
    (t nil)))

;;; Polyline

(defclass polyline-tool (tool)
  ((points :initform nil :accessor polyline-points))
  (:documentation "Collects vertices; Enter finishes an open polyline, clicking the first vertex closes it."))

(defmethod tool-kind ((tool polyline-tool)) :polyline)

(defmethod tool-prompt ((tool polyline-tool) app)
  (declare (ignore app))
  (if (polyline-points tool)
      (i18n:tr "Polyline: click vertices; Enter finishes; click the first vertex to close; Backspace removes the last")
      (i18n:tr "Polyline: click the first vertex")))

(defun polyline-coordinates-from (points)
  "Return the flat coordinate array of POINTS, which are stored last-first.

POINTS: list of (x . y) conses, the newest first."
  (let* ((ordered (reverse points))
         (coordinates (make-array (* 2 (length ordered)) :element-type 'double-float)))
    (loop for (x . y) in ordered
          for index from 0 by 2
          do (setf (aref coordinates index) x
                   (aref coordinates (1+ index)) y))
    coordinates))

(defun finish-polyline (tool
                        app
                        closed)
  "Create the polyline from the collected vertices.

TOOL: the polyline tool.
APP: the application.
CLOSED: true to close it."
  (when (create-entity app (lambda ()
                             (dom:make-polyline :id (creation-id app)
                                                :layer-id (creation-layer-id app)
                                                :coordinates (polyline-coordinates-from (polyline-points tool))
                                                :closed closed)))
    (setf (polyline-points tool) nil)))

(defmethod tool-pointer-down ((tool polyline-tool) app cursor)
  (let ((points (polyline-points tool)))
    (cond ((and (>= (length points) 3)
                (let ((first (car (last points))))
                  (<= (geo:distance (car first)
                                    (cdr first)
                                    (cursor-raw-x cursor)
                                    (cursor-raw-y cursor))
                      (pick-radius-in-world app))))
           (finish-polyline tool
                            app
                            t))
          ((and points (= (car (first points)) (cursor-x cursor)) (= (cdr (first points)) (cursor-y cursor)))
           (set-message app
                        (i18n:tr "That vertex is already placed.")
                        :error))
          (t (push (point-key (cursor-x cursor) (cursor-y cursor)) (polyline-points tool))
             (app-request-redraw app)))))

(defmethod tool-pointer-move ((tool polyline-tool) app cursor)
  (declare (ignore cursor))
  (when (polyline-points tool)
    (app-request-redraw app)))

(defmethod tool-key ((tool polyline-tool) app key modifiers)
  (declare (ignore modifiers))
  (case key
    (:enter (cond ((>= (length (polyline-points tool)) 2) (finish-polyline tool
                                                                           app
                                                                           nil) t)
                  ((polyline-points tool) (set-message app
                                                       (i18n:tr "A polyline needs at least two vertices.")
                                                       :error) t)
                  (t nil)))
    (:backspace (when (polyline-points tool)
                  (pop (polyline-points tool))
                  (app-request-redraw app)
                  t))
    (t nil)))

(defmethod tool-preview-entities ((tool polyline-tool) app)
  (let ((cursor (app-cursor app))
        (points (polyline-points tool)))
    (when (and points cursor)
      (let ((entity (try-entity
                     (lambda ()
                       (dom:make-polyline :id (creation-id app)
                                          :layer-id (creation-layer-id app)
                                          :coordinates (polyline-coordinates-from
                                                        (cons (point-key (cursor-x cursor) (cursor-y cursor)) points))
                                          :closed nil)))))
        (and entity (list entity))))))

(defmethod tool-cancel ((tool polyline-tool) app)
  (setf (polyline-points tool) nil)
  (app-request-redraw app))

(defmethod tool-in-progress-p ((tool polyline-tool))
  (and (polyline-points tool) t))

(defmethod tool-anchor ((tool polyline-tool))
  (first (polyline-points tool)))

;;; Text

(defconstant +text-angle-snap-degrees+ 15d0
  "Step, in degrees, of the angles a dragged text snaps to.")

(defconstant +text-angle-snap-tolerance-degrees+ 3d0
  "How close, in degrees, a dragged text's angle must be to a multiple of the step to snap to it.")

(defclass text-tool (tool)
  ((anchor :initform nil :accessor text-tool-anchor)
   (angle :initform 0d0 :accessor text-tool-angle)
   (dragged :initform nil :accessor text-tool-dragged))
  (:documentation "Places a text annotation: a click puts it at that point, and dragging from the point
sets the direction of its baseline. The content is asked for when the button is released."))

(defmethod tool-kind ((tool text-tool)) :text)

(defmethod tool-prompt ((tool text-tool) app)
  (declare (ignore app))
  (i18n:tr "Text: click where the text starts, or drag to set its angle"))

(defun snapped-text-angle (angle)
  "Return ANGLE, in radians, moved onto the nearest multiple of the snap step when it is within the
tolerance of it.

ANGLE: a direction in radians."
  (let* ((degrees (base:radians->degrees angle))
         (nearest (* +text-angle-snap-degrees+ (fround degrees +text-angle-snap-degrees+))))
    (if (<= (abs (- degrees nearest)) +text-angle-snap-tolerance-degrees+)
        (base:degrees->radians nearest)
        angle)))

(defmethod tool-pointer-down ((tool text-tool) app cursor)
  (setf (text-tool-anchor tool) (point-key (cursor-x cursor) (cursor-y cursor))
        (text-tool-angle tool) 0d0
        (text-tool-dragged tool) nil)
  (app-request-redraw app))

(defmethod tool-pointer-move ((tool text-tool) app cursor)
  (let ((anchor (text-tool-anchor tool)))
    (when anchor
      (let ((dx (- (cursor-x cursor) (car anchor)))
            (dy (- (cursor-y cursor) (cdr anchor))))
        (when (> (geo:distance 0d0
                               0d0
                               dx
                               dy) (* 2 (pick-radius-in-world app)))
          (setf (text-tool-dragged tool) t
                (text-tool-angle tool) (snapped-text-angle (geo:normalize-angle (atan dy dx))))))
      (app-request-redraw app))))

(defmethod tool-pointer-up ((tool text-tool) app cursor)
  (declare (ignore cursor))
  (let ((anchor (text-tool-anchor tool))
        (angle (if (text-tool-dragged tool) (text-tool-angle tool) 0d0)))
    (setf (text-tool-anchor tool) nil
          (text-tool-dragged tool) nil)
    (when anchor
      (let ((x (car anchor))
            (y (cdr anchor)))
        (open-text-prompt app
                          (i18n:tr "Text")
                          (i18n:tr "Text to place at the clicked point:")
                          ""
                          (lambda (string)
                            (create-entity app
                                           (lambda ()
                                             (dom:make-text :id (creation-id app)
                                                            :layer-id (creation-layer-id app)
                                                            :x x
                                                            :y y
                                                            :string string
                                                            :height (dom:drawing-default-text-height
                                                                     (app-drawing app))
                                                            :angle angle)))))))))

(defmethod tool-preview-entities ((tool text-tool) app)
  (let ((anchor (text-tool-anchor tool)))
    (when (and anchor (text-tool-dragged tool))
      (let ((entity (try-entity (lambda ()
                                  (dom:make-text :id (creation-id app)
                                                 :layer-id (creation-layer-id app)
                                                 :x (car anchor)
                                                 :y (cdr anchor)
                                                 :string "Abc"
                                                 :height (dom:drawing-default-text-height (app-drawing app))
                                                 :angle (text-tool-angle tool))))))
        (and entity (list entity))))))

(defmethod tool-cancel ((tool text-tool) app)
  (setf (text-tool-anchor tool) nil
        (text-tool-dragged tool) nil)
  (app-request-redraw app))

(defmethod tool-in-progress-p ((tool text-tool))
  (and (text-tool-anchor tool) (text-tool-dragged tool) t))

;;; Dimension

(defclass dimension-tool (tool)
  ((first-point :initform nil :accessor dimension-first-point)
   (second-point :initform nil :accessor dimension-second-point))
  (:documentation "Creates an aligned dimension from two clicks and a third that places its line."))

(defmethod tool-kind ((tool dimension-tool)) :dimension)

(defmethod tool-prompt ((tool dimension-tool) app)
  (declare (ignore app))
  (cond ((null (dimension-first-point tool)) (i18n:tr "Dimension: click the first point"))
        ((null (dimension-second-point tool)) (i18n:tr "Dimension: click the second point (Esc cancels)"))
        (t (i18n:tr "Dimension: click to place the dimension line (Esc cancels)"))))

(defun dimension-offset-for (tool
                             x
                             y)
  "Return the signed offset of a dimension line through (X, Y): the distance of that point from
the line through the two definition points, positive on the left of the first-to-second
direction.

TOOL: the dimension tool with both points set.
X: world x.
Y: world y."
  (let* ((first (dimension-first-point tool))
         (second (dimension-second-point tool))
         (dx (- (car second) (car first)))
         (dy (- (cdr second) (cdr first)))
         (length (sqrt (+ (* dx dx) (* dy dy)))))
    (/ (+ (* (- x (car first)) (- dy)) (* (- y (cdr first)) dx)) length)))

(defun build-dimension (tool
                        app
                        x
                        y)
  "Build the dimension whose line passes through (X, Y).

TOOL: the dimension tool with both points set.
APP: the application.
X: world x of the line.
Y: world y of the line."
  (let* ((drawing (app-drawing app))
         (unit (dom:drawing-display-unit drawing))
         (first (dimension-first-point tool))
         (second (dimension-second-point tool)))
    (dom:make-dimension :id (creation-id app)
                        :layer-id (creation-layer-id app)
                        :x1 (car first)
                        :y1 (cdr first)
                        :x2 (car second)
                        :y2 (cdr second)
                        :offset (dimension-offset-for tool
                                                      x
                                                      y)
                        :text-height (dom:drawing-default-text-height drawing)
                        :unit unit
                        :precision (base:default-display-precision unit))))

(defmethod tool-pointer-down ((tool dimension-tool) app cursor)
  (let ((x (cursor-x cursor))
        (y (cursor-y cursor)))
    (cond ((null (dimension-first-point tool))
           (setf (dimension-first-point tool) (point-key x y))
           (app-request-redraw app))
          ((null (dimension-second-point tool))
           (if (base:positive-length-p (geo:distance (car (dimension-first-point tool))
                                                     (cdr (dimension-first-point tool))
                                                     x
                                                     y))
               (setf (dimension-second-point tool) (point-key x y))
               (set-message app
                            (i18n:tr "The two points must be apart.")
                            :error))
           (app-request-redraw app))
          (t (when (create-entity app (lambda () (build-dimension tool
                                                                  app
                                                                  x
                                                                  y)))
               (setf (dimension-first-point tool) nil
                     (dimension-second-point tool) nil))))))

(defmethod tool-pointer-move ((tool dimension-tool) app cursor)
  (declare (ignore cursor))
  (when (dimension-first-point tool)
    (app-request-redraw app)))

(defmethod tool-preview-entities ((tool dimension-tool) app)
  (let ((cursor (app-cursor app))
        (first (dimension-first-point tool)))
    (when (and first cursor)
      (let ((entity (try-entity
                     (lambda ()
                       (if (dimension-second-point tool)
                           (build-dimension tool
                                            app
                                            (cursor-x cursor)
                                            (cursor-y cursor))
                           (dom:make-line :id (creation-id app)
                                          :layer-id (creation-layer-id app)
                                          :x1 (car first)
                                          :y1 (cdr first)
                                          :x2 (cursor-x cursor)
                                          :y2 (cursor-y cursor)))))))
        (and entity (list entity))))))

(defmethod tool-cancel ((tool dimension-tool) app)
  (setf (dimension-first-point tool) nil
        (dimension-second-point tool) nil)
  (app-request-redraw app))

(defmethod tool-in-progress-p ((tool dimension-tool))
  (and (dimension-first-point tool) t))

(defmethod tool-anchor ((tool dimension-tool))
  (and (null (dimension-second-point tool))
       (dimension-first-point tool)))

(defun make-tool (kind)
  "Create the tool named by KIND.

KIND: :select, :line, :rectangle, :circle, :polyline, :text, :dimension, :arc, :ellipse, :polygon, :rotate,
:mirror, :scale, :measure-distance, :measure-angle, :measure-area, :radius-dimension, :diameter-dimension,
:angular-dimension, :hatch, :solid-fill, :trim, :extend, :offset, :fillet, :spread or :subtract."
  (ecase kind
    (:select (make-instance 'select-tool))
    (:line (make-instance 'line-tool))
    (:rectangle (make-instance 'rectangle-tool))
    (:circle (make-instance 'circle-tool))
    (:polyline (make-instance 'polyline-tool))
    (:text (make-instance 'text-tool))
    (:dimension (make-instance 'dimension-tool))
    (:arc (make-instance 'arc-tool))
    (:ellipse (make-instance 'ellipse-tool))
    (:polygon (make-instance 'polygon-tool))
    (:rotate (make-instance 'rotate-tool))
    (:mirror (make-instance 'mirror-tool))
    (:scale (make-instance 'scale-tool))
    (:measure-distance (make-instance 'measure-distance-tool))
    (:measure-angle (make-instance 'measure-angle-tool))
    (:measure-area (make-instance 'measure-area-tool))
    (:radius-dimension (make-instance 'radial-dimension-tool))
    (:diameter-dimension (make-instance 'radial-dimension-tool :diameter t))
    (:angular-dimension (make-instance 'angular-dimension-tool))
    (:hatch (make-instance 'hatch-tool :pattern :lines))
    (:solid-fill (make-instance 'hatch-tool :pattern :solid))
    (:trim (make-instance 'trim-tool))
    (:extend (make-instance 'extend-tool))
    (:offset (make-instance 'offset-tool))
    (:fillet (make-instance 'fillet-tool))
    (:spread (make-instance 'spread-tool))
    (:subtract (make-instance 'subtract-tool))))

(defparameter *tool-shortcuts*
  '((:l . :line) (:r . :rectangle) (:c . :circle) (:p . :polyline) (:t . :text) (:d . :dimension)
    (:a . :arc) (:e . :ellipse) (:n . :polygon) (:m . :measure-distance) (:i . :insert))
  "Letter keys that activate tools.")
