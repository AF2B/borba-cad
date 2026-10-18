(in-package #:borba-cad/app)

;;;; The tools that dimension round things: the radius or the diameter of a circle or an arc, picked with a click and
;;;; given a direction by a second, and the angle between two arms, given by a vertex, a point on each arm and a
;;;; last click that places the arc.

(defparameter *radial-dimension-kinds* '(:circle :arc)
  "The entities whose radius a radial dimension can measure.")

(defconstant +default-radial-angle+ (/ (float pi 1d0) 4)
  "The direction of the leader of a radial dimension while the pointer is at the center: 45 degrees.")

(defconstant +default-angle-precision+ 1
  "Decimals of a degree that a new angle dimension shows.")

;;; Radius and diameter

(defclass radial-dimension-tool (tool)
  ((diameter :initarg :diameter :initform nil :reader radial-tool-diameter)
   (target :initform nil :accessor radial-tool-target))
  (:documentation "Dimensions the radius, or with DIAMETER the diameter, of a circle or an arc: a click on it, then a
click that gives the direction of the leader."))

(defmethod tool-kind ((tool radial-dimension-tool))
  (if (radial-tool-diameter tool)
      :diameter-dimension
      :radius-dimension))

(defmethod tool-prompt ((tool radial-dimension-tool) app)
  (declare (ignore app))
  (cond ((and (radial-tool-diameter tool) (radial-tool-target tool))
         (i18n:tr "Diameter dimension: click where the line should point (Esc cancels)"))
        ((radial-tool-diameter tool) (i18n:tr "Diameter dimension: click a circle or an arc"))
        ((radial-tool-target tool) (i18n:tr "Radius dimension: click where the leader should point (Esc cancels)"))
        (t (i18n:tr "Radius dimension: click a circle or an arc"))))

(defun round-entity-circle (entity)
  "Return the center and the radius of the circle that a circle or an arc lies on, as three values.

ENTITY: a circle or an arc."
  (etypecase entity
    (dom:circle-entity (values (dom:circle-center-x entity)
                               (dom:circle-center-y entity)
                               (dom:circle-radius entity)))
    (dom:arc-entity (values (dom:arc-center-x entity)
                            (dom:arc-center-y entity)
                            (dom:arc-radius entity)))))

(defun build-radial-dimension (tool
                               app
                               x
                               y)
  "Build the radius or diameter dimension of the picked circle whose leader points toward (X, Y).

TOOL: the radial dimension tool with its target set.
APP: the application.
X: world x the leader points toward.
Y: world y the leader points toward."
  (multiple-value-bind (center-x center-y radius) (round-entity-circle (radial-tool-target tool))
    (let* ((drawing (app-drawing app))
           (unit (dom:drawing-display-unit drawing))
           (apart (base:positive-length-p (geo:distance center-x
                                                        center-y
                                                        x
                                                        y))))
      (dom:make-radial-dimension :id (creation-id app)
                                 :layer-id (creation-layer-id app)
                                 :center-x center-x
                                 :center-y center-y
                                 :radius radius
                                 :angle (if apart
                                            (atan (- y center-y) (- x center-x))
                                            +default-radial-angle+)
                                 :diameter (radial-tool-diameter tool)
                                 :text-height (dom:drawing-default-text-height drawing)
                                 :unit unit
                                 :precision (base:default-display-precision unit)))))

(defmethod tool-pointer-down ((tool radial-dimension-tool) app cursor)
  (let ((target (radial-tool-target tool)))
    (if target
        (when (create-entity app
                             (lambda ()
                               (build-radial-dimension tool
                                                       app
                                                       (cursor-x cursor)
                                                       (cursor-y cursor))))
          (setf (radial-tool-target tool) nil))
        (let ((entity (dom:pick-entity (app-drawing app)
                                       (cursor-raw-x cursor)
                                       (cursor-raw-y cursor)
                                       (pick-radius-in-world app)
                                       *radial-dimension-kinds*)))
          (if entity
              (setf (radial-tool-target tool) entity)
              (set-message app
                           (i18n:tr "Click a circle or an arc.")
                           :error))))
    (app-request-redraw app)))

(defmethod tool-pointer-move ((tool radial-dimension-tool) app cursor)
  (declare (ignore cursor))
  (when (radial-tool-target tool)
    (app-request-redraw app)))

(defmethod tool-preview-entities ((tool radial-dimension-tool) app)
  (let ((cursor (app-cursor app)))
    (when (and (radial-tool-target tool) cursor)
      (let ((entity (try-entity (lambda ()
                                  (build-radial-dimension tool
                                                          app
                                                          (cursor-x cursor)
                                                          (cursor-y cursor))))))
        (and entity (list entity))))))

(defmethod tool-cancel ((tool radial-dimension-tool) app)
  (setf (radial-tool-target tool) nil)
  (app-request-redraw app))

(defmethod tool-in-progress-p ((tool radial-dimension-tool))
  (and (radial-tool-target tool) t))

;;; Angle

(defclass angular-dimension-tool (tool)
  ((vertex :initform nil :accessor angular-tool-vertex)
   (first-arm :initform nil :accessor angular-tool-first-arm)
   (second-arm :initform nil :accessor angular-tool-second-arm))
  (:documentation "Dimensions the angle between two arms: the vertex, a point on each arm, and a click that gives the
radius of the arc and which of the two angles the arms make it measures."))

(defmethod tool-kind ((tool angular-dimension-tool)) :angular-dimension)

(defmethod tool-prompt ((tool angular-dimension-tool) app)
  (declare (ignore app))
  (cond ((angular-tool-second-arm tool) (i18n:tr "Angle dimension: click where the arc should go (Esc cancels)"))
        ((angular-tool-first-arm tool) (i18n:tr "Angle dimension: click a point on the second arm (Esc cancels)"))
        ((angular-tool-vertex tool) (i18n:tr "Angle dimension: click a point on the first arm (Esc cancels)"))
        (t (i18n:tr "Angle dimension: click the vertex"))))

(defun arc-side-angles (vertex
                        first
                        second
                        x
                        y)
  "Return the directions of the arm the angle starts at and the arm it ends at, as two values, for the angle between
two arms that has the point (X, Y) inside it: counter-clockwise from the first arm to the second, or from the second
to the first.

VERTEX: an (x . y) cons.
FIRST: a point on the first arm.
SECOND: a point on the second arm.
X: world x of the point that says which angle.
Y: world y of the point that says which angle."
  (let* ((first-direction (arm-angle vertex first))
         (second-direction (arm-angle vertex second))
         (pointer-direction (arm-angle vertex (point-key x y))))
    (if (< (geo:normalize-angle (- pointer-direction first-direction))
           (geo:normalize-angle (- second-direction first-direction)))
        (values first-direction second-direction)
        (values second-direction first-direction))))

(defun build-angular-dimension (tool
                                app
                                x
                                y)
  "Build the angle dimension of the two arms whose arc passes through (X, Y).

TOOL: the angular dimension tool with its vertex and both arms set.
APP: the application.
X: world x the arc passes through.
Y: world y the arc passes through."
  (let ((vertex (angular-tool-vertex tool)))
    (multiple-value-bind (start end) (arc-side-angles vertex
                                                      (angular-tool-first-arm tool)
                                                      (angular-tool-second-arm tool)
                                                      x
                                                      y)
      (dom:make-angular-dimension :id (creation-id app)
                                  :layer-id (creation-layer-id app)
                                  :vertex-x (car vertex)
                                  :vertex-y (cdr vertex)
                                  :start-angle start
                                  :end-angle end
                                  :radius (geo:distance (car vertex)
                                                        (cdr vertex)
                                                        x
                                                        y)
                                  :text-height (dom:drawing-default-text-height (app-drawing app))
                                  :precision +default-angle-precision+))))

(defun distinct-from-vertex-p (tool
                               point)
  "Return true when POINT is away from the vertex of the angular dimension tool.

TOOL: the angular dimension tool with its vertex set.
POINT: an (x . y) cons."
  (let ((vertex (angular-tool-vertex tool)))
    (base:positive-length-p (geo:distance (car vertex)
                                          (cdr vertex)
                                          (car point)
                                          (cdr point)))))

(defmethod tool-pointer-down ((tool angular-dimension-tool) app cursor)
  (let ((point (point-key (cursor-x cursor) (cursor-y cursor))))
    (cond ((null (angular-tool-vertex tool))
           (setf (angular-tool-vertex tool) point))
          ((not (distinct-from-vertex-p tool point))
           (set-message app
                        (i18n:tr "The two points must be apart.")
                        :error))
          ((null (angular-tool-first-arm tool))
           (setf (angular-tool-first-arm tool) point))
          ((null (angular-tool-second-arm tool))
           (if (< base:+angle-tolerance+
                  (measured-angle (angular-tool-vertex tool)
                                  (angular-tool-first-arm tool)
                                  point))
               (setf (angular-tool-second-arm tool) point)
               (set-message app
                            (i18n:tr "The two arms must point in different directions.")
                            :error)))
          (t (when (create-entity app
                                  (lambda ()
                                    (build-angular-dimension tool
                                                             app
                                                             (cursor-x cursor)
                                                             (cursor-y cursor))))
               (tool-cancel tool app))))
    (app-request-redraw app)))

(defmethod tool-pointer-move ((tool angular-dimension-tool) app cursor)
  (declare (ignore cursor))
  (when (angular-tool-vertex tool)
    (app-request-redraw app)))

(defmethod tool-preview-entities ((tool angular-dimension-tool) app)
  (let ((cursor (app-cursor app))
        (vertex (angular-tool-vertex tool)))
    (when (and vertex cursor)
      (if (angular-tool-second-arm tool)
          (let ((entity (try-entity (lambda ()
                                      (build-angular-dimension tool
                                                               app
                                                               (cursor-x cursor)
                                                               (cursor-y cursor))))))
            (and entity (list entity)))
          (let ((arm (try-entity (lambda ()
                                   (dom:make-line :id (creation-id app)
                                                  :layer-id (creation-layer-id app)
                                                  :x1 (car vertex)
                                                  :y1 (cdr vertex)
                                                  :x2 (cursor-x cursor)
                                                  :y2 (cursor-y cursor)))))
                (first-arm (angular-tool-first-arm tool)))
            (remove nil
                    (list arm
                          (and first-arm
                               (try-entity (lambda ()
                                             (dom:make-line :id (creation-id app)
                                                            :layer-id (creation-layer-id app)
                                                            :x1 (car vertex)
                                                            :y1 (cdr vertex)
                                                            :x2 (car first-arm)
                                                            :y2 (cdr first-arm))))))))))))

(defmethod tool-cancel ((tool angular-dimension-tool) app)
  (setf (angular-tool-vertex tool) nil
        (angular-tool-first-arm tool) nil
        (angular-tool-second-arm tool) nil)
  (app-request-redraw app))

(defmethod tool-in-progress-p ((tool angular-dimension-tool))
  (and (angular-tool-vertex tool) t))

(defmethod tool-anchor ((tool angular-dimension-tool))
  (angular-tool-vertex tool))

;;; Hatch

(defclass hatch-tool (tool)
  ((pattern :initarg :pattern :initform :lines :reader hatch-tool-pattern))
  (:documentation "Hatches the smallest closed shape under the pointer with a pattern, at the angle and spacing a new
hatch gets; the inspector changes them afterwards."))

(defmethod tool-kind ((tool hatch-tool))
  (if (eq :solid (hatch-tool-pattern tool))
      :solid-fill
      :hatch))

(defmethod tool-prompt ((tool hatch-tool) app)
  (declare (ignore app))
  (if (eq :solid (hatch-tool-pattern tool))
      (i18n:tr "Solid fill: click inside a closed shape")
      (i18n:tr "Hatch: click inside a closed shape")))

(defun hatch-of-region (tool
                        app
                        boundary)
  "Return the hatch of a region, with the pattern of the tool and the angle and spacing a new hatch gets. Signals
INVALID-ENTITY when it would not be valid.

TOOL: the hatch tool.
APP: the application.
BOUNDARY: the vertices of the region, as a flat array."
  (dom:make-hatch :id (creation-id app)
                  :layer-id (creation-layer-id app)
                  :coordinates boundary
                  :pattern (hatch-tool-pattern tool)
                  :spacing (dom:default-hatch-spacing boundary
                                                      (dom:drawing-default-text-height (app-drawing app)))))

(defun region-under-pointer (app
                             cursor)
  "Return the boundary of the smallest closed shape under the pointer, or NIL.

APP: the application.
CURSOR: the pointer position."
  (dom:hatch-region-at (app-drawing app)
                       (cursor-raw-x cursor)
                       (cursor-raw-y cursor)))

(defmethod tool-pointer-down ((tool hatch-tool) app cursor)
  (let ((boundary (region-under-pointer app cursor)))
    (if boundary
        (create-entity app
                       (lambda ()
                         (hatch-of-region tool
                                          app
                                          boundary)))
        (set-message app
                     (i18n:tr "Click inside a rectangle, circle, ellipse, polygon or closed polyline.")
                     :error)))
  (app-request-redraw app))

(defmethod tool-pointer-move ((tool hatch-tool) app cursor)
  (declare (ignore cursor))
  (app-request-redraw app))

(defmethod tool-preview-entities ((tool hatch-tool) app)
  (let* ((cursor (app-cursor app))
         (boundary (and cursor (region-under-pointer app cursor)))
         (hatch (and boundary
                     (try-entity (lambda ()
                                   (hatch-of-region tool
                                                    app
                                                    boundary))))))
    (and hatch (list hatch))))
