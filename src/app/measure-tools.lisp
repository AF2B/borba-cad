(in-package #:borba-cad/app)

;;;; The tools that measure and change nothing: the distance between two points, the angle at a vertex, and the
;;;; area a contour encloses. The result is drawn on the canvas as a labelled shape, in the preview color, and
;;;; said in the status bar; it stays on the canvas until the next click starts another measurement or the tool
;;;; is left.

(defconstant +measure-label-height-pixels+ 13d0
  "Height of the text that gives a measurement on the canvas, in pixels.")

(defconstant +measure-label-offset-pixels+ 10d0
  "How far the label of a measurement sits from the point it belongs to, in pixels.")

(defconstant +measure-angle-arc-pixels+ 44d0
  "Radius of the arc that marks a measured angle, in pixels, unless an arm is shorter than twice that.")

(defconstant +measure-minimum-contour-points+ 3
  "Corners a contour needs to enclose an area.")

(defparameter *measure-tool-kinds* '(:measure-distance :measure-angle :measure-area :calibrate)
  "The tools that measure, which the Measure button opens, and the one that calibrates an underlay by measuring.")

(defun measure-tool-kind-p (kind)
  "Return true when KIND is a tool that measures.

KIND: a tool kind keyword."
  (and (member kind *measure-tool-kinds*) t))

;;; What the tools share

(defclass measure-tool (tool)
  ((done :initform nil :accessor measure-done))
  (:documentation "Base of the measuring tools. DONE is true while the result of a finished measurement is on
display; the next click clears it and starts another."))

(defgeneric measurement-entities (tool app)
  (:documentation "Return the entities that draw what TOOL has measured or is measuring, with the label.

TOOL: a measuring tool.
APP: the application."))

(defmethod tool-preview-entities ((tool measure-tool) app)
  (measurement-entities tool app))

(defun measure-label (app
                      x
                      y
                      text)
  "Return the text entity that labels a measurement, a little above and to the right of a point, in a size that
does not change with the zoom.

APP: the application.
X: world x of the point.
Y: world y of the point.
TEXT: what the label says."
  (let ((viewport (app-viewport app)))
    (try-entity (lambda ()
                  (dom:make-text :id (creation-id app)
                                 :layer-id (creation-layer-id app)
                                 :x (+ x (view:pixels->world-length viewport +measure-label-offset-pixels+))
                                 :y (+ y (view:pixels->world-length viewport +measure-label-offset-pixels+))
                                 :string text
                                 :height (view:pixels->world-length viewport +measure-label-height-pixels+))))))

(defun measure-line (app
                     x1
                     y1
                     x2
                     y2)
  "Return the line entity that draws part of a measurement, or NIL when its ends coincide.

APP: the application.
X1: world x of the first end.
Y1: world y of the first end.
X2: world x of the second end.
Y2: world y of the second end."
  (try-entity (lambda ()
                (dom:make-line :id (creation-id app)
                               :layer-id (creation-layer-id app)
                               :x1 x1
                               :y1 y1
                               :x2 x2
                               :y2 y2))))

(defun display-unit-of (app)
  "Return the unit the drawing shows lengths in.

APP: the application."
  (dom:drawing-display-unit (app-drawing app)))

(defun length-text (app
                    millimeters)
  "Return a length as text, in the display unit of the drawing and with the unit's name.

APP: the application.
MILLIMETERS: the length."
  (base:format-length millimeters
                      (display-unit-of app)
                      :with-unit t))

(defun area-text (app
                  square-millimeters)
  "Return an area as text, in the display unit of the drawing and with the unit's name.

APP: the application.
SQUARE-MILLIMETERS: the area."
  (base:format-area square-millimeters
                    (display-unit-of app)
                    :with-unit t))

(defun angle-text (radians)
  "Return an angle as text, in degrees with two decimals.

RADIANS: the angle."
  (format nil
          "~a~a"
          (base:format-decimal (base:radians->degrees radians) 2)
          *degree-sign*))

(defun forget-finished-measurement (tool)
  "Forget the measurement on display, when there is one, so that the click that comes begins another. Return true
when there was one.

TOOL: a measuring tool."
  (when (measure-done tool)
    (tool-cancel tool nil)
    t))

;;; Distance

(defclass measure-distance-tool (measure-tool)
  ((first-point :initform nil :accessor distance-first-point)
   (second-point :initform nil :accessor distance-second-point))
  (:documentation "Measures the distance between two clicked points, and says how far apart they are along each
axis and in which direction the second lies from the first."))

(defmethod tool-kind ((tool measure-distance-tool)) :measure-distance)

(defmethod tool-prompt ((tool measure-distance-tool) app)
  (declare (ignore app))
  (cond ((measure-done tool) (i18n:tr "Distance: click a new first point; Esc leaves"))
        ((distance-first-point tool) (i18n:tr "Distance: click the second point (Esc cancels)"))
        (t (i18n:tr "Distance: click the first point"))))

(defun distance-message (app
                         first
                         second)
  "Say the distance between two points, with its parts.

APP: the application.
FIRST: an (x . y) cons.
SECOND: an (x . y) cons."
  (let ((dx (- (car second) (car first)))
        (dy (- (cdr second) (cdr first))))
    (set-message app
                 (i18n:trf "Distance: ~a   dx ~a   dy ~a   angle ~a"
                           (length-text app (geo:distance (car first)
                                                          (cdr first)
                                                          (car second)
                                                          (cdr second)))
                           (length-text app (abs dx))
                           (length-text app (abs dy))
                           (angle-text (geo:normalize-angle (atan dy dx)))))))

(defmethod tool-pointer-down ((tool measure-distance-tool) app cursor)
  (let ((point (point-key (cursor-x cursor) (cursor-y cursor)))
        (first (distance-first-point tool)))
    (cond ((forget-finished-measurement tool)
           (setf (distance-first-point tool) point))
          ((null first)
           (setf (distance-first-point tool) point))
          ((base:positive-length-p (geo:distance (car first)
                                                 (cdr first)
                                                 (car point)
                                                 (cdr point)))
           (setf (distance-second-point tool) point
                 (measure-done tool) t)
           (distance-message app
                             first
                             point))
          (t (set-message app
                          (i18n:tr "The two points must be apart.")
                          :error)))
    (app-request-redraw app)))

(defmethod tool-pointer-move ((tool measure-distance-tool) app cursor)
  (declare (ignore cursor))
  (when (and (distance-first-point tool) (not (measure-done tool)))
    (app-request-redraw app)))

(defmethod measurement-entities ((tool measure-distance-tool) app)
  (let* ((cursor (app-cursor app))
         (first (distance-first-point tool))
         (second (or (distance-second-point tool)
                     (and cursor (point-key (cursor-x cursor) (cursor-y cursor))))))
    (when (and first second)
      (let ((line (measure-line app
                                (car first)
                                (cdr first)
                                (car second)
                                (cdr second))))
        (when line
          (remove nil
                  (list line
                        (measure-label app
                                       (/ (+ (car first) (car second)) 2)
                                       (/ (+ (cdr first) (cdr second)) 2)
                                       (length-text app (geo:distance (car first)
                                                                      (cdr first)
                                                                      (car second)
                                                                      (cdr second)))))))))))

(defmethod tool-cancel ((tool measure-distance-tool) app)
  (declare (ignore app))
  (setf (distance-first-point tool) nil
        (distance-second-point tool) nil
        (measure-done tool) nil))

(defmethod tool-in-progress-p ((tool measure-distance-tool))
  (and (distance-first-point tool) (not (measure-done tool))))

(defmethod tool-anchor ((tool measure-distance-tool))
  (and (not (measure-done tool))
       (distance-first-point tool)))

;;; Angle

(defclass measure-angle-tool (measure-tool)
  ((vertex :initform nil :accessor angle-vertex)
   (first-arm :initform nil :accessor angle-first-arm)
   (second-arm :initform nil :accessor angle-second-arm))
  (:documentation "Measures the angle at a vertex between two arms: the vertex, then a point on each arm."))

(defmethod tool-kind ((tool measure-angle-tool)) :measure-angle)

(defmethod tool-prompt ((tool measure-angle-tool) app)
  (declare (ignore app))
  (cond ((measure-done tool) (i18n:tr "Angle: click a new vertex; Esc leaves"))
        ((angle-first-arm tool) (i18n:tr "Angle: click a point on the second arm (Esc cancels)"))
        ((angle-vertex tool) (i18n:tr "Angle: click a point on the first arm (Esc cancels)"))
        (t (i18n:tr "Angle: click the vertex"))))

(defun arm-angle (vertex
                  point)
  "Return the direction from a vertex to a point, in radians.

VERTEX: an (x . y) cons.
POINT: an (x . y) cons."
  (atan (- (cdr point) (cdr vertex))
        (- (car point) (car vertex))))

(defun measured-angle (vertex
                       first
                       second)
  "Return the angle at VERTEX between the arms toward FIRST and SECOND, in radians from 0 to pi, or NIL when an
arm has no length.

VERTEX: an (x . y) cons.
FIRST: an (x . y) cons.
SECOND: an (x . y) cons."
  (geo:angle-between-rays (car vertex)
                          (cdr vertex)
                          (car first)
                          (cdr first)
                          (car second)
                          (cdr second)))

(defun angle-message (app
                      angle)
  "Say a measured angle and the angle that completes it to a whole turn.

APP: the application.
ANGLE: the angle, in radians."
  (set-message app
               (i18n:trf "Angle: ~a   (the other side: ~a)"
                         (angle-text angle)
                         (angle-text (- geo:+two-pi+ angle)))))

(defmethod tool-pointer-down ((tool measure-angle-tool) app cursor)
  (let ((point (point-key (cursor-x cursor) (cursor-y cursor))))
    (cond ((forget-finished-measurement tool)
           (setf (angle-vertex tool) point))
          ((null (angle-vertex tool))
           (setf (angle-vertex tool) point))
          ((not (base:positive-length-p (geo:distance (car (angle-vertex tool))
                                                      (cdr (angle-vertex tool))
                                                      (car point)
                                                      (cdr point))))
           (set-message app
                        (i18n:tr "The two points must be apart.")
                        :error))
          ((null (angle-first-arm tool))
           (setf (angle-first-arm tool) point))
          (t
           (setf (angle-second-arm tool) point
                 (measure-done tool) t)
           (angle-message app
                          (measured-angle (angle-vertex tool)
                                          (angle-first-arm tool)
                                          point))))
    (app-request-redraw app)))

(defmethod tool-pointer-move ((tool measure-angle-tool) app cursor)
  (declare (ignore cursor))
  (when (and (angle-vertex tool) (not (measure-done tool)))
    (app-request-redraw app)))

(defun turn-sign (vertex
                  first
                  second)
  "Return 1 when the second arm lies counter-clockwise of the first, within half a turn, and -1 when clockwise.

VERTEX: an (x . y) cons.
FIRST: a point on the first arm.
SECOND: a point on the second arm."
  (if (>= (- (* (- (car first) (car vertex)) (- (cdr second) (cdr vertex)))
             (* (- (cdr first) (cdr vertex)) (- (car second) (car vertex))))
          0)
      1d0
      -1d0))

(defun angle-arc (app
                  vertex
                  first
                  second)
  "Return the arc that marks the angle at a vertex, or NIL when the arms are too close to being one line or
too short to show it.

APP: the application.
VERTEX: an (x . y) cons.
FIRST: a point on the first arm.
SECOND: a point on the second arm."
  (let* ((first-direction (arm-angle vertex first))
         (second-direction (arm-angle vertex second))
         (counter-clockwise (plusp (turn-sign vertex
                                              first
                                              second)))
         (shorter-arm (min (geo:distance (car vertex)
                                         (cdr vertex)
                                         (car first)
                                         (cdr first))
                           (geo:distance (car vertex)
                                         (cdr vertex)
                                         (car second)
                                         (cdr second))))
         (radius (min (view:pixels->world-length (app-viewport app) +measure-angle-arc-pixels+)
                      (/ shorter-arm 2))))
    (try-entity (lambda ()
                  (dom:make-arc :id (creation-id app)
                                :layer-id (creation-layer-id app)
                                :center-x (car vertex)
                                :center-y (cdr vertex)
                                :radius radius
                                :start-angle (if counter-clockwise first-direction second-direction)
                                :end-angle (if counter-clockwise second-direction first-direction))))))

(defun angle-label-point (vertex
                          first
                          second
                          angle)
  "Return where the label of an angle goes: on the bisector, halfway along the shorter arm.

VERTEX: an (x . y) cons.
FIRST: a point on the first arm.
SECOND: a point on the second arm.
ANGLE: the angle between the arms, in radians."
  (let* ((middle (+ (arm-angle vertex first)
                    (* (turn-sign vertex
                                  first
                                  second) (/ angle 2))))
         (reach (/ (min (geo:distance (car vertex)
                                      (cdr vertex)
                                      (car first)
                                      (cdr first))
                        (geo:distance (car vertex)
                                      (cdr vertex)
                                      (car second)
                                      (cdr second)))
                   2)))
    (point-key (+ (car vertex) (* reach (cos middle)))
               (+ (cdr vertex) (* reach (sin middle))))))

(defmethod measurement-entities ((tool measure-angle-tool) app)
  (let* ((cursor (app-cursor app))
         (vertex (angle-vertex tool))
         (first (angle-first-arm tool))
         (second (or (angle-second-arm tool)
                     (and first cursor (point-key (cursor-x cursor) (cursor-y cursor)))))
         (tip (or first
                  (and cursor (point-key (cursor-x cursor) (cursor-y cursor))))))
    (when (and vertex tip)
      (let* ((angle (and second (measured-angle vertex
                                                first
                                                second)))
             (label-point (and angle (angle-label-point vertex
                                                        first
                                                        second
                                                        angle))))
        (remove nil
                (list (measure-line app
                                    (car vertex)
                                    (cdr vertex)
                                    (car tip)
                                    (cdr tip))
                      (and second
                           (measure-line app
                                         (car vertex)
                                         (cdr vertex)
                                         (car second)
                                         (cdr second)))
                      (and angle (angle-arc app
                                            vertex
                                            first
                                            second))
                      (and angle
                           (measure-label app
                                          (car label-point)
                                          (cdr label-point)
                                          (angle-text angle)))))))))

(defmethod tool-cancel ((tool measure-angle-tool) app)
  (declare (ignore app))
  (setf (angle-vertex tool) nil
        (angle-first-arm tool) nil
        (angle-second-arm tool) nil
        (measure-done tool) nil))

(defmethod tool-in-progress-p ((tool measure-angle-tool))
  (and (angle-vertex tool) (not (measure-done tool))))

(defmethod tool-anchor ((tool measure-angle-tool))
  (and (not (measure-done tool))
       (angle-vertex tool)))

;;; Area

(defclass measure-area-tool (measure-tool)
  ((points :initform nil :accessor area-points))
  (:documentation "Measures the area and the perimeter of a contour given by its corners, the newest first. Enter
or a click on the first corner closes it."))

(defmethod tool-kind ((tool measure-area-tool)) :measure-area)

(defmethod tool-prompt ((tool measure-area-tool) app)
  (declare (ignore app))
  (cond ((measure-done tool) (i18n:tr "Area: click a corner to measure another contour; Esc leaves"))
        ((area-points tool)
         (i18n:tr "Area: click the corners; Enter or the first corner closes; Backspace removes the last"))
        (t (i18n:tr "Area: click the first corner of the contour"))))

(defun contour-coordinates (points)
  "Return the flat coordinate array of the corners of a contour, in the order they were given.

POINTS: the corners, the newest first, as (x . y) conses."
  (polyline-coordinates-from points))

(defun contour-perimeter (points)
  "Return the length of the closed contour through POINTS.

POINTS: the corners, as (x . y) conses."
  (geo:polyline-length (contour-coordinates points) t))

(defun finish-contour (tool
                       app)
  "Close the contour and say its area and perimeter, unless it encloses nothing.

TOOL: the area tool.
APP: the application."
  (let* ((points (area-points tool))
         (area (geo:contour-area (contour-coordinates points))))
    (cond ((< (length points) +measure-minimum-contour-points+)
           (set-message app
                        (i18n:tr "An area needs at least three corners.")
                        :error))
          ((<= area (* base:+length-tolerance+ base:+length-tolerance+))
           (set-message app
                        (i18n:tr "Those corners enclose no area.")
                        :error))
          (t
           (setf (measure-done tool) t)
           (set-message app
                        (i18n:trf "Area: ~a   perimeter: ~a"
                                  (area-text app area)
                                  (length-text app (contour-perimeter points))))))
    (app-request-redraw app)))

(defmethod tool-pointer-down ((tool measure-area-tool) app cursor)
  (let ((points (area-points tool)))
    (cond ((forget-finished-measurement tool)
           (push (point-key (cursor-x cursor) (cursor-y cursor)) (area-points tool))
           (app-request-redraw app))
          ((and (>= (length points) +measure-minimum-contour-points+)
                (let ((first (car (last points))))
                  (<= (geo:distance (car first)
                                    (cdr first)
                                    (cursor-raw-x cursor)
                                    (cursor-raw-y cursor))
                      (pick-radius-in-world app))))
           (finish-contour tool app))
          ((and points (= (car (first points)) (cursor-x cursor)) (= (cdr (first points)) (cursor-y cursor)))
           (set-message app
                        (i18n:tr "That vertex is already placed.")
                        :error))
          (t (push (point-key (cursor-x cursor) (cursor-y cursor)) (area-points tool))
             (app-request-redraw app)))))

(defmethod tool-pointer-move ((tool measure-area-tool) app cursor)
  (declare (ignore cursor))
  (when (and (area-points tool) (not (measure-done tool)))
    (app-request-redraw app)))

(defmethod tool-key ((tool measure-area-tool) app key modifiers)
  (declare (ignore modifiers))
  (case key
    (:enter (cond ((measure-done tool) nil)
                  ((area-points tool) (finish-contour tool app) t)
                  (t nil)))
    (:backspace (when (and (area-points tool) (not (measure-done tool)))
                  (pop (area-points tool))
                  (app-request-redraw app)
                  t))
    (t nil)))

(defun contour-label-point (points)
  "Return where the label of a contour goes: the middle of its bounds.

POINTS: the corners, as (x . y) conses."
  (let ((bounds (geo:polyline-bounds (contour-coordinates points))))
    (point-key (geo:bounds-center-x bounds)
               (geo:bounds-center-y bounds))))

(defmethod measurement-entities ((tool measure-area-tool) app)
  (let* ((cursor (app-cursor app))
         (corners (if (measure-done tool)
                      (area-points tool)
                      (and (area-points tool)
                           cursor
                           (cons (point-key (cursor-x cursor) (cursor-y cursor)) (area-points tool))))))
    (when corners
      (let* ((outline (try-entity (lambda ()
                                    (dom:make-polyline :id (creation-id app)
                                                       :layer-id (creation-layer-id app)
                                                       :coordinates (contour-coordinates corners)
                                                       :closed (>= (length corners)
                                                                   +measure-minimum-contour-points+)))))
             (area (geo:contour-area (contour-coordinates corners)))
             (label-point (contour-label-point corners)))
        (remove nil
                (list outline
                      (and outline
                           (plusp area)
                           (measure-label app
                                          (car label-point)
                                          (cdr label-point)
                                          (area-text app area)))))))))

(defmethod tool-cancel ((tool measure-area-tool) app)
  (declare (ignore app))
  (setf (area-points tool) nil
        (measure-done tool) nil))

(defmethod tool-in-progress-p ((tool measure-area-tool))
  (and (area-points tool) (not (measure-done tool))))

(defmethod tool-anchor ((tool measure-area-tool))
  (and (not (measure-done tool))
       (first (area-points tool))))

;;; The Measure menu

(defparameter *measure-menu-tools*
  (list (list :measure-distance :ruler (i18n:mark "Distance") "M")
        (list :measure-angle :angle (i18n:mark "Angle") "Shift+A")
        (list :measure-area :area (i18n:mark "Area") "Shift+Q")
        nil
        (list :quantities :quantities (i18n:mark "Quantities...") "Shift+L"))
  "The choices of the measure menu, top to bottom, as (tool-kind icon label shortcut). The labels are in English
and are translated where the menu is built.")

(defparameter *measure-button-icons*
  '((:measure-distance . :ruler) (:measure-angle . :angle) (:measure-area . :area) (:calibrate . :ruler))
  "The icon the Measure button wears while each measuring tool is active.")

(defparameter *measure-shortcuts* '((:a . :measure-angle) (:q . :measure-area) (:l . :quantities))
  "Letter keys that activate the measuring tools when Shift is held; the plain M is the distance.")
