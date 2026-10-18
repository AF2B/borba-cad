(in-package #:borba-cad/domain)

;;;; Editing what exists by what is around it: offsetting an entity, trimming a line, an arc or a circle where other
;;;; entities cross it, extending a line or an arc up to the next one, and rounding the corner of two lines with an
;;;; arc. Every function here works out a change set and changes nothing; an edit that cannot be done as asked
;;;; signals EDIT-REFUSED with the reason, in a sentence.

(defconstant +extension-reach+ 4d7
  "How far past its end a line is searched for something to extend to, in millimetres: more than the drawing is
wide, since coordinates stop at ten million.")

(defconstant +miter-limit+ 10d0
  "How many times the offset distance a corner of an offset polyline may stick out, before the corner is judged too
sharp to offset.")

(defconstant +rounded-corner-minimum-turn+ 1d-6
  "How far from straight, in radians, the two lines of a fillet must turn from each other, so that they are told
apart from one line.")

(defun refuse-edit (control
                    &rest arguments)
  "Signal EDIT-REFUSED with a sentence.

CONTROL: an English format control, translated before it is filled in.
ARGUMENTS: its format arguments."
  (error 'base:edit-refused
         :reason (apply #'i18n:trf control arguments)))

(defun require-editable (drawing
                         entity)
  "Signal EDIT-REFUSED unless ENTITY is on a layer that is not locked.

DRAWING: the drawing.
ENTITY: the entity to be changed."
  (unless (editable-entity-p drawing entity)
    (refuse-edit "That entity is on a locked layer.")))

;;; Offsetting

(defgeneric entity-offset (entity distance)
  (:documentation "Return the entity that lies DISTANCE from ENTITY, with the same id and layer, or NIL when this kind
of entity cannot be offset. A positive distance grows a circle, an arc, a rectangle or a polygon, and a closed
polyline, and puts a line or an open polyline on its left, looking from its start to its end; a negative one
does the opposite. Signals INVALID-ENTITY when the result would not be valid.

ENTITY: an entity.
DISTANCE: millimetres.")
  (:method ((entity entity) distance)
    (declare (ignore distance))
    nil))

(defgeneric entity-offset-through (entity x y)
  (:documentation "Return the signed distance that offsetting ENTITY must be given for the result to pass through the
point, on the side the offset uses, or NIL when this kind of entity cannot be offset. For a shape made of
segments it is the distance to the line of the nearest segment.

ENTITY: an entity.
X: world x of the point.
Y: world y of the point.")
  (:method ((entity entity) x y)
    (declare (ignore x y))
    nil))

(defun left-signed-distance (x1
                             y1
                             x2
                             y2
                             x
                             y)
  "Return the distance from a point to the line through two points, positive on the left of the direction from the
first to the second.

X1: x of the first point of the line.
Y1: y of the first point.
X2: x of the second point.
Y2: y of the second point.
X: x of the point.
Y: y of the point."
  (let* ((dx (- x2 x1))
         (dy (- y2 y1))
         (length (sqrt (+ (* dx dx) (* dy dy)))))
    (/ (- (* (- y y1) dx) (* (- x x1) dy)) length)))

(defun nearest-segment-distance (segments
                                 x
                                 y)
  "Return the signed distance, positive on the left, from a point to the line of the segment nearest to it.

SEGMENTS: a list of (x1 y1 x2 y2) lists.
X: world x of the point.
Y: world y of the point."
  (let ((best nil)
        (best-distance nil))
    (dolist (segment segments)
      (destructuring-bind (x1 y1 x2 y2) segment
        (let ((distance (geo:point-segment-distance x
                                                    y
                                                    x1
                                                    y1
                                                    x2
                                                    y2)))
          (when (or (null best-distance) (< distance best-distance))
            (setf best segment
                  best-distance distance)))))
    (destructuring-bind (x1 y1 x2 y2) best
      (left-signed-distance x1
                            y1
                            x2
                            y2
                            x
                            y))))

(defun primitive-segments (entity)
  "Return the segments an entity is made of, each as a list (x1 y1 x2 y2).

ENTITY: an entity."
  (loop for primitive in (entity-primitives entity)
        when (eq :segment (first primitive))
          collect (rest primitive)))

(defmethod entity-offset ((line line-entity) distance)
  (let* ((dx (- (line-x2 line) (line-x1 line)))
         (dy (- (line-y2 line) (line-y1 line)))
         (length (sqrt (+ (* dx dx) (* dy dy))))
         (shift-x (* (- (/ dy length)) distance))
         (shift-y (* (/ dx length) distance)))
    (update-line line
                 :x1 (+ (line-x1 line) shift-x)
                 :y1 (+ (line-y1 line) shift-y)
                 :x2 (+ (line-x2 line) shift-x)
                 :y2 (+ (line-y2 line) shift-y))))

(defmethod entity-offset-through ((line line-entity) x y)
  (left-signed-distance (line-x1 line)
                        (line-y1 line)
                        (line-x2 line)
                        (line-y2 line)
                        x
                        y))

(defmethod entity-offset ((circle circle-entity) distance)
  (update-circle circle :radius (+ (circle-radius circle) distance)))

(defmethod entity-offset-through ((circle circle-entity) x y)
  (- (geo:distance (circle-center-x circle)
                   (circle-center-y circle)
                   x
                   y)
     (circle-radius circle)))

(defmethod entity-offset ((arc arc-entity) distance)
  (update-arc arc :radius (+ (arc-radius arc) distance)))

(defmethod entity-offset-through ((arc arc-entity) x y)
  (- (geo:distance (arc-center-x arc)
                   (arc-center-y arc)
                   x
                   y)
     (arc-radius arc)))

(defmethod entity-offset ((rectangle rectangle-entity) distance)
  (update-rectangle rectangle
                    :min-x (- (rectangle-min-x rectangle) distance)
                    :min-y (- (rectangle-min-y rectangle) distance)
                    :width (+ (rectangle-width rectangle) (* 2 distance))
                    :height (+ (rectangle-height rectangle) (* 2 distance))))

(defmethod entity-offset-through ((rectangle rectangle-entity) x y)
  (- (nearest-segment-distance (primitive-segments rectangle)
                               x
                               y)))

(defmethod entity-offset ((polygon polygon-entity) distance)
  (update-polygon polygon
                  :radius (+ (polygon-radius polygon)
                             (/ distance (cos (/ geo:+pi+ (polygon-sides polygon)))))))

(defmethod entity-offset-through ((polygon polygon-entity) x y)
  (- (nearest-segment-distance (primitive-segments polygon)
                               x
                               y)))

(defun contour-turns-counter-clockwise-p (coordinates)
  "Return true when the closed contour through the vertices goes around counter-clockwise.

COORDINATES: simple-array of double-float holding x0 y0 x1 y1 ..."
  (let ((count (geo:polyline-vertex-count coordinates))
        (sum 0d0))
    (dotimes (index count)
      (let ((next (mod (1+ index) count)))
        (incf sum (- (* (aref coordinates (* 2 index)) (aref coordinates (1+ (* 2 next))))
                     (* (aref coordinates (* 2 next)) (aref coordinates (1+ (* 2 index))))))))
    (plusp sum)))

(defun polyline-left-shift (polyline
                            distance)
  "Return how far to the left the vertices of a polyline move for an offset of DISTANCE: the distance itself for an
open one, and for a closed one the distance outward, which is to the right when it goes counter-clockwise.

POLYLINE: a polyline entity.
DISTANCE: the offset, positive outward for a closed polyline."
  (if (and (polyline-closed polyline)
           (contour-turns-counter-clockwise-p (polyline-coordinates polyline)))
      (- distance)
      distance))

(defun offset-polyline-coordinates (coordinates
                                    closed
                                    shift)
  "Return the vertices of a polyline moved SHIFT to the left of each segment, the corners where two segments meet
kept sharp. Signals INVALID-ENTITY when a corner turns back on itself or is too sharp.

COORDINATES: simple-array of double-float holding x0 y0 x1 y1 ...
CLOSED: true when the last vertex joins the first.
SHIFT: how far to move, positive to the left."
  (let* ((count (geo:polyline-vertex-count coordinates))
         (result (make-array (length coordinates) :element-type 'double-float)))
    (labels ((vertex (index)
               (values (aref coordinates (* 2 index)) (aref coordinates (1+ (* 2 index)))))
           (left-normal (from
                         to)
             (multiple-value-bind (x1 y1) (vertex from)
               (multiple-value-bind (x2 y2) (vertex to)
                 (let ((length (geo:distance x1
                                             y1
                                             x2
                                             y2)))
                   (values (- (/ (- y2 y1) length)) (/ (- x2 x1) length)))))))
      (dotimes (index count)
        (multiple-value-bind (x y) (vertex index)
          (let* ((before (cond ((> index 0) (1- index))
                               (closed (1- count))))
                 (after (cond ((< index (1- count)) (1+ index))
                              (closed 0)))
                 (shift-x 0d0)
                 (shift-y 0d0))
            (cond ((and before after)
                   (multiple-value-bind (nx1 ny1) (left-normal before index)
                     (multiple-value-bind (nx2 ny2) (left-normal index after)
                       (let ((denominator (+ 1d0 (* nx1 nx2) (* ny1 ny2))))
                         (when (< denominator (/ 1d0 (* +miter-limit+ +miter-limit+)))
                           (invalid-entity-failure :polyline "a corner is too sharp to offset"))
                         (setf shift-x (/ (* shift (+ nx1 nx2)) denominator)
                               shift-y (/ (* shift (+ ny1 ny2)) denominator))))))
                  (before
                   (multiple-value-bind (nx ny) (left-normal before index)
                     (setf shift-x (* shift nx)
                           shift-y (* shift ny))))
                  (t
                   (multiple-value-bind (nx ny) (left-normal index after)
                     (setf shift-x (* shift nx)
                           shift-y (* shift ny)))))
            (setf (aref result (* 2 index)) (+ x shift-x)
                  (aref result (1+ (* 2 index))) (+ y shift-y))))))
    result))

(defmethod entity-offset ((polyline polyline-entity) distance)
  (update-polyline polyline
                   :coordinates (offset-polyline-coordinates (polyline-coordinates polyline)
                                                             (polyline-closed polyline)
                                                             (polyline-left-shift polyline distance))))

(defmethod entity-offset-through ((polyline polyline-entity) x y)
  (let ((left (nearest-segment-distance (primitive-segments polyline)
                                        x
                                        y)))
    (if (polyline-closed polyline)
        (polyline-left-shift polyline left)
        left)))

(defun offset-changes (drawing
                       id
                       distance)
  "Return the change set that adds the entity that lies DISTANCE from the entity with ID, on the same layer.
Signals EDIT-REFUSED when the entity is missing, on a locked layer or of a kind that cannot be offset, and
INVALID-ENTITY when the result would not be valid.

DRAWING: the drawing.
ID: id of the entity to offset.
DISTANCE: millimetres; positive grows or goes left, see ENTITY-OFFSET."
  (let ((entity (store-find (drawing-store drawing) id)))
    (unless entity
      (refuse-edit "That entity is no longer there."))
    (require-editable drawing entity)
    (let ((result (entity-offset entity distance)))
      (unless result
        (refuse-edit "That cannot be offset: use a line, circle, arc, rectangle, polygon or polyline."))
      (create-entity-changes (entity-with-identity result
                                                   (drawing-next-entity-id drawing)
                                                   (entity-layer-id entity))))))

;;; What cuts and what stops

(defun cutting-primitives (drawing
                           target-id)
  "Return the primitives of every entity on a visible layer except the one with TARGET-ID: what trimming and
extending look for.

DRAWING: the drawing.
TARGET-ID: id of the entity being edited."
  (let ((primitives '()))
    (map-entities (lambda (entity)
                    (let ((layer (find-layer drawing (entity-layer-id entity))))
                      (when (and layer
                                 (layer-visible layer)
                                 (/= (entity-id entity) target-id))
                        (dolist (primitive (entity-primitives entity))
                          (push primitive primitives)))))
                  (drawing-store drawing))
    primitives))

(defun crossings-with (target
                       primitives)
  "Return the points where a primitive crosses any of PRIMITIVES, as a list of (x . y) conses.

TARGET: a primitive.
PRIMITIVES: a list of primitives."
  (loop for other in primitives
        append (primitive-intersections target other)))

(defun distinct-sorted (values
                        gap)
  "Return VALUES sorted ascending, with each one that is within GAP of the one before it left out.

VALUES: a list of real numbers.
GAP: how close two values may be before they count as one."
  (let ((result '()))
    (dolist (value (sort (copy-list values) #'<))
      (when (or (null result) (> (- value (first result)) gap))
        (push value result)))
    (nreverse result)))

(defun existing-target (drawing
                        id
                        kinds)
  "Return the entity with ID, which must be editable and of one of KINDS, or signal EDIT-REFUSED.

DRAWING: the drawing.
ID: id of the entity.
KINDS: the entity kinds that are acceptable."
  (let ((entity (store-find (drawing-store drawing) id)))
    (unless entity
      (refuse-edit "That entity is no longer there."))
    (require-editable drawing entity)
    (unless (member (entity-kind entity) kinds)
      (refuse-edit "This works on lines, arcs and circles."))
    entity))

(defun replacing-changes (drawing
                          entity
                          pieces)
  "Return the change set that puts PIECES, a list of entities, where ENTITY was: the first takes its place with its
id, and each other is added with a new id.

DRAWING: the drawing.
ENTITY: the entity that is cut.
PIECES: one or two entities, made from it."
  (let ((next-id (drawing-next-entity-id drawing)))
    (cons (make-change (entity-id entity)
                       entity
                       (first pieces))
          (loop for piece in (rest pieces)
                collect (prog1 (make-change next-id
                                            nil
                                            (entity-with-identity piece
                                                                  next-id
                                                                  (entity-layer-id entity)))
                          (incf next-id))))))

;;; Trimming

(defun segment-parameter (x1
                          y1
                          x2
                          y2
                          x
                          y)
  "Return where the projection of a point falls along a segment: 0 at its start and 1 at its end.

X1: x of the start of the segment.
Y1: y of the start of the segment.
X2: x of its end.
Y2: y of its end.
X: x of the point.
Y: y of the point."
  (let ((dx (- x2 x1))
        (dy (- y2 y1)))
    (/ (+ (* (- x x1) dx) (* (- y y1) dy))
       (+ (* dx dx) (* dy dy)))))

(defun trim-line-changes (drawing
                          line
                          primitives
                          x
                          y)
  "Return the change set that cuts away the part of a line between the two crossings that surround a point.

DRAWING: the drawing.
LINE: a line entity.
PRIMITIVES: the primitives that cut it.
X: world x of the click.
Y: world y of the click."
  (let* ((x1 (line-x1 line))
         (y1 (line-y1 line))
         (x2 (line-x2 line))
         (y2 (line-y2 line))
         (length (geo:distance x1
                               y1
                               x2
                               y2))
         (margin (/ base:+length-tolerance+ length))
         (parameters (distinct-sorted (loop for point in (crossings-with (list :segment x1 y1 x2 y2) primitives)
                                            for parameter = (segment-parameter x1
                                                                               y1
                                                                               x2
                                                                               y2
                                                                               (car point)
                                                                               (cdr point))
                                            when (< margin parameter (- 1d0 margin))
                                              collect parameter)
                                      margin))
         (at (max 0d0 (min 1d0 (segment-parameter x1
                                                  y1
                                                  x2
                                                  y2
                                                  x
                                                  y)))))
    (when (null parameters)
      (refuse-edit "Nothing crosses the line there, so there is nothing to trim."))
    (let ((lower (find-if (lambda (parameter) (<= parameter at)) parameters :from-end t))
          (upper (find-if (lambda (parameter) (> parameter at)) parameters)))
      (flet ((piece (from
                     to)
               (update-line line
                            :x1 (+ x1 (* from (- x2 x1)))
                            :y1 (+ y1 (* from (- y2 y1)))
                            :x2 (+ x1 (* to (- x2 x1)))
                            :y2 (+ y1 (* to (- y2 y1))))))
        (replacing-changes drawing
                           line
                           (remove nil
                                   (list (and lower (piece 0d0 lower))
                                         (and upper (piece upper 1d0)))))))))

(defun arc-position (arc
                     x
                     y)
  "Return how far along an arc, in radians from its start, the direction of a point from its center lies, going
counter-clockwise, in the range 0 to a whole turn.

ARC: an arc entity.
X: world x of the point.
Y: world y of the point."
  (geo:normalize-angle (- (atan (- y (arc-center-y arc)) (- x (arc-center-x arc)))
                          (arc-start-angle arc))))

(defun trim-arc-changes (drawing
                         arc
                         primitives
                         x
                         y)
  "Return the change set that cuts away the part of an arc between the two crossings that surround a point.

DRAWING: the drawing.
ARC: an arc entity.
PRIMITIVES: the primitives that cut it.
X: world x of the click.
Y: world y of the click."
  (let* ((sweep (arc-sweep arc))
         (margin (/ base:+length-tolerance+ (arc-radius arc)))
         (positions (distinct-sorted (loop for point in (crossings-with (first (entity-primitives arc)) primitives)
                                           for position = (arc-position arc
                                                                        (car point)
                                                                        (cdr point))
                                           when (< margin position (- sweep margin))
                                             collect position)
                                     margin))
         (raw (arc-position arc
                            x
                            y))
         (at (cond ((<= raw sweep) raw)
                   ((> (- raw sweep) (- geo:+two-pi+ raw)) 0d0)
                   (t sweep))))
    (when (null positions)
      (refuse-edit "Nothing crosses the arc there, so there is nothing to trim."))
    (let ((lower (find-if (lambda (position) (<= position at)) positions :from-end t))
          (upper (find-if (lambda (position) (> position at)) positions))
          (start (arc-start-angle arc)))
      (flet ((piece (from
                     to)
               (update-arc arc
                           :start-angle (+ start from)
                           :end-angle (+ start to))))
        (replacing-changes drawing
                           arc
                           (remove nil
                                   (list (and lower (piece 0d0 lower))
                                         (and upper (piece upper sweep)))))))))

(defun trim-circle-changes (drawing
                            circle
                            primitives
                            x
                            y)
  "Return the change set that turns a circle into the arc that is left when the part between the two crossings that
surround a point is cut away.

DRAWING: the drawing.
CIRCLE: a circle entity.
PRIMITIVES: the primitives that cut it.
X: world x of the click.
Y: world y of the click."
  (let* ((center-x (circle-center-x circle))
         (center-y (circle-center-y circle))
         (margin (/ base:+length-tolerance+ (circle-radius circle)))
         (angles (distinct-sorted (loop for point in (crossings-with (first (entity-primitives circle)) primitives)
                                        collect (geo:normalize-angle (atan (- (cdr point) center-y)
                                                                           (- (car point) center-x))))
                                  margin))
         (at (geo:normalize-angle (atan (- y center-y) (- x center-x)))))
    (when (< (length angles) 2)
      (refuse-edit "A circle needs two crossings before part of it can be trimmed."))
    (let ((lower (or (find-if (lambda (angle) (<= angle at)) angles :from-end t) (car (last angles))))
          (upper (or (find-if (lambda (angle) (> angle at)) angles) (first angles))))
      (replacing-changes drawing
                         circle
                         (list (make-arc :id (entity-id circle)
                                         :layer-id (entity-layer-id circle)
                                         :center-x center-x
                                         :center-y center-y
                                         :radius (circle-radius circle)
                                         :start-angle upper
                                         :end-angle lower))))))

(defun trim-changes (drawing
                     id
                     x
                     y)
  "Return the change set that trims the line, arc or circle with ID: the part of it that lies between the two
places where other entities cross it, or between one and its end, and that holds the point, is cut away. A line
or an arc may be left in two pieces. Signals EDIT-REFUSED when it cannot be done.

DRAWING: the drawing.
ID: id of the entity to trim.
X: world x of the click, which says which part to cut away.
Y: world y of the click."
  (let* ((entity (existing-target drawing
                                  id
                                  '(:line :arc :circle)))
         (primitives (cutting-primitives drawing id)))
    (ecase (entity-kind entity)
      (:line (trim-line-changes drawing
                                entity
                                primitives
                                x
                                y))
      (:arc (trim-arc-changes drawing
                              entity
                              primitives
                              x
                              y))
      (:circle (trim-circle-changes drawing
                                    entity
                                    primitives
                                    x
                                    y)))))

;;; Extending

(defun extend-line-changes (drawing
                            line
                            primitives
                            x
                            y)
  "Return the change set that lengthens the end of a line that is nearest to a point, along the line, up to the first
thing that stands in its way.

DRAWING: the drawing.
LINE: a line entity.
PRIMITIVES: the primitives that stop it.
X: world x of the click.
Y: world y of the click."
  (declare (ignore drawing))
  (let* ((x1 (line-x1 line))
         (y1 (line-y1 line))
         (x2 (line-x2 line))
         (y2 (line-y2 line))
         (second-end (<= (geo:distance x2
                                       y2
                                       x
                                       y) (geo:distance x1
                                                        y1
                                                        x
                                                        y)))
         (end-x (if second-end x2 x1))
         (end-y (if second-end y2 y1))
         (length (geo:distance x1
                               y1
                               x2
                               y2))
         (direction-x (/ (if second-end (- x2 x1) (- x1 x2)) length))
         (direction-y (/ (if second-end (- y2 y1) (- y1 y2)) length))
         (reach (list :segment
                      end-x
                      end-y
                      (+ end-x (* direction-x +extension-reach+))
                      (+ end-y (* direction-y +extension-reach+))))
         (best nil)
         (best-distance nil))
    (dolist (point (crossings-with reach primitives))
      (let ((distance (+ (* (- (car point) end-x) direction-x) (* (- (cdr point) end-y) direction-y))))
        (when (and (> distance base:+length-tolerance+)
                   (or (null best-distance) (< distance best-distance)))
          (setf best point
                best-distance distance))))
    (unless best
      (refuse-edit "Nothing stands in the way of that end of the line, so there is nothing to extend to."))
    (list (make-change (entity-id line)
                       line
                       (if second-end
                           (update-line line
                                        :x2 (car best)
                                        :y2 (cdr best))
                           (update-line line
                                        :x1 (car best)
                                        :y1 (cdr best)))))))

(defun extend-arc-changes (drawing
                           arc
                           primitives
                           x
                           y)
  "Return the change set that lengthens the end of an arc that is nearest to a point, along its circle, up to the first
thing that stands in its way.

DRAWING: the drawing.
ARC: an arc entity.
PRIMITIVES: the primitives that stop it.
X: world x of the click.
Y: world y of the click."
  (declare (ignore drawing))
  (let* ((sweep (arc-sweep arc))
         (position (arc-position arc
                                 x
                                 y))
         (second-end (< (min (- sweep position) (- geo:+two-pi+ (- position sweep)))
                        (min position (- geo:+two-pi+ position))))
         (free (- geo:+two-pi+ sweep))
         (margin (/ base:+length-tolerance+ (arc-radius arc)))
         (circle (list :circle (arc-center-x arc) (arc-center-y arc) (arc-radius arc)))
         (best nil))
    (dolist (point (crossings-with circle primitives))
      (let* ((angle (atan (- (cdr point) (arc-center-y arc)) (- (car point) (arc-center-x arc))))
             (beyond (if second-end
                         (geo:normalize-angle (- angle (arc-end-angle arc)))
                         (geo:normalize-angle (- (arc-start-angle arc) angle)))))
        (when (and (< margin beyond (- free margin))
                   (or (null best) (< beyond best)))
          (setf best beyond))))
    (unless best
      (refuse-edit "Nothing stands in the way of that end of the arc, so there is nothing to extend to."))
    (list (make-change (entity-id arc)
                       arc
                       (if second-end
                           (update-arc arc :end-angle (+ (arc-end-angle arc) best))
                           (update-arc arc :start-angle (- (arc-start-angle arc) best)))))))

(defun extend-changes (drawing
                       id
                       x
                       y)
  "Return the change set that extends the line or the arc with ID, at the end that is nearer to the point, up to the
first entity that stands in its way. Signals EDIT-REFUSED when it cannot be done.

DRAWING: the drawing.
ID: id of the entity to extend.
X: world x of the click, which says which end.
Y: world y of the click."
  (let* ((entity (existing-target drawing
                                  id
                                  '(:line :arc)))
         (primitives (cutting-primitives drawing id)))
    (ecase (entity-kind entity)
      (:line (extend-line-changes drawing
                                  entity
                                  primitives
                                  x
                                  y))
      (:arc (extend-arc-changes drawing
                                entity
                                primitives
                                x
                                y)))))

;;; Filleting

(defun line-intersection-point (first
                                second)
  "Return, as two values, the point where the infinite lines through two line entities cross, or NIL when they are
parallel.

FIRST: a line entity.
SECOND: a line entity."
  (let* ((x1 (line-x1 first))
         (y1 (line-y1 first))
         (dx1 (- (line-x2 first) x1))
         (dy1 (- (line-y2 first) y1))
         (x3 (line-x1 second))
         (y3 (line-y1 second))
         (dx2 (- (line-x2 second) x3))
         (dy2 (- (line-y2 second) y3))
         (denominator (- (* dx1 dy2) (* dy1 dx2)))
         (scale (* (sqrt (+ (* dx1 dx1) (* dy1 dy1))) (sqrt (+ (* dx2 dx2) (* dy2 dy2))))))
    (unless (< (abs denominator) (* +rounded-corner-minimum-turn+ scale))
      (let ((along (/ (- (* (- x3 x1) dy2) (* (- y3 y1) dx2)) denominator)))
        (values (+ x1 (* along dx1))
                (+ y1 (* along dy1)))))))

(defun kept-side (line
                  corner-x
                  corner-y
                  x
                  y)
  "Return what a fillet keeps of a line: the direction, as two values of a unit vector, from the corner toward the
click, and the distance from the corner to the end of the line that is farther away in that direction.

LINE: a line entity.
CORNER-X: x of the corner.
CORNER-Y: y of the corner.
X: world x of the click on the line.
Y: world y of the click on the line."
  (let* ((length (geo:distance (line-x1 line)
                               (line-y1 line)
                               (line-x2 line)
                               (line-y2 line)))
         (ux (/ (- (line-x2 line) (line-x1 line)) length))
         (uy (/ (- (line-y2 line) (line-y1 line)) length))
         (toward-click (+ (* (- x corner-x) ux) (* (- y corner-y) uy)))
         (second-end-reach (+ (* (- (line-x2 line) corner-x) ux) (* (- (line-y2 line) corner-y) uy)))
         (first-end-reach (+ (* (- (line-x1 line) corner-x) ux) (* (- (line-y1 line) corner-y) uy)))
         (sign (cond ((> (abs toward-click) base:+length-tolerance+) (signum toward-click))
                     ((>= (abs second-end-reach) (abs first-end-reach)) (signum second-end-reach))
                     (t (signum first-end-reach)))))
    (values (* sign ux)
            (* sign uy)
            (max (* sign first-end-reach) (* sign second-end-reach)))))

(defun fillet-changes (drawing
                       first-id
                       first-x
                       first-y
                       second-id
                       second-x
                       second-y
                       radius)
  "Return the change set that rounds the corner of two lines: each is cut or lengthened to the point where an arc of
the given radius touches it, on the side of the corner where it was clicked, and the arc is added on the layer of the
first. With a radius of zero the lines are only taken to the corner. Signals EDIT-REFUSED when it cannot be done.

DRAWING: the drawing.
FIRST-ID: id of the first line.
FIRST-X: world x of the click on the first line.
FIRST-Y: world y of the click on the first line.
SECOND-ID: id of the second line.
SECOND-X: world x of the click on the second line.
SECOND-Y: world y of the click on the second line.
RADIUS: radius of the arc, millimetres, zero or more."
  (when (= first-id second-id)
    (refuse-edit "A corner needs two different lines."))
  (let ((first (existing-target drawing
                                first-id
                                '(:line)))
        (second (existing-target drawing
                                 second-id
                                 '(:line))))
    (multiple-value-bind (corner-x corner-y) (line-intersection-point first second)
      (unless corner-x
        (refuse-edit "The two lines are parallel, so they have no corner."))
      (multiple-value-bind (ux1 uy1 reach1) (kept-side first
                                                       corner-x
                                                       corner-y
                                                       first-x
                                                       first-y)
        (multiple-value-bind (ux2 uy2 reach2) (kept-side second
                                                         corner-x
                                                         corner-y
                                                         second-x
                                                         second-y)
          (let* ((turn (acos (max -1d0 (min 1d0 (+ (* ux1 ux2) (* uy1 uy2))))))
                 (tangent (if (zerop radius)
                              0d0
                              (/ radius (tan (/ turn 2))))))
            (when (or (<= turn +rounded-corner-minimum-turn+)
                      (>= turn (- geo:+pi+ +rounded-corner-minimum-turn+)))
              (refuse-edit "The two lines are parallel, so they have no corner."))
            (when (or (<= reach1 (+ tangent base:+length-tolerance+)) (<= reach2 (+ tangent base:+length-tolerance+)))
              (refuse-edit "The radius is too big for these lines: there is not enough line on that side."))
            (let* ((tangent-1-x (+ corner-x (* ux1 tangent)))
                   (tangent-1-y (+ corner-y (* uy1 tangent)))
                   (tangent-2-x (+ corner-x (* ux2 tangent)))
                   (tangent-2-y (+ corner-y (* uy2 tangent)))
                   (far-1-x (+ corner-x (* ux1 reach1)))
                   (far-1-y (+ corner-y (* uy1 reach1)))
                   (far-2-x (+ corner-x (* ux2 reach2)))
                   (far-2-y (+ corner-y (* uy2 reach2)))
                   (new-first (update-line first
                                           :x1 tangent-1-x
                                           :y1 tangent-1-y
                                           :x2 far-1-x
                                           :y2 far-1-y))
                   (new-second (update-line second
                                            :x1 tangent-2-x
                                            :y1 tangent-2-y
                                            :x2 far-2-x
                                            :y2 far-2-y))
                   (changes (list (make-change first-id
                                               first
                                               new-first)
                                  (make-change second-id
                                               second
                                               new-second))))
              (if (zerop radius)
                  changes
                  (let* ((bisector-x (+ ux1 ux2))
                         (bisector-y (+ uy1 uy2))
                         (bisector-length (sqrt (+ (* bisector-x bisector-x) (* bisector-y bisector-y))))
                         (center-distance (/ radius (sin (/ turn 2))))
                         (center-x (+ corner-x (* center-distance (/ bisector-x bisector-length))))
                         (center-y (+ corner-y (* center-distance (/ bisector-y bisector-length))))
                         (angle-1 (atan (- tangent-1-y center-y) (- tangent-1-x center-x)))
                         (angle-2 (atan (- tangent-2-y center-y) (- tangent-2-x center-x)))
                         (counter-clockwise (plusp (- (* (- tangent-1-x center-x) (- tangent-2-y center-y))
                                                      (* (- tangent-1-y center-y) (- tangent-2-x center-x))))))
                    (append changes
                            (create-entity-changes
                             (make-arc :id (drawing-next-entity-id drawing)
                                       :layer-id (entity-layer-id first)
                                       :center-x center-x
                                       :center-y center-y
                                       :radius radius
                                       :start-angle (if counter-clockwise angle-1 angle-2)
                                       :end-angle (if counter-clockwise angle-2 angle-1)))))))))))))
