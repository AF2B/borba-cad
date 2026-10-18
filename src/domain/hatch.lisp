(in-package #:borba-cad/domain)

(defparameter *hatch-patterns* '(:solid :lines :cross)
  "The ways a hatch fills its region: solid, with parallel lines, or with two sets of lines at right angles.")

(defconstant +maximum-hatch-vertices+ 2000
  "Most vertices the boundary of a hatch may have.")

(defconstant +maximum-hatch-lines+ 5000
  "Most lines one set of a hatch may have, which keeps a spacing that is tiny next to the region from making a hatch
of millions of segments.")

(defconstant +circle-boundary-vertices+ 128
  "How many vertices stand for a circle or an ellipse when it is the boundary of a hatch.")

(defconstant +default-hatch-angle+ (/ (float pi 1d0) 4)
  "The direction of the lines of a new hatch: 45 degrees.")

(defstruct (hatch-entity (:include entity)
                         (:conc-name hatch-)
                         (:constructor %make-hatch (id
                                                    layer-id
                                                    bounds
                                                    coordinates
                                                    pattern
                                                    angle
                                                    spacing
                                                    segments
                                                    triangles))
                         (:copier nil))
  "A region filled with a pattern: solid, or lines at an angle and a spacing, or two sets of them at right angles. The
lines are cut by the boundary, and one of them passes through the first vertex, so the pattern moves with the
region.

COORDINATES: flat array x0 y0 x1 y1 ... of the vertices of the boundary, a simple polygon; owned by the entity.
PATTERN: :solid, :lines or :cross.
ANGLE: direction of the lines, in radians, kept in [0, pi) since a line has no sense.
SPACING: distance between two lines, in millimetres.
SEGMENTS: the lines, as a flat array x1 y1 x2 y2 ..., computed at construction; empty for a solid hatch.
TRIANGLES: the triangles that cover the region, as a flat array of six numbers each, computed at construction; empty
unless the hatch is solid."
  (coordinates (make-array 0 :element-type 'double-float)
   :type (simple-array double-float (*))
   :read-only t)
  (pattern :lines :type keyword :read-only t)
  (angle 0d0 :type double-float :read-only t)
  (spacing 1d0 :type double-float :read-only t)
  (segments (make-array 0 :element-type 'double-float) :type (simple-array double-float (*)) :read-only t)
  (triangles (make-array 0 :element-type 'double-float) :type (simple-array double-float (*)) :read-only t))

(defun hatch-line-families (angle
                            pattern)
  "Return the directions of the sets of lines a pattern draws.

ANGLE: the direction of the first set, in radians.
PATTERN: :solid, :lines or :cross."
  (ecase pattern
    (:solid '())
    (:lines (list angle))
    (:cross (list angle (+ angle (/ geo:+pi+ 2))))))

(defun hatch-pattern-segments (coordinates
                               pattern
                               angle
                               spacing)
  "Return the lines of a pattern in a region, as a flat array of segments.

COORDINATES: the boundary.
PATTERN: :solid, :lines or :cross.
ANGLE: the direction of the lines, in radians.
SPACING: the distance between two lines."
  (let ((families (hatch-line-families angle pattern)))
    (apply #'concatenate
           '(simple-array double-float (*))
           (or (loop for direction in families
                     collect (geo:hatch-segments coordinates
                                                 direction
                                                 spacing
                                                 (aref coordinates 0)
                                                 (aref coordinates 1)))
               (list (make-array 0 :element-type 'double-float))))))

(defun make-hatch (&key id
                        layer-id
                        coordinates
                        (pattern :lines)
                        (angle +default-hatch-angle+)
                        spacing)
  "Create a validated hatch. Signals INVALID-ENTITY when an invariant is violated: a boundary of at least three and at
most +MAXIMUM-HATCH-VERTICES+ vertices, in range, that encloses an area; a known pattern; and a spacing that is
above the tolerance and does not make more than +MAXIMUM-HATCH-LINES+ lines.

ID: positive entity id.
LAYER-ID: id of the layer.
COORDINATES: sequence of doubles x0 y0 x1 y1 ... of the boundary; copied.
PATTERN: :solid, :lines or :cross.
ANGLE: direction of the lines in radians; any finite angle, kept in [0, pi).
SPACING: distance between two lines, in millimetres."
  (require-identifiers :hatch
                       id
                       layer-id)
  (let* ((copy (copy-validated-coordinates coordinates :hatch))
         (count (geo:polyline-vertex-count copy)))
    (when (< count 3)
      (invalid-entity-failure :hatch
                              "its boundary has ~d vertices; it needs at least 3"
                              count))
    (when (> count +maximum-hatch-vertices+)
      (invalid-entity-failure :hatch
                              "its boundary has ~d vertices; the limit is ~d"
                              count
                              +maximum-hatch-vertices+))
    (unless (member pattern *hatch-patterns*)
      (invalid-entity-failure :hatch
                              "its pattern must be one of ~{~(~a~)~^, ~}, got ~s"
                              *hatch-patterns*
                              pattern))
    (require-angle :hatch
                   "angle"
                   angle)
    (require-positive-length :hatch
                             "spacing"
                             spacing)
    (unless (> (geo:contour-area copy) (* base:+length-tolerance+ base:+length-tolerance+))
      (invalid-entity-failure :hatch "its boundary encloses no area"))
    (let ((direction (mod angle geo:+pi+)))
      (dolist (family (hatch-line-families direction pattern))
        (let ((lines (geo:hatch-line-count copy
                                           family
                                           spacing
                                           (aref copy 0)
                                           (aref copy 1))))
          (when (> lines +maximum-hatch-lines+)
            (invalid-entity-failure :hatch
                                    "its spacing is too small for its size: it would make ~d lines; the limit is ~d"
                                    lines
                                    +maximum-hatch-lines+))))
      (let ((bounds (geo:polyline-bounds copy)))
        (require-bounds-in-range :hatch bounds)
        (%make-hatch id
                     layer-id
                     bounds
                     copy
                     pattern
                     direction
                     spacing
                     (hatch-pattern-segments copy
                                             pattern
                                             direction
                                             spacing)
                     (if (eq pattern :solid)
                         (geo:triangulate-polygon copy)
                         (make-array 0 :element-type 'double-float)))))))

(defun update-hatch (hatch
                     &key (id (entity-id hatch))
                          (layer-id (entity-layer-id hatch))
                          (coordinates (hatch-coordinates hatch))
                          (pattern (hatch-pattern hatch))
                          (angle (hatch-angle hatch))
                          (spacing (hatch-spacing hatch)))
  "Return a validated copy of HATCH with the given fields replaced.

HATCH: the hatch to copy.
ID: replacement id.
LAYER-ID: replacement layer id.
COORDINATES: replacement boundary.
PATTERN: replacement pattern.
ANGLE: replacement direction of the lines.
SPACING: replacement spacing."
  (make-hatch :id id
              :layer-id layer-id
              :coordinates coordinates
              :pattern pattern
              :angle angle
              :spacing spacing))

(defun default-hatch-spacing (coordinates
                              text-height)
  "Return the spacing a new hatch of a region gets: half the default text height of the drawing, or more when that
would make too many lines.

COORDINATES: the boundary of the region.
TEXT-HEIGHT: the default text height of the drawing."
  (let* ((bounds (geo:polyline-bounds coordinates))
         (reach (geo:distance (geo:bounds-min-x bounds)
                              (geo:bounds-min-y bounds)
                              (geo:bounds-max-x bounds)
                              (geo:bounds-max-y bounds))))
    (max (/ text-height 2)
         (/ reach (/ +maximum-hatch-lines+ 2)))))

(defmethod entity-kind ((hatch hatch-entity))
  :hatch)

(defun hatch-boundary-distance (hatch
                                x
                                y)
  "Return the distance from a point to the boundary of a hatch.

HATCH: a hatch entity.
X: point x.
Y: point y."
  (geo:point-polyline-distance x
                               y
                               (hatch-coordinates hatch)
                               t))

(defmethod entity-pick-distance ((hatch hatch-entity)
                                 x
                                 y)
  (let ((segments (hatch-segments hatch))
        (nearest (hatch-boundary-distance hatch
                                          x
                                          y)))
    (if (and (eq :solid (hatch-pattern hatch))
             (geo:point-in-polygon-p x
                                     y
                                     (hatch-coordinates hatch)))
        0d0
        (progn
          (loop for index from 0 below (length segments) by 4
                do (setf nearest (min nearest
                                      (geo:point-segment-distance x
                                                                  y
                                                                  (aref segments index)
                                                                  (aref segments (+ index 1))
                                                                  (aref segments (+ index 2))
                                                                  (aref segments (+ index 3))))))
          nearest))))

(defmethod entity-translated ((hatch hatch-entity)
                              dx
                              dy)
  (let ((moved (copy-seq (hatch-coordinates hatch))))
    (loop for index from 0 below (length moved) by 2
          do (incf (aref moved index) dx)
             (incf (aref moved (1+ index)) dy))
    (update-hatch hatch :coordinates moved)))

(defmethod entity-snap-points ((hatch hatch-entity))
  nil)

(defmethod entity-properties ((hatch hatch-entity))
  (list (make-property :pattern
                       (i18n:tr "Pattern")
                       :pattern
                       (hatch-pattern hatch)
                       t)
        (make-property :angle
                       (i18n:tr "Angle")
                       :angle
                       (hatch-angle hatch)
                       t)
        (make-property :spacing
                       (i18n:tr "Spacing")
                       :length
                       (hatch-spacing hatch)
                       t)))

(defmethod entity-derived-values ((hatch hatch-entity))
  (list (make-derived-value (i18n:tr "Area")
                            :area
                            (geo:contour-area (hatch-coordinates hatch)))
        (make-derived-value (i18n:tr "Perimeter")
                            :length
                            (geo:polyline-length (hatch-coordinates hatch) t))
        (make-derived-value (i18n:tr "Vertices")
                            :count
                            (geo:polyline-vertex-count (hatch-coordinates hatch)))))

(defmethod entity-with-property ((hatch hatch-entity)
                                 key
                                 value)
  (ecase key
    (:pattern (update-hatch hatch :pattern value))
    (:angle (update-hatch hatch :angle value))
    (:spacing (update-hatch hatch :spacing value))))

(defmethod entity-with-layer ((hatch hatch-entity)
                              layer-id)
  (update-hatch hatch :layer-id layer-id))

(defmethod entity-with-identity ((hatch hatch-entity)
                                 id
                                 layer-id)
  (update-hatch hatch
                :id id
                :layer-id layer-id))

(defmethod entity-touches-bounds-p ((hatch hatch-entity)
                                    bounds)
  (or (geo:polyline-touches-bounds-p (hatch-coordinates hatch)
                                     t
                                     bounds)
      (geo:point-in-polygon-p (geo:bounds-center-x bounds)
                              (geo:bounds-center-y bounds)
                              (hatch-coordinates hatch))))

(defmethod entity-grips ((hatch hatch-entity))
  (let ((coordinates (hatch-coordinates hatch)))
    (loop for index from 0 below (min +maximum-polyline-grips+ (floor (length coordinates) 2))
          collect (make-grip index
                             (aref coordinates (* 2 index))
                             (aref coordinates (1+ (* 2 index)))
                             :point))))

(defmethod entity-with-grip ((hatch hatch-entity)
                             grip
                             x
                             y)
  (let ((coordinates (copy-seq (hatch-coordinates hatch)))
        (index (grip-id grip)))
    (setf (aref coordinates (* 2 index)) x
          (aref coordinates (1+ (* 2 index))) y)
    (update-hatch hatch :coordinates coordinates)))
