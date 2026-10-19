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
                                                    holes
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
HOLES: a list of such arrays, polygons inside the boundary that the pattern leaves unfilled; owned by the entity.
PATTERN: :solid, :lines or :cross.
ANGLE: direction of the lines, in radians, kept in [0, pi) since a line has no sense.
SPACING: distance between two lines, in millimetres.
SEGMENTS: the lines, as a flat array x1 y1 x2 y2 ..., computed at construction; empty for a solid hatch.
TRIANGLES: the triangles that cover the region, as a flat array of six numbers each, computed at construction; empty
unless the hatch is solid."
  (coordinates (make-array 0 :element-type 'double-float)
   :type (simple-array double-float (*))
   :read-only t)
  (holes '() :type list :read-only t)
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
                               holes
                               pattern
                               angle
                               spacing)
  "Return the lines of a pattern in a region, as a flat array of segments.

COORDINATES: the boundary.
HOLES: the polygons left unfilled inside it.
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
                                                 (aref coordinates 1)
                                                 holes))
               (list (make-array 0 :element-type 'double-float))))))

(defun hatch-hole-rings (holes
                         hole-coordinates
                         hole-sizes)
  "Return the holes of a hatch as a list of validated arrays: those that were given as a list, or, when the sizes are
given, those that the concatenated coordinates hold, as a file stores them.

HOLES: a list of sequences of doubles x0 y0 x1 y1 ..., or NIL.
HOLE-COORDINATES: the vertices of all the holes one after the other, or NIL.
HOLE-SIZES: how many vertices each hole has, as numbers, or NIL."
  (let ((rings (if (and hole-sizes (plusp (length hole-sizes)))
                   (let ((start 0))
                     (loop for size across hole-sizes
                           collect (let ((end (+ start (* 2 (round size)))))
                                     (prog1 (subseq hole-coordinates start (min end (length hole-coordinates)))
                                       (setf start end)))))
                   holes)))
    (loop for ring in rings
          collect (let ((copy (copy-validated-coordinates ring :hatch)))
                    (when (< (geo:polyline-vertex-count copy) 3)
                      (invalid-entity-failure :hatch
                                              "a hole has ~d vertices; it needs at least 3"
                                              (geo:polyline-vertex-count copy)))
                    (unless (> (geo:contour-area copy) (* base:+length-tolerance+ base:+length-tolerance+))
                      (invalid-entity-failure :hatch "a hole encloses no area"))
                    copy))))

(defun make-hatch (&key id
                        layer-id
                        coordinates
                        holes
                        hole-coordinates
                        hole-sizes
                        (pattern :lines)
                        (angle +default-hatch-angle+)
                        spacing)
  "Create a validated hatch. Signals INVALID-ENTITY when an invariant is violated: a boundary of at least three and at
most +MAXIMUM-HATCH-VERTICES+ vertices, in range, that encloses an area, and holes of the same kind, all of them with
at most that many vertices together; a known pattern; and a spacing that is above the tolerance and does not make
more than +MAXIMUM-HATCH-LINES+ lines.

ID: positive entity id.
LAYER-ID: id of the layer.
COORDINATES: sequence of doubles x0 y0 x1 y1 ... of the boundary; copied.
HOLES: a list of such sequences, polygons the pattern leaves unfilled; copied.
HOLE-COORDINATES: instead of HOLES, the vertices of all the holes one after the other, as a file stores them.
HOLE-SIZES: with HOLE-COORDINATES, how many vertices each hole has.
PATTERN: :solid, :lines or :cross.
ANGLE: direction of the lines in radians; any finite angle, kept in [0, pi).
SPACING: distance between two lines, in millimetres."
  (require-identifiers :hatch
                       id
                       layer-id)
  (let* ((copy (copy-validated-coordinates coordinates :hatch))
         (rings (hatch-hole-rings holes
                                  hole-coordinates
                                  hole-sizes))
         (count (+ (geo:polyline-vertex-count copy)
                   (loop for ring in rings
                         sum (geo:polyline-vertex-count ring)))))
    (when (< (geo:polyline-vertex-count copy) 3)
      (invalid-entity-failure :hatch
                              "its boundary has ~d vertices; it needs at least 3"
                              (geo:polyline-vertex-count copy)))
    (when (> count +maximum-hatch-vertices+)
      (invalid-entity-failure :hatch
                              "its boundary and holes have ~d vertices; the limit is ~d"
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
                     rings
                     pattern
                     direction
                     spacing
                     (hatch-pattern-segments copy
                                             rings
                                             pattern
                                             direction
                                             spacing)
                     (cond ((not (eq pattern :solid))
                            (make-array 0 :element-type 'double-float))
                           (rings
                            (geo:region-triangles copy rings))
                           (t
                            (geo:triangulate-polygon copy))))))))

(defun update-hatch (hatch
                     &key (id (entity-id hatch))
                          (layer-id (entity-layer-id hatch))
                          (coordinates (hatch-coordinates hatch))
                          (holes (hatch-holes hatch))
                          (pattern (hatch-pattern hatch))
                          (angle (hatch-angle hatch))
                          (spacing (hatch-spacing hatch)))
  "Return a validated copy of HATCH with the given fields replaced.

HATCH: the hatch to copy.
ID: replacement id.
LAYER-ID: replacement layer id.
COORDINATES: replacement boundary.
HOLES: replacement holes.
PATTERN: replacement pattern.
ANGLE: replacement direction of the lines.
SPACING: replacement spacing."
  (make-hatch :id id
              :layer-id layer-id
              :coordinates coordinates
              :holes holes
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

(defun hatch-hole-coordinates (hatch)
  "Return the vertices of all the holes of a hatch one after the other, as a file stores them.

HATCH: a hatch entity."
  (apply #'concatenate
         '(simple-array double-float (*))
         (or (hatch-holes hatch)
             (list (make-array 0 :element-type 'double-float)))))

(defun hatch-hole-sizes (hatch)
  "Return how many vertices each hole of a hatch has, as an array of numbers.

HATCH: a hatch entity."
  (make-array (length (hatch-holes hatch))
              :element-type 'double-float
              :initial-contents (mapcar (lambda (ring) (float (geo:polyline-vertex-count ring) 1d0))
                                        (hatch-holes hatch))))

(defun hatch-boundary-distance (hatch
                                x
                                y)
  "Return the distance from a point to the boundary of a hatch, or of one of its holes.

HATCH: a hatch entity.
X: point x.
Y: point y."
  (loop for ring in (cons (hatch-coordinates hatch) (hatch-holes hatch))
        minimize (geo:point-polyline-distance x
                                              y
                                              ring
                                              t)))

(defmethod entity-pick-distance ((hatch hatch-entity)
                                 x
                                 y)
  (let ((segments (hatch-segments hatch))
        (nearest (hatch-boundary-distance hatch
                                          x
                                          y)))
    (if (and (eq :solid (hatch-pattern hatch))
             (geo:point-in-rings-p x
                                   y
                                   (hatch-coordinates hatch)
                                   (hatch-holes hatch)))
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

(defun translated-ring (coordinates
                        dx
                        dy)
  "Return a polygon moved by a distance along x and a distance along y.

COORDINATES: a flat array of vertices.
DX: the distance along x.
DY: the distance along y."
  (let ((moved (copy-seq coordinates)))
    (loop for index from 0 below (length moved) by 2
          do (incf (aref moved index) dx)
             (incf (aref moved (1+ index)) dy))
    moved))

(defmethod entity-translated ((hatch hatch-entity)
                              dx
                              dy)
  (update-hatch hatch
                :coordinates (translated-ring (hatch-coordinates hatch)
                                              dx
                                              dy)
                :holes (mapcar (lambda (ring) (translated-ring ring
                                                               dx
                                                               dy))
                               (hatch-holes hatch))))

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
                            (- (geo:contour-area (hatch-coordinates hatch))
                               (loop for ring in (hatch-holes hatch)
                                     sum (geo:contour-area ring))))
        (make-derived-value (i18n:tr "Perimeter")
                            :length
                            (loop for ring in (cons (hatch-coordinates hatch) (hatch-holes hatch))
                                  sum (geo:polyline-length ring t)))
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
      (geo:point-in-rings-p (geo:bounds-center-x bounds)
                            (geo:bounds-center-y bounds)
                            (hatch-coordinates hatch)
                            (hatch-holes hatch))))

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
