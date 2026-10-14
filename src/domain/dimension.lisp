(in-package #:borba-cad/domain)

(defstruct (dimension-entity (:include entity)
                             (:conc-name dimension-)
                             (:constructor %make-dimension (id
                                                            layer-id
                                                            bounds
                                                            x1
                                                            y1
                                                            x2
                                                            y2
                                                            offset
                                                            text-height
                                                            unit
                                                            precision
                                                            text
                                                            layout))
                             (:copier nil))
  "An aligned linear dimension between two definition points. The measured value is derived
from the points; the displayed text is derived from it, the unit and the precision.

X1: x of the first definition point.
Y1: y of the first definition point.
X2: x of the second definition point.
Y2: y of the second definition point.
OFFSET: signed distance of the dimension line from the measured segment (left of P1-P2 is positive).
TEXT-HEIGHT: text height (em size); every part of the dimension scales with it.
UNIT: length unit keyword of the displayed text.
PRECISION: decimals in the displayed text.
TEXT: the displayed text, computed at construction.
LAYOUT: the geometric layout of all parts, computed at construction."
  (x1 0d0 :type double-float :read-only t)
  (y1 0d0 :type double-float :read-only t)
  (x2 0d0 :type double-float :read-only t)
  (y2 0d0 :type double-float :read-only t)
  (offset 0d0 :type double-float :read-only t)
  (text-height 1d0 :type double-float :read-only t)
  (unit :millimeter :type keyword :read-only t)
  (precision 0 :type (integer 0) :read-only t)
  (text "" :type string :read-only t)
  (layout nil :type (or null geo:dimension-layout) :read-only t))

(defun make-dimension (&key id
                            layer-id
                            x1
                            y1
                            x2
                            y2
                            offset
                            text-height
                            unit
                            precision)
  "Create a validated aligned dimension. Signals INVALID-ENTITY when an invariant is violated.

ID: positive entity id.
LAYER-ID: id of the layer.
X1: x of the first definition point, in millimetres.
Y1: y of the first definition point.
X2: x of the second definition point.
Y2: y of the second definition point.
OFFSET: signed distance of the dimension line from the measured segment.
TEXT-HEIGHT: text height (em size), above the length tolerance.
UNIT: displayed length unit keyword.
PRECISION: decimals displayed, 0 to +MAXIMUM-DIMENSION-PRECISION+."
  (require-identifiers :dimension
                       id
                       layer-id)
  (require-coordinate :dimension
                      "x1"
                      x1)
  (require-coordinate :dimension
                      "y1"
                      y1)
  (require-coordinate :dimension
                      "x2"
                      x2)
  (require-coordinate :dimension
                      "y2"
                      y2)
  (require-coordinate :dimension
                      "offset"
                      offset)
  (require-positive-length :dimension
                           "text height"
                           text-height)
  (unless (base:length-unit-p unit)
    (invalid-entity-failure :dimension
                            "its unit must be one of ~{~(~a~)~^, ~}, got ~s"
                            (base:supported-length-units)
                            unit))
  (unless (typep precision `(integer 0 ,+maximum-dimension-precision+))
    (invalid-entity-failure :dimension
                            "its precision must be between 0 and ~d decimals, got ~s"
                            +maximum-dimension-precision+
                            precision))
  (let ((measured (geo:distance x1
                                y1
                                x2
                                y2)))
    (unless (base:positive-length-p measured)
      (invalid-entity-failure :dimension
                              "its definition points are ~f mm apart; they must be more than ~f mm apart"
                              measured
                              base:+length-tolerance+))
    (let* ((text (base:format-length measured
                                     unit
                                     :precision precision))
           (layout (geo:compute-dimension-layout x1
                                                 y1
                                                 x2
                                                 y2
                                                 offset
                                                 text-height
                                                 (length text)))
           (bounds (geo:dimension-layout-bounds layout)))
      (require-bounds-in-range :dimension bounds)
      (%make-dimension id
                       layer-id
                       bounds
                       x1
                       y1
                       x2
                       y2
                       offset
                       text-height
                       unit
                       precision
                       text
                       layout))))

(defun update-dimension (dimension
                         &key (layer-id (entity-layer-id dimension))
                              (x1 (dimension-x1 dimension))
                              (y1 (dimension-y1 dimension))
                              (x2 (dimension-x2 dimension))
                              (y2 (dimension-y2 dimension))
                              (offset (dimension-offset dimension))
                              (text-height (dimension-text-height dimension))
                              (unit (dimension-unit dimension))
                              (precision (dimension-precision dimension)))
  "Return a validated copy of DIMENSION with the given fields replaced.

DIMENSION: the dimension to copy.
LAYER-ID: replacement layer id.
X1: replacement x of the first point.
Y1: replacement y of the first point.
X2: replacement x of the second point.
Y2: replacement y of the second point.
OFFSET: replacement offset.
TEXT-HEIGHT: replacement text height.
UNIT: replacement display unit.
PRECISION: replacement decimals."
  (make-dimension :id (entity-id dimension)
                  :layer-id layer-id
                  :x1 x1
                  :y1 y1
                  :x2 x2
                  :y2 y2
                  :offset offset
                  :text-height text-height
                  :unit unit
                  :precision precision))

(defun dimension-measured-length (dimension)
  "Return the measured length of DIMENSION in millimetres (derived, never stored).

DIMENSION: a dimension entity."
  (geo:distance (dimension-x1 dimension)
                (dimension-y1 dimension)
                (dimension-x2 dimension)
                (dimension-y2 dimension)))

(defmethod entity-kind ((dimension dimension-entity))
  :dimension)

(defmethod entity-pick-distance ((dimension dimension-entity)
                                 x
                                 y)
  (let* ((layout (dimension-layout dimension))
         (line (geo:dimension-layout-dimension-line layout))
         (extensions (geo:dimension-layout-extension-lines layout)))
    (min (geo:point-segment-distance x
                                     y
                                     (aref line 0)
                                     (aref line 1)
                                     (aref line 2)
                                     (aref line 3))
         (geo:point-segment-distance x
                                     y
                                     (aref extensions 0)
                                     (aref extensions 1)
                                     (aref extensions 2)
                                     (aref extensions 3))
         (geo:point-segment-distance x
                                     y
                                     (aref extensions 4)
                                     (aref extensions 5)
                                     (aref extensions 6)
                                     (aref extensions 7))
         (geo:point-oriented-box-distance x
                                          y
                                          (geo:dimension-layout-text-x layout)
                                          (geo:dimension-layout-text-y layout)
                                          (geo:dimension-layout-text-width layout)
                                          (* geo:+text-ascent-ratio+ (dimension-text-height dimension))
                                          (* geo:+text-descent-ratio+ (dimension-text-height dimension))
                                          (geo:dimension-layout-text-angle layout)))))

(defmethod entity-translated ((dimension dimension-entity)
                              dx
                              dy)
  (update-dimension dimension
                    :x1 (+ (dimension-x1 dimension) dx)
                    :y1 (+ (dimension-y1 dimension) dy)
                    :x2 (+ (dimension-x2 dimension) dx)
                    :y2 (+ (dimension-y2 dimension) dy)))

(defmethod entity-snap-points ((dimension dimension-entity))
  (list (list (dimension-x1 dimension) (dimension-y1 dimension) :endpoint)
        (list (dimension-x2 dimension) (dimension-y2 dimension) :endpoint)))

(defmethod entity-properties ((dimension dimension-entity))
  (append (coordinate-properties (list (list :x1 (i18n:mark "Point 1 X"))
                                               (list :y1 (i18n:mark "Point 1 Y"))
                                               (list :x2 (i18n:mark "Point 2 X"))
                                               (list :y2 (i18n:mark "Point 2 Y")))
                                 (list (dimension-x1 dimension) (dimension-y1 dimension)
                                       (dimension-x2 dimension) (dimension-y2 dimension)))
          (list (make-property :offset
                               (i18n:tr "Offset")
                               :length
                               (dimension-offset dimension)
                               t)
                (make-property :text-height
                               (i18n:tr "Text height")
                               :length
                               (dimension-text-height dimension)
                               t)
                (make-property :unit
                               (i18n:tr "Unit")
                               :unit
                               (dimension-unit dimension)
                               t)
                (make-property :precision
                               (i18n:tr "Decimals")
                               :integer
                               (dimension-precision dimension)
                               t))))

(defmethod entity-derived-values ((dimension dimension-entity))
  (list (make-derived-value (i18n:tr "Measured length")
                            :length
                            (dimension-measured-length dimension))))

(defmethod entity-with-property ((dimension dimension-entity)
                                 key
                                 value)
  (ecase key
    (:x1 (update-dimension dimension :x1 value))
    (:y1 (update-dimension dimension :y1 value))
    (:x2 (update-dimension dimension :x2 value))
    (:y2 (update-dimension dimension :y2 value))
    (:offset (update-dimension dimension :offset value))
    (:text-height (update-dimension dimension :text-height value))
    (:unit (update-dimension dimension :unit value))
    (:precision (update-dimension dimension :precision value))))

(defmethod entity-with-layer ((dimension dimension-entity)
                              layer-id)
  (update-dimension dimension :layer-id layer-id))
