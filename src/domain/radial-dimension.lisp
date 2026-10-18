(in-package #:borba-cad/domain)

(defparameter *diameter-sign* (string (code-char 216))
  "The sign that starts the text of a diameter dimension, built from its code point to keep sources ASCII.")

(defparameter *radius-sign* "R"
  "The sign that starts the text of a radius dimension.")

(defstruct (radial-dimension-entity (:include entity)
                                    (:conc-name radial-dimension-)
                                    (:constructor %make-radial-dimension (id
                                                                          layer-id
                                                                          bounds
                                                                          center-x
                                                                          center-y
                                                                          radius
                                                                          angle
                                                                          diameter
                                                                          text-height
                                                                          unit
                                                                          precision
                                                                          text
                                                                          layout))
                                    (:copier nil))
  "A radius or diameter dimension of a circle or an arc: a leader from the center through the circle at an angle,
and the measured value. The value is derived from the radius; the text is derived from it, the unit and the
precision.

CENTER-X: x of the center of the circle.
CENTER-Y: y of the center of the circle.
RADIUS: radius of the circle.
ANGLE: direction of the leader from the center, in radians counter-clockwise from +x, in [0, 2 pi).
DIAMETER: true for a diameter dimension, false for a radius dimension.
TEXT-HEIGHT: text height (em size); every part of the dimension scales with it.
UNIT: length unit keyword of the displayed text.
PRECISION: decimals in the displayed text.
TEXT: the displayed text, computed at construction.
LAYOUT: the geometric layout of all parts, computed at construction."
  (center-x 0d0 :type double-float :read-only t)
  (center-y 0d0 :type double-float :read-only t)
  (radius 1d0 :type double-float :read-only t)
  (angle 0d0 :type double-float :read-only t)
  (diameter nil :type boolean :read-only t)
  (text-height 1d0 :type double-float :read-only t)
  (unit :millimeter :type keyword :read-only t)
  (precision 0 :type (integer 0) :read-only t)
  (text "" :type string :read-only t)
  (layout nil :type (or null geo:annotation-layout) :read-only t))

(defun radial-dimension-text-for (radius
                                  diameter
                                  unit
                                  precision)
  "Return the text a radius or diameter dimension shows: the sign and the value.

RADIUS: radius of the circle.
DIAMETER: true to show the diameter.
UNIT: length unit keyword.
PRECISION: decimals."
  (concatenate 'string
               (if diameter *diameter-sign* *radius-sign*)
               (base:format-length (if diameter (* 2 radius) radius)
                                   unit
                                   :precision precision)))

(defun make-radial-dimension (&key id
                                   layer-id
                                   center-x
                                   center-y
                                   radius
                                   angle
                                   diameter
                                   text-height
                                   unit
                                   precision)
  "Create a validated radius or diameter dimension. Signals INVALID-ENTITY when an invariant is violated.

ID: positive entity id.
LAYER-ID: id of the layer.
CENTER-X: x of the center, in millimetres.
CENTER-Y: y of the center.
RADIUS: radius, above the length tolerance.
ANGLE: direction of the leader in radians; any finite angle, kept reduced to [0, 2 pi).
DIAMETER: true for a diameter dimension.
TEXT-HEIGHT: text height (em size), above the length tolerance.
UNIT: displayed length unit keyword.
PRECISION: decimals displayed, 0 to +MAXIMUM-DIMENSION-PRECISION+."
  (require-identifiers :radial-dimension
                       id
                       layer-id)
  (require-coordinate :radial-dimension
                      "center-x"
                      center-x)
  (require-coordinate :radial-dimension
                      "center-y"
                      center-y)
  (require-positive-length :radial-dimension
                           "radius"
                           radius)
  (require-angle :radial-dimension
                 "angle"
                 angle)
  (require-positive-length :radial-dimension
                           "text height"
                           text-height)
  (unless (base:length-unit-p unit)
    (invalid-entity-failure :radial-dimension
                            "its unit must be one of ~{~(~a~)~^, ~}, got ~s"
                            (base:supported-length-units)
                            unit))
  (unless (typep precision `(integer 0 ,+maximum-dimension-precision+))
    (invalid-entity-failure :radial-dimension
                            "its precision must be between 0 and ~d decimals, got ~s"
                            +maximum-dimension-precision+
                            precision))
  (let* ((leader (geo:normalize-angle angle))
         (flag (and diameter t))
         (text (radial-dimension-text-for radius
                                          flag
                                          unit
                                          precision))
         (layout (geo:compute-radial-dimension-layout center-x
                                                      center-y
                                                      radius
                                                      leader
                                                      flag
                                                      text-height
                                                      (length text)))
         (bounds (geo:annotation-layout-bounds layout)))
    (require-bounds-in-range :radial-dimension bounds)
    (%make-radial-dimension id
                            layer-id
                            bounds
                            center-x
                            center-y
                            radius
                            leader
                            flag
                            text-height
                            unit
                            precision
                            text
                            layout)))

(defun update-radial-dimension (dimension
                                &key (id (entity-id dimension))
                                     (layer-id (entity-layer-id dimension))
                                     (center-x (radial-dimension-center-x dimension))
                                     (center-y (radial-dimension-center-y dimension))
                                     (radius (radial-dimension-radius dimension))
                                     (angle (radial-dimension-angle dimension))
                                     (diameter (radial-dimension-diameter dimension))
                                     (text-height (radial-dimension-text-height dimension))
                                     (unit (radial-dimension-unit dimension))
                                     (precision (radial-dimension-precision dimension)))
  "Return a validated copy of DIMENSION with the given fields replaced.

DIMENSION: the dimension to copy.
ID: replacement id.
LAYER-ID: replacement layer id.
CENTER-X: replacement center x.
CENTER-Y: replacement center y.
RADIUS: replacement radius.
ANGLE: replacement leader direction.
DIAMETER: replacement diameter flag.
TEXT-HEIGHT: replacement text height.
UNIT: replacement display unit.
PRECISION: replacement decimals."
  (make-radial-dimension :id id
                         :layer-id layer-id
                         :center-x center-x
                         :center-y center-y
                         :radius radius
                         :angle angle
                         :diameter diameter
                         :text-height text-height
                         :unit unit
                         :precision precision))

(defmethod entity-kind ((dimension radial-dimension-entity))
  :radial-dimension)

(defmethod entity-pick-distance ((dimension radial-dimension-entity)
                                 x
                                 y)
  (geo:annotation-pick-distance (radial-dimension-layout dimension)
                                x
                                y))

(defmethod entity-touches-bounds-p ((dimension radial-dimension-entity)
                                    bounds)
  (geo:annotation-touches-bounds-p (radial-dimension-layout dimension)
                                   bounds))

(defmethod entity-translated ((dimension radial-dimension-entity)
                              dx
                              dy)
  (update-radial-dimension dimension
                           :center-x (+ (radial-dimension-center-x dimension) dx)
                           :center-y (+ (radial-dimension-center-y dimension) dy)))

(defun radial-dimension-edge (dimension)
  "Return the point where the leader of DIMENSION meets the circle, as two values.

DIMENSION: a radial dimension entity."
  (values (+ (radial-dimension-center-x dimension)
             (* (radial-dimension-radius dimension) (cos (radial-dimension-angle dimension))))
          (+ (radial-dimension-center-y dimension)
             (* (radial-dimension-radius dimension) (sin (radial-dimension-angle dimension))))))

(defmethod entity-snap-points ((dimension radial-dimension-entity))
  (multiple-value-bind (edge-x edge-y) (radial-dimension-edge dimension)
    (list (list (radial-dimension-center-x dimension) (radial-dimension-center-y dimension) :center)
          (list edge-x edge-y :endpoint))))

(defmethod entity-properties ((dimension radial-dimension-entity))
  (append (coordinate-properties (list (list :center-x (i18n:mark "Center X"))
                                       (list :center-y (i18n:mark "Center Y")))
                                 (list (radial-dimension-center-x dimension)
                                       (radial-dimension-center-y dimension)))
          (list (make-property :radius
                               (i18n:tr "Radius")
                               :length
                               (radial-dimension-radius dimension)
                               t)
                (make-property :angle
                               (i18n:tr "Angle")
                               :angle
                               (radial-dimension-angle dimension)
                               t)
                (make-property :diameter
                               (i18n:tr "Diameter")
                               :boolean
                               (radial-dimension-diameter dimension)
                               t)
                (make-property :text-height
                               (i18n:tr "Text height")
                               :length
                               (radial-dimension-text-height dimension)
                               t)
                (make-property :unit
                               (i18n:tr "Unit")
                               :unit
                               (radial-dimension-unit dimension)
                               t)
                (make-property :precision
                               (i18n:tr "Decimals")
                               :integer
                               (radial-dimension-precision dimension)
                               t))))

(defmethod entity-derived-values ((dimension radial-dimension-entity))
  (list (make-derived-value (i18n:tr "Measured length")
                            :length
                            (if (radial-dimension-diameter dimension)
                                (* 2 (radial-dimension-radius dimension))
                                (radial-dimension-radius dimension)))))

(defmethod entity-with-property ((dimension radial-dimension-entity)
                                 key
                                 value)
  (ecase key
    (:center-x (update-radial-dimension dimension :center-x value))
    (:center-y (update-radial-dimension dimension :center-y value))
    (:radius (update-radial-dimension dimension :radius value))
    (:angle (update-radial-dimension dimension :angle value))
    (:diameter (update-radial-dimension dimension :diameter value))
    (:text-height (update-radial-dimension dimension :text-height value))
    (:unit (update-radial-dimension dimension :unit value))
    (:precision (update-radial-dimension dimension :precision value))))

(defmethod entity-with-layer ((dimension radial-dimension-entity)
                              layer-id)
  (update-radial-dimension dimension :layer-id layer-id))

(defmethod entity-with-identity ((dimension radial-dimension-entity)
                                 id
                                 layer-id)
  (update-radial-dimension dimension
                           :id id
                           :layer-id layer-id))

(defmethod entity-grips ((dimension radial-dimension-entity))
  (multiple-value-bind (edge-x edge-y) (radial-dimension-edge dimension)
    (list (make-grip :direction
                     edge-x
                     edge-y
                     :point)
          (make-grip :move
                     (radial-dimension-center-x dimension)
                     (radial-dimension-center-y dimension)
                     :move))))

(defmethod entity-with-grip ((dimension radial-dimension-entity)
                             grip
                             x
                             y)
  (if (eq (grip-id grip) :move)
      (moved-by-grip dimension
                     grip
                     x
                     y)
      (update-radial-dimension dimension
                               :angle (atan (- y (radial-dimension-center-y dimension))
                                            (- x (radial-dimension-center-x dimension))))))
