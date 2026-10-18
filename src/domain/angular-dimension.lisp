(in-package #:borba-cad/domain)

(defparameter *angle-degree-sign* (string (code-char 176))
  "The degree sign that ends the text of an angle dimension, built from its code point to keep sources ASCII.")

(defstruct (angular-dimension-entity (:include entity)
                                     (:conc-name angular-dimension-)
                                     (:constructor %make-angular-dimension (id
                                                                            layer-id
                                                                            bounds
                                                                            vertex-x
                                                                            vertex-y
                                                                            start-angle
                                                                            end-angle
                                                                            radius
                                                                            text-height
                                                                            precision
                                                                            sweep
                                                                            text
                                                                            layout))
                                     (:copier nil))
  "An angle dimension: the angle at a vertex between two arms, drawn as an arc of a given radius from the first arm
to the second, counter-clockwise. The value is derived from the arms.

VERTEX-X: x of the vertex.
VERTEX-Y: y of the vertex.
START-ANGLE: direction of the first arm, in radians counter-clockwise from +x, in [0, 2 pi).
END-ANGLE: direction of the second arm, in [0, 2 pi).
RADIUS: radius of the dimension arc.
TEXT-HEIGHT: text height (em size); every part of the dimension scales with it.
PRECISION: decimals of the angle in degrees.
SWEEP: the angle measured, in (0, 2 pi); computed from the arms.
TEXT: the displayed text, computed at construction.
LAYOUT: the geometric layout of all parts, computed at construction."
  (vertex-x 0d0 :type double-float :read-only t)
  (vertex-y 0d0 :type double-float :read-only t)
  (start-angle 0d0 :type double-float :read-only t)
  (end-angle 0d0 :type double-float :read-only t)
  (radius 1d0 :type double-float :read-only t)
  (text-height 1d0 :type double-float :read-only t)
  (precision 0 :type (integer 0) :read-only t)
  (sweep 0d0 :type double-float :read-only t)
  (text "" :type string :read-only t)
  (layout nil :type (or null geo:annotation-layout) :read-only t))

(defun angular-dimension-text-for (sweep
                                   precision)
  "Return the text an angle dimension shows: the angle in degrees.

SWEEP: the angle, in radians.
PRECISION: decimals."
  (concatenate 'string
               (base:format-decimal (base:radians->degrees sweep) precision)
               *angle-degree-sign*))

(defun make-angular-dimension (&key id
                                    layer-id
                                    vertex-x
                                    vertex-y
                                    start-angle
                                    end-angle
                                    radius
                                    text-height
                                    precision)
  "Create a validated angle dimension. Signals INVALID-ENTITY when an invariant is violated.

ID: positive entity id.
LAYER-ID: id of the layer.
VERTEX-X: x of the vertex, in millimetres.
VERTEX-Y: y of the vertex.
START-ANGLE: direction of the first arm in radians; any finite angle, kept reduced to [0, 2 pi).
END-ANGLE: direction of the second arm; the angle is measured counter-clockwise from the first arm, and the
two must differ.
RADIUS: radius of the dimension arc, above the length tolerance.
TEXT-HEIGHT: text height (em size), above the length tolerance.
PRECISION: decimals displayed, 0 to +MAXIMUM-DIMENSION-PRECISION+."
  (require-identifiers :angular-dimension
                       id
                       layer-id)
  (require-coordinate :angular-dimension
                      "vertex-x"
                      vertex-x)
  (require-coordinate :angular-dimension
                      "vertex-y"
                      vertex-y)
  (require-angle :angular-dimension
                 "start-angle"
                 start-angle)
  (require-angle :angular-dimension
                 "end-angle"
                 end-angle)
  (require-positive-length :angular-dimension
                           "radius"
                           radius)
  (require-positive-length :angular-dimension
                           "text height"
                           text-height)
  (unless (typep precision `(integer 0 ,+maximum-dimension-precision+))
    (invalid-entity-failure :angular-dimension
                            "its precision must be between 0 and ~d decimals, got ~s"
                            +maximum-dimension-precision+
                            precision))
  (let* ((start (geo:normalize-angle start-angle))
         (end (geo:normalize-angle end-angle))
         (sweep (geo:arc-sweep start end)))
    (unless (and (> sweep base:+angle-tolerance+)
                 (< sweep (- geo:+two-pi+ base:+angle-tolerance+)))
      (invalid-entity-failure :angular-dimension "its two arms must point in different directions"))
    (let* ((text (angular-dimension-text-for sweep precision))
           (layout (geo:compute-angular-dimension-layout vertex-x
                                                         vertex-y
                                                         start
                                                         end
                                                         radius
                                                         text-height
                                                         (length text)))
           (bounds (geo:annotation-layout-bounds layout)))
      (require-bounds-in-range :angular-dimension bounds)
      (%make-angular-dimension id
                               layer-id
                               bounds
                               vertex-x
                               vertex-y
                               start
                               end
                               radius
                               text-height
                               precision
                               sweep
                               text
                               layout))))

(defun update-angular-dimension (dimension
                                 &key (id (entity-id dimension))
                                      (layer-id (entity-layer-id dimension))
                                      (vertex-x (angular-dimension-vertex-x dimension))
                                      (vertex-y (angular-dimension-vertex-y dimension))
                                      (start-angle (angular-dimension-start-angle dimension))
                                      (end-angle (angular-dimension-end-angle dimension))
                                      (radius (angular-dimension-radius dimension))
                                      (text-height (angular-dimension-text-height dimension))
                                      (precision (angular-dimension-precision dimension)))
  "Return a validated copy of DIMENSION with the given fields replaced.

DIMENSION: the dimension to copy.
ID: replacement id.
LAYER-ID: replacement layer id.
VERTEX-X: replacement vertex x.
VERTEX-Y: replacement vertex y.
START-ANGLE: replacement direction of the first arm.
END-ANGLE: replacement direction of the second arm.
RADIUS: replacement arc radius.
TEXT-HEIGHT: replacement text height.
PRECISION: replacement decimals."
  (make-angular-dimension :id id
                          :layer-id layer-id
                          :vertex-x vertex-x
                          :vertex-y vertex-y
                          :start-angle start-angle
                          :end-angle end-angle
                          :radius radius
                          :text-height text-height
                          :precision precision))

(defmethod entity-kind ((dimension angular-dimension-entity))
  :angular-dimension)

(defmethod entity-pick-distance ((dimension angular-dimension-entity)
                                 x
                                 y)
  (geo:annotation-pick-distance (angular-dimension-layout dimension)
                                x
                                y))

(defmethod entity-touches-bounds-p ((dimension angular-dimension-entity)
                                    bounds)
  (geo:annotation-touches-bounds-p (angular-dimension-layout dimension)
                                   bounds))

(defmethod entity-translated ((dimension angular-dimension-entity)
                              dx
                              dy)
  (update-angular-dimension dimension
                            :vertex-x (+ (angular-dimension-vertex-x dimension) dx)
                            :vertex-y (+ (angular-dimension-vertex-y dimension) dy)))

(defun angular-dimension-arc-point (dimension
                                    angle)
  "Return the point of the dimension arc in a direction, as two values.

DIMENSION: an angular dimension entity.
ANGLE: direction from the vertex, in radians."
  (values (+ (angular-dimension-vertex-x dimension)
             (* (angular-dimension-radius dimension) (cos angle)))
          (+ (angular-dimension-vertex-y dimension)
             (* (angular-dimension-radius dimension) (sin angle)))))

(defmethod entity-snap-points ((dimension angular-dimension-entity))
  (multiple-value-bind (start-x start-y) (angular-dimension-arc-point dimension
                                                                      (angular-dimension-start-angle dimension))
    (multiple-value-bind (end-x end-y) (angular-dimension-arc-point dimension
                                                                    (angular-dimension-end-angle dimension))
      (list (list (angular-dimension-vertex-x dimension) (angular-dimension-vertex-y dimension) :endpoint)
            (list start-x start-y :endpoint)
            (list end-x end-y :endpoint)))))

(defmethod entity-properties ((dimension angular-dimension-entity))
  (append (coordinate-properties (list (list :vertex-x (i18n:mark "Vertex X"))
                                       (list :vertex-y (i18n:mark "Vertex Y")))
                                 (list (angular-dimension-vertex-x dimension)
                                       (angular-dimension-vertex-y dimension)))
          (list (make-property :start-angle
                               (i18n:tr "Start angle")
                               :angle
                               (angular-dimension-start-angle dimension)
                               t)
                (make-property :end-angle
                               (i18n:tr "End angle")
                               :angle
                               (angular-dimension-end-angle dimension)
                               t)
                (make-property :radius
                               (i18n:tr "Radius")
                               :length
                               (angular-dimension-radius dimension)
                               t)
                (make-property :text-height
                               (i18n:tr "Text height")
                               :length
                               (angular-dimension-text-height dimension)
                               t)
                (make-property :precision
                               (i18n:tr "Decimals")
                               :integer
                               (angular-dimension-precision dimension)
                               t))))

(defmethod entity-derived-values ((dimension angular-dimension-entity))
  (list (make-derived-value (i18n:tr "Angle")
                            :angle
                            (angular-dimension-sweep dimension))))

(defmethod entity-with-property ((dimension angular-dimension-entity)
                                 key
                                 value)
  (ecase key
    (:vertex-x (update-angular-dimension dimension :vertex-x value))
    (:vertex-y (update-angular-dimension dimension :vertex-y value))
    (:start-angle (update-angular-dimension dimension :start-angle value))
    (:end-angle (update-angular-dimension dimension :end-angle value))
    (:radius (update-angular-dimension dimension :radius value))
    (:text-height (update-angular-dimension dimension :text-height value))
    (:precision (update-angular-dimension dimension :precision value))))

(defmethod entity-with-layer ((dimension angular-dimension-entity)
                              layer-id)
  (update-angular-dimension dimension :layer-id layer-id))

(defmethod entity-with-identity ((dimension angular-dimension-entity)
                                 id
                                 layer-id)
  (update-angular-dimension dimension
                            :id id
                            :layer-id layer-id))

(defmethod entity-grips ((dimension angular-dimension-entity))
  (let ((middle (+ (angular-dimension-start-angle dimension) (/ (angular-dimension-sweep dimension) 2))))
    (multiple-value-bind (start-x start-y) (angular-dimension-arc-point dimension
                                                                        (angular-dimension-start-angle dimension))
      (multiple-value-bind (end-x end-y) (angular-dimension-arc-point dimension
                                                                      (angular-dimension-end-angle dimension))
        (multiple-value-bind (middle-x middle-y) (angular-dimension-arc-point dimension middle)
          (list (make-grip :start
                           start-x
                           start-y
                           :point)
                (make-grip :end
                           end-x
                           end-y
                           :point)
                (make-grip :radius
                           middle-x
                           middle-y
                           :point)
                (make-grip :move
                           (angular-dimension-vertex-x dimension)
                           (angular-dimension-vertex-y dimension)
                           :move)))))))

(defmethod entity-with-grip ((dimension angular-dimension-entity)
                             grip
                             x
                             y)
  (let ((vertex-x (angular-dimension-vertex-x dimension))
        (vertex-y (angular-dimension-vertex-y dimension)))
    (ecase (grip-id grip)
      (:move (moved-by-grip dimension
                            grip
                            x
                            y))
      (:start (update-angular-dimension dimension :start-angle (atan (- y vertex-y) (- x vertex-x))))
      (:end (update-angular-dimension dimension :end-angle (atan (- y vertex-y) (- x vertex-x))))
      (:radius (update-angular-dimension dimension
                                         :radius (geo:distance vertex-x
                                                               vertex-y
                                                               x
                                                               y))))))
