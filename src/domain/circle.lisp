(in-package #:borba-cad/domain)

(defstruct (circle-entity (:include entity)
                          (:conc-name circle-)
                          (:constructor %make-circle (id
                                                      layer-id
                                                      bounds
                                                      center-x
                                                      center-y
                                                      radius))
                          (:copier nil))
  "A circle.

CENTER-X: x of the center.
CENTER-Y: y of the center.
RADIUS: radius, above the length tolerance."
  (center-x 0d0 :type double-float :read-only t)
  (center-y 0d0 :type double-float :read-only t)
  (radius 1d0 :type double-float :read-only t))

(defun make-circle (&key id
                         layer-id
                         center-x
                         center-y
                         radius)
  "Create a validated circle. Signals INVALID-ENTITY when an invariant is violated.

ID: positive entity id.
LAYER-ID: id of the layer.
CENTER-X: x of the center, in millimetres.
CENTER-Y: y of the center.
RADIUS: radius."
  (require-identifiers :circle
                       id
                       layer-id)
  (require-coordinate :circle
                      "center-x"
                      center-x)
  (require-coordinate :circle
                      "center-y"
                      center-y)
  (require-positive-length :circle
                           "radius"
                           radius)
  (let ((bounds (geo:circle-bounds center-x
                                   center-y
                                   radius)))
    (require-bounds-in-range :circle bounds)
    (%make-circle id
                  layer-id
                  bounds
                  center-x
                  center-y
                  radius)))

(defun update-circle (circle
                      &key (id (entity-id circle))
                           (layer-id (entity-layer-id circle))
                           (center-x (circle-center-x circle))
                           (center-y (circle-center-y circle))
                           (radius (circle-radius circle)))
  "Return a validated copy of CIRCLE with the given fields replaced.

CIRCLE: the circle to copy.
ID: replacement id.
LAYER-ID: replacement layer id.
CENTER-X: replacement center x.
CENTER-Y: replacement center y.
RADIUS: replacement radius."
  (make-circle :id id
               :layer-id layer-id
               :center-x center-x
               :center-y center-y
               :radius radius))

(defmethod entity-kind ((circle circle-entity))
  :circle)

(defmethod entity-pick-distance ((circle circle-entity)
                                 x
                                 y)
  (geo:point-circle-outline-distance x
                                     y
                                     (circle-center-x circle)
                                     (circle-center-y circle)
                                     (circle-radius circle)))

(defmethod entity-translated ((circle circle-entity)
                              dx
                              dy)
  (update-circle circle
                 :center-x (+ (circle-center-x circle) dx)
                 :center-y (+ (circle-center-y circle) dy)))

(defmethod entity-snap-points ((circle circle-entity))
  (list (list (circle-center-x circle) (circle-center-y circle) :center)))

(defmethod entity-properties ((circle circle-entity))
  (list (make-property :center-x
                       (i18n:tr "Center X")
                       :coordinate
                       (circle-center-x circle)
                       t)
        (make-property :center-y
                       (i18n:tr "Center Y")
                       :coordinate
                       (circle-center-y circle)
                       t)
        (make-property :radius
                       (i18n:tr "Radius")
                       :length
                       (circle-radius circle)
                       t)))

(defmethod entity-derived-values ((circle circle-entity))
  (list (make-derived-value (i18n:tr "Diameter")
                            :length
                            (* 2d0 (circle-radius circle)))
        (make-derived-value (i18n:tr "Circumference")
                            :length
                            (geo:circle-circumference (circle-radius circle)))
        (make-derived-value (i18n:tr "Area")
                            :area
                            (geo:circle-area (circle-radius circle)))))

(defmethod entity-with-property ((circle circle-entity)
                                 key
                                 value)
  (ecase key
    (:center-x (update-circle circle :center-x value))
    (:center-y (update-circle circle :center-y value))
    (:radius (update-circle circle :radius value))))

(defmethod entity-with-layer ((circle circle-entity)
                              layer-id)
  (update-circle circle :layer-id layer-id))

(defmethod entity-with-identity ((circle circle-entity)
                                 id
                                 layer-id)
  (update-circle circle
                 :id id
                 :layer-id layer-id))
