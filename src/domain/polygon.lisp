(in-package #:borba-cad/domain)

(defstruct (polygon-entity (:include entity)
                           (:conc-name polygon-)
                           (:constructor %make-polygon (id
                                                        layer-id
                                                        bounds
                                                        center-x
                                                        center-y
                                                        radius
                                                        sides
                                                        rotation
                                                        vertices))
                           (:copier nil))
  "A regular polygon, given by the circle through its vertices, its number of sides and the
direction of its first vertex.

CENTER-X: x of the center.
CENTER-Y: y of the center.
RADIUS: radius of the circle through the vertices, above the length tolerance.
SIDES: number of sides, from 3 to +MAXIMUM-POLYGON-SIDES+.
ROTATION: direction of the first vertex, in radians counter-clockwise from +x, in [0, 2 pi).
VERTICES: the vertices as alternating x and y values; computed once at construction."
  (center-x 0d0 :type double-float :read-only t)
  (center-y 0d0 :type double-float :read-only t)
  (radius 1d0 :type double-float :read-only t)
  (sides 3 :type (integer 3) :read-only t)
  (rotation 0d0 :type double-float :read-only t)
  (vertices nil :type (simple-array double-float (*)) :read-only t))

(defun make-polygon (&key id
                          layer-id
                          center-x
                          center-y
                          radius
                          sides
                          (rotation 0d0))
  "Create a validated regular polygon. Signals INVALID-ENTITY when an invariant is violated.

ID: positive entity id.
LAYER-ID: id of the layer.
CENTER-X: x of the center, in millimetres.
CENTER-Y: y of the center.
RADIUS: radius of the circle through the vertices.
SIDES: number of sides, an integer from 3 to +MAXIMUM-POLYGON-SIDES+.
ROTATION: direction of the first vertex in radians; any finite angle, kept reduced to [0, 2 pi)."
  (require-identifiers :polygon
                       id
                       layer-id)
  (require-coordinate :polygon
                      "center-x"
                      center-x)
  (require-coordinate :polygon
                      "center-y"
                      center-y)
  (require-positive-length :polygon
                           "radius"
                           radius)
  (require-angle :polygon
                 "rotation"
                 rotation)
  (unless (typep sides `(integer 3 ,+maximum-polygon-sides+))
    (invalid-entity-failure :polygon
                            "the number of sides must be a whole number from 3 to ~d, got ~a"
                            +maximum-polygon-sides+
                            sides))
  (let* ((turn (geo:normalize-angle rotation))
         (vertices (geo:regular-polygon-vertices center-x
                                                 center-y
                                                 radius
                                                 sides
                                                 turn))
         (bounds (geo:polyline-bounds vertices)))
    (require-bounds-in-range :polygon bounds)
    (%make-polygon id
                   layer-id
                   bounds
                   center-x
                   center-y
                   radius
                   sides
                   turn
                   vertices)))

(defun update-polygon (polygon
                       &key (id (entity-id polygon))
                            (layer-id (entity-layer-id polygon))
                            (center-x (polygon-center-x polygon))
                            (center-y (polygon-center-y polygon))
                            (radius (polygon-radius polygon))
                            (sides (polygon-sides polygon))
                            (rotation (polygon-rotation polygon)))
  "Return a validated copy of POLYGON with the given fields replaced.

POLYGON: the polygon to copy.
ID: replacement id.
LAYER-ID: replacement layer id.
CENTER-X: replacement center x.
CENTER-Y: replacement center y.
RADIUS: replacement radius.
SIDES: replacement number of sides.
ROTATION: replacement direction of the first vertex."
  (make-polygon :id id
                :layer-id layer-id
                :center-x center-x
                :center-y center-y
                :radius radius
                :sides sides
                :rotation rotation))

(defmethod entity-kind ((polygon polygon-entity))
  :polygon)

(defmethod entity-pick-distance ((polygon polygon-entity)
                                 x
                                 y)
  (geo:point-polyline-distance x
                               y
                               (polygon-vertices polygon)
                               t))

(defmethod entity-translated ((polygon polygon-entity)
                              dx
                              dy)
  (update-polygon polygon
                  :center-x (+ (polygon-center-x polygon) dx)
                  :center-y (+ (polygon-center-y polygon) dy)))

(defmethod entity-snap-points ((polygon polygon-entity))
  (let ((vertices (polygon-vertices polygon)))
    (cons (list (polygon-center-x polygon) (polygon-center-y polygon) :center)
          (loop for index from 0 below (length vertices) by 2
                collect (list (aref vertices index) (aref vertices (1+ index)) :endpoint)))))

(defmethod entity-properties ((polygon polygon-entity))
  (list (make-property :center-x
                       (i18n:tr "Center X")
                       :coordinate
                       (polygon-center-x polygon)
                       t)
        (make-property :center-y
                       (i18n:tr "Center Y")
                       :coordinate
                       (polygon-center-y polygon)
                       t)
        (make-property :radius
                       (i18n:tr "Radius")
                       :length
                       (polygon-radius polygon)
                       t)
        (make-property :sides
                       (i18n:tr "Sides")
                       :integer
                       (polygon-sides polygon)
                       t)
        (make-property :rotation
                       (i18n:tr "Rotation")
                       :angle
                       (polygon-rotation polygon)
                       t)))

(defmethod entity-derived-values ((polygon polygon-entity))
  (let ((side (geo:regular-polygon-side-length (polygon-radius polygon) (polygon-sides polygon))))
    (list (make-derived-value (i18n:tr "Side length")
                              :length
                              side)
          (make-derived-value (i18n:tr "Perimeter")
                              :length
                              (* side (polygon-sides polygon)))
          (make-derived-value (i18n:tr "Area")
                              :area
                              (geo:regular-polygon-area (polygon-radius polygon) (polygon-sides polygon))))))

(defmethod entity-with-property ((polygon polygon-entity)
                                 key
                                 value)
  (ecase key
    (:center-x (update-polygon polygon :center-x value))
    (:center-y (update-polygon polygon :center-y value))
    (:radius (update-polygon polygon :radius value))
    (:sides (update-polygon polygon :sides value))
    (:rotation (update-polygon polygon :rotation value))))

(defmethod entity-with-layer ((polygon polygon-entity)
                              layer-id)
  (update-polygon polygon :layer-id layer-id))

(defmethod entity-with-identity ((polygon polygon-entity)
                                 id
                                 layer-id)
  (update-polygon polygon
                  :id id
                  :layer-id layer-id))

(defmethod entity-touches-bounds-p ((polygon polygon-entity)
                                    bounds)
  (geo:polyline-touches-bounds-p (polygon-vertices polygon)
                                 t
                                 bounds))
