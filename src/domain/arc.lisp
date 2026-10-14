(in-package #:borba-cad/domain)

(defstruct (arc-entity (:include entity)
                       (:conc-name arc-)
                       (:constructor %make-arc (id
                                                layer-id
                                                bounds
                                                center-x
                                                center-y
                                                radius
                                                start-angle
                                                end-angle
                                                sweep))
                       (:copier nil))
  "An arc: the part of a circle that is swept counter-clockwise from a start angle to an end angle.

CENTER-X: x of the center.
CENTER-Y: y of the center.
RADIUS: radius, above the length tolerance.
START-ANGLE: direction of the start, in radians counter-clockwise from +x, in [0, 2 pi).
END-ANGLE: direction of the end, in [0, 2 pi).
SWEEP: how far the arc turns from its start to its end, in (0, 2 pi); computed from the angles."
  (center-x 0d0 :type double-float :read-only t)
  (center-y 0d0 :type double-float :read-only t)
  (radius 1d0 :type double-float :read-only t)
  (start-angle 0d0 :type double-float :read-only t)
  (end-angle 0d0 :type double-float :read-only t)
  (sweep 0d0 :type double-float :read-only t))

(defun make-arc (&key id
                      layer-id
                      center-x
                      center-y
                      radius
                      start-angle
                      end-angle)
  "Create a validated arc. Signals INVALID-ENTITY when an invariant is violated.

ID: positive entity id.
LAYER-ID: id of the layer.
CENTER-X: x of the center, in millimetres.
CENTER-Y: y of the center.
RADIUS: radius.
START-ANGLE: direction of the start in radians; any finite angle, kept reduced to [0, 2 pi).
END-ANGLE: direction of the end; the arc turns counter-clockwise from the start to it, and the
two must differ."
  (require-identifiers :arc
                       id
                       layer-id)
  (require-coordinate :arc
                      "center-x"
                      center-x)
  (require-coordinate :arc
                      "center-y"
                      center-y)
  (require-positive-length :arc
                           "radius"
                           radius)
  (require-angle :arc
                 "start-angle"
                 start-angle)
  (require-angle :arc
                 "end-angle"
                 end-angle)
  (let* ((start (geo:normalize-angle start-angle))
         (end (geo:normalize-angle end-angle))
         (sweep (geo:arc-sweep start end)))
    (unless (and (> sweep base:+angle-tolerance+)
                 (< sweep (- geo:+two-pi+ base:+angle-tolerance+)))
      (invalid-entity-failure :arc "its start and end angles must differ"))
    (let ((bounds (geo:arc-bounds center-x
                                  center-y
                                  radius
                                  start
                                  sweep)))
      (require-bounds-in-range :arc bounds)
      (%make-arc id
                 layer-id
                 bounds
                 center-x
                 center-y
                 radius
                 start
                 end
                 sweep))))

(defun update-arc (arc
                   &key (id (entity-id arc))
                        (layer-id (entity-layer-id arc))
                        (center-x (arc-center-x arc))
                        (center-y (arc-center-y arc))
                        (radius (arc-radius arc))
                        (start-angle (arc-start-angle arc))
                        (end-angle (arc-end-angle arc)))
  "Return a validated copy of ARC with the given fields replaced.

ARC: the arc to copy.
ID: replacement id.
LAYER-ID: replacement layer id.
CENTER-X: replacement center x.
CENTER-Y: replacement center y.
RADIUS: replacement radius.
START-ANGLE: replacement start angle.
END-ANGLE: replacement end angle."
  (make-arc :id id
            :layer-id layer-id
            :center-x center-x
            :center-y center-y
            :radius radius
            :start-angle start-angle
            :end-angle end-angle))

(defmethod entity-kind ((arc arc-entity))
  :arc)

(defmethod entity-pick-distance ((arc arc-entity)
                                 x
                                 y)
  (geo:point-arc-distance x
                          y
                          (arc-center-x arc)
                          (arc-center-y arc)
                          (arc-radius arc)
                          (arc-start-angle arc)
                          (arc-sweep arc)))

(defmethod entity-translated ((arc arc-entity)
                              dx
                              dy)
  (update-arc arc
              :center-x (+ (arc-center-x arc) dx)
              :center-y (+ (arc-center-y arc) dy)))

(defmethod entity-snap-points ((arc arc-entity))
  (let ((center-x (arc-center-x arc))
        (center-y (arc-center-y arc))
        (radius (arc-radius arc)))
    (multiple-value-bind (start-x start-y)
        (geo:arc-end-point center-x
                           center-y
                           radius
                           (arc-start-angle arc))
      (multiple-value-bind (end-x end-y)
          (geo:arc-end-point center-x
                             center-y
                             radius
                             (arc-end-angle arc))
        (list (list center-x center-y :center)
              (list start-x start-y :endpoint)
              (list end-x end-y :endpoint))))))

(defmethod entity-properties ((arc arc-entity))
  (list (make-property :center-x
                       (i18n:tr "Center X")
                       :coordinate
                       (arc-center-x arc)
                       t)
        (make-property :center-y
                       (i18n:tr "Center Y")
                       :coordinate
                       (arc-center-y arc)
                       t)
        (make-property :radius
                       (i18n:tr "Radius")
                       :length
                       (arc-radius arc)
                       t)
        (make-property :start-angle
                       (i18n:tr "Start angle")
                       :angle
                       (arc-start-angle arc)
                       t)
        (make-property :end-angle
                       (i18n:tr "End angle")
                       :angle
                       (arc-end-angle arc)
                       t)))

(defmethod entity-derived-values ((arc arc-entity))
  (list (make-derived-value (i18n:tr "Length")
                            :length
                            (geo:arc-length (arc-radius arc) (arc-sweep arc)))
        (make-derived-value (i18n:tr "Sweep")
                            :angle
                            (arc-sweep arc))))

(defmethod entity-with-property ((arc arc-entity)
                                 key
                                 value)
  (ecase key
    (:center-x (update-arc arc :center-x value))
    (:center-y (update-arc arc :center-y value))
    (:radius (update-arc arc :radius value))
    (:start-angle (update-arc arc :start-angle value))
    (:end-angle (update-arc arc :end-angle value))))

(defmethod entity-with-layer ((arc arc-entity)
                              layer-id)
  (update-arc arc :layer-id layer-id))

(defmethod entity-with-identity ((arc arc-entity)
                                 id
                                 layer-id)
  (update-arc arc
              :id id
              :layer-id layer-id))

(defmethod entity-touches-bounds-p ((arc arc-entity)
                                    bounds)
  (geo:arc-touches-bounds-p (arc-center-x arc)
                            (arc-center-y arc)
                            (arc-radius arc)
                            (arc-start-angle arc)
                            (arc-sweep arc)
                            bounds))

(defmethod entity-grips ((arc arc-entity))
  (let ((center-x (arc-center-x arc))
        (center-y (arc-center-y arc))
        (radius (arc-radius arc)))
    (multiple-value-bind (start-x start-y)
        (geo:arc-end-point center-x
                           center-y
                           radius
                           (arc-start-angle arc))
      (multiple-value-bind (end-x end-y)
          (geo:arc-end-point center-x
                             center-y
                             radius
                             (arc-end-angle arc))
        (multiple-value-bind (middle-x middle-y)
            (geo:arc-end-point center-x
                               center-y
                               radius
                               (+ (arc-start-angle arc) (/ (arc-sweep arc) 2)))
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
                           center-x
                           center-y
                           :move)))))))

(defmethod entity-with-grip ((arc arc-entity)
                             grip
                             x
                             y)
  (let ((direction (atan (- y (arc-center-y arc)) (- x (arc-center-x arc)))))
    (ecase (grip-id grip)
      (:start (update-arc arc :start-angle direction))
      (:end (update-arc arc :end-angle direction))
      (:radius (update-arc arc :radius (geo:distance (arc-center-x arc)
                                                     (arc-center-y arc)
                                                     x
                                                     y)))
      (:move (moved-by-grip arc
                            grip
                            x
                            y)))))
