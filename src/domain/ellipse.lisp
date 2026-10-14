(in-package #:borba-cad/domain)

(defstruct (ellipse-entity (:include entity)
                           (:conc-name ellipse-)
                           (:constructor %make-ellipse (id
                                                        layer-id
                                                        bounds
                                                        center-x
                                                        center-y
                                                        radius-x
                                                        radius-y
                                                        rotation))
                           (:copier nil))
  "An ellipse, given by its center, its two semi-axes and the direction of the first.

CENTER-X: x of the center.
CENTER-Y: y of the center.
RADIUS-X: the semi-axis in the direction ROTATION, above the length tolerance.
RADIUS-Y: the semi-axis across it, above the length tolerance.
ROTATION: direction of the first axis, in radians counter-clockwise from +x, in [0, pi): an ellipse
is the same after half a turn."
  (center-x 0d0 :type double-float :read-only t)
  (center-y 0d0 :type double-float :read-only t)
  (radius-x 1d0 :type double-float :read-only t)
  (radius-y 1d0 :type double-float :read-only t)
  (rotation 0d0 :type double-float :read-only t))

(defun half-turn-angle (angle)
  "Return ANGLE reduced to [0, pi), the range of the rotation of a figure that is the same after
half a turn.

ANGLE: an angle in radians."
  (let ((reduced (mod angle (float pi 1d0))))
    (if (>= reduced (float pi 1d0)) 0d0 reduced)))

(defun make-ellipse (&key id
                          layer-id
                          center-x
                          center-y
                          radius-x
                          radius-y
                          (rotation 0d0))
  "Create a validated ellipse. Signals INVALID-ENTITY when an invariant is violated.

ID: positive entity id.
LAYER-ID: id of the layer.
CENTER-X: x of the center, in millimetres.
CENTER-Y: y of the center.
RADIUS-X: semi-axis in the direction ROTATION.
RADIUS-Y: semi-axis across it.
ROTATION: direction of the first axis in radians; any finite angle, kept reduced to [0, pi)."
  (require-identifiers :ellipse
                       id
                       layer-id)
  (require-coordinate :ellipse
                      "center-x"
                      center-x)
  (require-coordinate :ellipse
                      "center-y"
                      center-y)
  (require-positive-length :ellipse
                           "radius-x"
                           radius-x)
  (require-positive-length :ellipse
                           "radius-y"
                           radius-y)
  (require-angle :ellipse
                 "rotation"
                 rotation)
  (let* ((turn (half-turn-angle rotation))
         (bounds (geo:ellipse-bounds center-x
                                     center-y
                                     radius-x
                                     radius-y
                                     turn)))
    (require-bounds-in-range :ellipse bounds)
    (%make-ellipse id
                   layer-id
                   bounds
                   center-x
                   center-y
                   radius-x
                   radius-y
                   turn)))

(defun update-ellipse (ellipse
                       &key (id (entity-id ellipse))
                            (layer-id (entity-layer-id ellipse))
                            (center-x (ellipse-center-x ellipse))
                            (center-y (ellipse-center-y ellipse))
                            (radius-x (ellipse-radius-x ellipse))
                            (radius-y (ellipse-radius-y ellipse))
                            (rotation (ellipse-rotation ellipse)))
  "Return a validated copy of ELLIPSE with the given fields replaced.

ELLIPSE: the ellipse to copy.
ID: replacement id.
LAYER-ID: replacement layer id.
CENTER-X: replacement center x.
CENTER-Y: replacement center y.
RADIUS-X: replacement first semi-axis.
RADIUS-Y: replacement second semi-axis.
ROTATION: replacement direction of the first axis."
  (make-ellipse :id id
                :layer-id layer-id
                :center-x center-x
                :center-y center-y
                :radius-x radius-x
                :radius-y radius-y
                :rotation rotation))

(defmethod entity-kind ((ellipse ellipse-entity))
  :ellipse)

(defmethod entity-pick-distance ((ellipse ellipse-entity)
                                 x
                                 y)
  (geo:point-ellipse-distance x
                              y
                              (ellipse-center-x ellipse)
                              (ellipse-center-y ellipse)
                              (ellipse-radius-x ellipse)
                              (ellipse-radius-y ellipse)
                              (ellipse-rotation ellipse)))

(defmethod entity-translated ((ellipse ellipse-entity)
                              dx
                              dy)
  (update-ellipse ellipse
                  :center-x (+ (ellipse-center-x ellipse) dx)
                  :center-y (+ (ellipse-center-y ellipse) dy)))

(defmethod entity-snap-points ((ellipse ellipse-entity))
  (let* ((center-x (ellipse-center-x ellipse))
         (center-y (ellipse-center-y ellipse))
         (cosine (cos (ellipse-rotation ellipse)))
         (sine (sin (ellipse-rotation ellipse)))
         (along-x (* (ellipse-radius-x ellipse) cosine))
         (along-y (* (ellipse-radius-x ellipse) sine))
         (across-x (- (* (ellipse-radius-y ellipse) sine)))
         (across-y (* (ellipse-radius-y ellipse) cosine)))
    (list (list center-x center-y :center)
          (list (+ center-x along-x) (+ center-y along-y) :endpoint)
          (list (- center-x along-x) (- center-y along-y) :endpoint)
          (list (+ center-x across-x) (+ center-y across-y) :endpoint)
          (list (- center-x across-x) (- center-y across-y) :endpoint))))

(defmethod entity-properties ((ellipse ellipse-entity))
  (list (make-property :center-x
                       (i18n:tr "Center X")
                       :coordinate
                       (ellipse-center-x ellipse)
                       t)
        (make-property :center-y
                       (i18n:tr "Center Y")
                       :coordinate
                       (ellipse-center-y ellipse)
                       t)
        (make-property :radius-x
                       (i18n:tr "Radius X")
                       :length
                       (ellipse-radius-x ellipse)
                       t)
        (make-property :radius-y
                       (i18n:tr "Radius Y")
                       :length
                       (ellipse-radius-y ellipse)
                       t)
        (make-property :rotation
                       (i18n:tr "Rotation")
                       :angle
                       (ellipse-rotation ellipse)
                       t)))

(defmethod entity-derived-values ((ellipse ellipse-entity))
  (list (make-derived-value (i18n:tr "Perimeter")
                            :length
                            (geo:ellipse-perimeter (ellipse-radius-x ellipse) (ellipse-radius-y ellipse)))
        (make-derived-value (i18n:tr "Area")
                            :area
                            (geo:ellipse-area (ellipse-radius-x ellipse) (ellipse-radius-y ellipse)))))

(defmethod entity-with-property ((ellipse ellipse-entity)
                                 key
                                 value)
  (ecase key
    (:center-x (update-ellipse ellipse :center-x value))
    (:center-y (update-ellipse ellipse :center-y value))
    (:radius-x (update-ellipse ellipse :radius-x value))
    (:radius-y (update-ellipse ellipse :radius-y value))
    (:rotation (update-ellipse ellipse :rotation value))))

(defmethod entity-with-layer ((ellipse ellipse-entity)
                              layer-id)
  (update-ellipse ellipse :layer-id layer-id))

(defmethod entity-with-identity ((ellipse ellipse-entity)
                                 id
                                 layer-id)
  (update-ellipse ellipse
                  :id id
                  :layer-id layer-id))

(defmethod entity-touches-bounds-p ((ellipse ellipse-entity)
                                    bounds)
  (geo:ellipse-touches-bounds-p (ellipse-center-x ellipse)
                                (ellipse-center-y ellipse)
                                (ellipse-radius-x ellipse)
                                (ellipse-radius-y ellipse)
                                (ellipse-rotation ellipse)
                                bounds))

(defmethod entity-grips ((ellipse ellipse-entity))
  (let ((snaps (entity-snap-points ellipse)))
    (list (make-grip :first-axis-end
                     (first (second snaps))
                     (second (second snaps))
                     :point)
          (make-grip :first-axis-start
                     (first (third snaps))
                     (second (third snaps))
                     :point)
          (make-grip :second-axis-end
                     (first (fourth snaps))
                     (second (fourth snaps))
                     :point)
          (make-grip :second-axis-start
                     (first (fifth snaps))
                     (second (fifth snaps))
                     :point)
          (make-grip :move
                     (ellipse-center-x ellipse)
                     (ellipse-center-y ellipse)
                     :move))))

(defmethod entity-with-grip ((ellipse ellipse-entity)
                             grip
                             x
                             y)
  (let* ((dx (- x (ellipse-center-x ellipse)))
         (dy (- y (ellipse-center-y ellipse)))
         (reach (sqrt (+ (* dx dx) (* dy dy)))))
    (ecase (grip-id grip)
      (:first-axis-end (update-ellipse ellipse
                                       :radius-x reach
                                       :rotation (atan dy dx)))
      (:first-axis-start (update-ellipse ellipse
                                         :radius-x reach
                                         :rotation (atan (- dy) (- dx))))
      ((:second-axis-end :second-axis-start)
       (update-ellipse ellipse
                       :radius-y (abs (- (* dy (cos (ellipse-rotation ellipse)))
                                         (* dx (sin (ellipse-rotation ellipse)))))))
      (:move (moved-by-grip ellipse
                            grip
                            x
                            y)))))
