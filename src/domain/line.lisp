(in-package #:borba-cad/domain)

(defstruct (line-entity (:include entity)
                        (:conc-name line-)
                        (:constructor %make-line (id
                                                  layer-id
                                                  bounds
                                                  x1
                                                  y1
                                                  x2
                                                  y2))
                        (:copier nil))
  "A straight segment between two points.

X1: x of the start point.
Y1: y of the start point.
X2: x of the end point.
Y2: y of the end point."
  (x1 0d0 :type double-float :read-only t)
  (y1 0d0 :type double-float :read-only t)
  (x2 0d0 :type double-float :read-only t)
  (y2 0d0 :type double-float :read-only t))

(defun make-line (&key id
                       layer-id
                       x1
                       y1
                       x2
                       y2)
  "Create a validated line. Signals INVALID-ENTITY when an invariant is violated.

ID: positive entity id.
LAYER-ID: id of the layer.
X1: x of the start point, in millimetres.
Y1: y of the start point.
X2: x of the end point.
Y2: y of the end point."
  (require-identifiers :line
                       id
                       layer-id)
  (require-coordinate :line
                      "x1"
                      x1)
  (require-coordinate :line
                      "y1"
                      y1)
  (require-coordinate :line
                      "x2"
                      x2)
  (require-coordinate :line
                      "y2"
                      y2)
  (let ((length (geo:segment-length x1
                                    y1
                                    x2
                                    y2)))
    (unless (base:positive-length-p length)
      (invalid-entity-failure :line
                              "its length is ~f mm; a line needs a length above ~f mm"
                              length
                              base:+length-tolerance+)))
  (%make-line id
              layer-id
              (geo:bounds-from-corners x1
                                       y1
                                       x2
                                       y2)
              x1
              y1
              x2
              y2))

(defun update-line (line
                    &key (id (entity-id line))
                         (layer-id (entity-layer-id line))
                         (x1 (line-x1 line))
                         (y1 (line-y1 line))
                         (x2 (line-x2 line))
                         (y2 (line-y2 line)))
  "Return a validated copy of LINE with the given fields replaced.

LINE: the line to copy.
ID: replacement id.
LAYER-ID: replacement layer id.
X1: replacement start x.
Y1: replacement start y.
X2: replacement end x.
Y2: replacement end y."
  (make-line :id id
             :layer-id layer-id
             :x1 x1
             :y1 y1
             :x2 x2
             :y2 y2))

(defmethod entity-kind ((line line-entity))
  :line)

(defmethod entity-pick-distance ((line line-entity)
                                 x
                                 y)
  (geo:point-segment-distance x
                              y
                              (line-x1 line)
                              (line-y1 line)
                              (line-x2 line)
                              (line-y2 line)))

(defmethod entity-translated ((line line-entity)
                              dx
                              dy)
  (update-line line
               :x1 (+ (line-x1 line) dx)
               :y1 (+ (line-y1 line) dy)
               :x2 (+ (line-x2 line) dx)
               :y2 (+ (line-y2 line) dy)))

(defmethod entity-snap-points ((line line-entity))
  (list (list (line-x1 line) (line-y1 line) :endpoint)
        (list (line-x2 line) (line-y2 line) :endpoint)))

(defmethod entity-properties ((line line-entity))
  (coordinate-properties (list (list :x1 (i18n:mark "Start X"))
                               (list :y1 (i18n:mark "Start Y"))
                               (list :x2 (i18n:mark "End X"))
                               (list :y2 (i18n:mark "End Y")))
                         (list (line-x1 line) (line-y1 line) (line-x2 line) (line-y2 line))))

(defmethod entity-derived-values ((line line-entity))
  (list (make-derived-value (i18n:tr "Length")
                            :length
                            (geo:segment-length (line-x1 line)
                                                (line-y1 line)
                                                (line-x2 line)
                                                (line-y2 line)))
        (make-derived-value (i18n:tr "Angle")
                            :angle
                            (geo:segment-angle (line-x1 line)
                                               (line-y1 line)
                                               (line-x2 line)
                                               (line-y2 line)))))

(defmethod entity-with-property ((line line-entity)
                                 key
                                 value)
  (ecase key
    (:x1 (update-line line :x1 value))
    (:y1 (update-line line :y1 value))
    (:x2 (update-line line :x2 value))
    (:y2 (update-line line :y2 value))))

(defmethod entity-with-layer ((line line-entity)
                              layer-id)
  (update-line line :layer-id layer-id))

(defmethod entity-with-identity ((line line-entity)
                                 id
                                 layer-id)
  (update-line line
                 :id id
                 :layer-id layer-id))

(defmethod entity-touches-bounds-p ((line line-entity)
                                    bounds)
  (geo:segment-touches-bounds-p (line-x1 line)
                                (line-y1 line)
                                (line-x2 line)
                                (line-y2 line)
                                bounds))
