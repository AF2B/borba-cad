(in-package #:borba-cad/domain)

(defstruct (rectangle-entity (:include entity)
                             (:conc-name rectangle-)
                             (:constructor %make-rectangle (id
                                                            layer-id
                                                            bounds
                                                            min-x
                                                            min-y
                                                            width
                                                            height))
                             (:copier nil))
  "An axis-aligned rectangle.

MIN-X: x of the left edge.
MIN-Y: y of the bottom edge.
WIDTH: extent along x, above the length tolerance.
HEIGHT: extent along y, above the length tolerance."
  (min-x 0d0 :type double-float :read-only t)
  (min-y 0d0 :type double-float :read-only t)
  (width 0d0 :type double-float :read-only t)
  (height 0d0 :type double-float :read-only t))

(defun make-rectangle (&key id
                            layer-id
                            min-x
                            min-y
                            width
                            height)
  "Create a validated rectangle. Signals INVALID-ENTITY when an invariant is violated.

ID: positive entity id.
LAYER-ID: id of the layer.
MIN-X: x of the left edge, in millimetres.
MIN-Y: y of the bottom edge.
WIDTH: extent along x.
HEIGHT: extent along y."
  (require-identifiers :rectangle
                       id
                       layer-id)
  (require-coordinate :rectangle
                      "min-x"
                      min-x)
  (require-coordinate :rectangle
                      "min-y"
                      min-y)
  (require-positive-length :rectangle
                           "width"
                           width)
  (require-positive-length :rectangle
                           "height"
                           height)
  (let ((bounds (geo:make-bounds min-x
                                 min-y
                                 (+ min-x width)
                                 (+ min-y height))))
    (require-bounds-in-range :rectangle bounds)
    (%make-rectangle id
                     layer-id
                     bounds
                     min-x
                     min-y
                     width
                     height)))

(defun make-rectangle-from-corners (&key id
                                         layer-id
                                         x1
                                         y1
                                         x2
                                         y2)
  "Create a rectangle from two opposite corners given in any order.

ID: positive entity id.
LAYER-ID: id of the layer.
X1: x of the first corner.
Y1: y of the first corner.
X2: x of the opposite corner.
Y2: y of the opposite corner."
  (require-coordinate :rectangle
                      "x1"
                      x1)
  (require-coordinate :rectangle
                      "y1"
                      y1)
  (require-coordinate :rectangle
                      "x2"
                      x2)
  (require-coordinate :rectangle
                      "y2"
                      y2)
  (make-rectangle :id id
                  :layer-id layer-id
                  :min-x (min x1 x2)
                  :min-y (min y1 y2)
                  :width (abs (- x2 x1))
                  :height (abs (- y2 y1))))

(defun update-rectangle (rectangle
                         &key (id (entity-id rectangle))
                              (layer-id (entity-layer-id rectangle))
                              (min-x (rectangle-min-x rectangle))
                              (min-y (rectangle-min-y rectangle))
                              (width (rectangle-width rectangle))
                              (height (rectangle-height rectangle)))
  "Return a validated copy of RECTANGLE with the given fields replaced.

RECTANGLE: the rectangle to copy.
ID: replacement id.
LAYER-ID: replacement layer id.
MIN-X: replacement left edge.
MIN-Y: replacement bottom edge.
WIDTH: replacement width.
HEIGHT: replacement height."
  (make-rectangle :id id
                  :layer-id layer-id
                  :min-x min-x
                  :min-y min-y
                  :width width
                  :height height))

(defmethod entity-kind ((rectangle rectangle-entity))
  :rectangle)

(defmethod entity-pick-distance ((rectangle rectangle-entity)
                                 x
                                 y)
  (let ((bounds (entity-bounds rectangle)))
    (geo:point-rectangle-outline-distance x
                                          y
                                          (geo:bounds-min-x bounds)
                                          (geo:bounds-min-y bounds)
                                          (geo:bounds-max-x bounds)
                                          (geo:bounds-max-y bounds))))

(defmethod entity-translated ((rectangle rectangle-entity)
                              dx
                              dy)
  (update-rectangle rectangle
                    :min-x (+ (rectangle-min-x rectangle) dx)
                    :min-y (+ (rectangle-min-y rectangle) dy)))

(defmethod entity-snap-points ((rectangle rectangle-entity))
  (let ((bounds (entity-bounds rectangle)))
    (list (list (geo:bounds-min-x bounds) (geo:bounds-min-y bounds) :endpoint)
          (list (geo:bounds-max-x bounds) (geo:bounds-min-y bounds) :endpoint)
          (list (geo:bounds-max-x bounds) (geo:bounds-max-y bounds) :endpoint)
          (list (geo:bounds-min-x bounds) (geo:bounds-max-y bounds) :endpoint))))

(defmethod entity-properties ((rectangle rectangle-entity))
  (list (make-property :min-x
                       (i18n:tr "Left")
                       :coordinate
                       (rectangle-min-x rectangle)
                       t)
        (make-property :min-y
                       (i18n:tr "Bottom")
                       :coordinate
                       (rectangle-min-y rectangle)
                       t)
        (make-property :width
                       (i18n:tr "Width")
                       :length
                       (rectangle-width rectangle)
                       t)
        (make-property :height
                       (i18n:tr "Height")
                       :length
                       (rectangle-height rectangle)
                       t)))

(defmethod entity-derived-values ((rectangle rectangle-entity))
  (list (make-derived-value (i18n:tr "Area")
                            :area
                            (geo:rectangle-area (rectangle-width rectangle)
                                                (rectangle-height rectangle)))
        (make-derived-value (i18n:tr "Perimeter")
                            :length
                            (geo:rectangle-perimeter (rectangle-width rectangle)
                                                     (rectangle-height rectangle)))))

(defmethod entity-with-property ((rectangle rectangle-entity)
                                 key
                                 value)
  (ecase key
    (:min-x (update-rectangle rectangle :min-x value))
    (:min-y (update-rectangle rectangle :min-y value))
    (:width (update-rectangle rectangle :width value))
    (:height (update-rectangle rectangle :height value))))

(defmethod entity-with-layer ((rectangle rectangle-entity)
                              layer-id)
  (update-rectangle rectangle :layer-id layer-id))

(defmethod entity-with-identity ((rectangle rectangle-entity)
                                 id
                                 layer-id)
  (update-rectangle rectangle
                 :id id
                 :layer-id layer-id))

(defmethod entity-touches-bounds-p ((rectangle rectangle-entity)
                                    bounds)
  (let* ((left (rectangle-min-x rectangle))
         (bottom (rectangle-min-y rectangle))
         (right (+ left (rectangle-width rectangle)))
         (top (+ bottom (rectangle-height rectangle))))
    (or (geo:segment-touches-bounds-p left
                                      bottom
                                      right
                                      bottom
                                      bounds)
        (geo:segment-touches-bounds-p right
                                      bottom
                                      right
                                      top
                                      bounds)
        (geo:segment-touches-bounds-p right
                                      top
                                      left
                                      top
                                      bounds)
        (geo:segment-touches-bounds-p left
                                      top
                                      left
                                      bottom
                                      bounds))))

(defun rectangle-corner (rectangle
                         corner)
  "Return, as two values, the coordinates of a corner of RECTANGLE.

RECTANGLE: a rectangle.
CORNER: :bottom-left, :bottom-right, :top-right or :top-left."
  (let ((left (rectangle-min-x rectangle))
        (bottom (rectangle-min-y rectangle)))
    (let ((right (+ left (rectangle-width rectangle)))
          (top (+ bottom (rectangle-height rectangle))))
      (ecase corner
        (:bottom-left (values left bottom))
        (:bottom-right (values right bottom))
        (:top-right (values right top))
        (:top-left (values left top))))))

(defparameter *opposite-corners*
  '((:bottom-left . :top-right) (:bottom-right . :top-left) (:top-right . :bottom-left)
    (:top-left . :bottom-right))
  "The corner opposite to each corner of a rectangle, which stays where it is when a corner is dragged.")

(defmethod entity-grips ((rectangle rectangle-entity))
  (append (loop for (corner) in *opposite-corners*
                collect (multiple-value-bind (x y)
                            (rectangle-corner rectangle corner)
                          (make-grip corner
                                     x
                                     y
                                     :point)))
          (list (make-grip :move
                           (+ (rectangle-min-x rectangle) (/ (rectangle-width rectangle) 2))
                           (+ (rectangle-min-y rectangle) (/ (rectangle-height rectangle) 2))
                           :move))))

(defmethod entity-with-grip ((rectangle rectangle-entity)
                             grip
                             x
                             y)
  (if (eq (grip-id grip) :move)
      (moved-by-grip rectangle
                     grip
                     x
                     y)
      (multiple-value-bind (fixed-x fixed-y)
          (rectangle-corner rectangle (cdr (assoc (grip-id grip) *opposite-corners*)))
        (make-rectangle-from-corners :id (entity-id rectangle)
                                     :layer-id (entity-layer-id rectangle)
                                     :x1 fixed-x
                                     :y1 fixed-y
                                     :x2 x
                                     :y2 y))))
