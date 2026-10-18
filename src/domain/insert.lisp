(in-package #:borba-cad/domain)

;;;; The placement of a block in a drawing: where its base point goes, how much larger or smaller it is, how far it is
;;;; turned and whether it is flipped. It holds the block and the entities of the block as they are placed, worked out
;;;; once, so that drawing, picking and snapping it are the same as for any other entities.

(defconstant +maximum-insert-scale+ 1d6
  "Largest scale of a placement, which is also the reciprocal of the smallest.")

(defstruct (insert-entity (:include entity)
                          (:conc-name insert-)
                          (:constructor %make-insert (id
                                                      layer-id
                                                      bounds
                                                      block
                                                      x
                                                      y
                                                      scale
                                                      rotation
                                                      flipped
                                                      entities))
                          (:copier nil))
  "A block placed in a drawing.

BLOCK: the block definition.
X: x of the point where the base point of the block goes.
Y: y of that point.
SCALE: how many times larger than the definition it is.
ROTATION: how far it is turned, in radians counter-clockwise, in [0, 2 pi).
FLIPPED: true when the block is reflected across its own vertical axis before it is turned.
ENTITIES: the entities of the block as placed, on the layer of the placement, computed at construction."
  (block nil :type (or null block-definition) :read-only t)
  (x 0d0 :type double-float :read-only t)
  (y 0d0 :type double-float :read-only t)
  (scale 1d0 :type double-float :read-only t)
  (rotation 0d0 :type double-float :read-only t)
  (flipped nil :type boolean :read-only t)
  (entities nil :type list :read-only t))

(defun placed-entity (entity
                      x
                      y
                      scale
                      rotation
                      flipped
                      layer-id)
  "Return an entity of a block as it is placed: flipped, then scaled and turned about the base point, then moved.

ENTITY: an entity of the definition.
X: x of the point the base point goes to.
Y: y of that point.
SCALE: the scale.
ROTATION: the turn in radians.
FLIPPED: true to reflect it across the vertical axis first.
LAYER-ID: the layer the result is on."
  (let* ((flipped-entity (if flipped
                             (entity-mirrored entity
                                              0d0
                                              0d0
                                              0d0
                                              1d0)
                             entity))
         (scaled (if (= scale 1d0)
                     flipped-entity
                     (entity-scaled flipped-entity
                                    0d0
                                    0d0
                                    scale)))
         (turned (if (zerop rotation)
                     scaled
                     (entity-rotated scaled
                                     0d0
                                     0d0
                                     rotation))))
    (entity-with-layer (entity-translated turned
                                          x
                                          y) layer-id)))

(defun make-insert (&key id
                         layer-id
                         block
                         x
                         y
                         (scale 1d0)
                         (rotation 0d0)
                         flipped)
  "Create a validated placement of a block. Signals INVALID-ENTITY when an invariant is violated, including when
placing the block makes one of its entities invalid, for example by taking it out of the drawing.

ID: positive entity id.
LAYER-ID: id of the layer.
BLOCK: the block definition.
X: x of the point where the base point goes, in millimetres.
Y: y of that point.
SCALE: how many times larger, above zero.
ROTATION: the turn in radians; any finite angle, kept reduced to [0, 2 pi).
FLIPPED: true to reflect the block across its vertical axis before it is turned."
  (require-identifiers :insert
                       id
                       layer-id)
  (unless (typep block 'block-definition)
    (invalid-entity-failure :insert
                            "it needs a block, got ~s"
                            block))
  (require-coordinate :insert
                      "x"
                      x)
  (require-coordinate :insert
                      "y"
                      y)
  (unless (and (typep scale 'double-float)
               (< (/ +maximum-insert-scale+) scale +maximum-insert-scale+))
    (invalid-entity-failure :insert
                            "its scale must be a number between ~f and ~f, got ~a"
                            (/ +maximum-insert-scale+)
                            +maximum-insert-scale+
                            scale))
  (require-angle :insert
                 "rotation"
                 rotation)
  (let* ((turn (geo:normalize-angle rotation))
         (flag (and flipped t))
         (entities (handler-case (loop for entity in (block-definition-entities block)
                                       collect (placed-entity entity
                                                              x
                                                              y
                                                              scale
                                                              turn
                                                              flag
                                                              layer-id))
                     (base:invalid-entity (condition)
                       (invalid-entity-failure :insert
                                               "placing it would make one of its entities invalid: ~a"
                                               condition))))
         (bounds (entities-bounds entities)))
    (require-bounds-in-range :insert bounds)
    (%make-insert id
                  layer-id
                  bounds
                  block
                  x
                  y
                  scale
                  turn
                  flag
                  entities)))

(defun update-insert (insert
                      &key (id (entity-id insert))
                           (layer-id (entity-layer-id insert))
                           (block (insert-block insert))
                           (x (insert-x insert))
                           (y (insert-y insert))
                           (scale (insert-scale insert))
                           (rotation (insert-rotation insert))
                           (flipped (insert-flipped insert)))
  "Return a validated copy of INSERT with the given fields replaced.

INSERT: the placement to copy.
ID: replacement id.
LAYER-ID: replacement layer id.
BLOCK: replacement block definition.
X: replacement x.
Y: replacement y.
SCALE: replacement scale.
ROTATION: replacement turn.
FLIPPED: replacement flip."
  (make-insert :id id
               :layer-id layer-id
               :block block
               :x x
               :y y
               :scale scale
               :rotation rotation
               :flipped flipped))

(defmethod entity-kind ((insert insert-entity))
  :insert)

(defmethod entity-pick-distance ((insert insert-entity)
                                 x
                                 y)
  (loop for entity in (insert-entities insert)
        minimize (entity-pick-distance entity
                                       x
                                       y)))

(defmethod entity-translated ((insert insert-entity)
                              dx
                              dy)
  (update-insert insert
                 :x (+ (insert-x insert) dx)
                 :y (+ (insert-y insert) dy)))

(defmethod entity-snap-points ((insert insert-entity))
  (cons (list (insert-x insert) (insert-y insert) :endpoint)
        (loop for entity in (insert-entities insert)
              append (entity-snap-points entity))))

(defmethod entity-properties ((insert insert-entity))
  (append (list (make-property :block
                               (i18n:tr "Block")
                               :text
                               (block-definition-name (insert-block insert))
                               nil))
          (coordinate-properties (list (list :x (i18n:mark "Position X"))
                                       (list :y (i18n:mark "Position Y")))
                                 (list (insert-x insert) (insert-y insert)))
          (list (make-property :scale
                               (i18n:tr "Scale")
                               :factor
                               (insert-scale insert)
                               t)
                (make-property :rotation
                               (i18n:tr "Rotation")
                               :angle
                               (insert-rotation insert)
                               t)
                (make-property :flipped
                               (i18n:tr "Flipped")
                               :boolean
                               (insert-flipped insert)
                               t))))

(defmethod entity-derived-values ((insert insert-entity))
  (list (make-derived-value (i18n:tr "Entities")
                            :count
                            (length (insert-entities insert)))))

(defmethod entity-with-property ((insert insert-entity)
                                 key
                                 value)
  (ecase key
    (:x (update-insert insert :x value))
    (:y (update-insert insert :y value))
    (:scale (update-insert insert :scale value))
    (:rotation (update-insert insert :rotation value))
    (:flipped (update-insert insert :flipped value))))

(defmethod entity-with-layer ((insert insert-entity)
                              layer-id)
  (update-insert insert :layer-id layer-id))

(defmethod entity-with-identity ((insert insert-entity)
                                 id
                                 layer-id)
  (update-insert insert
                 :id id
                 :layer-id layer-id))

(defmethod entity-touches-bounds-p ((insert insert-entity)
                                    bounds)
  (some (lambda (entity)
          (entity-touches-bounds-p entity bounds))
        (insert-entities insert)))

(defmethod entity-grips ((insert insert-entity))
  (list (make-grip :move
                   (insert-x insert)
                   (insert-y insert)
                   :move)))

(defmethod entity-with-grip ((insert insert-entity)
                             grip
                             x
                             y)
  (moved-by-grip insert
                 grip
                 x
                 y))

;;; Rotating, mirroring and scaling a placement

(defmethod entity-rotated ((insert insert-entity) center-x center-y angle)
  (multiple-value-bind (x y) (rotate-xy (insert-x insert)
                                        (insert-y insert)
                                        center-x
                                        center-y
                                        angle)
    (update-insert insert
                   :x x
                   :y y
                   :rotation (+ (insert-rotation insert) angle))))

(defmethod entity-mirrored ((insert insert-entity) line-x1 line-y1 line-x2 line-y2)
  (multiple-value-bind (x y) (mirror-xy (insert-x insert)
                                        (insert-y insert)
                                        line-x1
                                        line-y1
                                        line-x2
                                        line-y2)
    (update-insert insert
                   :x x
                   :y y
                   :rotation (+ (mirror-angle (insert-rotation insert)
                                              line-x1
                                              line-y1
                                              line-x2
                                              line-y2)
                                geo:+pi+)
                   :flipped (not (insert-flipped insert)))))

(defmethod entity-scaled ((insert insert-entity) center-x center-y factor)
  (multiple-value-bind (x y) (scale-xy (insert-x insert)
                                       (insert-y insert)
                                       center-x
                                       center-y
                                       factor)
    (update-insert insert
                   :x x
                   :y y
                   :scale (* factor (insert-scale insert)))))
