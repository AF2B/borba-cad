(in-package #:borba-cad/domain)

;;;; The entity protocol. Entities are immutable values; "changing" one builds a new value
;;;; with the same id. Adding an entity type means: a file in this directory with its struct,
;;;; validating constructor, UPDATE-<TYPE> function and the methods below; a serializer and
;;;; parser clause in persistence; a drawing routine in scene; a creation tool in app.

(defconstant +maximum-polyline-vertices+ 100000
  "Largest vertex count of a polyline. Bounds memory use when loading untrusted files.")

(defconstant +maximum-text-length+ 256
  "Largest number of characters in a text annotation.")

(defconstant +maximum-dimension-precision+ 6
  "Largest number of decimals shown in a dimension text.")

(defconstant +inspector-maximum-vertex-rows+ 6
  "Number of polyline vertices that the inspector lists for numeric editing.")

(defparameter *entity-kinds* '(:line :rectangle :circle :polyline :text :dimension)
  "Every entity kind keyword, in toolbar order.")

(defun entity-kinds ()
  "Return the list of entity kind keywords."
  (copy-list *entity-kinds*))

(defstruct (entity (:constructor nil)
                   (:copier nil))
  "Base of every drawing entity.

ID: unique within the drawing, assigned once, never reused; ascending ids define stacking order.
LAYER-ID: id of the layer the entity belongs to.
BOUNDS: world-space bounds, computed once at construction."
  (id 1 :type (integer 1) :read-only t)
  (layer-id 0 :type (integer 0) :read-only t)
  (bounds nil :type (or null geo:bounds) :read-only t))

(defstruct (property (:constructor make-property (key
                                                  label
                                                  kind
                                                  value
                                                  editable)))
  "One row of the property inspector.

KEY: identifies the property for ENTITY-WITH-PROPERTY.
LABEL: English label shown to the user.
KIND: :coordinate, :length, :text, :boolean, :unit or :integer.
VALUE: current value (millimetres for lengths and coordinates).
EDITABLE: true when the user may change it."
  key
  (label "" :type string)
  (kind :length :type keyword)
  value
  (editable t :type boolean))

(defstruct (derived-value (:constructor make-derived-value (label
                                                            kind
                                                            value)))
  "A read-only quantity computed from an entity, shown in the inspector.

LABEL: English label.
KIND: :length, :area, :angle (radians) or :count.
VALUE: the number."
  (label "" :type string)
  (kind :length :type keyword)
  value)

(defgeneric entity-kind (entity)
  (:documentation "Return the entity's kind keyword (:line, :rectangle, :circle, :polyline,
:text or :dimension).

ENTITY: an entity."))

(defgeneric entity-pick-distance (entity x y)
  (:documentation "Return the distance in millimetres from the point to the entity's selectable
geometry (outline for shapes, filled box for text); zero when the point is on it.

ENTITY: an entity.
X: point x.
Y: point y."))

(defgeneric entity-translated (entity dx dy)
  (:documentation "Return a copy of ENTITY moved by (DX, DY), with the same id.

ENTITY: an entity.
DX: displacement along x.
DY: displacement along y."))

(defgeneric entity-snap-points (entity)
  (:documentation "Return the object-snap candidates of ENTITY as a list of (x y kind) lists,
where kind is :endpoint or :center.

ENTITY: an entity."))

(defgeneric entity-properties (entity)
  (:documentation "Return the inspector rows of ENTITY as a list of PROPERTY values.

ENTITY: an entity."))

(defgeneric entity-derived-values (entity)
  (:documentation "Return the read-only derived quantities of ENTITY as DERIVED-VALUE values.

ENTITY: an entity."))

(defgeneric entity-with-property (entity key value)
  (:documentation "Return a copy of ENTITY with one property changed, validated by the entity's
constructor. Signals INVALID-ENTITY when the result would be invalid.

ENTITY: an entity.
KEY: a PROPERTY-KEY returned by ENTITY-PROPERTIES.
VALUE: new value (double-float, string, boolean or unit keyword according to the kind)."))

(defgeneric entity-with-layer (entity layer-id)
  (:documentation "Return a copy of ENTITY on another layer, with the same id.

ENTITY: an entity.
LAYER-ID: id of the destination layer."))

;;; Validation helpers shared by the entity constructors.

(defun invalid-entity-failure (kind
                               control
                               &rest arguments)
  "Signal INVALID-ENTITY for KIND with a reason built from CONTROL and ARGUMENTS.

KIND: entity kind keyword.
CONTROL: format control string.
ARGUMENTS: format arguments."
  (error 'base:invalid-entity
         :kind kind
         :reason (apply #'i18n:trf control arguments)))

(defun require-identifiers (kind
                            id
                            layer-id)
  "Signal INVALID-ENTITY unless ID and LAYER-ID are acceptable identifiers.

KIND: entity kind keyword.
ID: entity id, a positive integer.
LAYER-ID: layer id, a non-negative integer."
  (unless (typep id '(integer 1))
    (invalid-entity-failure kind
                            "the id must be a positive integer, got ~s"
                            id))
  (unless (typep layer-id '(integer 0))
    (invalid-entity-failure kind
                            "the layer id must be a non-negative integer, got ~s"
                            layer-id)))

(defun require-coordinate (kind
                           name
                           value)
  "Signal INVALID-ENTITY unless VALUE is a double-float within the coordinate range.

KIND: entity kind keyword.
NAME: property name used in the message.
VALUE: the value to check."
  (unless (base:coordinate-p value)
    (invalid-entity-failure kind
                            "~a must be a number between -~f and ~f mm, got ~a"
                            name
                            base:+maximum-coordinate-magnitude+
                            base:+maximum-coordinate-magnitude+
                            value)))

(defun require-positive-length (kind
                                name
                                value)
  "Signal INVALID-ENTITY unless VALUE is a double-float length above the tolerance.

KIND: entity kind keyword.
NAME: property name used in the message.
VALUE: the value to check."
  (unless (base:positive-length-p value)
    (invalid-entity-failure kind
                            "~a must be a number above ~f mm, got ~a"
                            name
                            base:+length-tolerance+
                            value)))

(defun require-bounds-in-range (kind
                                bounds)
  "Signal INVALID-ENTITY when BOUNDS reach outside the allowed coordinate range.

KIND: entity kind keyword.
BOUNDS: the entity's bounds."
  (unless (and (base:coordinate-p (geo:bounds-min-x bounds))
               (base:coordinate-p (geo:bounds-min-y bounds))
               (base:coordinate-p (geo:bounds-max-x bounds))
               (base:coordinate-p (geo:bounds-max-y bounds)))
    (invalid-entity-failure kind
                            "it extends beyond the drawing limit of ~f mm"
                            base:+maximum-coordinate-magnitude+)))

(defun coordinate-properties (keys-and-labels
                              entity-values)
  "Build editable :coordinate PROPERTY rows.

KEYS-AND-LABELS: list of (key label) pairs.
ENTITY-VALUES: list of the matching values, in the same order."
  (loop for (key label) in keys-and-labels
        for value in entity-values
        collect (make-property key
                               (i18n:tr label)
                               :coordinate
                               value
                               t)))
