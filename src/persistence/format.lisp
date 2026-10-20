(in-package #:borba-cad/persistence)

;;;; The entity format table. One entry per entity kind drives both the writer and the reader,
;;;; so they cannot drift apart. Adding an entity kind means adding one entry here.
;;;;
;;;; Format versions. The program writes the newest version and reads every version from the oldest
;;;; supported one. What a version added is recorded here, in the table, as the SINCE of a field or of
;;;; an entity kind, together with the DEFAULT that a field gets when a file is older than the field:
;;;; that table is the migration, and a file of an older version is read as the newer one with the
;;;; defaults filled in. Version 2 added the angle of a text, the arc, the ellipse and the polygon. Version 3 added
;;;; the radius, diameter and angle dimensions. Version 4 added the hatch. Version 5 added the blocks
;;;; section and the placement of a block. Version 6 added the holes of a hatch. Version 7 added the sheets that
;;;; are printed. Version 8 added, to a block, what drew it when it is a parametric block. Version 9 added the
;;;; underlays: pictures under the drawing, kept whole, as text, with where they stand.

(defconstant +supported-format-version+ 9
  "The newest format version: the one this program writes, and the newest it reads.")

(defconstant +oldest-supported-format-version+ 1
  "The oldest format version this program still reads, migrating it as it is read.")

(defstruct (field-spec (:constructor make-field-spec (key
                                                      name
                                                      type
                                                      accessor
                                                      &key (since +oldest-supported-format-version+)
                                                           default)))
  "How one entity property is stored.

KEY: keyword passed to the entity constructor.
NAME: field name in the file.
TYPE: :real, :reals, :boolean, :string, :unit, :pattern, :block or :integer.
ACCESSOR: symbol of the function that reads the property from an entity.
SINCE: the first format version that has the field; a file of an older version does not have it.
DEFAULT: the value the property gets when the file is older than SINCE."
  (key :x :type keyword :read-only t)
  (name "" :type string :read-only t)
  (type :real :type keyword :read-only t)
  (accessor nil :type symbol :read-only t)
  (since +oldest-supported-format-version+ :type integer :read-only t)
  (default nil :read-only t))

(defstruct (entity-format (:constructor make-entity-format (kind
                                                            name
                                                            constructor
                                                            fields
                                                            &key (since +oldest-supported-format-version+))))
  "How one entity kind is stored.

KIND: entity kind keyword.
NAME: name of the entity list in the file.
CONSTRUCTOR: symbol of the validating constructor, called with keyword arguments.
FIELDS: list of FIELD-SPEC for every property except id and layer.
SINCE: the first format version that has this kind of entity."
  (kind :line :type keyword :read-only t)
  (name "" :type string :read-only t)
  (constructor nil :type symbol :read-only t)
  (fields nil :type list :read-only t)
  (since +oldest-supported-format-version+ :type integer :read-only t))

(defun real-fields (&rest names-and-accessors)
  "Build :real field specs whose keyword is the upcased field name.

NAMES-AND-ACCESSORS: alternating field names and accessor symbols."
  (loop for (name accessor) on names-and-accessors by #'cddr
        collect (make-field-spec (intern (string-upcase name) :keyword)
                                 name
                                 :real
                                 accessor)))

(defparameter *entity-formats*
  (list (make-entity-format :line
                            "line"
                            'dom:make-line
                            (real-fields "x1"
                                         'dom:line-x1
                                         "y1"
                                         'dom:line-y1
                                         "x2"
                                         'dom:line-x2
                                         "y2"
                                         'dom:line-y2))
        (make-entity-format :rectangle
                            "rectangle"
                            'dom:make-rectangle
                            (real-fields "min-x"
                                         'dom:rectangle-min-x
                                         "min-y"
                                         'dom:rectangle-min-y
                                         "width"
                                         'dom:rectangle-width
                                         "height"
                                         'dom:rectangle-height))
        (make-entity-format :circle
                            "circle"
                            'dom:make-circle
                            (real-fields "center-x"
                                         'dom:circle-center-x
                                         "center-y"
                                         'dom:circle-center-y
                                         "radius"
                                         'dom:circle-radius))
        (make-entity-format :polyline
                            "polyline"
                            'dom:make-polyline
                            (list (make-field-spec :closed
                                                   "closed"
                                                   :boolean
                                                   'dom:polyline-closed)
                                  (make-field-spec :coordinates
                                                   "vertices"
                                                   :reals
                                                   'dom:polyline-coordinates)))
        (make-entity-format :text
                            "text"
                            'dom:make-text
                            (append (real-fields "x"
                                                 'dom:text-x
                                                 "y"
                                                 'dom:text-y
                                                 "height"
                                                 'dom:text-height)
                                    (list (make-field-spec :string
                                                           "string"
                                                           :string
                                                           'dom:text-string)
                                          (make-field-spec :angle
                                                           "angle"
                                                           :real
                                                           'dom:text-angle
                                                           :since 2
                                                           :default 0d0))))
        (make-entity-format :dimension
                            "dimension"
                            'dom:make-dimension
                            (append (real-fields "x1"
                                                 'dom:dimension-x1
                                                 "y1"
                                                 'dom:dimension-y1
                                                 "x2"
                                                 'dom:dimension-x2
                                                 "y2"
                                                 'dom:dimension-y2
                                                 "offset"
                                                 'dom:dimension-offset
                                                 "text-height"
                                                 'dom:dimension-text-height)
                                    (list (make-field-spec :unit
                                                           "unit"
                                                           :unit
                                                           'dom:dimension-unit)
                                          (make-field-spec :precision
                                                           "precision"
                                                           :integer
                                                           'dom:dimension-precision))))
        (make-entity-format :arc
                            "arc"
                            'dom:make-arc
                            (real-fields "center-x"
                                         'dom:arc-center-x
                                         "center-y"
                                         'dom:arc-center-y
                                         "radius"
                                         'dom:arc-radius
                                         "start-angle"
                                         'dom:arc-start-angle
                                         "end-angle"
                                         'dom:arc-end-angle)
                            :since 2)
        (make-entity-format :ellipse
                            "ellipse"
                            'dom:make-ellipse
                            (real-fields "center-x"
                                         'dom:ellipse-center-x
                                         "center-y"
                                         'dom:ellipse-center-y
                                         "radius-x"
                                         'dom:ellipse-radius-x
                                         "radius-y"
                                         'dom:ellipse-radius-y
                                         "rotation"
                                         'dom:ellipse-rotation)
                            :since 2)
        (make-entity-format :polygon
                            "polygon"
                            'dom:make-polygon
                            (append (real-fields "center-x"
                                                 'dom:polygon-center-x
                                                 "center-y"
                                                 'dom:polygon-center-y
                                                 "radius"
                                                 'dom:polygon-radius)
                                    (list (make-field-spec :sides
                                                           "sides"
                                                           :integer
                                                           'dom:polygon-sides))
                                    (real-fields "rotation"
                                                 'dom:polygon-rotation))
                            :since 2)
        (make-entity-format :radial-dimension
                            "radial-dimension"
                            'dom:make-radial-dimension
                            (append (real-fields "center-x"
                                                 'dom:radial-dimension-center-x
                                                 "center-y"
                                                 'dom:radial-dimension-center-y
                                                 "radius"
                                                 'dom:radial-dimension-radius
                                                 "angle"
                                                 'dom:radial-dimension-angle)
                                    (list (make-field-spec :diameter
                                                           "diameter"
                                                           :boolean
                                                           'dom:radial-dimension-diameter))
                                    (real-fields "text-height"
                                                 'dom:radial-dimension-text-height)
                                    (list (make-field-spec :unit
                                                           "unit"
                                                           :unit
                                                           'dom:radial-dimension-unit)
                                          (make-field-spec :precision
                                                           "precision"
                                                           :integer
                                                           'dom:radial-dimension-precision)))
                            :since 3)
        (make-entity-format :angular-dimension
                            "angular-dimension"
                            'dom:make-angular-dimension
                            (append (real-fields "vertex-x"
                                                 'dom:angular-dimension-vertex-x
                                                 "vertex-y"
                                                 'dom:angular-dimension-vertex-y
                                                 "start-angle"
                                                 'dom:angular-dimension-start-angle
                                                 "end-angle"
                                                 'dom:angular-dimension-end-angle
                                                 "radius"
                                                 'dom:angular-dimension-radius
                                                 "text-height"
                                                 'dom:angular-dimension-text-height)
                                    (list (make-field-spec :precision
                                                           "precision"
                                                           :integer
                                                           'dom:angular-dimension-precision)))
                            :since 3)
        (make-entity-format :hatch
                            "hatch"
                            'dom:make-hatch
                            (list (make-field-spec :coordinates
                                                   "vertices"
                                                   :reals
                                                   'dom:hatch-coordinates)
                                  (make-field-spec :pattern
                                                   "pattern"
                                                   :pattern
                                                   'dom:hatch-pattern)
                                  (make-field-spec :angle
                                                   "angle"
                                                   :real
                                                   'dom:hatch-angle)
                                  (make-field-spec :spacing
                                                   "spacing"
                                                   :real
                                                   'dom:hatch-spacing)
                                  (make-field-spec :hole-coordinates
                                                   "hole-vertices"
                                                   :reals
                                                   'dom:hatch-hole-coordinates
                                                   :since 6
                                                   :default (make-array 0 :element-type 'double-float))
                                  (make-field-spec :hole-sizes
                                                   "hole-sizes"
                                                   :reals
                                                   'dom:hatch-hole-sizes
                                                   :since 6
                                                   :default (make-array 0 :element-type 'double-float)))
                            :since 4)
        (make-entity-format :insert
                            "insert"
                            'dom:make-insert
                            (append (list (make-field-spec :block
                                                           "block"
                                                           :block
                                                           'dom:insert-block))
                                    (real-fields "x"
                                                 'dom:insert-x
                                                 "y"
                                                 'dom:insert-y
                                                 "scale"
                                                 'dom:insert-scale
                                                 "rotation"
                                                 'dom:insert-rotation)
                                    (list (make-field-spec :flipped
                                                           "flipped"
                                                           :boolean
                                                           'dom:insert-flipped)))
                            :since 5))
  "The storage format of every entity kind.")

(defun find-format-by-kind (kind)
  "Return the entity format for an entity kind keyword.

KIND: entity kind keyword."
  (or (find kind *entity-formats* :key #'entity-format-kind)
      (error "No storage format for entity kind ~s" kind)))

(defun find-format-by-name (name)
  "Return the entity format whose list name is NAME, or NIL.

NAME: the name read from the file."
  (find name *entity-formats* :key #'entity-format-name :test #'string=))
