(in-package #:borba-cad/persistence)

;;;; The entity format table. One entry per entity kind drives both the writer and the reader,
;;;; so they cannot drift apart. Adding an entity kind means adding one entry here.

(defconstant +supported-format-version+ 1
  "The only format version this program reads and writes.")

(defstruct (field-spec (:constructor make-field-spec (key
                                                      name
                                                      type
                                                      accessor)))
  "How one entity property is stored.

KEY: keyword passed to the entity constructor.
NAME: field name in the file.
TYPE: :real, :reals, :boolean, :string, :unit or :integer.
ACCESSOR: symbol of the function that reads the property from an entity."
  (key :x :type keyword :read-only t)
  (name "" :type string :read-only t)
  (type :real :type keyword :read-only t)
  (accessor nil :type symbol :read-only t))

(defstruct (entity-format (:constructor make-entity-format (kind
                                                            name
                                                            constructor
                                                            fields)))
  "How one entity kind is stored.

KIND: entity kind keyword.
NAME: name of the entity list in the file.
CONSTRUCTOR: symbol of the validating constructor, called with keyword arguments.
FIELDS: list of FIELD-SPEC for every property except id and layer."
  (kind :line :type keyword :read-only t)
  (name "" :type string :read-only t)
  (constructor nil :type symbol :read-only t)
  (fields nil :type list :read-only t))

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
                                                           'dom:text-string))))
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
                                                           'dom:dimension-precision)))))
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
