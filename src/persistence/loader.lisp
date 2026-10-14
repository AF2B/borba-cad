(in-package #:borba-cad/persistence)

;;;; Interpretation of the text read by the reader: strict validation of every field, then
;;;; construction of the drawing. Top-level fields must appear in the order the writer emits
;;;; them, which lets the entities section be read one entity at a time (bounded memory).

(defconstant +maximum-identifier+ 4611686018427387903
  "Largest accepted entity or layer id (2^62 - 1, the largest fixnum on SBCL x86-64).")

(defun call-with-content-errors (node
                                 thunk)
  "Call THUNK, converting domain errors it signals into content errors located at NODE.

NODE: the element being interpreted.
THUNK: a function of no arguments."
  (handler-case (funcall thunk)
    (base:borba-cad-error (condition)
      (content-failure node
                       "~a"
                       condition))))

(defun node-number (node)
  "Return the double-float value of a number node; integers are accepted and converted.

NODE: an element."
  (case (node-kind node)
    (:real (node-value node))
    (:integer (handler-case (coerce (node-value node) 'double-float)
                (arithmetic-error () (content-failure node "the number is too large"))))
    (t (content-failure node "expected a number"))))

(defun node-integer (node
                     minimum
                     maximum)
  "Return the value of an integer node within a range.

NODE: an element.
MINIMUM: smallest accepted value.
MAXIMUM: largest accepted value."
  (unless (eq (node-kind node) :integer)
    (content-failure node "expected a whole number"))
  (unless (<= minimum (node-value node) maximum)
    (content-failure node
                     "~d is outside the allowed range ~d to ~d"
                     (node-value node)
                     minimum
                     maximum))
  (node-value node))

(defun node-boolean (node)
  "Return true for the word t and false for the word nil.

NODE: an element."
  (cond ((and (eq (node-kind node) :word) (string= (node-value node) "t")) t)
        ((and (eq (node-kind node) :word) (string= (node-value node) "nil")) nil)
        (t (content-failure node "expected t or nil"))))

(defun node-string (node)
  "Return the text of a string node.

NODE: an element."
  (unless (eq (node-kind node) :string)
    (content-failure node "expected a quoted string"))
  (node-value node))

(defun node-unit (node)
  "Return the length unit keyword named by a word node.

NODE: an element."
  (unless (eq (node-kind node) :word)
    (content-failure node "expected a unit name"))
  (or (find (node-value node) (base:supported-length-units)
            :key (lambda (unit) (string-downcase (symbol-name unit)))
            :test #'string=)
      (content-failure node
                       "unknown unit ~s; use one of ~{~(~a~)~^, ~}"
                       (node-value node)
                       (base:supported-length-units))))

(defun single-value (value-nodes
                     field-node)
  "Return the only value of a field, or fail when there is not exactly one.

VALUE-NODES: the elements after the field name.
FIELD-NODE: the whole field, for error positions."
  (unless (= 1 (length value-nodes))
    (content-failure field-node "this field needs exactly one value"))
  (first value-nodes))

(defun parse-field-value (spec
                          value-nodes
                          field-node)
  "Interpret the value elements of one field according to its type.

SPEC: the FIELD-SPEC.
VALUE-NODES: elements after the field name.
FIELD-NODE: the whole field, for error positions."
  (ecase (field-spec-type spec)
    (:real (node-number (single-value value-nodes field-node)))
    (:integer (node-integer (single-value value-nodes field-node)
                            0
                            most-positive-fixnum))
    (:entity-id (node-integer (single-value value-nodes field-node)
                              1
                              +maximum-identifier+))
    (:layer-id (node-integer (single-value value-nodes field-node)
                             0
                             +maximum-identifier+))
    (:boolean (node-boolean (single-value value-nodes field-node)))
    (:string (node-string (single-value value-nodes field-node)))
    (:unit (node-unit (single-value value-nodes field-node)))
    (:reals (let ((values (make-array (length value-nodes) :element-type 'double-float)))
              (loop for node in value-nodes
                    for index from 0
                    do (setf (aref values index) (node-number node)))
              values))
    (:rgb (progn
            (unless (= 3 (length value-nodes))
              (content-failure field-node "a color needs three numbers: red green blue"))
            (mapcar (lambda (node) (node-integer node
                                                 0
                                                 255)) value-nodes)))))

(defvar *format-version* +supported-format-version+
  "The format version of the file being read: fields and entity kinds that it predates are not in it.")

(defun parse-named-fields (node
                           all-specs
                           owner)
  "Interpret NODE, a list (head (name value...)...), as a set of named fields. Unknown,
repeated and missing fields are errors. A field that the version of the file predates is not
allowed in it and takes its default value, which is how an older file is migrated. Returns a
property list from field keys to values.

NODE: the list element.
ALL-SPECS: list of FIELD-SPEC the list may contain; all that the file's version has are required.
OWNER: description of the list used in error messages."
  (let ((seen (make-hash-table :test 'equal))
        (specs (remove-if (lambda (spec) (> (field-spec-since spec) *format-version*)) all-specs))
        (values '()))
    (dolist (field-node (rest (node-value node)))
      (unless (eq (node-kind field-node) :list)
        (content-failure field-node
                         "expected a field such as (name value) inside ~a"
                         owner))
      (let ((name-node (first (node-value field-node))))
        (unless (and name-node (eq (node-kind name-node) :word))
          (content-failure field-node "a field must start with its name"))
        (let ((spec (find (node-value name-node) specs :key #'field-spec-name :test #'string=)))
          (unless spec
            (content-failure name-node
                             "unknown field ~s in ~a"
                             (node-value name-node)
                             owner))
          (when (gethash (field-spec-name spec) seen)
            (content-failure name-node
                             "field ~s appears twice in ~a"
                             (field-spec-name spec)
                             owner))
          (setf (gethash (field-spec-name spec) seen) t)
          (setf (getf values (field-spec-key spec))
                (parse-field-value spec
                                   (rest (node-value field-node))
                                   field-node)))))
    (dolist (spec specs)
      (unless (gethash (field-spec-name spec) seen)
        (content-failure node
                         "missing field ~s in ~a"
                         (field-spec-name spec)
                         owner)))
    (dolist (spec all-specs)
      (when (> (field-spec-since spec) *format-version*)
        (setf (getf values (field-spec-key spec)) (field-spec-default spec))))
    values))

(defparameter *common-entity-fields*
  (list (make-field-spec :id
                         "id"
                         :entity-id
                         nil)
        (make-field-spec :layer-id
                         "layer"
                         :layer-id
                         nil))
  "Fields every entity has, ahead of its kind-specific ones.")

(defparameter *layer-fields*
  (list (make-field-spec :id
                         "id"
                         :layer-id
                         nil)
        (make-field-spec :name
                         "name"
                         :string
                         nil)
        (make-field-spec :color
                         "color"
                         :rgb
                         nil)
        (make-field-spec :visible
                         "visible"
                         :boolean
                         nil)
        (make-field-spec :locked
                         "locked"
                         :boolean
                         nil))
  "Fields of a layer.")

(defparameter *grid-fields*
  (list (make-field-spec :spacing
                         "spacing"
                         :real
                         nil)
        (make-field-spec :visible
                         "visible"
                         :boolean
                         nil)
        (make-field-spec :snap
                         "snap"
                         :boolean
                         nil)
        (make-field-spec :object-snap
                         "object-snap"
                         :boolean
                         nil))
  "Fields of the grid section.")

(defun expect-field (source
                     name)
  "Read the next top-level field and check that its name is NAME; returns the list element.

SOURCE: the source.
NAME: the field name required at this point."
  (skip-blanks source)
  (unless (eql (source-peek source) #\()
    (syntax-failure source
                    "expected the field (~a ...)"
                    name))
  (let* ((node (read-node source 1))
         (head (first (node-value node))))
    (unless (and head (eq (node-kind head) :word) (string= (node-value head) name))
      (content-failure node
                       "expected the field (~a ...) here"
                       name))
    node))

(defun field-single-value (node)
  "Return the only value element of a top-level field.

NODE: the field element, for example (format-version 1)."
  (single-value (rest (node-value node)) node))

(defun check-format-version (node)
  "Check the format-version field, signaling DRAWING-FILE-VERSION-ERROR for versions this
program cannot read, and return the version.

NODE: the (format-version N) element."
  (let ((value-node (field-single-value node)))
    (unless (eq (node-kind value-node) :integer)
      (content-failure value-node "the format version must be a whole number"))
    (let ((version (node-value value-node)))
      (cond ((> version +supported-format-version+)
             (error 'base:drawing-file-version-error
                    :path *file-path*
                    :line (node-line value-node)
                    :column (node-column value-node)
                    :reason (format nil "format version ~d is newer than the version ~d this program ~
                                         reads; open it with a newer Borba CAD"
                                    version
                                    +supported-format-version+)))
            ((< version +oldest-supported-format-version+)
             (error 'base:drawing-file-version-error
                    :path *file-path*
                    :line (node-line value-node)
                    :column (node-column value-node)
                    :reason (format nil "format version ~d is not supported; this program reads versions ~d to ~d"
                                    version
                                    +oldest-supported-format-version+
                                    +supported-format-version+)))
            (t version)))))

(defun parse-layers (node
                     drawing)
  "Restore every layer listed in the (layers ...) element into DRAWING.

NODE: the layers element.
DRAWING: the drawing being built."
  (let ((seen (make-hash-table)))
    (dolist (layer-node (rest (node-value node)))
      (unless (and (eq (node-kind layer-node) :list)
                   (let ((head (first (node-value layer-node))))
                     (and head (eq (node-kind head) :word) (string= (node-value head) "layer"))))
        (content-failure layer-node "expected a layer such as (layer (id 0) ...)"))
      (let* ((fields (parse-named-fields layer-node
                                         *layer-fields*
                                         "layer"))
             (id (getf fields :id)))
        (when (gethash id seen)
          (content-failure layer-node
                           "layer id ~d appears twice"
                           id))
        (setf (gethash id seen) t)
        (call-with-content-errors
         layer-node
         (lambda ()
           (destructuring-bind (red green blue) (getf fields :color)
             (dom:restore-layer drawing
                                id
                                (getf fields :name)
                                (dom:make-rgb red
                                              green
                                              blue)
                                (getf fields :visible)
                                (getf fields :locked)))))))
    (unless (gethash dom:+default-layer-id+ seen)
      (content-failure node
                       "the default layer (id ~d) is missing"
                       dom:+default-layer-id+))))

(defun parse-entity (node
                     drawing
                     next-entity-id
                     seen-ids)
  "Interpret one entity element and build the entity.

NODE: the entity element, for example (line (id 1) ...).
DRAWING: the drawing whose layers the entity may refer to.
NEXT-ENTITY-ID: the file's next-entity-id; every id must be below it.
SEEN-IDS: hash table of entity ids already read."
  (unless (eq (node-kind node) :list)
    (content-failure node "expected an entity such as (line (id 1) ...)"))
  (let ((head (first (node-value node))))
    (unless (and head (eq (node-kind head) :word))
      (content-failure node "an entity must start with its type name"))
    (let ((entity-format (find-format-by-name (node-value head))))
      (unless (and entity-format (<= (entity-format-since entity-format) *format-version*))
        (content-failure head
                         "unknown entity type ~s"
                         (node-value head)))
      (let* ((fields (parse-named-fields node
                                         (append *common-entity-fields* (entity-format-fields entity-format))
                                         (entity-format-name entity-format)))
             (id (getf fields :id))
             (layer-id (getf fields :layer-id)))
        (when (gethash id seen-ids)
          (content-failure node
                           "entity id ~d appears twice"
                           id))
        (setf (gethash id seen-ids) t)
        (unless (< id next-entity-id)
          (content-failure node
                           "entity id ~d is not below next-entity-id ~d"
                           id
                           next-entity-id))
        (unless (dom:find-layer drawing layer-id)
          (content-failure node
                           "entity ~d refers to the missing layer ~d"
                           id
                           layer-id))
        (call-with-content-errors node
                                  (lambda ()
                                    (apply (entity-format-constructor entity-format) fields)))))))

(defun read-entities-section (source
                              drawing
                              next-entity-id)
  "Read the (entities ...) section one entity at a time; returns the entities in file order.

SOURCE: the source, positioned before the section.
DRAWING: the drawing being built.
NEXT-ENTITY-ID: the file's next-entity-id."
  (skip-blanks source)
  (unless (eql (source-peek source) #\()
    (syntax-failure source "expected the field (entities ...)"))
  (let ((line (source-line source))
        (column (source-column source))
        (seen-ids (make-hash-table))
        (entities '()))
    (source-advance source)
    (let ((head (read-node source 1)))
      (unless (and (eq (node-kind head) :word) (string= (node-value head) "entities"))
        (content-failure head "expected the field (entities ...) here")))
    (loop
      (skip-blanks source)
      (let ((character (source-peek source)))
        (cond ((null character)
               (syntax-failure source
                               "the list opened at line ~d, column ~d is never closed"
                               line
                               column))
              ((char= character #\))
               (source-advance source)
               (return (nreverse entities)))
              (t (push (parse-entity (read-node source 2)
                                     drawing
                                     next-entity-id
                                     seen-ids)
                       entities)))))))

(defun parse-drawing-source (source)
  "Read and validate a whole drawing from SOURCE.

SOURCE: the source."
  (skip-blanks source)
  (unless (eql (source-peek source) #\()
    (syntax-failure source "a drawing file must start with (drawing"))
  (source-advance source)
  (let ((head (read-node source 1))
        (drawing (dom:make-drawing)))
    (unless (and (eq (node-kind head) :word) (string= (node-value head) "drawing"))
      (content-failure head "a drawing file must start with (drawing ...)"))
    (setf *format-version* (check-format-version (expect-field source "format-version")))
    (let ((unit-node (expect-field source "unit")))
      (unless (eq :millimeter (node-unit (field-single-value unit-node)))
        (content-failure unit-node "only millimeter is supported as the storage unit")))
    (let* ((display-unit (node-unit (field-single-value (expect-field source "display-unit"))))
           (grid-node (expect-field source "grid"))
           (grid (parse-named-fields grid-node
                                     *grid-fields*
                                     "grid"))
           (text-height-node (expect-field source "default-text-height"))
           (text-height (node-number (field-single-value text-height-node)))
           (current-layer-node (expect-field source "current-layer"))
           (current-layer (node-integer (field-single-value current-layer-node)
                                        0
                                        +maximum-identifier+))
           (next-layer-node (expect-field source "next-layer-id"))
           (next-layer-id (node-integer (field-single-value next-layer-node)
                                        1
                                        +maximum-identifier+))
           (next-entity-id (node-integer (field-single-value (expect-field source "next-entity-id"))
                                         1
                                         +maximum-identifier+)))
      (unless (dom:valid-grid-spacing-p (getf grid :spacing))
        (content-failure grid-node
                         "the grid spacing must be between ~f and ~f mm"
                         dom:+minimum-grid-spacing+
                         base:+maximum-coordinate-magnitude+))
      (unless (base:positive-length-p text-height)
        (content-failure text-height-node "the default text height must be a positive length"))
      (parse-layers (expect-field source "layers") drawing)
      (unless (dom:find-layer drawing current-layer)
        (content-failure current-layer-node
                         "the current layer ~d does not exist"
                         current-layer))
      (when (<= next-layer-id (reduce #'max (mapcar #'dom:layer-id (dom:layers-in-order drawing))))
        (content-failure next-layer-node
                         "next-layer-id ~d is not above every layer id"
                         next-layer-id))
      (dom:restore-drawing-state drawing
                                 :current-layer-id current-layer
                                 :next-entity-id next-entity-id
                                 :next-layer-id next-layer-id
                                 :display-unit display-unit
                                 :grid-spacing (getf grid :spacing)
                                 :grid-visible (getf grid :visible)
                                 :snap-to-grid (getf grid :snap)
                                 :object-snap (getf grid :object-snap)
                                 :default-text-height text-height)
      (let ((entities (read-entities-section source
                                             drawing
                                             next-entity-id)))
        (skip-blanks source)
        (unless (eql (source-peek source) #\))
          (syntax-failure source "expected the closing parenthesis of the drawing"))
        (source-advance source)
        (skip-blanks source)
        (when (source-peek source)
          (syntax-failure source "unexpected content after the end of the drawing"))
        (dom:apply-changes drawing (dom:create-entities-changes entities))))
    drawing))

(defun parse-drawing-text (text
                           &key path)
  "Parse and validate drawing text; returns a new drawing. Signals a DRAWING-FILE-ERROR
subclass naming the line and column of the first problem. Nothing is evaluated.

TEXT: the file contents.
PATH: file name used in error messages, or NIL."
  (let ((*file-path* path)
        (*format-version* +supported-format-version+)
        (source (make-source (coerce text '(simple-array character (*))))))
    (when (and (plusp (length (source-text source)))
               (char= (schar (source-text source) 0) (code-char #xFEFF)))
      (incf (source-position source)))
    (parse-drawing-source source)))
