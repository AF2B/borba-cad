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

(defun node-pattern (node)
  "Return the hatch pattern keyword named by a word node.

NODE: an element."
  (unless (eq (node-kind node) :word)
    (content-failure node "expected a hatch pattern name"))
  (or (find (node-value node) dom:*hatch-patterns*
            :key (lambda (pattern) (string-downcase (symbol-name pattern)))
            :test #'string=)
      (content-failure node
                       "unknown hatch pattern ~s; use one of ~{~(~a~)~^, ~}"
                       (node-value node)
                       dom:*hatch-patterns*)))

(defvar *reading-drawing* nil
  "The drawing being read, which the placements of blocks look their blocks up in.")

(defun node-block (node)
  "Return the block definition that a string node names, from the blocks read so far.

NODE: an element."
  (unless (eq (node-kind node) :string)
    (content-failure node "expected the name of a block as a quoted string"))
  (or (and *reading-drawing* (dom:find-block *reading-drawing* (node-value node)))
      (content-failure node
                       "unknown block ~s; a block must be defined in the (blocks ...) section"
                       (node-value node))))

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
    (:pattern (node-pattern (single-value value-nodes field-node)))
    (:block (node-block (single-value value-nodes field-node)))
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

(defun parse-block-origin (node)
  "Interpret a (generator \"name\" (arg \"parameter\" 1.0) ...) element of a block.

NODE: the generator element."
  (let ((values (rest (node-value node))))
    (when (null values)
      (content-failure node "a generator needs its name, and then what it was given"))
    (let ((generator (node-string (first values)))
          (seen '()))
      (call-with-content-errors
       node
       (lambda ()
         (dom:make-block-origin generator
                                (loop for argument in (rest values)
                                      collect (progn
                                                (unless (and (equal "arg" (head-name argument))
                                                             (= 3 (length (node-value argument))))
                                                  (content-failure argument
                                                                   "expected (arg \"parameter\" number) here"))
                                                (let ((name (node-string (second (node-value argument)))))
                                                  (when (member name seen :test #'string=)
                                                    (content-failure argument
                                                                     "the parameter ~s appears twice"
                                                                     name))
                                                  (push name seen)
                                                  (cons name (node-number (third (node-value argument)))))))))))))

(defun parse-block (node
                    drawing)
  "Interpret one (block (name ...) (entities ...)) element and add the block to the drawing.

NODE: the block element.
DRAWING: the drawing being built."
  (unless (and (eq (node-kind node) :list)
               (let ((head (first (node-value node))))
                 (and head (eq (node-kind head) :word) (string= (node-value head) "block"))))
    (content-failure node "expected a block such as (block (name \"door\") (entities ...))"))
  (let* ((fields (rest (node-value node)))
         (generator-field (and (>= *format-version* 8)
                               (= 3 (length fields))
                               (second fields)))
         (fields (if generator-field
                     (list (first fields) (third fields))
                     fields)))
    (unless (and (= 2 (length fields))
                 (every (lambda (field) (eq (node-kind field) :list)) fields)
                 (or (null generator-field) (equal "generator" (head-name generator-field))))
      (content-failure node "a block has two fields: (name ...) and (entities ...)"))
    (destructuring-bind (name-field entities-field) fields
      (let ((name-head (first (node-value name-field)))
            (entities-head (first (node-value entities-field))))
        (unless (and name-head (eq (node-kind name-head) :word) (string= (node-value name-head) "name"))
          (content-failure name-field "expected the field (name ...) here"))
        (unless (and entities-head (eq (node-kind entities-head) :word)
                     (string= (node-value entities-head) "entities"))
          (content-failure entities-field "expected the field (entities ...) here"))
        (let* ((name (node-string (single-value (rest (node-value name-field)) name-field)))
               (seen-ids (make-hash-table))
               (entities (mapcar (lambda (entity-node)
                                   (parse-entity entity-node
                                                 drawing
                                                 (1+ +maximum-identifier+)
                                                 seen-ids))
                                 (rest (node-value entities-field)))))
          (call-with-content-errors node
                                    (lambda ()
                                      (when (dom:find-block drawing name)
                                        (content-failure node
                                                         "block ~s is defined twice"
                                                         name))
                                      (dom:define-block drawing
                                                        (dom:make-block-definition
                                                         :name name
                                                         :entities entities
                                                         :origin (and generator-field
                                                                      (parse-block-origin generator-field)))))))))))

(defun parse-blocks (node
                     drawing)
  "Read every block of the (blocks ...) element into the drawing.

NODE: the blocks element.
DRAWING: the drawing being built."
  (dolist (block-node (rest (node-value node)))
    (parse-block block-node drawing)))

(defparameter *viewport-fields*
  (real-fields "x"
               nil
               "y"
               nil
               "width"
               nil
               "height"
               nil
               "center-x"
               nil
               "center-y"
               nil
               "scale"
               nil)
  "Fields of a viewport of a sheet.")

(defun head-name (node)
  "Return the name that starts a list element, or NIL when it is not a list that starts with a word.

NODE: an element."
  (let ((head (and (eq (node-kind node) :list) (first (node-value node)))))
    (and head (eq (node-kind head) :word) (node-value head))))

(defun parse-viewport (node)
  "Interpret one (viewport (x ...) ...) element.

NODE: the viewport element."
  (unless (equal "viewport" (head-name node))
    (content-failure node "expected a viewport such as (viewport (x 0.0) ...)"))
  (let ((fields (parse-named-fields node
                                    *viewport-fields*
                                    "viewport")))
    (call-with-content-errors node
                              (lambda ()
                                (dom:make-viewport :x (getf fields :x)
                                                   :y (getf fields :y)
                                                   :width (getf fields :width)
                                                   :height (getf fields :height)
                                                   :center-x (getf fields :center-x)
                                                   :center-y (getf fields :center-y)
                                                   :scale (getf fields :scale))))))

(defun parse-title-fields (node)
  "Read the (fields (project \"...\") ...) element of a sheet into an alist.

NODE: the fields element."
  (let ((seen '()))
    (dolist (field-node (rest (node-value node)))
      (let* ((name (head-name field-node))
             (key (and name (find name dom:*title-field-keys* :key #'string-downcase :test #'string=))))
        (unless key
          (content-failure field-node
                           "unknown field of the title block; use one of ~{~(~a~)~^, ~}"
                           dom:*title-field-keys*))
        (when (assoc key seen)
          (content-failure field-node
                           "the field ~(~a~) appears twice"
                           key))
        (push (cons key (node-string (single-value (rest (node-value field-node)) field-node))) seen)))
    (nreverse seen)))

(defun parse-sheet (node)
  "Interpret one (sheet (name ...) (paper ...) (landscape ...) (fields ...) (viewports ...)) element.

NODE: the sheet element."
  (unless (equal "sheet" (head-name node))
    (content-failure node "expected a sheet such as (sheet (name \"A\") ...)"))
  (let ((name nil)
        (paper nil)
        (landscape :unset)
        (fields :unset)
        (viewports :unset))
    (dolist (child (rest (node-value node)))
      (let ((head (head-name child)))
        (flet ((once (current)
                 (unless (or (null current) (eq current :unset))
                   (content-failure child
                                    "the field ~a appears twice in the sheet"
                                    head))))
          (cond ((equal head "name")
                 (once name)
                 (setf name (node-string (single-value (rest (node-value child)) child))))
                ((equal head "paper")
                 (once paper)
                 (let ((word (single-value (rest (node-value child)) child)))
                   (setf paper (or (and (eq (node-kind word) :word)
                                        (find (node-value word) dom:*sheet-papers*
                                              :key #'string-downcase
                                              :test #'string=))
                                   (content-failure word
                                                    "unknown paper; use one of ~{~(~a~)~^, ~}"
                                                    dom:*sheet-papers*)))))
                ((equal head "landscape")
                 (once landscape)
                 (setf landscape (node-boolean (single-value (rest (node-value child)) child))))
                ((equal head "fields")
                 (once fields)
                 (setf fields (parse-title-fields child)))
                ((equal head "viewports")
                 (once viewports)
                 (setf viewports (mapcar #'parse-viewport (rest (node-value child)))))
                (t (content-failure child "unknown field in a sheet; expected name, paper, landscape, fields or ~
viewports"))))))
    (when (or (null name) (null paper) (eq landscape :unset) (eq fields :unset) (eq viewports :unset))
      (content-failure node "a sheet has five fields: name, paper, landscape, fields and viewports"))
    (call-with-content-errors node
                              (lambda ()
                                (dom:make-sheet :name name
                                                :paper paper
                                                :landscape landscape
                                                :fields fields
                                                :viewports viewports)))))

(defun parse-sheets (node
                     drawing)
  "Read every sheet of the (sheets ...) element into the drawing.

NODE: the sheets element.
DRAWING: the drawing being built."
  (call-with-content-errors node
                            (lambda ()
                              (dom:set-sheets drawing (mapcar #'parse-sheet (rest (node-value node)))))))

(defparameter *underlay-field-names*
  '("id" "name" "kind" "page" "dpi" "size" "position" "rotation" "scale" "opacity" "visible" "data")
  "The fields of an underlay, all of which it has.")

(defun underlay-data-bytes (node)
  "Return the bytes that the (data \"...\" ...) field of an underlay holds as Base64 text.

NODE: the data field."
  (let ((text (with-output-to-string (stream)
                (dolist (line (rest (node-value node)))
                  (write-string (node-string line) stream)))))
    (or (base:base64-decode text)
        (content-failure node "the picture is not valid Base64 text"))))

(defun parse-underlay (node)
  "Interpret one (underlay (id ...) (name ...) ...) element.

NODE: the underlay element."
  (unless (equal "underlay" (head-name node))
    (content-failure node "expected an underlay such as (underlay (id 1) ...)"))
  (let ((fields (make-hash-table :test #'equal)))
    (dolist (child (rest (node-value node)))
      (let ((head (head-name child)))
        (unless (member head *underlay-field-names* :test #'equal)
          (content-failure child
                           "unknown field in an underlay; expected ~{~a~^, ~}"
                           *underlay-field-names*))
        (when (gethash head fields)
          (content-failure child
                           "the field ~a appears twice in the underlay"
                           head))
        (setf (gethash head fields) child)))
    (dolist (name *underlay-field-names*)
      (unless (gethash name fields)
        (content-failure node
                         "an underlay has the fields ~{~a~^, ~}; ~a is missing"
                         *underlay-field-names*
                         name)))
    (flet ((values-of (name)
             (rest (node-value (gethash name fields))))
           (field (name)
             (gethash name fields)))
      (let* ((kind-node (single-value (values-of "kind") (field "kind")))
             (kind (or (and (eq (node-kind kind-node) :word)
                            (find (node-value kind-node) dom:*underlay-kinds*
                                  :key #'string-downcase
                                  :test #'string=))
                       (content-failure kind-node
                                        "unknown kind of underlay; use one of ~{~(~a~)~^, ~}"
                                        dom:*underlay-kinds*)))
             (dpi-node (single-value (values-of "dpi") (field "dpi")))
             (size (values-of "size"))
             (position (values-of "position")))
        (unless (= 2 (length size))
          (content-failure (field "size") "the size is two whole numbers: width and height"))
        (unless (= 2 (length position))
          (content-failure (field "position") "the position is two numbers: x and y"))
        (let ((id (node-integer (single-value (values-of "id") (field "id"))
                                0
                                +maximum-identifier+))
              (name (node-string (single-value (values-of "name") (field "name"))))
              (page (node-integer (single-value (values-of "page") (field "page"))
                                  1
                                  dom:+maximum-underlay-page+))
              (dpi (if (and (eq (node-kind dpi-node) :word) (string= (node-value dpi-node) "auto"))
                       nil
                       (node-number dpi-node)))
              (pixel-width (node-integer (first size)
                                         1
                                         dom:+maximum-underlay-side+))
              (pixel-height (node-integer (second size)
                                          1
                                          dom:+maximum-underlay-side+))
              (x (node-number (first position)))
              (y (node-number (second position)))
              (rotation (node-number (single-value (values-of "rotation") (field "rotation"))))
              (scale (node-number (single-value (values-of "scale") (field "scale"))))
              (opacity (node-number (single-value (values-of "opacity") (field "opacity"))))
              (visible (node-boolean (single-value (values-of "visible") (field "visible"))))
              (data (underlay-data-bytes (field "data"))))
          (call-with-content-errors node
                                    (lambda ()
                                      (dom:make-underlay :id id
                                                         :name name
                                                         :kind kind
                                                         :data data
                                                         :page page
                                                         :dpi dpi
                                                         :pixel-width pixel-width
                                                         :pixel-height pixel-height
                                                         :x x
                                                         :y y
                                                         :rotation rotation
                                                         :scale scale
                                                         :opacity opacity
                                                         :visible visible))))))))

(defun parse-underlays (node
                        drawing)
  "Read every underlay of the (underlays ...) element into the drawing.

NODE: the underlays element.
DRAWING: the drawing being built."
  (call-with-content-errors node
                            (lambda ()
                              (dom:set-underlays drawing (mapcar #'parse-underlay (rest (node-value node)))))))

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
    (setf *format-version* (check-format-version (expect-field source "format-version"))
          *reading-drawing* drawing)
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
      (when (>= *format-version* 5)
        (parse-blocks (expect-field source "blocks") drawing))
      (when (>= *format-version* 7)
        (parse-sheets (expect-field source "sheets") drawing))
      (when (>= *format-version* 9)
        (parse-underlays (expect-field source "underlays") drawing))
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
        (*reading-drawing* nil)
        (*format-version* +supported-format-version+)
        (source (make-source (coerce text '(simple-array character (*))))))
    (when (and (plusp (length (source-text source)))
               (char= (schar (source-text source) 0) (code-char #xFEFF)))
      (incf (source-position source)))
    (parse-drawing-source source)))
