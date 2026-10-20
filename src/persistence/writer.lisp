(in-package #:borba-cad/persistence)

(defconstant +largest-exact-integer-double+ 1d15
  "Integral doubles below this magnitude are printed through the integer printer, which is much
faster than the general float printer and prints the same digits.")

(defun format-double (value)
  "Return the shortest decimal text that reads back as exactly VALUE, bit for bit (including
negative zero). Never uses a reader macro or exponent marker other than e.

VALUE: a finite double-float."
  (declare (type double-float value))
  (cond ((zerop value)
         (if (minusp (float-sign value)) "-0.0" "0.0"))
        ((and (< (abs value) +largest-exact-integer-double+)
              (= value (ftruncate value)))
         (format nil "~d.0" (truncate value)))
        (t (let ((*read-default-float-format* 'double-float)
                 (*print-readably* nil)
                 (*print-pretty* nil))
             (prin1-to-string value)))))

(defun write-quoted-string (string
                            stream)
  "Write STRING between quotes, escaping quotes and backslashes.

STRING: the text.
STREAM: output stream."
  (write-char #\" stream)
  (loop for character across string
        do (when (member character '(#\" #\\))
             (write-char #\\ stream))
           (write-char character stream))
  (write-char #\" stream))

(defun write-field-value (type
                          value
                          stream)
  "Write one property value according to its storage TYPE.

TYPE: :real, :reals, :boolean, :string, :unit, :pattern, :block or :integer.
VALUE: the property value.
STREAM: output stream."
  (ecase type
    (:real (write-string (format-double value) stream))
    (:reals (loop for index from 0 below (length value)
                  do (when (plusp index) (write-char #\Space stream))
                     (write-string (format-double (aref value index)) stream)))
    (:boolean (write-string (if value "t" "nil") stream))
    (:string (write-quoted-string value stream))
    (:unit (write-string (string-downcase (symbol-name value)) stream))
    (:pattern (write-string (string-downcase (symbol-name value)) stream))
    (:block (write-quoted-string (dom:block-definition-name value) stream))
    (:integer (format stream "~d" value))))

(defun write-entity (entity
                     stream)
  "Write one entity as a single line.

ENTITY: the entity.
STREAM: output stream."
  (let ((entity-format (find-format-by-kind (dom:entity-kind entity))))
    (format stream "  (~a (id ~d) (layer ~d)"
            (entity-format-name entity-format)
            (dom:entity-id entity)
            (dom:entity-layer-id entity))
    (dolist (field (entity-format-fields entity-format))
      (format stream " (~a " (field-spec-name field))
      (write-field-value (field-spec-type field)
                         (funcall (field-spec-accessor field) entity)
                         stream)
      (write-char #\) stream))
    (write-char #\) stream)
    (terpri stream)))

(defun write-layer (layer
                    stream)
  "Write one layer as a single line.

LAYER: the layer.
STREAM: output stream."
  (let ((color (dom:layer-color layer)))
    (format stream "  (layer (id ~d) (name " (dom:layer-id layer))
    (write-quoted-string (dom:layer-name layer) stream)
    (format stream ") (color ~d ~d ~d) (visible ~a) (locked ~a))~%"
            (dom:rgb-red color)
            (dom:rgb-green color)
            (dom:rgb-blue color)
            (if (dom:layer-visible layer) "t" "nil")
            (if (dom:layer-locked layer) "t" "nil"))))

(defun write-block (definition
                    stream)
  "Write one block definition: its name and its entities.

DEFINITION: the block definition.
STREAM: output stream."
  (format stream "  (block (name ")
  (write-quoted-string (dom:block-definition-name definition) stream)
  (format stream ")~%   (entities~%")
  (dolist (entity (dom:block-definition-entities definition))
    (write-string "   " stream)
    (write-entity entity stream))
  (format stream "   ))~%"))

(defun write-viewport (viewport
                       stream)
  "Write one viewport of a sheet as a single line.

VIEWPORT: the viewport.
STREAM: output stream."
  (format stream "    (viewport (x ~a) (y ~a) (width ~a) (height ~a) (center-x ~a) (center-y ~a) (scale ~a))~%"
          (format-double (dom:viewport-x viewport))
          (format-double (dom:viewport-y viewport))
          (format-double (dom:viewport-width viewport))
          (format-double (dom:viewport-height viewport))
          (format-double (dom:viewport-center-x viewport))
          (format-double (dom:viewport-center-y viewport))
          (format-double (dom:viewport-scale viewport))))

(defun write-sheet (sheet
                    stream)
  "Write one sheet: its name, its paper, its title block and its viewports.

SHEET: the sheet.
STREAM: output stream."
  (format stream "  (sheet (name ")
  (write-quoted-string (dom:sheet-name sheet) stream)
  (format stream ") (paper ~(~a~)) (landscape ~a)~%"
          (dom:sheet-paper sheet)
          (if (dom:sheet-landscape sheet) "t" "nil"))
  (format stream "   (fields")
  (loop for (key . text) in (dom:sheet-fields sheet)
        do (format stream " (~(~a~) " key)
           (write-quoted-string text stream)
           (write-char #\) stream))
  (format stream ")~%   (viewports~%")
  (dolist (viewport (dom:sheet-viewports sheet))
    (write-viewport viewport stream))
  (format stream "   ))~%"))

(defun write-drawing (drawing
                      stream)
  "Write DRAWING in the .bcad format (the newest version).

DRAWING: the drawing.
STREAM: character output stream; UTF-8 is expected."
  (format stream "(drawing~%")
  (format stream " (format-version ~d)~%" +supported-format-version+)
  (format stream " (unit ~(~a~))~%" (dom:drawing-unit drawing))
  (format stream " (display-unit ~(~a~))~%" (dom:drawing-display-unit drawing))
  (format stream " (grid (spacing ~a) (visible ~a) (snap ~a) (object-snap ~a))~%"
          (format-double (dom:drawing-grid-spacing drawing))
          (if (dom:drawing-grid-visible drawing) "t" "nil")
          (if (dom:drawing-snap-to-grid drawing) "t" "nil")
          (if (dom:drawing-object-snap drawing) "t" "nil"))
  (format stream " (default-text-height ~a)~%" (format-double (dom:drawing-default-text-height drawing)))
  (format stream " (current-layer ~d)~%" (dom:drawing-current-layer-id drawing))
  (format stream " (next-layer-id ~d)~%" (dom:drawing-next-layer-id drawing))
  (format stream " (next-entity-id ~d)~%" (dom:drawing-next-entity-id drawing))
  (format stream " (layers~%")
  (dolist (layer (dom:layers-in-order drawing))
    (write-layer layer stream))
  (format stream " )~%")
  (format stream " (blocks~%")
  (dolist (definition (dom:block-definitions drawing))
    (write-block definition stream))
  (format stream " )~%")
  (format stream " (sheets~%")
  (dolist (sheet (dom:drawing-sheets drawing))
    (write-sheet sheet stream))
  (format stream " )~%")
  (format stream " (entities~%")
  (dom:map-entities (lambda (entity) (write-entity entity stream))
                    (dom:drawing-store drawing))
  (format stream " ))~%")
  drawing)

(defun drawing-to-string (drawing)
  "Return DRAWING serialized as a string.

DRAWING: the drawing."
  (with-output-to-string (stream)
    (write-drawing drawing stream)))
