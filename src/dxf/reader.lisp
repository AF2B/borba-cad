(in-package #:borba-cad/dxf)

;;;; Reading a DXF file as what it is made of: groups, a number (the group code) on one line and a value on
;;;; the next, that make up records (a header variable, a layer, an entity), that make up sections.
;;;; Nothing is evaluated and every limit is checked, because a DXF file is data from elsewhere.

(defconstant +maximum-groups+ 12000000
  "Most groups (pairs of lines) a DXF file may have: well above the largest drawing the program is meant
to open, and enough to bound the memory a file can ask for.")

(defconstant +maximum-block-depth+ 8
  "Most levels of blocks inside blocks that are followed when a drawing is imported.")

(defparameter *binary-dxf-signature* "AutoCAD Binary DXF"
  "The text that a binary DXF file begins with.")

(defstruct (record (:constructor make-record (type
                                              pairs
                                              line)))
  "One record of a DXF file: group 0 gives its type, and the groups after it, up to the next group 0, its
data.

TYPE: the type, a string such as \"LINE\", or the name of a header variable such as \"$INSUNITS\".
PAIRS: the groups after it, as (code . value) conses with the value as a string, in order.
LINE: the line of the file the record starts on.
CHILDREN: for a polyline, its vertex records, in order."
  type
  pairs
  line
  (children nil))

(defstruct (dxf-block (:constructor make-dxf-block (name
                                                    base-x
                                                    base-y
                                                    records)))
  "A block: drawing that an INSERT repeats.

NAME: the name of the block.
BASE-X: x of its base point.
BASE-Y: y of its base point.
RECORDS: the entity records in it."
  name
  base-x
  base-y
  records)

(defstruct (dxf-document (:constructor make-dxf-document (header
                                                          layers
                                                          blocks
                                                          entities)))
  "What a DXF file holds that a drawing is made from.

HEADER: the header variables, as records.
LAYERS: the layer records of the layer table.
BLOCKS: the blocks, as DXF-BLOCK values.
ENTITIES: the entity records of the entities section, in order."
  header
  layers
  blocks
  entities)

;;; Encoding

(defun valid-utf-8-p (octets)
  "Return true when OCTETS are well-formed UTF-8.

OCTETS: a vector of bytes."
  (let ((index 0)
        (length (length octets)))
    (loop while (< index length)
          do (let* ((byte (aref octets index))
                    (continuation (cond ((< byte #x80) 0)
                                        ((<= #xC2 byte #xDF) 1)
                                        ((<= #xE0 byte #xEF) 2)
                                        ((<= #xF0 byte #xF4) 3)
                                        (t (return-from valid-utf-8-p nil)))))
               (when (> (+ index continuation) (1- length))
                 (return-from valid-utf-8-p nil))
               (loop for offset from 1 to continuation
                     unless (= #x80 (logand #xC0 (aref octets (+ index offset))))
                       do (return-from valid-utf-8-p nil))
               (incf index (1+ continuation))))
    t))

(defun decode-dxf-octets (octets)
  "Return the text of a DXF file given its bytes: UTF-8 when they are well-formed UTF-8 (which pure
ASCII is), and Windows Western European otherwise, the code page that DXF files from Brazil have
been written in. Signals DRAWING-FILE-ERROR for a binary DXF file.

OCTETS: the bytes of the file."
  (let ((signature (map 'vector #'char-code *binary-dxf-signature*)))
    (when (and (>= (length octets) (length signature))
               (every #'= signature octets))
      (error 'base:drawing-file-error
             :reason (i18n:tr "is a binary DXF file, which is not supported; save it as an ASCII DXF"))))
  (let ((start (if (and (>= (length octets) 3)
                        (= #xEF (aref octets 0))
                        (= #xBB (aref octets 1))
                        (= #xBF (aref octets 2)))
                   3
                   0)))
    (sbcl:octets->string (if (zerop start) octets (subseq octets start))
                         (if (valid-utf-8-p octets) :utf-8 :cp1252))))

;;; Numbers

(defun parse-real (text)
  "Return the number written in TEXT as a double-float, or NIL when it is not one: an optional sign,
digits with a decimal point, and an optional exponent, which is how DXF files write numbers.

TEXT: a string."
  (let* ((string (string-trim '(#\Space #\Tab) text))
         (length (length string))
         (index 0)
         (sign 1)
         (mantissa 0)
         (scale 0)
         (digits 0))
    (when (zerop length)
      (return-from parse-real nil))
    (case (char string 0)
      (#\- (setf sign -1) (incf index))
      (#\+ (incf index)))
    (loop while (and (< index length) (digit-char-p (char string index)))
          do (setf mantissa (+ (* 10 mantissa) (digit-char-p (char string index))))
             (incf digits)
             (incf index))
    (when (and (< index length) (char= #\. (char string index)))
      (incf index)
      (loop while (and (< index length) (digit-char-p (char string index)))
            do (setf mantissa (+ (* 10 mantissa) (digit-char-p (char string index))))
               (incf digits)
               (decf scale)
               (incf index)))
    (when (zerop digits)
      (return-from parse-real nil))
    (when (and (< index length) (char-equal #\e (char string index)))
      (let ((exponent (parse-integer string :start (1+ index) :junk-allowed nil :radix 10)))
        (setf scale (+ scale exponent)
              index length)))
    (and (= index length)
         (<= -400 scale 400)
         (let ((value (* sign mantissa (expt 10 scale))))
           (and (< (abs value) (expt 10 300))
                (float value 1d0))))))

;;; Groups, records and sections

(defun line-bounds (text
                    start)
  "Return the end of the line that begins at START, and where the next one begins, as two values; a
line ends at a line feed, a carriage return, or both.

TEXT: the file text.
START: index where the line begins."
  (let* ((length (length text))
         (end (or (position-if (lambda (character) (member character '(#\Newline #\Return))) text :start start)
                  length))
         (next (cond ((>= end length) length)
                     ((and (char= #\Return (char text end))
                           (< (1+ end) length)
                           (char= #\Newline (char text (1+ end))))
                      (+ end 2))
                     (t (1+ end)))))
    (values end next)))

(defun group-failure (line
                      control
                      &rest arguments)
  "Signal DRAWING-FILE-ERROR for a problem at a line of a DXF file.

LINE: the line number.
CONTROL: format control of the reason.
ARGUMENTS: format arguments."
  (error 'base:drawing-file-error
         :reason (i18n:trf "line ~d: ~a"
                           line
                           (apply #'i18n:trf control arguments))))

(defun read-groups (text)
  "Return the groups of a DXF file as a list of (code value line) lists, in order. A line that is blank where
a group code belongs is skipped, which is how the blank lines that many programs leave at the end of the
file, or between sections, are read; and the group 0 EOF ends the file, so that whatever follows it is never
looked at.

TEXT: the file text."
  (let ((groups '())
        (count 0)
        (position 0)
        (line 1)
        (length (length text)))
    (loop while (< position length)
          do (multiple-value-bind (code-end code-next)
                 (line-bounds text position)
               (let* ((code-text (string-trim '(#\Space #\Tab) (subseq text position code-end)))
                      (code (and (plusp (length code-text))
                                 (every #'digit-char-p (string-left-trim "-" code-text))
                                 (parse-integer code-text))))
                 (cond ((zerop (length code-text))
                        (setf position code-next
                              line (1+ line)))
                       (t
                        (unless code
                          (group-failure line
                                         "expected a group code, a whole number, found ~s"
                                         code-text))
                        (when (>= code-next length)
                          (group-failure line "the file ends after a group code, without its value"))
                        (multiple-value-bind (value-end value-next)
                            (line-bounds text code-next)
                          (let ((value (subseq text code-next value-end)))
                            (when (= code 0)
                              (setf value (string-trim '(#\Space #\Tab) value)))
                            (push (list code value line) groups)
                            (when (> (incf count) +maximum-groups+)
                              (group-failure line
                                             "the file has more than ~d groups"
                                             +maximum-groups+))
                            (when (and (= code 0) (string= value "EOF"))
                              (return))
                            (setf position value-next
                                  line (+ line 2)))))))))
    (nreverse groups)))

(defun group-records (groups)
  "Split the groups of a DXF file into sections, each a list of records, and return them as an alist
from the name of the section to its records. The groups of the header, which are not led by a group
0, are made records named for the variable.

GROUPS: the groups, as read by READ-GROUPS."
  (let ((sections '())
        (section-name nil)
        (records '())
        (current nil))
    (flet ((close-record ()
             (when current
               (setf (record-pairs current) (nreverse (record-pairs current)))
               (push current records)
               (setf current nil)))
           (close-section ()
             (when section-name
               (push (cons section-name (nreverse records)) sections))
             (setf section-name nil
                   records '())))
      (let ((remaining groups))
        (loop while remaining
              do (destructuring-bind (code value line) (pop remaining)
                   (cond ((and (= code 0) (string= value "EOF"))
                          (close-record)
                          (close-section)
                          (return))
                         ((and (= code 0) (string= value "SECTION"))
                          (close-record)
                          (close-section)
                          (let ((name-group (pop remaining)))
                            (unless (and name-group (= 2 (first name-group)))
                              (group-failure line "a SECTION must be followed by its name"))
                            (setf section-name (string-trim '(#\Space #\Tab) (second name-group)))))
                         ((and (= code 0) (string= value "ENDSEC"))
                          (close-record)
                          (close-section))
                         ((and (= code 0) section-name)
                          (close-record)
                          (setf current (make-record value
                                                     nil
                                                     line)))
                         ((and (= code 9) (equal section-name "HEADER"))
                          (close-record)
                          (setf current (make-record value
                                                     nil
                                                     line)))
                         (current
                          (push (cons code value) (record-pairs current))))))
        (close-record)
        (close-section)))
    (nreverse sections)))

(defun group-children (records)
  "Return RECORDS with the vertices of every POLYLINE attached to it as its children, and the
attributes of an INSERT dropped: the records between a POLYLINE or an INSERT and its SEQEND.

RECORDS: entity records in order."
  (let ((result '())
        (remaining records))
    (loop while remaining
          do (let ((record (pop remaining)))
               (cond ((member (record-type record) '("POLYLINE" "INSERT") :test #'string=)
                      (let ((children '()))
                        (loop while (and remaining (not (string= "SEQEND" (record-type (first remaining))))
                                         (member (record-type (first remaining)) '("VERTEX" "ATTRIB") :test #'string=))
                              do (push (pop remaining) children))
                        (when (and remaining (string= "SEQEND" (record-type (first remaining))))
                          (pop remaining))
                        (setf (record-children record) (nreverse children))
                        (push record result)))
                     ((string= "SEQEND" (record-type record)))
                     (t (push record result)))))
    (nreverse result)))

(defun pair-value (pairs
                   code)
  "Return the first value with a group code in PAIRS, as a string, or NIL.

PAIRS: a list of (code . value) conses.
CODE: the group code."
  (cdr (assoc code pairs)))

(defun pair-values (pairs
                    code)
  "Return every value with a group code in PAIRS, in order.

PAIRS: a list of (code . value) conses.
CODE: the group code."
  (loop for (key . value) in pairs
        when (= key code)
          collect value))

(defun pair-real (pairs
                  code
                  &optional default)
  "Return the number that the first group with a code in PAIRS holds, or DEFAULT when there is no such
group or it is not a number.

PAIRS: a list of (code . value) conses.
CODE: the group code.
DEFAULT: the value when the group is missing."
  (let ((value (pair-value pairs code)))
    (or (and value (parse-real value)) default)))

(defun pair-integer (pairs
                     code
                     &optional default)
  "Return the whole number that the first group with a code in PAIRS holds, or DEFAULT.

PAIRS: a list of (code . value) conses.
CODE: the group code.
DEFAULT: the value when the group is missing or is not a whole number."
  (let ((value (pair-value pairs code)))
    (or (and value
             (let ((trimmed (string-trim '(#\Space #\Tab) value)))
               (and (plusp (length trimmed))
                    (every #'digit-char-p (string-left-trim "-+" trimmed))
                    (parse-integer trimmed))))
        default)))

(defun blocks-from-records (records)
  "Return the blocks defined by the records of the blocks section, as a list of DXF-BLOCK values.

RECORDS: the records of the section, in order."
  (let ((blocks '())
        (current nil)
        (members '()))
    (flet ((close-block ()
             (when current
               (setf (dxf-block-records current) (group-children (nreverse members)))
               (push current blocks)
               (setf current nil
                     members '()))))
      (dolist (record records)
        (cond ((string= (record-type record) "BLOCK")
               (close-block)
               (setf current (make-dxf-block (or (pair-value (record-pairs record) 2) "")
                                             (pair-real (record-pairs record)
                                                        10
                                                        0d0)
                                             (pair-real (record-pairs record)
                                                        20
                                                        0d0)
                                             nil)))
              ((string= (record-type record) "ENDBLK")
               (close-block))
              (current
               (push record members))))
      (close-block))
    (nreverse blocks)))

(defun parse-dxf-text (text)
  "Read the text of an ASCII DXF file and return the DXF-DOCUMENT that has what a drawing is made
from: the header variables, the layers, the blocks and the entities. Signals DRAWING-FILE-ERROR,
naming the line, when the text is not a DXF file.

TEXT: the text of the file."
  (let* ((sections (group-records (read-groups text)))
         (header (cdr (assoc "HEADER" sections :test #'string=)))
         (tables (cdr (assoc "TABLES" sections :test #'string=)))
         (blocks (cdr (assoc "BLOCKS" sections :test #'string=)))
         (entities (cdr (assoc "ENTITIES" sections :test #'string=))))
    (unless (or header tables entities blocks)
      (error 'base:drawing-file-error
             :reason (i18n:tr "is not a DXF file: it has no header, tables, blocks or entities")))
    (make-dxf-document header
                       (remove "LAYER" tables :key #'record-type :test-not #'string=)
                       (blocks-from-records blocks)
                       (group-children entities))))

(defun header-pairs (document
                     name)
  "Return the groups of a header variable of DOCUMENT, or NIL when it has none.

DOCUMENT: a DXF-DOCUMENT.
NAME: the name of the variable, such as \"$INSUNITS\"."
  (let ((record (find name (dxf-document-header document) :key #'record-type :test #'string=)))
    (and record (record-pairs record))))

(defun document-units (document)
  "Return the unit in which the drawing of DOCUMENT is made, as :millimeter, :centimeter or :meter,
:inch or :foot, or NIL when the file does not say (no $INSUNITS, or 0, which is unitless) or says one
that the program does not take.

DOCUMENT: a DXF-DOCUMENT."
  (case (pair-integer (header-pairs document "$INSUNITS")
                      70
                      0)
    (1 :inch)
    (2 :foot)
    (4 :millimeter)
    (5 :centimeter)
    (6 :meter)
    (t nil)))
