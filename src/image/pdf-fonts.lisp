(in-package #:borba-cad/image)

;;;; The fonts of a PDF page (ISO 32000-1, section 9): a simple font, whose codes are bytes and whose glyphs are found
;;;; by encoding and name or by character map, and a composite font, whose codes go through a CMap to CIDs and from
;;;; them to glyphs. Only fonts that carry their outlines are drawn, as TrueType (FontFile2, or an OpenType file) or
;;;; as CFF (FontFile3); a font that is left out of the file, a Type 1 font program (FontFile) and a Type 3 font are
;;;; not drawn, but their widths still move the text along. A font gives, for the bytes of a string, the outline,
;;;; the width and the nature of each glyph.

(defconstant +default-glyph-width+ 500
  "The width, in thousandths of an em, that a glyph is given when the font says none.")

(defconstant +symbolic-font-flag+ 4
  "The bit of the flags of a font descriptor that says the font is symbolic.")

(defconstant +maximum-cmap-entries+ 200000
  "How many ranges and characters a CMap may map before the program calls it too large.")

(defstruct (pdf-font (:conc-name font-))
  "A font of a page ready to show text.

KIND: :TRUETYPE, :CFF or :NONE for a font whose outlines are not there.
PROGRAM: the TRUETYPE-FONT or the CFF-FONT.
COMPOSITE: true for a Type 0 font.
ENCODING: for a simple font, the vector of 256 glyph names, or NIL.
SYMBOLIC: true when the font descriptor says the font is symbolic.
WIDTHS: a function of a code (simple) or a CID (composite) that returns the width in thousandths of an em.
CMAP: for a composite font, the function that reads a code from a string, as a list (codespaces ranges chars).
CID-TO-GID: for a composite TrueType font, NIL or :IDENTITY for CID = glyph, or the bytes of the map.
SCALE: how many units of text space a unit of the font is."
  (kind :none :type keyword)
  (program nil)
  (composite nil)
  (encoding nil)
  (symbolic nil)
  (widths nil)
  (cmap nil)
  (cid-to-gid nil)
  (scale 0.001d0 :type double-float))

;;; The widths

(defun simple-font-widths (document
                           dictionary
                           descriptor)
  "Return the function of a code that gives the width of a glyph of a simple font, from the Widths of its dictionary.

DOCUMENT: the document.
DICTIONARY: the font dictionary.
DESCRIPTOR: its font descriptor, or NIL."
  (let* ((first (let ((value (pdf-get document
                                      dictionary
                                      "FirstChar"))) (if (integerp value) value 0)))
         (widths (pdf-get document
                          dictionary
                          "Widths"))
         (missing (let ((value (pdf-get document
                                        descriptor
                                        "MissingWidth")))
                    (if (realp value) value +default-glyph-width+))))
    (lambda (code)
      (let ((entry (and (vectorp widths) (<= 0 (- code first) (1- (length widths)))
                        (pdf-resolve document (aref widths (- code first))))))
        (if (realp entry) entry missing)))))

(defun composite-font-widths (document
                              descendant)
  "Return the function of a CID that gives the width of a glyph of a composite font, from the W and DW entries.

DOCUMENT: the document.
DESCENDANT: the dictionary of the descendant font."
  (let ((table (make-hash-table))
        (default (let ((value (pdf-get document
                                       descendant
                                       "DW"))) (if (realp value) value 1000)))
        (entries (pdf-get document
                          descendant
                          "W")))
    (when (vectorp entries)
      (let ((items (map 'list (lambda (item) (pdf-resolve document item)) entries)))
        (loop while items
              do (let ((first (pop items))
                       (second (pop items)))
                   (cond ((and (integerp first) (vectorp second))
                          (loop for width across second
                                for cid from first
                                do (let ((width (pdf-resolve document width)))
                                     (when (realp width)
                                       (setf (gethash cid table) width)))))
                         ((and (integerp first) (integerp second) items)
                          (let ((width (pop items)))
                            (when (and (realp width) (<= (- second first) +maximum-cmap-entries+))
                              (loop for cid from first to second
                                    do (setf (gethash cid table) width))))))))))
    (lambda (cid)
      (gethash cid table default))))

;;; The encoding

(defun simple-font-encoding (document
                             dictionary
                             embedded)
  "Return the vector of 256 glyph names that the Encoding of a simple font makes, or NIL when the font has none and
keeps its own.

DOCUMENT: the document.
DICTIONARY: the font dictionary.
EMBEDDED: true when the font carries its outlines, whose own encoding is then what a missing Encoding means."
  (let ((encoding (pdf-get document
                           dictionary
                           "Encoding")))
    (flet ((with-differences (base differences)
             (let ((names (copy-seq (or base (make-array 256 :initial-element nil))))
                   (code 0))
               (when (vectorp differences)
                 (loop for item across differences
                       do (let ((item (pdf-resolve document item)))
                            (cond ((integerp item)
                                   (setf code item))
                                  ((and (stringp item) (<= 0 code 255))
                                   (setf (svref names code) item)
                                   (incf code))))))
               names)))
      (cond ((stringp encoding)
             (named-encoding encoding))
            ((hash-table-p encoding)
             (let* ((base-name (pdf-get document
                                        encoding
                                        "BaseEncoding"))
                    (base (or (and (stringp base-name) (named-encoding base-name))
                              (and (not embedded) *standard-encoding*))))
               (with-differences base (pdf-get document
                                               encoding
                                               "Differences"))))
            (t
             (and (not embedded) *standard-encoding*))))))

;;; CMaps

(defun parse-cmap (bytes)
  "Read an embedded CMap into a list of its codespace ranges, as (length low high), its ranges of CIDs, as (low high
cid), and its single mappings, as (code cid).

BYTES: the decoded stream."
  (let ((reader (make-reader-on bytes))
        (codespaces '())
        (ranges '())
        (chars '())
        (previous '())
        (total 0))
    (flet ((code-of (octets)
             (and (typep octets 'octets)
                  (let ((value 0))
                    (loop for byte across octets do (setf value (+ (* value 256) byte)))
                    value))))
      (loop
        (let ((token (handler-case (read-pdf-value reader :references nil)
                       (base:image-error () :eof))))
          (cond ((eq token :eof)
                 (return))
                ((pdf-operator-p token)
                 (let ((name (cdr token))
                       (items (reverse previous)))
                   (setf previous '())
                   (cond ((string= name "endcodespacerange")
                          (loop for (low high) on items by #'cddr
                                while high
                                do (push (list (length low) (code-of low) (code-of high)) codespaces)))
                         ((string= name "endcidrange")
                          (loop for (low high cid) on items by #'cdddr
                                while cid
                                do (when (< (incf total) +maximum-cmap-entries+)
                                     (push (list (code-of low) (code-of high) cid) ranges))))
                         ((string= name "endcidchar")
                          (loop for (code cid) on items by #'cddr
                                while cid
                                do (when (< (incf total) +maximum-cmap-entries+)
                                     (push (list (code-of code) cid) chars)))))))
                (t
                 (push token previous))))))
    (list (nreverse codespaces) (nreverse ranges) (nreverse chars))))

(defun cmap-next-code (cmap
                       bytes
                       position)
  "Read the code at a position of a string shown with a composite font, and return the code, its length and its CID.

CMAP: what PARSE-CMAP returned, or NIL for Identity-H.
BYTES: the string.
POSITION: where the code begins."
  (let* ((available (- (length bytes) position))
         (length (if cmap
                     (or (loop for size from 1 to (min 4 available)
                               do (let ((code (let ((value 0))
                                                (dotimes (index size value)
                                                  (setf value (+ (* value 256) (aref bytes (+ position index))))))))
                                    (when (some (lambda (space)
                                                  (and (= (first space) size) (<= (second space) code (third space))))
                                                (first cmap))
                                      (return size))))
                         (min 2 available))
                     (min 2 available)))
         (code (let ((value 0))
                 (dotimes (index length value)
                   (setf value (+ (* value 256) (aref bytes (+ position index))))))))
    (values code
            length
            (if cmap
                (or (let ((range (find-if (lambda (entry) (<= (first entry) code (second entry))) (second cmap))))
                      (and range (+ (third range) (- code (first range)))))
                    (let ((char (find code (third cmap) :key #'first)))
                      (and char (second char)))
                    0)
                code))))

;;; Loading

(defun font-program-bytes (document
                           descriptor
                           key)
  "Return the decoded bytes of the font program under KEY of a font descriptor, or NIL.

DOCUMENT: the document.
DESCRIPTOR: the font descriptor.
KEY: FontFile, FontFile2 or FontFile3."
  (let ((object (and descriptor (pdf-object-of document (gethash key descriptor)))))
    (and object (object-stream-bytes document object))))

(defun load-font-program (document
                          descriptor)
  "Return the font program of a font descriptor, as two values: :TRUETYPE or :CFF and the parsed font; or :NONE and
NIL when it is not there or is not of a kind this program reads.

DOCUMENT: the document.
DESCRIPTOR: the font descriptor, or NIL."
  (handler-case
      (let ((truetype (font-program-bytes document
                                          descriptor
                                          "FontFile2"))
            (other (font-program-bytes document
                                       descriptor
                                       "FontFile3")))
        (cond (truetype
               (values :truetype (parse-truetype truetype)))
              ((and other (>= (length other) 4) (= (aref other 0) 1))
               (values :cff (parse-cff other)))
              ((and other (>= (length other) 12) (zerop (aref other 0)) (= (aref other 1) 1))
               (values :truetype (parse-truetype other)))
              ((and other (>= (length other) 12) (equalp (subseq other 0 4) (map 'octets #'char-code "OTTO")))
               (let ((cff (assoc "CFF " (read-table-directory other) :test #'string=)))
                 (if cff
                     (values :cff (parse-cff (subseq other (second cff) (+ (second cff) (third cff)))))
                     (values :none nil))))
              (t
               (values :none nil))))
    (base:image-error ()
      (values :none nil))))

(defun load-pdf-font (document
                      dictionary)
  "Return the PDF-FONT that a font dictionary describes. Never signals: a font that cannot be read gives a font with no
outlines, whose text is left out.

DOCUMENT: the document.
DICTIONARY: the font dictionary."
  (let* ((subtype (pdf-get document
                           dictionary
                           "Subtype"))
         (composite (equal subtype "Type0"))
         (descendant (and composite
                          (let ((list (pdf-get document
                                               dictionary
                                               "DescendantFonts")))
                            (and (vectorp list)
                                 (plusp (length list))
                                 (pdf-resolve document (aref list 0))))))
         (descriptor (pdf-get document
                              (if composite descendant dictionary)
                              "FontDescriptor")))
    (multiple-value-bind (kind program)
        (load-font-program document descriptor)
      (let ((font (make-pdf-font :kind kind
                                 :program program
                                 :composite composite
                                 :symbolic (let ((flags (pdf-get document
                                                                 descriptor
                                                                 "Flags")))
                                             (and (integerp flags) (logtest flags +symbolic-font-flag+)))
                                 :scale (case kind
                                          (:truetype (/ 1d0 (truetype-units-per-em program)))
                                          (:cff (first (cff-matrix program)))
                                          (t 0.001d0)))))
        (if composite
            (let ((encoding (pdf-get document
                                     dictionary
                                     "Encoding"))
                  (map (pdf-get document
                                descendant
                                "CIDToGIDMap")))
              (setf (font-widths font) (composite-font-widths document descendant)
                    (font-cmap font) (let ((object (and (not (stringp encoding))
                                                        (pdf-object-of document (gethash "Encoding" dictionary)))))
                                       (and object
                                            (let ((bytes (object-stream-bytes document object)))
                                              (and bytes (parse-cmap bytes)))))
                    (font-cid-to-gid font) (let ((object (and descendant
                                                              (pdf-object-of document
                                                                             (gethash "CIDToGIDMap" descendant)))))
                                             (if object
                                                 (object-stream-bytes document object)
                                                 (if (equal map "Identity") :identity nil)))))
            (setf (font-widths font) (simple-font-widths document
                                                         dictionary
                                                         descriptor)
                  (font-encoding font) (simple-font-encoding document
                                                             dictionary
                                                             (not (eq kind :none)))))
        font))))

;;; Glyphs

(defun truetype-simple-glyph (font
                              code)
  "Return the glyph that a code of a simple TrueType font stands for.

FONT: the PDF-FONT.
CODE: the character code."
  (let* ((program (font-program font))
         (name (and (font-encoding font) (svref (font-encoding font) code)))
         (unicode (and name (glyph-name-unicode name)))
         (symbolic-first (or (font-symbolic font) (null name))))
    (flet ((unicode-glyph ()
             (and unicode
                  (or (truetype-glyph-for program
                                          3
                                          1
                                          unicode)
                      (truetype-glyph-for program
                                          0
                                          3
                                          unicode)
                      (truetype-glyph-for program
                                          0
                                          4
                                          unicode)
                      (truetype-glyph-for program
                                          3
                                          10
                                          unicode))))
           (symbol-glyph ()
             (or (truetype-glyph-for program
                                     3
                                     0
                                     code)
                 (truetype-glyph-for program
                                     3
                                     0
                                     (+ #xF000 code))
                 (truetype-glyph-for program
                                     3
                                     0
                                     (+ #xF100 code))
                 (truetype-glyph-for program
                                     3
                                     0
                                     (+ #xF200 code))
                 (truetype-glyph-for program
                                     1
                                     0
                                     code))))
      (or (if symbolic-first
              (or (symbol-glyph) (unicode-glyph))
              (or (unicode-glyph) (symbol-glyph)))
          (and (< code (truetype-glyph-count program)) code)
          0))))

(defun cff-simple-glyph (font
                         code)
  "Return the glyph that a code of a simple CFF font stands for.

FONT: the PDF-FONT.
CODE: the character code."
  (let* ((program (font-program font))
         (name (and (font-encoding font) (svref (font-encoding font) code))))
    (or (and name (cff-glyph-named program name))
        (cdr (assoc code (cff-encoding program)))
        (let ((standard (svref *standard-encoding* code)))
          (and standard (cff-glyph-named program standard)))
        (and (cff-cid-keyed program) (cff-glyph-of-cid program code))
        0)))

(defun composite-glyph (font
                        cid)
  "Return the glyph that a CID of a composite font stands for.

FONT: the PDF-FONT.
CID: the CID."
  (let ((program (font-program font)))
    (ecase (font-kind font)
      (:truetype
       (let ((map (font-cid-to-gid font)))
         (if (typep map 'octets)
             (if (< (1+ (* 2 cid)) (length map)) (octet-word map (* 2 cid)) 0)
             cid)))
      (:cff
       (or (cff-glyph-of-cid program cid) 0)))))

(defun font-outline (font
                     glyph)
  "Return the commands that draw a glyph of a font, in the units of the font.

FONT: the PDF-FONT.
GLYPH: the glyph number."
  (ecase (font-kind font)
    (:truetype (truetype-outline (font-program font) glyph))
    (:cff (cff-outline (font-program font) glyph))
    (:none '())))

(defun font-string-glyphs (font
                           bytes)
  "Return the glyphs of a string shown with a font, as a list of (commands width space-p): the commands that draw the
glyph in the units of the font, its width in thousandths of an em, and true when the code is the single-byte code 32.

FONT: the PDF-FONT.
BYTES: the string."
  (let ((glyphs '())
        (position 0))
    (loop while (< position (length bytes))
          do (if (font-composite font)
                 (multiple-value-bind (code length cid)
                     (cmap-next-code (font-cmap font)
                                     bytes
                                     position)
                   (declare (ignore code))
                   (incf position length)
                   (push (list (if (eq (font-kind font) :none)
                                   '()
                                   (font-outline font (composite-glyph font cid)))
                               (funcall (font-widths font) cid)
                               nil)
                         glyphs))
                 (let ((code (aref bytes position)))
                   (incf position)
                   (push (list (case (font-kind font)
                                 (:truetype (font-outline font (truetype-simple-glyph font code)))
                                 (:cff (font-outline font (cff-simple-glyph font code)))
                                 (t '()))
                               (funcall (font-widths font) code)
                               (= code 32))
                         glyphs))))
    (nreverse glyphs)))
