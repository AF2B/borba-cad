(in-package #:borba-cad/image)

;;;; The structure of a PDF file (ISO 32000-1, section 7): its indirect objects, found by reading the file from the
;;;; start, not through the cross-reference tables, so that a file whose tables are damaged or missing still opens;
;;;; the objects kept inside object streams; the filters that code a stream; and the tree of pages. A password-
;;;; protected file is named and refused.

(defconstant +pdf-stream-limit+ (* 400 1024 1024)
  "The most bytes a stream of a PDF may expand to.")

(defconstant +pdf-maximum-page-depth+ 32
  "How deep the tree of pages may go.")

(defparameter *default-page-box* '(0 0 612 792)
  "The page box of a page that names none: a Letter sheet, in points.")

(defconstant +pdf-reference-chain+ 32
  "How many references in a row are followed before the program calls it a loop.")

(defstruct (pdf-object (:conc-name object-))
  "An indirect object: its value, and when it is a stream, where its data lies in the file."
  (number 0 :type fixnum)
  (value nil)
  (stream-start nil)
  (stream-end nil))

(defstruct (pdf-document (:conc-name document-))
  "A PDF file read: its bytes, its indirect objects by number, and the dictionaries of its trailers."
  (data (make-octets 0) :type octets)
  (objects (make-hash-table) :type hash-table)
  (trailers '()))

(defparameter *pdf-keyword-stream*
  (map 'octets #'char-code "stream")
  "The bytes of the word that opens the data of a stream.")

(defparameter *pdf-keyword-endstream*
  (map 'octets #'char-code "endstream")
  "The bytes of the word that closes the data of a stream.")

(defparameter *pdf-keyword-obj*
  (map 'octets #'char-code "obj")
  "The bytes of the word that opens the value of an indirect object.")

(defparameter *pdf-keyword-trailer*
  (map 'octets #'char-code "trailer")
  "The bytes of the word that opens a trailer.")

(defun find-octets (data
                    pattern
                    start
                    end)
  "Return the position of the first place from START where PATTERN occurs in DATA before END, or NIL.

DATA: the bytes searched.
PATTERN: the bytes looked for.
START: where to begin.
END: the position after the last byte that may be part of a match."
  (declare (type octets data pattern)
           (type fixnum start end)
           (optimize (speed 3) (safety 1))
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((first (aref pattern 0))
        (size (length pattern)))
    (loop for index of-type fixnum from start to (- end size)
          do (when (and (= (aref data index) first)
                        (loop for offset of-type fixnum from 1 below size
                              always (= (aref data (+ index offset)) (aref pattern offset))))
               (return index)))))

;;; Reading the objects

(defun backward-digits (data
                        end)
  "Read the run of digits that ends just before END, and return its value and the position where it starts, or NIL
when there is none.

DATA: the bytes.
END: the position after the last digit."
  (declare (type octets data)
           (type fixnum end))
  (let ((start end))
    (loop while (and (> start 0) (digit-byte-p (aref data (1- start))))
          do (decf start))
    (when (and (< start end) (<= (- end start) +pdf-maximum-integer-digits+))
      (values (parse-integer (map 'string #'code-char (subseq data start end))) start))))

(defun object-header-before (data
                             at)
  "Return the number of the object whose header `12 0 obj` ends just before AT, or NIL when no header does.

DATA: the bytes.
AT: the position of the word obj."
  (declare (type octets data)
           (type fixnum at))
  (let ((position at))
    (flet ((skip-white-backwards ()
             (loop while (and (> position 0) (pdf-white-p (aref data (1- position))))
                   do (decf position))))
      (when (and (> position 0) (pdf-white-p (aref data (1- position))))
        (skip-white-backwards)
        (multiple-value-bind (generation generation-start)
            (backward-digits data position)
          (when generation
            (setf position generation-start)
            (when (and (> position 0) (pdf-white-p (aref data (1- position))))
              (skip-white-backwards)
              (multiple-value-bind (number number-start)
                  (backward-digits data position)
                (when (and number
                           (or (zerop number-start)
                               (not (pdf-regular-p (aref data (1- number-start))))))
                  number)))))))))

(defun stream-data-start (data
                          position
                          end)
  "Return the position where the data of a stream begins if the word stream is at POSITION, or NIL when it is not.

DATA: the bytes.
POSITION: where the word may be.
END: the position after the last byte of the file."
  (declare (type octets data)
           (type fixnum position end))
  (when (and (<= (+ position 6) end)
             (null (mismatch *pdf-keyword-stream* data :start2 position :end2 (+ position 6))))
    (let ((after (+ position 6)))
      (cond ((and (< (1+ after) end) (= (aref data after) 13) (= (aref data (1+ after)) 10))
             (+ after 2))
            ((and (< after end) (member (aref data after) '(10 13)))
             (1+ after))
            (t
             after)))))

(defun stream-data-end (data
                        start
                        end
                        length)
  "Return the position after the data of a stream that begins at START. The Length of the dictionary is believed when
the word endstream follows the data it counts, and the word is searched for when it is not.

DATA: the bytes.
START: where the data begins.
END: the position after the last byte of the file.
LENGTH: the Length entry of the dictionary when it is a plain integer, else NIL."
  (declare (type octets data)
           (type fixnum start end))
  (or (and (integerp length)
           (<= 0 length (- end start))
           (let ((reader (make-reader-on data
                                         :start
                                         (+ start length)
                                         :end
                                         end)))
             (skip-pdf-white reader)
             (and (<= (+ (reader-position reader) 9) end)
                  (null (mismatch *pdf-keyword-endstream* data
                                  :start2 (reader-position reader)
                                  :end2 (+ (reader-position reader) 9)))
                  (+ start length))))
      (let ((found (find-octets data
                                *pdf-keyword-endstream*
                                start
                                end)))
        (cond ((null found)
               end)
              ((and (> found start) (= (aref data (1- found)) 10))
               (if (and (> found (1+ start)) (= (aref data (- found 2)) 13))
                   (- found 2)
                   (1- found)))
              ((and (> found start) (= (aref data (1- found)) 13))
               (1- found))
              (t
               found)))))

(defun scan-pdf-objects (document)
  "Find the indirect objects of the file by reading it from the start, and enter them in the document. An object
defined twice keeps its last definition, as an updated file means.

DOCUMENT: the document, holding the bytes."
  (let* ((data (document-data document))
         (end (length data))
         (position 0))
    (declare (type fixnum position end))
    (loop
      (let ((at (find-octets data
                             *pdf-keyword-obj*
                             position
                             end)))
        (unless at
          (return))
        (let ((number (and (or (>= (+ at 3) end) (not (pdf-regular-p (aref data (+ at 3)))))
                           (object-header-before data at))))
          (setf position (+ at 3))
          (when number
            (handler-case
                (let* ((reader (make-reader-on data
                                               :start
                                               (+ at 3)
                                               :end
                                               end))
                       (value (read-pdf-value reader)))
                  (unless (or (eq value :eof) (pdf-operator-p value))
                    (let ((object (make-pdf-object :number number :value value)))
                      (skip-pdf-white reader)
                      (let ((start (and (hash-table-p value)
                                        (stream-data-start data
                                                           (reader-position reader)
                                                           end))))
                        (when start
                          (let ((stop (stream-data-end data
                                                       start
                                                       end
                                                       (gethash "Length" value))))
                            (setf (object-stream-start object) start
                                  (object-stream-end object) stop
                                  position (max position stop)))))
                      (setf (gethash number (document-objects document)) object))))
              (base:image-error ()
                nil))))))))

(defun scan-pdf-trailers (document)
  "Find the trailer dictionaries of the file, those after the word trailer and those of cross-reference streams, and
keep them in the document.

DOCUMENT: the document."
  (let* ((data (document-data document))
         (end (length data))
         (position 0))
    (loop
      (let ((at (find-octets data
                             *pdf-keyword-trailer*
                             position
                             end)))
        (unless at
          (return))
        (setf position (+ at 7))
        (handler-case
            (let ((value (read-pdf-value (make-reader-on data
                                                         :start
                                                         position
                                                         :end
                                                         end))))
              (when (hash-table-p value)
                (push value (document-trailers document))))
          (base:image-error ()
            nil))))
    (maphash (lambda (number object)
               (declare (ignore number))
               (let ((value (object-value object)))
                 (when (and (hash-table-p value)
                            (equal (gethash "Type" value) "XRef"))
                   (push value (document-trailers document)))))
             (document-objects document))))

(defun pdf-resolve (document
                    value)
  "Follow references from VALUE to the value they lead to; an object that does not exist is null.

DOCUMENT: the document.
VALUE: a value that may be a reference."
  (let ((steps 0))
    (loop while (pdf-reference-p value)
          do (when (> (incf steps) +pdf-reference-chain+)
               (pdf-damaged))
             (let ((object (gethash (second value) (document-objects document))))
               (setf value (if object (object-value object) :null))))
    value))

(defun pdf-get (document
                dictionary
                key)
  "Return the value under KEY of a dictionary, with references followed, or NIL when the entry is missing or null.

DOCUMENT: the document.
DICTIONARY: a dictionary, or anything else for which the answer is NIL.
KEY: the name of the entry."
  (when (hash-table-p dictionary)
    (let ((value (pdf-resolve document (gethash key dictionary))))
      (unless (eq value :null)
        value))))

(defun pdf-object-of (document
                      value)
  "Return the indirect object a reference VALUE leads to, or NIL when VALUE is not a reference to one.

DOCUMENT: the document.
VALUE: what an entry holds."
  (let ((steps 0))
    (loop while (pdf-reference-p value)
          do (when (> (incf steps) +pdf-reference-chain+)
               (pdf-damaged))
             (let ((object (gethash (second value) (document-objects document))))
               (unless object
                 (return-from pdf-object-of nil))
               (when (not (pdf-reference-p (object-value object)))
                 (return-from pdf-object-of object))
               (setf value (object-value object))))
    nil))

;;; Filters

(defun ascii-hex-decode (bytes)
  "Decode the bytes of an ASCIIHexDecode stream.

BYTES: the coded bytes."
  (let ((out (make-array (floor (length bytes) 2) :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t))
        (high nil))
    (loop for byte across bytes
          do (cond ((= byte 62)
                    (return))
                   ((pdf-white-p byte))
                   ((digit-char-p (code-char byte) 16)
                    (let ((digit (digit-char-p (code-char byte) 16)))
                      (if high
                          (progn (vector-push-extend (+ (* 16 high) digit) out)
                                 (setf high nil))
                          (setf high digit))))
                   (t
                    (pdf-damaged))))
    (when high
      (vector-push-extend (* 16 high) out))
    (coerce out 'octets)))

(defun ascii-85-decode (bytes)
  "Decode the bytes of an ASCII85Decode stream.

BYTES: the coded bytes."
  (let ((out (make-array (length bytes) :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t))
        (group (make-array 5 :element-type 'fixnum :initial-element 84))
        (count 0)
        (start 0))
    (when (and (>= (length bytes) 2) (= (aref bytes 0) 60) (= (aref bytes 1) 126))
      (setf start 2))
    (flet ((flush (keep)
             (let ((value 0))
               (dotimes (index 5)
                 (setf value (+ (* value 85) (aref group index))))
               (loop for shift in '(24 16 8 0)
                     repeat keep
                     do (vector-push-extend (logand #xFF (ash value (- shift))) out)))
             (fill group 84)
             (setf count 0)))
      (loop for index from start below (length bytes)
            for byte = (aref bytes index)
            do (cond ((= byte 126)
                      (return))
                     ((pdf-white-p byte))
                     ((and (= byte 122) (zerop count))
                      (dotimes (each 4)
                        (vector-push-extend 0 out)))
                     ((<= 33 byte 117)
                      (setf (aref group count) (- byte 33))
                      (incf count)
                      (when (= count 5)
                        (flush 4)))
                     (t
                      (pdf-damaged))))
      (when (> count 1)
        (flush (1- count))))
    (coerce out 'octets)))

(defun run-length-decode (bytes)
  "Decode the bytes of a RunLengthDecode stream.

BYTES: the coded bytes."
  (let ((out (make-array (* 2 (length bytes)) :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t))
        (position 0))
    (loop while (< position (length bytes))
          do (let ((length (aref bytes position)))
               (incf position)
               (cond ((= length 128)
                      (return))
                     ((< length 128)
                      (let ((stop (min (length bytes) (+ position length 1))))
                        (loop for index from position below stop
                              do (vector-push-extend (aref bytes index) out))
                        (setf position stop)))
                     ((< position (length bytes))
                      (dotimes (each (- 257 length))
                        (vector-push-extend (aref bytes position) out))
                      (incf position)))
               (when (> (length out) +pdf-stream-limit+)
                 (image-failure "This picture is larger than the program reads."))))
    (coerce out 'octets)))

(defun lzw-decode (bytes
                   early-change)
  "Decode the bytes of an LZWDecode stream.

BYTES: the coded bytes.
EARLY-CHANGE: 1 when codes get one bit wider one code early, as PDF does unless told otherwise, else 0."
  (let ((prefix (make-array 4096 :element-type 'fixnum :initial-element -1))
        (suffix (make-array 4096 :element-type '(unsigned-byte 8) :initial-element 0))
        (lengths (make-array 4096 :element-type 'fixnum :initial-element 1))
        (out (make-array (* 4 (length bytes)) :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t))
        (next 258)
        (width 9)
        (previous -1)
        (buffer 0)
        (count 0))
    (dotimes (index 256)
      (setf (aref suffix index) index))
    (flet ((emit (code)
             (let* ((length (aref lengths code))
                    (base (length out)))
               (when (> (+ base length) +pdf-stream-limit+)
                 (image-failure "This picture is larger than the program reads."))
               (dotimes (each length)
                 (vector-push-extend 0 out))
               (let ((current code))
                 (loop for index from (+ base length -1) downto base
                       do (setf (aref out index) (aref suffix current)
                                current (aref prefix current))))
               (aref out base))))
      (loop for byte across bytes
            do (setf buffer (logior (ash buffer 8) byte)
                     count (+ count 8))
               (loop while (>= count width)
                     do (let ((code (ldb (byte width (- count width)) buffer)))
                          (decf count width)
                          (setf buffer (ldb (byte count 0) buffer))
                          (cond ((= code 256)
                                 (setf next 258
                                       width 9
                                       previous -1))
                                ((= code 257)
                                 (return-from lzw-decode (coerce out 'octets)))
                                (t
                                 (cond ((< code next)
                                        (let ((first (emit code)))
                                          (when (and (>= previous 0) (< next 4096))
                                            (setf (aref prefix next) previous
                                                  (aref suffix next) first
                                                  (aref lengths next) (1+ (aref lengths previous)))
                                            (incf next))))
                                       ((and (= code next) (>= previous 0) (< next 4096))
                                        (let ((first (let ((current previous))
                                                       (loop while (>= (aref prefix current) 0)
                                                             do (setf current (aref prefix current)))
                                                       (aref suffix current))))
                                          (setf (aref prefix next) previous
                                                (aref suffix next) first
                                                (aref lengths next) (1+ (aref lengths previous)))
                                          (incf next)
                                          (emit code)))
                                       (t
                                        (pdf-damaged)))
                                 (setf previous code)
                                 (setf width (cond ((>= (+ next early-change) 2048) 12)
                                                   ((>= (+ next early-change) 1024) 11)
                                                   ((>= (+ next early-change) 512) 10)
                                                   (t 9)))))))))
    (coerce out 'octets)))

(defun parameter-integer (document
                          parameters
                          key
                          default)
  "Return the integer under KEY of a dictionary of decode parameters, or DEFAULT.

DOCUMENT: the document.
PARAMETERS: the dictionary, or NIL.
KEY: the name of the entry.
DEFAULT: what to return when the entry is missing."
  (let ((value (pdf-get document
                        parameters
                        key)))
    (if (integerp value) value default)))

(defun undo-tiff-predictor (bytes
                            colors
                            bits
                            columns)
  "Undo the horizontal differencing of the TIFF predictor of a stream.

BYTES: the decoded bytes.
COLORS: samples per pixel.
BITS: bits per sample.
COLUMNS: pixels per row."
  (unless (member bits '(8 16))
    (image-failure "This PDF uses a coding of its pictures that the program does not read."))
  (let* ((bytes-per-sample (floor bits 8))
         (row-size (* colors bytes-per-sample columns))
         (step (* colors bytes-per-sample)))
    (when (plusp row-size)
      (loop for row-start from 0 below (length bytes) by row-size
            do (loop for index from (+ row-start step) below (min (length bytes) (+ row-start row-size))
                     do (setf (aref bytes index)
                              (logand #xFF (+ (aref bytes index) (aref bytes (- index step))))))))
    bytes))

(defun undo-png-predictor (bytes
                           colors
                           bits
                           columns)
  "Undo the PNG filters of a stream: each row is a filter byte and the row.

BYTES: the decoded bytes.
COLORS: samples per pixel.
BITS: bits per sample.
COLUMNS: pixels per row."
  (let* ((row-bytes (ceiling (* colors bits columns) 8))
         (stride (1+ row-bytes))
         (rows (ceiling (length bytes) stride))
         (padded (if (= (length bytes) (* rows stride))
                     bytes
                     (let ((copy (make-octets (* rows stride))))
                       (replace copy bytes)
                       copy)))
         (out (make-octets (* rows row-bytes))))
    (unfilter-rows padded
                   0
                   rows
                   row-bytes
                   (max 1 (ceiling (* colors bits) 8)))
    (dotimes (row rows)
      (replace out padded
               :start1 (* row row-bytes)
               :start2 (+ 1 (* row stride))
               :end2 (+ 1 (* row stride) row-bytes)))
    out))

(defun apply-predictor (document
                        bytes
                        parameters)
  "Undo the predictor that the decode parameters of a Flate or LZW stream name.

DOCUMENT: the document.
BYTES: the decoded bytes.
PARAMETERS: the dictionary of decode parameters, or NIL."
  (let ((predictor (parameter-integer document
                                      parameters
                                      "Predictor"
                                      1))
        (colors (parameter-integer document
                                   parameters
                                   "Colors"
                                   1))
        (bits (parameter-integer document
                                 parameters
                                 "BitsPerComponent"
                                 8))
        (columns (parameter-integer document
                                    parameters
                                    "Columns"
                                    1)))
    (unless (and (<= 1 colors 32) (member bits '(1 2 4 8 16)) (<= 1 columns 1000000))
      (pdf-damaged))
    (cond ((<= predictor 1) bytes)
          ((= predictor 2) (undo-tiff-predictor bytes
                                                colors
                                                bits
                                                columns))
          ((>= predictor 10) (undo-png-predictor bytes
                                                 colors
                                                 bits
                                                 columns))
          (t (pdf-damaged)))))

(defparameter *filter-abbreviations*
  '(("AHx" . "ASCIIHexDecode") ("A85" . "ASCII85Decode") ("LZW" . "LZWDecode") ("Fl" . "FlateDecode")
    ("RL" . "RunLengthDecode") ("CCF" . "CCITTFaxDecode") ("DCT" . "DCTDecode"))
  "The short names of the filters that inline images use, with the long names.")

(defparameter *image-filters*
  '("DCTDecode" "CCITTFaxDecode" "JBIG2Decode" "JPXDecode")
  "The filters whose output is a picture file or data of its own kind, which the decoding of a stream stops at.")

(defun stream-filters (document
                       dictionary)
  "Return the filters of a stream and their parameters as a list of (name . dictionary).

DOCUMENT: the document.
DICTIONARY: the dictionary of the stream."
  (let* ((filter (or (pdf-get document
                              dictionary
                              "Filter") (pdf-get document
                                                 dictionary
                                                 "F")))
         (parameters (or (pdf-get document
                                  dictionary
                                  "DecodeParms") (pdf-get document
                                                          dictionary
                                                          "DP")))
         (names (cond ((stringp filter) (list filter))
                      ((vectorp filter) (map 'list (lambda (item) (pdf-resolve document item)) filter))
                      (t '())))
         (parameter-list (cond ((hash-table-p parameters) (list parameters))
                               ((vectorp parameters) (map 'list (lambda (item) (pdf-resolve document item)) parameters))
                               (t '()))))
    (loop for name in names
          for index from 0
          collect (cons (or (cdr (assoc name *filter-abbreviations* :test #'string=)) name)
                        (let ((own (nth index parameter-list)))
                          (and (hash-table-p own) own))))))

(defun decode-stream-bytes (document
                            dictionary
                            encoded)
  "Undo the filters of a stream as far as they lead to bytes, and return those bytes, the name of the picture filter
the decoding stopped at (or NIL) and its parameters.

DOCUMENT: the document.
DICTIONARY: the dictionary of the stream.
ENCODED: the bytes of the stream as the file holds them."
  (let ((bytes encoded))
    (loop for (name . parameters) in (stream-filters document dictionary)
          do (cond ((member name *image-filters* :test #'string=)
                    (return-from decode-stream-bytes (values bytes name parameters)))
                   ((string= name "FlateDecode")
                    (setf bytes (apply-predictor document
                                                 (inflate bytes :limit +pdf-stream-limit+)
                                                 parameters)))
                   ((string= name "LZWDecode")
                    (setf bytes (apply-predictor document
                                                 (lzw-decode bytes
                                                             (parameter-integer document
                                                                                parameters
                                                                                "EarlyChange"
                                                                                1))
                                                 parameters)))
                   ((string= name "ASCIIHexDecode")
                    (setf bytes (ascii-hex-decode bytes)))
                   ((string= name "ASCII85Decode")
                    (setf bytes (ascii-85-decode bytes)))
                   ((string= name "RunLengthDecode")
                    (setf bytes (run-length-decode bytes)))
                   ((string= name "Crypt"))
                   (t
                    (image-failure "This PDF uses a coding of its pictures that the program does not read."))))
    (values bytes nil nil)))

(defun object-stream-bytes (document
                            object)
  "Return the decoded bytes of the stream of an indirect object, the name of the picture filter it stopped at (or
NIL) and that filter's parameters, or NIL when the object has no stream.

DOCUMENT: the document.
OBJECT: the indirect object."
  (when (and object (object-stream-start object))
    (decode-stream-bytes document
                         (object-value object)
                         (subseq (document-data document) (object-stream-start object) (object-stream-end object)))))

;;; Opening

(defun expand-object-streams (document)
  "Enter the objects kept inside object streams (PDF 1.5) in the document, those that no object outside them defines.

DOCUMENT: the document."
  (let ((streams '()))
    (maphash (lambda (number object)
               (declare (ignore number))
               (when (and (hash-table-p (object-value object))
                          (object-stream-start object)
                          (equal (gethash "Type" (object-value object)) "ObjStm"))
                 (push object streams)))
             (document-objects document))
    (dolist (stream streams)
      (handler-case
          (let* ((dictionary (object-value stream))
                 (count (pdf-get document
                                 dictionary
                                 "N"))
                 (first (pdf-get document
                                 dictionary
                                 "First"))
                 (bytes (object-stream-bytes document stream)))
            (when (and (integerp count) (integerp first) bytes (<= 0 count 1000000))
              (let ((header (make-reader-on bytes :end (min first (length bytes)))))
                (dotimes (each count)
                  (let ((number (read-pdf-value header :references nil))
                        (offset (read-pdf-value header :references nil)))
                    (unless (and (integerp number) (integerp offset))
                      (return))
                    (unless (gethash number (document-objects document))
                      (when (< (+ first offset) (length bytes))
                        (let ((value (read-pdf-value (make-reader-on bytes :start (+ first offset)))))
                          (unless (or (eq value :eof) (pdf-operator-p value))
                            (setf (gethash number (document-objects document))
                                  (make-pdf-object :number number :value value)))))))))))
        (base:image-error ()
          nil)))))

(defun open-pdf (bytes)
  "Return the document that the bytes of a PDF file make. Signals IMAGE-ERROR when the file is not a PDF, is damaged,
or is protected by a password.

BYTES: the file."
  (declare (type octets bytes))
  (unless (starts-with-p bytes *pdf-signature*)
    (pdf-damaged))
  (let ((document (make-pdf-document :data bytes)))
    (scan-pdf-objects document)
    (scan-pdf-trailers document)
    (when (some (lambda (trailer) (gethash "Encrypt" trailer)) (document-trailers document))
      (image-failure "This PDF is protected by a password, which the program cannot open."))
    (expand-object-streams document)
    (when (zerop (hash-table-count (document-objects document)))
      (pdf-damaged))
    document))

;;; Pages

(defstruct (pdf-page (:conc-name page-))
  "A page of a document: its dictionary with the entries it inherits from the tree above it put in."
  (dictionary nil)
  (number 0 :type fixnum))

(defparameter *inherited-page-keys*
  '("Resources" "MediaBox" "CropBox" "Rotate" "UserUnit")
  "The entries of a page that it takes from the nodes above it in the tree when it has none of its own.")

(defun collect-pages (document
                      node
                      inherited
                      visited
                      depth)
  "Return the pages below a node of the tree of pages, in order, as a list of dictionaries with inherited entries put
in.

DOCUMENT: the document.
NODE: the node, a dictionary or a reference to one.
INHERITED: an alist of the inheritable entries of the nodes above.
VISITED: a hash table of the nodes seen, to stop loops.
DEPTH: how deep the node is."
  (let ((dictionary (pdf-resolve document node)))
    (when (and (hash-table-p dictionary)
               (<= depth +pdf-maximum-page-depth+)
               (not (gethash dictionary visited)))
      (setf (gethash dictionary visited) t)
      (let ((inherited (append (loop for key in *inherited-page-keys*
                                     when (gethash key dictionary)
                                       collect (cons key (gethash key dictionary)))
                               inherited))
            (kids (pdf-get document
                           dictionary
                           "Kids")))
        (if (vectorp kids)
            (loop for kid across kids
                  append (collect-pages document
                                        kid
                                        inherited
                                        visited
                                        (1+ depth)))
            (let ((page (make-hash-table :test 'equal)))
              (loop for (key . value) in (reverse inherited)
                    do (setf (gethash key page) value))
              (maphash (lambda (key value)
                         (setf (gethash key page) value))
                       dictionary)
              (list page)))))))

(defun pdf-pages (document)
  "Return the pages of a document, in order, as PDF-PAGE structures. Signals IMAGE-ERROR when it has none.

DOCUMENT: the document."
  (let* ((root-reference (loop for trailer in (document-trailers document)
                               for root = (gethash "Root" trailer)
                               when root
                                 do (return root)))
         (catalog (and root-reference (pdf-resolve document root-reference)))
         (pages-node (and (hash-table-p catalog) (gethash "Pages" catalog)))
         (dictionaries (or (and pages-node
                                (collect-pages document
                                               pages-node
                                               '()
                                               (make-hash-table :test 'eq)
                                               0))
                           (let ((found '()))
                             (maphash (lambda (number object)
                                        (let ((value (object-value object)))
                                          (when (and (hash-table-p value)
                                                     (equal (gethash "Type" value) "Page"))
                                            (push (cons number value) found))))
                                      (document-objects document))
                             (mapcar #'cdr (sort found #'< :key #'car))))))
    (when (null dictionaries)
      (image-failure "This PDF has no pages."))
    (loop for dictionary in dictionaries
          for number from 1
          collect (make-pdf-page :dictionary dictionary :number number))))

(defun page-box (document
                 page)
  "Return the visible box of a page as (x0 y0 x1 y1) in points, with x0 less than x1 and y0 less than y1: the crop box
inside the media box.

DOCUMENT: the document.
PAGE: the page."
  (flet ((box-of (key)
           (let ((value (pdf-get document
                                 (page-dictionary page)
                                 key)))
             (when (and (vectorp value) (= (length value) 4))
               (let ((numbers (map 'list (lambda (item) (pdf-resolve document item)) value)))
                 (when (every #'realp numbers)
                   (list (min (first numbers) (third numbers))
                         (min (second numbers) (fourth numbers))
                         (max (first numbers) (third numbers))
                         (max (second numbers) (fourth numbers)))))))))
    (let* ((media (or (box-of "MediaBox") *default-page-box*))
           (crop (box-of "CropBox"))
           (box (if crop
                    (list (max (first media) (first crop))
                          (max (second media) (second crop))
                          (min (third media) (third crop))
                          (min (fourth media) (fourth crop)))
                    media)))
      (if (and (< (first box) (third box)) (< (second box) (fourth box)))
          box
          media))))

(defun page-rotation (document
                      page)
  "Return how many degrees clockwise a page is turned when shown: 0, 90, 180 or 270.

DOCUMENT: the document.
PAGE: the page."
  (let ((value (pdf-get document
                        (page-dictionary page)
                        "Rotate")))
    (if (integerp value)
        (mod (* 90 (round value 90)) 360)
        0)))

(defun page-size-points (document
                         page)
  "Return the width and the height of a page as it is shown, in points of 1/72 inch, after its rotation and unit.

DOCUMENT: the document.
PAGE: the page."
  (let* ((box (page-box document page))
         (unit (let ((value (pdf-get document
                                     (page-dictionary page)
                                     "UserUnit")))
                 (if (and (realp value) (<= 0.01 value 100)) value 1)))
         (width (* unit (- (third box) (first box))))
         (height (* unit (- (fourth box) (second box)))))
    (if (member (page-rotation document page) '(90 270))
        (values height width)
        (values width height))))

(defun page-content-bytes (document
                           page)
  "Return the content of a page: its content streams decoded and joined.

DOCUMENT: the document.
PAGE: the page."
  (let* ((contents (gethash "Contents" (page-dictionary page)))
         (references (cond ((null contents) '())
                           ((vectorp contents) (coerce contents 'list))
                           (t (list contents))))
         (pieces (loop for reference in references
                       for object = (pdf-object-of document reference)
                       for bytes = (and object (object-stream-bytes document object))
                       when bytes
                         collect bytes))
         (total (+ (length pieces) (reduce #'+ pieces :key #'length)))
         (joined (make-octets total))
         (fill 0))
    (dolist (piece pieces joined)
      (replace joined piece :start1 fill)
      (incf fill (length piece))
      (setf (aref joined fill) 10)
      (incf fill))))
