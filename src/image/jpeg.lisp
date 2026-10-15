(in-package #:borba-cad/image)

;;;; A baseline JPEG encoder: 8 bits, three components (Y, Cb, Cr) at full resolution, Huffman tables made for each
;;;; picture from the symbols it holds (Annex K.2 of ITU T.81, which keeps every code within 16 bits and leaves
;;;; the all-ones code unused). A drawing is made of flat areas and thin lines, which this codes well.

(defconstant +default-jpeg-quality+ 90
  "The quality a picture is encoded with when none is asked for, from 1 to 100.")

(defconstant +block-size+ 8
  "Side of a block of pixels that is transformed and coded as a unit.")

(defconstant +block-area+ 64
  "Pixels in a block.")

(defconstant +maximum-huffman-length+ 16
  "Longest code a JPEG Huffman table may have, in bits.")

(defconstant +longest-code-search+ 64
  "Longest code length that is looked for before the lengths are limited to 16. The procedure of the standard
supposes 32 is enough; a picture cannot have so many symbols that 64 is not.")

(defconstant +end-of-block+ #x00
  "The symbol that says the rest of the coefficients of a block are zero.")

(defconstant +zero-run-of-sixteen+ #xF0
  "The symbol for sixteen zero coefficients that are not the end of the block.")

(defparameter *zigzag*
  (coerce '(0 1 8 16 9 2 3 10 17 24 32 25 18 11 4 5 12 19 26 33 40 48 41 34 27 20 13 6 7 14 21 28
            35 42 49 56 57 50 43 36 29 22 15 23 30 37 44 51 58 59 52 45 38 31 39 46 53 60 61 54 47 55 62 63)
          '(simple-array fixnum (64)))
  "The place in a block, row by row, of each coefficient in the order they are coded.")

(defparameter *luminance-quantization*
  (coerce '(16 11 10 16 24 40 51 61 12 12 14 19 26 58 60 55 14 13 16 24 40 57 69 56 14 17 22 29 51 87 80 62
            18 22 37 56 68 109 103 77 24 35 55 64 81 104 113 92 49 64 78 87 103 121 120 101
            72 92 95 98 112 100 103 99)
          '(simple-array fixnum (64)))
  "The table of ITU T.81 Annex K.1 for the brightness, at quality 50.")

(defparameter *chrominance-quantization*
  (coerce '(17 18 24 47 99 99 99 99 18 21 26 66 99 99 99 99 24 26 56 99 99 99 99 99 47 66 99 99 99 99 99 99
            99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99)
          '(simple-array fixnum (64)))
  "The table of ITU T.81 Annex K.1 for the colors, at quality 50.")

;;; Quantization

(defun scaled-quantization (base
                            quality)
  "Return a quantization table for a quality: the base table scaled as the common libraries do, with every
value from 1 to 255.

BASE: the table at quality 50.
QUALITY: from 1 (smallest file) to 100 (best picture)."
  (let* ((quality (max 1 (min 100 quality)))
         (scale (if (< quality 50)
                    (floor 5000 quality)
                    (- 200 (* 2 quality))))
         (table (make-array +block-area+ :element-type 'fixnum)))
    (dotimes (index +block-area+ table)
      (setf (aref table index)
            (max 1 (min 255 (floor (+ (* (aref base index) scale) 50) 100)))))))

;;; The transform

(declaim (type (simple-array double-float (8 8)) *cosines*))

(defparameter *cosines*
  (let ((matrix (make-array '(8 8) :element-type 'double-float)))
    (dotimes (frequency 8 matrix)
      (dotimes (position 8)
        (setf (aref matrix frequency position)
              (* (if (zerop frequency) (/ 1d0 (sqrt 2d0)) 1d0)
                 0.5d0
                 (cos (/ (* (+ (* 2 position) 1) frequency pi) 16d0)))))))
  "The matrix of the discrete cosine transform of eight values: row u, column x is c(u)/2 cos((2x+1)u pi/16).")

(defun transform-block (block
                        coefficients
                        scratch)
  "Transform a block of 64 values with the discrete cosine transform, rows then columns, into COEFFICIENTS.

BLOCK: 64 double-floats, row by row, already centered on zero.
COEFFICIENTS: where the 64 coefficients go, row by row.
SCRATCH: 64 double-floats to work in."
  (declare (type (simple-array double-float (64)) block coefficients scratch)
           (optimize (speed 3) (safety 0)))
  (dotimes (row 8)
    (dotimes (frequency 8)
      (let ((sum 0d0))
        (declare (type double-float sum))
        (dotimes (position 8)
          (incf sum (* (aref *cosines* frequency position) (aref block (+ (* row 8) position)))))
        (setf (aref scratch (+ (* row 8) frequency)) sum))))
  (dotimes (column 8)
    (dotimes (frequency 8)
      (let ((sum 0d0))
        (declare (type double-float sum))
        (dotimes (position 8)
          (incf sum (* (aref *cosines* frequency position) (aref scratch (+ (* position 8) column)))))
        (setf (aref coefficients (+ (* frequency 8) column)) sum)))))

;;; The planes

(defun color-planes (width
                     height
                     rgb)
  "Return the three planes of a picture, brightness and the two colors, as arrays of double-floats of
WIDTH x HEIGHT.

WIDTH: width in pixels.
HEIGHT: height in pixels.
RGB: the pixels, row by row from the top, three bytes each."
  (declare (type fixnum width height)
           (type (simple-array (unsigned-byte 8) (*)) rgb)
           (optimize (speed 3) (safety 0)))
  (let* ((count (* width height))
         (y (make-array count :element-type 'double-float))
         (cb (make-array count :element-type 'double-float))
         (cr (make-array count :element-type 'double-float)))
    (dotimes (index count)
      (let ((r (float (aref rgb (* 3 index)) 1d0))
            (g (float (aref rgb (+ 1 (* 3 index))) 1d0))
            (b (float (aref rgb (+ 2 (* 3 index))) 1d0)))
        (setf (aref y index) (+ (* 0.299d0 r) (* 0.587d0 g) (* 0.114d0 b))
              (aref cb index) (+ 128d0 (* -0.168736d0 r) (* -0.331264d0 g) (* 0.5d0 b))
              (aref cr index) (+ 128d0 (* 0.5d0 r) (* -0.418688d0 g) (* -0.081312d0 b)))))
    (list y cb cr)))

(defun quantized-blocks (plane
                         width
                         height
                         quantization)
  "Return the blocks of a plane, transformed and quantized, as one array of 64 integers for each block in the
order the coefficients are coded, blocks left to right and top to bottom; the edges are repeated to fill the
last blocks.

PLANE: double-floats of WIDTH x HEIGHT.
WIDTH: width in pixels.
HEIGHT: height in pixels.
QUANTIZATION: the table, 64 values row by row."
  (declare (type fixnum width height)
           (type (simple-array double-float (*)) plane)
           (type (simple-array fixnum (64)) quantization)
           (optimize (speed 3) (safety 0)))
  (let* ((columns (ceiling width +block-size+))
         (rows (ceiling height +block-size+))
         (result (make-array (* columns rows)))
         (block (make-array 64 :element-type 'double-float))
         (coefficients (make-array 64 :element-type 'double-float))
         (scratch (make-array 64 :element-type 'double-float)))
    (dotimes (block-row rows)
      (dotimes (block-column columns)
        (dotimes (row 8)
          (let ((source-row (min (1- height) (+ (* block-row 8) row))))
            (declare (type fixnum source-row))
            (dotimes (column 8)
              (let ((source-column (min (1- width) (+ (* block-column 8) column))))
                (declare (type fixnum source-column))
                (setf (aref block (+ (* row 8) column))
                      (- (aref plane (+ (* source-row width) source-column)) 128d0))))))
        (transform-block block
                         coefficients
                         scratch)
        (let ((coded (make-array 64 :element-type '(signed-byte 16))))
          (dotimes (index 64)
            (let* ((place (aref *zigzag* index))
                   (value (/ (aref coefficients place) (float (aref quantization place) 1d0))))
              (setf (aref coded index) (the (signed-byte 16) (round (the (double-float -40000d0 40000d0) value))))))
          (setf (aref result (+ (* block-row columns) block-column)) coded))))
    result))

;;; Huffman tables

(defstruct (huffman (:constructor make-huffman (counts
                                                symbols
                                                codes
                                                sizes)))
  "A Huffman table of a JPEG.

COUNTS: how many codes there are of each length from 1 to 16.
SYMBOLS: the symbols in order of the length of their code.
CODES: the code of each symbol, by symbol.
SIZES: the length of the code of each symbol, by symbol."
  counts
  symbols
  codes
  sizes)

(defun code-lengths (frequencies)
  "Return the length of the Huffman code of each symbol, by the procedure of ITU T.81 Annex K.2: the two least
frequent are joined again and again. Symbol 256, which has a frequency of one, is there so that no real
symbol gets the code of all ones.

FREQUENCIES: a vector of 257 frequencies."
  (let ((frequency (copy-seq frequencies))
        (size (make-array 257 :initial-element 0))
        (others (make-array 257 :initial-element -1)))
    (loop
      (let ((first nil)
            (second nil))
        (dotimes (symbol 257)
          (when (plusp (aref frequency symbol))
            (when (or (null first) (<= (aref frequency symbol) (aref frequency first)))
              (setf first symbol))))
        (dotimes (symbol 257)
          (when (and (plusp (aref frequency symbol)) (not (eql symbol first)))
            (when (or (null second) (<= (aref frequency symbol) (aref frequency second)))
              (setf second symbol))))
        (unless second
          (return))
        (incf (aref frequency first) (aref frequency second))
        (setf (aref frequency second) 0)
        (loop
          (incf (aref size first))
          (when (minusp (aref others first))
            (return))
          (setf first (aref others first)))
        (setf (aref others first) second)
        (loop
          (incf (aref size second))
          (when (minusp (aref others second))
            (return))
          (setf second (aref others second)))))
    size))

(defun build-huffman (frequencies)
  "Return the Huffman table for the symbols of a picture, no code longer than 16 bits.

FREQUENCIES: a vector of 256 frequencies, by symbol."
  (let* ((all (make-array 257 :initial-element 0))
         (counts (make-array (1+ +longest-code-search+) :initial-element 0)))
    (dotimes (symbol 256)
      (setf (aref all symbol) (aref frequencies symbol)))
    (setf (aref all 256) 1)
    (let ((sizes (code-lengths all)))
      (dotimes (symbol 257)
        (when (plusp (aref sizes symbol))
          (incf (aref counts (aref sizes symbol)))))
      (loop for length from +longest-code-search+ above +maximum-huffman-length+
            do (loop while (plusp (aref counts length))
                     do (let ((shorter (- length 2)))
                          (loop while (zerop (aref counts shorter))
                                do (decf shorter))
                          (decf (aref counts length) 2)
                          (incf (aref counts (1- length)))
                          (incf (aref counts (1+ shorter)) 2)
                          (decf (aref counts shorter)))))
      (let ((ordered (sort (loop for symbol from 0 to 256
                                 when (plusp (aref sizes symbol))
                                   collect symbol)
                           (lambda (a b)
                             (or (< (aref sizes a) (aref sizes b))
                                 (and (= (aref sizes a) (aref sizes b)) (< a b))))))
            (longest +maximum-huffman-length+))
        (loop while (zerop (aref counts longest))
              do (decf longest))
        (decf (aref counts longest))
        (setf ordered (butlast ordered))
        (let ((codes (make-array 256 :initial-element 0))
              (final-sizes (make-array 256 :initial-element 0))
              (code 0)
              (remaining ordered))
          (loop for length from 1 to +maximum-huffman-length+
                do (dotimes (index (aref counts length))
                     (let ((symbol (pop remaining)))
                       (setf (aref codes symbol) code
                             (aref final-sizes symbol) length)
                       (incf code)))
                   (setf code (ash code 1)))
          (make-huffman (subseq counts 1 (1+ +maximum-huffman-length+))
                        ordered
                        codes
                        final-sizes))))))

;;; Symbols

(defun category (value)
  "Return the number of bits of the size of a coefficient: the category of ITU T.81 Table 5.

VALUE: an integer."
  (integer-length (abs value)))

(defun block-symbols (coded
                      previous-dc
                      visit)
  "Call VISIT for each symbol a block is coded with, and return the DC of the block. VISIT takes the kind (:dc or
:ac), the symbol, the number of bits of extra data and the extra data.

CODED: the 64 quantized coefficients in the order they are coded.
PREVIOUS-DC: the DC coefficient of the block before, of the same component.
VISIT: function of the kind, the symbol, the bit count and the bits."
  (declare (type (simple-array (signed-byte 16) (64)) coded)
           (type fixnum previous-dc)
           (type function visit))
  (let* ((dc (aref coded 0))
         (difference (- dc previous-dc))
         (size (category difference)))
    (funcall visit :dc size size (if (minusp difference) (+ difference (1- (ash 1 size))) difference))
    (let ((run 0))
      (declare (type fixnum run))
      (loop for index from 1 below 64
            do (let ((value (aref coded index)))
                 (cond ((zerop value)
                        (incf run))
                       (t
                        (loop while (> run 15)
                              do (funcall visit :ac +zero-run-of-sixteen+ 0 0)
                                 (decf run 16))
                        (let ((size (category value)))
                          (funcall visit :ac (logior (ash run 4) size) size
                                   (if (minusp value) (+ value (1- (ash 1 size))) value)))
                        (setf run 0)))))
      (when (plusp run)
        (funcall visit :ac +end-of-block+ 0 0)))
    dc))

;;; Writing

(defstruct (bit-writer (:constructor make-bit-writer ()))
  "The entropy-coded bytes of a picture, and the bits not yet made into a byte.

BYTES: what was written.
BUFFER: bits waiting, in the low end.
COUNT: how many."
  (bytes (make-array 65536 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
  (buffer 0 :type (unsigned-byte 32))
  (count 0 :type fixnum))

(defun write-bits (writer
                   value
                   count)
  "Append COUNT bits to the entropy-coded data, with a zero byte after any byte of all ones, as the format needs.

WRITER: the bit writer.
VALUE: the bits, as a number.
COUNT: how many of them."
  (declare (type bit-writer writer)
           (type fixnum count))
  (when (plusp count)
    (setf (bit-writer-buffer writer)
          (logior (ash (bit-writer-buffer writer) count) (logand value (1- (ash 1 count)))))
    (incf (bit-writer-count writer) count)
    (loop while (>= (bit-writer-count writer) 8)
          do (let ((byte (logand #xFF (ash (bit-writer-buffer writer) (- 8 (bit-writer-count writer))))))
               (vector-push-extend byte (bit-writer-bytes writer))
               (when (= byte #xFF)
                 (vector-push-extend 0 (bit-writer-bytes writer)))
               (decf (bit-writer-count writer) 8)
               (setf (bit-writer-buffer writer)
                     (logand (bit-writer-buffer writer) (1- (ash 1 (bit-writer-count writer)))))))))

(defun finish-bits (writer)
  "Fill the last byte with ones, as the format asks.

WRITER: the bit writer."
  (let ((missing (mod (- (bit-writer-count writer)) 8)))
    (write-bits writer
                (1- (ash 1 missing))
                missing)))

(defun push-word (bytes
                  value)
  "Append a 16-bit number to BYTES, high byte first.

BYTES: the adjustable vector of bytes.
VALUE: the number."
  (vector-push-extend (ldb (byte 8 8) value) bytes)
  (vector-push-extend (ldb (byte 8 0) value) bytes))

(defun push-marker (bytes
                    marker
                    payload)
  "Append a marker segment: FF, the marker, the length, and the bytes of PAYLOAD (numbers, or sequences of them).

BYTES: the adjustable vector of bytes.
MARKER: the byte after FF.
PAYLOAD: a list of bytes and sequences of bytes."
  (let ((flat (loop for item in payload
                    append (if (integerp item) (list item) (coerce item 'list)))))
    (vector-push-extend #xFF bytes)
    (vector-push-extend marker bytes)
    (push-word bytes (+ 2 (length flat)))
    (dolist (byte flat)
      (vector-push-extend byte bytes))))

(defun quantization-segment (bytes
                             id
                             table)
  "Append the segment that defines a quantization table, its values in the order they are coded.

BYTES: the adjustable vector of bytes.
ID: the number of the table.
TABLE: 64 values, row by row."
  (push-marker bytes
               #xDB
               (list id
                     (loop for index from 0 below 64 collect (aref table (aref *zigzag* index))))))

(defun huffman-segment (bytes
                        class
                        id
                        table)
  "Append the segment that defines a Huffman table.

BYTES: the adjustable vector of bytes.
CLASS: 0 for the sizes of the DC coefficients, 1 for the AC coefficients.
ID: the number of the table.
TABLE: the Huffman table."
  (push-marker bytes
               #xC4
               (list (logior (ash class 4) id)
                     (huffman-counts table)
                     (huffman-symbols table))))

(defun encode-jpeg (width
                    height
                    rgb
                    &key (quality +default-jpeg-quality+))
  "Return the bytes of a JPEG file of a picture.

WIDTH: width in pixels, from 1 to 65535.
HEIGHT: height in pixels, from 1 to 65535.
RGB: the pixels, row by row from the top, a byte each of red, green and blue.
QUALITY: from 1 (smallest file) to 100 (best picture)."
  (check-type width (integer 1 65535))
  (check-type height (integer 1 65535))
  (unless (and (typep rgb '(simple-array (unsigned-byte 8) (*)))
               (= (length rgb) (* 3 width height)))
    (error "A picture of ~d x ~d needs ~d bytes of pixels." width height (* 3 width height)))
  (let* ((luminance (scaled-quantization *luminance-quantization* quality))
         (chrominance (scaled-quantization *chrominance-quantization* quality))
         (planes (color-planes width
                               height
                               rgb))
         (tables (list luminance chrominance chrominance))
         (components (loop for plane in planes
                           for table in tables
                           collect (quantized-blocks plane
                                                     width
                                                     height
                                                     table)))
         (block-count (length (first components)))
         (histograms (list (list (make-array 256 :initial-element 0) (make-array 256 :initial-element 0))
                           (list (make-array 256 :initial-element 0) (make-array 256 :initial-element 0)))))
    (flet ((histogram (component kind)
             (let ((set (if (zerop component) (first histograms) (second histograms))))
               (if (eq kind :dc) (first set) (second set)))))
      (let ((previous (list 0 0 0)))
        (dotimes (index block-count)
          (loop for component in components
                for number from 0
                do (setf (nth number previous)
                         (block-symbols (aref component index)
                                        (nth number previous)
                                        (lambda (kind symbol size bits)
                                          (declare (ignore size bits))
                                          (incf (aref (histogram number kind) symbol))))))))
      (let* ((huffman-dc-0 (build-huffman (histogram 0 :dc)))
             (huffman-ac-0 (build-huffman (histogram 0 :ac)))
             (huffman-dc-1 (build-huffman (histogram 1 :dc)))
             (huffman-ac-1 (build-huffman (histogram 1 :ac)))
             (writer (make-bit-writer))
             (output (make-array 4096 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
        (let ((previous (list 0 0 0)))
          (dotimes (index block-count)
            (loop for component in components
                  for number from 0
                  do (let ((dc-table (if (zerop number) huffman-dc-0 huffman-dc-1))
                           (ac-table (if (zerop number) huffman-ac-0 huffman-ac-1)))
                       (setf (nth number previous)
                             (block-symbols (aref component index)
                                            (nth number previous)
                                            (lambda (kind symbol size bits)
                                              (let ((table (if (eq kind :dc) dc-table ac-table)))
                                                (write-bits writer
                                                            (aref (huffman-codes table) symbol)
                                                            (aref (huffman-sizes table) symbol))
                                                (write-bits writer
                                                            bits
                                                            size)))))))))
        (finish-bits writer)
        (vector-push-extend #xFF output)
        (vector-push-extend #xD8 output)
        (push-marker output
                     #xE0
                     (list (map 'list #'char-code "JFIF") 0 1 1 0 0 1 0 1 0 0))
        (quantization-segment output
                              0
                              luminance)
        (quantization-segment output
                              1
                              chrominance)
        (push-marker output
                     #xC0
                     (list 8
                           (ldb (byte 8 8) height)
                           (ldb (byte 8 0) height)
                           (ldb (byte 8 8) width)
                           (ldb (byte 8 0) width)
                           3 1 #x11 0 2 #x11 1 3 #x11 1))
        (huffman-segment output
                         0
                         0
                         huffman-dc-0)
        (huffman-segment output
                         1
                         0
                         huffman-ac-0)
        (huffman-segment output
                         0
                         1
                         huffman-dc-1)
        (huffman-segment output
                         1
                         1
                         huffman-ac-1)
        (push-marker output
                     #xDA
                     (list 3 1 #x00 2 #x11 3 #x11 0 63 0))
        (loop for byte across (bit-writer-bytes writer)
              do (vector-push-extend byte output))
        (vector-push-extend #xFF output)
        (vector-push-extend #xD9 output)
        (coerce output '(simple-array (unsigned-byte 8) (*)))))))
