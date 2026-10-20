(in-package #:borba-cad/image)

;;;; A JPEG reader (ITU T.81): baseline and extended sequential and progressive Huffman coding, 8 bits, with restart
;;;; intervals, any sampling of the colors, one to four components, and the Adobe marker that tells RGB, YCbCr, CMYK
;;;; and YCCK apart. Arithmetic coding, lossless and hierarchical files, and 12-bit samples are named and refused.
;;;; The coefficients of all scans are gathered first, so a progressive file needs nothing more than a baseline one,
;;;; then each block goes through the integer inverse transform of the Independent JPEG Group ("islow", Loeffler,
;;;; Ligtenberg and Moschytz) and the planes are put together as RGBA.

(defconstant +jpeg-marker-prefix+ #xFF
  "The byte that starts every marker.")

(defconstant +marker-soi+ #xD8
  "Start of image.")

(defconstant +marker-eoi+ #xD9
  "End of image.")

(defconstant +marker-sos+ #xDA
  "Start of scan.")

(defconstant +marker-dqt+ #xDB
  "Quantization tables.")

(defconstant +marker-dht+ #xC4
  "Huffman tables.")

(defconstant +marker-dri+ #xDD
  "Restart interval.")

(defconstant +marker-app14+ #xEE
  "The application segment of Adobe.")

(defconstant +marker-app0+ #xE0
  "The application segment of JFIF.")

(defconstant +marker-sof0+ #xC0
  "Frame header of a baseline file.")

(defconstant +marker-sof1+ #xC1
  "Frame header of an extended sequential file.")

(defconstant +marker-sof2+ #xC2
  "Frame header of a progressive file.")

(defconstant +marker-restart-first+ #xD0
  "The first of the eight restart markers.")

(defconstant +marker-restart-last+ #xD7
  "The last of the eight restart markers.")

(defconstant +jpeg-maximum-tables+ 4
  "How many tables of each kind a file may hold.")

(defconstant +jpeg-block-size+ 64
  "Coefficients in a block.")

(defconstant +jpeg-lookahead-bits+ 8
  "Bits of a Huffman code that a lookup table decodes in one step.")

(defconstant +jpeg-longest-code+ 16
  "Bits of the longest Huffman code.")

(defconstant +jpeg-maximum-sampling+ 4
  "The largest horizontal or vertical sampling factor of a component.")

(defconstant +jpeg-sample-center+ 128
  "The value added to the output of the transform to bring samples to the range of bytes.")

(defconstant +idct-constant-bits+ 13
  "Bits of fraction of the constants of the inverse transform.")

(defconstant +idct-first-pass-bits+ 2
  "Bits of fraction kept between the two passes of the inverse transform.")

(defconstant +adobe-transform-none+ 0
  "The Adobe transform of a file whose colors are stored as they are (RGB, or CMYK).")

(defconstant +adobe-transform-ycc+ 1
  "The Adobe transform of a file of YCbCr.")

(defconstant +adobe-transform-ycck+ 2
  "The Adobe transform of a file of YCCK.")

(defconstant +color-fixed-bits+ 16
  "Bits of fraction of the constants of the color conversion.")

(defstruct (decoding-table (:conc-name decoding-))
  "A Huffman table for decoding: a lookup for codes of up to 8 bits, and for longer ones the highest code and the
offset from code to symbol at each length."
  (lookup (make-array 256 :element-type '(unsigned-byte 16) :initial-element 0)
   :type (simple-array (unsigned-byte 16) (256)))
  (maxcode (make-array 18 :element-type 'fixnum :initial-element -1)
   :type (simple-array fixnum (18)))
  (offset (make-array 18 :element-type 'fixnum :initial-element 0)
   :type (simple-array fixnum (18)))
  (symbols (make-octets 256)
   :type octets))

(defstruct (jpeg-component (:conc-name component-))
  "One component of a frame (the brightness, a color) and what is known of its blocks."
  (id 0 :type fixnum)
  (h 1 :type fixnum)
  (v 1 :type fixnum)
  (quantization-id 0 :type fixnum)
  (blocks-per-line 0 :type fixnum)
  (blocks-per-column 0 :type fixnum)
  (scan-blocks-per-line 0 :type fixnum)
  (scan-blocks-per-column 0 :type fixnum)
  (coefficients (make-array 0 :element-type '(signed-byte 16)) :type (simple-array (signed-byte 16) (*)))
  (plane (make-octets 0) :type octets))

(defstruct (jpeg-frame (:conc-name frame-))
  "The frame header of a file, and what the scans gathered under it."
  (width 0 :type fixnum)
  (height 0 :type fixnum)
  (progressive nil)
  (components #() :type simple-vector)
  (h-max 1 :type fixnum)
  (v-max 1 :type fixnum)
  (mcus-per-line 0 :type fixnum)
  (mcus-per-column 0 :type fixnum))

(defstruct (jpeg-bits (:conc-name bits-))
  "The entropy-coded data of a scan being read bit by bit, most significant first, with the byte stuffing removed.
At a marker, or at the end of the data, zero bits are supplied and counted."
  (data (make-octets 0) :type octets)
  (position 0 :type fixnum)
  (buffer 0 :type (unsigned-byte 62))
  (count 0 :type fixnum)
  (overrun 0 :type fixnum)
  (at-marker nil))

(defstruct (scan-component (:conc-name scanned-))
  "A component as one scan codes it: its tables, and the last DC value for the difference coding."
  (component nil :type (or null jpeg-component))
  (dc-table nil :type (or null decoding-table))
  (ac-table nil :type (or null decoding-table))
  (predictor 0 :type fixnum))

(declaim (inline clamp-coefficient))

(defun clamp-coefficient (value)
  "Return VALUE limited to what a coefficient of a block can hold, so that damaged data cannot overflow it.

VALUE: the number."
  (declare (type fixnum value))
  (max -32768 (min 32767 value)))

(defun jpeg-damaged ()
  "Signal IMAGE-ERROR for a JPEG file that does not follow the format."
  (image-failure "This JPEG file is damaged."))

(defun jpeg-truncated ()
  "Signal IMAGE-ERROR for a JPEG file that ends too soon."
  (image-failure "This picture file ends before the picture does."))

;;; Huffman tables

(defun build-decoding-table-for-jpeg (counts
                                      symbols)
  "Return the decoding table of a Huffman table as a DHT segment gives it. Signals IMAGE-ERROR when the lengths
cannot belong to a prefix code.

COUNTS: sixteen numbers, how many codes there are of each length from 1 to 16.
SYMBOLS: the symbols in the order of their codes."
  (declare (type octets counts symbols))
  (let ((table (make-decoding-table))
        (code 0)
        (index 0))
    (replace (decoding-symbols table) symbols)
    (loop for length from 1 to +jpeg-longest-code+
          for count = (aref counts (1- length))
          do (when (> (+ code count) (ash 1 length))
               (jpeg-damaged))
             (when (plusp count)
               (setf (aref (decoding-offset table) length) (- index code)
                     (aref (decoding-maxcode table) length) (+ code count -1))
               (when (<= length +jpeg-lookahead-bits+)
                 (dotimes (each count)
                   (let ((first (ash (+ code each) (- +jpeg-lookahead-bits+ length))))
                     (dotimes (fill (ash 1 (- +jpeg-lookahead-bits+ length)))
                       (setf (aref (decoding-lookup table) (+ first fill))
                             (logior (ash length 8) (aref symbols (+ index each))))))))
               (incf index count)
               (incf code count))
             (setf code (ash code 1)))
    table))

;;; Bits

(defun next-entropy-byte (bits)
  "Return the next byte of the entropy-coded data, undoing the byte stuffing. At a marker or at the end of the data
it returns zero, and counts it as padding.

BITS: the bit reader."
  (declare (type jpeg-bits bits)
           (optimize (speed 3) (safety 1))
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((data (bits-data bits)))
    (loop
      (let ((position (bits-position bits)))
        (cond ((or (bits-at-marker bits)
                   (>= position (length data)))
               (incf (bits-overrun bits) 8)
               (return 0))
              ((/= (aref data position) +jpeg-marker-prefix+)
               (setf (bits-position bits) (1+ position))
               (return (aref data position)))
              ((>= (1+ position) (length data))
               (incf (bits-overrun bits) 8)
               (return 0))
              ((zerop (aref data (1+ position)))
               (setf (bits-position bits) (+ position 2))
               (return +jpeg-marker-prefix+))
              ((= (aref data (1+ position)) +jpeg-marker-prefix+)
               (setf (bits-position bits) (1+ position)))
              (t
               (setf (bits-at-marker bits) t)))))))

(declaim (inline fill-jpeg-bits peek-jpeg-bits drop-jpeg-bits))

(defun fill-jpeg-bits (bits
                       wanted)
  "Make sure the buffer holds at least WANTED bits.

BITS: the bit reader.
WANTED: how many bits, at most 24."
  (declare (type jpeg-bits bits)
           (type fixnum wanted)
           (optimize (speed 3) (safety 1))
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (loop while (< (bits-count bits) wanted)
        do (setf (bits-buffer bits) (logior (logand (ash (bits-buffer bits) 8) #xFFFFFFFFFFFF)
                                            (next-entropy-byte bits))
                 (bits-count bits) (+ 8 (bits-count bits)))))

(defun peek-jpeg-bits (bits
                       wanted)
  "Return the next WANTED bits as a number, without taking them.

BITS: the bit reader.
WANTED: how many bits, at most 24."
  (declare (type jpeg-bits bits)
           (type fixnum wanted)
           (optimize (speed 3) (safety 1))
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (fill-jpeg-bits bits wanted)
  (logand (ash (bits-buffer bits) (- (- (bits-count bits) wanted)))
          (1- (ash 1 wanted))))

(defun drop-jpeg-bits (bits
                       dropped)
  "Take DROPPED bits from the buffer, which must hold that many.

BITS: the bit reader.
DROPPED: how many bits."
  (declare (type jpeg-bits bits)
           (type fixnum dropped)
           (optimize (speed 3) (safety 1))
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (decf (bits-count bits) dropped))

(defun receive-jpeg-bits (bits
                          wanted)
  "Return and take the next WANTED bits as a number.

BITS: the bit reader.
WANTED: how many bits, from 0 to 16."
  (declare (type jpeg-bits bits)
           (type fixnum wanted)
           (optimize (speed 3) (safety 1))
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (if (zerop wanted)
      0
      (prog1 (peek-jpeg-bits bits wanted)
        (drop-jpeg-bits bits wanted))))

(defun receive-and-extend (bits
                           size)
  "Return the signed number that SIZE bits code, as a coefficient is coded: a value under half of the range is
negative.

BITS: the bit reader.
SIZE: its bits, from 0 to 16."
  (declare (type jpeg-bits bits)
           (type fixnum size)
           (optimize (speed 3) (safety 1))
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (if (zerop size)
      0
      (let ((value (receive-jpeg-bits bits size)))
        (declare (type fixnum value))
        (if (< value (ash 1 (1- size)))
            (+ value 1 (- (ash 1 size)))
            value))))

(defun decode-huffman-symbol (bits
                              table)
  "Return the next symbol of the entropy-coded data under a Huffman table.

BITS: the bit reader.
TABLE: the decoding table."
  (declare (type jpeg-bits bits)
           (type decoding-table table)
           (optimize (speed 3) (safety 1))
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let* ((peek (peek-jpeg-bits bits +jpeg-longest-code+))
         (entry (aref (decoding-lookup table) (ash peek (- +jpeg-lookahead-bits+ +jpeg-longest-code+)))))
    (declare (type fixnum peek entry))
    (cond ((plusp entry)
           (drop-jpeg-bits bits (ash entry -8))
           (logand entry #xFF))
          (t
           (loop for length of-type fixnum from (1+ +jpeg-lookahead-bits+) to +jpeg-longest-code+
                 do (let ((code (ash peek (- length +jpeg-longest-code+))))
                      (when (<= code (aref (decoding-maxcode table) length))
                        (drop-jpeg-bits bits length)
                        (return-from decode-huffman-symbol
                          (aref (decoding-symbols table)
                                (logand #xFF (+ code (aref (decoding-offset table) length))))))))
           (jpeg-damaged)))))

;;; Blocks

(defun decode-baseline-block (bits
                              scanned
                              coefficients
                              start)
  "Read a block of a sequential file: the DC difference, then the AC coefficients as runs of zeros and a value.

BITS: the bit reader.
SCANNED: the component as the scan has it.
COEFFICIENTS: where the coefficients of the component go.
START: the position of this block in them."
  (declare (type jpeg-bits bits)
           (type scan-component scanned)
           (type (simple-array (signed-byte 16) (*)) coefficients)
           (type fixnum start)
           (optimize (speed 3) (safety 1))
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((size (decode-huffman-symbol bits (scanned-dc-table scanned))))
    (when (> size 15)
      (jpeg-damaged))
    (incf (scanned-predictor scanned) (receive-and-extend bits size))
    (setf (aref coefficients start) (clamp-coefficient (scanned-predictor scanned))))
  (let ((index 1))
    (declare (type fixnum index))
    (loop while (< index +jpeg-block-size+)
          do (let* ((symbol (decode-huffman-symbol bits (scanned-ac-table scanned)))
                    (size (logand symbol 15))
                    (run (ash symbol -4)))
               (declare (type fixnum symbol size run))
               (cond ((plusp size)
                      (incf index run)
                      (when (>= index +jpeg-block-size+)
                        (jpeg-damaged))
                      (setf (aref coefficients (+ start (aref *zigzag* index)))
                            (clamp-coefficient (receive-and-extend bits size)))
                      (incf index))
                     ((= run 15)
                      (incf index 16))
                     (t
                      (return)))))))

(defun decode-dc-first-block (bits
                              scanned
                              coefficients
                              start
                              low)
  "Read the DC coefficient of a block in the first scan of a progressive file.

BITS: the bit reader.
SCANNED: the component as the scan has it.
COEFFICIENTS: where the coefficients of the component go.
START: the position of this block in them.
LOW: the successive approximation bit position: the value read is shifted left by it."
  (declare (type jpeg-bits bits)
           (type scan-component scanned)
           (type (simple-array (signed-byte 16) (*)) coefficients)
           (type fixnum start low)
           (optimize (speed 3) (safety 1))
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((size (decode-huffman-symbol bits (scanned-dc-table scanned))))
    (when (> size 15)
      (jpeg-damaged))
    (incf (scanned-predictor scanned) (receive-and-extend bits size))
    (setf (aref coefficients start) (clamp-coefficient (ash (scanned-predictor scanned) low)))))

(defun decode-dc-refine-block (bits
                               coefficients
                               start
                               low)
  "Read one more bit of the DC coefficient of a block, in a refining scan of a progressive file.

BITS: the bit reader.
COEFFICIENTS: where the coefficients of the component go.
START: the position of this block in them.
LOW: the bit position this scan refines."
  (declare (type jpeg-bits bits)
           (type (simple-array (signed-byte 16) (*)) coefficients)
           (type fixnum start low)
           (optimize (speed 3) (safety 1))
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (when (plusp (receive-jpeg-bits bits 1))
    (setf (aref coefficients start) (clamp-coefficient (logior (aref coefficients start) (ash 1 low))))))

(defun decode-ac-first-block (bits
                              scanned
                              coefficients
                              start
                              first
                              last
                              low
                              end-of-band)
  "Read the AC coefficients of a band of a block in the first scan that has them in a progressive file. Returns the
count of blocks, after this one, that are empty in this band.

BITS: the bit reader.
SCANNED: the component as the scan has it.
COEFFICIENTS: where the coefficients of the component go.
START: the position of this block in them.
FIRST: the first coefficient of the band.
LAST: the last coefficient of the band.
LOW: the successive approximation bit position.
END-OF-BAND: how many empty blocks are still owed from an earlier run."
  (declare (type jpeg-bits bits)
           (type scan-component scanned)
           (type (simple-array (signed-byte 16) (*)) coefficients)
           (type fixnum start first last low end-of-band)
           (optimize (speed 3) (safety 1))
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (when (plusp end-of-band)
    (return-from decode-ac-first-block (1- end-of-band)))
  (let ((index first))
    (declare (type fixnum index))
    (loop while (<= index last)
          do (let* ((symbol (decode-huffman-symbol bits (scanned-ac-table scanned)))
                    (size (logand symbol 15))
                    (run (ash symbol -4)))
               (declare (type fixnum symbol size run))
               (cond ((plusp size)
                      (incf index run)
                      (when (> index last)
                        (jpeg-damaged))
                      (setf (aref coefficients (+ start (aref *zigzag* index)))
                            (clamp-coefficient (ash (receive-and-extend bits size) low)))
                      (incf index))
                     ((= run 15)
                      (incf index 16))
                     (t
                      (return-from decode-ac-first-block
                        (+ (1- (ash 1 run)) (receive-jpeg-bits bits run))))))))
  0)

(defun decode-ac-refine-block (bits
                               scanned
                               coefficients
                               start
                               first
                               last
                               low
                               end-of-band)
  "Read one more bit of the AC coefficients of a band of a block, in a refining scan of a progressive file: the
coefficients already found get a bit more, and new ones of the size of that bit appear. Returns the count of blocks,
after this one, that are empty in this band.

BITS: the bit reader.
SCANNED: the component as the scan has it.
COEFFICIENTS: where the coefficients of the component go.
START: the position of this block in them.
FIRST: the first coefficient of the band.
LAST: the last coefficient of the band.
LOW: the bit position this scan refines.
END-OF-BAND: how many empty blocks are still owed from an earlier run."
  (declare (type jpeg-bits bits)
           (type scan-component scanned)
           (type (simple-array (signed-byte 16) (*)) coefficients)
           (type fixnum start first last low end-of-band)
           (optimize (speed 3) (safety 1))
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((positive (ash 1 low))
        (negative (- (ash 1 low)))
        (index first))
    (declare (type fixnum positive negative index))
    (flet ((refine (place)
             (declare (type fixnum place))
             (let ((value (aref coefficients place)))
               (when (and (plusp (receive-jpeg-bits bits 1))
                          (zerop (logand value positive)))
                 (setf (aref coefficients place)
                       (clamp-coefficient (if (>= value 0)
                                              (+ value positive)
                                              (+ value negative))))))))
      (declare (inline refine))
      (when (zerop end-of-band)
        (loop while (<= index last)
              do (let* ((symbol (decode-huffman-symbol bits (scanned-ac-table scanned)))
                        (size (logand symbol 15))
                        (run (ash symbol -4))
                        (new-value 0))
                   (declare (type fixnum symbol size run new-value))
                   (cond ((plusp size)
                          (unless (= size 1)
                            (jpeg-damaged))
                          (setf new-value (if (plusp (receive-jpeg-bits bits 1)) positive negative)))
                         ((/= run 15)
                          (setf end-of-band (+ (ash 1 run) (receive-jpeg-bits bits run)))
                          (return)))
                   (loop while (<= index last)
                         do (let ((place (+ start (aref *zigzag* index))))
                              (cond ((/= (aref coefficients place) 0)
                                     (refine place))
                                    (t
                                     (decf run)
                                     (when (minusp run)
                                       (return))))
                              (incf index)))
                   (when (and (/= new-value 0)
                              (<= index last))
                     (setf (aref coefficients (+ start (aref *zigzag* index))) (clamp-coefficient new-value)))
                   (incf index))))
      (when (plusp end-of-band)
        (loop while (<= index last)
              do (let ((place (+ start (aref *zigzag* index))))
                   (when (/= (aref coefficients place) 0)
                     (refine place))
                   (incf index)))
        (decf end-of-band))))
  end-of-band)

;;; Scans

(defun skip-to-restart-marker (bits)
  "Move past the restart marker that ends a restart interval, throwing away the bits left in the buffer. Signals
IMAGE-ERROR when the data has no restart marker where it must.

BITS: the bit reader."
  (declare (type jpeg-bits bits))
  (when (> (bits-overrun bits) (bits-count bits))
    (jpeg-truncated))
  (let ((data (bits-data bits))
        (position (bits-position bits)))
    (loop while (and (< (1+ position) (length data))
                     (not (and (= (aref data position) +jpeg-marker-prefix+)
                               (/= (aref data (1+ position)) 0)
                               (/= (aref data (1+ position)) +jpeg-marker-prefix+))))
          do (incf position))
    (unless (and (< (1+ position) (length data))
                 (<= +marker-restart-first+ (aref data (1+ position)) +marker-restart-last+))
      (jpeg-damaged))
    (setf (bits-position bits) (+ position 2)
          (bits-buffer bits) 0
          (bits-count bits) 0
          (bits-overrun bits) 0
          (bits-at-marker bits) nil)))

(defun decode-jpeg-scan (frame
                         bits
                         scanned-components
                         first
                         last
                         high
                         low
                         restart-interval)
  "Read the entropy-coded data of one scan into the coefficients of its components.

FRAME: the frame header.
BITS: the bit reader, at the start of the data.
SCANNED-COMPONENTS: the components of the scan, as a list of SCAN-COMPONENTs.
FIRST: the first coefficient of the band the scan carries.
LAST: the last coefficient of the band.
HIGH: the bit position the scan refines from, zero for the first scan of a band.
LOW: the bit position the scan carries.
RESTART-INTERVAL: how many MCUs between restart markers, zero for none."
  (declare (type jpeg-frame frame)
           (type jpeg-bits bits)
           (type fixnum first last high low restart-interval))
  (let* ((progressive (frame-progressive frame))
         (single (null (rest scanned-components)))
         (end-of-band 0)
         (until-restart restart-interval))
    (declare (type fixnum end-of-band until-restart))
    (flet ((block-at (scanned
                      row
                      column)
             (let* ((component (scanned-component scanned))
                    (coefficients (component-coefficients component))
                    (start (* +jpeg-block-size+ (+ column (* row (component-blocks-per-line component))))))
               (cond ((not progressive)
                      (decode-baseline-block bits
                                             scanned
                                             coefficients
                                             start))
                     ((zerop first)
                      (if (zerop high)
                          (decode-dc-first-block bits
                                                 scanned
                                                 coefficients
                                                 start
                                                 low)
                          (decode-dc-refine-block bits
                                                  coefficients
                                                  start
                                                  low)))
                     ((zerop high)
                      (setf end-of-band
                            (decode-ac-first-block bits
                                                   scanned
                                                   coefficients
                                                   start
                                                   first
                                                   last
                                                   low
                                                   end-of-band)))
                     (t
                      (setf end-of-band
                            (decode-ac-refine-block bits
                                                    scanned
                                                    coefficients
                                                    start
                                                    first
                                                    last
                                                    low
                                                    end-of-band))))))
           (restart-if-due ()
             (when (plusp restart-interval)
               (decf until-restart)
               (when (zerop until-restart)
                 (skip-to-restart-marker bits)
                 (setf until-restart restart-interval
                       end-of-band 0)
                 (dolist (scanned scanned-components)
                   (setf (scanned-predictor scanned) 0))))))
      (if single
          (let* ((scanned (first scanned-components))
                 (component (scanned-component scanned))
                 (columns (component-scan-blocks-per-line component))
                 (rows (component-scan-blocks-per-column component)))
            (dotimes (row rows)
              (dotimes (column columns)
                (block-at scanned row column)
                (unless (and (= row (1- rows)) (= column (1- columns)))
                  (restart-if-due)))))
          (dotimes (mcu-row (frame-mcus-per-column frame))
            (dotimes (mcu-column (frame-mcus-per-line frame))
              (dolist (scanned scanned-components)
                (let ((component (scanned-component scanned)))
                  (dotimes (v (component-v component))
                    (dotimes (h (component-h component))
                      (block-at scanned
                                (+ (* mcu-row (component-v component)) v)
                                (+ (* mcu-column (component-h component)) h))))))
              (unless (and (= mcu-row (1- (frame-mcus-per-column frame)))
                           (= mcu-column (1- (frame-mcus-per-line frame))))
                (restart-if-due))))))
    (when (> (bits-overrun bits) (bits-count bits))
      (jpeg-truncated))))

;;; The inverse transform

(defconstant +fix-0-298631336+ 2446
  "0.298631336 in fixed point, with the bits of +IDCT-CONSTANT-BITS+.")

(defconstant +fix-0-390180644+ 3196
  "0.390180644 in fixed point.")

(defconstant +fix-0-541196100+ 4433
  "0.541196100 in fixed point.")

(defconstant +fix-0-765366865+ 6270
  "0.765366865 in fixed point.")

(defconstant +fix-0-899976223+ 7373
  "0.899976223 in fixed point.")

(defconstant +fix-1-175875602+ 9633
  "1.175875602 in fixed point.")

(defconstant +fix-1-501321110+ 12299
  "1.501321110 in fixed point.")

(defconstant +fix-1-847759065+ 15137
  "1.847759065 in fixed point.")

(defconstant +fix-1-961570560+ 16069
  "1.961570560 in fixed point.")

(defconstant +fix-2-053119869+ 16819
  "2.053119869 in fixed point.")

(defconstant +fix-2-562915447+ 20995
  "2.562915447 in fixed point.")

(defconstant +fix-3-072711026+ 25172
  "3.072711026 in fixed point.")

(defmacro transform-eight ((in0 in1 in2 in3 in4 in5 in6 in7)
                           (out0 out1 out2 out3 out4 out5 out6 out7)
                           bits)
  "Expand into the one-dimensional inverse transform of eight values: the forms IN0 to IN7 are read first, and then
the places OUT0 to OUT7 are set to the results, scaled down by BITS bits.

IN0: the form of the coefficient of frequency 0, and so up to IN7.
OUT0: the place of the sample 0, and so up to OUT7.
BITS: how many bits of fraction to drop from the results."
  `(let* ((z2 ,in2)
          (z3 ,in6)
          (z1 (* (+ z2 z3) +fix-0-541196100+))
          (tmp2 (+ z1 (* z3 (- +fix-1-847759065+))))
          (tmp3 (+ z1 (* z2 +fix-0-765366865+)))
          (even0 ,in0)
          (even4 ,in4)
          (tmp0 (ash (+ even0 even4) +idct-constant-bits+))
          (tmp1 (ash (- even0 even4) +idct-constant-bits+))
          (tmp10 (+ tmp0 tmp3))
          (tmp13 (- tmp0 tmp3))
          (tmp11 (+ tmp1 tmp2))
          (tmp12 (- tmp1 tmp2))
          (odd7 ,in7)
          (odd5 ,in5)
          (odd3 ,in3)
          (odd1 ,in1)
          (sum-a (+ odd7 odd1))
          (sum-b (+ odd5 odd3))
          (sum-c (+ odd7 odd3))
          (sum-d (+ odd5 odd1))
          (sum-e (* (+ sum-c sum-d) +fix-1-175875602+))
          (scaled7 (* odd7 +fix-0-298631336+))
          (scaled5 (* odd5 +fix-2-053119869+))
          (scaled3 (* odd3 +fix-3-072711026+))
          (scaled1 (* odd1 +fix-1-501321110+))
          (low-a (* sum-a (- +fix-0-899976223+)))
          (low-b (* sum-b (- +fix-2-562915447+)))
          (low-c (+ (* sum-c (- +fix-1-961570560+)) sum-e))
          (low-d (+ (* sum-d (- +fix-0-390180644+)) sum-e))
          (result0 (+ scaled7 low-a low-c))
          (result1 (+ scaled5 low-b low-d))
          (result2 (+ scaled3 low-b low-c))
          (result3 (+ scaled1 low-a low-d)))
     (declare (type fixnum z1 z2 z3 even0 even4 tmp0 tmp1 tmp2 tmp3 tmp10 tmp11 tmp12 tmp13
                    odd7 odd5 odd3 odd1 sum-a sum-b sum-c sum-d sum-e scaled7 scaled5 scaled3 scaled1
                    low-a low-b low-c low-d result0 result1 result2 result3))
     (setf ,out0 (ash (+ tmp10 result3 (ash 1 (1- ,bits))) (- ,bits))
           ,out7 (ash (+ (- tmp10 result3) (ash 1 (1- ,bits))) (- ,bits))
           ,out1 (ash (+ tmp11 result2 (ash 1 (1- ,bits))) (- ,bits))
           ,out6 (ash (+ (- tmp11 result2) (ash 1 (1- ,bits))) (- ,bits))
           ,out2 (ash (+ tmp12 result1 (ash 1 (1- ,bits))) (- ,bits))
           ,out5 (ash (+ (- tmp12 result1) (ash 1 (1- ,bits))) (- ,bits))
           ,out3 (ash (+ tmp13 result0 (ash 1 (1- ,bits))) (- ,bits))
           ,out4 (ash (+ (- tmp13 result0) (ash 1 (1- ,bits))) (- ,bits)))))

(defun inverse-transform-block (coefficients
                                start
                                quantization
                                work
                                plane
                                plane-start
                                stride)
  "Turn the 64 coefficients of a block into 8 by 8 samples of a plane: dequantize them, apply the inverse cosine
transform, center the result and limit it to the range of a byte. The constants are those of the Independent JPEG
Group's accurate integer transform, which gives the same samples as the common decoders.

COEFFICIENTS: the coefficients of the component, each block in the order of the picture.
START: where this block begins in them.
QUANTIZATION: the 64 steps of its quantization table, in the order of the block.
WORK: a vector of 64 fixnums to work in.
PLANE: the samples of the component, row by row.
PLANE-START: the position of the top left sample of this block in it.
STRIDE: samples in a row of the plane."
  (declare (type (simple-array (signed-byte 16) (*)) coefficients)
           (type (simple-array fixnum (64)) quantization work)
           (type octets plane)
           (type fixnum start plane-start stride)
           (optimize (speed 3) (safety 1))
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((first-bits (- +idct-constant-bits+ +idct-first-pass-bits+))
        (second-bits (+ +idct-constant-bits+ +idct-first-pass-bits+ 3)))
    (declare (type fixnum first-bits second-bits))
    (flet ((dequantized (index)
             (declare (type fixnum index))
             (the fixnum (* (aref coefficients (+ start index)) (aref quantization index)))))
      (declare (inline dequantized))
      (dotimes (column 8)
        (transform-eight ((dequantized column)
                          (dequantized (+ column 8))
                          (dequantized (+ column 16))
                          (dequantized (+ column 24))
                          (dequantized (+ column 32))
                          (dequantized (+ column 40))
                          (dequantized (+ column 48))
                          (dequantized (+ column 56)))
                         ((aref work column)
                          (aref work (+ column 8))
                          (aref work (+ column 16))
                          (aref work (+ column 24))
                          (aref work (+ column 32))
                          (aref work (+ column 40))
                          (aref work (+ column 48))
                          (aref work (+ column 56)))
                         first-bits)))
    (dotimes (row 8)
      (let ((base (* row 8))
            (out (+ plane-start (* row stride))))
        (declare (type fixnum base out))
        (transform-eight ((aref work base)
                          (aref work (+ base 1))
                          (aref work (+ base 2))
                          (aref work (+ base 3))
                          (aref work (+ base 4))
                          (aref work (+ base 5))
                          (aref work (+ base 6))
                          (aref work (+ base 7)))
                         ((aref work base)
                          (aref work (+ base 1))
                          (aref work (+ base 2))
                          (aref work (+ base 3))
                          (aref work (+ base 4))
                          (aref work (+ base 5))
                          (aref work (+ base 6))
                          (aref work (+ base 7)))
                         second-bits)
        (dotimes (column 8)
          (setf (aref plane (+ out column))
                (max 0 (min 255 (+ (aref work (+ base column)) +jpeg-sample-center+)))))))))

;;; Planes and colors

(defun component-quantization (component
                               tables)
  "Return the quantization table that a component names, in the order of a block, or signal IMAGE-ERROR when the file
has not defined it.

COMPONENT: the component.
TABLES: the vector of the four quantization tables a file may define."
  (or (svref tables (component-quantization-id component))
      (jpeg-damaged)))

(defun build-component-plane (frame
                              component
                              tables)
  "Fill the plane of a component with the samples its coefficients transform into.

FRAME: the frame header.
COMPONENT: the component.
TABLES: the vector of the four quantization tables a file may define."
  (declare (ignore frame))
  (let* ((quantization (component-quantization component tables))
         (stride (* 8 (component-blocks-per-line component)))
         (plane (make-octets (* stride 8 (component-blocks-per-column component))))
         (work (make-array 64 :element-type 'fixnum :initial-element 0)))
    (dotimes (row (component-blocks-per-column component))
      (dotimes (column (component-blocks-per-line component))
        (inverse-transform-block (component-coefficients component)
                                 (* +jpeg-block-size+ (+ column (* row (component-blocks-per-line component))))
                                 quantization
                                 work
                                 plane
                                 (+ (* column 8) (* row 8 stride))
                                 stride)))
    (setf (component-plane component) plane)))

(defun fixed-table (multiplier
                    offset)
  "Return a table of 256 numbers, each the product of the byte minus 128 and MULTIPLIER, plus OFFSET: the terms of the
conversion from YCbCr to RGB, which libjpeg keeps in fixed point.

MULTIPLIER: the factor, in fixed point.
OFFSET: what is added to each product."
  (let ((table (make-array 256 :element-type 'fixnum)))
    (dotimes (index 256 table)
      (setf (aref table index) (+ (* multiplier (- index +jpeg-sample-center+)) offset)))))

(defparameter *red-from-cr* (fixed-table 91881 (ash 1 (1- +color-fixed-bits+)))
  "The red term of a Cr sample, in fixed point and rounded.")

(defparameter *blue-from-cb* (fixed-table 116130 (ash 1 (1- +color-fixed-bits+)))
  "The blue term of a Cb sample, in fixed point and rounded.")

(defparameter *green-from-cr* (fixed-table -46802 0)
  "The green term of a Cr sample, in fixed point.")

(defparameter *green-from-cb* (fixed-table -22554 (ash 1 (1- +color-fixed-bits+)))
  "The green term of a Cb sample, in fixed point and rounded.")

(declaim (type (simple-array fixnum (256)) *red-from-cr* *blue-from-cb* *green-from-cr* *green-from-cb*))

(defun color-model (frame
                    adobe-transform
                    jfif)
  "Return how the components of a file make up colors: :GRAY, :YCC, :RGB, :CMYK or :YCCK.

FRAME: the frame header.
ADOBE-TRANSFORM: the transform byte of the Adobe marker, or NIL when the file has none.
JFIF: true when the file has the JFIF marker, which means YCbCr."
  (let ((components (frame-components frame)))
    (ecase (length components)
      (1 :gray)
      (3 (cond (adobe-transform
                (if (= adobe-transform +adobe-transform-none+) :rgb :ycc))
               (jfif :ycc)
               ((equal (map 'list #'component-id components) '(82 71 66)) :rgb)
               (t :ycc)))
      (4 (if (eql adobe-transform +adobe-transform-ycck+) :ycck :cmyk)))))

(defun component-columns (frame
                          component)
  "Return a vector that tells, for each column of the picture, which column of the plane of a component it takes its
sample from.

FRAME: the frame header.
COMPONENT: the component."
  (let ((columns (make-array (frame-width frame) :element-type 'fixnum)))
    (dotimes (column (frame-width frame) columns)
      (setf (aref columns column) (floor (* column (component-h component)) (frame-h-max frame))))))

(defun planes-to-raster (frame
                         model
                         inverted)
  "Return the raster that the planes of a frame make, converting the colors.

FRAME: the frame header, its components holding the planes.
MODEL: what COLOR-MODEL returned.
INVERTED: true for a file of CMYK that stores 255 where there is no ink, as the files made by Adobe programs do."
  (declare (optimize (speed 3) (safety 1))
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let* ((width (frame-width frame))
         (height (frame-height frame))
         (raster (make-raster width height))
         (pixels (raster-pixels raster))
         (components (frame-components frame))
         (count (length components))
         (maps (map 'vector (lambda (component) (component-columns frame component)) components))
         (starts (make-array 4 :element-type 'fixnum :initial-element 0))
         (strides (make-array 4 :element-type 'fixnum :initial-element 0))
         (planes (make-array 4 :initial-element (make-octets 0))))
    (declare (type fixnum width height count)
             (type octets pixels)
             (type (simple-array fixnum (4)) starts strides))
    (dotimes (index count)
      (setf (svref planes index) (component-plane (svref components index))
            (aref strides index) (* 8 (component-blocks-per-line (svref components index)))))
    (dotimes (row height)
      (dotimes (index count)
        (setf (aref starts index)
              (* (aref strides index)
                 (floor (* row (component-v (svref components index))) (frame-v-max frame)))))
      (let ((out (* +raster-channels+ row width))
            (first-plane (svref planes 0))
            (first-map (svref maps 0)))
        (declare (type fixnum out)
                 (type octets first-plane)
                 (type (simple-array fixnum (*)) first-map))
        (macrolet ((sample (index column)
                     `(aref (the octets (svref planes ,index))
                            (+ (aref starts ,index)
                               (aref (the (simple-array fixnum (*)) (svref maps ,index)) ,column))))
                   (clamp (value)
                     `(max 0 (min 255 ,value)))
                   (put (red green blue)
                     `(progn (setf (aref pixels out) ,red
                                   (aref pixels (+ out 1)) ,green
                                   (aref pixels (+ out 2)) ,blue
                                   (aref pixels (+ out 3)) +opaque+)
                             (incf out +raster-channels+))))
          (ecase model
            (:gray
             (dotimes (column width)
               (let ((gray (aref first-plane (+ (aref starts 0) (aref first-map column)))))
                 (put gray gray gray))))
            (:rgb
             (dotimes (column width)
               (put (sample 0 column) (sample 1 column) (sample 2 column))))
            (:ycc
             (dotimes (column width)
               (let ((y (sample 0 column))
                     (cb (sample 1 column))
                     (cr (sample 2 column)))
                 (put (clamp (+ y (ash (aref *red-from-cr* cr) (- +color-fixed-bits+))))
                      (clamp (+ y (ash (+ (aref *green-from-cb* cb) (aref *green-from-cr* cr))
                                       (- +color-fixed-bits+))))
                      (clamp (+ y (ash (aref *blue-from-cb* cb) (- +color-fixed-bits+))))))))
            ((:cmyk :ycck)
             (dotimes (column width)
               (let* ((k (sample 3 column))
                      (first (sample 0 column))
                      (second (sample 1 column))
                      (third (sample 2 column)))
                 (multiple-value-bind (cyan magenta yellow)
                     (if (eq model :ycck)
                         (values (- 255 (clamp (+ first (ash (aref *red-from-cr* third) (- +color-fixed-bits+)))))
                                 (- 255 (clamp (+ first (ash (+ (aref *green-from-cb* second)
                                                                (aref *green-from-cr* third))
                                                             (- +color-fixed-bits+)))))
                                 (- 255 (clamp (+ first (ash (aref *blue-from-cb* second) (- +color-fixed-bits+))))))
                         (values first second third))
                   (if inverted
                       (put (floor (* cyan k) 255)
                            (floor (* magenta k) 255)
                            (floor (* yellow k) 255))
                       (put (floor (* (- 255 cyan) (- 255 k)) 255)
                            (floor (* (- 255 magenta) (- 255 k)) 255)
                            (floor (* (- 255 yellow) (- 255 k)) 255)))))))))))
    raster))

;;; The file

(defparameter *jfif-tag*
  (coerce '(#x4A #x46 #x49 #x46 #x00) 'octets)
  "The first bytes of the JFIF application segment: the letters JFIF and a zero.")

(defparameter *adobe-tag*
  (coerce '(#x41 #x64 #x6F #x62 #x65) 'octets)
  "The first bytes of the Adobe application segment: the letters Adobe.")

(defun next-marker-position (bytes
                             position)
  "Return the position of the next marker at or after POSITION, or NIL when the file holds no more. A marker is a
0xFF byte followed by a byte that is neither zero (stuffing) nor 0xFF (filling).

BYTES: the file.
POSITION: where to start looking."
  (declare (type octets bytes)
           (type fixnum position))
  (loop for index of-type fixnum from position below (1- (length bytes))
        when (and (= (aref bytes index) +jpeg-marker-prefix+)
                  (/= (aref bytes (1+ index)) 0)
                  (/= (aref bytes (1+ index)) +jpeg-marker-prefix+))
          do (return index)))

(defun unsupported-jpeg ()
  "Signal IMAGE-ERROR for a JPEG file of a kind that is not read."
  (image-failure
   "This JPEG file uses arithmetic coding, lossless coding or 12-bit samples, which the program does not read."))

(defun read-jpeg-frame (bytes
                        start
                        end
                        progressive)
  "Return the frame header that the segment from START to END holds.

BYTES: the file.
START: the position of the first byte of the segment's data.
END: the position after the segment.
PROGRESSIVE: true for a progressive frame."
  (when (< (- end start) 6)
    (jpeg-damaged))
  (unless (= (aref bytes start) 8)
    (unsupported-jpeg))
  (let* ((height (octet-word bytes (+ start 1)))
         (width (octet-word bytes (+ start 3)))
         (count (aref bytes (+ start 5))))
    (when (zerop height)
      (jpeg-damaged))
    (check-picture-size width height)
    (unless (and (member count '(1 3 4))
                 (>= (- end start) (+ 6 (* 3 count))))
      (jpeg-damaged))
    (let* ((components (coerce (loop for index below count
                                     for at = (+ start 6 (* 3 index))
                                     collect (make-jpeg-component :id (aref bytes at)
                                                                  :h (ash (aref bytes (+ at 1)) -4)
                                                                  :v (logand (aref bytes (+ at 1)) 15)
                                                                  :quantization-id (aref bytes (+ at 2))))
                               'simple-vector))
           (valid (every (lambda (component)
                           (and (<= 1 (component-h component) +jpeg-maximum-sampling+)
                                (<= 1 (component-v component) +jpeg-maximum-sampling+)
                                (< (component-quantization-id component) +jpeg-maximum-tables+)))
                         components))
           (h-max (if valid (reduce #'max components :key #'component-h) (jpeg-damaged)))
           (v-max (reduce #'max components :key #'component-v))
           (frame (make-jpeg-frame :width width
                                   :height height
                                   :progressive progressive
                                   :components components
                                   :h-max h-max
                                   :v-max v-max
                                   :mcus-per-line (ceiling width (* 8 h-max))
                                   :mcus-per-column (ceiling height (* 8 v-max)))))
      (loop for component across components
            do (setf (component-blocks-per-line component) (* (frame-mcus-per-line frame) (component-h component))
                     (component-blocks-per-column component) (* (frame-mcus-per-column frame) (component-v component))
                     (component-scan-blocks-per-line component)
                     (ceiling (ceiling (* width (component-h component)) h-max) 8)
                     (component-scan-blocks-per-column component)
                     (ceiling (ceiling (* height (component-v component)) v-max) 8)
                     (component-coefficients component)
                     (make-array (* +jpeg-block-size+
                                    (component-blocks-per-line component)
                                    (component-blocks-per-column component))
                                 :element-type '(signed-byte 16)
                                 :initial-element 0)))
      frame)))

(defun read-quantization-tables (bytes
                                 start
                                 end
                                 tables)
  "Read the quantization tables of a DQT segment into TABLES, each in the order of a block.

BYTES: the file.
START: the position of the first byte of the segment's data.
END: the position after the segment.
TABLES: the vector of the four tables a file may define."
  (let ((position start))
    (loop while (< position end)
          do (let* ((precision (ash (aref bytes position) -4))
                    (id (logand (aref bytes position) 15))
                    (size (if (zerop precision) 1 2))
                    (table (make-array +jpeg-block-size+ :element-type 'fixnum :initial-element 1)))
               (unless (and (<= precision 1)
                            (< id +jpeg-maximum-tables+)
                            (<= (+ position 1 (* size +jpeg-block-size+)) end))
                 (jpeg-damaged))
               (dotimes (index +jpeg-block-size+)
                 (setf (aref table (aref *zigzag* index))
                       (if (= size 1)
                           (aref bytes (+ position 1 index))
                           (octet-word bytes (+ position 1 (* 2 index))))))
               (setf (svref tables id) table)
               (incf position (+ 1 (* size +jpeg-block-size+)))))))

(defun read-huffman-tables (bytes
                            start
                            end
                            dc-tables
                            ac-tables)
  "Read the Huffman tables of a DHT segment into the vectors for DC and AC tables.

BYTES: the file.
START: the position of the first byte of the segment's data.
END: the position after the segment.
DC-TABLES: the vector of the four tables for DC coefficients.
AC-TABLES: the vector of the four tables for AC coefficients."
  (let ((position start))
    (loop while (< position end)
          do (let* ((class (ash (aref bytes position) -4))
                    (id (logand (aref bytes position) 15)))
               (unless (and (<= class 1)
                            (< id +jpeg-maximum-tables+)
                            (<= (+ position 17) end))
                 (jpeg-damaged))
               (let* ((counts (subseq bytes (+ position 1) (+ position 17)))
                      (total (reduce #'+ counts)))
                 (unless (and (<= total 256)
                              (<= (+ position 17 total) end))
                   (jpeg-damaged))
                 (setf (svref (if (zerop class) dc-tables ac-tables) id)
                       (build-decoding-table-for-jpeg counts (subseq bytes (+ position 17) (+ position 17 total))))
                 (incf position (+ 17 total)))))))

(defun scan-components-of (bytes
                           start
                           end
                           frame
                           dc-tables
                           ac-tables)
  "Return the components of a scan, and the band and approximation it carries, as five values: the list of
SCAN-COMPONENTs, the first coefficient, the last, the bit position it refines from and the bit position it carries.

BYTES: the file.
START: the position of the first byte of the SOS segment's data.
END: the position after the segment.
FRAME: the frame header.
DC-TABLES: the vector of the four tables for DC coefficients.
AC-TABLES: the vector of the four tables for AC coefficients."
  (when (< (- end start) 1)
    (jpeg-damaged))
  (let ((count (aref bytes start)))
    (unless (and (<= 1 count 4)
                 (= (- end start) (+ 4 (* 2 count))))
      (jpeg-damaged))
    (let* ((scanned (loop for index below count
                          for at = (+ start 1 (* 2 index))
                          collect (let ((component (find (aref bytes at) (frame-components frame) :key #'component-id))
                                        (tables (aref bytes (1+ at))))
                                    (unless (and component
                                                 (< (ash tables -4) +jpeg-maximum-tables+)
                                                 (< (logand tables 15) +jpeg-maximum-tables+))
                                      (jpeg-damaged))
                                    (make-scan-component :component component
                                                         :dc-table (svref dc-tables (ash tables -4))
                                                         :ac-table (svref ac-tables (logand tables 15))))))
           (tail (+ start 1 (* 2 count)))
           (first (aref bytes tail))
           (last (aref bytes (+ tail 1)))
           (high (ash (aref bytes (+ tail 2)) -4))
           (low (logand (aref bytes (+ tail 2)) 15)))
      (unless (= count (length (remove-duplicates scanned :key #'scanned-component)))
        (jpeg-damaged))
      (cond ((not (frame-progressive frame))
             (dolist (each scanned)
               (unless (and (scanned-dc-table each) (scanned-ac-table each))
                 (jpeg-damaged)))
             (setf first 0
                   last 63
                   high 0
                   low 0))
            ((zerop first)
             (unless (and (zerop last)
                          (<= low 13)
                          (or (zerop high) (= high (1+ low))))
               (jpeg-damaged))
             (when (zerop high)
               (dolist (each scanned)
                 (unless (scanned-dc-table each)
                   (jpeg-damaged)))))
            (t
             (unless (and (= count 1)
                          (<= first last 63)
                          (<= low 13)
                          (or (zerop high) (= high (1+ low)))
                          (scanned-ac-table (first scanned)))
               (jpeg-damaged))))
      (values scanned first last high low))))

(defun read-jpeg (bytes)
  "Return the raster of a JPEG file. Signals IMAGE-ERROR when the file is damaged, is cut short, is of a kind that is
not read, or holds a picture larger than the program reads.

BYTES: the file."
  (declare (type octets bytes))
  (unless (and (>= (length bytes) 4)
               (= (aref bytes 0) +jpeg-marker-prefix+)
               (= (aref bytes 1) +marker-soi+))
    (jpeg-damaged))
  (let ((position 2)
        (quantization (make-array +jpeg-maximum-tables+ :initial-element nil))
        (dc-tables (make-array +jpeg-maximum-tables+ :initial-element nil))
        (ac-tables (make-array +jpeg-maximum-tables+ :initial-element nil))
        (frame nil)
        (restart-interval 0)
        (adobe-transform nil)
        (jfif nil)
        (scans 0))
    (loop
      (let ((marker-position (next-marker-position bytes position)))
        (unless marker-position
          (return))
        (let ((marker (aref bytes (1+ marker-position))))
          (setf position (+ marker-position 2))
          (cond ((= marker +marker-eoi+)
                 (return))
                ((or (= marker +marker-soi+)
                     (= marker 1)
                     (<= +marker-restart-first+ marker +marker-restart-last+)))
                (t
                 (let* ((size (octet-word bytes position))
                        (start (+ position 2))
                        (end (+ position size)))
                   (when (or (< size 2) (> end (length bytes)))
                     (jpeg-truncated))
                   (cond ((member marker (list +marker-sof0+ +marker-sof1+ +marker-sof2+))
                          (when frame
                            (jpeg-damaged))
                          (setf frame (read-jpeg-frame bytes
                                                       start
                                                       end
                                                       (= marker +marker-sof2+))))
                         ((and (<= #xC3 marker #xCF)
                               (not (member marker (list +marker-dht+ #xC8 #xCC))))
                          (unsupported-jpeg))
                         ((= marker +marker-dqt+)
                          (read-quantization-tables bytes
                                                    start
                                                    end
                                                    quantization))
                         ((= marker +marker-dht+)
                          (read-huffman-tables bytes
                                               start
                                               end
                                               dc-tables
                                               ac-tables))
                         ((= marker +marker-dri+)
                          (setf restart-interval (octet-word bytes start)))
                         ((and (= marker +marker-app14+)
                               (>= (- end start) 12)
                               (equalp (subseq bytes start (+ start 5)) *adobe-tag*))
                          (setf adobe-transform (aref bytes (+ start 11))))
                         ((and (= marker +marker-app0+)
                               (>= (- end start) 5)
                               (equalp (subseq bytes start (+ start 5)) *jfif-tag*))
                          (setf jfif t))
                         ((= marker +marker-sos+)
                          (unless frame
                            (jpeg-damaged))
                          (multiple-value-bind (scanned first last high low)
                              (scan-components-of bytes
                                                  start
                                                  end
                                                  frame
                                                  dc-tables
                                                  ac-tables)
                            (let ((bits (make-jpeg-bits :data bytes :position end)))
                              (decode-jpeg-scan frame
                                                bits
                                                scanned
                                                first
                                                last
                                                high
                                                low
                                                restart-interval)
                              (incf scans)
                              (setf end (bits-position bits))))))
                   (setf position end)))))))
    (unless (and frame (plusp scans))
      (jpeg-damaged))
    (loop for component across (frame-components frame)
          do (build-component-plane frame
                                    component
                                    quantization))
    (planes-to-raster frame
                      (color-model frame
                                   adobe-transform
                                   jfif)
                      (and adobe-transform t))))
