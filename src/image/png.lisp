(in-package #:borba-cad/image)

;;;; A PNG reader: every color type and bit depth, interlaced (Adam7) or not, with a palette, a transparency chunk or
;;;; an alpha channel. 16-bit samples are reduced to their high byte. Gamma and color profiles are ignored: a picture
;;;; to trace over is read for its shapes. The format is the PNG specification, ISO/IEC 15948.

(defconstant +png-signature-size+ 8
  "Bytes of the signature that opens a PNG file.")

(defconstant +png-chunk-overhead+ 12
  "Bytes of a chunk besides its data: the length, the type and the CRC.")

(defconstant +png-header-size+ 13
  "Bytes of the data of the IHDR chunk.")

(defconstant +png-gray+ 0
  "The color type of a picture of grays.")

(defconstant +png-rgb+ 2
  "The color type of a picture of red, green and blue.")

(defconstant +png-palette+ 3
  "The color type of a picture of indexes into a palette.")

(defconstant +png-gray-alpha+ 4
  "The color type of a picture of grays with opacity.")

(defconstant +png-rgba+ 6
  "The color type of a picture of red, green, blue and opacity.")

(defconstant +png-palette-entry-size+ 3
  "Bytes of a color of the palette.")

(defconstant +png-maximum-palette-size+ 256
  "The most colors a palette has.")

(defconstant +png-filter-none+ 0
  "The filter that leaves a row as it is.")

(defconstant +png-filter-sub+ 1
  "The filter that codes each byte as the difference from the pixel to its left.")

(defconstant +png-filter-up+ 2
  "The filter that codes each byte as the difference from the pixel above.")

(defconstant +png-filter-average+ 3
  "The filter that codes each byte as the difference from the mean of the pixels at left and above.")

(defconstant +png-filter-paeth+ 4
  "The filter that codes each byte as the difference from the nearest of left, above and upper left.")

(defparameter *png-signature*
  (coerce '(#x89 #x50 #x4E #x47 #x0D #x0A #x1A #x0A) 'octets)
  "The first eight bytes of every PNG file.")

(defparameter *adam7-passes*
  '((0 0 8 8) (4 0 8 8) (0 4 4 8) (2 0 4 4) (0 2 2 4) (1 0 2 2) (0 1 1 2))
  "The seven passes of an interlaced picture, each as (first column, first row, column step, row step).")

(defparameter *png-depths*
  '((0 1 2 4 8 16) (2 8 16) (3 1 2 4 8) (4 8 16) (6 8 16))
  "The bit depths each color type allows, as (color type . depths).")

(defstruct (png-header (:conc-name header-))
  "What the IHDR chunk of a PNG file says."
  (width 0 :type fixnum)
  (height 0 :type fixnum)
  (depth 8 :type fixnum)
  (color-type 0 :type fixnum)
  (interlaced nil))

(defun png-damaged ()
  "Signal IMAGE-ERROR for a PNG file that does not follow the format."
  (image-failure "This PNG file is damaged."))

(defun png-channels (color-type)
  "Return how many samples a pixel of that color type has.

COLOR-TYPE: the color type of the header."
  (ecase color-type
    (0 1)
    (2 3)
    (3 1)
    (4 2)
    (6 4)))

(defun parse-png-header (bytes
                         position)
  "Return the header that the data of an IHDR chunk holds, or signal IMAGE-ERROR when it is not valid.

BYTES: the file.
POSITION: where the data of the chunk starts."
  (let ((header (make-png-header :width (octet-long bytes position)
                                 :height (octet-long bytes (+ position 4))
                                 :depth (aref bytes (+ position 8))
                                 :color-type (aref bytes (+ position 9))
                                 :interlaced (case (aref bytes (+ position 12))
                                               (0 nil)
                                               (1 t)
                                               (otherwise (png-damaged))))))
    (unless (and (zerop (aref bytes (+ position 10)))
                 (zerop (aref bytes (+ position 11)))
                 (member (header-depth header) (cdr (assoc (header-color-type header) *png-depths*))))
      (png-damaged))
    (check-picture-size (header-width header) (header-height header))
    header))

(defun read-png-chunks (bytes)
  "Return the header, the palette bytes, the transparency bytes and the data of a PNG file, as four values: the
palette and transparency are NIL when the file has none, and the data is the IDAT chunks joined. Signals IMAGE-ERROR
when the file is not a PNG or its chunks are damaged.

BYTES: the file."
  (unless (and (>= (length bytes) (+ +png-signature-size+ +png-chunk-overhead+))
               (null (mismatch *png-signature* bytes :end2 +png-signature-size+)))
    (png-damaged))
  (let ((position +png-signature-size+)
        (header nil)
        (palette nil)
        (transparency nil)
        (segments '())
        (total 0))
    (loop
      (when (> (+ position +png-chunk-overhead+) (length bytes))
        (return))
      (let* ((size (octet-long bytes position))
             (type (map 'string #'code-char (subseq bytes (+ position 4) (+ position 8))))
             (data (+ position 8))
             (next (+ data size 4)))
        (when (> next (length bytes))
          (image-failure "This picture file ends before the picture does."))
        (when (member type '("IHDR" "PLTE" "IDAT") :test #'string=)
          (unless (= (octet-long bytes (+ data size))
                     (crc-32 bytes
                             (+ position 4)
                             (+ data size)))
            (png-damaged)))
        (cond ((string= type "IHDR")
               (unless (and (null header) (= size +png-header-size+))
                 (png-damaged))
               (setf header (parse-png-header bytes data)))
              ((null header)
               (png-damaged))
              ((string= type "PLTE")
               (unless (and (plusp size)
                            (zerop (mod size +png-palette-entry-size+))
                            (<= size (* +png-maximum-palette-size+ +png-palette-entry-size+)))
                 (png-damaged))
               (setf palette (subseq bytes data (+ data size))))
              ((string= type "tRNS")
               (setf transparency (subseq bytes data (+ data size))))
              ((string= type "IDAT")
               (push (cons data size) segments)
               (incf total size))
              ((string= type "IEND")
               (return)))
        (setf position next)))
    (when (or (null header)
              (null segments))
      (png-damaged))
    (when (and (= (header-color-type header) +png-palette+)
               (null palette))
      (png-damaged))
    (let ((joined (make-octets total))
          (fill 0))
      (dolist (segment (reverse segments))
        (replace joined bytes
                 :start1 fill
                 :start2 (car segment)
                 :end2 (+ (car segment) (cdr segment)))
        (incf fill (cdr segment)))
      (values header palette transparency joined))))

(defun png-passes (header)
  "Return the passes of a picture as (first column, first row, column step, row step, columns, rows) lists, leaving
out those that hold no pixel. A picture that is not interlaced is one pass.

HEADER: the header of the file."
  (let ((width (header-width header))
        (height (header-height header)))
    (if (header-interlaced header)
        (loop for (x0 y0 dx dy) in *adam7-passes*
              for columns = (ceiling (- width x0) dx)
              for rows = (ceiling (- height y0) dy)
              when (and (plusp columns) (plusp rows))
                collect (list x0 y0 dx dy columns rows))
        (list (list 0 0 1 1 width height)))))

(defun png-row-bytes (header
                      columns)
  "Return how many bytes a row of COLUMNS pixels takes, without its filter byte.

HEADER: the header of the file.
COLUMNS: pixels in the row."
  (ceiling (* columns (png-channels (header-color-type header)) (header-depth header)) 8))

(defun paeth-predictor (left
                        above
                        upper-left)
  "Return the one of LEFT, ABOVE and UPPER-LEFT that is nearest to LEFT + ABOVE - UPPER-LEFT, as the Paeth filter does.

LEFT: the byte at the left.
ABOVE: the byte above.
UPPER-LEFT: the byte above and at the left."
  (declare (type (unsigned-byte 8) left above upper-left)
           (optimize (speed 3) (safety 0))
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let* ((estimate (- (+ left above) upper-left))
         (from-left (abs (- estimate left)))
         (from-above (abs (- estimate above)))
         (from-upper-left (abs (- estimate upper-left))))
    (declare (type fixnum estimate from-left from-above from-upper-left))
    (cond ((and (<= from-left from-above) (<= from-left from-upper-left)) left)
          ((<= from-above from-upper-left) above)
          (t upper-left))))

(defun unfilter-rows (raw
                      offset
                      rows
                      row-bytes
                      pixel-bytes)
  "Undo the filters of ROWS rows of RAW in place. Each row is a filter byte and ROW-BYTES bytes. Signals IMAGE-ERROR
for a filter that does not exist.

RAW: the inflated data.
OFFSET: where the first row starts.
ROWS: how many rows.
ROW-BYTES: bytes in a row, after the filter byte.
PIXEL-BYTES: bytes the filters look back by, a pixel's but at least one."
  (declare (type octets raw)
           (type fixnum offset rows row-bytes pixel-bytes)
           (optimize (speed 3) (safety 1))
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((stride (1+ row-bytes)))
    (declare (type fixnum stride))
    (dotimes (row rows)
      (let* ((start (+ offset (* row stride)))
             (data (1+ start))
             (previous (- data stride))
             (filter (aref raw start)))
        (declare (type fixnum start data previous filter))
        (flet ((left (index)
                 (declare (type fixnum index))
                 (if (>= index pixel-bytes) (aref raw (+ data (- index pixel-bytes))) 0))
               (above (index)
                 (declare (type fixnum index))
                 (if (plusp row) (aref raw (+ previous index)) 0))
               (upper-left (index)
                 (declare (type fixnum index))
                 (if (and (plusp row) (>= index pixel-bytes)) (aref raw (+ previous (- index pixel-bytes))) 0)))
          (declare (inline left above upper-left))
          (case filter
            (#.+png-filter-none+)
            (#.+png-filter-sub+
             (dotimes (index row-bytes)
               (setf (aref raw (+ data index))
                     (logand #xFF (+ (aref raw (+ data index)) (left index))))))
            (#.+png-filter-up+
             (when (plusp row)
               (dotimes (index row-bytes)
                 (setf (aref raw (+ data index))
                       (logand #xFF (+ (aref raw (+ data index)) (aref raw (+ previous index))))))))
            (#.+png-filter-average+
             (dotimes (index row-bytes)
               (setf (aref raw (+ data index))
                     (logand #xFF (+ (aref raw (+ data index))
                                     (ash (+ (left index) (above index)) -1))))))
            (#.+png-filter-paeth+
             (dotimes (index row-bytes)
               (setf (aref raw (+ data index))
                     (logand #xFF (+ (aref raw (+ data index))
                                     (paeth-predictor (left index)
                                                      (above index)
                                                      (upper-left index)))))))
            (otherwise
             (png-damaged))))))))

(declaim (inline sample-at))

(defun sample-at (raw
                  start
                  depth
                  index)
  "Return the sample at INDEX of a row, as a number of DEPTH bits.

RAW: the inflated data.
START: where the row's bytes begin.
DEPTH: bits in a sample.
INDEX: which sample of the row."
  (declare (type octets raw)
           (type fixnum start depth index)
           (optimize (speed 3) (safety 1))
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (case depth
    (8 (aref raw (+ start index)))
    (16 (logior (ash (aref raw (+ start (* 2 index))) 8)
                (aref raw (+ start (* 2 index) 1))))
    (t (let* ((bit (* index depth))
              (byte (aref raw (+ start (ash bit -3)))))
         (logand (ash byte (- (- 8 depth (logand bit 7))))
                 (1- (ash 1 depth)))))))

(defun png-high-byte (sample
                      depth)
  "Return a sample of DEPTH bits as a byte, scaling the smaller depths up to the full range.

SAMPLE: the sample.
DEPTH: its bits."
  (declare (type fixnum sample depth)
           (optimize (speed 3) (safety 0))
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (case depth
    (8 sample)
    (16 (ash sample -8))
    (4 (* sample 17))
    (2 (* sample 85))
    (t (* sample 255))))

(defun png-transparent-colors (header
                               transparency)
  "Return the colors the transparency chunk makes clear, for a picture of grays or of colors: NIL, a gray sample, or a
list of the three samples of a color.

HEADER: the header of the file.
TRANSPARENCY: the data of the tRNS chunk, or NIL."
  (when transparency
    (case (header-color-type header)
      (#.+png-gray+ (when (>= (length transparency) 2)
                      (octet-word transparency 0)))
      (#.+png-rgb+ (when (>= (length transparency) 6)
                     (list (octet-word transparency 0)
                           (octet-word transparency 2)
                           (octet-word transparency 4)))))))

(defun convert-png-row (raw
                        start
                        header
                        palette
                        transparency
                        clear-color
                        pixels
                        out
                        columns
                        step)
  "Write the pixels of one row of a PNG as RGBA bytes into a raster.

RAW: the unfiltered data.
START: where the row's bytes begin.
HEADER: the header of the file.
PALETTE: the palette bytes, or NIL.
TRANSPARENCY: the data of the tRNS chunk, or NIL.
CLEAR-COLOR: what PNG-TRANSPARENT-COLORS returned.
PIXELS: the bytes of the raster.
OUT: where the first pixel goes.
COLUMNS: how many pixels the row has.
STEP: how many bytes apart consecutive pixels go."
  (declare (type octets raw pixels)
           (type fixnum start out columns step)
           (optimize (speed 3) (safety 1))
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((depth (header-depth header)))
    (declare (type fixnum depth))
    (macrolet ((put (red green blue alpha)
                 `(progn (setf (aref pixels out) ,red
                               (aref pixels (+ out 1)) ,green
                               (aref pixels (+ out 2)) ,blue
                               (aref pixels (+ out 3)) ,alpha)
                         (incf out step))))
      (ecase (header-color-type header)
        (#.+png-gray+
         (dotimes (column columns)
           (let* ((sample (sample-at raw
                                     start
                                     depth
                                     column))
                  (gray (png-high-byte sample depth)))
             (put gray gray gray (if (eql sample clear-color) 0 +opaque+)))))
        (#.+png-rgb+
         (dotimes (column columns)
           (let* ((red (sample-at raw
                                  start
                                  depth
                                  (* 3 column)))
                  (green (sample-at raw
                                    start
                                    depth
                                    (+ 1 (* 3 column))))
                  (blue (sample-at raw
                                   start
                                   depth
                                   (+ 2 (* 3 column)))))
             (put (png-high-byte red depth)
                  (png-high-byte green depth)
                  (png-high-byte blue depth)
                  (if (and clear-color (equal clear-color (list red green blue))) 0 +opaque+)))))
        (#.+png-palette+
         (let ((entries (floor (length palette) +png-palette-entry-size+)))
           (dotimes (column columns)
             (let ((index (sample-at raw
                                     start
                                     depth
                                     column)))
               (when (>= index entries)
                 (png-damaged))
               (put (aref palette (* 3 index))
                    (aref palette (+ 1 (* 3 index)))
                    (aref palette (+ 2 (* 3 index)))
                    (if (and transparency (< index (length transparency)))
                        (aref transparency index)
                        +opaque+))))))
        (#.+png-gray-alpha+
         (dotimes (column columns)
           (let ((gray (png-high-byte (sample-at raw
                                                 start
                                                 depth
                                                 (* 2 column)) depth)))
             (put gray gray gray (png-high-byte (sample-at raw
                                                           start
                                                           depth
                                                           (1+ (* 2 column))) depth)))))
        (#.+png-rgba+
         (dotimes (column columns)
           (put (png-high-byte (sample-at raw
                                          start
                                          depth
                                          (* 4 column)) depth)
                (png-high-byte (sample-at raw
                                          start
                                          depth
                                          (+ 1 (* 4 column))) depth)
                (png-high-byte (sample-at raw
                                          start
                                          depth
                                          (+ 2 (* 4 column))) depth)
                (png-high-byte (sample-at raw
                                          start
                                          depth
                                          (+ 3 (* 4 column))) depth))))))))

(defun read-png (bytes)
  "Return the raster of a PNG file. Signals IMAGE-ERROR when the file is damaged, is cut short, or holds a picture
larger than the program reads.

BYTES: the file."
  (multiple-value-bind (header palette transparency data)
      (read-png-chunks bytes)
    (let* ((passes (png-passes header))
           (expected (loop for (nil nil
                                    nil
                                    nil
                                    columns
                                    rows) in passes
                           sum (* rows (1+ (png-row-bytes header columns)))))
           (raw (inflate data
                         :size-hint expected
                         :limit expected))
           (raster (make-raster (header-width header) (header-height header)))
           (pixel-bytes (max 1 (floor (* (png-channels (header-color-type header)) (header-depth header)) 8)))
           (clear-color (png-transparent-colors header transparency))
           (offset 0))
      (unless (= (length raw) expected)
        (image-failure "This picture file ends before the picture does."))
      (loop for (x0 y0 dx dy columns rows) in passes
            for row-bytes = (png-row-bytes header columns)
            do (unfilter-rows raw
                              offset
                              rows
                              row-bytes
                              pixel-bytes)
               (dotimes (row rows)
                 (convert-png-row raw
                                  (+ offset (* row (1+ row-bytes)) 1)
                                  header
                                  palette
                                  transparency
                                  clear-color
                                  (raster-pixels raster)
                                  (* +raster-channels+ (+ x0 (* (+ y0 (* row dy)) (header-width header))))
                                  columns
                                  (* +raster-channels+ dx)))
               (incf offset (* rows (1+ row-bytes))))
      raster)))
