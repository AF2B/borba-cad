(in-package #:borba-cad/platform)

;;;; A minimal PNG encoder (RGB, 8 bits, stored deflate blocks) so screenshots need no library.

(defconstant +png-stored-block-limit+ 65535
  "Largest payload of one stored deflate block.")

(sb-ext:defglobal **crc-table**
  (let ((table (make-array 256 :element-type '(unsigned-byte 32))))
    (dotimes (n 256 table)
      (let ((c n))
        (dotimes (k 8)
          (setf c (if (logbitp 0 c) (logxor #xEDB88320 (ash c -1)) (ash c -1))))
        (setf (aref table n) c)))))

(defun crc32 (bytes)
  "Return the CRC-32 of a byte vector, as PNG chunks require.

BYTES: vector of octets."
  (let ((c #xFFFFFFFF))
    (declare (type (unsigned-byte 32) c))
    (loop for byte across bytes
          do (setf c (logxor (aref **crc-table** (logand (logxor c byte) #xFF)) (ash c -8))))
    (logxor c #xFFFFFFFF)))

(defun adler32 (bytes)
  "Return the Adler-32 checksum of a byte vector, as the zlib stream requires.

BYTES: vector of octets."
  (let ((a 1) (b 0))
    (loop for byte across bytes
          do (setf a (mod (+ a byte) 65521)
                   b (mod (+ b a) 65521)))
    (logior (ash b 16) a)))

(defun big-endian-32 (number)
  "Return NUMBER as four big-endian octets in a list.

NUMBER: an unsigned 32-bit integer."
  (list (ldb (byte 8 24) number) (ldb (byte 8 16) number) (ldb (byte 8 8) number) (ldb (byte 8 0) number)))

(defun octets (sequence)
  "Return SEQUENCE as a simple octet vector.

SEQUENCE: a sequence of integers 0 to 255."
  (coerce sequence '(simple-array (unsigned-byte 8) (*))))

(defun write-png-chunk (stream
                        type
                        data)
  "Write one PNG chunk.

STREAM: binary output stream.
TYPE: four-letter chunk type.
DATA: octet vector."
  (let ((type-bytes (map '(simple-array (unsigned-byte 8) (*)) #'char-code type)))
    (write-sequence (octets (big-endian-32 (length data))) stream)
    (write-sequence type-bytes stream)
    (write-sequence data stream)
    (write-sequence (octets (big-endian-32 (crc32 (concatenate '(simple-array (unsigned-byte 8) (*)) type-bytes data))))
                    stream)))

(defun write-rgb-png (path
                      width
                      height
                      pixels
                      &key (flip-vertical t))
  "Write an RGB image as a PNG file.

PATH: output file name.
WIDTH: image width in pixels.
HEIGHT: image height in pixels.
PIXELS: octet vector of WIDTH x HEIGHT x 3 values, one row after another.
FLIP-VERTICAL: true when the rows run bottom to top, as OpenGL returns them."
  (let* ((stride (* width 3))
         (raw (make-array (* height (1+ stride)) :element-type '(unsigned-byte 8))))
    (dotimes (row height)
      (let ((source-row (if flip-vertical (- height 1 row) row))
            (target (* row (1+ stride))))
        (setf (aref raw target) 0)
        (replace raw pixels :start1 (1+ target) :start2 (* source-row stride) :end2 (* (1+ source-row) stride))))
    (let ((stream-data (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
      (vector-push-extend #x78 stream-data)
      (vector-push-extend #x01 stream-data)
      (loop with total = (length raw)
            for start from 0 below (max total 1) by +png-stored-block-limit+
            for end = (min total (+ start +png-stored-block-limit+))
            for length = (- end start)
            do (vector-push-extend (if (= end total) 1 0) stream-data)
               (vector-push-extend (ldb (byte 8 0) length) stream-data)
               (vector-push-extend (ldb (byte 8 8) length) stream-data)
               (vector-push-extend (ldb (byte 8 0) (lognot length)) stream-data)
               (vector-push-extend (ldb (byte 8 8) (lognot length)) stream-data)
               (loop for index from start below end
                     do (vector-push-extend (aref raw index) stream-data)))
      (dolist (byte (big-endian-32 (adler32 raw)))
        (vector-push-extend byte stream-data))
      (with-open-file (out path :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
        (write-sequence (octets '(137 80 78 71 13 10 26 10)) out)
        (write-png-chunk out
                         "IHDR"
                         (octets (append (big-endian-32 width) (big-endian-32 height) '(8 2 0 0 0))))
        (write-png-chunk out
                         "IDAT"
                         (octets stream-data))
        (write-png-chunk out
                         "IEND"
                         (octets '())))
      path)))
