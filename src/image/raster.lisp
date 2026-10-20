(in-package #:borba-cad/image)

;;;; The pictures the program reads: their pixels, and the checks that keep a hostile or broken file from asking for
;;;; more memory than the program means to give.

(defconstant +raster-channels+ 4
  "Bytes of a pixel of a raster: red, green, blue and opacity.")

(defconstant +opaque+ 255
  "The opacity byte of a pixel that hides what is under it.")

(defconstant +maximum-raster-pixels+ 50000000
  "The most pixels of a picture the program reads. A raster holds four bytes of each, and the picture is held as the
file and as the raster at once, so this bounds the memory a picture takes.")

(defconstant +maximum-raster-side+ 65535
  "The longest side of a picture the program reads, in pixels.")

(defstruct (raster (:constructor %make-raster))
  "A picture: its size in pixels and its pixels as bytes, four each (red, green, blue, opacity), row by row from the
top."
  (width 0 :type fixnum)
  (height 0 :type fixnum)
  (pixels (make-octets 0) :type octets))

(defun check-picture-size (width
                           height)
  "Signal IMAGE-ERROR when a picture of that size is empty or larger than the program reads.

WIDTH: width in pixels.
HEIGHT: height in pixels."
  (when (or (<= width 0)
            (<= height 0))
    (image-failure "This picture has no pixels."))
  (when (or (> width +maximum-raster-side+)
            (> height +maximum-raster-side+)
            (> (* width height) +maximum-raster-pixels+))
    (image-failure "This picture is larger than the program reads.")))

(defun make-raster (width
                    height)
  "Return a raster of that size, all pixels transparent black, or signal IMAGE-ERROR when it is too large.

WIDTH: width in pixels.
HEIGHT: height in pixels."
  (check-picture-size width height)
  (%make-raster :width width
                :height height
                :pixels (make-octets (* width height +raster-channels+))))

(defun octet-word (octets
                   position)
  "Return the big-endian 16-bit number at POSITION, or signal IMAGE-ERROR when the vector ends before it.

OCTETS: the vector.
POSITION: where the number starts."
  (declare (type octets octets)
           (type fixnum position))
  (unless (<= 0 position (- (length octets) 2))
    (image-failure "This picture file ends before the picture does."))
  (logior (ash (aref octets position) 8)
          (aref octets (1+ position))))

(defun octet-long (octets
                   position)
  "Return the big-endian 32-bit number at POSITION, or signal IMAGE-ERROR when the vector ends before it.

OCTETS: the vector.
POSITION: where the number starts."
  (declare (type octets octets)
           (type fixnum position))
  (unless (<= 0 position (- (length octets) 4))
    (image-failure "This picture file ends before the picture does."))
  (logior (ash (aref octets position) 24)
          (ash (aref octets (+ position 1)) 16)
          (ash (aref octets (+ position 2)) 8)
          (aref octets (+ position 3))))

(sb-ext:defglobal **crc-table**
  (let ((table (make-array 256 :element-type '(unsigned-byte 32))))
    (dotimes (index 256 table)
      (let ((crc index))
        (dotimes (bit 8)
          (setf crc (if (logbitp 0 crc)
                        (logxor #xEDB88320 (ash crc -1))
                        (ash crc -1))))
        (setf (aref table index) crc))))
  "The table of the CRC-32 that PNG chunks carry, for each byte.")

(defun crc-32 (octets
               start
               end)
  "Return the CRC-32 of a stretch of a vector, as PNG chunks carry.

OCTETS: the vector.
START: the first byte of the stretch.
END: the position after its last byte."
  (declare (type octets octets)
           (type fixnum start end)
           (optimize (speed 3) (safety 1))
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((crc #xFFFFFFFF))
    (declare (type (unsigned-byte 32) crc))
    (loop for position of-type fixnum from start below end
          do (setf crc (logxor (aref (the (simple-array (unsigned-byte 32) (256)) **crc-table**)
                                     (logand (logxor crc (aref octets position)) #xFF))
                               (ash crc -8))))
    (logxor crc #xFFFFFFFF)))

(defparameter *pdf-signature*
  (coerce '(#x25 #x50 #x44 #x46 #x2D) 'octets)
  "The first bytes of every PDF file: %PDF-.")

(defun starts-with-p (bytes
                      signature)
  "Return true when the vector begins with the bytes of a signature.

BYTES: the vector.
SIGNATURE: the bytes it must begin with."
  (declare (type octets bytes signature))
  (and (>= (length bytes) (length signature))
       (null (mismatch signature bytes :end2 (length signature)))))
