(in-package #:borba-cad/test-support)

;;;; FNV-1a (64 bit) checksums, used to prove that two runs of a benchmark built the very same
;;;; workload: equal checksums mean equal drawings, byte for byte.

(defconstant +fnv-offset-basis+ #xCBF29CE484222325
  "FNV-1a 64-bit offset basis.")

(defconstant +fnv-prime+ #x100000001B3
  "FNV-1a 64-bit prime.")

(defun fnv1a-64 (string)
  "Return the FNV-1a 64-bit hash of the UTF-8 encoding of STRING.

STRING: the text to hash."
  (let ((hash +fnv-offset-basis+)
        (octets (sb-ext:string-to-octets string :external-format :utf-8)))
    (declare (type (unsigned-byte 64) hash)
             (type (simple-array (unsigned-byte 8) (*)) octets)
             (optimize (speed 3) (safety 0)))
    (loop for octet across octets
          do (setf hash (ldb (byte 64 0) (* (logxor hash octet) +fnv-prime+))))
    hash))

(defun drawing-checksum (drawing)
  "Return the checksum of the canonical text of DRAWING, as 16 hexadecimal digits.

DRAWING: the drawing."
  (format nil "~(~16,'0x~)" (fnv1a-64 (borba-cad/persistence:drawing-to-string drawing))))
