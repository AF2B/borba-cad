(in-package #:borba-cad/image)

;;;; Reading a picture file of any kind the program knows, from its bytes: the kind is told by how the file begins,
;;;; never by its name.

(defconstant +maximum-picture-bytes+ (* 48 1024 1024)
  "The largest picture file the program takes, in bytes. The drawing keeps the file whole, as text.")

(defparameter *jpeg-signature*
  (coerce '(#xFF #xD8 #xFF) 'octets)
  "The first bytes of every JPEG file.")

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

(defun picture-kind (bytes)
  "Return :PNG, :JPEG or :PDF according to how a file begins, or NIL for a file of any other kind.

BYTES: the file."
  (declare (type octets bytes))
  (cond ((starts-with-p bytes *png-signature*) :png)
        ((starts-with-p bytes *jpeg-signature*) :jpeg)
        ((starts-with-p bytes *pdf-signature*) :pdf)))

(defun check-picture-bytes (bytes)
  "Signal IMAGE-ERROR when a picture file is empty or larger than the program takes.

BYTES: the file."
  (declare (type octets bytes))
  (when (> (length bytes) +maximum-picture-bytes+)
    (image-failure "This picture file is larger than the program takes."))
  (when (zerop (length bytes))
    (image-failure "This picture file is empty.")))

(defun read-picture (bytes)
  "Return the raster of a PNG or JPEG file. Signals IMAGE-ERROR when the file is of another kind, damaged, or too large.

BYTES: the file."
  (declare (type octets bytes))
  (check-picture-bytes bytes)
  (ecase (or (picture-kind bytes)
             (image-failure "This file is not a PNG or JPEG picture."))
    (:png (read-png bytes))
    (:jpeg (read-jpeg bytes))))
