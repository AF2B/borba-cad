(in-package #:borba-cad/image)

;;;; Reading a picture file of any kind the program knows, from its bytes: the kind is told by how the file begins,
;;;; never by its name.

(defconstant +maximum-picture-bytes+ (* 48 1024 1024)
  "The largest picture file the program takes, in bytes. The drawing keeps the file whole, as text.")

(defparameter *jpeg-signature*
  (coerce '(#xFF #xD8 #xFF) 'octets)
  "The first bytes of every JPEG file.")

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

(defun picture-page-count (bytes)
  "Return how many pages a picture file has: those of a PDF, one for the other kinds.

BYTES: the file."
  (declare (type octets bytes))
  (check-picture-bytes bytes)
  (if (eq (picture-kind bytes) :pdf)
      (length (pdf-pages (open-pdf bytes)))
      1))

(defun read-picture (bytes
                     &key (page 1)
                          dpi)
  "Return the raster of a PNG or JPEG file, or of a page of a PDF, and the resolution a PDF page was drawn at in dots
per inch (NIL for the other kinds, which have no size on paper). Signals IMAGE-ERROR when the file is of another
kind, damaged, protected, or too large.

BYTES: the file.
PAGE: which page of a PDF, counted from one.
DPI: the resolution to draw a PDF page at, or NIL to choose it from the page."
  (declare (type octets bytes))
  (check-picture-bytes bytes)
  (handler-case
      (ecase (or (picture-kind bytes)
                 (image-failure "This file is not a PNG, JPEG or PDF picture."))
        (:png (values (read-png bytes) nil))
        (:jpeg (values (read-jpeg bytes) nil))
        (:pdf (let* ((document (open-pdf bytes))
                     (pages (pdf-pages document)))
                (unless (<= 1 page (length pages))
                  (image-failure "This PDF has ~d pages, so there is no page ~d."
                                 (length pages)
                                 page))
                (render-pdf-page document
                                 (nth (1- page) pages)
                                 dpi))))
    (arithmetic-error ()
      (image-failure "This picture holds numbers the program cannot work with."))
    (storage-condition ()
      (image-failure "This picture needs more memory than the program has."))))
