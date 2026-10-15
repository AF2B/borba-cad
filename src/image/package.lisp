(defpackage #:borba-cad/image
  (:use #:cl)
  (:documentation "Making picture files from pixels: a JPEG encoder in plain Lisp, so that the program needs no
library to export what the canvas shows.")
  (:export
   #:+default-jpeg-quality+
   #:encode-jpeg))
