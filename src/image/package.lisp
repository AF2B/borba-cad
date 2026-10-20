(defpackage #:borba-cad/image
  (:use #:cl)
  (:local-nicknames (#:base #:borba-cad/base)
                    (#:i18n #:borba-cad/i18n))
  (:documentation "Making picture files from pixels, a JPEG encoder in plain Lisp, and reading them back: PNG and
JPEG files and the pictures inside a PDF, so that the program needs no library to export what the canvas shows or to
show a scan under the drawing.")
  (:export
   #:+default-jpeg-quality+
   #:encode-jpeg
   ;; Reading
   #:octets
   #:raster
   #:raster-width
   #:raster-height
   #:raster-pixels
   #:make-raster
   #:reduce-raster
   #:+raster-channels+
   #:+maximum-picture-bytes+
   #:picture-kind
   #:picture-page-count
   #:read-picture))
