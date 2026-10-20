(in-package #:borba-cad/tests)

(def-suite pdfs
  :description "Reading PDF files: their objects and filters, their pages drawn with lines, fills, clips and
pictures, and the refusal of what cannot be read."
  :in all)

(in-suite pdfs)

;;; Building PDF files for the tests

(defun latin-1 (text)
  "Return the bytes of TEXT, each character as one byte."
  (map '(simple-array (unsigned-byte 8) (*)) #'char-code text))

(defun joined (&rest pieces)
  "Return the bytes of all PIECES, strings or vectors of bytes, one after another."
  (let ((all (loop for piece in pieces
                   append (coerce (if (stringp piece) (latin-1 piece) piece) 'list))))
    (coerce all '(simple-array (unsigned-byte 8) (*)))))

(defun stored-zlib (bytes)
  "Return BYTES in a zlib stream made of one stored block."
  (joined (octets-of #x78 #x01 #x01 (ldb (byte 8 0) (length bytes)) (ldb (byte 8 8) (length bytes))
                     (ldb (byte 8 0) (logxor #xFFFF (length bytes))) (ldb (byte 8 8) (logxor #xFFFF (length bytes))))
          bytes
          (octets-of 0 0 0 0)))

(defun pdf-object-bytes (number
                         dictionary
                         &optional stream)
  "Return the text of an indirect object: its number, the entries of its dictionary and, if given, a stream."
  (if stream
      (joined (format nil "~d 0 obj~%<< ~a /Length ~d >>~%stream~%" number dictionary (length stream))
              stream
              (format nil "~%endstream~%endobj~%"))
      (joined (format nil "~d 0 obj~%<< ~a >>~%endobj~%" number dictionary))))

(defun pdf-with-objects (&rest objects)
  "Return a PDF file made of OBJECTS, each a list of number, dictionary text and an optional stream; the first is the
catalog and the file has no cross-reference table, as a damaged one has none."
  (apply #'joined
         "%PDF-1.4"
         (string #\Newline)
         (append (loop for (number dictionary stream) in objects
                       collect (pdf-object-bytes number dictionary stream))
                 (list (format nil "trailer~%<< /Root 1 0 R /Size 99 >>~%%%EOF~%")))))

(defun page-pdf (content
                 &key (width 100)
                      (height 80)
                      (resources "")
                      (page-entries "")
                      (pages-entries "")
                      extra-objects
                      (content-dictionary ""))
  "Return a PDF file of one page WIDTH by HEIGHT points whose content is the text or bytes CONTENT."
  (apply #'pdf-with-objects
         (list 1 "/Type /Catalog /Pages 2 0 R")
         (list 2 (format nil "/Type /Pages /Kids [3 0 R] /Count 1 ~a" pages-entries))
         (list 3 (format nil "/Type /Page /Parent 2 0 R /MediaBox [0 0 ~d ~d] /Contents 4 0 R /Resources << ~a >> ~a"
                         width height resources page-entries))
         (list 4 content-dictionary (if (stringp content) (latin-1 content) content))
         extra-objects))

(defun draw-page (content
                  &rest keys)
  "Draw the page of CONTENT, built with the other arguments as PAGE-PDF takes them, at 72 dots per inch, which makes
a pixel of a point."
  (image:read-picture (apply #'page-pdf content keys) :dpi 72))

(defun pixel-at (raster
                 x
                 y)
  "Return the red, green and blue of the pixel at column X and row Y of RASTER, as a list."
  (let ((offset (* 4 (+ x (* y (image:raster-width raster))))))
    (list (aref (image:raster-pixels raster) offset)
          (aref (image:raster-pixels raster) (+ offset 1))
          (aref (image:raster-pixels raster) (+ offset 2)))))

(defparameter *white* '(255 255 255)
  "The color of the paper a page is drawn on.")

(defun near-color-p (actual
                     expected
                     &optional (tolerance 2))
  "Return true when each channel of the color ACTUAL is within TOLERANCE of the one of EXPECTED."
  (every (lambda (a e) (<= (abs (- a e)) tolerance)) actual expected))

(defun pdf-error-text (thunk)
  "Return the text of the IMAGE-ERROR that calling THUNK signals, or NIL when it signals none."
  (handler-case (progn (funcall thunk)
                       nil)
    (base:image-error (condition)
      (princ-to-string condition))))

;;; Syntax

(defun parse-text (text)
  "Return the PDF value that TEXT holds."
  (image::read-pdf-value (image::make-reader-on (latin-1 text))))

(test the-syntax-reader-reads-numbers-names-strings-and-booleans
  (is (eql 42 (parse-text "42")))
  (is (eql -7 (parse-text "-7")))
  (is (= 3.5d0 (parse-text "3.5")))
  (is (= -0.25d0 (parse-text "-.25")))
  (is (= 4d0 (parse-text "4.")))
  (is (equal "Name" (parse-text "/Name")))
  (is (equal "A B" (parse-text "/A#20B")))
  (is (equalp (latin-1 "text (with) parens") (parse-text "(text (with) parens)")))
  (is (equalp (octets-of 10 9 65 83) (parse-text "(\\n\\t\\101\\123)")))
  (is (equalp (octets-of #x90 #x1F #xA0) (parse-text "<901FA>")))
  (is (eq :true (parse-text "true")))
  (is (eq :false (parse-text "false")))
  (is (eq :null (parse-text "null"))))

(test the-syntax-reader-reads-arrays-dictionaries-and-references
  (let ((array (parse-text "[1 2.5 /N (s) [3]]")))
    (is (= 5 (length array)))
    (is (equalp (vector 3) (aref array 4))))
  (let ((dictionary (parse-text "<< /A 1 /B [1 2] /C 12 0 R /D << /E /F >> >>")))
    (is (eql 1 (gethash "A" dictionary)))
    (is (equal '(:ref 12 0) (gethash "C" dictionary)))
    (is (equal "F" (gethash "E" (gethash "D" dictionary)))))
  (is (equalp (vector 1 2 3) (parse-text "[1 2 3]")))
  (is (equal '(:op . "re") (parse-text "re"))))

(test the-syntax-reader-skips-comments-and-white-space
  (is (eql 5 (parse-text (format nil "  % a comment~%  5"))))
  (is (eq :eof (parse-text "   "))))

(test the-syntax-reader-refuses-what-is-unbalanced-or-too-deep
  (is (pdf-error-text (lambda () (parse-text "[1 2"))))
  (is (pdf-error-text (lambda () (parse-text "<< /A 1"))))
  (is (pdf-error-text (lambda () (parse-text "(open"))))
  (is (pdf-error-text (lambda () (parse-text (concatenate 'string (make-string 200 :initial-element #\[))))))
  (is (pdf-error-text (lambda () (parse-text "<< 1 2 >>"))))
  (is (pdf-error-text (lambda () (parse-text "<12G>")))))

;;; Filters

(test the-ascii-hex-and-ascii-85-filters-decode-as-the-standard-says
  (is (equalp (latin-1 "Hello") (image::ascii-hex-decode (latin-1 "48 65 6C 6c 6F>"))))
  (is (equalp (octets-of #x90) (image::ascii-hex-decode (latin-1"9>"))))
  (is (equalp (latin-1 "Man is distinguished")
              (image::ascii-85-decode (latin-1 "9jqo^BlbD-BleB1DJ+*+F(f,q~>"))))
  (is (equalp (latin-1 "Hello World!") (image::ascii-85-decode (latin-1 "87cURD]i,\"Ebo80~>"))))
  (is (equalp (octets-of 0 0 0 0) (image::ascii-85-decode (latin-1 "z~>"))))
  (is (pdf-error-text (lambda () (image::ascii-85-decode (latin-1 "9jq{o~>"))))))

(test the-run-length-filter-decodes-literal-and-repeated-runs
  (is (equalp (octets-of 1 2 3 9 9 9 9 7)
              (image::run-length-decode (octets-of 2 1 2 3 253 9 0 7 128 55)))))

(test the-lzw-filter-decodes-the-example-of-the-standard
  (is (equalp (latin-1 "-----A---B")
              (image::lzw-decode (octets-of #x80 #x0B #x60 #x50 #x22 #x0C #x0C #x85 #x01) 1))))

(test a-content-stream-can-be-stored-flate-or-hex-coded
  (let ((plain "1 0 0 rg 10 10 20 30 re f"))
    (dolist (variant (list (list "" (latin-1 plain))
                           (list "/Filter /FlateDecode" (stored-zlib (latin-1 plain)))
                           (list "/Filter /ASCIIHexDecode"
                                 (latin-1 (format nil "~{~2,'0X~}>" (map 'list #'char-code plain))))
                           (list "/Filter [/ASCIIHexDecode /FlateDecode]"
                                 (latin-1 (format nil "~{~2,'0X~}>"
                                                  (coerce (stored-zlib (latin-1 plain)) 'list))))))
      (let ((raster (draw-page (second variant) :content-dictionary (first variant))))
        (is (near-color-p '(255 0 0) (pixel-at raster 20 50)) "~a" (first variant))))))

(test a-png-predictor-undoes-the-up-filter-of-each-row
  (let* ((rows (joined (octets-of 2 10 20 30) (octets-of 2 1 1 1)))
         (decoded (image::undo-png-predictor rows 1 8 3)))
    (is (equalp (octets-of 10 20 30 11 21 31) decoded))))

;;; Pages

(test a-filled-rectangle-covers-the-pixels-it-says-and-no-others
  (let ((raster (draw-page "1 0 0 rg 10 10 20 30 re f")))
    (is (= 100 (image:raster-width raster)))
    (is (= 80 (image:raster-height raster)))
    (is (equal '(255 0 0) (pixel-at raster 10 40)))
    (is (equal '(255 0 0) (pixel-at raster 29 69)))
    (is (equal *white* (pixel-at raster 9 40)))
    (is (equal *white* (pixel-at raster 30 40)))
    (is (equal *white* (pixel-at raster 20 39)))
    (is (equal *white* (pixel-at raster 20 70)))))

(test a-fraction-of-a-pixel-is-blended-by-its-coverage
  (let ((raster (draw-page "0 g 10.5 10 20 30 re f")))
    (is (near-color-p '(128 128 128) (pixel-at raster 10 40) 3))
    (is (equal '(0 0 0) (pixel-at raster 11 40)))))

(test a-stroked-line-has-the-width-and-the-color-it-says
  (let ((raster (draw-page "0 0 1 RG 4 w 10 40 m 90 40 l S")))
    (is (equal '(0 0 255) (pixel-at raster 50 38)))
    (is (equal '(0 0 255) (pixel-at raster 50 41)))
    (is (equal *white* (pixel-at raster 50 36)))
    (is (equal *white* (pixel-at raster 50 43)))
    (is (equal *white* (pixel-at raster 5 40)))))

(test the-ends-and-corners-of-a-line-follow-its-cap-and-join
  (let ((butt (draw-page "0 g 6 w 0 J 20 40 m 80 40 l S"))
        (square (draw-page "0 g 6 w 2 J 20 40 m 80 40 l S"))
        (round (draw-page "0 g 6 w 1 J 20 40 m 80 40 l S")))
    (is (equal *white* (pixel-at butt 18 40)))
    (is (equal '(0 0 0) (pixel-at square 18 40)))
    (is (equal '(0 0 0) (pixel-at round 18 40)))
    (is (equal *white* (pixel-at round 16 36))))
  (let ((miter (draw-page "0 g 8 w 0 j 20 20 m 60 20 l 60 60 l S"))
        (bevel (draw-page "0 g 8 w 2 j 20 20 m 60 20 l 60 60 l S")))
    (is (equal '(0 0 0) (pixel-at miter 63 62)))
    (is (equal *white* (pixel-at bevel 63 62)))))

(test a-dashed-line-alternates-marks-and-gaps
  (let ((raster (draw-page "0 g 4 w [10 10] 0 d 5 40 m 95 40 l S")))
    (is (equal '(0 0 0) (pixel-at raster 10 40)))
    (is (equal *white* (pixel-at raster 20 40)))
    (is (equal '(0 0 0) (pixel-at raster 30 40)))
    (is (equal *white* (pixel-at raster 40 40))))
  (let ((shifted (draw-page "0 g 4 w [10 10] 10 d 5 40 m 95 40 l S")))
    (is (equal *white* (pixel-at shifted 10 40)))
    (is (equal '(0 0 0) (pixel-at shifted 20 40)))))

(test the-even-odd-rule-leaves-a-hole-and-the-non-zero-rule-does-not
  (let ((even-odd (draw-page "0 g 10 10 60 60 re 25 25 30 30 re f*"))
        (non-zero (draw-page "0 g 10 10 60 60 re 25 25 30 30 re f")))
    (is (equal '(0 0 0) (pixel-at even-odd 15 40)))
    (is (equal *white* (pixel-at even-odd 40 40)))
    (is (equal '(0 0 0) (pixel-at non-zero 40 40)))))

(test the-transform-of-cm-moves-scales-and-turns-what-follows
  (let ((raster (draw-page "q 2 0 0 2 10 10 cm 1 0 0 rg 0 0 10 10 re f Q 0 0 1 rg 60 60 10 10 re f")))
    (is (equal '(255 0 0) (pixel-at raster 15 55)))
    (is (equal '(255 0 0) (pixel-at raster 29 69)))
    (is (equal *white* (pixel-at raster 31 60)))
    (is (equal '(0 0 255) (pixel-at raster 65 15))))
  (let ((turned (draw-page "q 0 1 -1 0 50 10 cm 0 g 0 0 20 10 re f Q")))
    (is (equal '(0 0 0) (pixel-at turned 45 60)))
    (is (equal *white* (pixel-at turned 60 65)))))

(test q-and-q-save-and-restore-the-colors-and-the-line-width
  (let ((raster (draw-page "1 0 0 rg q 0 1 0 rg 10 10 10 10 re f Q 30 10 10 10 re f")))
    (is (equal '(0 255 0) (pixel-at raster 15 65)))
    (is (equal '(255 0 0) (pixel-at raster 35 65)))))

(test a-clipping-path-limits-the-painting-that-follows-it
  (let ((raster (draw-page "q 20 20 30 30 re W n 0 g 0 0 100 80 re f Q 1 0 0 rg 70 5 10 10 re f")))
    (is (equal '(0 0 0) (pixel-at raster 30 40)))
    (is (equal *white* (pixel-at raster 10 40)))
    (is (equal *white* (pixel-at raster 60 40)))
    (is (equal '(255 0 0) (pixel-at raster 75 70)))))

(test curves-are-drawn-as-curves
  (let ((raster (draw-page (concatenate 'string
                                        "0 g 50 10 m 77.6 10 100 32.4 100 60 c 100 87.6 77.6 110 50 110 c "
                                        "22.4 110 0 87.6 0 60 c 0 32.4 22.4 10 50 10 c f")
                           :width 100
                           :height 120)))
    (is (equal '(0 0 0) (pixel-at raster 50 60)))
    (is (equal '(0 0 0) (pixel-at raster 50 15)))
    (is (equal *white* (pixel-at raster 2 2)))
    (is (equal *white* (pixel-at raster 97 117)))))

(test colors-in-the-device-spaces-are-converted-to-rgb
  (let ((raster (draw-page (concatenate 'string
                                        "0.5 g 0 0 10 10 re f 0 1 0 rg 10 0 10 10 re f "
                                        "0 0 0 1 k 20 0 10 10 re f 1 0 0 0 k 30 0 10 10 re f"))))
    (is (near-color-p '(128 128 128) (pixel-at raster 5 75)))
    (is (equal '(0 255 0) (pixel-at raster 15 75)))
    (is (equal '(0 0 0) (pixel-at raster 25 75)))
    (is (equal '(0 255 255) (pixel-at raster 35 75)))))

(test colors-set-through-a-named-color-space-follow-that-space
  (let ((raster (draw-page "/Cs1 cs 1 0 0 sc 0 0 10 10 re f /Cs1 CS 0 0 1 SC 4 w 20 5 m 40 5 l S"
                           :resources "/ColorSpace << /Cs1 [/ICCBased 5 0 R] >>"
                           :extra-objects (list (list 5 "/N 3" (octets-of 0))))))
    (is (equal '(255 0 0) (pixel-at raster 5 75)))
    (is (equal '(0 0 255) (pixel-at raster 30 75))))
  (let ((raster (draw-page "/Pal cs 1 sc 0 0 10 10 re f"
                           :resources "/ColorSpace << /Pal [/Indexed /DeviceRGB 1 <FF0000 00FF00>] >>")))
    (is (equal '(0 255 0) (pixel-at raster 5 75)))))

(test a-transparent-graphics-state-blends-with-what-is-under-it
  (let ((raster (draw-page "0 g /Half gs 0 0 20 20 re f"
                           :resources "/ExtGState << /Half << /ca 0.5 >> >>")))
    (is (near-color-p '(128 128 128) (pixel-at raster 10 70) 3))))

(test a-form-xobject-is-drawn-with-its-matrix-and-resources
  (let ((raster (draw-page "q 1 0 0 1 30 30 cm /Fm Do Q"
                           :resources "/XObject << /Fm 5 0 R >>"
                           :extra-objects (list (xobject 5 "Form" "/BBox [0 0 100 100] /Matrix [2 0 0 2 0 0]"
                                                      (latin-1 "0 0 1 rg 0 0 5 5 re f"))))))
    (is (equal '(0 0 255) (pixel-at raster 35 45)))
    (is (equal *white* (pixel-at raster 45 45)))
    (is (equal *white* (pixel-at raster 35 35)))))

(test forms-that-draw-themselves-do-not-run-forever
  (let ((raster (draw-page "0 g 0 0 10 10 re f /Fm Do"
                           :resources "/XObject << /Fm 5 0 R >>"
                           :extra-objects (list (xobject 5
                                                        "Form"
                                                        "/BBox [0 0 100 100] /Resources << /XObject << /Fm 5 0 R >> >>"
                                                        (latin-1 "/Fm Do"))))))
    (is (equal '(0 0 0) (pixel-at raster 5 75)))))

(defun xobject (number
                subtype
                entries
                stream)
  "Return the object list of an XObject of SUBTYPE (\"Form\" or \"Image\") with the dictionary ENTRIES and the bytes
STREAM."
  (list number (format nil "/Type /XObject /Subtype /~a ~a" subtype entries) stream))

(defun image-xobject (number
                      width
                      height
                      color-space
                      bits
                      samples
                      &optional (extra ""))
  "Return the object list of an image of raw samples."
  (list number
        (format nil "/Type /XObject /Subtype /Image /Width ~d /Height ~d /ColorSpace ~a /BitsPerComponent ~d ~a"
                width height color-space bits extra)
        samples))

(test an-image-fills-the-unit-square-the-matrix-gives
  (let ((raster (draw-page "q 40 0 0 40 10 20 cm /Im Do Q"
                           :resources "/XObject << /Im 5 0 R >>"
                           :extra-objects (list (image-xobject 5 2 2 "/DeviceRGB" 8
                                                               (octets-of 255 0 0  0 255 0
                                                                          0 0 255  255 255 0))))))
    (is (near-color-p '(255 0 0) (pixel-at raster 12 22) 2))
    (is (near-color-p '(0 255 0) (pixel-at raster 47 22) 2))
    (is (near-color-p '(0 0 255) (pixel-at raster 12 57) 2))
    (is (near-color-p '(255 255 0) (pixel-at raster 47 57) 2))
    (is (equal *white* (pixel-at raster 5 40)))
    (is (equal *white* (pixel-at raster 60 40)))))

(test images-of-every-color-space-and-depth-are-read
  (flet ((center (&rest keys)
           (pixel-at (apply #'draw-page "q 8 0 0 8 0 0 cm /Im Do Q" :resources "/XObject << /Im 5 0 R >>" keys) 1 78)))
    (is (equal '(128 128 128)
               (center :extra-objects (list (image-xobject 5 1 1 "/DeviceGray" 8 (octets-of 128))))))
    (is (equal '(255 255 255)
               (center :extra-objects (list (image-xobject 5 1 1 "/DeviceGray" 1 (octets-of #x80))))))
    (is (equal '(0 0 0)
               (center :extra-objects (list (image-xobject 5 1 1 "/DeviceGray" 1 (octets-of #x00))))))
    (is (equal '(255 255 255)
               (center :extra-objects (list (image-xobject 5 1 1 "/DeviceGray" 8 (octets-of 0)
                                                           "/Decode [1 0]")))))
    (is (equal '(0 255 255)
               (center :extra-objects (list (image-xobject 5 1 1 "/DeviceCMYK" 8 (octets-of 255 0 0 0))))))
    (is (equal '(0 255 0)
               (center :extra-objects (list (image-xobject 5 1 1 "[/Indexed /DeviceRGB 2 <FF0000 00FF00 0000FF>]" 8
                                                           (octets-of 1))))))
    (is (equal '(0 0 255)
               (center :extra-objects (list (image-xobject 5 1 1 "[/Indexed /DeviceRGB 2 <FF0000 00FF00 0000FF>]" 2
                                                           (octets-of #x80))))))
    (is (equal '(17 34 51)
               (center :extra-objects (list (image-xobject 5 1 1 "/DeviceRGB" 16
                                                           (octets-of #x11 #x00 #x22 #x00 #x33 #x00))))))
    (is (equal '(10 20 30)
               (center :extra-objects (list (image-xobject 5 1 1 "/DeviceRGB" 8
                                                           (stored-zlib (octets-of 10 20 30))
                                                           "/Filter /FlateDecode")))))))

(test a-stencil-mask-paints-the-fill-color-where-the-sample-is-zero
  (let ((raster (draw-page "1 0 0 rg q 20 0 0 10 0 70 cm /Im Do Q"
                           :resources "/XObject << /Im 5 0 R >>"
                           :extra-objects (list (xobject 5
                                                        "Image"
                                                        "/Width 2 /Height 1 /ImageMask true /BitsPerComponent 1"
                                                        (octets-of #x40))))))
    (is (equal '(255 0 0) (pixel-at raster 2 5)))
    (is (equal *white* (pixel-at raster 17 5)))))

(test a-soft-mask-makes-an-image-partly-clear
  (let ((raster (draw-page "q 20 0 0 10 0 70 cm /Im Do Q"
                           :resources "/XObject << /Im 5 0 R >>"
                           :extra-objects (list (xobject 5
                                                        "Image"
                                                        (concatenate 'string
                                                                     "/Width 1 /Height 1 /ColorSpace /DeviceRGB "
                                                                     "/BitsPerComponent 8 /SMask 6 0 R")
                                                        (octets-of 0 0 0))
                                                (xobject 6
                                                        "Image"
                                                        "/Width 1 /Height 1 /ColorSpace /DeviceGray /BitsPerComponent 8"
                                                        (octets-of 128))))))
    (is (near-color-p '(127 127 127) (pixel-at raster 5 5) 3))))

(defun unreadable-picture (filter)
  "Return the entries of the dictionary of a one-bit image that FILTER, a name the program does not read, codes."
  (format nil "/Width 8 /Height 8 /ColorSpace /DeviceGray /BitsPerComponent 1 /Filter ~a" filter))

(test a-picture-kept-in-a-form-the-program-does-not-read-is-named
  (let ((text (pdf-error-text
               (lambda ()
                 (draw-page "q 20 0 0 10 0 70 cm /Im Do Q"
                            :resources "/XObject << /Im 5 0 R >>"
                            :extra-objects (list (xobject 5
                                                         "Image"
                                                         (unreadable-picture "/CCITTFaxDecode")
                                                         (octets-of 1 2 3 4))))))))
    (is (search "fax" text)))
  (let ((raster (draw-page "0 g 0 0 10 10 re f q 20 0 0 10 0 70 cm /Im Do Q"
                           :resources "/XObject << /Im 5 0 R >>"
                           :extra-objects (list (xobject 5
                                                        "Image"
                                                        (unreadable-picture "/JBIG2Decode")
                                                        (octets-of 1 2 3 4))))))
    (is (equal '(0 0 0) (pixel-at raster 5 75)))))

(test a-page-that-draws-nothing-the-program-can-show-says-so
  (is (search "text" (pdf-error-text (lambda () (draw-page "BT /F1 12 Tf 10 10 Td (Hello) Tj ET")))))
  (is (pdf-error-text (lambda () (draw-page "")))))

(test the-rotation-of-a-page-turns-what-is-drawn
  (let ((upright (draw-page "0 g 0 0 20 10 re f"))
        (right (draw-page "0 g 0 0 20 10 re f" :page-entries "/Rotate 90"))
        (upside-down (draw-page "0 g 0 0 20 10 re f" :page-entries "/Rotate 180"))
        (left (draw-page "0 g 0 0 20 10 re f" :page-entries "/Rotate 270")))
    (is (equal '(0 0 0) (pixel-at upright 10 75)))
    (is (= 80 (image:raster-width right)))
    (is (= 100 (image:raster-height right)))
    (is (equal '(0 0 0) (pixel-at right 5 10)))
    (is (equal *white* (pixel-at right 5 90)))
    (is (equal '(0 0 0) (pixel-at upside-down 90 5)))
    (is (= 80 (image:raster-width left)))
    (is (equal '(0 0 0) (pixel-at left 75 90)))))

(test the-box-of-a-page-is-the-crop-box-inside-the-media-box
  (let ((raster (draw-page "0 g 0 0 100 80 re f" :page-entries "/CropBox [10 10 60 50]")))
    (is (= 50 (image:raster-width raster)))
    (is (= 40 (image:raster-height raster)))
    (is (equal '(0 0 0) (pixel-at raster 0 0))))
  (let ((raster (draw-page "1 0 0 rg 10 10 10 10 re f" :page-entries "/MediaBox [10 10 60 50]")))
    (is (equal '(255 0 0) (pixel-at raster 2 37)))))

(test a-page-takes-the-entries-of-its-parents-that-it-lacks
  (let ((raster (image:read-picture (pdf-with-objects
                                     (list 1 "/Type /Catalog /Pages 2 0 R")
                                     (list 2 (concatenate 'string
                                                          "/Type /Pages /Kids [3 0 R] /Count 1 /MediaBox [0 0 60 40] "
                                                          "/Resources << /ExtGState << /Half << /ca 0.5 >> >> >>"))
                                     (list 3 "/Type /Page /Parent 2 0 R /Contents 4 0 R")
                                     (list 4 "" (latin-1 "0 g /Half gs 0 0 10 10 re f")))
                                    :dpi 72)))
    (is (= 60 (image:raster-width raster)))
    (is (near-color-p '(128 128 128) (pixel-at raster 5 35) 3))))

(test the-pages-of-a-tree-come-in-order-and-a-page-can-be-chosen
  (let ((bytes (pdf-with-objects
                (list 1 "/Type /Catalog /Pages 2 0 R")
                (list 2 "/Type /Pages /Kids [3 0 R 4 0 R] /Count 3")
                (list 3 "/Type /Pages /Parent 2 0 R /Kids [5 0 R 6 0 R] /Count 2 /MediaBox [0 0 10 10]")
                (list 4 "/Type /Page /Parent 2 0 R /MediaBox [0 0 30 10] /Contents 7 0 R")
                (list 5 "/Type /Page /Parent 3 0 R /Contents 7 0 R")
                (list 6 "/Type /Page /Parent 3 0 R /MediaBox [0 0 20 10] /Contents 7 0 R")
                (list 7 "" (latin-1 "0 g 0 0 5 5 re f")))))
    (is (= 3 (image:picture-page-count bytes)))
    (is (= 10 (image:raster-width (image:read-picture bytes :page 1 :dpi 72))))
    (is (= 20 (image:raster-width (image:read-picture bytes :page 2 :dpi 72))))
    (is (= 30 (image:raster-width (image:read-picture bytes :page 3 :dpi 72))))
    (is (search "3 pages" (pdf-error-text (lambda () (image:read-picture bytes :page 4)))))
    (is (pdf-error-text (lambda () (image:read-picture bytes :page 0))))))

(test the-resolution-scales-the-page-and-a-very-large-page-is-brought-down-to-fit
  (let ((raster (image:read-picture (page-pdf "0 g 0 0 10 10 re f") :dpi 144)))
    (is (= 200 (image:raster-width raster)))
    (is (= 160 (image:raster-height raster)))
    (is (equal '(0 0 0) (pixel-at raster 10 150))))
  (multiple-value-bind (raster dpi)
      (image:read-picture (page-pdf "0 g 0 0 10 10 re f" :width 14400 :height 14400) :dpi 600)
    (is (<= (* (image:raster-width raster) (image:raster-height raster)) image::+maximum-raster-pixels+))
    (is (< dpi 600))))

(test a-clipping-path-that-is-not-a-box-limits-the-painting-to-its-shape
  (let ((raster (draw-page "q 10 10 m 90 10 l 50 70 l h W n 0 g 0 0 100 80 re f Q")))
    (is (equal '(0 0 0) (pixel-at raster 50 50)) "inside the triangle")
    (is (equal *white* (pixel-at raster 15 20)) "inside its box and outside it")
    (is (equal *white* (pixel-at raster 85 20)))
    (is (equal *white* (pixel-at raster 5 5)))
    (is (equal *white* (pixel-at raster 50 5)) "above its tip")))

(test the-clipping-path-follows-the-even-odd-rule-when-it-is-asked-to
  (let ((nonzero (draw-page "q 10 10 80 60 re 30 20 40 40 re W n 0 g 0 0 100 80 re f Q"))
        (even-odd (draw-page "q 10 10 80 60 re 30 20 40 40 re W* n 0 g 0 0 100 80 re f Q")))
    (is (equal '(0 0 0) (pixel-at nonzero 50 40)) "the hole is inside")
    (is (equal *white* (pixel-at even-odd 50 40)) "the hole is left out")
    (is (equal '(0 0 0) (pixel-at even-odd 20 40)))))

(test clipping-paths-in-force-together-let-through-only-what-all-of-them-do
  (let ((raster (draw-page (concatenate 'string
                                        "q 10 10 m 90 10 l 50 70 l h W n "
                                        "q 0 40 100 40 re W n 0 g 0 0 100 80 re f Q "
                                        "1 0 0 rg 0 0 100 30 re f Q"))))
    (is (equal '(0 0 0) (pixel-at raster 50 20)) "inside the triangle and above the line y = 40")
    (is (equal *white* (pixel-at raster 50 45)) "inside the triangle, below that line and above the red")
    (is (equal '(255 0 0) (pixel-at raster 50 60)) "inside the triangle and below 30")
    (is (equal *white* (pixel-at raster 15 60)) "outside the triangle")
    (is (equal *white* (pixel-at raster 50 5)) "above the tip")))

(test lines-drawn-inside-a-clipping-path-stop-at-its-edge-as-a-hatch-does
  (let ((raster (draw-page (concatenate 'string
                                        "q 20 20 m 80 20 l 50 60 l h W n 0 g 2 w "
                                        "0 30 m 100 30 l S 0 40 m 100 40 l S Q"))))
    (is (equal '(0 0 0) (pixel-at raster 50 49)) "a line inside")
    (is (equal *white* (pixel-at raster 10 49)) "and none outside, on the same line")
    (is (equal *white* (pixel-at raster 95 49)))))

(test a-fill-painted-with-a-pattern-is-left-out-and-does-not-turn-black
  (let ((raster (draw-page "/Pattern cs /P1 scn 10 10 30 30 re f 0 0 1 rg 50 10 20 20 re f"
                           :resources "/Pattern << >>")))
    (is (equal *white* (pixel-at raster 20 50)))
    (is (equal '(0 0 255) (pixel-at raster 60 60)) "the paint that follows is not touched")))

(test a-picture-is-drawn-only-inside-the-clip
  (let ((raster (draw-page "q 10 10 m 90 10 l 50 70 l h W n q 80 0 0 60 10 10 cm /Im Do Q Q"
                           :resources "/XObject << /Im 5 0 R >>"
                           :extra-objects (list (image-xobject 5 1 1 "/DeviceRGB" 8 (octets-of 255 0 0))))))
    (is (near-color-p '(255 0 0) (pixel-at raster 50 50) 2))
    (is (equal *white* (pixel-at raster 15 20)))))

;;; Fixtures made by other programs

(defun mean-difference (raster
                        expected)
  "Return the mean, over the red, green and blue channels, of the difference between a raster and the bytes of EXPECTED,
or :SIZE when the sizes differ."
  (let ((pixels (image:raster-pixels raster))
        (total 0)
        (count 0))
    (if (/= (length pixels) (length expected))
        :size
        (progn (dotimes (index (floor (length pixels) 4))
                 (dotimes (channel 3)
                   (incf total (abs (- (aref pixels (+ channel (* 4 index))) (aref expected (+ channel (* 4 index))))))
                   (incf count)))
               (/ total count)))))

(test a-page-drawn-by-cairo-looks-as-poppler-draws-it
  (multiple-value-bind (raster dpi)
      (image:read-picture (picture-fixture "pdf-vector.pdf") :dpi 72)
    (is (= 72 dpi))
    (let ((difference (mean-difference raster (picture-fixture "pdf-vector.rgba"))))
      (is (numberp difference))
      (is (< difference 1.5) "mean difference ~a" difference))))

(test the-text-of-a-page-is-drawn-from-the-fonts-it-carries
  (let ((raster (image:read-picture (picture-fixture "pdf-text.pdf") :dpi 72)))
    (let ((difference (mean-difference raster (picture-fixture "pdf-text.rgba"))))
      (is (numberp difference))
      (is (< difference 2.5) "mean difference ~a" difference))
    (is (some (lambda (index) (< (aref (image:raster-pixels raster) (* 4 index)) 100))
              (loop for index below (* 160 30) collect index))
        "there is ink where the first line is")))

(test the-numbers-of-a-cff-dictionary-are-read-with-their-signs-and-fractions
  (is (equal '((17 -397 397 -1 0 107 -107 1000 -2000))
             (image::read-cff-dictionary (octets-of 252 33 248 33 138 139 246 32 28 3 232 28 #xF8 #x30 17)
                                         0
                                         15))
      "the encodings of one, two and three bytes, negative and positive")
  (is (= 0.001d0 (image::parse-cff-real "0.001")))
  (is (= -0.0015d0 (image::parse-cff-real "-1.5e-3")))
  (is (= 12d0 (image::parse-cff-real "12")))
  (is (= 0d0 (image::parse-cff-real ""))))

(test the-names-of-glyphs-give-their-unicode-values
  (is (= 65 (image::glyph-name-unicode "A")))
  (is (= 233 (image::glyph-name-unicode "eacute")))
  (is (= 8364 (image::glyph-name-unicode "Euro")))
  (is (= 65 (image::glyph-name-unicode "uni0041")))
  (is (= 97 (image::glyph-name-unicode "a.alt")) "a suffix after a dot is left out")
  (is (null (image::glyph-name-unicode "nosuchglyph"))))

(test the-encodings-give-the-glyph-of-each-code
  (is (equal "A" (svref image::*winansi-encoding* 65)))
  (is (equal "Euro" (svref image::*winansi-encoding* 128)))
  (is (equal "eacute" (svref image::*winansi-encoding* 233)))
  (is (equal "quoteright" (svref image::*standard-encoding* 39)))
  (is (equal "Adieresis" (svref image::*mac-roman-encoding* 128)))
  (is (null (svref image::*standard-encoding* 128))))

(test a-page-with-a-scan-is-drawn-at-the-resolution-of-the-scan-and-the-pixels-come-through-whole
  (dolist (name '("pdf-scan-flate" "pdf-scan-1bit" "pdf-scan-indexed"))
    (multiple-value-bind (raster dpi)
        (image:read-picture (picture-fixture (format nil "~a.pdf" name)))
      (is (= 72 dpi) "~a" name)
      (is (= 40 (image:raster-width raster)) "~a" name)
      (is (= 30 (image:raster-height raster)) "~a" name)
      (is (eql 0 (largest-difference raster (picture-fixture (format nil "~a.rgba" name)))) "~a" name)))
  (multiple-value-bind (raster dpi)
      (image:read-picture (picture-fixture "pdf-scan-dense.pdf"))
    (is (= 144 dpi))
    (is (= 40 (image:raster-width raster)))))

(test a-jpeg-in-a-pdf-decodes-as-the-jpeg-does
  (let ((raster (image:read-picture (picture-fixture "pdf-scan-jpeg.pdf"))))
    (is (eql 0 (largest-difference raster (picture-fixture "pdf-scan-jpeg.rgba"))))))

(test objects-inside-an-object-stream-with-a-cross-reference-stream-are-found
  (let ((raster (image:read-picture (picture-fixture "pdf-objstm.pdf") :dpi 72)))
    (is (= 100 (image:raster-width raster)))
    (is (= 60 (image:raster-height raster)))
    (let ((difference (mean-difference raster (picture-fixture "pdf-objstm.rgba"))))
      (is (numberp difference))
      (is (< difference 1.5) "mean difference ~a" difference))))

;;; Refusals

(test a-pdf-that-is-protected-by-a-password-says-so
  (let ((bytes (page-pdf "0 g 0 0 5 5 re f" :extra-objects (list (list 5 "/Filter /Standard /V 1 /R 2")))))
    (is (not (pdf-error-text (lambda () (image:read-picture bytes)))) "unprotected control")
    (let ((protected (joined bytes (format nil "trailer~%<< /Root 1 0 R /Encrypt 5 0 R >>~%"))))
      (is (search "password" (pdf-error-text (lambda () (image:read-picture protected))))))))

(test a-file-without-pages-or-objects-is-refused
  (is (pdf-error-text (lambda () (image:read-picture (latin-1 "%PDF-1.4 nothing here")))))
  (is (pdf-error-text (lambda () (image:read-picture (pdf-with-objects (list 1 "/Type /Catalog")))))))

(test damaged-pdfs-are-met-only-with-image-errors
  (let ((state (sb-ext:seed-random-state 21))
        (originals (list (page-pdf (concatenate 'string
                                                     "1 0 0 rg 10 10 20 30 re f 0 0 1 RG 4 w [5 3] 0 d 5 5 m 90 70 l S "
                                                     "q 2 0 0 2 0 0 cm /Fm Do Q")
                                   :resources "/XObject << /Fm 5 0 R >>"
                                   :extra-objects (list (xobject 5 "Form" "/BBox [0 0 50 50]"
                                                              (latin-1 "0 g 1 1 10 10 re f"))))
                         (picture-fixture "pdf-scan-flate.pdf")
                         (picture-fixture "pdf-objstm.pdf"))))
    (dolist (original originals)
      (dotimes (trial 150)
        (let ((copy (copy-seq original)))
          (dotimes (each (1+ (random 4 state)))
            (setf (aref copy (random (length copy) state)) (random 256 state)))
          (when (zerop (random 4 state))
            (setf copy (subseq copy 0 (random (length copy) state))))
          (finishes (image-error-p (lambda () (image:read-picture copy :dpi 36)))))))))

(test hostile-numbers-in-a-page-do-not-escape-as-other-errors
  (dolist (content '("1e30 0 0 1e30 0 0 cm 0 g 0 0 1 1 re f"
                     "1000000000 w 0 0 m 100 100 l S"
                     "[0.00000001 0.00000001] 0 d 2 w 0 0 m 100 80 l S"
                     "0 g 99999999999999999999 0 m 5 5 l 5 99999999999999999999 l f"
                     "0 g 10 10 m 20 20 l 30 10 l 10 10 l 1e308 1e308 l f"
                     "q q q q q q q q q q Q Q Q Q Q Q Q Q Q Q Q Q Q Q"))
    (finishes (image-error-p (lambda () (draw-page content))))))
