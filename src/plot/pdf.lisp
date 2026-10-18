(in-package #:borba-cad/plot)

;;;; A PDF file written by hand: a catalog, one page, the drawing as drawing operators in the content stream, and one of
;;;; the standard fonts, Courier, whose characters are all 0.6 of their size wide, as the text of the drawing is
;;;; measured everywhere else. Nothing is compressed and no library is needed.

(defconstant +points-per-millimeter+ (/ 72d0 25.4d0)
  "How many PDF points, 1/72 of an inch, there are in a millimetre.")

(defconstant +pdf-decimals+ 3
  "Decimals of every number of the content stream.")

(defconstant +first-high-latin-1-code+ 128
  "The first character code that a PDF string writes as an octal escape.")

(defconstant +last-latin-1-code+ 255
  "The last character code the PDF font has a glyph for; later ones are written as a question mark.")

(defun compact-number (value
                       decimals)
  "Return a number with at most DECIMALS decimals and no zeros at the end of them.

VALUE: a real number.
DECIMALS: the most decimals to keep."
  (let ((text (base:format-decimal value decimals)))
    (if (find #\. text)
        (string-right-trim "." (string-right-trim "0" text))
        text)))

(defun pdf-number (value)
  "Return a number as the content stream writes it.

VALUE: a real number."
  (compact-number value +pdf-decimals+))

(defun point-number (millimeters)
  "Return a length in millimetres as a number of PDF points.

MILLIMETERS: a real number."
  (pdf-number (* millimeters +points-per-millimeter+)))

(defun color-operands (color)
  "Return the three numbers of a color as the operators of the content stream take them.

COLOR: a list of red, green and blue, each from 0 to 1."
  (format nil "~{~a~^ ~}" (mapcar #'pdf-number color)))

(defun pdf-string (string)
  "Return a string as a PDF literal string, in parentheses, with the characters of Latin-1 that are not printable
ASCII as octal escapes and the ones the font has no glyph for as a question mark; and how many were written as one.

STRING: the characters."
  (let ((replaced 0))
    (values (with-output-to-string (stream)
              (write-char #\( stream)
              (loop for character across string
                    for code = (char-code character)
                    do (cond ((find character "()\\")
                              (write-char #\\ stream)
                              (write-char character stream))
                             ((<= 32 code 126) (write-char character stream))
                             ((<= +first-high-latin-1-code+ code +last-latin-1-code+)
                              (format stream "\\~3,'0o" code))
                             (t (write-char #\? stream)
                                (incf replaced))))
              (write-char #\) stream))
            replaced)))

(defun write-stroke (stream
                     stroke)
  "Write the operators of a stroke.

STREAM: the output stream.
STROKE: a plot stroke."
  (let ((points (plot-stroke-points stroke)))
    (when (cdr points)
      (format stream "~a RG~%" (color-operands (plot-stroke-color stroke)))
      (loop for (x . y) in points
            for first = t then nil
            do (format stream "~a ~a ~:[l~;m~]~%"
                       (point-number x)
                       (point-number y)
                       first))
      (write-line (if (plot-stroke-closed stroke) "s" "S") stream))))

(defun write-fill (stream
                   fill)
  "Write the operators of a filled polygon.

STREAM: the output stream.
FILL: a plot fill."
  (format stream "~a rg~%" (color-operands (plot-fill-color fill)))
  (loop for (x . y) in (plot-fill-points fill)
        for first = t then nil
        do (format stream "~a ~a ~:[l~;m~]~%"
                   (point-number x)
                   (point-number y)
                   first))
  (write-line "f" stream))

(defun write-text (stream
                   text)
  "Write the operators of a line of text, and return how many of its characters were written as a question mark.

STREAM: the output stream.
TEXT: a plot text."
  (multiple-value-bind (literal replaced) (pdf-string (plot-text-string text))
    (let ((cosine (cos (plot-text-angle text)))
          (sine (sin (plot-text-angle text))))
      (format stream "BT~%/F1 ~a Tf~%~a rg~%~a ~a ~a ~a ~a ~a Tm~%~a Tj~%ET~%"
              (point-number (plot-text-height text))
              (color-operands (plot-text-color text))
              (pdf-number cosine)
              (pdf-number sine)
              (pdf-number (- sine))
              (pdf-number cosine)
              (point-number (plot-text-x text))
              (point-number (plot-text-y text))
              literal))
    replaced))

(defun pdf-content (plan)
  "Return the content stream of the page, and how many characters of text were written as a question mark because the
font has no glyph for them.

PLAN: the plot plan."
  (let ((replaced 0))
    (values (with-output-to-string (stream)
              (format stream "1 J~%1 j~%~a w~%" (point-number (plot-settings-line-width (plot-plan-settings plan))))
              (dolist (item (plot-plan-items plan))
                (etypecase item
                  (plot-stroke (write-stroke stream item))
                  (plot-fill (write-fill stream item))
                  (plot-text (incf replaced (write-text stream item))))))
            replaced)))

(defun pdf-objects (plan
                    content)
  "Return the objects of the file, as a list of strings: the catalog, the page tree, the page, its content, the font and
the information.

PLAN: the plot plan.
CONTENT: the content stream."
  (list "<< /Type /Catalog /Pages 2 0 R >>"
        "<< /Type /Pages /Kids [3 0 R] /Count 1 >>"
        (format nil "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 ~a ~a] /Contents 4 0 R ~
                     /Resources << /Font << /F1 5 0 R >> >> >>"
                (compact-number (* (plot-plan-width plan) +points-per-millimeter+) 2)
                (compact-number (* (plot-plan-height plan) +points-per-millimeter+) 2))
        (format nil "<< /Length ~d >>~%stream~%~aendstream" (length content) content)
        "<< /Type /Font /Subtype /Type1 /BaseFont /Courier /Encoding /WinAnsiEncoding >>"
        "<< /Producer (Borba CAD) /Creator (Borba CAD) >>"))

(defun pdf-file-text (plan
                      content)
  "Return the whole file as a string whose every character is one byte of it.

PLAN: the plot plan.
CONTENT: the content stream."
  (let* ((header (format nil "%PDF-1.4~%%~{~a~}~%"
                         (mapcar #'code-char '(226 227 207 211))))
         (objects (pdf-objects plan content))
         (offsets '())
         (body (with-output-to-string (stream)
                 (let ((position (length header)))
                   (loop for object in objects
                         for number from 1
                         do (let ((text (format nil "~d 0 obj~%~a~%endobj~%" number object)))
                              (push position offsets)
                              (incf position (length text))
                              (write-string text stream)))))))
    (let* ((xref-position (+ (length header) (length body)))
           (entries (format nil "~{~a~}"
                            (loop for offset in (reverse offsets)
                                  collect (format nil "~10,'0d 00000 n ~%" offset)))))
      (concatenate 'string
                   header
                   body
                   (format nil "xref~%0 ~d~%0000000000 65535 f ~%~atrailer~%~
                                << /Size ~d /Root 1 0 R /Info 6 0 R >>~%startxref~%~d~%%%EOF~%"
                           (1+ (length objects))
                           entries
                           (1+ (length objects))
                           xref-position)))))

(defun render-pdf (drawing
                   settings)
  "Return the drawing as the bytes of a PDF file, and the plot plan that was used, and how many characters of text had
to be written as a question mark because the PDF font cannot show them. Signals EXPORT-REFUSED when nothing is visible.

DRAWING: the drawing.
SETTINGS: the plot settings."
  (let ((plan (plan-plot drawing settings)))
    (multiple-value-bind (content replaced) (pdf-content plan)
      (let ((text (pdf-file-text plan content)))
        (values (map '(vector (unsigned-byte 8)) #'char-code text)
                plan
                replaced)))))
