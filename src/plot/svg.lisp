(in-package #:borba-cad/plot)

;;;; An SVG file: the sheet in millimetres, strokes as polylines and polygons, arrowheads as filled polygons, text
;;;; as text in a monospace font. SVG has y going down, so every y is taken from the height of the sheet.

(defconstant +svg-decimals+ 3
  "Decimals of every coordinate.")

(defconstant +byte-maximum+ 255d0
  "The largest value of a color channel in a hexadecimal color.")

(defun svg-number (value)
  "Return a number as a coordinate of the file.

VALUE: a real number."
  (compact-number value +svg-decimals+))

(defun svg-color (color)
  "Return a color as #rrggbb.

COLOR: a list of red, green and blue, each from 0 to 1."
  (string-downcase (format nil "#~{~2,'0x~}"
                           (mapcar (lambda (channel) (round (* channel +byte-maximum+))) color))))

(defun xml-escaped (string)
  "Return a string with the characters that XML reserves written as entities.

STRING: the characters."
  (with-output-to-string (stream)
    (loop for character across string
          do (case character
               (#\& (write-string "&amp;" stream))
               (#\< (write-string "&lt;" stream))
               (#\> (write-string "&gt;" stream))
               (#\" (write-string "&quot;" stream))
               (t (write-char character stream))))))

(defun svg-points (plan
                   points)
  "Return points as the value of a points attribute.

PLAN: the plot plan.
POINTS: list of (x . y) conses in sheet millimetres."
  (format nil "~{~a~^ ~}"
          (loop for (x . y) in points
                collect (format nil "~a,~a"
                                (svg-number x)
                                (svg-number (- (plot-plan-height plan) y))))))

(defun write-svg-item (stream
                       plan
                       item)
  "Write one plot item as an element.

STREAM: the output stream.
PLAN: the plot plan.
ITEM: a plot stroke, fill or text."
  (etypecase item
    (plot-stroke
     (when (cdr (plot-stroke-points item))
       (format stream "<~:[polyline~;polygon~] points=\"~a\" stroke=\"~a\"/>~%"
               (plot-stroke-closed item)
               (svg-points plan (plot-stroke-points item))
               (svg-color (plot-stroke-color item)))))
    (plot-fill
     (format stream "<polygon points=\"~a\" fill=\"~a\" stroke=\"none\"/>~%"
             (svg-points plan (plot-fill-points item))
             (svg-color (plot-fill-color item))))
    (plot-text
     (let* ((x (svg-number (plot-text-x item)))
            (y (svg-number (- (plot-plan-height plan) (plot-text-y item))))
            (turn (if (/= 0d0 (plot-text-angle item))
                      (format nil " transform=\"rotate(~a ~a ~a)\""
                              (svg-number (- (base:radians->degrees (plot-text-angle item))))
                              x
                              y)
                      "")))
       (format stream "<text x=\"~a\" y=\"~a\" font-size=\"~a\" fill=\"~a\" stroke=\"none\"~a ~
                       xml:space=\"preserve\">~a</text>~%"
               x
               y
               (svg-number (plot-text-height item))
               (svg-color (plot-text-color item))
               turn
               (xml-escaped (plot-text-string item)))))))

(defun render-svg (drawing
                   settings)
  "Return the drawing as the text of an SVG file, and the plot plan that was used. Signals EXPORT-REFUSED when nothing
is visible.

DRAWING: the drawing.
SETTINGS: the plot settings."
  (let* ((plan (plan-plot drawing settings))
         (width (svg-number (plot-plan-width plan)))
         (height (svg-number (plot-plan-height plan))))
    (values (with-output-to-string (stream)
              (format stream "<?xml version=\"1.0\" encoding=\"UTF-8\"?>~%")
              (format stream "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"~amm\" height=\"~amm\" ~
                              viewBox=\"0 0 ~a ~a\">~%"
                      width
                      height
                      width
                      height)
              (format stream "<rect width=\"~a\" height=\"~a\" fill=\"white\"/>~%" width height)
              (format stream "<g fill=\"none\" stroke-linecap=\"round\" stroke-linejoin=\"round\" ~
                              stroke-width=\"~a\" font-family=\"Courier New, Courier, monospace\">~%"
                      (svg-number (plot-settings-line-width (plot-plan-settings plan))))
              (dolist (item (plot-plan-items plan))
                (write-svg-item stream
                                plan
                                item))
              (format stream "</g>~%</svg>~%"))
            plan)))
