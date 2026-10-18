(in-package #:borba-cad/dxf)

;;;; Writing a drawing as a DXF file of version R12 (AC1009), the oldest that is read by everything:
;;;; lines, circles, arcs, texts and polylines are written as what they are; a rectangle, a regular polygon
;;;; and an ellipse are closed polylines; a dimension is its lines, its arrowheads and its text. Text
;;;; is written in ASCII, the characters beyond it as \U+hhhh, which DXF readers turn back.

(defconstant +ellipse-export-vertices+ 128
  "Number of vertices of the polyline that an ellipse is written as.")

(defparameter *line-end* (coerce (list #\Return #\Newline) 'string)
  "How a line of a DXF file ends, as the files of AutoCAD end theirs.")

(defparameter *forbidden-layer-characters* "<>/\\\":;?*|=`"
  "The characters a layer name may not have in a DXF file.")

(defparameter *dxf-version* "AC1009"
  "The version the file says it is: AutoCAD Release 12.")

(defparameter *dxf-code-page* "ANSI_1252"
  "The code page the file says its text is in; the text is ASCII, so any would do.")

(defvar *dxf-stream* nil
  "The stream the writer functions write to.")

(defun group (code
              value)
  "Write one group: its code, then its value.

CODE: the group code.
VALUE: a string, a whole number or a number."
  (format *dxf-stream* "~3d~a~a~a" code *line-end* value *line-end*))

(defun real-text (value)
  "Return a number as the text of a DXF group: decimal, without an exponent, and without the zeros that
end the fraction.

VALUE: a real number."
  (let* ((text (format nil "~,12f" (float value 1d0)))
         (trimmed (string-right-trim "0" text)))
    (if (char= #\. (char trimmed (1- (length trimmed))))
        (concatenate 'string trimmed "0")
        trimmed)))

(defun group-real (code
                   value)
  "Write one group whose value is a number.

CODE: the group code.
VALUE: the number."
  (group code (real-text value)))

(defun group-point (first-code
                    x
                    y)
  "Write the three groups of a point: x, y and a zero z, with codes FIRST-CODE, FIRST-CODE plus 10 and plus 20.

FIRST-CODE: the code of x.
X: x of the point.
Y: y of the point."
  (group-real first-code x)
  (group-real (+ first-code 10) y)
  (group-real (+ first-code 20) 0))

(defun ascii-text (string)
  "Return a text as ASCII, the characters outside it written as \\U+hhhh.

STRING: the text."
  (with-output-to-string (stream)
    (loop for character across string
          do (let ((code (char-code character)))
               (cond ((<= 32 code 126) (write-char character stream))
                     ((<= 127 code #xFFFF) (format stream "\\U+~4,'0X" code))
                     (t (write-char #\? stream)))))))

(defun sanitized-layer-name (name)
  "Return a layer name as a DXF file accepts it: without the characters it forbids.

NAME: the name of a layer."
  (let ((cleaned (substitute-if #\_ (lambda (character) (or (find character *forbidden-layer-characters*)
                                                             (< (char-code character) 32)))
                                name)))
    (if (zerop (length (string-trim " " cleaned))) "0" cleaned)))

(defun layer-names-for-export (drawing)
  "Return a hash table from layer id to the name the layer has in the file: its own, made acceptable, and
different from the name of every other layer when case is not told apart, as DXF readers do.

DRAWING: the drawing."
  (let ((names (make-hash-table))
        (used '()))
    (dolist (layer (dom:layers-in-order drawing))
      (let* ((base (sanitized-layer-name (dom:layer-name layer)))
             (name (if (member base used :test #'string-equal)
                       (loop for number from 2
                             for candidate = (format nil "~a_~d" base number)
                             unless (member candidate used :test #'string-equal)
                               return candidate)
                       base)))
        (push name used)
        (setf (gethash (dom:layer-id layer) names) name)))
    names))

(defun write-header (drawing)
  "Write the header section.

DRAWING: the drawing."
  (group 0 "SECTION")
  (group 2 "HEADER")
  (group 9 "$ACADVER")
  (group 1 *dxf-version*)
  (group 9 "$DWGCODEPAGE")
  (group 3 *dxf-code-page*)
  (group 9 "$INSUNITS")
  (group 70 4)
  (let ((extent nil))
    (dolist (entity (dom:drawing-entities drawing))
      (setf extent (if extent
                       (geo:bounds-union extent (dom:entity-bounds entity))
                       (dom:entity-bounds entity))))
    (when extent
      (group 9 "$EXTMIN")
      (group-point 10
                   (geo:bounds-min-x extent)
                   (geo:bounds-min-y extent))
      (group 9 "$EXTMAX")
      (group-point 10
                   (geo:bounds-max-x extent)
                   (geo:bounds-max-y extent))))
  (group 0 "ENDSEC"))

(defun write-tables (drawing
                     names
                     report)
  "Write the tables section: the one line type, the layers, and the one text style.

DRAWING: the drawing.
NAMES: hash table from layer id to the name in the file.
REPORT: the report, in which approximated colors are noted."
  (group 0 "SECTION")
  (group 2 "TABLES")
  (group 0 "TABLE")
  (group 2 "LTYPE")
  (group 70 1)
  (group 0 "LTYPE")
  (group 2 "CONTINUOUS")
  (group 70 0)
  (group 3 "Solid line")
  (group 72 65)
  (group 73 0)
  (group 40 "0.0")
  (group 0 "ENDTAB")
  (group 0 "TABLE")
  (group 2 "LAYER")
  (group 70 (length (dom:layers-in-order drawing)))
  (dolist (layer (dom:layers-in-order drawing))
    (let* ((color (dom:layer-color layer))
           (index (rgb->aci color))
           (approximated (let ((back (aci->rgb index)))
                           (or (/= (dom:rgb-red color) (dom:rgb-red back))
                               (/= (dom:rgb-green color) (dom:rgb-green back))
                               (/= (dom:rgb-blue color) (dom:rgb-blue back))))))
      (when approximated
        (note report (i18n:tr "Layer colors were approximated by the 256 colors of AutoCAD.")))
      (group 0 "LAYER")
      (group 2 (ascii-text (gethash (dom:layer-id layer) names)))
      (group 70 (if (dom:layer-locked layer) 4 0))
      (group 62 (if (dom:layer-visible layer) index (- index)))
      (group 6 "CONTINUOUS")))
  (group 0 "ENDTAB")
  (group 0 "TABLE")
  (group 2 "STYLE")
  (group 70 1)
  (group 0 "STYLE")
  (group 2 "STANDARD")
  (group 70 0)
  (group 40 "0.0")
  (group 41 "1.0")
  (group 50 "0.0")
  (group 71 0)
  (group 42 "0.2")
  (group 3 "txt")
  (group 4 "")
  (group 0 "ENDTAB")
  (group 0 "ENDSEC"))

(defun radians->degrees* (radians)
  "Return RADIANS in degrees.

RADIANS: an angle in radians."
  (* radians (/ +degrees-per-half-turn+ (float pi 1d0))))

(defun write-polyline (layer
                       coordinates
                       closed)
  "Write a polyline entity: its header, a vertex for each pair of coordinates, and the end mark.

LAYER: the name of the layer.
COORDINATES: a vector of alternating x and y values.
CLOSED: true when the last vertex joins the first."
  (group 0 "POLYLINE")
  (group 8 layer)
  (group 66 1)
  (group-point 10
               0d0
               0d0)
  (group 70 (if closed 1 0))
  (loop for index from 0 below (length coordinates) by 2
        do (group 0 "VERTEX")
           (group 8 layer)
           (group-point 10
                        (aref coordinates index)
                        (aref coordinates (1+ index))))
  (group 0 "SEQEND")
  (group 8 layer))

(defun write-line-entity (layer
                          x1
                          y1
                          x2
                          y2)
  "Write a LINE entity.

LAYER: the name of the layer.
X1: x of the start.
Y1: y of the start.
X2: x of the end.
Y2: y of the end."
  (group 0 "LINE")
  (group 8 layer)
  (group-point 10
               x1
               y1)
  (group-point 11
               x2
               y2))

(defun write-text-entity (layer
                          x
                          y
                          height
                          string
                          angle)
  "Write a TEXT entity.

LAYER: the name of the layer.
X: x of the start of its baseline.
Y: y of the start of its baseline.
HEIGHT: the text height.
STRING: the text.
ANGLE: the direction of the baseline, in radians."
  (group 0 "TEXT")
  (group 8 layer)
  (group-point 10
               x
               y)
  (group-real 40 height)
  (group 1 (ascii-text string))
  (group-real 50 (radians->degrees* angle))
  (group 7 "STANDARD"))

(defun write-dimension (layer
                        dimension)
  "Write a dimension as its parts: two extension lines, the dimension line, two arrowheads as filled
triangles, and its text.

LAYER: the name of the layer.
DIMENSION: a dimension entity."
  (let* ((layout (dom:dimension-layout dimension))
         (extensions (geo:dimension-layout-extension-lines layout))
         (line (geo:dimension-layout-dimension-line layout))
         (arrows (geo:dimension-layout-arrowheads layout)))
    (loop for offset in '(0 4)
          do (write-line-entity layer
                                (aref extensions offset)
                                (aref extensions (+ offset 1))
                                (aref extensions (+ offset 2))
                                (aref extensions (+ offset 3))))
    (write-line-entity layer
                       (aref line 0)
                       (aref line 1)
                       (aref line 2)
                       (aref line 3))
    (loop for offset in '(0 6)
          do (group 0 "SOLID")
             (group 8 layer)
             (group-point 10
                          (aref arrows offset)
                          (aref arrows (+ offset 1)))
             (group-point 11
                          (aref arrows (+ offset 2))
                          (aref arrows (+ offset 3)))
             (group-point 12
                          (aref arrows (+ offset 4))
                          (aref arrows (+ offset 5)))
             (group-point 13
                          (aref arrows (+ offset 4))
                          (aref arrows (+ offset 5))))
    (write-text-entity
                       layer
                       (geo:dimension-layout-text-x layout)
                       (geo:dimension-layout-text-y layout)
                       (dom:dimension-text-height dimension)
                       (dom:dimension-text dimension)
                       (geo:dimension-layout-text-angle layout))))

(defun write-annotation (layer
                         layout
                         text)
  "Write a radius, diameter or angle dimension as its parts: its segments as lines, its arrowheads as filled
triangles, and its text.

LAYER: the name of the layer.
LAYOUT: its annotation layout.
TEXT: the text it shows."
  (let ((segments (geo:annotation-layout-segments layout))
        (arrows (geo:annotation-layout-arrowheads layout)))
    (loop for offset from 0 below (length segments) by 4
          do (write-line-entity layer
                                (aref segments offset)
                                (aref segments (+ offset 1))
                                (aref segments (+ offset 2))
                                (aref segments (+ offset 3))))
    (loop for offset from 0 below (length arrows) by 6
          do (group 0 "SOLID")
             (group 8 layer)
             (group-point 10
                          (aref arrows offset)
                          (aref arrows (+ offset 1)))
             (group-point 11
                          (aref arrows (+ offset 2))
                          (aref arrows (+ offset 3)))
             (group-point 12
                          (aref arrows (+ offset 4))
                          (aref arrows (+ offset 5)))
             (group-point 13
                          (aref arrows (+ offset 4))
                          (aref arrows (+ offset 5))))
    (write-text-entity layer
                       (geo:annotation-layout-text-x layout)
                       (geo:annotation-layout-text-y layout)
                       (geo:annotation-layout-text-height layout)
                       text
                       (geo:annotation-layout-text-angle layout))))

(defun write-hatch (layer
                    hatch)
  "Write a hatch as its parts: the lines of a pattern as lines, and the triangles of a solid one as filled triangles.

LAYER: the name of the layer.
HATCH: a hatch entity."
  (let ((segments (dom:hatch-segments hatch))
        (triangles (dom:hatch-triangles hatch)))
    (loop for offset from 0 below (length segments) by 4
          do (write-line-entity layer
                                (aref segments offset)
                                (aref segments (+ offset 1))
                                (aref segments (+ offset 2))
                                (aref segments (+ offset 3))))
    (loop for offset from 0 below (length triangles) by 6
          do (group 0 "SOLID")
             (group 8 layer)
             (group-point 10
                          (aref triangles offset)
                          (aref triangles (+ offset 1)))
             (group-point 11
                          (aref triangles (+ offset 2))
                          (aref triangles (+ offset 3)))
             (group-point 12
                          (aref triangles (+ offset 4))
                          (aref triangles (+ offset 5)))
             (group-point 13
                          (aref triangles (+ offset 4))
                          (aref triangles (+ offset 5))))))

(defun write-entity-as-dxf (entity
                            layer
                            report)
  "Write one entity.

ENTITY: the entity.
LAYER: the name of its layer in the file.
REPORT: the report, in which what changes shape is noted."
  (count-up (dxf-report-imported report) (dom:entity-kind entity))
  (etypecase entity
    (dom:line-entity
     (write-line-entity layer
                        (dom:line-x1 entity)
                        (dom:line-y1 entity)
                        (dom:line-x2 entity)
                        (dom:line-y2 entity)))
    (dom:rectangle-entity
     (let* ((left (dom:rectangle-min-x entity))
            (bottom (dom:rectangle-min-y entity))
            (right (+ left (dom:rectangle-width entity)))
            (top (+ bottom (dom:rectangle-height entity))))
       (write-polyline layer
                       (coerce (list left bottom right bottom right top left top) '(simple-array double-float (*)))
                       t)))
    (dom:circle-entity
     (group 0 "CIRCLE")
     (group 8 layer)
     (group-point 10
                  (dom:circle-center-x entity)
                  (dom:circle-center-y entity))
     (group-real 40 (dom:circle-radius entity)))
    (dom:polyline-entity
     (write-polyline layer
                     (dom:polyline-coordinates entity)
                     (dom:polyline-closed entity)))
    (dom:text-entity
     (write-text-entity layer
                        (dom:text-x entity)
                        (dom:text-y entity)
                        (dom:text-height entity)
                        (dom:text-string entity)
                        (dom:text-angle entity)))
    (dom:dimension-entity
     (note report (i18n:tr "Dimensions were written as their lines, arrowheads and text."))
     (write-dimension layer entity))
    (dom:hatch-entity
     (note report (i18n:tr "Hatches were written as lines or filled triangles."))
     (write-hatch layer entity))
    (dom:insert-entity
     (note report (i18n:tr "Blocks were written as the entities they stand for."))
     (dolist (placed (dom:insert-entities entity))
       (write-entity-as-dxf placed
                            layer
                            report)))
    (dom:radial-dimension-entity
     (note report (i18n:tr "Dimensions were written as their lines, arrowheads and text."))
     (write-annotation layer
                       (dom:radial-dimension-layout entity)
                       (dom:radial-dimension-text entity)))
    (dom:angular-dimension-entity
     (note report (i18n:tr "Dimensions were written as their lines, arrowheads and text."))
     (write-annotation layer
                       (dom:angular-dimension-layout entity)
                       (dom:angular-dimension-text entity)))
    (dom:arc-entity
     (group 0 "ARC")
     (group 8 layer)
     (group-point 10
                  (dom:arc-center-x entity)
                  (dom:arc-center-y entity))
     (group-real 40 (dom:arc-radius entity))
     (group-real 50 (radians->degrees* (dom:arc-start-angle entity)))
     (group-real 51 (radians->degrees* (dom:arc-end-angle entity))))
    (dom:ellipse-entity
     (note report (i18n:tr "Ellipses were written as polylines."))
     (write-polyline layer
                     (geo:ellipse-points (dom:ellipse-center-x entity)
                                         (dom:ellipse-center-y entity)
                                         (dom:ellipse-radius-x entity)
                                         (dom:ellipse-radius-y entity)
                                         (dom:ellipse-rotation entity)
                                         +ellipse-export-vertices+)
                     t))
    (dom:polygon-entity
     (note report (i18n:tr "Regular polygons were written as polylines."))
     (write-polyline layer
                     (dom:polygon-vertices entity)
                     t))))

(defun export-drawing-text (drawing)
  "Return the DXF text of DRAWING, and a DXF-REPORT of how many entities of each kind were written and
what changed shape in the writing.

DRAWING: the drawing."
  (let* ((report (make-dxf-report))
         (names (layer-names-for-export drawing))
         (text (with-output-to-string (*dxf-stream*)
                 (write-header drawing)
                 (write-tables drawing
                               names
                               report)
                 (group 0 "SECTION")
                 (group 2 "BLOCKS")
                 (group 0 "ENDSEC")
                 (group 0 "SECTION")
                 (group 2 "ENTITIES")
                 (dolist (entity (dom:drawing-entities drawing))
                   (write-entity-as-dxf entity
                                        (ascii-text (gethash (dom:entity-layer-id entity) names))
                                        report))
                 (group 0 "ENDSEC")
                 (group 0 "EOF"))))
    (values text report)))

(defun export-summary (report)
  "Return what an export did as a sentence or two for the user.

REPORT: the DXF-REPORT of the export."
  (with-output-to-string (stream)
    (format stream "~a"
            (i18n:trf "~d ~:*~[entities~;entity~:;entities~] exported." (report-imported-total report)))
    (dolist (sentence (report-notes report))
      (format stream " ~a" sentence))))
