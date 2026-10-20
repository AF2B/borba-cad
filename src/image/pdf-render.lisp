(in-package #:borba-cad/image)

;;;; Drawing a page of a PDF into a raster: the content of the page is run as the PDF model says (ISO 32000-1,
;;;; section 8): a stack of graphics states with a current transform, paths built point by point and then filled,
;;;; stroked or used to clip, colors in the device spaces and the spaces that reduce to them, images and form
;;;; XObjects. Text is not drawn, since that needs the fonts; shadings and patterns are not drawn; a clipping path
;;;; limits the drawing to its bounding box. A page made of one scanned picture is drawn at the picture's own
;;;; resolution.

(defconstant +default-render-dpi+ 150
  "The resolution a page without a large picture is drawn at, in dots per inch.")

(defconstant +minimum-render-dpi+ 36
  "The lowest resolution a page is drawn at.")

(defconstant +maximum-render-dpi+ 600
  "The highest resolution a page is drawn at.")

(defconstant +points-per-inch+ 72
  "Points in an inch, the unit of the coordinates of a page.")

(defconstant +form-depth-limit+ 12
  "How deep forms may nest in a page.")

(defconstant +operand-limit+ 4096
  "How many operands may wait for an operator before the program calls the page damaged.")

(defconstant +picture-share-of-page+ 0.5d0
  "The share of the page a picture must cover for the page to be drawn at the picture's resolution.")

(defconstant +curve-tolerance+ 0.1d0
  "How far, in pixels, the lines that stand for a curve may be from the curve.")

(defconstant +maximum-curve-segments+ 150
  "The most lines a curve is cut into.")

(declaim (type (simple-array double-float (6)) +identity-matrix+))

(sb-ext:defglobal +identity-matrix+
  (make-array 6 :element-type 'double-float :initial-contents '(1d0 0d0 0d0 1d0 0d0 0d0))
  "The matrix that changes nothing.")

(defstruct (graphics (:conc-name gs-) (:copier copy-graphics))
  "The graphics state of a page: what the next shape is drawn with."
  (matrix +identity-matrix+ :type (simple-array double-float (6)))
  (fill-color (list 0 0 0))
  (stroke-color (list 0 0 0))
  (fill-space (make-color-space))
  (stroke-space (make-color-space))
  (line-width 1d0 :type double-float)
  (cap 0 :type fixnum)
  (join 0 :type fixnum)
  (miter-limit 10d0 :type double-float)
  (dash '())
  (dash-phase 0d0 :type double-float)
  (fill-alpha 1d0 :type double-float)
  (stroke-alpha 1d0 :type double-float)
  (clip (make-fill-clip) :type fill-clip))

(defstruct (subpath (:conc-name subpath-))
  "A run of connected points of a path, in raster coordinates."
  (points (make-array 16 :element-type 'double-float :adjustable t :fill-pointer 0))
  (closed nil))

(defstruct (render-context (:conc-name ctx-))
  "Everything running a page keeps: the raster, the state, the path being built and the resources in force."
  (document nil)
  (raster nil)
  (graphics (make-graphics) :type graphics)
  (stack '())
  (subpaths '())
  (pending-clip nil)
  (resources nil)
  (images (make-hash-table))
  (painted nil)
  (skipped '())
  (depth 0 :type fixnum)
  (dry-run nil)
  (placements '())
  (page-area 1d0 :type double-float))

;;; Matrices

(defun matrix-of (a b c d e f)
  "Return the matrix with those six numbers.

A: the horizontal scale.
B: the vertical shear.
C: the horizontal shear.
D: the vertical scale.
E: the horizontal shift.
F: the vertical shift."
  (make-array 6
              :element-type 'double-float
              :initial-contents (mapcar (lambda (number) (float number 1d0)) (list a b c d e f))))

(defun matrix-then (first
                    second)
  "Return the matrix that does what FIRST does and then what SECOND does.

FIRST: the matrix applied first.
SECOND: the matrix applied after it."
  (declare (type (simple-array double-float (6)) first second))
  (destructuring-bind (a1 b1 c1 d1 e1 f1)
      (coerce first 'list)
    (destructuring-bind (a2 b2 c2 d2 e2 f2)
        (coerce second 'list)
      (matrix-of (+ (* a1 a2) (* b1 c2))
                 (+ (* a1 b2) (* b1 d2))
                 (+ (* c1 a2) (* d1 c2))
                 (+ (* c1 b2) (* d1 d2))
                 (+ (* e1 a2) (* f1 c2) e2)
                 (+ (* e1 b2) (* f1 d2) f2)))))

(defun matrix-scale (matrix)
  "Return the factor by which a matrix scales lengths, taken as the square root of its determinant.

MATRIX: the matrix."
  (declare (type (simple-array double-float (6)) matrix))
  (sqrt (abs (- (* (aref matrix 0) (aref matrix 3)) (* (aref matrix 1) (aref matrix 2))))))

(defun operand-number (operands
                       index)
  "Return operand INDEX as a double, or zero when it is missing or not a number.

OPERANDS: the list of operands.
INDEX: which one, from zero."
  (let ((value (nth index operands)))
    (if (realp value) (float value 1d0) 0d0)))

;;; Paths

(defconstant +coordinate-limit+ 1d7
  "The farthest from the raster, in pixels, a point of a path is kept; what is beyond is far outside the page anyway.")

(defun limit-coordinate (value)
  "Return a raster coordinate brought within +COORDINATE-LIMIT+, so that damaged or hostile numbers cannot overflow
what follows.

VALUE: the coordinate."
  (declare (type double-float value))
  (max (- +coordinate-limit+) (min +coordinate-limit+ value)))

(defun device-point (context
                     x
                     y)
  "Return the raster position, as two values, of a point in the current user space.

CONTEXT: the render context.
X: x in user space.
Y: y in user space."
  (let ((matrix (gs-matrix (ctx-graphics context))))
    (values (limit-coordinate (+ (* (aref matrix 0) x) (* (aref matrix 2) y) (aref matrix 4)))
            (limit-coordinate (+ (* (aref matrix 1) x) (* (aref matrix 3) y) (aref matrix 5))))))

(defun current-subpath (context)
  "Return the subpath being built, or NIL when the path has none.

CONTEXT: the render context."
  (first (ctx-subpaths context)))

(defun start-subpath (context
                      x
                      y)
  "Begin a new subpath at a point.

CONTEXT: the render context.
X: x in raster coordinates.
Y: y in raster coordinates."
  (let ((subpath (make-subpath)))
    (vector-push-extend x (subpath-points subpath))
    (vector-push-extend y (subpath-points subpath))
    (push subpath (ctx-subpaths context))
    subpath))

(defun extend-subpath (context
                       x
                       y)
  "Add a point to the subpath being built, beginning one there when none is.

CONTEXT: the render context.
X: x in raster coordinates.
Y: y in raster coordinates."
  (let ((subpath (current-subpath context)))
    (if (and subpath (not (subpath-closed subpath)))
        (progn (vector-push-extend x (subpath-points subpath))
               (vector-push-extend y (subpath-points subpath)))
        (start-subpath context
                       x
                       y))))

(defun current-point (context)
  "Return the last point of the path as two values, or NIL when there is none.

CONTEXT: the render context."
  (let ((subpath (current-subpath context)))
    (when subpath
      (let ((points (subpath-points subpath)))
        (values (aref points (- (length points) 2))
                (aref points (- (length points) 1)))))))

(defun add-curve (context
                  x1
                  y1
                  x2
                  y2
                  x3
                  y3)
  "Add a cubic Bezier curve from the current point to the subpath, as lines. All the points are in raster
coordinates.

CONTEXT: the render context.
X1: x of the first control point.
Y1: y of the first control point.
X2: x of the second control point.
Y2: y of the second control point.
X3: x of the end point.
Y3: y of the end point."
  (multiple-value-bind (x0 y0)
      (current-point context)
    (unless x0
      (start-subpath context
                     x1
                     y1)
      (setf x0 x1
            y0 y1))
    (let* ((first-bend (sqrt (+ (expt (+ x0 (* -2 x1) x2) 2) (expt (+ y0 (* -2 y1) y2) 2))))
           (second-bend (sqrt (+ (expt (+ x1 (* -2 x2) x3) 2) (expt (+ y1 (* -2 y2) y3) 2))))
           (segments (max 1 (min +maximum-curve-segments+
                                 (ceiling (sqrt (/ (* 0.75d0 (max first-bend second-bend)) +curve-tolerance+)))))))
      (loop for step from 1 to segments
            do (let* ((time (/ step segments))
                      (rest (- 1 time))
                      (b0 (* rest rest rest))
                      (b1 (* 3 rest rest time))
                      (b2 (* 3 rest time time))
                      (b3 (* time time time)))
                 (extend-subpath context
                                 (+ (* b0 x0) (* b1 x1) (* b2 x2) (* b3 x3))
                                 (+ (* b0 y0) (* b1 y1) (* b2 y2) (* b3 y3))))))))

(defun path-bounds (context)
  "Return the box around the path as four values: lowest x, lowest y, highest x, highest y; or NIL for an empty path.

CONTEXT: the render context."
  (let ((low-x nil) (low-y nil) (high-x nil) (high-y nil))
    (dolist (subpath (ctx-subpaths context))
      (let ((points (subpath-points subpath)))
        (loop for index from 0 below (length points) by 2
              do (let ((x (aref points index))
                       (y (aref points (1+ index))))
                   (setf low-x (if low-x (min low-x x) x)
                         high-x (if high-x (max high-x x) x)
                         low-y (if low-y (min low-y y) y)
                         high-y (if high-y (max high-y y) y))))))
    (when low-x
      (values low-x low-y high-x high-y))))

(defun intersect-clip (context
                       low-x
                       low-y
                       high-x
                       high-y)
  "Narrow the clip of the current state to a box.

CONTEXT: the render context.
LOW-X: the left of the box, in raster coordinates.
LOW-Y: the top.
HIGH-X: the right.
HIGH-Y: the bottom."
  (let* ((graphics (ctx-graphics context))
         (clip (gs-clip graphics)))
    (setf (gs-clip graphics)
          (make-fill-clip :x0 (max (clip-x0 clip) (floor low-x))
                          :y0 (max (clip-y0 clip) (floor low-y))
                          :x1 (min (clip-x1 clip) (ceiling high-x))
                          :y1 (min (clip-y1 clip) (ceiling high-y))))))

(defun fill-subpaths (context
                      even-odd)
  "Fill the path with the fill color of the current state.

CONTEXT: the render context.
EVEN-ODD: true for the even-odd rule."
  (let ((graphics (ctx-graphics context))
        (edges (make-edge-list)))
    (dolist (subpath (ctx-subpaths context))
      (when (>= (length (subpath-points subpath)) 4)
        (add-polygon-edges edges (coerce (subpath-points subpath) '(simple-array double-float (*))))))
    (destructuring-bind (red green blue)
        (gs-fill-color graphics)
      (fill-edges (ctx-raster context)
                  edges
                  even-odd
                  red
                  green
                  blue
                  (gs-fill-alpha graphics)
                  (gs-clip graphics)))))

(defun stroke-subpaths (context)
  "Stroke the path with the stroke color and the line style of the current state.

CONTEXT: the render context."
  (let* ((graphics (ctx-graphics context))
         (scale (matrix-scale (gs-matrix graphics)))
         (half (/ (max +minimum-line-width+ (* (gs-line-width graphics) scale)) 2))
         (pattern (mapcar (lambda (length) (* length scale)) (gs-dash graphics)))
         (edges (make-edge-list)))
    (destructuring-bind (red green blue)
        (gs-stroke-color graphics)
      (flet ((flush ()
               (fill-edges (ctx-raster context)
                           edges
                           nil
                           red
                           green
                           blue
                           (gs-stroke-alpha graphics)
                           (gs-clip graphics))
               (setf edges (make-edge-list))))
        (dolist (subpath (reverse (ctx-subpaths context)))
          (let ((points (coerce (subpath-points subpath) '(simple-array double-float (*)))))
            (if (and pattern (some #'plusp pattern))
                (dolist (piece (dash-polyline points
                                              (subpath-closed subpath)
                                              pattern
                                              (* (gs-dash-phase graphics) scale)))
                  (stroke-polyline edges
                                   piece
                                   nil
                                   half
                                   (gs-cap graphics)
                                   (gs-join graphics)
                                   (gs-miter-limit graphics)))
                (stroke-polyline edges
                                 points
                                 (subpath-closed subpath)
                                 half
                                 (gs-cap graphics)
                                 (gs-join graphics)
                                 (gs-miter-limit graphics))))
          (when (> (edges-count edges) +stroke-flush-edges+)
            (flush)))
        (flush)))))

(defun paint-path (context
                   &key fill
                        even-odd
                        stroke
                        close)
  "Paint the path as an operator of painting says, apply a clip that was asked for, and forget the path.

CONTEXT: the render context.
FILL: true to fill it.
EVEN-ODD: true to fill by the even-odd rule.
STROKE: true to stroke it.
CLOSE: true to close the subpath being built first."
  (when (and close (current-subpath context))
    (setf (subpath-closed (current-subpath context)) t))
  (unless (or (ctx-dry-run context) (null (ctx-subpaths context)))
    (when fill
      (fill-subpaths context even-odd)
      (setf (ctx-painted context) t))
    (when stroke
      (stroke-subpaths context)
      (setf (ctx-painted context) t)))
  (when (ctx-pending-clip context)
    (multiple-value-bind (low-x low-y high-x high-y)
        (path-bounds context)
      (if low-x
          (intersect-clip context
                          low-x
                          low-y
                          high-x
                          high-y)
          (intersect-clip context
                          0
                          0
                          0
                          0)))
    (setf (ctx-pending-clip context) nil))
  (setf (ctx-subpaths context) '()))

;;; Resources

(defun find-resource (context
                      category
                      name)
  "Return the entry called NAME of a category of the current resources (XObject, ColorSpace, ExtGState), or NIL.

CONTEXT: the render context.
CATEGORY: the key of the category.
NAME: the name of the entry."
  (let* ((document (ctx-document context))
         (table (pdf-get document
                         (ctx-resources context)
                         category)))
    (and (hash-table-p table)
         (gethash name table))))

(defun apply-extended-state (context
                             name)
  "Apply the entries of an ExtGState dictionary the program uses: line style and opacity.

CONTEXT: the render context.
NAME: the name of the dictionary in the resources."
  (let* ((document (ctx-document context))
         (dictionary (pdf-resolve document (find-resource context
                                                          "ExtGState"
                                                          name)))
         (graphics (ctx-graphics context)))
    (when (hash-table-p dictionary)
      (let ((width (pdf-get document
                            dictionary
                            "LW"))
            (cap (pdf-get document
                          dictionary
                          "LC"))
            (join (pdf-get document
                           dictionary
                           "LJ"))
            (limit (pdf-get document
                            dictionary
                            "ML"))
            (dash (pdf-get document
                           dictionary
                           "D"))
            (stroke-alpha (pdf-get document
                                   dictionary
                                   "CA"))
            (fill-alpha (pdf-get document
                                 dictionary
                                 "ca")))
        (when (realp width) (setf (gs-line-width graphics) (float width 1d0)))
        (when (integerp cap) (setf (gs-cap graphics) cap))
        (when (integerp join) (setf (gs-join graphics) join))
        (when (realp limit) (setf (gs-miter-limit graphics) (float limit 1d0)))
        (when (and (vectorp dash) (= (length dash) 2))
          (set-dash context
                    (pdf-resolve document (aref dash 0))
                    (pdf-resolve document (aref dash 1))))
        (when (realp stroke-alpha) (setf (gs-stroke-alpha graphics) (float (max 0 (min 1 stroke-alpha)) 1d0)))
        (when (realp fill-alpha) (setf (gs-fill-alpha graphics) (float (max 0 (min 1 fill-alpha)) 1d0)))))))

(defun set-dash (context
                 array
                 phase)
  "Set the dash pattern of the current state.

CONTEXT: the render context.
ARRAY: the vector of dash and gap lengths, or anything else for a solid line.
PHASE: how far into the pattern the line starts."
  (let ((graphics (ctx-graphics context)))
    (setf (gs-dash graphics)
          (if (vectorp array)
              (loop for item across array
                    when (realp item)
                      collect (float (abs item) 1d0))
              '())
          (gs-dash-phase graphics) (if (realp phase) (float phase 1d0) 0d0))))

;;; Images and forms

(defun note-placement (context
                       image)
  "Record that an image is placed on the page, to choose the resolution the page is drawn at.

CONTEXT: the render context.
IMAGE: the dictionary of the image."
  (let* ((document (ctx-document context))
         (matrix (gs-matrix (ctx-graphics context)))
         (width (pdf-get document
                         image
                         "Width")))
    (when (integerp width)
      (push (list width
                  (sqrt (+ (expt (aref matrix 0) 2) (expt (aref matrix 1) 2)))
                  (/ (abs (- (* (aref matrix 0) (aref matrix 3)) (* (aref matrix 1) (aref matrix 2))))
                     (ctx-page-area context)))
            (ctx-placements context)))))

(defun draw-image-object (context
                          object)
  "Draw an image XObject with the current transform, or note why it could not be.

CONTEXT: the render context.
OBJECT: the indirect object of the image."
  (let ((graphics (ctx-graphics context)))
    (if (ctx-dry-run context)
        (note-placement context (object-value object))
        (handler-case
            (let ((image (or (gethash (object-number object) (ctx-images context))
                             (setf (gethash (object-number object) (ctx-images context))
                                   (decode-pdf-image (ctx-document context)
                                                     object
                                                     (ctx-resources context)
                                                     (gs-fill-color graphics))))))
              (draw-image (ctx-raster context)
                          image
                          (gs-matrix graphics)
                          (gs-fill-alpha graphics)
                          (gs-clip graphics))
              (setf (ctx-painted context) t))
          (base:image-error (condition)
            (push condition (ctx-skipped context)))))))

(defun draw-form-object (context
                         object)
  "Run the content of a form XObject inside the current state.

CONTEXT: the render context.
OBJECT: the indirect object of the form."
  (let* ((document (ctx-document context))
         (dictionary (object-value object))
         (matrix (let ((numbers (pdf-get document
                                         dictionary
                                         "Matrix")))
                   (if (and (vectorp numbers) (= (length numbers) 6))
                       (let ((values (map 'list (lambda (item) (pdf-resolve document item)) numbers)))
                         (if (every #'realp values) (apply #'matrix-of values) +identity-matrix+))
                       +identity-matrix+)))
         (bytes (object-stream-bytes document object))
         (saved-graphics (ctx-graphics context))
         (saved-stack (ctx-stack context))
         (saved-resources (ctx-resources context))
         (saved-subpaths (ctx-subpaths context)))
    (when (and bytes (< (ctx-depth context) +form-depth-limit+))
      (setf (ctx-graphics context) (copy-graphics saved-graphics)
            (ctx-stack context) '()
            (ctx-subpaths context) '())
      (setf (gs-matrix (ctx-graphics context)) (matrix-then matrix (gs-matrix saved-graphics)))
      (let ((box (pdf-get document
                          dictionary
                          "BBox"))
            (own-resources (pdf-get document
                                    dictionary
                                    "Resources")))
        (when own-resources
          (setf (ctx-resources context) own-resources))
        (when (and (vectorp box) (= (length box) 4))
          (let ((numbers (map 'list (lambda (item) (pdf-resolve document item)) box)))
            (when (every #'realp numbers)
              (let ((points (list (multiple-value-list (device-point context
                                                                     (first numbers)
                                                                     (second numbers)))
                                  (multiple-value-list (device-point context
                                                                     (third numbers)
                                                                     (second numbers)))
                                  (multiple-value-list (device-point context
                                                                     (third numbers)
                                                                     (fourth numbers)))
                                  (multiple-value-list (device-point context
                                                                     (first numbers)
                                                                     (fourth numbers))))))
                (intersect-clip context
                                (reduce #'min points :key #'first)
                                (reduce #'min points :key #'second)
                                (reduce #'max points :key #'first)
                                (reduce #'max points :key #'second)))))))
      (incf (ctx-depth context))
      (unwind-protect (run-content context bytes)
        (decf (ctx-depth context))
        (setf (ctx-graphics context) saved-graphics
              (ctx-stack context) saved-stack
              (ctx-resources context) saved-resources
              (ctx-subpaths context) saved-subpaths)))))

(defun draw-xobject (context
                     name)
  "Draw the XObject of the resources that has a name: an image or a form.

CONTEXT: the render context.
NAME: the name in the resources."
  (let* ((document (ctx-document context))
         (object (pdf-object-of document (find-resource context
                                                        "XObject"
                                                        name)))
         (subtype (and object (pdf-get document
                                       (object-value object)
                                       "Subtype"))))
    (cond ((equal subtype "Image") (draw-image-object context object))
          ((equal subtype "Form") (draw-form-object context object)))))

;;; Colors

(defun set-color-space (context
                        name
                        stroke)
  "Set the color space of fills or strokes by name, and the color to black.

CONTEXT: the render context.
NAME: the name of a space.
STROKE: true to set the space of strokes, false for fills."
  (let ((space (parse-color-space (ctx-document context)
                                  name
                                  (ctx-resources context)))
        (graphics (ctx-graphics context)))
    (if stroke
        (setf (gs-stroke-space graphics) space
              (gs-stroke-color graphics) (list 0 0 0))
        (setf (gs-fill-space graphics) space
              (gs-fill-color graphics) (list 0 0 0)))))

(defun set-color (context
                  operands
                  stroke
                  &optional space)
  "Set the fill or stroke color from the operands of a color operator.

CONTEXT: the render context.
OPERANDS: the operands.
STROKE: true to set the color of strokes, false for fills.
SPACE: the color space to read them in, or NIL for the current one of that kind."
  (let* ((graphics (ctx-graphics context))
         (space (or space (if stroke (gs-stroke-space graphics) (gs-fill-space graphics)))))
    (unless (eq (space-kind space) :pattern)
      (multiple-value-bind (red green blue)
          (color-from-operands space operands)
        (if stroke
            (setf (gs-stroke-space graphics) space
                  (gs-stroke-color graphics) (list red green blue))
            (setf (gs-fill-space graphics) space
                  (gs-fill-color graphics) (list red green blue)))))))

;;; Operators

(defun skip-inline-image (reader)
  "Move the reader past an inline image, which the program does not draw.

READER: the reader, just after the operator BI."
  (loop
    (let ((token (read-pdf-value reader :references nil)))
      (when (or (eq token :eof) (pdf-operator-p token "ID"))
        (return))))
  (let ((data (reader-data reader))
        (end (reader-end reader)))
    (loop for position from (reader-position reader) below (- end 1)
          do (when (and (= (aref data position) 69)
                        (= (aref data (1+ position)) 73)
                        (or (= position 0) (pdf-white-p (aref data (1- position))))
                        (or (>= (+ position 2) end) (pdf-white-p (aref data (+ position 2)))))
               (setf (reader-position reader) (+ position 2))
               (return-from skip-inline-image nil)))
    (setf (reader-position reader) end)))

(defun run-operator (context
                     name
                     operands)
  "Do what a content operator says.

CONTEXT: the render context.
NAME: the operator.
OPERANDS: its operands, in order."
  (let ((graphics (ctx-graphics context)))
    (flet ((number (index)
             (operand-number operands index)))
      (cond
        ((string= name "q")
         (push graphics (ctx-stack context))
         (setf (ctx-graphics context) (copy-graphics graphics)))
        ((string= name "Q")
         (when (ctx-stack context)
           (setf (ctx-graphics context) (pop (ctx-stack context)))))
        ((string= name "cm")
         (setf (gs-matrix graphics)
               (matrix-then (matrix-of (number 0)
                                       (number 1)
                                       (number 2)
                                       (number 3)
                                       (number 4)
                                       (number 5))
                            (gs-matrix graphics))))
        ((string= name "w") (setf (gs-line-width graphics) (abs (number 0))))
        ((string= name "J") (setf (gs-cap graphics) (max 0 (min 2 (round (number 0))))))
        ((string= name "j") (setf (gs-join graphics) (max 0 (min 2 (round (number 0))))))
        ((string= name "M") (setf (gs-miter-limit graphics) (max 1d0 (number 0))))
        ((string= name "d") (set-dash context
                                      (first operands)
                                      (second operands)))
        ((string= name "gs") (when (stringp (first operands)) (apply-extended-state context (first operands))))
        ((string= name "m")
         (multiple-value-bind (x y)
             (device-point context
                           (number 0)
                           (number 1))
           (start-subpath context
                          x
                          y)))
        ((string= name "l")
         (multiple-value-bind (x y)
             (device-point context
                           (number 0)
                           (number 1))
           (extend-subpath context
                           x
                           y)))
        ((string= name "c")
         (multiple-value-bind (x1 y1) (device-point context
                                                    (number 0)
                                                    (number 1))
           (multiple-value-bind (x2 y2) (device-point context
                                                      (number 2)
                                                      (number 3))
             (multiple-value-bind (x3 y3) (device-point context
                                                        (number 4)
                                                        (number 5))
               (add-curve context
                          x1
                          y1
                          x2
                          y2
                          x3
                          y3)))))
        ((string= name "v")
         (multiple-value-bind (x0 y0) (current-point context)
           (multiple-value-bind (x2 y2) (device-point context
                                                      (number 0)
                                                      (number 1))
             (multiple-value-bind (x3 y3) (device-point context
                                                        (number 2)
                                                        (number 3))
               (add-curve context
                          (or x0 x2)
                          (or y0 y2)
                          x2
                          y2
                          x3
                          y3)))))
        ((string= name "y")
         (multiple-value-bind (x1 y1) (device-point context
                                                    (number 0)
                                                    (number 1))
           (multiple-value-bind (x3 y3) (device-point context
                                                      (number 2)
                                                      (number 3))
             (add-curve context
                        x1
                        y1
                        x3
                        y3
                        x3
                        y3))))
        ((string= name "h")
         (let ((subpath (current-subpath context)))
           (when subpath
             (setf (subpath-closed subpath) t))))
        ((string= name "re")
         (let ((x (number 0)) (y (number 1)) (width (number 2)) (height (number 3)))
           (multiple-value-bind (x0 y0) (device-point context
                                                      x
                                                      y)
             (multiple-value-bind (x1 y1) (device-point context
                                                        (+ x width)
                                                        y)
               (multiple-value-bind (x2 y2) (device-point context
                                                          (+ x width)
                                                          (+ y height))
                 (multiple-value-bind (x3 y3) (device-point context
                                                            x
                                                            (+ y height))
                   (let ((subpath (start-subpath context
                                                 x0
                                                 y0)))
                     (dolist (point (list x1 y1 x2 y2 x3 y3))
                       (vector-push-extend point (subpath-points subpath)))
                     (setf (subpath-closed subpath) t)))))))
         )
        ((string= name "S") (paint-path context :stroke t))
        ((string= name "s") (paint-path context
                                        :stroke
                                        t
                                        :close
                                        t))
        ((or (string= name "f") (string= name "F")) (paint-path context :fill t))
        ((string= name "f*") (paint-path context
                                         :fill
                                         t
                                         :even-odd
                                         t))
        ((string= name "B") (paint-path context
                                        :fill
                                        t
                                        :stroke
                                        t))
        ((string= name "B*") (paint-path context
                                         :fill
                                         t
                                         :even-odd
                                         t
                                         :stroke
                                         t))
        ((string= name "b") (paint-path context
                                        :fill
                                        t
                                        :stroke
                                        t
                                        :close
                                        t))
        ((string= name "b*") (paint-path context
                                         :fill
                                         t
                                         :even-odd
                                         t
                                         :stroke
                                         t
                                         :close
                                         t))
        ((string= name "n") (paint-path context))
        ((or (string= name "W") (string= name "W*")) (setf (ctx-pending-clip context) t))
        ((string= name "g") (set-color context
                                       operands
                                       nil
                                       (make-color-space)))
        ((string= name "G") (set-color context
                                       operands
                                       t
                                       (make-color-space)))
        ((string= name "rg") (set-color context
                                        operands
                                        nil
                                        (space-from-count 3)))
        ((string= name "RG") (set-color context
                                        operands
                                        t
                                        (space-from-count 3)))
        ((string= name "k") (set-color context
                                       operands
                                       nil
                                       (space-from-count 4)))
        ((string= name "K") (set-color context
                                       operands
                                       t
                                       (space-from-count 4)))
        ((string= name "cs") (when (stringp (first operands)) (set-color-space context
                                                                               (first operands)
                                                                               nil)))
        ((string= name "CS") (when (stringp (first operands)) (set-color-space context
                                                                               (first operands)
                                                                               t)))
        ((or (string= name "sc") (string= name "scn")) (set-color context
                                                                  operands
                                                                  nil))
        ((or (string= name "SC") (string= name "SCN")) (set-color context
                                                                  operands
                                                                  t))
        ((string= name "Do") (when (stringp (first operands)) (draw-xobject context (first operands))))))))

(defun run-content (context
                    bytes)
  "Run the content stream held in BYTES in a render context. A stream that goes wrong in the middle is run as far as it
is sound.

CONTEXT: the render context.
BYTES: the decoded content."
  (let ((reader (make-reader-on bytes))
        (operands '())
        (count 0))
    (declare (type fixnum count))
    (loop
      (let ((token (handler-case (read-pdf-value reader :references nil)
                     (base:image-error ()
                       :eof))))
        (cond ((eq token :eof)
               (return))
              ((pdf-operator-p token)
               (if (string= (cdr token) "BI")
                   (skip-inline-image reader)
                   (run-operator context
                                 (cdr token)
                                 (nreverse operands)))
               (setf operands '()
                     count 0))
              (t
               (push token operands)
               (when (> (incf count) +operand-limit+)
                 (setf operands '()
                       count 0))))))))

;;; Pages

(defun page-matrix (document
                    page
                    dpi)
  "Return the matrix that carries the user space of a page to a raster drawn at a resolution, with the page turned as
it is shown.

DOCUMENT: the document.
PAGE: the page.
DPI: dots per inch."
  (destructuring-bind (x0 y0 x1 y1)
      (page-box document page)
    (let* ((unit (let ((value (pdf-get document
                                       (page-dictionary page)
                                       "UserUnit")))
                   (if (and (realp value) (<= 0.01 value 100)) value 1)))
           (factor (* unit (/ dpi +points-per-inch+)))
           (width (* (- x1 x0) factor))
           (height (* (- y1 y0) factor))
           (upright (matrix-of factor
                               0
                               0
                               (- factor)
                               (- (* x0 factor))
                               (* y1 factor))))
      (case (page-rotation document page)
        (90 (matrix-then upright (matrix-of 0
                                            1
                                            -1
                                            0
                                            height
                                            0)))
        (180 (matrix-then upright (matrix-of -1
                                             0
                                             0
                                             -1
                                             width
                                             height)))
        (270 (matrix-then upright (matrix-of 0
                                             -1
                                             1
                                             0
                                             0
                                             width)))
        (t upright)))))

(defun page-context (document
                     page
                     raster
                     dpi
                     dry-run)
  "Return a render context ready to run the content of a page.

DOCUMENT: the document.
PAGE: the page.
RASTER: the raster drawn into (a one-pixel raster for a dry run).
DPI: dots per inch.
DRY-RUN: true to only note the placement of pictures."
  (let ((context (make-render-context :document document
                                      :raster raster
                                      :resources (pdf-get document
                                                          (page-dictionary page)
                                                          "Resources")
                                      :dry-run dry-run)))
    (setf (gs-matrix (ctx-graphics context)) (page-matrix document
                                                          page
                                                          dpi)
          (gs-clip (ctx-graphics context)) (make-fill-clip :x0 0
                                                           :y0 0
                                                           :x1 (raster-width raster)
                                                           :y1 (raster-height raster)))
    context))

(defun page-pixel-size (document
                        page
                        dpi)
  "Return the width and the height, in pixels, of a page drawn at a resolution.

DOCUMENT: the document.
PAGE: the page.
DPI: dots per inch."
  (multiple-value-bind (width height)
      (page-size-points document page)
    (values (max 1 (ceiling (* width dpi) +points-per-inch+))
            (max 1 (ceiling (* height dpi) +points-per-inch+)))))

(defun fit-render-dpi (document
                       page
                       dpi)
  "Return the resolution to draw a page at: DPI, brought into the allowed range and lowered when the page would
have more pixels than the program reads.

DOCUMENT: the document.
PAGE: the page.
DPI: the wanted dots per inch."
  (multiple-value-bind (width height)
      (page-size-points document page)
    (let* ((wanted (max +minimum-render-dpi+ (min +maximum-render-dpi+ dpi)))
           (by-area (* +points-per-inch+ (sqrt (/ +maximum-raster-pixels+ (* width height)))))
           (by-side (/ (* +points-per-inch+ +maximum-raster-side+) (max width height))))
      (min wanted (* 0.98 by-area) (* 0.98 by-side)))))

(defun page-picture-dpi (document
                         page)
  "Return the resolution of the pictures that cover most of a page, or NIL when no picture does, so that a scanned
page is drawn without losing its detail.

DOCUMENT: the document.
PAGE: the page."
  (multiple-value-bind (width height)
      (page-size-points document page)
    (let ((context (page-context document
                                 page
                                 (make-raster 1 1)
                                 +points-per-inch+
                                 t)))
      (setf (ctx-page-area context) (float (* width height) 1d0))
      (handler-case (run-content context (page-content-bytes document page))
        (base:image-error ()
          nil))
      (let ((dpis (loop for (pixels placed-width share) in (ctx-placements context)
                        when (and (>= share +picture-share-of-page+) (> placed-width 0))
                          collect (* +points-per-inch+ (/ pixels placed-width)))))
        (and dpis (reduce #'max dpis))))))

(defun render-pdf-page (document
                        page
                        dpi)
  "Draw a page into a raster and return it with the resolution it was drawn at. Signals IMAGE-ERROR when the page
draws nothing the program can show, or only pictures it cannot read.

DOCUMENT: the document.
PAGE: the page.
DPI: dots per inch, or NIL to choose: the resolution of a scan that covers the page, else 150."
  (let* ((chosen (fit-render-dpi document
                                 page
                                 (or dpi
                                     (page-picture-dpi document page)
                                     +default-render-dpi+))))
    (multiple-value-bind (width height)
        (page-pixel-size document
                         page
                         chosen)
      (let* ((raster (make-raster width height))
             (context (page-context document
                                    page
                                    raster
                                    chosen
                                    nil)))
        (fill (raster-pixels raster) 255)
        (run-content context (page-content-bytes document page))
        (unless (ctx-painted context)
          (if (ctx-skipped context)
              (error (first (last (ctx-skipped context))))
              (image-failure "This PDF page draws no lines or pictures; the program does not draw text.")))
        (values raster chosen)))))
