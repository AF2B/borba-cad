(in-package #:borba-cad/shapes)

;;;; Shapes from SVG files: the outlines of a drawing made in any vector program, read as polylines. What is read is the
;;;; path, the polygon, the polyline, the line, the rectangle, the circle and the ellipse, with the transforms of the
;;;; groups they are in; the colors, the strokes and the text are not looked at. A curve is cut into segments so that it
;;;; stays within a fraction of its own length of the true curve, whatever the units of the file are.

(defconstant +maximum-svg-bytes+ (* 2 1024 1024)
  "Largest SVG file that is read.")

(defconstant +maximum-svg-points+ 60000
  "Most points that the outlines of one SVG file may have once its curves are cut into segments.")

(defconstant +curve-error-fraction+ 0.0015d0
  "How far a flattened curve may stray from the true one, as a fraction of the length of its control polygon.")

(defconstant +arc-step-degrees+ 7.5d0
  "Largest angle, in degrees, that one segment of a flattened arc turns through.")

(defconstant +round-shape-segments+ 72
  "How many segments a circle or an ellipse of a file is made of.")

;;; Numbers

(defun svg-space-p (character)
  "Return true for a character that separates the numbers of a path: white space or a comma.

CHARACTER: a character."
  (or (member character '(#\Space #\Tab #\Newline #\Return #\Page))
      (char= character #\,)))

(defun read-svg-number (text
                        start)
  "Read a number in the form that paths use (a sign, digits, a point, an exponent; no space needed between numbers, so
10-5 is two) from START, after any space. Returns the number as a double-float and the position after it, or NIL when
there is no number there.

TEXT: the text.
START: where to begin."
  (let ((position start)
        (length (length text)))
    (loop while (and (< position length) (svg-space-p (char text position)))
          do (incf position))
    (let ((sign 1)
          (integer 0)
          (fraction 0)
          (scale 1)
          (digits 0)
          (exponent 0))
      (when (and (< position length) (find (char text position) "+-"))
        (when (char= (char text position) #\-)
          (setf sign -1))
        (incf position))
      (loop while (and (< position length) (digit-char-p (char text position)))
            do (setf integer (+ (* 10 integer) (digit-char-p (char text position))))
               (incf digits)
               (incf position))
      (when (and (< position length) (char= (char text position) #\.))
        (incf position)
        (loop while (and (< position length) (digit-char-p (char text position)))
              do (setf fraction (+ (* 10 fraction) (digit-char-p (char text position)))
                       scale (* 10 scale))
                 (incf digits)
                 (incf position)))
      (when (zerop digits)
        (return-from read-svg-number nil))
      (when (and (< position length) (char-equal (char text position) #\e))
        (let ((after (1+ position))
              (exponent-sign 1)
              (exponent-digits 0))
          (when (and (< after length) (find (char text after) "+-"))
            (when (char= (char text after) #\-)
              (setf exponent-sign -1))
            (incf after))
          (loop while (and (< after length) (digit-char-p (char text after)))
                do (setf exponent (+ (* 10 exponent) (digit-char-p (char text after))))
                   (incf exponent-digits)
                   (incf after))
          (when (plusp exponent-digits)
            (setf position after
                  exponent (* exponent-sign (min exponent 300))))))
      (values (float (* sign (+ integer (/ fraction scale)) (expt 10 exponent)) 1d0)
              position))))

(defun read-svg-numbers (text)
  "Return all the numbers in TEXT, separated by spaces or commas, as a list of double-floats.

TEXT: a text such as the points of a polygon."
  (let ((numbers '())
        (position 0))
    (loop
      (multiple-value-bind (number following) (read-svg-number text position)
        (unless number
          (return))
        (push number numbers)
        (setf position following)))
    (nreverse numbers)))

;;; Paths

(defstruct (path-state (:constructor make-path-state ()))
  "Where a path has got to while it is read.

X: x of the current point.
Y: y of the current point.
START-X: x of the start of the subpath.
START-Y: y of the start of the subpath.
CUBIC-X: x of the second control point of the last cubic curve, for a smooth one that follows, or NIL.
CUBIC-Y: its y.
QUADRATIC-X: x of the control point of the last quadratic curve, or NIL.
QUADRATIC-Y: its y.
POINTS: the points of the contour being made, last first.
CONTOURS: the contours that are finished, last first."
  (x 0d0 :type double-float)
  (y 0d0 :type double-float)
  (start-x 0d0 :type double-float)
  (start-y 0d0 :type double-float)
  cubic-x
  cubic-y
  quadratic-x
  quadratic-y
  (points '() :type list)
  (contours '() :type list))

(defun curve-tolerance (&rest points)
  "Return how far a curve through these control points may stray from the polyline, as a fraction of its size.

POINTS: the control points, conses of x and y."
  (let ((total 0d0))
    (loop for (a b) on points
          while b
          do (incf total (point-distance a b)))
    (max 1d-9 (* +curve-error-fraction+ total))))

(defun path-finish-contour (state
                            closed)
  "Put the points of the contour that is being made among the finished ones, when there are enough of them.

STATE: the state of the path.
CLOSED: true when the contour is closed."
  (let ((points (reverse (path-state-points state))))
    (when (and closed
               (rest points)
               (<= (point-distance (first points) (car (last points))) 1d-9))
      (setf points (butlast points)))
    (when (>= (length points) (if closed 3 2))
      (push (make-contour points closed) (path-state-contours state)))
    (setf (path-state-points state) '())))

(defun path-move-to (state
                     x
                     y)
  "Begin a new subpath at a point.

STATE: the state of the path.
X: x of the point.
Y: y of the point."
  (path-finish-contour state nil)
  (setf (path-state-x state) x
        (path-state-y state) y
        (path-state-start-x state) x
        (path-state-start-y state) y
        (path-state-points state) (list (cons x y))))

(defun path-ensure-started (state)
  "Begin a contour at the current point when none is being made, which happens after a subpath is closed.

STATE: the state of the path."
  (when (null (path-state-points state))
    (setf (path-state-points state) (list (cons (path-state-x state) (path-state-y state))))))

(defun path-add-points (state
                        points)
  "Add points to the contour being made and move the current point to the last of them.

STATE: the state of the path.
POINTS: a list of conses of x and y, in order."
  (dolist (point points)
    (push point (path-state-points state)))
  (let ((last (car (last points))))
    (setf (path-state-x state) (car last)
          (path-state-y state) (cdr last))))

(defun path-line-to (state
                     x
                     y)
  "Draw a straight line to a point.

STATE: the state of the path.
X: x of the point.
Y: y of the point."
  (path-ensure-started state)
  (path-add-points state (list (cons x y)))
  (setf (path-state-cubic-x state) nil
        (path-state-quadratic-x state) nil))

(defun path-cubic-to (state
                      x1
                      y1
                      x2
                      y2
                      x
                      y)
  "Draw a cubic curve to a point.

STATE: the state of the path.
X1: x of the first control point.
Y1: y of the first control point.
X2: x of the second control point.
Y2: y of the second control point.
X: x of the end.
Y: y of the end."
  (path-ensure-started state)
  (let ((start (cons (path-state-x state) (path-state-y state)))
        (first (cons x1 y1))
        (second (cons x2 y2))
        (end (cons x y)))
    (path-add-points state (flatten-cubic start
                                          first
                                          second
                                          end
                                          (curve-tolerance start
                                                           first
                                                           second
                                                           end))))
  (setf (path-state-cubic-x state) x2
        (path-state-cubic-y state) y2
        (path-state-quadratic-x state) nil))

(defun path-quadratic-to (state
                          x1
                          y1
                          x
                          y)
  "Draw a quadratic curve to a point.

STATE: the state of the path.
X1: x of the control point.
Y1: y of the control point.
X: x of the end.
Y: y of the end."
  (path-ensure-started state)
  (let ((start (cons (path-state-x state) (path-state-y state)))
        (control (cons x1 y1))
        (end (cons x y)))
    (path-add-points state (flatten-quadratic start
                                              control
                                              end
                                              (curve-tolerance start
                                                               control
                                                               end))))
  (setf (path-state-quadratic-x state) x1
        (path-state-quadratic-y state) y1
        (path-state-cubic-x state) nil))

(defun path-arc-to (state
                    radius-x
                    radius-y
                    rotation
                    large
                    sweep
                    x
                    y)
  "Draw an elliptical arc to a point, as the SVG specification says an arc given by its two ends is worked out.

STATE: the state of the path.
RADIUS-X: the x radius of the ellipse.
RADIUS-Y: its y radius.
ROTATION: the turn of the ellipse, in degrees.
LARGE: true for the longer of the two arcs.
SWEEP: true for the arc that goes counter-clockwise in the file's coordinates.
X: x of the end.
Y: y of the end."
  (path-ensure-started state)
  (let* ((x1 (path-state-x state))
         (y1 (path-state-y state))
         (rx (abs radius-x))
         (ry (abs radius-y)))
    (cond ((and (= x1 x) (= y1 y))
           nil)
          ((or (zerop rx) (zerop ry))
           (path-line-to state
                         x
                         y))
          (t
           (let* ((phi (* (/ pi 180) rotation))
                  (cos-phi (cos phi))
                  (sin-phi (sin phi))
                  (half-dx (/ (- x1 x) 2))
                  (half-dy (/ (- y1 y) 2))
                  (x1p (+ (* cos-phi half-dx) (* sin-phi half-dy)))
                  (y1p (+ (* (- sin-phi) half-dx) (* cos-phi half-dy)))
                  (lambda-value (+ (/ (* x1p x1p) (* rx rx)) (/ (* y1p y1p) (* ry ry)))))
             (when (> lambda-value 1)
               (setf rx (* rx (sqrt lambda-value))
                     ry (* ry (sqrt lambda-value))))
             (let* ((numerator (- (* rx rx ry ry) (* rx rx y1p y1p) (* ry ry x1p x1p)))
                    (denominator (+ (* rx rx y1p y1p) (* ry ry x1p x1p)))
                    (coefficient (* (if (eq (and large t) (and sweep t)) -1 1)
                                    (sqrt (max 0d0 (/ numerator denominator)))))
                    (cxp (* coefficient (/ (* rx y1p) ry)))
                    (cyp (* coefficient (- (/ (* ry x1p) rx))))
                    (cx (+ (- (* cos-phi cxp) (* sin-phi cyp)) (/ (+ x1 x) 2)))
                    (cy (+ (* sin-phi cxp) (* cos-phi cyp) (/ (+ y1 y) 2)))
                    (theta (atan (/ (- y1p cyp) ry) (/ (- x1p cxp) rx)))
                    (end-theta (atan (/ (- (- y1p) cyp) ry) (/ (- (- x1p) cxp) rx)))
                    (delta (- end-theta theta)))
               (cond ((and (not sweep) (plusp delta))
                      (decf delta (* 2 pi)))
                     ((and sweep (minusp delta))
                      (incf delta (* 2 pi))))
               (let ((steps (max 1 (ceiling (abs delta) (* (/ pi 180) +arc-step-degrees+))))
                     (points '()))
                 (loop for step from 1 to steps
                       do (let* ((angle (+ theta (* delta (/ step (float steps 1d0)))))
                                 (ex (* rx (cos angle)))
                                 (ey (* ry (sin angle))))
                            (push (cons (+ cx (- (* cos-phi ex) (* sin-phi ey)))
                                        (+ cy (* sin-phi ex) (* cos-phi ey)))
                                  points)))
                 (setf points (nreverse points))
                 (setf (car (last points)) (cons x y))
                 (path-add-points state points))))))
    (setf (path-state-cubic-x state) nil
          (path-state-quadratic-x state) nil)))

(defun path-close (state)
  "Close the subpath: the contour that is being made joins its first point.

STATE: the state of the path."
  (path-finish-contour state t)
  (setf (path-state-x state) (path-state-start-x state)
        (path-state-y state) (path-state-start-y state)
        (path-state-cubic-x state) nil
        (path-state-quadratic-x state) nil))

(defun svg-arguments-needed (command)
  "Return how many numbers a path command takes.

COMMAND: a letter, in either case."
  (ecase (char-upcase command)
    ((#\M #\L #\T) 2)
    ((#\H #\V) 1)
    ((#\C) 6)
    ((#\S #\Q) 4)
    (#\A 7)
    (#\Z 0)))

(defun read-arc-flag (text
                      start)
  "Read the flag of an arc, a single 0 or 1 that needs no space after it. Returns true or false and the position after
it, or NIL when there is no flag.

TEXT: the text of the path.
START: where to begin."
  (let ((position start)
        (length (length text)))
    (loop while (and (< position length) (svg-space-p (char text position)))
          do (incf position))
    (when (and (< position length) (find (char text position) "01"))
      (values (char= (char text position) #\1)
              (1+ position)))))

(defun apply-path-command (state
                           command
                           arguments)
  "Do what one command of a path says.

STATE: the state of the path.
COMMAND: the letter, in upper case for absolute coordinates and lower case for relative ones.
ARGUMENTS: the numbers it takes; for an arc the two flags are T or NIL."
  (let* ((relative (lower-case-p command))
         (base-x (if relative (path-state-x state) 0d0))
         (base-y (if relative (path-state-y state) 0d0)))
    (flet ((absolute-x (value)
             (+ base-x value))
           (absolute-y (value)
             (+ base-y value)))
      (ecase (char-upcase command)
        (#\M (path-move-to state
                           (absolute-x (first arguments))
                           (absolute-y (second arguments))))
        (#\L (path-line-to state
                           (absolute-x (first arguments))
                           (absolute-y (second arguments))))
        (#\H (path-line-to state
                           (absolute-x (first arguments))
                           (path-state-y state)))
        (#\V (path-line-to state
                           (path-state-x state)
                           (absolute-y (first arguments))))
        (#\C (path-cubic-to state
                            (absolute-x (first arguments))
                            (absolute-y (second arguments))
                            (absolute-x (third arguments))
                            (absolute-y (fourth arguments))
                            (absolute-x (fifth arguments))
                            (absolute-y (sixth arguments))))
        (#\S (let ((reflected-x (if (path-state-cubic-x state)
                                    (- (* 2 (path-state-x state)) (path-state-cubic-x state))
                                    (path-state-x state)))
                   (reflected-y (if (path-state-cubic-x state)
                                    (- (* 2 (path-state-y state)) (path-state-cubic-y state))
                                    (path-state-y state))))
               (path-cubic-to state
                              reflected-x
                              reflected-y
                              (absolute-x (first arguments))
                              (absolute-y (second arguments))
                              (absolute-x (third arguments))
                              (absolute-y (fourth arguments)))))
        (#\Q (path-quadratic-to state
                                (absolute-x (first arguments))
                                (absolute-y (second arguments))
                                (absolute-x (third arguments))
                                (absolute-y (fourth arguments))))
        (#\T (let ((reflected-x (if (path-state-quadratic-x state)
                                    (- (* 2 (path-state-x state)) (path-state-quadratic-x state))
                                    (path-state-x state)))
                   (reflected-y (if (path-state-quadratic-x state)
                                    (- (* 2 (path-state-y state)) (path-state-quadratic-y state))
                                    (path-state-y state))))
               (path-quadratic-to state
                                  reflected-x
                                  reflected-y
                                  (absolute-x (first arguments))
                                  (absolute-y (second arguments)))))
        (#\A (path-arc-to state
                          (first arguments)
                          (second arguments)
                          (third arguments)
                          (fourth arguments)
                          (fifth arguments)
                          (absolute-x (sixth arguments))
                          (absolute-y (seventh arguments))))
        (#\Z (path-close state))))))

(defun read-path-arguments (text
                            start
                            command)
  "Read the numbers of one command of a path. Returns them as a list and the position after them, or NIL when they are
not all there. For an arc the second and third of the flags are read as flags, which need no separator.

TEXT: the text of the path.
START: where the numbers begin.
COMMAND: the letter of the command."
  (let ((position start)
        (arguments '()))
    (dotimes (index (svg-arguments-needed command))
      (if (and (char-equal command #\A) (member index '(3 4)))
          (multiple-value-bind (flag following) (read-arc-flag text position)
            (when (null following)
              (return-from read-path-arguments nil))
            (push flag arguments)
            (setf position following))
          (multiple-value-bind (number following) (read-svg-number text position)
            (when (null number)
              (return-from read-path-arguments nil))
            (push number arguments)
            (setf position following))))
    (values (nreverse arguments) position)))

(defun parse-svg-path (text)
  "Return the contours that the data of an SVG path draws, in the coordinates of the file: open ones for the subpaths
that are not closed and closed ones for those that are, with the curves cut into segments. Signals INVALID-INPUT when
the data is not made of commands and numbers.

TEXT: the value of the d attribute of a path."
  (let ((state (make-path-state))
        (position 0)
        (length (length text))
        (command nil)
        (started nil))
    (loop
      (loop while (and (< position length) (svg-space-p (char text position)))
            do (incf position))
      (when (>= position length)
        (return))
      (let ((character (char text position)))
        (cond ((find character "MmLlHhVvCcSsQqTtAaZz")
               (when (and (not started) (char-not-equal character #\M))
                 (error 'base:invalid-input
                        :text text
                        :reason (i18n:tr "a path must begin with a command such as M")))
               (setf command character
                     started t)
               (incf position)
               (when (char-equal command #\Z)
                 (apply-path-command state
                                     command
                                     '())))
              ((null command)
               (error 'base:invalid-input
                      :text text
                      :reason (i18n:tr "a path must begin with a command such as M")))
              ((char-equal command #\Z)
               (error 'base:invalid-input
                      :text text
                      :reason (i18n:tr "a command of a path has too few numbers or something that is not a number")))
              (t nil)))
      (when (and command (not (char-equal command #\Z)))
        (multiple-value-bind (arguments following) (read-path-arguments text
                                                                        position
                                                                        command)
          (when (null following)
            (error 'base:invalid-input
                   :text text
                   :reason (i18n:tr "a command of a path has too few numbers or something that is not a number")))
          (setf position following)
          (apply-path-command state
                              command
                              arguments)
          (when (char-equal command #\M)
            (setf command (if (char= command #\M) #\L #\l))))))
    (path-finish-contour state nil)
    (reverse (path-state-contours state))))

;;; Transforms

(defun matrix* (a
                b)
  "Return the matrix that does what B does and then what A does, as a list of the six numbers (a b c d e f) of an SVG
matrix.

A: a matrix.
B: a matrix."
  (destructuring-bind (a1 b1 c1 d1 e1 f1) a
    (destructuring-bind (a2 b2 c2 d2 e2 f2) b
      (list (+ (* a1 a2) (* c1 b2))
            (+ (* b1 a2) (* d1 b2))
            (+ (* a1 c2) (* c1 d2))
            (+ (* b1 c2) (* d1 d2))
            (+ (* a1 e2) (* c1 f2) e1)
            (+ (* b1 e2) (* d1 f2) f1)))))

(defparameter *identity-matrix* (list 1d0 0d0 0d0 1d0 0d0 0d0)
  "The matrix that changes nothing.")

(defun transform-matrix (name
                         numbers)
  "Return the matrix of one transform of an SVG element, or NIL when it is not a transform that is known or has the
wrong number of numbers.

NAME: matrix, translate, scale, rotate, skewX or skewY.
NUMBERS: its arguments, double-floats."
  (let ((count (length numbers)))
    (cond ((and (string-equal name "matrix") (= count 6))
           (copy-list numbers))
          ((and (string-equal name "translate") (<= 1 count 2))
           (list 1d0 0d0 0d0 1d0 (first numbers) (if (= count 2) (second numbers) 0d0)))
          ((and (string-equal name "scale") (<= 1 count 2))
           (list (first numbers) 0d0 0d0 (if (= count 2) (second numbers) (first numbers)) 0d0 0d0))
          ((and (string-equal name "rotate") (member count '(1 3)))
           (let* ((angle (* (/ pi 180) (first numbers)))
                  (cosine (cos angle))
                  (sine (sin angle))
                  (turn (list cosine sine (- sine) cosine 0d0 0d0)))
             (if (= count 3)
                 (matrix* (list 1d0 0d0 0d0 1d0 (second numbers) (third numbers))
                          (matrix* turn (list 1d0 0d0 0d0 1d0 (- (second numbers)) (- (third numbers)))))
                 turn)))
          ((and (string-equal name "skewX") (= count 1))
           (list 1d0 0d0 (tan (* (/ pi 180) (first numbers))) 1d0 0d0 0d0))
          ((and (string-equal name "skewY") (= count 1))
           (list 1d0 (tan (* (/ pi 180) (first numbers))) 0d0 1d0 0d0 0d0))
          (t nil))))

(defun parse-transform (text)
  "Return the matrix that the value of a transform attribute stands for: its transforms done one after the other, the
first on the left. A transform that is not known is left out.

TEXT: the value of the attribute, such as translate(10 5) rotate(30)."
  (let ((matrix *identity-matrix*)
        (position 0)
        (length (length text)))
    (loop
      (loop while (and (< position length) (or (svg-space-p (char text position))))
            do (incf position))
      (when (>= position length)
        (return))
      (let* ((open (position #\( text :start position))
             (close (and open (position #\) text :start open))))
        (when (or (null open) (null close))
          (return))
        (let ((matrix-of-this (transform-matrix (string-trim " ," (subseq text position open))
                                                (read-svg-numbers (subseq text (1+ open) close)))))
          (when matrix-of-this
            (setf matrix (matrix* matrix matrix-of-this))))
        (setf position (1+ close))))
    matrix))

(defun transform-point (matrix
                        point)
  "Return a point moved by a matrix.

MATRIX: a list of six numbers.
POINT: a cons of x and y."
  (destructuring-bind (a b c d e f) matrix
    (cons (+ (* a (car point)) (* c (cdr point)) e)
          (+ (* b (car point)) (* d (cdr point)) f))))

;;; The elements of a file

(defun svg-attribute (attributes
                      name)
  "Return the value of an attribute of an element, or NIL.

ATTRIBUTES: an association list of names and values.
NAME: the name."
  (cdr (assoc name attributes :test #'string=)))

(defun svg-number-attribute (attributes
                             name
                             &optional (default 0d0))
  "Return a number that an attribute holds, or a default when there is none.

ATTRIBUTES: an association list of names and values.
NAME: the name.
DEFAULT: the number to give when the attribute is not there or is not a number."
  (let ((value (svg-attribute attributes name)))
    (or (and value (read-svg-number value 0))
        default)))

(defun points-contour (numbers
                       closed)
  "Return a contour made of pairs of numbers, or NIL when there are fewer than two points.

NUMBERS: x and y, point after point.
CLOSED: true for a closed contour."
  (let ((points (loop for (x y) on numbers by #'cddr
                      while y
                      collect (cons x y))))
    (and (>= (length points) (if closed 3 2))
         (make-contour points closed))))

(defun ellipse-contour (center-x
                        center-y
                        radius-x
                        radius-y)
  "Return the closed contour of an ellipse.

CENTER-X: x of the center.
CENTER-Y: y of the center.
RADIUS-X: the radius along x.
RADIUS-Y: the radius along y."
  (make-contour (loop for index from 0 below +round-shape-segments+
                      collect (let ((angle (* 2 pi (/ index (float +round-shape-segments+ 1d0)))))
                                (cons (+ center-x (* radius-x (cos angle)))
                                      (+ center-y (* radius-y (sin angle))))))
                t))

(defun element-contours (name
                         attributes)
  "Return the contours that an element of a file draws, in its own coordinates, or NIL when it is not an element that
draws one of the shapes that are read.

NAME: the name of the element.
ATTRIBUTES: an association list of names and values."
  (cond ((string= name "path")
         (let ((data (svg-attribute attributes "d")))
           (and data (parse-svg-path data))))
        ((string= name "polygon")
         (let ((contour (points-contour (read-svg-numbers (or (svg-attribute attributes "points") "")) t)))
           (and contour (list contour))))
        ((string= name "polyline")
         (let ((contour (points-contour (read-svg-numbers (or (svg-attribute attributes "points") "")) nil)))
           (and contour (list contour))))
        ((string= name "line")
         (list (make-contour (list (cons (svg-number-attribute attributes "x1")
                                         (svg-number-attribute attributes "y1"))
                                   (cons (svg-number-attribute attributes "x2")
                                         (svg-number-attribute attributes "y2")))
                             nil)))
        ((string= name "rect")
         (let* ((x (svg-number-attribute attributes "x"))
                (y (svg-number-attribute attributes "y"))
                (width (svg-number-attribute attributes "width"))
                (height (svg-number-attribute attributes "height")))
           (and (plusp width)
                (plusp height)
                (list (make-contour (list (cons x y)
                                          (cons (+ x width) y)
                                          (cons (+ x width) (+ y height))
                                          (cons x (+ y height)))
                                    t)))))
        ((string= name "circle")
         (let ((radius (svg-number-attribute attributes "r")))
           (and (plusp radius)
                (list (ellipse-contour (svg-number-attribute attributes "cx")
                                       (svg-number-attribute attributes "cy")
                                       radius
                                       radius)))))
        ((string= name "ellipse")
         (let ((radius-x (svg-number-attribute attributes "rx"))
               (radius-y (svg-number-attribute attributes "ry")))
           (and (plusp radius-x)
                (plusp radius-y)
                (list (ellipse-contour (svg-number-attribute attributes "cx")
                                       (svg-number-attribute attributes "cy")
                                       radius-x
                                       radius-y)))))
        (t nil)))

(defparameter *skipped-svg-elements*
  '("defs" "clipPath" "mask" "symbol" "marker" "pattern" "metadata" "title" "desc" "style" "script"
    "linearGradient" "radialGradient" "filter")
  "The elements of an SVG file whose contents are not drawn where they are.")

(defun skip-markup (text
                    position)
  "Return the position after a comment, a processing instruction, a CDATA section or a declaration that begins at
POSITION, or NIL when what is there is an element.

TEXT: the text of the file.
POSITION: where a < is."
  (flet ((after (marker from)
           (let ((found (search marker text :start2 from)))
             (if found
                 (+ found (length marker))
                 (length text)))))
    (cond ((and (<= (+ position 4) (length text)) (string= "<!--" text :start2 position :end2 (+ position 4)))
           (after "-->" (+ position 4)))
          ((and (<= (+ position 9) (length text)) (string= "<![CDATA[" text :start2 position :end2 (+ position 9)))
           (after "]]>" (+ position 9)))
          ((and (< (1+ position) (length text)) (char= #\? (char text (1+ position))))
           (after "?>" (+ position 2)))
          ((and (< (1+ position) (length text)) (char= #\! (char text (1+ position))))
           (let ((depth 0)
                 (index (+ position 2)))
             (loop while (< index (length text))
                   do (let ((character (char text index)))
                        (cond ((char= character #\[) (incf depth))
                              ((char= character #\]) (decf depth))
                              ((and (char= character #\>) (<= depth 0))
                               (return-from skip-markup (1+ index)))))
                      (incf index))
             (length text)))
          (t nil))))

(defun read-tag (text
                 position)
  "Read the element tag that begins at POSITION, where a < is. Returns its kind (:open, :close or :empty), its name, its
attributes as an association list, and the position after the >.

TEXT: the text of the file.
POSITION: where the < is."
  (let* ((length (length text))
         (index (1+ position))
         (kind :open))
    (when (and (< index length) (char= (char text index) #\/))
      (setf kind :close)
      (incf index))
    (let ((name-start index))
      (loop while (and (< index length)
                       (not (member (char text index) '(#\Space #\Tab #\Newline #\Return #\/ #\>))))
            do (incf index))
      (let ((name (subseq text name-start index))
            (attributes '()))
        (loop
          (loop while (and (< index length) (member (char text index) '(#\Space #\Tab #\Newline #\Return)))
                do (incf index))
          (cond ((>= index length)
                 (return))
                ((char= (char text index) #\>)
                 (incf index)
                 (return))
                ((and (char= (char text index) #\/) (< (1+ index) length) (char= (char text (1+ index)) #\>))
                 (when (eq kind :open)
                   (setf kind :empty))
                 (incf index 2)
                 (return))
                (t
                 (let ((attribute-start index))
                   (loop while (and (< index length)
                                    (not (member (char text index) '(#\= #\Space #\Tab #\Newline #\Return #\> #\/))))
                         do (incf index))
                   (let ((attribute-name (subseq text attribute-start index)))
                     (loop while (and (< index length) (member (char text index) '(#\Space #\Tab #\Newline #\Return)))
                           do (incf index))
                     (cond ((and (< index length) (char= (char text index) #\=))
                            (incf index)
                            (loop while (and (< index length)
                                             (member (char text index) '(#\Space #\Tab #\Newline #\Return)))
                                  do (incf index))
                            (when (and (< index length) (find (char text index) "\"'"))
                              (let* ((quote (char text index))
                                     (end (or (position quote text :start (1+ index)) length)))
                                (push (cons attribute-name (subseq text (1+ index) end)) attributes)
                                (setf index (min length (1+ end))))))
                           ((zerop (length attribute-name))
                            (incf index))))))))
        (values kind
                name
                (nreverse attributes)
                index)))))

(defun hidden-element-p (attributes)
  "Return true when an element says it is not to be drawn.

ATTRIBUTES: an association list of names and values."
  (or (equal (svg-attribute attributes "display") "none")
      (equal (svg-attribute attributes "visibility") "hidden")
      (let ((style (svg-attribute attributes "style")))
        (and style
             (or (search "display:none" (remove #\Space style))
                 (search "visibility:hidden" (remove #\Space style)))))))

(defun read-svg-contours (text)
  "Return the contours that an SVG file draws, in the coordinates of the file with y pointing up, as a list. Signals
INVALID-INPUT when the file is too large or draws too many points.

TEXT: the text of the file."
  (when (> (length text) +maximum-svg-bytes+)
    (error 'base:invalid-input
           :text ""
           :reason (i18n:tr "the file is too large to be a shape")))
  (let ((contours '())
        (total 0)
        (position 0)
        (matrices (list *identity-matrix*))
        (skipping 0)
        (length (length text)))
    (loop
      (let ((open (position #\< text :start position)))
        (when (null open)
          (return))
        (let ((after (skip-markup text open)))
          (if after
              (setf position after)
              (multiple-value-bind (kind name attributes following) (read-tag text open)
                (setf position following)
                (cond ((plusp skipping)
                       (case kind
                         (:open (incf skipping))
                         (:close (decf skipping))))
                      ((eq kind :close)
                       (when (rest matrices)
                         (pop matrices)))
                      ((or (member name *skipped-svg-elements* :test #'string=)
                           (hidden-element-p attributes))
                       (when (eq kind :open)
                         (setf skipping 1)))
                      (t
                       (let* ((own (let ((transform (svg-attribute attributes "transform")))
                                     (if transform (parse-transform transform) *identity-matrix*)))
                              (matrix (matrix* (first matrices) own)))
                         (dolist (contour (element-contours name attributes))
                           (incf total (length (contour-points contour)))
                           (when (> total +maximum-svg-points+)
                             (error 'base:invalid-input
                                    :text ""
                                    :reason (i18n:tr "the file draws too many points to be a shape")))
                           (push (make-contour (mapcar (lambda (point)
                                                         (let ((moved (transform-point matrix point)))
                                                           (cons (car moved) (- (cdr moved)))))
                                                       (contour-points contour))
                                               (contour-closed contour))
                                 contours))
                         (when (eq kind :open)
                           (push matrix matrices)))))))))
      (when (>= position length)
        (return)))
    (nreverse contours)))

;;; Shapes of the user

(defun fit-contours-to-box (contours)
  "Return contours scaled so that the larger of their width and their height is 100 units, and moved so that the middle
of the lowest edge is at the origin: the point a shape is placed by.

CONTOURS: a list of contours."
  (let* ((points (loop for contour in contours
                       append (contour-points contour)))
         (low (loop for point in points minimize (cdr point)))
         (high (loop for point in points maximize (cdr point)))
         (left (loop for point in points minimize (car point)))
         (right (loop for point in points maximize (car point)))
         (largest (max (- high low) (- right left))))
    (when (<= largest 0d0)
      (error 'base:invalid-input
             :text ""
             :reason (i18n:tr "the outlines have no size")))
    (let ((factor (/ 100d0 largest))
          (middle (/ (+ left right) 2)))
      (map-points (lambda (point)
                    (cons (* factor (- (car point) middle))
                          (* factor (- (cdr point) low))))
                  contours))))

(defun read-svg-shape (text
                       id
                       name)
  "Return the shape that an SVG file draws, 100 units across its larger side with its base at the origin. Signals
INVALID-INPUT when the file is too large, draws too much, or draws none of the elements that are read.

TEXT: the text of the file.
ID: the id to give the shape.
NAME: the name to give the shape."
  (let ((contours (read-svg-contours text)))
    (when (null contours)
      (error 'base:invalid-input
             :text ""
             :reason (i18n:tr "the file has no path, polygon, line, rectangle, circle or ellipse to make a shape of")))
    (make-shape id
                name
                (fit-contours-to-box contours))))

(defparameter *user-area-title* (i18n:mark "My shapes")
  "The title of the area and of the group that hold the SVG files that are not in a folder of an area.")

(defun svg-file-name-p (name)
  "Return true for the name of an SVG file.

NAME: a file name."
  (and (> (length name) 4)
       (string-equal ".svg" name :start2 (- (length name) 4))))

(defun folder-entries (directory
                       kind)
  "Return the names of the entries of a folder that are of a kind, sorted without regard to case.

DIRECTORY: a pathname.
KIND: :file or :directory."
  (sort (loop for (name entry-kind) in (sbcl:directory-listing directory)
              when (and (eq entry-kind kind)
                        (plusp (length name))
                        (char/= #\. (char name 0)))
                collect name)
        #'string-lessp))

(defun read-shape-files (directory
                         prefix
                         problems)
  "Return the shapes that the SVG files directly in a folder draw, in the order of their names, and the problems that
came up, as a second value: a list of conses of the name of a file and a sentence, added to PROBLEMS.

DIRECTORY: a pathname.
PREFIX: what the ids of the shapes begin with, to tell the folders apart.
PROBLEMS: the problems so far."
  (let ((shapes '()))
    (dolist (name (remove-if-not #'svg-file-name-p (folder-entries directory :file)))
      (let ((path (merge-pathnames name (uiop:ensure-directory-pathname directory))))
        (handler-case
            (push (read-svg-shape (persist:read-file-text path)
                                  (format nil "~a~a" prefix name)
                                  (subseq name 0 (- (length name) 4)))
                  shapes)
          (base:borba-cad-error (condition)
            (push (cons name (remove #\Newline (princ-to-string condition))) problems)))))
    (values (nreverse shapes) problems)))

(defun load-user-areas (directory)
  "Return the areas that the user's folder of shapes holds, and the problems that came up as a second value. A folder
inside it is an area, a folder inside that a group, and the SVG files in a group are its shapes; the SVG files that are
directly in an area make a group of that name, and those directly in the folder make the area My shapes. A file that
cannot be read is left out and listed among the problems, as a cons of its name and a sentence.

DIRECTORY: the folder of shapes, or NIL for none."
  (let ((areas '())
        (problems '()))
    (when (and directory (eq :directory (sbcl:file-kind directory)))
      (let ((base (uiop:ensure-directory-pathname directory)))
        (multiple-value-bind (shapes found) (read-shape-files base
                                                              ""
                                                              problems)
          (setf problems found)
          (when shapes
            (push (make-area "my-shapes"
                             *user-area-title*
                             (list (make-group "my-shapes"
                                               *user-area-title*
                                               shapes)))
                  areas)))
        (dolist (area-name (folder-entries base :directory))
          (let ((area-directory (merge-pathnames (concatenate 'string area-name "/") base))
                (groups '()))
            (multiple-value-bind (shapes found) (read-shape-files area-directory
                                                                  (format nil "~a/" area-name)
                                                                  problems)
              (setf problems found)
              (when shapes
                (push (make-group area-name
                                  area-name
                                  shapes) groups)))
            (dolist (group-name (folder-entries area-directory :directory))
              (multiple-value-bind (shapes found)
                  (read-shape-files (merge-pathnames (concatenate 'string group-name "/") area-directory)
                                    (format nil "~a/~a/" area-name group-name)
                                    problems)
                (setf problems found)
                (when shapes
                  (push (make-group (format nil "~a/~a" area-name group-name)
                                    group-name
                                    shapes) groups))))
            (when groups
              (push (make-area area-name
                               area-name
                               (nreverse groups)) areas))))))
    (values (nreverse areas)
            (nreverse problems))))
