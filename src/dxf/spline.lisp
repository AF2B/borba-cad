(in-package #:borba-cad/dxf)

;;;; B-spline curves, as DXF files describe them (degree, knots, control points and, for a rational one, weights),
;;;; cut into points. The program has no curve of its own, so a spline is drawn as a polyline through points
;;;; close enough together that the eye cannot tell.

(defconstant +maximum-spline-control-points+ 100000
  "Most control points of a spline that are followed; a spline with more is left out.")

(defconstant +spline-samples-per-span+ 16
  "How many points are taken along each span of a spline, between two of its distinct knots.")

(defconstant +minimum-spline-samples+ 16
  "Fewest segments a spline is cut into.")

(defconstant +maximum-spline-samples+ 4000
  "Most segments a spline is cut into.")

(defun valid-spline-p (degree
                       knots
                       count)
  "Return true when a degree, a vector of knots and a number of control points describe a curve that can be
evaluated: knots that never decrease, as many as the control points and the degree plus one, and a
parameter range that is not a single point.

DEGREE: the degree of the spline.
KNOTS: the vector of knots.
COUNT: how many control points it has."
  (and (integerp degree)
       (>= degree 1)
       (<= count +maximum-spline-control-points+)
       (>= count (1+ degree))
       (= (length knots) (+ count degree 1))
       (loop for index from 1 below (length knots)
             always (<= (aref knots (1- index)) (aref knots index)))
       (< (aref knots degree) (aref knots count))))

(defun spline-span (degree
                    knots
                    count
                    parameter)
  "Return the index of the span of the knots that holds a parameter: the greatest index from DEGREE below COUNT
whose knot is not above PARAMETER, so that the last parameter belongs to the last span.

DEGREE: the degree of the spline.
KNOTS: the vector of knots.
COUNT: how many control points it has.
PARAMETER: where on the curve."
  (let ((low degree)
        (high (1- count)))
    (loop while (< low high)
          do (let ((middle (ceiling (+ low high) 2)))
               (if (<= (aref knots middle) parameter)
                   (setf low middle)
                   (setf high (1- middle)))))
    low))

(defun spline-point (degree
                     knots
                     points
                     weights
                     parameter)
  "Return, as two values, the point of a B-spline at a parameter, by de Boor's algorithm in homogeneous
coordinates, which is also right for a rational spline.

DEGREE: the degree of the spline.
KNOTS: the vector of knots.
POINTS: the vector of control points, each a list of x and y.
WEIGHTS: the vector of weights, one for each control point, or NIL when they are all one.
PARAMETER: where on the curve."
  (let* ((count (length points))
         (span (spline-span degree
                            knots
                            count
                            parameter))
         (xs (make-array (1+ degree)))
         (ys (make-array (1+ degree)))
         (ws (make-array (1+ degree))))
    (loop for index from 0 to degree
          do (let* ((position (+ index (- span degree)))
                    (point (aref points position))
                    (weight (if weights (aref weights position) 1d0)))
               (setf (aref xs index) (* weight (first point))
                     (aref ys index) (* weight (second point))
                     (aref ws index) weight)))
    (loop for round from 1 to degree
          do (loop for index from degree downto round
                   do (let* ((left (aref knots (+ index (- span degree))))
                             (right (aref knots (+ index 1 (- span round))))
                             (alpha (if (= right left)
                                        0d0
                                        (/ (- parameter left) (- right left)))))
                        (setf (aref xs index) (+ (* (- 1d0 alpha) (aref xs (1- index))) (* alpha (aref xs index)))
                              (aref ys index) (+ (* (- 1d0 alpha) (aref ys (1- index))) (* alpha (aref ys index)))
                              (aref ws index) (+ (* (- 1d0 alpha) (aref ws (1- index))) (* alpha (aref ws index)))))))
    (let ((weight (aref ws degree)))
      (if (> (abs weight) 1d-12)
          (values (/ (aref xs degree) weight) (/ (aref ys degree) weight))
          (values (aref xs degree) (aref ys degree))))))

(defun spline-samples (degree
                       knots
                       points
                       weights)
  "Return the points along a B-spline, from its start to its end, as a list of (x y) lists, or NIL when the
numbers do not describe a curve.

DEGREE: the degree of the spline.
KNOTS: the vector of knots.
POINTS: the vector of control points, each a list of x and y.
WEIGHTS: the vector of weights, or NIL."
  (let ((count (length points)))
    (when (and (valid-spline-p degree
                               knots
                               count)
               (or (null weights) (and (= (length weights) count) (every #'plusp weights))))
      (let* ((start (aref knots degree))
             (end (aref knots count))
             (samples (max +minimum-spline-samples+
                           (min +maximum-spline-samples+
                                (* +spline-samples-per-span+ (- count degree))))))
        (loop for index from 0 to samples
              collect (let ((parameter (if (= index samples)
                                           end
                                           (+ start (* (- end start) (/ index (float samples 1d0)))))))
                        (multiple-value-bind (x y) (spline-point degree
                                                                 knots
                                                                 points
                                                                 weights
                                                                 parameter)
                          (list x y))))))))
