(in-package #:borba-cad/domain)

;;;; Rotating, mirroring and scaling entities. Each takes an entity and answers a validated entity with the same
;;;; id and layer. A rectangle that is turned to a slant (or mirrored across a slanted line) cannot stay a
;;;; rectangle, which has sides along the axes, and becomes the closed polyline of its four corners.

(defconstant +right-angle-tolerance+ 1d-9
  "How far, in radians, an angle may be from a multiple of a right angle and still count as one.")

(defun rotate-xy (x
                  y
                  center-x
                  center-y
                  angle)
  "Return, as two values, the point (X, Y) turned about the center by an angle.

X: x of the point.
Y: y of the point.
CENTER-X: x of the center of the turn.
CENTER-Y: y of the center.
ANGLE: radians, counter-clockwise."
  (let ((dx (- x center-x))
        (dy (- y center-y))
        (cosine (cos angle))
        (sine (sin angle)))
    (values (+ center-x (- (* dx cosine) (* dy sine)))
            (+ center-y (+ (* dx sine) (* dy cosine))))))

(defun mirror-xy (x
                  y
                  line-x1
                  line-y1
                  line-x2
                  line-y2)
  "Return, as two values, the reflection of the point (X, Y) in the line through two points.

X: x of the point.
Y: y of the point.
LINE-X1: x of the first point of the line.
LINE-Y1: y of the first point.
LINE-X2: x of the second point.
LINE-Y2: y of the second point."
  (let* ((dx (- line-x2 line-x1))
         (dy (- line-y2 line-y1))
         (length-squared (+ (* dx dx) (* dy dy)))
         (along (/ (+ (* (- x line-x1) dx) (* (- y line-y1) dy)) length-squared))
         (foot-x (+ line-x1 (* along dx)))
         (foot-y (+ line-y1 (* along dy))))
    (values (- (* 2 foot-x) x)
            (- (* 2 foot-y) y))))

(defun scale-xy (x
                 y
                 center-x
                 center-y
                 factor)
  "Return, as two values, the point (X, Y) moved away from the center, or toward it, by a factor.

X: x of the point.
Y: y of the point.
CENTER-X: x of the center of the scaling.
CENTER-Y: y of the center.
FACTOR: the number its distance from the center is multiplied by."
  (values (+ center-x (* factor (- x center-x)))
          (+ center-y (* factor (- y center-y)))))

(defun mirror-angle (angle
                     line-x1
                     line-y1
                     line-x2
                     line-y2)
  "Return the direction ANGLE has after a reflection in the line through two points.

ANGLE: a direction in radians.
LINE-X1: x of the first point of the line.
LINE-Y1: y of the first point.
LINE-X2: x of the second point.
LINE-Y2: y of the second point."
  (- (* 2 (atan (- line-y2 line-y1) (- line-x2 line-x1))) angle))

(defgeneric entity-rotated (entity center-x center-y angle)
  (:documentation "Return ENTITY turned about a point.

ENTITY: an entity.
CENTER-X: x of the center of the turn.
CENTER-Y: y of the center.
ANGLE: radians, counter-clockwise."))

(defgeneric entity-mirrored (entity line-x1 line-y1 line-x2 line-y2)
  (:documentation "Return ENTITY reflected in the line through two points. A text is not turned back to front:
only its insertion point is reflected.

ENTITY: an entity.
LINE-X1: x of the first point of the line.
LINE-Y1: y of the first point.
LINE-X2: x of the second point.
LINE-Y2: y of the second point."))

(defgeneric entity-scaled (entity center-x center-y factor)
  (:documentation "Return ENTITY scaled about a point: every length of it, a text height included, is multiplied by
FACTOR.

ENTITY: an entity.
CENTER-X: x of the center of the scaling.
CENTER-Y: y of the center.
FACTOR: a positive number."))

;;; Lines, circles, arcs, ellipses, polygons

(defmethod entity-rotated ((line line-entity) center-x center-y angle)
  (multiple-value-bind (x1 y1) (rotate-xy (line-x1 line)
                                          (line-y1 line)
                                          center-x
                                          center-y
                                          angle)
    (multiple-value-bind (x2 y2) (rotate-xy (line-x2 line)
                                            (line-y2 line)
                                            center-x
                                            center-y
                                            angle)
      (update-line line
                   :x1
                   x1
                   :y1
                   y1
                   :x2
                   x2
                   :y2
                   y2))))

(defmethod entity-mirrored ((line line-entity) line-x1 line-y1 line-x2 line-y2)
  (multiple-value-bind (x1 y1) (mirror-xy (line-x1 line)
                                          (line-y1 line)
                                          line-x1
                                          line-y1
                                          line-x2
                                          line-y2)
    (multiple-value-bind (x2 y2) (mirror-xy (line-x2 line)
                                            (line-y2 line)
                                            line-x1
                                            line-y1
                                            line-x2
                                            line-y2)
      (update-line line
                   :x1
                   x1
                   :y1
                   y1
                   :x2
                   x2
                   :y2
                   y2))))

(defmethod entity-scaled ((line line-entity) center-x center-y factor)
  (multiple-value-bind (x1 y1) (scale-xy (line-x1 line)
                                         (line-y1 line)
                                         center-x
                                         center-y
                                         factor)
    (multiple-value-bind (x2 y2) (scale-xy (line-x2 line)
                                           (line-y2 line)
                                           center-x
                                           center-y
                                           factor)
      (update-line line
                   :x1
                   x1
                   :y1
                   y1
                   :x2
                   x2
                   :y2
                   y2))))

(defmethod entity-rotated ((circle circle-entity) center-x center-y angle)
  (multiple-value-bind (x y) (rotate-xy (circle-center-x circle)
                                        (circle-center-y circle)
                                        center-x
                                        center-y
                                        angle)
    (update-circle circle
                   :center-x
                   x
                   :center-y
                   y)))

(defmethod entity-mirrored ((circle circle-entity) line-x1 line-y1 line-x2 line-y2)
  (multiple-value-bind (x y) (mirror-xy (circle-center-x circle)
                                        (circle-center-y circle)
                                        line-x1
                                        line-y1
                                        line-x2
                                        line-y2)
    (update-circle circle
                   :center-x
                   x
                   :center-y
                   y)))

(defmethod entity-scaled ((circle circle-entity) center-x center-y factor)
  (multiple-value-bind (x y) (scale-xy (circle-center-x circle)
                                       (circle-center-y circle)
                                       center-x
                                       center-y
                                       factor)
    (update-circle circle
                   :center-x
                   x
                   :center-y
                   y
                   :radius
                   (* factor (circle-radius circle)))))

(defmethod entity-rotated ((arc arc-entity) center-x center-y angle)
  (multiple-value-bind (x y) (rotate-xy (arc-center-x arc)
                                        (arc-center-y arc)
                                        center-x
                                        center-y
                                        angle)
    (update-arc arc
                :center-x x
                :center-y y
                :start-angle (+ (arc-start-angle arc) angle)
                :end-angle (+ (arc-end-angle arc) angle))))

(defmethod entity-mirrored ((arc arc-entity) line-x1 line-y1 line-x2 line-y2)
  (multiple-value-bind (x y) (mirror-xy (arc-center-x arc)
                                        (arc-center-y arc)
                                        line-x1
                                        line-y1
                                        line-x2
                                        line-y2)
    (update-arc arc
                :center-x x
                :center-y y
                :start-angle (mirror-angle (arc-end-angle arc)
                                           line-x1
                                           line-y1
                                           line-x2
                                           line-y2)
                :end-angle (mirror-angle (arc-start-angle arc)
                                         line-x1
                                         line-y1
                                         line-x2
                                         line-y2))))

(defmethod entity-scaled ((arc arc-entity) center-x center-y factor)
  (multiple-value-bind (x y) (scale-xy (arc-center-x arc)
                                       (arc-center-y arc)
                                       center-x
                                       center-y
                                       factor)
    (update-arc arc
                :center-x
                x
                :center-y
                y
                :radius
                (* factor (arc-radius arc)))))

(defmethod entity-rotated ((ellipse ellipse-entity) center-x center-y angle)
  (multiple-value-bind (x y) (rotate-xy (ellipse-center-x ellipse)
                                        (ellipse-center-y ellipse)
                                        center-x
                                        center-y
                                        angle)
    (update-ellipse ellipse
                    :center-x
                    x
                    :center-y
                    y
                    :rotation
                    (+ (ellipse-rotation ellipse) angle))))

(defmethod entity-mirrored ((ellipse ellipse-entity) line-x1 line-y1 line-x2 line-y2)
  (multiple-value-bind (x y) (mirror-xy (ellipse-center-x ellipse)
                                        (ellipse-center-y ellipse)
                                        line-x1
                                        line-y1
                                        line-x2
                                        line-y2)
    (update-ellipse ellipse
                    :center-x x
                    :center-y y
                    :rotation (mirror-angle (ellipse-rotation ellipse)
                                            line-x1
                                            line-y1
                                            line-x2
                                            line-y2))))

(defmethod entity-scaled ((ellipse ellipse-entity) center-x center-y factor)
  (multiple-value-bind (x y) (scale-xy (ellipse-center-x ellipse)
                                       (ellipse-center-y ellipse)
                                       center-x
                                       center-y
                                       factor)
    (update-ellipse ellipse
                    :center-x x
                    :center-y y
                    :radius-x (* factor (ellipse-radius-x ellipse))
                    :radius-y (* factor (ellipse-radius-y ellipse)))))

(defmethod entity-rotated ((polygon polygon-entity) center-x center-y angle)
  (multiple-value-bind (x y) (rotate-xy (polygon-center-x polygon)
                                        (polygon-center-y polygon)
                                        center-x
                                        center-y
                                        angle)
    (update-polygon polygon
                    :center-x
                    x
                    :center-y
                    y
                    :rotation
                    (+ (polygon-rotation polygon) angle))))

(defmethod entity-mirrored ((polygon polygon-entity) line-x1 line-y1 line-x2 line-y2)
  (multiple-value-bind (x y) (mirror-xy (polygon-center-x polygon)
                                        (polygon-center-y polygon)
                                        line-x1
                                        line-y1
                                        line-x2
                                        line-y2)
    (update-polygon polygon
                    :center-x x
                    :center-y y
                    :rotation (mirror-angle (polygon-rotation polygon)
                                            line-x1
                                            line-y1
                                            line-x2
                                            line-y2))))

(defmethod entity-scaled ((polygon polygon-entity) center-x center-y factor)
  (multiple-value-bind (x y) (scale-xy (polygon-center-x polygon)
                                       (polygon-center-y polygon)
                                       center-x
                                       center-y
                                       factor)
    (update-polygon polygon
                    :center-x
                    x
                    :center-y
                    y
                    :radius
                    (* factor (polygon-radius polygon)))))

;;; Polylines and rectangles

(defun map-coordinates (coordinates
                        function)
  "Return a new vector of coordinates with every point of COORDINATES replaced by what FUNCTION gives for it.

COORDINATES: a vector of alternating x and y values.
FUNCTION: function of x and y that returns two values."
  (let ((result (make-array (length coordinates) :element-type 'double-float)))
    (loop for index from 0 below (length coordinates) by 2
          do (multiple-value-bind (x y) (funcall function (aref coordinates index) (aref coordinates (1+ index)))
               (setf (aref result index) x
                     (aref result (1+ index)) y)))
    result))

(defmethod entity-rotated ((polyline polyline-entity) center-x center-y angle)
  (update-polyline polyline
                   :coordinates (map-coordinates (polyline-coordinates polyline)
                                                 (lambda (x y) (rotate-xy x
                                                                          y
                                                                          center-x
                                                                          center-y
                                                                          angle)))))

(defmethod entity-mirrored ((polyline polyline-entity) line-x1 line-y1 line-x2 line-y2)
  (update-polyline polyline
                   :coordinates (map-coordinates (polyline-coordinates polyline)
                                                 (lambda (x y)
                                                   (mirror-xy x
                                                              y
                                                              line-x1
                                                              line-y1
                                                              line-x2
                                                              line-y2)))))

(defmethod entity-scaled ((polyline polyline-entity) center-x center-y factor)
  (update-polyline polyline
                   :coordinates (map-coordinates (polyline-coordinates polyline)
                                                 (lambda (x y) (scale-xy x
                                                                         y
                                                                         center-x
                                                                         center-y
                                                                         factor)))))

(defun rectangle-corners (rectangle)
  "Return the four corners of a rectangle as a vector of alternating x and y values, counter-clockwise from the
lower left.

RECTANGLE: a rectangle entity."
  (let* ((x1 (rectangle-min-x rectangle))
         (y1 (rectangle-min-y rectangle))
         (x2 (+ x1 (rectangle-width rectangle)))
         (y2 (+ y1 (rectangle-height rectangle))))
    (make-array 8 :element-type 'double-float :initial-contents (list x1 y1 x2 y1 x2 y2 x1 y2))))

(defun axis-aligned-corners-p (corners)
  "Return true when four corners, in order around a shape, are those of a rectangle with sides along the axes.

CORNERS: a vector of alternating x and y values."
  (flet ((same (a b)
           (< (abs (- a b)) 1d-9)))
    (or (and (same (aref corners 0) (aref corners 2))
             (same (aref corners 3) (aref corners 5))
             (same (aref corners 4) (aref corners 6))
             (same (aref corners 7) (aref corners 1)))
        (and (same (aref corners 1) (aref corners 3))
             (same (aref corners 2) (aref corners 4))
             (same (aref corners 5) (aref corners 7))
             (same (aref corners 6) (aref corners 0))))))

(defun rectangle-from-transformed-corners (rectangle
                                           corners)
  "Return RECTANGLE as the corners say it has become: a rectangle with sides along the axes when they are the
corners of one, and otherwise the closed polyline through them.

RECTANGLE: the entity that was transformed, for its id and layer.
CORNERS: a vector of alternating x and y values."
  (if (axis-aligned-corners-p corners)
      (let ((xs (list (aref corners 0) (aref corners 2) (aref corners 4) (aref corners 6)))
            (ys (list (aref corners 1) (aref corners 3) (aref corners 5) (aref corners 7))))
        (update-rectangle rectangle
                          :min-x (reduce #'min xs)
                          :min-y (reduce #'min ys)
                          :width (- (reduce #'max xs) (reduce #'min xs))
                          :height (- (reduce #'max ys) (reduce #'min ys))))
      (make-polyline :id (entity-id rectangle)
                     :layer-id (entity-layer-id rectangle)
                     :coordinates corners
                     :closed t)))

(defmethod entity-rotated ((rectangle rectangle-entity) center-x center-y angle)
  (rectangle-from-transformed-corners rectangle
                                      (map-coordinates (rectangle-corners rectangle)
                                                       (lambda (x y) (rotate-xy x
                                                                                y
                                                                                center-x
                                                                                center-y
                                                                                angle)))))

(defmethod entity-mirrored ((rectangle rectangle-entity) line-x1 line-y1 line-x2 line-y2)
  (rectangle-from-transformed-corners rectangle
                                      (map-coordinates (rectangle-corners rectangle)
                                                       (lambda (x y)
                                                         (mirror-xy x
                                                                    y
                                                                    line-x1
                                                                    line-y1
                                                                    line-x2
                                                                    line-y2)))))

(defmethod entity-scaled ((rectangle rectangle-entity) center-x center-y factor)
  (rectangle-from-transformed-corners rectangle
                                      (map-coordinates (rectangle-corners rectangle)
                                                       (lambda (x y) (scale-xy x
                                                                               y
                                                                               center-x
                                                                               center-y
                                                                               factor)))))

;;; Texts and dimensions

(defmethod entity-rotated ((text text-entity) center-x center-y angle)
  (multiple-value-bind (x y) (rotate-xy (text-x text)
                                        (text-y text)
                                        center-x
                                        center-y
                                        angle)
    (update-text text
                 :x
                 x
                 :y
                 y
                 :angle
                 (+ (text-angle text) angle))))

(defmethod entity-mirrored ((text text-entity) line-x1 line-y1 line-x2 line-y2)
  (multiple-value-bind (x y) (mirror-xy (text-x text)
                                        (text-y text)
                                        line-x1
                                        line-y1
                                        line-x2
                                        line-y2)
    (update-text text
                 :x
                 x
                 :y
                 y)))

(defmethod entity-scaled ((text text-entity) center-x center-y factor)
  (multiple-value-bind (x y) (scale-xy (text-x text)
                                       (text-y text)
                                       center-x
                                       center-y
                                       factor)
    (update-text text
                 :x
                 x
                 :y
                 y
                 :height
                 (* factor (text-height text)))))

(defmethod entity-rotated ((dimension dimension-entity) center-x center-y angle)
  (multiple-value-bind (x1 y1) (rotate-xy (dimension-x1 dimension)
                                          (dimension-y1 dimension)
                                          center-x
                                          center-y
                                          angle)
    (multiple-value-bind (x2 y2) (rotate-xy (dimension-x2 dimension)
                                            (dimension-y2 dimension)
                                            center-x
                                            center-y
                                            angle)
      (update-dimension dimension
                        :x1
                        x1
                        :y1
                        y1
                        :x2
                        x2
                        :y2
                        y2))))

(defmethod entity-mirrored ((dimension dimension-entity) line-x1 line-y1 line-x2 line-y2)
  (multiple-value-bind (x1 y1) (mirror-xy (dimension-x1 dimension)
                                          (dimension-y1 dimension)
                                          line-x1
                                          line-y1
                                          line-x2
                                          line-y2)
    (multiple-value-bind (x2 y2) (mirror-xy (dimension-x2 dimension)
                                            (dimension-y2 dimension)
                                            line-x1
                                            line-y1
                                            line-x2
                                            line-y2)
      (update-dimension dimension
                        :x1
                        x1
                        :y1
                        y1
                        :x2
                        x2
                        :y2
                        y2
                        :offset
                        (- (dimension-offset dimension))))))

(defmethod entity-scaled ((dimension dimension-entity) center-x center-y factor)
  (multiple-value-bind (x1 y1) (scale-xy (dimension-x1 dimension)
                                         (dimension-y1 dimension)
                                         center-x
                                         center-y
                                         factor)
    (multiple-value-bind (x2 y2) (scale-xy (dimension-x2 dimension)
                                           (dimension-y2 dimension)
                                           center-x
                                           center-y
                                           factor)
      (update-dimension dimension
                        :x1 x1
                        :y1 y1
                        :x2 x2
                        :y2 y2
                        :offset (* factor (dimension-offset dimension))
                        :text-height (* factor (dimension-text-height dimension))))))

(defmethod entity-rotated ((dimension radial-dimension-entity) center-x center-y angle)
  (multiple-value-bind (x y) (rotate-xy (radial-dimension-center-x dimension)
                                        (radial-dimension-center-y dimension)
                                        center-x
                                        center-y
                                        angle)
    (update-radial-dimension dimension
                             :center-x x
                             :center-y y
                             :angle (+ (radial-dimension-angle dimension) angle))))

(defmethod entity-mirrored ((dimension radial-dimension-entity) line-x1 line-y1 line-x2 line-y2)
  (multiple-value-bind (x y) (mirror-xy (radial-dimension-center-x dimension)
                                        (radial-dimension-center-y dimension)
                                        line-x1
                                        line-y1
                                        line-x2
                                        line-y2)
    (update-radial-dimension dimension
                             :center-x x
                             :center-y y
                             :angle (mirror-angle (radial-dimension-angle dimension)
                                                  line-x1
                                                  line-y1
                                                  line-x2
                                                  line-y2))))

(defmethod entity-scaled ((dimension radial-dimension-entity) center-x center-y factor)
  (multiple-value-bind (x y) (scale-xy (radial-dimension-center-x dimension)
                                       (radial-dimension-center-y dimension)
                                       center-x
                                       center-y
                                       factor)
    (update-radial-dimension dimension
                             :center-x x
                             :center-y y
                             :radius (* factor (radial-dimension-radius dimension))
                             :text-height (* factor (radial-dimension-text-height dimension)))))

(defmethod entity-rotated ((dimension angular-dimension-entity) center-x center-y angle)
  (multiple-value-bind (x y) (rotate-xy (angular-dimension-vertex-x dimension)
                                        (angular-dimension-vertex-y dimension)
                                        center-x
                                        center-y
                                        angle)
    (update-angular-dimension dimension
                              :vertex-x x
                              :vertex-y y
                              :start-angle (+ (angular-dimension-start-angle dimension) angle)
                              :end-angle (+ (angular-dimension-end-angle dimension) angle))))

(defmethod entity-mirrored ((dimension angular-dimension-entity) line-x1 line-y1 line-x2 line-y2)
  (multiple-value-bind (x y) (mirror-xy (angular-dimension-vertex-x dimension)
                                        (angular-dimension-vertex-y dimension)
                                        line-x1
                                        line-y1
                                        line-x2
                                        line-y2)
    (update-angular-dimension dimension
                              :vertex-x x
                              :vertex-y y
                              :start-angle (mirror-angle (angular-dimension-end-angle dimension)
                                                         line-x1
                                                         line-y1
                                                         line-x2
                                                         line-y2)
                              :end-angle (mirror-angle (angular-dimension-start-angle dimension)
                                                       line-x1
                                                       line-y1
                                                       line-x2
                                                       line-y2))))

(defmethod entity-scaled ((dimension angular-dimension-entity) center-x center-y factor)
  (multiple-value-bind (x y) (scale-xy (angular-dimension-vertex-x dimension)
                                       (angular-dimension-vertex-y dimension)
                                       center-x
                                       center-y
                                       factor)
    (update-angular-dimension dimension
                              :vertex-x x
                              :vertex-y y
                              :radius (* factor (angular-dimension-radius dimension))
                              :text-height (* factor (angular-dimension-text-height dimension)))))

(defun transformed-hatch-coordinates (hatch
                                      transform)
  "Return the boundary of a hatch with every vertex moved by TRANSFORM.

HATCH: a hatch entity.
TRANSFORM: function of x and y that returns the new x and y as two values."
  (let* ((coordinates (hatch-coordinates hatch))
         (result (make-array (length coordinates) :element-type 'double-float)))
    (loop for index from 0 below (length coordinates) by 2
          do (multiple-value-bind (x y) (funcall transform
                                                 (aref coordinates index)
                                                 (aref coordinates (1+ index)))
               (setf (aref result index) x
                     (aref result (1+ index)) y)))
    result))

(defmethod entity-rotated ((hatch hatch-entity) center-x center-y angle)
  (update-hatch hatch
                :coordinates (transformed-hatch-coordinates hatch
                                                            (lambda (x y)
                                                              (rotate-xy x
                                                                         y
                                                                         center-x
                                                                         center-y
                                                                         angle)))
                :angle (+ (hatch-angle hatch) angle)))

(defmethod entity-mirrored ((hatch hatch-entity) line-x1 line-y1 line-x2 line-y2)
  (update-hatch hatch
                :coordinates (transformed-hatch-coordinates hatch
                                                            (lambda (x y)
                                                              (mirror-xy x
                                                                         y
                                                                         line-x1
                                                                         line-y1
                                                                         line-x2
                                                                         line-y2)))
                :angle (mirror-angle (hatch-angle hatch)
                                     line-x1
                                     line-y1
                                     line-x2
                                     line-y2)))

(defmethod entity-scaled ((hatch hatch-entity) center-x center-y factor)
  (update-hatch hatch
                :coordinates (transformed-hatch-coordinates hatch
                                                            (lambda (x y)
                                                              (scale-xy x
                                                                        y
                                                                        center-x
                                                                        center-y
                                                                        factor)))
                :spacing (* factor (hatch-spacing hatch))))
