(in-package #:borba-cad/geometry)

;;;; Whether a shape touches a window, which is what a crossing selection asks of every entity: the
;;;; outline of a shape touches the window when the outline passes through it, and a shape that has
;;;; the window wholly inside it, without its outline reaching it, does not touch.

(defun segment-touches-bounds-p (x1
                                 y1
                                 x2
                                 y2
                                 bounds)
  "Return true when a segment has a point inside BOUNDS or on its edge.

X1: x of the segment start.
Y1: y of the segment start.
X2: x of the segment end.
Y2: y of the segment end.
BOUNDS: the window."
  (declare (type double-float x1 y1 x2 y2))
  (and (clip-segment-to-bounds x1
                               y1
                               x2
                               y2
                               bounds) t))

(defun polyline-touches-bounds-p (coordinates
                                  closed
                                  bounds)
  "Return true when a polyline has a point inside BOUNDS or on its edge.

COORDINATES: vector of alternating x and y values, at least one vertex.
CLOSED: true when the last vertex joins the first.
BOUNDS: the window."
  (let ((count (floor (length coordinates) 2)))
    (flet ((x (index) (aref coordinates (* 2 index)))
           (y (index) (aref coordinates (1+ (* 2 index)))))
      (or (and (= count 1)
               (bounds-contains-point-p bounds
                                        (x 0)
                                        (y 0)))
          (loop for index from 0 below (1- count)
                thereis (segment-touches-bounds-p (x index)
                                                  (y index)
                                                  (x (1+ index))
                                                  (y (1+ index))
                                                  bounds))
          (and closed
               (> count 2)
               (segment-touches-bounds-p (x (1- count))
                                         (y (1- count))
                                         (x 0)
                                         (y 0)
                                         bounds))))))

(defun circle-outline-touches-bounds-p (center-x
                                        center-y
                                        radius
                                        bounds)
  "Return true when the outline of a circle has a point inside BOUNDS or on its edge: the window
holds a point at the distance RADIUS from the center, which is so when the nearest point of the
window is no farther than that and the farthest corner no nearer.

CENTER-X: x of the center.
CENTER-Y: y of the center.
RADIUS: the radius.
BOUNDS: the window."
  (declare (type double-float center-x center-y radius))
  (let* ((nearest-x (max (bounds-min-x bounds) (min center-x (bounds-max-x bounds))))
         (nearest-y (max (bounds-min-y bounds) (min center-y (bounds-max-y bounds))))
         (far-x (if (> (- center-x (bounds-min-x bounds)) (- (bounds-max-x bounds) center-x))
                    (bounds-min-x bounds)
                    (bounds-max-x bounds)))
         (far-y (if (> (- center-y (bounds-min-y bounds)) (- (bounds-max-y bounds) center-y))
                    (bounds-min-y bounds)
                    (bounds-max-y bounds))))
    (and (<= (distance center-x
                       center-y
                       nearest-x
                       nearest-y) radius)
         (>= (distance center-x
                       center-y
                       far-x
                       far-y) radius))))

(defun oriented-box-touches-bounds-p (origin-x
                                      origin-y
                                      length
                                      ascent
                                      descent
                                      angle
                                      bounds)
  "Return true when a filled rotated box and BOUNDS have a point in common: an edge of the box
reaches the window, or the window lies inside the box. The box is the one that
ORIENTED-BOX-BOUNDS describes.

ORIGIN-X: x of the box origin.
ORIGIN-Y: y of the box origin.
LENGTH: extent along the direction.
ASCENT: extent on the left of the direction.
DESCENT: extent on the right of the direction.
ANGLE: direction in radians, counter-clockwise from +x.
BOUNDS: the window."
  (declare (type double-float origin-x origin-y length ascent descent angle))
  (let* ((cosine (cos angle))
         (sine (sin angle))
         (corners (loop for (along across) in (list (list 0d0 (- descent))
                                                    (list length (- descent))
                                                    (list length ascent)
                                                    (list 0d0 ascent))
                        collect (cons (+ origin-x (* along cosine) (* (- across) sine))
                                      (+ origin-y (* along sine) (* across cosine))))))
    (or (loop for (from to) on (append corners (list (first corners)))
              while to
              thereis (segment-touches-bounds-p (car from)
                                                (cdr from)
                                                (car to)
                                                (cdr to)
                                                bounds))
        (<= (point-oriented-box-distance (bounds-min-x bounds)
                                         (bounds-min-y bounds)
                                         origin-x
                                         origin-y
                                         length
                                         ascent
                                         descent
                                         angle)
            0d0))))
