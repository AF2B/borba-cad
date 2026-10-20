(in-package #:borba-cad/image)

;;;; Filling and stroking shapes into a raster with smooth edges, for drawing the lines of a PDF page. A shape is a
;;;; list of edges; a row of pixels is cut into sub-rows, each cut by the edges into spans that the winding rule
;;;; says are inside, and the spans add up to the share of each pixel that is covered. A stroke is made of convex
;;;; pieces (a quadrilateral for each segment, a disc or a wedge for each join and cap) that all wind the same way,
;;;; so that filling them with the non-zero rule fills their union.

(defconstant +sub-rows+ 4
  "How many sub-rows a row of pixels is cut into when a shape is filled.")

(defconstant +maximum-fill-edges+ 8000000
  "The most edges a shape that cannot be cut up (a filled one) may have before the page is called too complex.")

(defconstant +stroke-flush-edges+ 1000000
  "How many edges of strokes are gathered before they are drawn and the list is emptied.")

(defconstant +minimum-line-width+ 1.0d0
  "The narrowest line drawn, in pixels, so that a thin line of a plan stays visible.")

(defconstant +flat-angle-sine+ 1d-6
  "Under this sine of the angle between two segments the join between them is not drawn.")

(defstruct (edge-list (:conc-name edges-))
  "The edges of a shape: each from (x0, y0) to (x1, y1) with y0 under y1, and +1 when the shape's outline went
down along it, -1 when it went up."
  (x0 (make-array 1024 :element-type 'double-float) :type (simple-array double-float (*)))
  (y0 (make-array 1024 :element-type 'double-float) :type (simple-array double-float (*)))
  (x1 (make-array 1024 :element-type 'double-float) :type (simple-array double-float (*)))
  (y1 (make-array 1024 :element-type 'double-float) :type (simple-array double-float (*)))
  (direction (make-array 1024 :element-type '(signed-byte 8)) :type (simple-array (signed-byte 8) (*)))
  (count 0 :type fixnum))

(defun grow-edge-list (edges)
  "Double the room of an edge list.

EDGES: the edge list."
  (declare (type edge-list edges))
  (flet ((bigger (vector)
           (let ((copy (make-array (* 2 (length vector)) :element-type (array-element-type vector))))
             (replace copy vector)
             copy)))
    (setf (edges-x0 edges) (bigger (edges-x0 edges))
          (edges-y0 edges) (bigger (edges-y0 edges))
          (edges-x1 edges) (bigger (edges-x1 edges))
          (edges-y1 edges) (bigger (edges-y1 edges))
          (edges-direction edges) (bigger (edges-direction edges)))))

(defun add-edge (edges
                 x0
                 y0
                 x1
                 y1)
  "Add the edge from (X0, Y0) to (X1, Y1) to an edge list; edges that are level are left out.

EDGES: the edge list.
X0: x of the start.
Y0: y of the start.
X1: x of the end.
Y1: y of the end."
  (declare (type edge-list edges)
           (type double-float x0 y0 x1 y1)
           (optimize (speed 3) (safety 1))
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (unless (= y0 y1)
    (when (>= (edges-count edges) (length (edges-x0 edges)))
      (grow-edge-list edges))
    (let ((index (edges-count edges)))
      (if (< y0 y1)
          (setf (aref (edges-x0 edges) index) x0
                (aref (edges-y0 edges) index) y0
                (aref (edges-x1 edges) index) x1
                (aref (edges-y1 edges) index) y1
                (aref (edges-direction edges) index) 1)
          (setf (aref (edges-x0 edges) index) x1
                (aref (edges-y0 edges) index) y1
                (aref (edges-x1 edges) index) x0
                (aref (edges-y1 edges) index) y0
                (aref (edges-direction edges) index) -1))
      (incf (edges-count edges)))))

(defun add-polygon-edges (edges
                          points)
  "Add the edges of a closed polygon to an edge list.

EDGES: the edge list.
POINTS: a vector of doubles, x then y of each corner in turn."
  (declare (type edge-list edges)
           (type (simple-array double-float (*)) points))
  (let ((count (floor (length points) 2)))
    (dotimes (index count)
      (let ((next (mod (1+ index) count)))
        (add-edge edges
                  (aref points (* 2 index))
                  (aref points (1+ (* 2 index)))
                  (aref points (* 2 next))
                  (aref points (1+ (* 2 next))))))))

(defun polygon-area (points)
  "Return the signed area of a polygon, positive when its corners go one way round and negative the other.

POINTS: a vector of doubles, x then y of each corner in turn."
  (declare (type (simple-array double-float (*)) points))
  (let ((count (floor (length points) 2))
        (sum 0d0))
    (dotimes (index count (/ sum 2))
      (let ((next (mod (1+ index) count)))
        (incf sum (- (* (aref points (* 2 index)) (aref points (1+ (* 2 next))))
                     (* (aref points (* 2 next)) (aref points (1+ (* 2 index))))))))))

(defun add-oriented-polygon (edges
                             points)
  "Add a polygon to an edge list turned, if need be, to wind the one way that every piece of a stroke winds, so that
the non-zero rule fills their union.

EDGES: the edge list.
POINTS: a vector of doubles, x then y of each corner in turn."
  (declare (type (simple-array double-float (*)) points))
  (if (minusp (polygon-area points))
      (let* ((count (floor (length points) 2))
             (reversed (make-array (length points) :element-type 'double-float)))
        (dotimes (index count)
          (setf (aref reversed (* 2 (- count index 1))) (aref points (* 2 index))
                (aref reversed (1+ (* 2 (- count index 1)))) (aref points (1+ (* 2 index)))))
        (add-polygon-edges edges reversed))
      (add-polygon-edges edges points)))

;;; Filling

(defstruct (fill-clip (:conc-name clip-))
  "The pixels a drawing may reach: columns from X0 up to X1 and rows from Y0 up to Y1."
  (x0 0 :type fixnum)
  (y0 0 :type fixnum)
  (x1 0 :type fixnum)
  (y1 0 :type fixnum))

(defun sort-crossings (positions
                       directions
                       count)
  "Sort the first COUNT crossings of a sub-row by position, keeping each with its direction.

POSITIONS: the positions of the crossings.
DIRECTIONS: the directions of the crossings.
COUNT: how many there are."
  (declare (type (simple-array double-float (*)) positions)
           (type (simple-array fixnum (*)) directions)
           (type fixnum count)
           (optimize (speed 3) (safety 1))
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (if (<= count 64)
      (loop for index of-type fixnum from 1 below count
            do (let ((position (aref positions index))
                     (direction (aref directions index))
                     (before (1- index)))
                 (declare (type fixnum before))
                 (loop while (and (>= before 0) (> (aref positions before) position))
                       do (setf (aref positions (1+ before)) (aref positions before)
                                (aref directions (1+ before)) (aref directions before))
                          (decf before))
                 (setf (aref positions (1+ before)) position
                       (aref directions (1+ before)) direction)))
      (let ((pairs (make-array count)))
        (dotimes (index count)
          (setf (svref pairs index) (cons (aref positions index) (aref directions index))))
        (setf pairs (sort pairs #'< :key #'car))
        (dotimes (index count)
          (setf (aref positions index) (car (svref pairs index))
                (aref directions index) (cdr (svref pairs index)))))))

(defstruct (fill-state (:conc-name fill-))
  "What filling a shape keeps while it goes down the rows: the edges ordered by the row they start in, those that cross
the current row, the crossings of the current sub-row, and the coverage gathered for the current row of pixels."
  (width 0 :type fixnum)
  (clip-x0 0 :type fixnum)
  (clip-x1 0 :type fixnum)
  (clip-y0 0 :type fixnum)
  (rows 0 :type fixnum)
  (x0 (make-array 0 :element-type 'double-float) :type (simple-array double-float (*)))
  (y0 (make-array 0 :element-type 'double-float) :type (simple-array double-float (*)))
  (x1 (make-array 0 :element-type 'double-float) :type (simple-array double-float (*)))
  (y1 (make-array 0 :element-type 'double-float) :type (simple-array double-float (*)))
  (direction (make-array 0 :element-type '(signed-byte 8)) :type (simple-array (signed-byte 8) (*)))
  (bucket-start (make-array 0 :element-type 'fixnum) :type (simple-array fixnum (*)))
  (order (make-array 0 :element-type 'fixnum) :type (simple-array fixnum (*)))
  (active (make-array 256 :element-type 'fixnum) :type (simple-array fixnum (*)))
  (active-count 0 :type fixnum)
  (positions (make-array 256 :element-type 'double-float) :type (simple-array double-float (*)))
  (directions (make-array 256 :element-type 'fixnum) :type (simple-array fixnum (*)))
  (cover (make-array 0 :element-type 'double-float) :type (simple-array double-float (*)))
  (diff (make-array 0 :element-type 'double-float) :type (simple-array double-float (*)))
  (min-x 0 :type fixnum)
  (max-x 0 :type fixnum))

(defun prepare-fill (raster
                     edges
                     clip)
  "Return the state for filling a shape into a raster, or NIL when nothing of it can fall inside the clip.

RASTER: the raster.
EDGES: the edge list of the shape.
CLIP: the FILL-CLIP the painting may not leave."
  (declare (type raster raster)
           (type edge-list edges)
           (type fill-clip clip)
           (optimize (speed 3) (safety 1))
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let* ((count (edges-count edges))
         (width (raster-width raster))
         (clip-x0 (max 0 (clip-x0 clip)))
         (clip-x1 (min width (clip-x1 clip)))
         (clip-y0 (max 0 (clip-y0 clip)))
         (clip-y1 (min (raster-height raster) (clip-y1 clip)))
         (rows (- clip-y1 clip-y0)))
    (declare (type fixnum count width clip-x0 clip-x1 clip-y0 clip-y1 rows))
    (when (or (zerop count) (<= rows 0) (<= clip-x1 clip-x0))
      (return-from prepare-fill nil))
    (when (> count +maximum-fill-edges+)
      (image-failure "This PDF page is too complex for the program."))
    (let* ((y0 (edges-y0 edges))
           (y1 (edges-y1 edges))
           (low (float clip-y0 1d0))
           (high (float clip-y1 1d0))
           (bucket-start (make-array (+ rows 2) :element-type 'fixnum :initial-element 0))
           (order (make-array count :element-type 'fixnum :initial-element 0)))
      (declare (type (simple-array double-float (*)) y0 y1)
               (type double-float low high))
      (flet ((first-row (index)
               (declare (type fixnum index))
               (max 0 (- (the fixnum (floor (aref y0 index))) clip-y0)))
             (inside-p (index)
               (declare (type fixnum index))
               (and (> (aref y1 index) low)
                    (< (aref y0 index) high))))
        (declare (inline first-row inside-p))
        (dotimes (index count)
          (when (inside-p index)
            (incf (aref bucket-start (1+ (first-row index))))))
        (loop for row of-type fixnum from 1 to (1+ rows)
              do (incf (aref bucket-start row) (aref bucket-start (1- row))))
        (let ((next (copy-seq bucket-start)))
          (declare (type (simple-array fixnum (*)) next))
          (dotimes (index count)
            (when (inside-p index)
              (let ((row (first-row index)))
                (setf (aref order (aref next row)) index)
                (incf (aref next row)))))))
      (make-fill-state :width width
                       :clip-x0 clip-x0
                       :clip-x1 clip-x1
                       :clip-y0 clip-y0
                       :rows rows
                       :x0 (edges-x0 edges)
                       :y0 y0
                       :x1 (edges-x1 edges)
                       :y1 y1
                       :direction (edges-direction edges)
                       :bucket-start bucket-start
                       :order order
                       :cover (make-array (+ width 2) :element-type 'double-float :initial-element 0d0)
                       :diff (make-array (+ width 2) :element-type 'double-float :initial-element 0d0)))))

(defun activate-row (state
                     row)
  "Make the edges that start in a row of the fill current, and drop those that ended above it.

STATE: the fill state.
ROW: the row, counted from the top of the clip."
  (declare (type fill-state state)
           (type fixnum row)
           (optimize (speed 3) (safety 1))
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((starts (fill-bucket-start state))
        (order (fill-order state))
        (y1 (fill-y1 state))
        (top (float (+ row (fill-clip-y0 state)) 1d0)))
    (declare (type double-float top))
    (loop for slot of-type fixnum from (aref starts row) below (aref starts (1+ row))
          do (when (>= (fill-active-count state) (length (fill-active state)))
               (let ((bigger (make-array (* 2 (length (fill-active state))) :element-type 'fixnum)))
                 (replace bigger (fill-active state))
                 (setf (fill-active state) bigger)))
             (setf (aref (fill-active state) (fill-active-count state)) (aref order slot))
             (incf (fill-active-count state)))
    (let ((kept 0)
          (active (fill-active state)))
      (declare (type fixnum kept))
      (dotimes (slot (fill-active-count state))
        (let ((index (aref active slot)))
          (when (> (aref y1 index) top)
            (setf (aref active kept) index)
            (incf kept))))
      (setf (fill-active-count state) kept))
    (when (> (fill-active-count state) (length (fill-positions state)))
      (setf (fill-positions state) (make-array (* 2 (fill-active-count state)) :element-type 'double-float)
            (fill-directions state) (make-array (* 2 (fill-active-count state)) :element-type 'fixnum)))))

(defun accumulate-sub-row (state
                           y
                           even-odd)
  "Cut one sub-row of the current row by the active edges and add the spans that are inside to the coverage.

STATE: the fill state.
Y: the vertical position of the sub-row, in pixels.
EVEN-ODD: true for the even-odd rule, false for the non-zero rule."
  (declare (type fill-state state)
           (type double-float y)
           (optimize (speed 3) (safety 1))
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((active (fill-active state))
        (x0 (fill-x0 state))
        (y0 (fill-y0 state))
        (x1 (fill-x1 state))
        (y1 (fill-y1 state))
        (edge-direction (fill-direction state))
        (positions (fill-positions state))
        (directions (fill-directions state))
        (crossings 0))
    (declare (type fixnum crossings))
    (dotimes (slot (fill-active-count state))
      (let ((index (aref active slot)))
        (when (and (<= (aref y0 index) y) (< y (aref y1 index)))
          (setf (aref positions crossings)
                (+ (aref x0 index)
                   (* (- y (aref y0 index))
                      (/ (- (aref x1 index) (aref x0 index))
                         (- (aref y1 index) (aref y0 index)))))
                (aref directions crossings) (aref edge-direction index))
          (incf crossings))))
    (when (> crossings 1)
      (sort-crossings positions
                      directions
                      crossings)
      (let ((winding 0)
            (cover (fill-cover state))
            (diff (fill-diff state))
            (low (float (fill-clip-x0 state) 1d0))
            (high (float (fill-clip-x1 state) 1d0)))
        (declare (type fixnum winding)
                 (type double-float low high))
        (dotimes (crossing (1- crossings))
          (incf winding (aref directions crossing))
          (when (if even-odd (oddp (1+ crossing)) (/= winding 0))
            (let ((xa (max low (aref positions crossing)))
                  (xb (min high (aref positions (1+ crossing)))))
              (declare (type double-float xa xb))
              (when (> xb xa)
                (let ((ia (the fixnum (floor xa)))
                      (ib (the fixnum (floor xb))))
                  (declare (type fixnum ia ib))
                  (setf (fill-min-x state) (min (fill-min-x state) ia)
                        (fill-max-x state) (max (fill-max-x state) ib))
                  (cond ((= ia ib)
                         (incf (aref cover ia) (- xb xa)))
                        (t
                         (incf (aref cover ia) (- (float (1+ ia) 1d0) xa))
                         (incf (aref cover ib) (- xb (float ib 1d0)))
                         (when (> ib (1+ ia))
                           (incf (aref diff (1+ ia)) 1d0)
                           (decf (aref diff ib) 1d0)))))))))))))

(defun blend-row (state
                  raster
                  device-row
                  red
                  green
                  blue
                  alpha)
  "Paint the pixels of a row with the coverage gathered, and clear the coverage.

STATE: the fill state.
RASTER: the raster.
DEVICE-ROW: the row, counted from the top of the raster.
RED: the red of the paint, 0 to 255.
GREEN: the green of the paint.
BLUE: the blue of the paint.
ALPHA: the opacity of the paint, 0 to 1."
  (declare (type fill-state state)
           (type raster raster)
           (type fixnum device-row red green blue)
           (type double-float alpha)
           (optimize (speed 3) (safety 1))
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((cover (fill-cover state))
        (diff (fill-diff state))
        (pixels (raster-pixels raster))
        (width (fill-width state))
        (running 0d0)
        (last (min (fill-max-x state) (1- (fill-clip-x1 state)))))
    (declare (type double-float running)
             (type fixnum width last))
    (loop for x of-type fixnum from (fill-min-x state) to last
          do (incf running (aref diff x))
             (let ((coverage (* alpha (min 1d0 (/ (+ (aref cover x) running) +sub-rows+)))))
               (declare (type double-float coverage))
               (when (> coverage 0d0)
                 (let ((at (* +raster-channels+ (+ x (* device-row width)))))
                   (declare (type fixnum at))
                   (macrolet ((mix (offset paint)
                                `(let ((old (aref pixels (+ at ,offset))))
                                   (setf (aref pixels (+ at ,offset))
                                         (the (unsigned-byte 8)
                                              (max 0 (min 255 (round (+ old (* coverage (- ,paint old)))))))))))
                     (mix 0 red)
                     (mix 1 green)
                     (mix 2 blue))))))
    (loop for x of-type fixnum from (fill-min-x state) to (min (1+ (fill-max-x state)) (1+ width))
          do (setf (aref cover x) 0d0
                   (aref diff x) 0d0))))

(defun fill-edges (raster
                   edges
                   even-odd
                   red
                   green
                   blue
                   alpha
                   clip)
  "Paint the inside of a shape into a raster with a color, blending the pixels its edge cuts.

RASTER: the raster.
EDGES: the edge list of the shape.
EVEN-ODD: true for the even-odd rule, false for the non-zero rule.
RED: the red of the paint, 0 to 255.
GREEN: the green of the paint.
BLUE: the blue of the paint.
ALPHA: the opacity of the paint, 0 to 1.
CLIP: the FILL-CLIP the painting may not leave."
  (let ((state (prepare-fill raster
                             edges
                             clip)))
    (when state
      (dotimes (row (fill-rows state))
        (let ((device-row (+ row (fill-clip-y0 state))))
          (setf (fill-min-x state) (fill-clip-x1 state)
                (fill-max-x state) (fill-clip-x0 state))
          (activate-row state row)
          (dotimes (sub +sub-rows+)
            (accumulate-sub-row state
                                (+ device-row (/ (+ sub 0.5d0) +sub-rows+))
                                even-odd))
          (when (<= (fill-min-x state) (fill-max-x state))
            (blend-row state
                       raster
                       device-row
                       red
                       green
                       blue
                       alpha)))))
    nil))

;;; Strokes

(defconstant +cap-butt+ 0
  "The line cap that ends a line flat at its end point.")

(defconstant +cap-round+ 1
  "The line cap that ends a line with a half circle.")

(defconstant +cap-square+ 2
  "The line cap that ends a line flat, half a width past its end point.")

(defconstant +join-miter+ 0
  "The line join that meets the two sides in a point.")

(defconstant +join-round+ 1
  "The line join that rounds the corner.")

(defconstant +join-bevel+ 2
  "The line join that cuts the corner flat.")

(defconstant +circle-tolerance+ 0.1d0
  "How far, in pixels, the polygon that stands for a circle may be from the circle.")

(defconstant +minimum-circle-sides+ 8
  "The fewest sides of a polygon that stands for a circle.")

(defconstant +maximum-circle-sides+ 72
  "The most sides of a polygon that stands for a circle.")

(defconstant +minimum-dash-period+ 0.5d0
  "A dash pattern that repeats in less than this many pixels is drawn as a solid line, as it looks like one.")

(defconstant +maximum-dashes+ 2000000
  "The most dashes one path is cut into before the line is drawn solid.")

(defconstant +coincident-length+ 1d-9
  "Under this distance two points of a path are one.")

(defun doubles (&rest numbers)
  "Return a vector of doubles holding NUMBERS.

NUMBERS: the numbers."
  (make-array (length numbers)
              :element-type 'double-float
              :initial-contents (mapcar (lambda (number) (float number 1d0)) numbers)))

(defun add-disc (edges
                 x
                 y
                 radius)
  "Add a disc, as a polygon, to the pieces of a stroke.

EDGES: the edge list.
X: x of the center.
Y: y of the center.
RADIUS: the radius in pixels."
  (declare (type double-float x y radius))
  (let* ((sides (if (<= radius +circle-tolerance+)
                    +minimum-circle-sides+
                    (max +minimum-circle-sides+
                         (min +maximum-circle-sides+
                              (ceiling (/ pi (acos (- 1 (/ +circle-tolerance+ radius)))))))))
         (points (make-array (* 2 sides) :element-type 'double-float)))
    (dotimes (index sides)
      (let ((angle (/ (* 2 pi index) sides)))
        (setf (aref points (* 2 index)) (+ x (* radius (cos angle)))
              (aref points (1+ (* 2 index))) (+ y (* radius (sin angle))))))
    (add-oriented-polygon edges points)))

(defun add-stroke-segment (edges
                           x0
                           y0
                           x1
                           y1
                           half)
  "Add the rectangle that a line segment of some width covers to the pieces of a stroke.

EDGES: the edge list.
X0: x of the start.
Y0: y of the start.
X1: x of the end.
Y1: y of the end.
HALF: half the width of the line, in pixels."
  (declare (type double-float x0 y0 x1 y1 half))
  (let* ((dx (- x1 x0))
         (dy (- y1 y0))
         (length (sqrt (+ (* dx dx) (* dy dy)))))
    (when (> length +coincident-length+)
      (let ((nx (/ (* (- dy) half) length))
            (ny (/ (* dx half) length)))
        (add-oriented-polygon edges
                              (doubles (+ x0 nx)
                                       (+ y0 ny)
                                       (+ x1 nx)
                                       (+ y1 ny)
                                       (- x1 nx)
                                       (- y1 ny)
                                       (- x0 nx)
                                       (- y0 ny)))))))

(defun add-stroke-join (edges
                        x
                        y
                        in-x
                        in-y
                        out-x
                        out-y
                        half
                        join
                        miter-limit)
  "Add the piece that joins two segments at a corner to the pieces of a stroke.

EDGES: the edge list.
X: x of the corner.
Y: y of the corner.
IN-X: x of the unit direction of the segment that arrives.
IN-Y: y of that direction.
OUT-X: x of the unit direction of the segment that leaves.
OUT-Y: y of that direction.
HALF: half the width of the line, in pixels.
JOIN: the line join, one of the +JOIN-...+ constants.
MITER-LIMIT: the longest miter, as a multiple of the width."
  (declare (type double-float x y in-x in-y out-x out-y half miter-limit))
  (let ((cross (- (* in-x out-y) (* in-y out-x)))
        (dot (+ (* in-x out-x) (* in-y out-y))))
    (unless (and (< (abs cross) +flat-angle-sine+) (> dot 0))
      (if (= join +join-round+)
          (add-disc edges
                    x
                    y
                    half)
          (let* ((side (if (plusp cross) -1d0 1d0))
                 (n1x (* side (- in-y) half))
                 (n1y (* side in-x half))
                 (n2x (* side (- out-y) half))
                 (n2y (* side out-x half))
                 (cosine (+ (* in-x out-x) (* in-y out-y)))
                 (miter-ratio (if (> (+ 1 cosine) 1d-9)
                                  (/ 1 (sqrt (/ (+ 1 cosine) 2)))
                                  most-positive-double-float)))
            (if (and (= join +join-miter+) (<= miter-ratio miter-limit))
                (let ((scale (/ 1 (+ 1 cosine))))
                  (add-oriented-polygon edges
                                        (doubles x
                                                 y
                                                 (+ x n1x)
                                                 (+ y n1y)
                                                 (+ x (* scale (+ n1x n2x)))
                                                 (+ y (* scale (+ n1y n2y)))
                                                 (+ x n2x)
                                                 (+ y n2y))))
                (add-oriented-polygon edges
                                      (doubles x
                                               y
                                               (+ x n1x)
                                               (+ y n1y)
                                               (+ x n2x)
                                               (+ y n2y)))))))))

(defun add-stroke-cap (edges
                       x
                       y
                       dx
                       dy
                       half
                       cap)
  "Add the end of a line to the pieces of a stroke.

EDGES: the edge list.
X: x of the end point.
Y: y of the end point.
DX: x of the unit direction pointing out of the line.
DY: y of that direction.
HALF: half the width of the line, in pixels.
CAP: the line cap, one of the +CAP-...+ constants."
  (declare (type double-float x y dx dy half))
  (cond ((= cap +cap-round+)
         (add-disc edges
                   x
                   y
                   half))
        ((= cap +cap-square+)
         (add-stroke-segment edges
                             x
                             y
                             (+ x (* dx half))
                             (+ y (* dy half))
                             half))))

(defun distinct-points (points
                        closed)
  "Return the points of a path without the repeats of a point right after itself, and for a closed path without a last
point that is the first.

POINTS: a vector of doubles, x then y of each point in turn.
CLOSED: true for a closed path."
  (declare (type (simple-array double-float (*)) points))
  (let ((kept '()))
    (dotimes (index (floor (length points) 2))
      (let ((x (aref points (* 2 index)))
            (y (aref points (1+ (* 2 index)))))
        (unless (and kept
                     (< (abs (- x (first (first kept)))) +coincident-length+)
                     (< (abs (- y (second (first kept)))) +coincident-length+))
          (push (list x y) kept))))
    (when (and closed
               (> (length kept) 1)
               (< (abs (- (first (first kept)) (first (car (last kept))))) +coincident-length+)
               (< (abs (- (second (first kept)) (second (car (last kept))))) +coincident-length+))
      (pop kept))
    (nreverse kept)))

(defun stroke-polyline (edges
                        points
                        closed
                        half
                        cap
                        join
                        miter-limit)
  "Add the pieces that stroke a path to an edge list: a rectangle for each segment, a piece for each corner, and the
ends.

EDGES: the edge list.
POINTS: a vector of doubles, x then y of each point of the path in turn.
CLOSED: true for a closed path, which has joins all round and no ends.
HALF: half the width of the line, in pixels.
CAP: the line cap, one of the +CAP-...+ constants.
JOIN: the line join, one of the +JOIN-...+ constants.
MITER-LIMIT: the longest miter, as a multiple of the width."
  (let* ((distinct (distinct-points points closed))
         (count (length distinct))
         (vertices (coerce distinct 'simple-vector))
         (closed (and closed (>= count 3))))
    (cond ((zerop count))
          ((= count 1)
           (let ((x (first (svref vertices 0)))
                 (y (second (svref vertices 0))))
             (cond ((= cap +cap-round+)
                    (add-disc edges
                              x
                              y
                              half))
                   ((= cap +cap-square+)
                    (add-oriented-polygon edges
                                          (doubles (- x half)
                                                   (- y half)
                                                   (+ x half)
                                                   (- y half)
                                                   (+ x half)
                                                   (+ y half)
                                                   (- x half)
                                                   (+ y half)))))))
          (t
           (let* ((segments (if closed count (1- count)))
                  (directions (make-array segments)))
             (dotimes (index segments)
               (destructuring-bind (x0 y0)
                   (svref vertices index)
                 (destructuring-bind (x1 y1)
                     (svref vertices (mod (1+ index) count))
                   (let* ((dx (- x1 x0))
                          (dy (- y1 y0))
                          (length (sqrt (+ (* dx dx) (* dy dy)))))
                     (setf (svref directions index) (list (/ dx length) (/ dy length)))
                     (add-stroke-segment edges
                                         x0
                                         y0
                                         x1
                                         y1
                                         half)))))
             (loop for index from (if closed 0 1) below segments
                   do (let ((previous (svref directions (mod (1- index) segments)))
                            (next (svref directions (mod index segments)))
                            (vertex (svref vertices (mod index count))))
                        (add-stroke-join edges
                                         (first vertex)
                                         (second vertex)
                                         (first previous)
                                         (second previous)
                                         (first next)
                                         (second next)
                                         half
                                         join
                                         miter-limit)))
             (unless closed
               (let ((first-direction (svref directions 0))
                     (last-direction (svref directions (1- segments)))
                     (start (svref vertices 0))
                     (end (svref vertices (1- count))))
                 (add-stroke-cap edges
                                 (first start)
                                 (second start)
                                 (- (first first-direction))
                                 (- (second first-direction))
                                 half
                                 cap)
                 (add-stroke-cap edges
                                 (first end)
                                 (second end)
                                 (first last-direction)
                                 (second last-direction)
                                 half
                                 cap))))))))

(defun dash-polyline (points
                      closed
                      pattern
                      phase)
  "Cut a path into the dashes that a dash pattern leaves, and return them as a list of vectors of points. A pattern too
fine to see, or that would cut the path into too many dashes, leaves the path whole.

POINTS: a vector of doubles, x then y of each point of the path in turn.
CLOSED: true for a closed path, which is cut along its closing segment too.
PATTERN: a list of dash and gap lengths in pixels, alternating, starting with a dash.
PHASE: how far into the pattern the path starts, in pixels."
  (let* ((vertices (let ((list (distinct-points points nil)))
                     (if (and closed (> (length list) 1))
                         (append list (list (first list)))
                         list)))
         (pattern (coerce pattern 'simple-vector))
         (total (reduce #'+ pattern))
         (pieces '())
         (dash-count 0)
         (current nil))
    (when (or (<= total +minimum-dash-period+) (< (length vertices) 2))
      (return-from dash-polyline (list points)))
    (let* ((phase (mod phase (* total (if (oddp (length pattern)) 2 1))))
           (index 0)
           (remaining 0d0)
           (drawing t))
      (loop
        (let ((length (svref pattern (mod index (length pattern)))))
          (if (>= phase length)
              (progn (decf phase length)
                     (incf index)
                     (setf drawing (not drawing)))
              (progn (setf remaining (- length phase)
                           phase 0)
                     (return)))))
      (labels ((begin-dash (x y)
                 (setf current (make-array 8 :element-type 'double-float :adjustable t :fill-pointer 0))
                 (vector-push-extend x current)
                 (vector-push-extend y current))
               (add-point (x y)
                 (vector-push-extend x current)
                 (vector-push-extend y current))
               (end-dash ()
                 (when current
                   (push (coerce current '(simple-array double-float (*))) pieces)
                   (incf dash-count)
                   (setf current nil))))
        (when drawing
          (begin-dash (first (first vertices)) (second (first vertices))))
        (loop for (start end) on vertices
              while end
              do (let* ((x0 (first start))
                        (y0 (second start))
                        (dx (- (first end) x0))
                        (dy (- (second end) y0))
                        (length (sqrt (+ (* dx dx) (* dy dy))))
                        (travelled 0d0))
                   (loop while (< travelled length)
                         do (when (> dash-count +maximum-dashes+)
                              (return-from dash-polyline (list points)))
                            (let ((step (min remaining (- length travelled))))
                              (incf travelled step)
                              (decf remaining step)
                              (when drawing
                                (add-point (+ x0 (* dx (/ travelled length))) (+ y0 (* dy (/ travelled length)))))
                              (when (<= remaining 1d-12)
                                (when drawing
                                  (end-dash))
                                (setf drawing (not drawing))
                                (incf index)
                                (setf remaining (svref pattern (mod index (length pattern))))
                                (when drawing
                                  (begin-dash (+ x0 (* dx (/ travelled length)))
                                              (+ y0 (* dy (/ travelled length))))))))))
        (end-dash)))
    (nreverse pieces)))
