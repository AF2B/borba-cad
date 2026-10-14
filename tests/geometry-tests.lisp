(in-package #:borba-cad/tests)

(def-suite geometry
  :description "Computational geometry: bounds, distances, clipping, measures, text and dimensions."
  :in all)

(in-suite geometry)

(defconstant +test-pi+ (float pi 1d0)
  "Pi as a double-float, for expected values.")

;;; Bounds

(test bounds-from-corners-normalizes-corner-order
  (let ((bounds (geo:bounds-from-corners 5d0 -2d0 -1d0 4d0)))
    (is (= -1d0 (geo:bounds-min-x bounds)))
    (is (= -2d0 (geo:bounds-min-y bounds)))
    (is (= 5d0 (geo:bounds-max-x bounds)))
    (is (= 4d0 (geo:bounds-max-y bounds)))
    (is (= 6d0 (geo:bounds-width bounds)))
    (is (= 6d0 (geo:bounds-height bounds)))
    (is (= 2d0 (geo:bounds-center-x bounds)))
    (is (= 1d0 (geo:bounds-center-y bounds)))))

(test bounds-union-covers-both-operands
  (let ((union (geo:bounds-union (geo:make-bounds 0d0 0d0 1d0 1d0)
                                 (geo:make-bounds -3d0 0.5d0 0.5d0 7d0))))
    (is (= -3d0 (geo:bounds-min-x union)))
    (is (= 0d0 (geo:bounds-min-y union)))
    (is (= 1d0 (geo:bounds-max-x union)))
    (is (= 7d0 (geo:bounds-max-y union)))))

(test bounds-intersection-counts-touching-and-rejects-gaps
  (let ((unit (geo:make-bounds 0d0 0d0 1d0 1d0)))
    (is (geo:bounds-intersect-p unit (geo:make-bounds 1d0 1d0 2d0 2d0)))      ; shared corner
    (is (geo:bounds-intersect-p unit (geo:make-bounds 0.5d0 0.5d0 0.6d0 0.6d0))) ; nested
    (is (geo:bounds-intersect-p unit (geo:make-bounds -1d0 -1d0 3d0 3d0)))    ; enclosing
    (is (not (geo:bounds-intersect-p unit (geo:make-bounds 1.0000001d0 0d0 2d0 1d0))))
    (is (not (geo:bounds-intersect-p unit (geo:make-bounds 0d0 -3d0 1d0 -0.1d0))))))

(test bounds-containment
  (let ((outer (geo:make-bounds 0d0 0d0 10d0 10d0)))
    (is (geo:bounds-contains-bounds-p outer (geo:make-bounds 2d0 2d0 3d0 3d0)))
    (is (geo:bounds-contains-bounds-p outer outer))
    (is (not (geo:bounds-contains-bounds-p outer (geo:make-bounds 2d0 2d0 10.5d0 3d0))))
    (is (geo:bounds-contains-point-p outer 10d0 0d0))
    (is (geo:bounds-contains-point-p outer 5d0 5d0))
    (is (not (geo:bounds-contains-point-p outer 10.01d0 5d0)))))

(test expand-bounds-grows-and-shrinks
  (let ((grown (geo:expand-bounds (geo:make-bounds 0d0 0d0 10d0 10d0) 2d0))
        (shrunk (geo:expand-bounds (geo:make-bounds 0d0 0d0 10d0 10d0) -1d0)))
    (is (= -2d0 (geo:bounds-min-x grown)))
    (is (= 12d0 (geo:bounds-max-y grown)))
    (is (= 1d0 (geo:bounds-min-y shrunk)))
    (is (= 9d0 (geo:bounds-max-x shrunk)))))

;;; Scalars

(test distance-matches-a-pythagorean-triple-and-is-symmetric
  (is (= 5d0 (geo:distance 0d0 0d0 3d0 4d0)))
  (is (= 5d0 (geo:distance 3d0 4d0 0d0 0d0)))
  (is (= 0d0 (geo:distance 7d0 -2d0 7d0 -2d0)))
  (is (approx= 1.4142135623730951d0 (geo:distance 0d0 0d0 1d0 1d0))))

(test normalize-angle-wraps-into-zero-to-two-pi
  (is (approx= (* 1.5d0 +test-pi+) (geo:normalize-angle (- (/ +test-pi+ 2)))))
  (is (= 0d0 (geo:normalize-angle (* 2 +test-pi+))))
  (is (approx= +test-pi+ (geo:normalize-angle (* 5 +test-pi+))))
  (is (= 0d0 (geo:normalize-angle 0d0)))
  (is (< (geo:normalize-angle -1d-300) (* 2 +test-pi+))))

(test segment-angle-covers-all-quadrants
  (is (= 0d0 (geo:segment-angle 0d0 0d0 1d0 0d0)))
  (is (approx= (/ +test-pi+ 2) (geo:segment-angle 0d0 0d0 0d0 1d0)))
  (is (approx= +test-pi+ (geo:segment-angle 0d0 0d0 -1d0 0d0)))
  (is (approx= (* 1.5d0 +test-pi+) (geo:segment-angle 0d0 0d0 0d0 -1d0)))
  (is (approx= (/ +test-pi+ 4) (geo:segment-angle 0d0 0d0 1d0 1d0)))
  (is (approx= (* 1.75d0 +test-pi+) (geo:segment-angle 0d0 0d0 1d0 -1d0))))

(test snap-coordinate-rounds-to-the-nearest-multiple-with-ties-upward
  (is (= 100d0 (geo:snap-coordinate 149d0 100d0)))
  (is (= 200d0 (geo:snap-coordinate 150d0 100d0)))
  (is (= 200d0 (geo:snap-coordinate 151d0 100d0)))
  (is (= 0d0 (geo:snap-coordinate -50d0 100d0)))
  (is (= -100d0 (geo:snap-coordinate -51d0 100d0)))
  (is (= 0d0 (geo:snap-coordinate 0d0 100d0))))

(test snap-coordinate-property-result-is-a-grid-multiple-within-half-a-step
  (let ((prng (rng:make-prng 11)))
    (dotimes (i 2000)
      (let* ((spacing (rng:prng-pick prng '(0.5d0 1d0 10d0 100d0 250d0 1000d0)))
             (value (rng:prng-next-double prng -100000d0 100000d0))
             (snapped (geo:snap-coordinate value spacing))
             (multiple (fround (/ snapped spacing))))
        (is (= snapped (* spacing multiple)))
        (is (<= (abs (- snapped value)) (+ (* 0.5d0 spacing) 1d-9)))))))

;;; Point-to-shape distances

(test closest-point-on-segment-analytic-cases
  (multiple-value-bind (x y parameter) (geo:closest-point-on-segment 5d0 3d0 0d0 0d0 10d0 0d0)
    (is (= 5d0 x)) (is (= 0d0 y)) (is (= 0.5d0 parameter)))
  (multiple-value-bind (x y parameter) (geo:closest-point-on-segment -4d0 3d0 0d0 0d0 10d0 0d0)
    (is (= 0d0 x)) (is (= 0d0 y)) (is (= 0d0 parameter)))
  (multiple-value-bind (x y parameter) (geo:closest-point-on-segment 14d0 -3d0 0d0 0d0 10d0 0d0)
    (is (= 10d0 x)) (is (= 0d0 y)) (is (= 1d0 parameter))))

(test point-segment-distance-handles-degenerate-segments
  (is (= 5d0 (geo:point-segment-distance 5d0 6d0 2d0 2d0 2d0 2d0)))
  (multiple-value-bind (x y parameter) (geo:closest-point-on-segment 5d0 6d0 2d0 2d0 2d0 2d0)
    (is (= 2d0 x)) (is (= 2d0 y)) (is (= 0d0 parameter))))

(test point-segment-distance-matches-the-textbook-formulas
  ;; Perpendicular distance to a diagonal line through the origin: |ax - by| / hypot(a, b).
  (is (approx= (/ 2d0 (sqrt 2d0)) (geo:point-segment-distance 2d0 0d0 -5d0 -5d0 5d0 5d0)))
  ;; Beyond an endpoint, the distance is the distance to that endpoint.
  (is (approx= 5d0 (geo:point-segment-distance 13d0 4d0 0d0 0d0 10d0 0d0))))

(test point-segment-distance-agrees-with-brute-force-sampling
  ;; Independent reference: minimum over 20001 points sampled along the segment. The nearest
  ;; sample is within half a sample step of the true closest point, so
  ;; computed <= reference <= computed + step / 2.
  (let ((prng (rng:make-prng 21))
        (samples 20000))
    (dotimes (i 200)
      (let* ((x1 (rng:prng-next-double prng -1000d0 1000d0))
             (y1 (rng:prng-next-double prng -1000d0 1000d0))
             (x2 (rng:prng-next-double prng -1000d0 1000d0))
             (y2 (rng:prng-next-double prng -1000d0 1000d0))
             (px (rng:prng-next-double prng -1500d0 1500d0))
             (py (rng:prng-next-double prng -1500d0 1500d0))
             (computed (geo:point-segment-distance px py x1 y1 x2 y2))
             (step (/ (geo:distance x1 y1 x2 y2) samples))
             (reference (loop for index from 0 to samples
                              for fraction = (/ index (float samples 1d0))
                              minimize (geo:distance px py
                                                     (+ x1 (* fraction (- x2 x1)))
                                                     (+ y1 (* fraction (- y2 y1)))))))
        (is (<= computed (+ reference 1d-9)))
        (is (<= reference (+ computed (* 0.5d0 step) 1d-9)))))))

(test circle-outline-distance
  (is (= 10d0 (geo:point-circle-outline-distance 0d0 0d0 0d0 0d0 10d0)))
  (is (approx= 0d0 (geo:point-circle-outline-distance 6d0 8d0 0d0 0d0 10d0)))
  (is (approx= 40d0 (geo:point-circle-outline-distance 30d0 40d0 0d0 0d0 10d0)))
  (is (approx= 3d0 (geo:point-circle-outline-distance 7d0 0d0 0d0 0d0 10d0))))

(test rectangle-outline-distance-analytic-cases
  ;; Rectangle (0,0)-(10,5).
  (is (= 2d0 (geo:point-rectangle-outline-distance 5d0 2d0 0d0 0d0 10d0 5d0)))   ; inside, bottom edge nearest
  (is (= 5d0 (geo:point-rectangle-outline-distance 13d0 9d0 0d0 0d0 10d0 5d0)))  ; outside corner 3-4-5
  (is (= 3d0 (geo:point-rectangle-outline-distance 5d0 8d0 0d0 0d0 10d0 5d0)))   ; above
  (is (= 0d0 (geo:point-rectangle-outline-distance 10d0 2d0 0d0 0d0 10d0 5d0)))  ; on the boundary
  (is (= 0d0 (geo:point-rectangle-outline-distance 0d0 0d0 0d0 0d0 10d0 5d0)))   ; corner
  (is (= 2d0 (geo:point-rectangle-outline-distance -2d0 2d0 0d0 0d0 10d0 5d0)))) ; left

(test rectangle-outline-distance-agrees-with-the-four-edges
  (let ((prng (rng:make-prng 33)))
    (dotimes (i 500)
      (let* ((min-x (rng:prng-next-double prng -500d0 0d0))
             (min-y (rng:prng-next-double prng -500d0 0d0))
             (max-x (rng:prng-next-double prng 1d0 500d0))
             (max-y (rng:prng-next-double prng 1d0 500d0))
             (px (rng:prng-next-double prng -800d0 800d0))
             (py (rng:prng-next-double prng -800d0 800d0))
             (reference (min (geo:point-segment-distance px py min-x min-y max-x min-y)
                             (geo:point-segment-distance px py max-x min-y max-x max-y)
                             (geo:point-segment-distance px py max-x max-y min-x max-y)
                             (geo:point-segment-distance px py min-x max-y min-x min-y))))
        (is (approx= reference
                     (geo:point-rectangle-outline-distance px py min-x min-y max-x max-y)))))))

(test polyline-distance-open-and-closed
  (let ((corner (make-array 6 :element-type 'double-float
                              :initial-contents '(0d0 0d0 10d0 0d0 10d0 10d0))))
    (is (= 5d0 (geo:point-polyline-distance 5d0 5d0 corner nil)))
    (is (approx= 0d0 (geo:point-polyline-distance 5d0 5d0 corner t)))     ; on the closing diagonal
    (is (= 5d0 (geo:point-polyline-distance -3d0 4d0 corner nil)))      ; nearest to the first vertex
    (is (= 0d0 (geo:point-polyline-distance 10d0 4d0 corner nil)))))

;;; Clipping

(defun clip-list (x1 y1 x2 y2 &optional (bounds (geo:make-bounds 0d0 0d0 10d0 10d0)))
  "Return the clipped segment as a list of four doubles, or NIL."
  (multiple-value-list (geo:clip-segment-to-bounds x1 y1 x2 y2 bounds)))

(test clipping-analytic-cases
  (is (equal '(2d0 2d0 8d0 8d0) (clip-list 2d0 2d0 8d0 8d0)))                ; inside
  (is (equal '(nil) (clip-list -5d0 5d0 -1d0 9d0)))                           ; outside
  (is (equal '(nil) (clip-list 11d0 0d0 11d0 10d0)))                          ; parallel, outside
  (is (equal '(0d0 5d0 10d0 5d0) (clip-list -5d0 5d0 15d0 5d0)))              ; crosses two edges
  (is (equal '(0d0 0d0 10d0 10d0) (clip-list -5d0 -5d0 15d0 15d0)))           ; diagonal through corners
  (is (equal '(0d0 0d0 0d0 10d0) (clip-list 0d0 -5d0 0d0 15d0)))              ; along the boundary
  (is (equal '(nil) (clip-list -5d0 6d0 5d0 16d0)))                           ; misses the corner
  (is (equal '(3d0 3d0 3d0 3d0) (clip-list 3d0 3d0 3d0 3d0)))                 ; point inside
  (is (equal '(nil) (clip-list 30d0 3d0 30d0 3d0))))                          ; point outside

(test clipping-keeps-far-endpoints-precise
  ;; A very long segment crossing a small window: the clipped endpoints must be exact to
  ;; double precision, not degraded by the distant endpoints.
  (let ((result (clip-list -1d7 5d0 1d7 5d0)))
    (is (equal '(0d0 5d0 10d0 5d0) result))))

(test clipping-property-result-is-inside-collinear-and-complete
  (let ((prng (rng:make-prng 55))
        (bounds (geo:make-bounds 0d0 0d0 10d0 10d0))
        (samples 2000))
    (dotimes (i 400)
      (let* ((x1 (rng:prng-next-double prng -20d0 30d0))
             (y1 (rng:prng-next-double prng -20d0 30d0))
             (x2 (rng:prng-next-double prng -20d0 30d0))
             (y2 (rng:prng-next-double prng -20d0 30d0))
             (result (clip-list x1 y1 x2 y2 bounds))
             (shrunk (geo:expand-bounds bounds -1d-9))
             (sample-inside (loop for index from 0 to samples
                                  for fraction = (/ index (float samples 1d0))
                                  thereis (geo:bounds-contains-point-p
                                           shrunk
                                           (+ x1 (* fraction (- x2 x1)))
                                           (+ y1 (* fraction (- y2 y1)))))))
        (if (first result)
            (destructuring-bind (cx1 cy1 cx2 cy2) result
              (let ((grown (geo:expand-bounds bounds 1d-9)))
                (is (geo:bounds-contains-point-p grown cx1 cy1))
                (is (geo:bounds-contains-point-p grown cx2 cy2)))
              ;; Collinear with the original: cross products vanish relative to the lengths.
              (let ((scale (max 1d0 (geo:distance x1 y1 x2 y2))))
                (is (< (abs (- (* (- x2 x1) (- cy1 y1)) (* (- y2 y1) (- cx1 x1)))) (* 1d-9 scale scale)))
                (is (< (abs (- (* (- x2 x1) (- cy2 y1)) (* (- y2 y1) (- cx2 x1)))) (* 1d-9 scale scale)))))
            (is (not sample-inside)))))))

;;; Measures

(test circle-and-rectangle-measures
  (is (approx= (* 100d0 +test-pi+) (geo:circle-area 10d0)))
  (is (approx= (* 20d0 +test-pi+) (geo:circle-circumference 10d0)))
  (is (= 600d0 (geo:rectangle-area 20d0 30d0)))
  (is (= 100d0 (geo:rectangle-perimeter 20d0 30d0)))
  (let ((bounds (geo:circle-bounds 5d0 -5d0 2d0)))
    (is (= 3d0 (geo:bounds-min-x bounds)))
    (is (= -7d0 (geo:bounds-min-y bounds)))
    (is (= 7d0 (geo:bounds-max-x bounds)))
    (is (= -3d0 (geo:bounds-max-y bounds)))))

(test polyline-length-bounds-and-vertex-count
  (let ((path (make-array 6 :element-type 'double-float
                            :initial-contents '(0d0 0d0 3d0 4d0 3d0 10d0))))
    (is (= 3 (geo:polyline-vertex-count path)))
    (is (= 11d0 (geo:polyline-length path nil)))
    (is (approx= (+ 11d0 (sqrt 109d0)) (geo:polyline-length path t)))
    (let ((bounds (geo:polyline-bounds path)))
      (is (= 0d0 (geo:bounds-min-x bounds)))
      (is (= 0d0 (geo:bounds-min-y bounds)))
      (is (= 3d0 (geo:bounds-max-x bounds)))
      (is (= 10d0 (geo:bounds-max-y bounds))))))

;;; Text extents

(test text-width-is-linear-in-characters-and-height
  (is (= 300d0 (geo:text-width 5 100d0)))
  (is (= 0d0 (geo:text-width 0 100d0)))
  (is (approx= (* 2 (geo:text-width 7 50d0)) (geo:text-width 7 100d0))))

(test oriented-box-bounds-for-axis-aligned-directions
  (let ((flat (geo:oriented-box-bounds 10d0 20d0 100d0 95d0 25d0 0d0)))
    (is (= 10d0 (geo:bounds-min-x flat)))
    (is (= -5d0 (geo:bounds-min-y flat)))
    (is (= 110d0 (geo:bounds-max-x flat)))
    (is (= 115d0 (geo:bounds-max-y flat))))
  (let ((upright (geo:oriented-box-bounds 10d0 20d0 100d0 95d0 25d0 (/ +test-pi+ 2))))
    (is (approx= -85d0 (geo:bounds-min-x upright)))
    (is (approx= 20d0 (geo:bounds-min-y upright)))
    (is (approx= 35d0 (geo:bounds-max-x upright)))
    (is (approx= 120d0 (geo:bounds-max-y upright)))))

(test oriented-box-bounds-at-45-degrees-covers-the-rotated-corners
  (let* ((bounds (geo:oriented-box-bounds 0d0 0d0 10d0 0d0 0d0 (/ +test-pi+ 4)))
         (side (/ 10d0 (sqrt 2d0))))
    (is (approx= 0d0 (geo:bounds-min-x bounds)))
    (is (approx= side (geo:bounds-max-x bounds)))
    (is (approx= side (geo:bounds-max-y bounds)))))

;;; Aligned dimensions

(defun layout-vector-equal (expected actual)
  "True when two double vectors match within the test tolerance."
  (and (= (length expected) (length actual))
       (every #'approx= expected actual)))

(test horizontal-dimension-layout-matches-hand-computed-values
  ;; p1 (0,0), p2 (1000,0), offset 500, text height 100, 4 characters.
  (let ((layout (geo:compute-dimension-layout 0d0 0d0 1000d0 0d0 500d0 100d0 4)))
    (is (layout-vector-equal #(0d0 500d0 1000d0 500d0) (geo:dimension-layout-dimension-line layout)))
    (is (layout-vector-equal #(0d0 40d0 0d0 560d0 1000d0 40d0 1000d0 560d0)
                             (geo:dimension-layout-extension-lines layout)))
    (is (layout-vector-equal #(0d0 500d0 100d0 515d0 100d0 485d0
                               1000d0 500d0 900d0 485d0 900d0 515d0)
                             (geo:dimension-layout-arrowheads layout)))
    (is (approx= 0d0 (geo:dimension-layout-text-angle layout)))
    (is (approx= 240d0 (geo:dimension-layout-text-width layout)))
    (is (approx= 380d0 (geo:dimension-layout-text-x layout)))
    (is (approx= 535d0 (geo:dimension-layout-text-y layout)))
    (let ((bounds (geo:dimension-layout-bounds layout)))
      (is (approx= 0d0 (geo:bounds-min-x bounds)))
      (is (approx= 1000d0 (geo:bounds-max-x bounds)))
      (is (approx= 40d0 (geo:bounds-min-y bounds)))
      (is (approx= 630d0 (geo:bounds-max-y bounds))))))

(test dimension-with-negative-offset-lies-on-the-right-and-text-stays-upright
  (let ((layout (geo:compute-dimension-layout 0d0 0d0 1000d0 0d0 -500d0 100d0 4)))
    (is (layout-vector-equal #(0d0 -500d0 1000d0 -500d0) (geo:dimension-layout-dimension-line layout)))
    (is (layout-vector-equal #(0d0 -40d0 0d0 -560d0 1000d0 -40d0 1000d0 -560d0)
                             (geo:dimension-layout-extension-lines layout)))
    (is (approx= 0d0 (geo:dimension-layout-text-angle layout)))
    (is (approx= -465d0 (geo:dimension-layout-text-y layout)))))

(test short-dimension-puts-arrows-outside-pointing-inward
  ;; Length 150 < 2 x arrow length (100 + 100): the arrow bodies lie outside the line.
  (let* ((layout (geo:compute-dimension-layout 0d0 0d0 150d0 0d0 300d0 100d0 3))
         (arrows (geo:dimension-layout-arrowheads layout)))
    (is (approx= 0d0 (aref arrows 0)))          ; tip of the first arrow at the line end
    (is (approx= -100d0 (aref arrows 2)))       ; its base is left of the line start
    (is (approx= 150d0 (aref arrows 6)))        ; tip of the second arrow
    (is (approx= 250d0 (aref arrows 8)))))      ; its base is right of the line end

(test dimension-text-always-reads-left-to-right
  (dolist (case '((0d0 0d0 1000d0 0d0)          ; rightward
                  (1000d0 0d0 0d0 0d0)          ; leftward: flipped
                  (0d0 0d0 0d0 1000d0)          ; upward: reads bottom to top
                  (0d0 1000d0 0d0 0d0)))        ; downward: flipped to bottom to top
    (destructuring-bind (x1 y1 x2 y2) case
      (let ((angle (geo:dimension-layout-text-angle
                    (geo:compute-dimension-layout x1 y1 x2 y2 200d0 100d0 4))))
        (is (<= (- (/ +test-pi+ 2)) (- angle 1d-12)))
        (is (<= angle (+ (/ +test-pi+ 2) 1d-12)))))))

(test dimension-layout-property-parts-are-consistent
  (let ((prng (rng:make-prng 77)))
    (dotimes (i 300)
      (let* ((x1 (rng:prng-next-double prng -5000d0 5000d0))
             (y1 (rng:prng-next-double prng -5000d0 5000d0))
             (x2 (+ x1 (rng:prng-next-double prng -3000d0 3000d0)))
             (y2 (+ y1 (rng:prng-next-double prng -3000d0 3000d0)))
             (offset (rng:prng-next-double prng -800d0 800d0))
             (height (rng:prng-next-double prng 20d0 300d0)))
        (when (> (geo:distance x1 y1 x2 y2) 1d0)
          (let* ((layout (geo:compute-dimension-layout x1 y1 x2 y2 offset height 6))
                 (line (geo:dimension-layout-dimension-line layout))
                 (arrows (geo:dimension-layout-arrowheads layout))
                 (bounds (geo:dimension-layout-bounds layout))
                 (grown (geo:expand-bounds bounds 1d-6)))
            ;; The dimension line is parallel to P1-P2 at distance |offset| and as long as it.
            (is (approx= (geo:distance x1 y1 x2 y2)
                         (geo:distance (aref line 0) (aref line 1) (aref line 2) (aref line 3))))
            (is (approx= (abs offset)
                         (geo:point-segment-distance (aref line 0) (aref line 1) x1 y1 x2 y2)))
            ;; Arrow tips coincide with the dimension-line ends.
            (is (approx= (aref line 0) (aref arrows 0)))
            (is (approx= (aref line 1) (aref arrows 1)))
            (is (approx= (aref line 2) (aref arrows 6)))
            (is (approx= (aref line 3) (aref arrows 7)))
            ;; The bounds contain every drawn point and the text origin.
            (loop for index from 0 below 12 by 2
                  do (is (geo:bounds-contains-point-p grown (aref arrows index) (aref arrows (1+ index)))))
            (is (geo:bounds-contains-point-p grown
                                             (geo:dimension-layout-text-x layout)
                                             (geo:dimension-layout-text-y layout)))))))))

;;; Touching a window (crossing selection)

(def-suite crossing-geometry
  :description "Whether a shape touches a window."
  :in all)

(in-suite crossing-geometry)

(defun window (x1 y1 x2 y2)
  "Return a window with the given corners."
  (geo:make-bounds x1 y1 x2 y2))

(test a-segment-touches-a-window-when-it-passes-through-it
  (let ((window (window 0d0 0d0 10d0 10d0)))
    (is-true (geo:segment-touches-bounds-p 2d0 2d0 5d0 5d0 window) "inside")
    (is-true (geo:segment-touches-bounds-p -5d0 5d0 15d0 5d0 window) "straight through")
    (is-true (geo:segment-touches-bounds-p -5d0 -5d0 5d0 5d0 window) "from outside to the middle")
    (is-true (geo:segment-touches-bounds-p -5d0 5d0 0d0 5d0 window) "ending on the edge")
    (is-true (geo:segment-touches-bounds-p 10d0 -5d0 10d0 15d0 window) "along an edge")
    (is-true (geo:segment-touches-bounds-p -1d0 1d0 1d0 -1d0 window) "through a corner")
    (is (not (geo:segment-touches-bounds-p -5d0 5d0 -0.001d0 5d0 window)) "just short of the edge")
    (is-true (geo:segment-touches-bounds-p -1d0 2d0 2d0 -1d0 window) "a diagonal that cuts the corner off")
    (is (not (geo:segment-touches-bounds-p -2d0 1d0 1d0 -2d0 window)) "a diagonal that passes the corner by")
    (is (not (geo:segment-touches-bounds-p 20d0 20d0 30d0 30d0 window)) "far away")))

(test a-segment-touches-a-window-exactly-when-a-point-of-it-is-in-the-window
  (let ((prng (rng:make-prng 71))
        (window (window -10d0 -5d0 12d0 8d0)))
    (dotimes (case 400)
      (let* ((x1 (rng:prng-next-double prng -30d0 30d0))
             (y1 (rng:prng-next-double prng -30d0 30d0))
             (x2 (rng:prng-next-double prng -30d0 30d0))
             (y2 (rng:prng-next-double prng -30d0 30d0))
             (nearest (loop for step from 0 to 3000
                            minimize (let ((fraction (/ step 3000d0)))
                                       (geo:point-bounds-distance (+ x1 (* fraction (- x2 x1)))
                                                                  (+ y1 (* fraction (- y2 y1)))
                                                                  window))))
             (touches (geo:segment-touches-bounds-p x1 y1 x2 y2 window)))
        (if touches
            (is (< nearest 0.05d0) "~a ~a ~a ~a: said to touch, nearest sample ~a" x1 y1 x2 y2 nearest)
            (is (> nearest 0d0) "~a ~a ~a ~a: said not to touch, but a sample is inside" x1 y1 x2 y2))))))

(test a-polyline-touches-a-window-through-any-of-its-segments-and-its-closing-one
  (let ((window (window 0d0 0d0 10d0 10d0))
        (open (make-array 6 :element-type 'double-float :initial-contents '(-5d0 -5d0 -5d0 -20d0 20d0 -20d0))))
    (is (not (geo:polyline-touches-bounds-p open nil window)) "an open polyline below the window")
    (is-true (geo:polyline-touches-bounds-p (make-array 6 :element-type 'double-float
                                                        :initial-contents '(5d0 5d0 -20d0 5d0 -20d0 -5d0))
                                            nil
                                            window)
             "the first segment reaches in")
    (let ((triangle (make-array 6 :element-type 'double-float :initial-contents '(-5d0 5d0 -5d0 -5d0 5d0 -5d0))))
      (is-true (geo:polyline-touches-bounds-p triangle t window) "only the closing segment reaches in")
      (is (not (geo:polyline-touches-bounds-p triangle nil window)) "left open, it does not"))
    (is-true (geo:polyline-touches-bounds-p (make-array 2 :element-type 'double-float :initial-contents '(5d0 5d0))
                                            nil
                                            window)
             "a single vertex inside")))

(test a-circle-touches-a-window-when-its-outline-does
  (let ((window (window 0d0 0d0 10d0 10d0)))
    (is (not (geo:circle-outline-touches-bounds-p 5d0 5d0 20d0 window)) "the window is inside the circle")
    (is-true (geo:circle-outline-touches-bounds-p 5d0 5d0 1d0 window) "the circle is inside the window")
    (is-true (geo:circle-outline-touches-bounds-p 20d0 5d0 10d0 window) "tangent to an edge")
    (is-true (geo:circle-outline-touches-bounds-p 15d0 5d0 8d0 window) "the outline cuts the window")
    (is (not (geo:circle-outline-touches-bounds-p 20d0 5d0 5d0 window)) "close but short")
    (is (not (geo:circle-outline-touches-bounds-p 50d0 50d0 5d0 window)) "far away")))

(test a-circle-touches-a-window-exactly-when-a-point-of-its-outline-is-in-it
  (let ((prng (rng:make-prng 72))
        (window (window -10d0 -5d0 12d0 8d0)))
    (dotimes (case 300)
      (let* ((cx (rng:prng-next-double prng -30d0 30d0))
             (cy (rng:prng-next-double prng -30d0 30d0))
             (radius (rng:prng-next-double prng 1d0 30d0))
             (nearest (loop for step from 0 below 4000
                            minimize (let ((angle (* 2 pi (/ step 4000d0))))
                                       (geo:point-bounds-distance (+ cx (* radius (cos angle)))
                                                                  (+ cy (* radius (sin angle)))
                                                                  window))))
             (touches (geo:circle-outline-touches-bounds-p cx cy radius window)))
        (if touches
            (is (< nearest 0.1d0) "~a ~a ~a: said to touch, nearest sample ~a" cx cy radius nearest)
            (is (> nearest 0d0) "~a ~a ~a: said not to touch, but a sample is inside" cx cy radius))))))

(test a-rotated-box-touches-a-window-through-an-edge-or-by-holding-it
  (let ((window (window 0d0 0d0 10d0 10d0)))
    (is-true (geo:oriented-box-touches-bounds-p 5d0 5d0 2d0 1d0 0.25d0 0d0 window) "inside")
    (is-true (geo:oriented-box-touches-bounds-p -5d0 5d0 8d0 1d0 0.25d0 0d0 window) "reaching in")
    (is-true (geo:oriented-box-touches-bounds-p -20d0 -20d0 60d0 40d0 40d0 0d0 window) "holding the window")
    (is (not (geo:oriented-box-touches-bounds-p -5d0 -5d0 2d0 1d0 0.25d0 0d0 window)) "beside it")
    (is-true (geo:oriented-box-touches-bounds-p -5d0 -5d0 20d0 1d0 0.25d0 (/ pi 4) window)
             "a box at 45 degrees that runs across the corner")
    (is (not (geo:oriented-box-touches-bounds-p -5d0 5d0 4d0 1d0 0.25d0 (/ pi 2) window))
        "turned upward, it misses the window")))
