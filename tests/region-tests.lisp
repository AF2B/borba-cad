(in-package #:borba-cad/tests)

(def-suite regions
  :description "Joining, cutting and overlapping regions, and hatches with holes: loops checked by their areas."
  :in all)

(in-suite regions)

;;; Helpers

(defun region-rectangle (x
                         y
                         width
                         height)
  "Return the region of a rectangle.

X: left edge.
Y: bottom edge.
WIDTH: its width.
HEIGHT: its height."
  (geo:polygon-region (make-array 8 :element-type 'double-float
                                    :initial-contents (mapcar (lambda (value) (float value 1d0))
                                                              (list x y (+ x width) y (+ x width) (+ y height)
                                                                    x (+ y height))))))

(defun region-polygon (&rest coordinates)
  "Return the region of a polygon given by its vertices.

COORDINATES: x and y, vertex after vertex."
  (geo:polygon-region (make-array (length coordinates) :element-type 'double-float
                                                       :initial-contents (mapcar (lambda (value) (float value 1d0))
                                                                                 coordinates))))

(defun region-circle (x
                      y
                      radius)
  "Return the region of a circle of many sides.

X: x of the center.
Y: y of the center.
RADIUS: the radius."
  (geo:polygon-region (geo:ellipse-points (float x 1d0) (float y 1d0) (float radius 1d0) (float radius 1d0) 0d0 96)))

(defun area-of (operation
                a
                b)
  "Return the area of the result of an operation on two regions.

OPERATION: :union, :intersection or :difference.
A: the first region.
B: the second."
  (geo:region-area (geo:region-boolean operation a b)))

(defun loops-of (operation
                 a
                 b)
  "Return the loops of the result of an operation on two regions.

OPERATION: :union, :intersection or :difference.
A: the first region.
B: the second."
  (geo:region-boolean operation a b))

(defun outer-loops (loops)
  "Return the loops that go counter-clockwise, the boundaries and not the holes.

LOOPS: a list of loops."
  (remove-if-not (lambda (loop-coordinates) (plusp (geo:contour-signed-area loop-coordinates))) loops))

(defun holes (loops)
  "Return the loops that go clockwise, the holes.

LOOPS: a list of loops."
  (remove-if-not (lambda (loop-coordinates) (minusp (geo:contour-signed-area loop-coordinates))) loops))

;;; Cases

(test two-overlapping-rectangles-join-cut-and-overlap
  (let ((a (region-rectangle 0 0 10 10))
        (b (region-rectangle 5 5 10 10)))
    (is (approx= 175d0 (area-of :union a b)))
    (is (approx= 25d0 (area-of :intersection a b)))
    (is (approx= 75d0 (area-of :difference a b)))
    (is (approx= 75d0 (area-of :difference b a)))
    (is (= 1 (length (loops-of :union a b))) "the union is one outline")
    (is (= 8 (floor (length (first (loops-of :union a b))) 2)) "of eight corners")
    (is (= 1 (length (loops-of :intersection a b))))))

(test shapes-that-do-not-meet-join-into-two-outlines-and-overlap-in-nothing
  (let ((a (region-rectangle 0 0 10 10))
        (b (region-rectangle 20 0 10 10)))
    (is (= 2 (length (loops-of :union a b))))
    (is (approx= 200d0 (area-of :union a b)))
    (is (null (loops-of :intersection a b)))
    (is (approx= 100d0 (area-of :difference a b)))
    (is (= 1 (length (loops-of :difference a b))))))

(test a-shape-inside-another-leaves-a-hole-when-taken-from-it
  (let* ((a (region-rectangle 0 0 10 10))
         (b (region-rectangle 3 3 4 4))
         (difference (loops-of :difference a b)))
    (is (= 2 (length difference)))
    (is (= 1 (length (outer-loops difference))))
    (is (= 1 (length (holes difference))))
    (is (approx= 84d0 (geo:region-area difference)))
    (is (approx= 100d0 (area-of :union a b)))
    (is (approx= 16d0 (area-of :intersection a b)))
    (is (null (loops-of :difference b a)) "nothing is left of the inner one")))

(test a-region-with-a-hole-takes-part-in-the-next-operation
  (let* ((ring (loops-of :difference (region-rectangle 0 0 10 10) (region-rectangle 3 3 4 4)))
         (bar (region-rectangle -2 4 14 2)))
    (is (approx= 100d0 (area-of :union ring bar)) "the ring, and the bar, once where they overlap: 84 + 28 - 12")
    (is (approx= 72d0 (area-of :difference ring bar)) "the ring loses the 12 that is under the bar")
    (is (= 2 (length (outer-loops (loops-of :difference ring bar)))) "the cut splits the ring into two")))

(test shapes-that-are-the-same-or-that-share-an-edge
  (let ((a (region-rectangle 0 0 10 10)))
    (is (approx= 100d0 (area-of :union a (region-rectangle 0 0 10 10))))
    (is (approx= 100d0 (area-of :intersection a (region-rectangle 0 0 10 10))))
    (is (null (loops-of :difference a (region-rectangle 0 0 10 10))) "the shape is taken from itself"))
  (let ((a (region-rectangle 0 0 10 10))
        (b (region-rectangle 10 0 10 10)))
    (is (approx= 200d0 (area-of :union a b)))
    (is (= 1 (length (loops-of :union a b))) "two squares that touch along an edge are one rectangle")
    (is (= 4 (floor (length (first (loops-of :union a b))) 2)) "with the shared corners gone")
    (is (null (loops-of :intersection a b)) "an edge has no area")
    (is (approx= 100d0 (area-of :difference a b)))))

(test shapes-that-touch-at-a-corner
  (let ((a (region-rectangle 0 0 10 10))
        (b (region-rectangle 10 10 10 10)))
    (is (approx= 200d0 (area-of :union a b)))
    (is (null (loops-of :intersection a b)))
    (is (approx= 100d0 (area-of :difference a b)))))

(test a-cut-across-leaves-two-pieces
  (let* ((a (region-rectangle 0 0 30 10))
         (cut (region-rectangle 10 -5 10 20))
         (difference (loops-of :difference a cut)))
    (is (= 2 (length difference)))
    (is (approx= 200d0 (geo:region-area difference)))
    (is (approx= 100d0 (area-of :intersection a cut)))))

(test an-l-shape-and-a-slanted-shape
  (let ((l (region-polygon 0 0 20 0 20 10 10 10 10 20 0 20))
        (diamond (region-polygon 10 -5 25 10 10 25 -5 10)))
    (is (approx= 300d0 (geo:region-area l)))
    (let ((union (area-of :union l diamond))
          (intersection (area-of :intersection l diamond)))
      (is (approx= (+ 300d0 450d0) (+ union intersection) 1d-6) "the areas add up: both together, plus what they share")
      (is (approx= (- 300d0 intersection) (area-of :difference l diamond) 1d-6)))))

(test circles-overlap-by-the-area-the-lens-has
  (let ((a (region-circle 0 0 10))
        (b (region-circle 10 0 10)))
    (let* ((lens (area-of :intersection a b))
           (exact (- (* 2 100 (acos 0.5d0)) (* 0.5d0 10 (sqrt (* 3 100))))))
      (is (< (abs (- exact lens)) (* 0.01d0 exact)) "within a hundredth of the lens: ~f against ~f" lens exact))
    (is (approx= (- (* 2 (geo:region-area a)) (area-of :intersection a b)) (area-of :union a b) 1d-6))))

(test the-loops-that-result-go-the-way-that-has-the-inside-on-the-left
  (dolist (operation '(:union :intersection :difference))
    (dolist (loop-coordinates (loops-of operation (region-rectangle 0 0 10 10) (region-rectangle 4 4 10 10)))
      (is (plusp (geo:contour-signed-area loop-coordinates)) "~a" operation)))
  (is (minusp (geo:contour-signed-area (first (holes (loops-of :difference
                                                              (region-rectangle 0 0 10 10)
                                                              (region-rectangle 3 3 2 2))))))
      "a hole goes clockwise"))

(test a-polygon-given-clockwise-is-turned-the-right-way-round
  (let ((clockwise (region-polygon 0 0 0 10 10 10 10 0)))
    (is (plusp (geo:region-area clockwise)))
    (is (approx= 100d0 (geo:region-area clockwise)))))

(test a-point-is-inside-a-region-by-how-many-times-the-loops-go-round-it
  (let ((ring (loops-of :difference (region-rectangle 0 0 10 10) (region-rectangle 3 3 4 4))))
    (is (= 1 (geo:region-winding-number 1d0 1d0 ring)))
    (is (= 0 (geo:region-winding-number 5d0 5d0 ring)) "in the hole")
    (is (= 0 (geo:region-winding-number 20d0 5d0 ring)))))

;;; Properties, over many shapes

(defun random-convex-region (prng)
  "Return a region that is a regular polygon, or a rectangle, of random size, place and turn.

PRNG: the random number generator."
  (let ((x (rng:prng-next-double prng -20d0 20d0))
        (y (rng:prng-next-double prng -20d0 20d0)))
    (if (rng:prng-next-boolean prng)
        (geo:polygon-region (geo:regular-polygon-vertices x
                                                         y
                                                         (rng:prng-next-double prng 4d0 18d0)
                                                         (+ 3 (rng:prng-next-below prng 9))
                                                         (rng:prng-next-double prng 0d0 6.28d0)))
        (let* ((width (rng:prng-next-double prng 4d0 30d0))
               (height (rng:prng-next-double prng 4d0 30d0))
               (angle (rng:prng-next-double prng 0d0 3.14d0))
               (cosine (cos angle))
               (sine (sin angle)))
          (geo:polygon-region
           (make-array 8 :element-type 'double-float
                         :initial-contents
                         (loop for (corner-x corner-y) in `((0d0 0d0) (,width 0d0) (,width ,height) (0d0 ,height))
                               append (list (+ x (- (* corner-x cosine) (* corner-y sine)))
                                            (+ y (* corner-x sine) (* corner-y cosine))))))))))

(test the-areas-of-the-three-operations-add-up-for-any-two-shapes
  (let ((prng (rng:make-prng 20261019)))
    (dotimes (case 300)
      (let* ((a (random-convex-region prng))
             (b (random-convex-region prng))
             (area-a (geo:region-area a))
             (area-b (geo:region-area b))
             (union (area-of :union a b))
             (intersection (area-of :intersection a b))
             (difference (area-of :difference a b)))
        (is (approx= (+ area-a area-b) (+ union intersection) 1d-6) "union and intersection, case ~d" case)
        (is (approx= (- area-a intersection) difference 1d-6) "difference, case ~d" case)))))

(test the-inside-of-the-result-is-what-the-operation-says-at-every-point
  (let ((prng (rng:make-prng 7)))
    (dotimes (case 120)
      (let* ((a (random-convex-region prng))
             (b (random-convex-region prng))
             (union (loops-of :union a b))
             (intersection (loops-of :intersection a b))
             (difference (loops-of :difference a b)))
        (dotimes (sample 40)
          (let* ((x (rng:prng-next-double prng -40d0 40d0))
                 (y (rng:prng-next-double prng -40d0 40d0))
                 (in-a (/= 0 (geo:region-winding-number x y a)))
                 (in-b (/= 0 (geo:region-winding-number x y b))))
            (is (eq (or in-a in-b) (/= 0 (geo:region-winding-number x y union))) "union at ~f ~f, case ~d" x y case)
            (is (eq (and in-a in-b) (/= 0 (geo:region-winding-number x y intersection)))
                "intersection at ~f ~f, case ~d" x y case)
            (is (eq (and in-a (not in-b)) (/= 0 (geo:region-winding-number x y difference)))
                "difference at ~f ~f, case ~d" x y case)))))))

(test chaining-operations-keeps-the-areas-true
  (let ((prng (rng:make-prng 99)))
    (dotimes (case 100)
      (let* ((a (random-convex-region prng))
             (b (random-convex-region prng))
             (c (random-convex-region prng))
             (without-b (loops-of :difference a b))
             (without-both (geo:region-boolean :difference without-b c)))
        (is (approx= (geo:region-area (geo:region-boolean :difference a (geo:region-boolean :union b c)))
                     (geo:region-area without-both)
                     1d-6)
            "A - B - C is A - (B + C), case ~d" case)))))

(test shapes-on-a-grid-meet-in-every-way-they-can
  (dolist (dx '(-10 -5 0 5 10))
    (dolist (dy '(-10 -5 0 5 10))
      (let ((a (region-rectangle 0 0 10 10))
            (b (region-rectangle dx dy 10 10)))
        (let ((union (area-of :union a b))
              (intersection (area-of :intersection a b)))
          (is (approx= 200d0 (+ union intersection) 1d-9) "offset ~d ~d" dx dy)
          (is (approx= (- 100d0 intersection) (area-of :difference a b) 1d-9) "difference at ~d ~d" dx dy))))))

;;; Hatches with holes

(defun square-coordinates (x
                           y
                           side)
  "Return the vertices of a square as a flat array.

X: left edge.
Y: bottom edge.
SIDE: its side."
  (make-array 8 :element-type 'double-float
                :initial-contents (mapcar (lambda (value) (float value 1d0))
                                          (list x y (+ x side) y (+ x side) (+ y side) x (+ y side)))))

(defun holed-hatch (&key (pattern :lines)
                         (holes (list (square-coordinates 3 3 4))))
  "Return a hatch of a square 10 across with holes.

PATTERN: :solid, :lines or :cross.
HOLES: the holes, a list of flat arrays."
  (dom:make-hatch :id 1
                  :layer-id 0
                  :coordinates (square-coordinates 0 0 10)
                  :holes holes
                  :pattern pattern
                  :angle 0d0
                  :spacing 1d0))

(defun covered-area (triangles)
  "Return the area that triangles cover, given as six numbers each.

TRIANGLES: a flat array."
  (loop for index from 0 below (length triangles) by 6
        sum (let ((x1 (aref triangles index))
                  (y1 (aref triangles (+ index 1)))
                  (x2 (aref triangles (+ index 2)))
                  (y2 (aref triangles (+ index 3)))
                  (x3 (aref triangles (+ index 4)))
                  (y3 (aref triangles (+ index 5))))
              (/ (abs (- (* (- x2 x1) (- y3 y1)) (* (- x3 x1) (- y2 y1)))) 2d0))))

(defun hatch-area (hatch)
  "Return the area that the inspector shows for a hatch.

HATCH: a hatch."
  (dom:derived-value-value (find "Area"
                                 (dom:entity-derived-values hatch)
                                 :key #'dom:derived-value-label
                                 :test #'string=)))

(test a-hatch-leaves-its-holes-unfilled
  (let* ((hatch (holed-hatch))
         (segments (dom:hatch-segments hatch)))
    (is (= 1 (length (dom:hatch-holes hatch))))
    (is (every (lambda (index)
                 (let ((y (aref segments (1+ index))))
                   (or (<= y 3d0)
                       (>= y 7d0)
                       (and (<= (aref segments index) 3d0) (<= (aref segments (+ index 2)) 3d0))
                       (and (>= (aref segments index) 7d0) (>= (aref segments (+ index 2)) 7d0)))))
               (loop for index from 0 below (length segments) by 4 collect index))
        "no line runs through the hole")
    (is (> (length segments) (length (dom:hatch-segments (holed-hatch :holes '()))))
        "the lines are cut in two there")))

(test a-solid-hatch-with-a-hole-covers-the-area-without-the-hole
  (let ((hatch (holed-hatch :pattern :solid)))
    (is (approx= 84d0 (covered-area (dom:hatch-triangles hatch)) 1d-9))
    (is (approx= 84d0 (hatch-area hatch)))
    (is (approx= 100d0 (covered-area (dom:hatch-triangles (holed-hatch :pattern :solid :holes '())))))))

(test the-picking-of-a-solid-hatch-goes-through-its-hole
  (let ((hatch (holed-hatch :pattern :solid)))
    (is (zerop (dom:entity-pick-distance hatch 1d0 1d0)))
    (is (plusp (dom:entity-pick-distance hatch 5d0 5d0)) "the hole is not the hatch")))

(test a-hatch-with-holes-moves-turns-and-scales-with-its-holes
  (let* ((hatch (holed-hatch))
         (moved (dom:entity-translated hatch 100d0 50d0))
         (scaled (dom:entity-scaled hatch 0d0 0d0 2d0))
         (turned (dom:entity-rotated hatch 0d0 0d0 (/ pi 2))))
    (is (= 103d0 (aref (first (dom:hatch-holes moved)) 0)))
    (is (= 6d0 (aref (first (dom:hatch-holes scaled)) 0)))
    (is (approx= -3d0 (aref (first (dom:hatch-holes turned)) 0) 1d-9) "a quarter turn sends (3, 3) to (-3, 3)")
    (is (approx= 3d0 (aref (first (dom:hatch-holes turned)) 1) 1d-9))
    (is (approx= (hatch-area hatch) (hatch-area turned)))))

(test a-hole-must-be-a-shape
  (signals base:invalid-entity
    (holed-hatch :holes (list (make-array 4 :element-type 'double-float :initial-contents '(1d0 1d0 2d0 2d0)))))
  (signals base:invalid-entity
    (holed-hatch :holes (list (make-array 6 :element-type 'double-float
                                            :initial-contents '(1d0 1d0 2d0 2d0 3d0 3d0))))))

(test a-hatch-with-holes-is-saved-and-read-back-with-them
  (let ((drawing (dom:make-drawing)))
    (dom:apply-changes drawing (dom:create-entity-changes (holed-hatch)))
    (let* ((text (persist:drawing-to-string drawing))
           (again (persist:parse-drawing-text text))
           (hatch (first (dom:drawing-entities again))))
      (is (search "(hole-sizes 4.0)" text))
      (is (= 1 (length (dom:hatch-holes hatch))))
      (is (equalp (square-coordinates 3 3 4) (first (dom:hatch-holes hatch))))
      (is (string= text (persist:drawing-to-string again)) "it is stable"))))

(test the-hatch-tool-leaves-out-the-shapes-inside-the-one-it-fills
  (let ((drawing (dom:make-drawing)))
    (dom:apply-changes drawing
                       (dom:create-entities-changes
                        (list (dom:make-rectangle :id 1 :layer-id 0 :min-x 0d0 :min-y 0d0 :width 100d0 :height 100d0)
                              (dom:make-circle :id 2 :layer-id 0 :center-x 30d0 :center-y 30d0 :radius 10d0)
                              (dom:make-rectangle :id 3 :layer-id 0 :min-x 60d0 :min-y 60d0 :width 20d0 :height 20d0)
                              (dom:make-rectangle :id 4 :layer-id 0 :min-x 200d0 :min-y 0d0 :width 5d0 :height 5d0))))
    (multiple-value-bind (boundary holes) (dom:hatch-region-at drawing 5d0 5d0)
      (is (= 8 (length boundary)) "the click is in the big rectangle")
      (is (= 2 (length holes)) "the circle and the small square are left out, the one far away is not")
      (let ((changes (dom:hatch-changes drawing 5d0 5d0 :spacing 4d0)))
        (is (= 2 (length (dom:hatch-holes (dom:change-after (first changes))))))))
    (multiple-value-bind (boundary holes) (dom:hatch-region-at drawing 30d0 30d0)
      (is (= 128 (floor (length boundary) 2)) "a click inside the circle hatches the circle")
      (is (null holes)))))

;;; The tools and the console

(defun two-squares ()
  "Return two squares that overlap, ids 1 and 2."
  (list (spread-rectangle 1 -500 -500 1000 1000)
        (spread-rectangle 2 0 0 1000 1000)))

(test union-joins-the-selected-shapes-into-one-outline-and-is-one-step-of-undo
  (let ((application (apply #'reshaping-app (two-squares))))
    (app:operation-select-all application)
    (choose-modify-tool-by-menu application :union)
    (is (= 1 (dom:drawing-entity-count (app:app-drawing application))))
    (let ((outline (first (entities-of application))))
      (is (eq :polyline (dom:entity-kind outline)))
      (is (dom:polyline-closed outline))
      (is (approx= 1750000d0 (geo:contour-area (dom:polyline-coordinates outline)))))
    (is (= 1 (hash-table-count (app:app-selection application))) "the result is selected")
    (is (search "Joined 2 shapes" (message-text application)))
    (app:operation-undo application)
    (is (equal '(:rectangle :rectangle) (mapcar #'dom:entity-kind (entities-of application))))))

(test intersect-keeps-where-the-selected-shapes-overlap
  (let ((application (apply #'reshaping-app (two-squares))))
    (app:operation-select-all application)
    (press application :i :shift)
    (is (= 1 (dom:drawing-entity-count (app:app-drawing application))))
    (is (approx= 250000d0 (geo:contour-area (dom:polyline-coordinates (first (entities-of application))))))
    (is (search "Kept where 2 shapes overlap" (message-text application)))))

(test shapes-that-do-not-overlap-say-so-and-change-nothing
  (let ((application (reshaping-app (spread-rectangle 1 0 0 100 100) (spread-rectangle 2 500 500 100 100))))
    (app:operation-select-all application)
    (press application :i :shift)
    (is (search "do not overlap" (message-text application)))
    (is (= 2 (dom:drawing-entity-count (app:app-drawing application))))))

(test union-and-intersect-need-two-closed-shapes
  (let ((application (reshaping-app (spread-rectangle 1 0 0 100 100) (cut-line 2 0 0 500 500))))
    (press application :u :shift)
    (is (search "Choose at least two closed shapes" (message-text application)) "nothing is selected")
    (app:operation-select-all application)
    (press application :u :shift)
    (is (search "Choose at least two closed shapes" (message-text application)) "a line is not a closed shape")
    (is (= 2 (dom:drawing-entity-count (app:app-drawing application))))))

(test subtract-cuts-the-shapes-clicked-away-from-the-first
  (let ((application (apply #'reshaping-app (two-squares))))
    (choose-modify-tool-by-menu application :subtract)
    (is (eq :subtract (app:tool-kind (app:app-tool application))))
    (is (search "click the shape to cut from" (app:tool-prompt (app:app-tool application) application)))
    (click application -400 -400)
    (is (= 1 (hash-table-count (app:app-selection application))) "the first is shown by being selected")
    (is (search "0 shapes to cut away" (app:tool-prompt (app:app-tool application) application)))
    (click application 800 800)
    (is (search "1 shape to cut away" (app:tool-prompt (app:app-tool application) application)))
    (is (= 2 (dom:drawing-entity-count (app:app-drawing application))) "nothing changes until Enter")
    (press application :enter)
    (is (= 1 (dom:drawing-entity-count (app:app-drawing application))))
    (is (approx= 750000d0 (geo:contour-area (dom:polyline-coordinates (first (entities-of application))))))
    (is (eq :select (app:tool-kind (app:app-tool application))))
    (app:operation-undo application)
    (is (= 2 (dom:drawing-entity-count (app:app-drawing application))))))

(test subtract-shows-the-result-before-it-is-made
  (let ((application (apply #'reshaping-app (two-squares))))
    (choose-modify-tool-by-menu application :subtract)
    (click application -400 -400)
    (multiple-value-bind (x y) (screen-position application 800 800)
      (send application (app:make-pointer-event :move nil x y)))
    (let ((preview (app:tool-preview-entities (app:app-tool application) application)))
      (is (= 1 (length preview)))
      (is (eq :polyline (dom:entity-kind (first preview)))))))

(test subtract-says-what-is-wrong-and-escape-forgets-the-first-shape
  (let ((application (apply #'reshaping-app (cut-line 3 2000 0 3000 0) (two-squares))))
    (choose-modify-tool-by-menu application :subtract)
    (click application 2500 0)
    (is (search "Click a rectangle" (message-text application)) "a line is not a closed shape")
    (click application -400 -400)
    (press application :enter)
    (is (search "Click the shapes to cut away first" (message-text application)))
    (click application -400 -400)
    (is (search "already part" (message-text application)))
    (press application :escape)
    (is (not (app:tool-in-progress-p (app:app-tool application))))
    (is (= 3 (dom:drawing-entity-count (app:app-drawing application))))))

(test taking-a-shape-from-one-it-lies-in-leaves-an-outline-and-a-hole
  (let ((application (reshaping-app (spread-rectangle 1 -1000 -1000 2000 2000)
                                    (spread-rectangle 2 -300 -300 600 600))))
    (choose-modify-tool-by-menu application :subtract)
    (click application -900 -900)
    (click application 0 0)
    (press application :enter)
    (is (= 2 (dom:drawing-entity-count (app:app-drawing application))) "the outline and the hole")
    (is (= 2 (hash-table-count (app:app-selection application))))))

(test the-console-joins-cuts-and-overlaps
  (let* ((drawing (dom:make-drawing))
         (context (script-env drawing)))
    (ran-ok context "(rect-size 0 0 100 100) (rect-size 50 50 100 100) (rect-size 0 200 10 10)")
    (is (equal '("(4)") (ran-ok context "(region-union '(1 2))")))
    (is (= 2 (dom:drawing-entity-count drawing)) "the squares were replaced by the outline")
    (is (search "do not overlap" (failure-of context "(region-intersect '(3 4))")))
    (is (equal '("(5)") (ran-ok context "(region-subtract 4 3)")) "the small square is far away: it only goes")
    (is (search "at least two" (failure-of context "(region-union 5)")))))
