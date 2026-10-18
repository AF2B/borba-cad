(in-package #:borba-cad/tests)

(def-suite hatching
  :description "Hatches and solid fills: the geometry, the entity, the file, the tool and the console."
  :in all)

(in-suite hatching)

;;; Helpers

(defun square (&optional (side 1000d0))
  "Return the vertices of a square from the origin, counter-clockwise, as a flat array."
  (doubles 0d0 0d0 side 0d0 side side 0d0 side))

(defun l-shape ()
  "Return the vertices of an L, counter-clockwise: a 1000 by 1000 square with the top right 500 by 500 taken out."
  (doubles 0d0 0d0 1000d0 0d0 1000d0 500d0 500d0 500d0 500d0 1000d0 0d0 1000d0))

(defun segment-count (segments)
  "Return how many segments a flat array holds."
  (floor (length segments) 4))

(defun triangle-area-sum (triangles)
  "Return the total area of the triangles of a flat array of six numbers each."
  (loop for index from 0 below (length triangles) by 6
        sum (/ (abs (- (* (- (aref triangles (+ index 2)) (aref triangles index))
                          (- (aref triangles (+ index 5)) (aref triangles (+ index 1))))
                       (* (- (aref triangles (+ index 4)) (aref triangles index))
                          (- (aref triangles (+ index 3)) (aref triangles (+ index 1))))))
               2)))

(defun hatch-near-p (expected actual &optional (tolerance 1d-6))
  "Return true when two numbers agree to TOLERANCE."
  (< (abs (- expected actual)) tolerance))

;;; Inside and outside

(test a-point-is-in-a-polygon-by-the-even-odd-rule
  (let ((square (square)))
    (is-true (geo:point-in-polygon-p 500d0 500d0 square))
    (is-false (geo:point-in-polygon-p 1500d0 500d0 square))
    (is-false (geo:point-in-polygon-p -1d0 500d0 square))
    (is-false (geo:point-in-polygon-p 500d0 2000d0 square)))
  (let ((l (l-shape)))
    (is-true (geo:point-in-polygon-p 250d0 750d0 l) "the arm of the L")
    (is-true (geo:point-in-polygon-p 750d0 250d0 l) "the foot")
    (is-false (geo:point-in-polygon-p 750d0 750d0 l) "the corner that was taken out"))
  (is-true (geo:point-in-polygon-p 500d0 500d0 (doubles 0d0 1000d0 1000d0 1000d0 1000d0 0d0 0d0 0d0))
           "clockwise is the same"))

(test the-signed-area-says-which-way-a-polygon-goes
  (is (= 1000000d0 (geo:contour-signed-area (square))))
  (is (= -1000000d0 (geo:contour-signed-area (doubles 0d0 0d0 0d0 1000d0 1000d0 1000d0 1000d0 0d0))))
  (is (= 750000d0 (geo:contour-signed-area (l-shape)))))

;;; Lines

(test lines-cut-by-a-square-run-from-side-to-side
  (let* ((segments (geo:hatch-segments (square) 0d0 100d0 0d0 0d0)))
    (is (= 9 (segment-count segments)) "100 apart, not on the edges: 100 to 900")
    (loop for index from 0 below (length segments) by 4
          do (is (= (aref segments (+ index 1)) (aref segments (+ index 3))) "horizontal")
             (is (hatch-near-p 0d0 (min (aref segments index) (aref segments (+ index 2)))))
             (is (hatch-near-p 1000d0 (max (aref segments index) (aref segments (+ index 2))))))))

(test lines-follow-the-angle
  (let ((segments (geo:hatch-segments (square) (/ pi 2) 250d0 0d0 0d0)))
    (is (= 3 (segment-count segments)))
    (loop for index from 0 below (length segments) by 4
          do (is (hatch-near-p (aref segments index) (aref segments (+ index 2))) "vertical")))
  (let ((segments (geo:hatch-segments (square) (/ pi 4) 100d0 0d0 0d0)))
    (is (plusp (segment-count segments)))
    (loop for index from 0 below (length segments) by 4
          do (let ((dx (- (aref segments (+ index 2)) (aref segments index)))
                   (dy (- (aref segments (+ index 3)) (aref segments (+ index 1)))))
               (is (hatch-near-p (abs dx) (abs dy) 1d-6) "at 45 degrees")))))

(test lines-stay-inside-a-concave-polygon
  (let ((segments (geo:hatch-segments (l-shape) 0d0 100d0 0d0 0d0)))
    (loop for index from 0 below (length segments) by 4
          do (let ((x1 (aref segments index))
                   (y (aref segments (+ index 1)))
                   (x2 (aref segments (+ index 2))))
               (is (geo:point-in-polygon-p (/ (+ x1 x2) 2) y (l-shape)) "the middle of each is inside")
               (when (> y 500d0)
                 (is (hatch-near-p 500d0 (- x2 x1)) "above 500 the L is only 500 wide")))
          )
    (is (= 9 (segment-count segments)) "one for each level: the L is one piece at every height")))

(test two-pieces-at-one-level-give-two-segments
  (let ((u (doubles 0d0 0d0 1000d0 0d0 1000d0 1000d0 700d0 1000d0 700d0 350d0 300d0 350d0 300d0 1000d0 0d0 1000d0)))
    (let ((segments (geo:hatch-segments u 0d0 100d0 0d0 0d0)))
      (is (= (+ 3 (* 2 6)) (segment-count segments)) "3 levels below the notch, 2 pieces at each of the 6 above"))))

(test the-pattern-is-aligned-to-the-origin-it-is-given
  (let ((aligned (geo:hatch-segments (square) 0d0 300d0 0d0 0d0))
        (shifted (geo:hatch-segments (square) 0d0 300d0 0d0 50d0)))
    (is (hatch-near-p 300d0 (aref aligned 1)))
    (is (hatch-near-p 50d0 (aref shifted 1)) "a line passes through the origin y of 50")))

(test the-lines-of-a-region-are-counted-before-they-are-made
  (is (= 11 (geo:hatch-line-count (square) 0d0 100d0 0d0 0d0)))
  (is (= 4 (geo:hatch-line-count (square) 0d0 333d0 0d0 0d0)))
  (is (< 1000 (geo:hatch-line-count (square 1d6) 0d0 100d0 0d0 0d0))))

;;; Triangles

(test a-solid-region-is-cut-into-triangles-that-cover-it-exactly
  (dolist (polygon (list (square) (l-shape)
                         (doubles 0d0 0d0 1000d0 0d0 500d0 800d0)
                         (doubles 0d0 1000d0 1000d0 1000d0 1000d0 0d0 0d0 0d0)
                         (doubles 0d0 0d0 400d0 0d0 400d0 300d0 700d0 300d0
                                  700d0 0d0 1100d0 0d0 1100d0 1000d0 0d0 1000d0)))
    (let ((triangles (geo:triangulate-polygon polygon)))
      (is (plusp (length triangles)))
      (is (zerop (mod (length triangles) 6)))
      (is (hatch-near-p (geo:contour-area polygon) (triangle-area-sum triangles) 1d-6)
          "the triangles add up to the area of ~a" polygon))))

(test a-circle-of-many-sides-is-triangulated-quickly-and-completely
  (let* ((points (geo:ellipse-points 0d0 0d0 1000d0 1000d0 0d0 128))
         (triangles (geo:triangulate-polygon points)))
    (is (= (* 6 126) (length triangles)) "n-2 triangles")
    (is (hatch-near-p (geo:contour-area points) (triangle-area-sum triangles) 1d-6))))

(test a-corner-that-is-straight-adds-no-sliver
  (let ((triangles (geo:triangulate-polygon (doubles 0d0 0d0 500d0 0d0 1000d0 0d0 1000d0 1000d0 0d0 1000d0))))
    (is (<= (length triangles) (* 6 3)) "at most n-2 triangles")
    (is (hatch-near-p 1000000d0 (triangle-area-sum triangles)))
    (loop for index from 0 below (length triangles) by 6
          do (is (< 1d0 (triangle-area-sum (subseq triangles index (+ index 6)))) "no triangle without area"))))

;;; The entity

(defun a-hatch (&key (id 1) (coordinates (square)) (pattern :lines) (angle 0d0) (spacing 100d0))
  "Return a hatch of a square from the origin."
  (dom:make-hatch :id id :layer-id 0 :coordinates coordinates :pattern pattern :angle angle :spacing spacing))

(test a-hatch-keeps-its-boundary-pattern-angle-and-spacing
  (let ((hatch (a-hatch :angle (+ pi 0.5d0) :pattern :cross :spacing 125d0)))
    (is (eq :hatch (dom:entity-kind hatch)))
    (is (eq :cross (dom:hatch-pattern hatch)))
    (is (hatch-near-p 0.5d0 (dom:hatch-angle hatch)) "a line has no sense, so the angle is kept in [0, pi)")
    (is (= 125d0 (dom:hatch-spacing hatch)))
    (is (= 8 (length (dom:hatch-coordinates hatch))))
    (is (hatch-near-p 1000000d0 (dom:derived-value-value (first (dom:entity-derived-values hatch)))))
    (is (equalp (dom:entity-bounds hatch) (geo:make-bounds 0d0 0d0 1000d0 1000d0)))))

(test the-lines-a-hatch-draws-depend-on-its-pattern
  (let ((lines (a-hatch :pattern :lines :angle 0d0 :spacing 100d0))
        (cross (a-hatch :pattern :cross :angle 0d0 :spacing 100d0))
        (solid (a-hatch :pattern :solid)))
    (is (= 9 (segment-count (dom:hatch-segments lines))))
    (is (zerop (length (dom:hatch-triangles lines))))
    (is (= 18 (segment-count (dom:hatch-segments cross))) "two families at right angles")
    (is (zerop (length (dom:hatch-segments solid))))
    (is (hatch-near-p 1000000d0 (triangle-area-sum (dom:hatch-triangles solid))))))

(test a-hatch-refuses-what-cannot-be-filled
  (signals-invalid-entity :hatch (a-hatch :coordinates (doubles 0d0 0d0 100d0 0d0)))
  (signals-invalid-entity :hatch (a-hatch :coordinates (doubles 0d0 0d0 100d0 0d0 200d0 0d0)) "no area")
  (signals-invalid-entity :hatch (a-hatch :coordinates (doubles 0d0 0d0 100d0 0d0 100d0)))
  (signals-invalid-entity :hatch (a-hatch :pattern :dots))
  (signals-invalid-entity :hatch (a-hatch :spacing 0d0))
  (signals-invalid-entity :hatch (a-hatch :spacing 0.1d0) "too many lines for the size")
  (signals-invalid-entity :hatch (a-hatch :angle 1d9))
  (signals-invalid-entity :hatch (a-hatch :coordinates (square (* 2 base:+maximum-coordinate-magnitude+))))
  (signals-invalid-entity :hatch (a-hatch :id 0))
  (is (a-hatch :spacing (/ 1000d0 (- dom:+maximum-hatch-lines+ 2))) "the limit is on the number of lines")
  (is (a-hatch :pattern :solid :spacing 0.001d0) "a solid hatch makes no lines, so its spacing does not matter"))

(test the-inspector-edits-the-pattern-the-angle-and-the-spacing
  (let ((hatch (a-hatch)))
    (is (equal '(:pattern :angle :spacing) (mapcar #'dom:property-key (dom:entity-properties hatch))))
    (is (eq :solid (dom:hatch-pattern (dom:entity-with-property hatch :pattern :solid))))
    (is (hatch-near-p 1d0 (dom:hatch-angle (dom:entity-with-property hatch :angle 1d0))))
    (is (= 200d0 (dom:hatch-spacing (dom:entity-with-property hatch :spacing 200d0))))
    (signals base:invalid-entity (dom:entity-with-property hatch :spacing 0d0))))

(test a-hatch-moves-with-the-pattern-attached-to-its-first-vertex
  (let* ((hatch (a-hatch :coordinates (square) :angle 0d0 :spacing 300d0))
         (moved (dom:entity-translated hatch 50d0 70d0)))
    (is (= 50d0 (aref (dom:hatch-coordinates moved) 0)))
    (is (= 70d0 (aref (dom:hatch-coordinates moved) 1)))
    (is (= (segment-count (dom:hatch-segments hatch)) (segment-count (dom:hatch-segments moved))))
    (is (hatch-near-p (+ 70d0 300d0) (aref (dom:hatch-segments moved) 1))
        "the first line is still 300 from the vertex")))

(test a-hatch-is-picked-on-its-lines-or-its-boundary-and-solid-all-over
  (let ((lines (a-hatch :spacing 250d0 :angle 0d0))
        (solid (a-hatch :pattern :solid)))
    (is (= 0d0 (dom:entity-pick-distance lines 500d0 250d0)) "on a line")
    (is (< (dom:entity-pick-distance lines 500d0 300d0) 51d0) "between two lines")
    (is (= 0d0 (dom:entity-pick-distance lines 500d0 0d0)) "on the boundary")
    (is (= 0d0 (dom:entity-pick-distance solid 500d0 500d0)) "anywhere inside a solid one")
    (is (< 0d0 (dom:entity-pick-distance solid 1500d0 500d0)))
    (is (dom:entity-touches-bounds-p lines (geo:make-bounds -10d0 400d0 10d0 600d0)) "a window across an edge")
    (is (dom:entity-touches-bounds-p solid (geo:make-bounds 400d0 400d0 600d0 600d0)) "a window inside")
    (is (not (dom:entity-touches-bounds-p solid (geo:make-bounds 2000d0 2000d0 2100d0 2100d0))))))

(test a-hatch-has-grips-at-its-vertices-and-offers-no-snap-points
  (let* ((hatch (a-hatch))
         (grips (dom:entity-grips hatch)))
    (is (= 4 (length grips)))
    (let ((stretched (dom:entity-with-grip hatch (third grips) 1200d0 1100d0)))
      (is (= 1200d0 (aref (dom:hatch-coordinates stretched) 4)))
      (is (= 1100d0 (aref (dom:hatch-coordinates stretched) 5))))
    (is (null (dom:entity-snap-points hatch)) "the shape it fills has them")))

(test rotating-mirroring-and-scaling-a-hatch-takes-its-pattern-along
  (let* ((hatch (a-hatch :angle 0d0 :spacing 100d0))
         (turned (dom:entity-rotated hatch 0d0 0d0 (/ pi 2))))
    (is (hatch-near-p (/ pi 2) (dom:hatch-angle turned)))
    (is (hatch-near-p 0d0 (aref (dom:hatch-coordinates turned) 2) 1d-6) "the vertex (1000, 0) went to (0, 1000)")
    (is (hatch-near-p 1000d0 (aref (dom:hatch-coordinates turned) 3) 1d-6)))
  (let ((mirrored (dom:entity-mirrored (a-hatch :angle 0.3d0) 0d0 0d0 1d0 0d0)))
    (is (hatch-near-p (- pi 0.3d0) (dom:hatch-angle mirrored))
        "lines at 0.3 radians mirrored in the x axis are at -0.3, which is pi-0.3"))
  (let ((scaled (dom:entity-scaled (a-hatch :spacing 100d0) 0d0 0d0 2d0)))
    (is (= 200d0 (dom:hatch-spacing scaled)))
    (is (= 2000d0 (aref (dom:hatch-coordinates scaled) 2)))))

;;; Finding the region

(defun region-drawing ()
  "Return a drawing with a big rectangle, a circle inside it, and a closed polyline beside them."
  (let ((drawing (dom:make-drawing)))
    (dom:apply-changes drawing
                       (dom:create-entities-changes
                        (list (dom:make-rectangle :id 1 :layer-id 0 :min-x 0d0 :min-y 0d0 :width 4000d0 :height 3000d0)
                              (dom:make-circle :id 2 :layer-id 0 :center-x 1000d0 :center-y 1500d0 :radius 600d0)
                              (dom:make-polyline :id 3 :layer-id 0 :closed t
                                                 :coordinates (doubles 5000d0 0d0 6000d0 0d0 5500d0 900d0))
                              (dom:make-polyline :id 4 :layer-id 0 :closed nil
                                                 :coordinates (doubles 7000d0 0d0 8000d0 0d0 8000d0 1000d0))
                              (dom:make-ellipse :id 5 :layer-id 0 :center-x 3000d0 :center-y 1500d0 :radius-x 500d0
                                                :radius-y 300d0))))
    drawing))

(test the-region-is-the-smallest-closed-shape-that-holds-the-point
  (let ((drawing (region-drawing)))
    (is (hatch-near-p (* pi 600d0 600d0) (geo:contour-area (dom:hatch-region-at drawing 1000d0 1500d0)) 3d4)
        "inside the circle, which is inside the rectangle")
    (is (hatch-near-p 12000000d0 (geo:contour-area (dom:hatch-region-at drawing 2000d0 500d0))) "the rectangle")
    (is (hatch-near-p 450000d0 (geo:contour-area (dom:hatch-region-at drawing 5500d0 300d0))) "the closed polyline")
    (is (dom:hatch-region-at drawing 3000d0 1500d0) "the ellipse")
    (is (null (dom:hatch-region-at drawing 4500d0 500d0)) "outside everything")
    (is (null (dom:hatch-region-at drawing 7800d0 300d0)) "an open polyline is not a region")))

(test a-hidden-layer-is-not-a-region
  (let ((drawing (region-drawing)))
    (dom:set-layer-visible drawing 0 nil)
    (is (null (dom:hatch-region-at drawing 2000d0 500d0)))))

(test the-hatch-change-set-adds-a-hatch-on-the-current-layer
  (let* ((drawing (region-drawing))
         (layer (dom:add-layer drawing "fills" (dom:make-rgb 5 6 7))))
    (dom:set-current-layer drawing (dom:layer-id layer))
    (let* ((changes (dom:hatch-changes drawing 2000d0 500d0 :pattern :cross :angle (/ pi 3)))
           (hatch (dom:change-after (first changes))))
      (is (= 1 (length changes)))
      (is (= 6 (dom:entity-id hatch)))
      (is (= (dom:layer-id layer) (dom:entity-layer-id hatch)))
      (is (eq :cross (dom:hatch-pattern hatch)))
      (is (= 125d0 (dom:hatch-spacing hatch)) "half the default text height of 250"))
    (handler-case (progn (dom:hatch-changes drawing 4500d0 500d0)
                         (fail "a point in no region was hatched"))
      (base:edit-refused (condition)
        (is (search "No closed shape" (princ-to-string condition)))))))

(test the-default-spacing-grows-when-the-region-is-so-big-that-it-would-make-too-many-lines
  (is (= 125d0 (dom:default-hatch-spacing (square) 250d0)))
  (is (> (dom:default-hatch-spacing (square 1d6) 250d0) 125d0))
  (is (a-hatch :coordinates (square 1d6) :spacing (dom:default-hatch-spacing (square 1d6) 250d0))))

;;; The file

(defun hatch-drawing ()
  "Return a drawing with a hatch of each pattern."
  (let ((drawing (dom:make-drawing)))
    (dom:apply-changes drawing
                       (dom:create-entities-changes
                        (list (a-hatch :id 1 :pattern :lines :angle 0.7d0 :spacing 120d0)
                              (a-hatch :id 2 :pattern :cross :coordinates (l-shape) :spacing 200d0)
                              (a-hatch :id 3 :pattern :solid :coordinates (square 300d0)))))
    drawing))

(test a-hatch-is-written-and-read-back-exactly
  (let* ((drawing (hatch-drawing))
         (text (persist:drawing-to-string drawing))
         (back (persist:parse-drawing-text text)))
    (is (search (concatenate 'string
                             "(hatch (id 1) (layer 0) (vertices 0.0 0.0 1000.0 0.0 1000.0 1000.0 0.0 1000.0) "
                             "(pattern lines) (angle 0.7)")
                text))
    (is (search "(pattern cross)" text))
    (is (search "(pattern solid)" text))
    (is (equal (entity-values drawing) (entity-values back)))
    (is (string= text (persist:drawing-to-string back)))
    (is (= (length (dom:hatch-segments (dom:store-find (dom:drawing-store drawing) 1)))
           (length (dom:hatch-segments (dom:store-find (dom:drawing-store back) 1)))))))

(test an-older-file-cannot-hold-a-hatch
  (let ((text (persist:drawing-to-string (hatch-drawing))))
    (dolist (version '(1 2 3))
      (let ((old (replace-once-text text
                                    (format nil "(format-version ~d)" persist:+supported-format-version+)
                                    (format nil "(format-version ~d)" version))))
        (handler-case (progn (persist:parse-drawing-text old)
                             (fail "a version ~d file with a hatch was accepted" version))
          (base:drawing-file-content-error (condition)
            (is (search "unknown entity type" (princ-to-string condition)))))))))

(test the-reader-refuses-a-hatch-that-is-not-valid
  (let ((text (persist:drawing-to-string (hatch-drawing))))
    (dolist (broken (list (replace-once-text text "(pattern lines)" "(pattern dots)")
                          (replace-once-text text "(pattern lines)" "(pattern 5)")
                          (replace-once-text text "(spacing 120.0)" "(spacing 0.0)")
                          (replace-once-text text "(pattern solid)" "(pattern \"solid\")")
                          (replace-once-text text "(vertices 0.0 0.0 1000.0 0.0 1000.0 1000.0 0.0 1000.0)"
                                             "(vertices 0.0 0.0 1000.0 0.0)")))
      (signals base:drawing-file-error (persist:parse-drawing-text broken)))))

(test a-version-3-file-is-read-and-saved-as-the-newest-version
  (let* ((fixture (fixture-text "v3-plan.bcad"))
         (drawing (persist:parse-drawing-text fixture))
         (written (persist:drawing-to-string drawing)))
    (is (search "(format-version 3)" fixture))
    (is (= 12 (dom:drawing-entity-count drawing)))
    (is (string= (replace-once-text fixture
                                    "(format-version 3)"
                                    (format nil "(format-version ~d)" persist:+supported-format-version+))
                 written))))

;;; The scene, DXF, plots and the console

(test hatches-are-drawn-as-lines-or-triangles
  (let ((lines (make-test-app :drawing (plot-drawing (a-hatch :spacing 250d0))))
        (solid (make-test-app :drawing (plot-drawing (a-hatch :pattern :solid)))))
    (set-test-view lines :scale 0.1d0 :center-x 500d0 :center-y 500d0)
    (set-test-view solid :scale 0.1d0 :center-x 500d0 :center-y 500d0)
    (app:app-render lines)
    (app:app-render solid)
    (is (= 1 (scene:scene-result-drawn-entities (app:app-last-scene-result lines))))
    (is (< (scene:draw-list-triangle-count (app:app-draw-list lines))
           (scene:draw-list-triangle-count (app:app-draw-list solid)))
        "a solid hatch has triangles of its own")))

(test a-hatch-is-written-to-dxf-as-lines-or-filled-triangles
  (let ((drawing (hatch-drawing)))
    (multiple-value-bind (text report) (dxf:export-drawing-text drawing)
      (let ((types (dxf-types text)))
        (is (< 10 (count "LINE" types :test #'string=)))
        (is (= 2 (count "SOLID" types :test #'string=)) "the solid square is two triangles"))
      (is (find-if (lambda (note) (search "Hatches" note)) (dxf:report-notes report))))))

(test a-hatch-is-plotted-as-strokes-and-fills
  (let* ((plan (plot:plan-plot (hatch-drawing) (plot:make-plot-settings)))
         (items (plot:plot-plan-items plan)))
    (is (< 10 (count-if (lambda (item) (typep item 'plot:plot-stroke)) items)))
    (is (= 2 (count-if (lambda (item) (typep item 'plot:plot-fill)) items)))))

(test the-console-hatches-a-region-or-a-polygon
  (let* ((drawing (region-drawing))
         (context (script-env drawing)))
    (is (equal '("6") (ran-ok context "(hatch 2000 500)")))
    (is (eq :lines (dom:hatch-pattern (stored context 6))))
    (is (equal '("7") (ran-ok context "(hatch 1000 1500 :pattern :solid)")))
    (is (eq :solid (dom:hatch-pattern (stored context 7))))
    (is (equal '("8")
               (ran-ok context "(hatch-polygon '((0 0) (500 0) (500 500)) :pattern :cross :angle 30 :spacing 50)")))
    (is (hatch-near-p (/ pi 6) (dom:hatch-angle (stored context 8))))
    (is (= 50d0 (dom:hatch-spacing (stored context 8))))
    (is (equal '("8") (ran-ok context "(edit 8 :pattern :lines)")))
    (is (search "no closed shape" (string-downcase (failure-of context "(hatch 4500 500)"))))
    (is (search "pattern" (failure-of context "(hatch 2000 500 :pattern :dots)")))
    (is (search "hatch-polygon" (script:script-result-output (run-script (script-env) "(help)"))))))

(test a-hatch-survives-a-trip-through-a-script
  (let* ((drawing (hatch-drawing))
         (again (dom:make-drawing))
         (result (run-script (script-env again) (script:drawing-script drawing))))
    (is (eq :ok (script:script-result-status result)) "~a" (script:script-result-message result))
    (is (equal (entity-shapes drawing) (entity-shapes again)))))

;;; The tool

(defun hatching-app ()
  "Return an application, with the grid snap and the object snap off, holding a rectangle and a circle in it."
  (let ((application (make-test-app)))
    (dom:set-snap-to-grid (app:app-drawing application) nil)
    (dom:set-object-snap (app:app-drawing application) nil)
    (app:commit-changes application
                        (dom:create-entities-changes
                         (list (dom:make-rectangle :id 1 :layer-id 0 :min-x -1500d0 :min-y -1000d0 :width 3000d0
                                                   :height 2000d0)
                               (dom:make-circle :id 2 :layer-id 0 :center-x 0d0 :center-y 0d0 :radius 500d0)))
                        "entity.create")
    application))

(test the-draw-menu-lists-the-hatch-and-the-solid-fill
  (let ((application (hatching-app)))
    (click-widget application :draw)
    (let ((kinds (mapcar (lambda (row) (second (ui:widget-id row))) (menu-rows application))))
      (is (equal '(:hatch :solid-fill) (last kinds 2))))
    (click-widget application '(:menu-item :solid-fill))
    (is (eq :solid-fill (app:tool-kind (app:app-tool application))))))

(test a-click-inside-a-shape-hatches-it-and-undoes-in-one-step
  (let ((application (hatching-app)))
    (choose-tool application :hatch)
    (move-pointer-to application 1000 0)
    (is (= 1 (length (app:tool-preview-entities (app:app-tool application) application))) "the preview")
    (click application 1000 0)
    (is (= 3 (dom:drawing-entity-count (app:app-drawing application))))
    (let ((hatch (last-entity application)))
      (is (eq :hatch (dom:entity-kind hatch)))
      (is (eq :lines (dom:hatch-pattern hatch)))
      (is (hatch-near-p 6000000d0 (geo:contour-area (dom:hatch-coordinates hatch)) 1d0) "the rectangle, not the circle")
      (is (= 125d0 (dom:hatch-spacing hatch))))
    (click application 0 0)
    (is (= 4 (dom:drawing-entity-count (app:app-drawing application))))
    (is (hatch-near-p (* pi 500 500) (geo:contour-area (dom:hatch-coordinates (last-entity application))) 2d4)
        "the innermost shape")
    (app:operation-undo application)
    (is (= 3 (dom:drawing-entity-count (app:app-drawing application))))
    (is (eq :hatch (app:tool-kind (app:app-tool application))) "the tool stays for the next one")))

(test the-solid-fill-makes-a-solid-hatch
  (let ((application (hatching-app)))
    (choose-tool application :solid-fill)
    (click application 1000 0)
    (is (eq :solid (dom:hatch-pattern (last-entity application))))))

(test a-click-outside-every-shape-says-so
  (let ((application (hatching-app)))
    (choose-tool application :hatch)
    (click application 2000 1500)
    (is (eq :error (cdr (app:app-message application))))
    (is (search "Click inside" (message-text application)))
    (is (= 2 (dom:drawing-entity-count (app:app-drawing application))))))

(test the-inspector-cycles-the-pattern-of-a-selected-hatch
  (let ((application (hatching-app)))
    (choose-tool application :hatch)
    (click application 1000 0)
    (click application 1000 0)
    (app:select-ids application (list (dom:entity-id (last-entity application))))
    (let ((button (find-widget application '(:property :pattern))))
      (is (not (null button)))
      (is (string= "Lines" (ui:widget-label button)))
      (click-widget application '(:property :pattern))
      (is (eq :cross (dom:hatch-pattern (last-entity application))))
      (click-widget application '(:property :pattern))
      (is (eq :solid (dom:hatch-pattern (last-entity application))))
      (click-widget application '(:property :pattern))
      (is (eq :lines (dom:hatch-pattern (last-entity application)))))))

(test the-hatch-tools-and-the-inspector-speak-every-language
  (dolist (language '(:pt-br :ru-ru))
    (i18n:with-language (language)
      (let ((application (hatching-app)))
        (click-widget application :draw)
        (is (null (untranslated-texts (remove nil (mapcar #'ui:widget-label (app:build-widgets application)))
                                      language))
            "the menu in ~a" language)
        (dolist (kind '(:hatch :solid-fill))
          (app:operation-set-tool application kind)
          (is (null (untranslated-texts (list (app:tool-prompt (app:app-tool application) application)) language))
              "~a in ~a" kind language))
        (click application 2000 1500)
        (is (null (untranslated-texts (list (message-text application)) language)) "the error in ~a" language)
        (click application 1000 0)
        (app:select-ids application (list (dom:entity-id (last-entity application))))
        (is (null (untranslated-texts (remove nil (mapcar #'ui:widget-label (app:build-widgets application)))
                                      language))
            "the inspector in ~a" language)))))
