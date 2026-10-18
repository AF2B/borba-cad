(in-package #:borba-cad/tests)

(def-suite reshaping
  :description "Offset, trim, extend and fillet: the geometry of each, the change sets, and the tools."
  :in all)

(in-suite reshaping)

;;; Helpers

(defun reshaping-drawing (&rest entities)
  "Return a drawing holding ENTITIES, which must have ids from 1 on a layer that exists."
  (let ((drawing (dom:make-drawing)))
    (dom:apply-changes drawing (dom:create-entities-changes entities))
    drawing))

(defun cut-line (id x1 y1 x2 y2)
  "Return a line entity on layer 0."
  (dom:make-line :id id :layer-id 0 :x1 (float x1 1d0) :y1 (float y1 1d0) :x2 (float x2 1d0) :y2 (float y2 1d0)))

(defun cut-arc (id x y radius start end)
  "Return an arc entity on layer 0, with angles in radians."
  (dom:make-arc :id id :layer-id 0 :center-x (float x 1d0) :center-y (float y 1d0) :radius (float radius 1d0)
                :start-angle (float start 1d0) :end-angle (float end 1d0)))

(defun cut-circle (id x y radius)
  "Return a circle entity on layer 0."
  (dom:make-circle :id id :layer-id 0 :center-x (float x 1d0) :center-y (float y 1d0) :radius (float radius 1d0)))

(defun applied (drawing changes)
  "Apply CHANGES to DRAWING and return DRAWING."
  (dom:apply-changes drawing changes)
  drawing)

(defun line-ends (drawing id)
  "Return the four coordinates of the line with ID, as a list."
  (let ((line (dom:store-find (dom:drawing-store drawing) id)))
    (list (dom:line-x1 line) (dom:line-y1 line) (dom:line-x2 line) (dom:line-y2 line))))

(defun numbers-close-p (expected
                        actual
                        &optional (tolerance 1d-6))
  "Return true when two lists of numbers agree to TOLERANCE."
  (and (= (length expected) (length actual))
       (every (lambda (a b) (< (abs (- a b)) tolerance)) expected actual)))

(defmacro refused-with ((text) &body body)
  "Check that BODY signals EDIT-REFUSED and that its message has TEXT in it."
  `(handler-case (progn ,@body (fail "expected EDIT-REFUSED, but nothing was signaled"))
     (base:edit-refused (condition)
       (is (search ,text (princ-to-string condition)) "the message was ~s" (princ-to-string condition)))))

;;; Offset

(test a-line-is-offset-to-its-left-when-the-distance-is-positive
  (let ((line (cut-line 1 0 0 1000 0)))
    (is (numbers-close-p '(0 100 1000 100) (let ((moved (dom:entity-offset line 100d0)))
                                              (list (dom:line-x1 moved) (dom:line-y1 moved)
                                                    (dom:line-x2 moved) (dom:line-y2 moved)))))
    (is (= -100d0 (dom:line-y1 (dom:entity-offset line -100d0))))
    (is (= 250d0 (dom:entity-offset-through line 500d0 250d0)))
    (is (= -300d0 (dom:entity-offset-through line 500d0 -300d0)))
    (is (= 1 (dom:entity-id (dom:entity-offset line 5d0))) "it keeps the id; the change set gives the new one")))

(test round-shapes-offset-by-their-radius
  (let ((circle (cut-circle 1 0 0 1000))
        (arc (cut-arc 1 0 0 1000 0.3d0 1.2d0)))
    (is (= 1200d0 (dom:circle-radius (dom:entity-offset circle 200d0))))
    (is (= 800d0 (dom:circle-radius (dom:entity-offset circle -200d0))))
    (signals base:invalid-entity (dom:entity-offset circle -1000d0))
    (is (= 500d0 (dom:entity-offset-through circle 1500d0 0d0)))
    (is (= -700d0 (dom:entity-offset-through circle 0d0 300d0)))
    (let ((bigger (dom:entity-offset arc 100d0)))
      (is (= 1100d0 (dom:arc-radius bigger)))
      (is (= 0.3d0 (dom:arc-start-angle bigger)))
      (is (= 1.2d0 (dom:arc-end-angle bigger))))
    (is (= 100d0 (dom:entity-offset-through arc 1100d0 0d0)))))

(test a-rectangle-grows-and-shrinks-on-every-side
  (let ((rectangle (dom:make-rectangle :id 1 :layer-id 0 :min-x 0d0 :min-y 0d0 :width 1000d0 :height 600d0)))
    (let ((grown (dom:entity-offset rectangle 50d0)))
      (is (equal '(-50d0 -50d0 1100d0 700d0)
                 (list (dom:rectangle-min-x grown) (dom:rectangle-min-y grown)
                       (dom:rectangle-width grown) (dom:rectangle-height grown)))))
    (let ((shrunk (dom:entity-offset rectangle -50d0)))
      (is (equal '(50d0 50d0 900d0 500d0)
                 (list (dom:rectangle-min-x shrunk) (dom:rectangle-min-y shrunk)
                       (dom:rectangle-width shrunk) (dom:rectangle-height shrunk)))))
    (signals base:invalid-entity (dom:entity-offset rectangle -300d0))
    (is (= 200d0 (dom:entity-offset-through rectangle 1200d0 300d0)) "outside is positive")
    (is (= -100d0 (dom:entity-offset-through rectangle 900d0 300d0)) "inside is negative")))

(test a-regular-polygon-moves-its-sides-by-the-distance
  (let* ((polygon (dom:make-polygon :id 1 :layer-id 0 :center-x 0d0 :center-y 0d0 :radius 1000d0 :sides 6
                                    :rotation 0d0))
         (apothem (* 1000d0 (cos (/ pi 6))))
         (bigger (dom:entity-offset polygon 100d0)))
    (is (< (abs (- (+ apothem 100d0) (* (dom:polygon-radius bigger) (cos (/ pi 6))))) 1d-9))
    (is (< (abs (- 150d0 (dom:entity-offset-through polygon 0d0 (+ apothem 150d0)))) 1d-6)
        "the top side of a hexagon with a vertex on the x axis faces straight up")))

(defun polyline-of (id closed &rest coordinates)
  "Return a polyline entity on layer 0 through the points, given as x y pairs."
  (dom:make-polyline :id id :layer-id 0 :closed closed
                     :coordinates (apply #'doubles (mapcar (lambda (value) (float value 1d0)) coordinates))))

(test an-open-polyline-moves-to-its-left-keeping-its-corners-sharp
  (let ((moved (dom:entity-offset (polyline-of 1 nil 0 0 1000 0 1000 1000) 100d0)))
    (is (numbers-close-p '(0 100 900 100 900 1000) (coerce (dom:polyline-coordinates moved) 'list)))
    (is (= 100d0 (dom:entity-offset-through (polyline-of 1 nil 0 0 1000 0 1000 1000) 500d0 100d0)))))

(test a-closed-polyline-grows-whichever-way-it-goes-around
  (dolist (points '((0 0 1000 0 1000 1000 0 1000) (0 1000 1000 1000 1000 0 0 0)))
    (let* ((square (apply #'polyline-of 1 t points))
           (outer (coerce (dom:polyline-coordinates (dom:entity-offset square 100d0)) 'list))
           (inner (coerce (dom:polyline-coordinates (dom:entity-offset square -100d0)) 'list)))
      (is (= -100d0 (reduce #'min outer)) "~s outward" points)
      (is (= 1100d0 (reduce #'max outer)))
      (is (= 100d0 (reduce #'min inner)) "~s inward" points)
      (is (= 900d0 (reduce #'max inner)))
      (is (= 150d0 (dom:entity-offset-through square 500d0 -150d0)) "outside is positive")
      (is (= -50d0 (dom:entity-offset-through square 500d0 50d0)) "inside is negative"))))

(test a-corner-that-turns-back-is-too-sharp-to-offset
  (signals-invalid-entity :polyline
    (dom:entity-offset (polyline-of 1 nil 0 0 1000 0 0 10) 50d0)))

(test what-cannot-be-offset-says-so
  (let ((text (dom:make-text :id 1 :layer-id 0 :x 0d0 :y 0d0 :string "x" :height 10d0)))
    (is (null (dom:entity-offset text 10d0)))
    (is (null (dom:entity-offset-through text 0d0 0d0))))
  (let ((ellipse (dom:make-ellipse :id 1 :layer-id 0 :center-x 0d0 :center-y 0d0 :radius-x 200d0 :radius-y 100d0)))
    (is (null (dom:entity-offset ellipse 10d0)))))

(test the-offset-change-set-adds-a-copy-on-the-layer-of-the-original
  (let* ((drawing (reshaping-drawing (cut-line 1 0 0 1000 0)))
         (layer (dom:add-layer drawing "walls" (dom:make-rgb 1 2 3))))
    (let ((line (dom:store-find (dom:drawing-store drawing) 1)))
      (dom:apply-changes drawing (list (dom:make-change 1
                                                        line
                                                        (dom:entity-with-layer line (dom:layer-id layer))))))
    (let ((changes (dom:offset-changes drawing 1 100d0)))
      (is (= 1 (length changes)))
      (is (null (dom:change-before (first changes))) "it is new")
      (let ((added (dom:change-after (first changes))))
        (is (= 2 (dom:entity-id added)))
        (is (= (dom:layer-id layer) (dom:entity-layer-id added)))
        (is (= 100d0 (dom:line-y1 added)))))
    (refused-with ("no longer there") (dom:offset-changes drawing 99 1d0))
    (dom:set-layer-locked drawing (dom:layer-id layer) t)
    (refused-with ("locked layer") (dom:offset-changes drawing 1 100d0))))

(test an-entity-that-cannot-be-offset-is-refused-in-a-sentence
  (let ((drawing (reshaping-drawing (dom:make-text :id 1 :layer-id 0 :x 0d0 :y 0d0 :string "x" :height 10d0))))
    (refused-with ("cannot be offset") (dom:offset-changes drawing 1 10d0))))

;;; Trim

(defun crossed-line-drawing (&rest more)
  "Return a drawing with a horizontal line, 1, from 0 to 1000, and two vertical lines across it at 300 and 700."
  (apply #'reshaping-drawing
         (cut-line 1 0 0 1000 0)
         (cut-line 2 300 -100 300 100)
         (cut-line 3 700 -100 700 100)
         more))

(test trimming-between-two-crossings-cuts-the-middle-out-and-leaves-two-lines
  (let ((drawing (crossed-line-drawing)))
    (applied drawing (dom:trim-changes drawing 1 500d0 0d0))
    (is (numbers-close-p '(0 0 300 0) (line-ends drawing 1)))
    (is (numbers-close-p '(700 0 1000 0) (line-ends drawing 4)) "the second piece is a new line")
    (is (= 4 (dom:drawing-entity-count drawing)))))

(test trimming-at-an-end-cuts-up-to-the-first-crossing
  (let ((drawing (crossed-line-drawing)))
    (applied drawing (dom:trim-changes drawing 1 100d0 0d0))
    (is (numbers-close-p '(300 0 1000 0) (line-ends drawing 1)))
    (is (= 3 (dom:drawing-entity-count drawing)) "one piece, so no new line"))
  (let ((drawing (crossed-line-drawing)))
    (applied drawing (dom:trim-changes drawing 1 900d0 0d0))
    (is (numbers-close-p '(0 0 700 0) (line-ends drawing 1)))))

(test a-click-off-the-line-trims-where-it-would-fall-on-it
  (let ((drawing (crossed-line-drawing)))
    (applied drawing (dom:trim-changes drawing 1 500d0 400d0))
    (is (numbers-close-p '(0 0 300 0) (line-ends drawing 1)))))

(test a-line-that-nothing-crosses-cannot-be-trimmed
  (let ((drawing (reshaping-drawing (cut-line 1 0 0 1000 0) (cut-line 2 2000 -100 2000 100))))
    (refused-with ("nothing to trim") (dom:trim-changes drawing 1 500d0 0d0))))

(test a-crossing-at-the-very-end-cuts-nothing
  (let ((drawing (reshaping-drawing (cut-line 1 0 0 1000 0) (cut-line 2 1000 -100 1000 100))))
    (refused-with ("nothing to trim") (dom:trim-changes drawing 1 500d0 0d0))))

(test every-kind-of-entity-cuts-a-line
  (dolist (cutter (list (dom:make-rectangle :id 2 :layer-id 0 :min-x 300d0 :min-y -100d0 :width 400d0 :height 200d0)
                        (cut-circle 2 500 0 200)
                        (cut-arc 2 500 0 200 -0.5d0 (- (* 2 pi) 0.7d0))
                        (polyline-of 2 nil 300 -100 300 100 700 100 700 -100)
                        (dom:make-polygon :id 2 :layer-id 0 :center-x 500d0 :center-y 0d0 :radius 300d0 :sides 4
                                          :rotation 0d0)))
    (let ((drawing (reshaping-drawing (cut-line 1 0 0 1000 0) cutter)))
      (applied drawing (dom:trim-changes drawing 1 500d0 0d0))
      (is (= 3 (dom:drawing-entity-count drawing)) "~a leaves two pieces" (dom:entity-kind cutter)))))

(test an-arc-is-trimmed-where-a-line-crosses-it
  (let ((drawing (reshaping-drawing (cut-arc 1 0 0 1000 0 pi) (cut-line 2 0 500 0 1500))))
    (applied drawing (dom:trim-changes drawing 1 (* 1000 (cos (/ pi 4))) (* 1000 (sin (/ pi 4)))))
    (let ((arc (dom:store-find (dom:drawing-store drawing) 1)))
      (is (< (abs (- (/ pi 2) (dom:arc-start-angle arc))) 1d-6))
      (is (< (abs (- pi (dom:arc-end-angle arc))) 1d-6))))
  (let ((drawing (reshaping-drawing (cut-arc 1 0 0 1000 0 pi) (cut-line 2 0 500 0 1500))))
    (applied drawing (dom:trim-changes drawing 1 (* 1000 (cos (* 3 (/ pi 4)))) (* 1000 (sin (* 3 (/ pi 4))))))
    (let ((arc (dom:store-find (dom:drawing-store drawing) 1)))
      (is (< (abs (- 0d0 (dom:arc-start-angle arc))) 1d-6))
      (is (< (abs (- (/ pi 2) (dom:arc-end-angle arc))) 1d-6)))))

(test an-arc-crossed-twice-leaves-two-arcs
  (let ((drawing (reshaping-drawing (cut-arc 1 0 0 1000 0 pi)
                                    (cut-line 2 -500 0 -500 1500)
                                    (cut-line 3 500 0 500 1500))))
    (applied drawing (dom:trim-changes drawing 1 0d0 1000d0))
    (is (= 4 (dom:drawing-entity-count drawing)))
    (is (eq :arc (dom:entity-kind (dom:store-find (dom:drawing-store drawing) 4))))))

(test a-circle-becomes-the-arc-that-is-left-when-a-piece-between-two-crossings-goes
  (let ((drawing (reshaping-drawing (cut-circle 1 0 0 1000)
                                    (cut-line 2 -500 -2000 -500 2000)
                                    (cut-line 3 500 -2000 500 2000))))
    (applied drawing (dom:trim-changes drawing 1 1000d0 0d0))
    (let ((arc (dom:store-find (dom:drawing-store drawing) 1)))
      (is (eq :arc (dom:entity-kind arc)) "the circle became an arc, with its id")
      (is (< (abs (- (/ pi 3) (dom:arc-start-angle arc))) 1d-6) "from 60 degrees")
      (is (< (abs (- (* 5 (/ pi 3)) (dom:arc-end-angle arc))) 1d-6) "round to 300 degrees, the long way"))))

(test a-circle-needs-two-crossings
  (let ((drawing (reshaping-drawing (cut-circle 1 0 0 1000) (cut-line 2 -2000 0 0 0))))
    (refused-with ("two crossings") (dom:trim-changes drawing 1 0d0 1000d0)))
  (let ((drawing (reshaping-drawing (cut-circle 1 0 0 1000))))
    (refused-with ("two crossings") (dom:trim-changes drawing 1 0d0 1000d0))))

(test trimming-ignores-entities-on-hidden-layers-and-refuses-locked-ones
  (let* ((drawing (crossed-line-drawing))
         (hidden (dom:add-layer drawing "hidden" (dom:make-rgb 1 2 3))))
    (dolist (id '(2 3))
      (let ((entity (dom:store-find (dom:drawing-store drawing) id)))
        (dom:apply-changes drawing (list (dom:make-change id entity (dom:entity-with-layer entity
                                                                                           (dom:layer-id hidden)))))))
    (dom:set-layer-visible drawing (dom:layer-id hidden) nil)
    (refused-with ("nothing to trim") (dom:trim-changes drawing 1 500d0 0d0))
    (dom:set-layer-visible drawing (dom:layer-id hidden) t)
    (is (dom:trim-changes drawing 1 500d0 0d0) "a locked layer still stops the cut, only the cut one is protected")
    (dom:set-layer-locked drawing 0 t)
    (refused-with ("locked layer") (dom:trim-changes drawing 1 500d0 0d0))))

(test only-lines-arcs-and-circles-are-trimmed
  (let ((drawing (reshaping-drawing (dom:make-rectangle :id 1 :layer-id 0 :min-x 0d0 :min-y 0d0 :width 100d0
                                                        :height 100d0)
                                    (cut-line 2 50 -100 50 200))))
    (refused-with ("lines, arcs and circles") (dom:trim-changes drawing 1 50d0 0d0))
    (refused-with ("no longer there") (dom:trim-changes drawing 77 50d0 0d0))))

;;; Extend

(test a-line-is-extended-up-to-the-next-entity
  (let ((drawing (reshaping-drawing (cut-line 1 0 0 500 0)
                                    (cut-line 2 1000 -100 1000 100)
                                    (cut-line 3 2000 -100 2000 100))))
    (applied drawing (dom:extend-changes drawing 1 450d0 0d0))
    (is (numbers-close-p '(0 0 1000 0) (line-ends drawing 1)) "to the first, not the second")))

(test the-end-nearer-to-the-click-is-the-one-that-grows
  (let ((drawing (reshaping-drawing (cut-line 1 500 0 1000 0)
                                    (cut-line 2 0 -100 0 100)
                                    (cut-line 3 2000 -100 2000 100))))
    (applied drawing (dom:extend-changes drawing 1 550d0 0d0))
    (is (numbers-close-p '(0 0 1000 0) (line-ends drawing 1)) "the start grew to the line at 0 and kept its direction"))
  (let ((drawing (reshaping-drawing (cut-line 1 500 0 1000 0)
                                    (cut-line 2 0 -100 0 100)
                                    (cut-line 3 2000 -100 2000 100))))
    (applied drawing (dom:extend-changes drawing 1 950d0 0d0))
    (is (numbers-close-p '(500 0 2000 0) (line-ends drawing 1)))))

(test a-line-is-extended-to-a-circle-or-an-arc
  (let ((drawing (reshaping-drawing (cut-line 1 0 0 100 0) (cut-circle 2 1000 0 300))))
    (applied drawing (dom:extend-changes drawing 1 90d0 0d0))
    (is (numbers-close-p '(0 0 700 0) (line-ends drawing 1)))))

(test a-line-with-nothing-in-its-way-cannot-be-extended
  (let ((drawing (reshaping-drawing (cut-line 1 0 0 500 0) (cut-line 2 1000 100 1000 200))))
    (refused-with ("nothing to extend to") (dom:extend-changes drawing 1 450d0 0d0)))
  (let ((drawing (reshaping-drawing (cut-line 1 0 0 500 0) (cut-line 2 -100 -100 -100 100))))
    (refused-with ("nothing to extend to") (dom:extend-changes drawing 1 450d0 0d0))))

(test an-arc-is-extended-along-its-circle
  (let ((drawing (reshaping-drawing (cut-arc 1 0 0 1000 0 (/ pi 2)) (cut-line 2 -500 0 -500 2000))))
    (applied drawing (dom:extend-changes drawing 1 0d0 1000d0))
    (let ((arc (dom:store-find (dom:drawing-store drawing) 1)))
      (is (< (abs (- (* 2 (/ pi 3)) (dom:arc-end-angle arc))) 1d-6) "the end grew to 120 degrees")
      (is (= 0d0 (dom:arc-start-angle arc)))))
  (let ((drawing (reshaping-drawing (cut-arc 1 0 0 1000 0 (/ pi 2)) (cut-line 2 0 -500 2000 -500))))
    (applied drawing (dom:extend-changes drawing 1 1000d0 0d0))
    (let ((arc (dom:store-find (dom:drawing-store drawing) 1)))
      (is (< (abs (- (* 11 (/ pi 6)) (dom:arc-start-angle arc))) 1d-6) "the start grew back to 330 degrees")
      (is (< (abs (- (/ pi 2) (dom:arc-end-angle arc))) 1d-6)))))

(test only-lines-and-arcs-are-extended
  (let ((drawing (reshaping-drawing (cut-circle 1 0 0 100) (cut-line 2 500 0 500 100))))
    (refused-with ("lines, arcs and circles") (dom:extend-changes drawing 1 100d0 0d0))))

;;; Fillet

(defun corner-drawing ()
  "Return a drawing with two lines that meet at the origin, along the axes."
  (reshaping-drawing (cut-line 1 0 0 1000 0) (cut-line 2 0 0 0 1000)))

(test a-fillet-rounds-a-corner-with-an-arc-and-takes-the-lines-to-where-it-touches-them
  (let ((drawing (corner-drawing)))
    (applied drawing (dom:fillet-changes drawing 1 500d0 0d0 2 0d0 500d0 100d0))
    (is (numbers-close-p '(100 0 1000 0) (line-ends drawing 1)))
    (is (numbers-close-p '(0 100 0 1000) (line-ends drawing 2)))
    (is (= 3 (dom:drawing-entity-count drawing)))
    (let ((arc (dom:store-find (dom:drawing-store drawing) 3)))
      (is (eq :arc (dom:entity-kind arc)))
      (is (numbers-close-p '(100 100 100) (list (dom:arc-center-x arc) (dom:arc-center-y arc) (dom:arc-radius arc))))
      (is (< (abs (- (/ pi 2) (dom:arc-sweep arc))) 1d-6) "a quarter, the short way, toward the corner")
      (is (< (abs (- pi (dom:arc-start-angle arc))) 1d-6)))))

(test a-fillet-of-a-slanted-corner-touches-both-lines
  (let* ((angle (/ pi 3))
         (drawing (reshaping-drawing (cut-line 1 0 0 1000 0)
                                     (cut-line 2 0 0 (* 1000 (cos angle)) (* 1000 (sin angle))))))
    (applied drawing (dom:fillet-changes drawing 1 500d0 0d0 2 (* 500 (cos angle)) (* 500 (sin angle)) 100d0))
    (let* ((arc (dom:store-find (dom:drawing-store drawing) 3))
           (center-x (dom:arc-center-x arc))
           (center-y (dom:arc-center-y arc)))
      (is (< (abs (- 100d0 (abs center-y))) 1d-6) "100 from the line along x")
      (is (< (abs (- 100d0 (abs (- (* center-x (sin angle)) (* center-y (cos angle)))))) 1d-6) "and from the other")
      (is (< (abs (- (* 100 (sqrt 3d0)) (first (line-ends drawing 1)))) 1d-6) "tangent length r over tan 30"))))

(test with-a-radius-of-zero-the-lines-are-only-taken-to-the-corner
  (let ((drawing (reshaping-drawing (cut-line 1 0 0 500 0) (cut-line 2 1000 -500 1000 500))))
    (applied drawing (dom:fillet-changes drawing 1 400d0 0d0 2 1000d0 400d0 0d0))
    (is (numbers-close-p '(1000 0 0 0) (line-ends drawing 1)) "grew to the corner")
    (is (numbers-close-p '(1000 0 1000 500) (line-ends drawing 2)) "cut back to it")
    (is (= 2 (dom:drawing-entity-count drawing)) "no arc")))

(test the-side-that-is-clicked-is-the-side-that-stays
  (let ((drawing (reshaping-drawing (cut-line 1 -1000 0 1000 0) (cut-line 2 0 -1000 0 1000))))
    (applied drawing (dom:fillet-changes drawing 1 -500d0 0d0 2 0d0 500d0 100d0))
    (is (numbers-close-p '(-100 0 -1000 0) (line-ends drawing 1)) "the left of the first, from its tangent point")
    (is (numbers-close-p '(0 100 0 1000) (line-ends drawing 2)) "the top of the second")
    (let ((arc (dom:store-find (dom:drawing-store drawing) 3)))
      (is (numbers-close-p '(-100 100) (list (dom:arc-center-x arc) (dom:arc-center-y arc)))))))

(test a-fillet-that-does-not-fit-is-refused
  (let ((drawing (reshaping-drawing (cut-line 1 0 0 150 0) (cut-line 2 0 0 0 1000))))
    (refused-with ("too big") (dom:fillet-changes drawing 1 100d0 0d0 2 0d0 500d0 200d0)))
  (let ((drawing (reshaping-drawing (cut-line 1 0 0 1000 0) (cut-line 2 500 -1000 500 1000))))
    (refused-with ("too big") (dom:fillet-changes drawing 1 900d0 0d0 2 500d0 500d0 600d0))
    (is (dom:fillet-changes drawing 1 900d0 0d0 2 500d0 500d0 400d0) "a smaller one fits")))

(test two-lines-that-never-meet-have-no-corner
  (let ((drawing (reshaping-drawing (cut-line 1 0 0 1000 0) (cut-line 2 0 100 1000 100))))
    (refused-with ("parallel") (dom:fillet-changes drawing 1 500d0 0d0 2 500d0 100d0 10d0)))
  (let ((drawing (reshaping-drawing (cut-line 1 0 0 1000 0))))
    (refused-with ("two different lines") (dom:fillet-changes drawing 1 500d0 0d0 1 400d0 0d0 10d0))))

(test only-lines-are-filleted
  (let ((drawing (reshaping-drawing (cut-line 1 0 0 1000 0) (cut-circle 2 500 500 100))))
    (refused-with ("lines, arcs and circles") (dom:fillet-changes drawing 1 500d0 0d0 2 600d0 500d0 10d0))))

(test a-fillet-is-one-set-of-changes-that-undoes-in-one-step
  (let* ((drawing (corner-drawing))
         (history (dom:make-history))
         (before (mapcar #'the-entity-signature (dom:drawing-entities drawing))))
    (dom:history-commit history drawing (dom:fillet-changes drawing 1 500d0 0d0 2 0d0 500d0 100d0))
    (is (= 3 (dom:drawing-entity-count drawing)))
    (dom:history-undo history drawing)
    (is (equal before (mapcar #'the-entity-signature (dom:drawing-entities drawing))))))

;;; The tools

(defun reshaping-app (&rest entities)
  "Return an application, with the grid snap and the object snap off, holding ENTITIES."
  (let ((application (make-test-app)))
    (dom:set-snap-to-grid (app:app-drawing application) nil)
    (dom:set-object-snap (app:app-drawing application) nil)
    (app:commit-changes application (dom:create-entities-changes entities) "entity.create")
    application))

(defun choose-modify-tool-by-menu (application
                                   kind)
  "Open the Modify menu with the mouse and click the row of the tool KIND."
  (click-widget application :modify)
  (click-widget application (list :menu-item kind)))

(defun reshaped-entity-ids (application)
  "Return the ids of the entities of APPLICATION, in stacking order."
  (mapcar #'dom:entity-id (entities-of application)))

(test the-keys-choose-the-reshaping-tools-and-they-need-no-selection
  (let ((application (reshaping-app (cut-line 1 -1000 0 1000 0))))
    (is (zerop (hash-table-count (app:app-selection application))))
    (dolist (spec '((:t :trim) (:e :extend) (:o :offset) (:f :fillet)))
      (press application (first spec) :shift)
      (is (eq (second spec) (app:tool-kind (app:app-tool application))))
      (press application :escape))
    (press application :r :shift)
    (is (eq :select (app:tool-kind (app:app-tool application))) "rotate still wants a selection")
    (press application :t)
    (is (eq :text (app:tool-kind (app:app-tool application))) "the plain letter is still text")
    (press application :escape)
    (choose-modify-tool-by-menu application :fillet)
    (is (eq :fillet (app:tool-kind (app:app-tool application))))
    (is (ui:widget-active (find-widget application :modify)))))

(test the-trim-tool-cuts-where-it-is-clicked-and-undoes-in-one-step
  (let ((application (reshaping-app (cut-line 1 -1500 0 1500 0)
                                    (cut-line 2 -500 -300 -500 300)
                                    (cut-line 3 500 -300 500 300))))
    (press application :t :shift)
    (click application 0 0)
    (is (= 4 (dom:drawing-entity-count (app:app-drawing application))))
    (is (numbers-close-p '(-1500 0 -500 0) (line-ends (app:app-drawing application) 1)))
    (is (numbers-close-p '(500 0 1500 0) (line-ends (app:app-drawing application) 4)))
    (is (eq :trim (app:tool-kind (app:app-tool application))) "it stays for the next cut")
    (app:operation-undo application)
    (is (= 3 (dom:drawing-entity-count (app:app-drawing application))) "one step took both pieces back")
    (is (numbers-close-p '(-1500 0 1500 0) (line-ends (app:app-drawing application) 1)))))

(test the-trim-tool-says-what-it-cannot-do
  (let ((application (reshaping-app (cut-line 1 -1500 0 1500 0)
                                    (dom:make-rectangle :id 2 :layer-id 0 :min-x -300d0 :min-y 500d0 :width 600d0
                                                        :height 300d0))))
    (press application :t :shift)
    (click application 1800 1500)
    (is (eq :error (cdr (app:app-message application))))
    (is (search "Click a line, an arc or a circle" (message-text application)))
    (click application 0 0)
    (is (search "nothing to trim" (message-text application)) "nothing crosses the line")
    (click application -300 500)
    (is (search "lines, arcs and circles" (message-text application)) "a rectangle is not trimmed")
    (is (= 2 (dom:drawing-entity-count (app:app-drawing application))))))

(test the-extend-tool-lengthens-the-end-near-the-click-up-to-the-next-entity
  (let ((application (reshaping-app (cut-line 1 -1000 0 0 0)
                                    (cut-line 2 800 -300 800 300))))
    (press application :e :shift)
    (click application -30 0)
    (is (numbers-close-p '(-1000 0 800 0) (line-ends (app:app-drawing application) 1)))
    (click application -30 0)
    (is (search "nothing to extend to" (message-text application)) "and a second time there is nothing more")
    (app:operation-undo application)
    (is (numbers-close-p '(-1000 0 0 0) (line-ends (app:app-drawing application) 1)))))

(test the-offset-tool-copies-an-entity-through-the-pointer
  (let ((application (reshaping-app (cut-line 1 -1000 0 1000 0))))
    (press application :o :shift)
    (click application 0 0)
    (is (app:tool-in-progress-p (app:app-tool application)))
    (is (search "type the distance" (app:tool-prompt (app:app-tool application) application)))
    (move-pointer-to application 0 300)
    (let ((preview (app:tool-preview-entities (app:app-tool application) application)))
      (is (= 1 (length preview)))
      (is (= 300d0 (dom:line-y1 (first preview)))))
    (click application 0 300)
    (is (equal '(1 2) (reshaped-entity-ids application)))
    (is (= 300d0 (dom:line-y1 (second (entities-of application)))))
    (is (not (app:tool-in-progress-p (app:app-tool application))) "ready for the next entity")
    (click application 0 0)
    (click application 0 -200)
    (is (= -200d0 (dom:line-y1 (third (entities-of application)))) "the other side")
    (app:operation-undo application)
    (is (= 2 (dom:drawing-entity-count (app:app-drawing application))))))

(test a-distance-can-be-typed-and-the-pointer-says-which-side
  (let ((application (reshaping-app (cut-line 1 -1000 0 1000 0))))
    (press application :o :shift)
    (click application 0 0)
    (move-pointer-to application 0 100)
    (typed-point application "250")
    (is (= 250d0 (dom:line-y1 (second (entities-of application)))))
    (click application 0 0)
    (move-pointer-to application 0 -100)
    (typed-point application "250")
    (is (= -250d0 (dom:line-y1 (third (entities-of application)))))))

(test a-typed-distance-is-in-the-display-unit-of-the-drawing
  (let ((application (reshaping-app (cut-line 1 -1000 0 1000 0))))
    (dom:set-display-unit (app:app-drawing application) :meter)
    (press application :o :shift)
    (click application 0 0)
    (move-pointer-to application 0 100)
    (typed-point application "0.5")
    (is (= 500d0 (dom:line-y1 (second (entities-of application)))))))

(test the-offset-tool-works-on-round-shapes-and-refuses-what-it-cannot
  (let ((application (reshaping-app (cut-circle 1 0 0 1000)
                                    (dom:make-text :id 2 :layer-id 0 :x 1500d0 :y 1500d0 :string "x"
                                                   :height 100d0))))
    (press application :o :shift)
    (click application 1000 0)
    (click application 1300 0)
    (is (= 1300d0 (dom:circle-radius (last-entity application))))
    (click application 1000 0)
    (move-pointer-to application 500 0)
    (click application 500 0)
    (is (= 500d0 (dom:circle-radius (last-entity application))) "inside makes it smaller")
    (click application 1500 1500)
    (is (eq :error (cdr (app:app-message application))))
    (is (search "cannot be offset" (message-text application)))
    (is (not (app:tool-in-progress-p (app:app-tool application))))
    (click application -1800 1800)
    (is (search "Click the entity to copy" (message-text application)))))

(test the-offset-tool-needs-the-pointer-away-from-the-entity
  (let ((application (reshaping-app (cut-line 1 -1000 0 1000 0))))
    (press application :o :shift)
    (click application 0 0)
    (click application 0 0)
    (is (eq :error (cdr (app:app-message application))))
    (is (search "away from the entity" (message-text application)))
    (is (= 1 (dom:drawing-entity-count (app:app-drawing application))))
    (press application :escape)
    (is (not (app:tool-in-progress-p (app:app-tool application))))))

(test the-fillet-tool-rounds-a-corner-with-the-radius-it-remembers
  (let ((application (reshaping-app (cut-line 1 0 0 1500 0) (cut-line 2 0 0 0 1500))))
    (is (= 100d0 (app:app-fillet-radius application)))
    (press application :f :shift)
    (is (search "radius 100 mm" (app:tool-prompt (app:app-tool application) application)))
    (click application 1000 0)
    (is (app:tool-in-progress-p (app:app-tool application)))
    (click application 0 1000)
    (is (= 3 (dom:drawing-entity-count (app:app-drawing application))))
    (is (numbers-close-p '(100 0 1500 0) (line-ends (app:app-drawing application) 1)))
    (is (= 100d0 (dom:arc-radius (last-entity application))))
    (app:operation-undo application)
    (is (= 2 (dom:drawing-entity-count (app:app-drawing application))) "one step")
    (is (numbers-close-p '(0 0 1500 0) (line-ends (app:app-drawing application) 1)))))

(test a-number-typed-sets-the-fillet-radius
  (let ((application (reshaping-app (cut-line 1 0 0 1500 0) (cut-line 2 0 0 0 1500))))
    (press application :f :shift)
    (typed-point application "250")
    (is (= 250d0 (app:app-fillet-radius application)))
    (is (search "250" (message-text application)))
    (typed-point application "-5")
    (is (eq :error (cdr (app:app-message application))) "a negative radius is refused")
    (is (= 250d0 (app:app-fillet-radius application)))
    (click application 1000 0)
    (click application 0 1000)
    (is (= 250d0 (dom:arc-radius (last-entity application))))
    (press application :escape)
    (press application :f :shift)
    (is (search "radius 250 mm" (app:tool-prompt (app:app-tool application) application))
        "the radius is remembered by the application, not by the tool")))

(test a-radius-of-zero-only-makes-the-corner
  (let ((application (reshaping-app (cut-line 1 0 -500 1500 -500) (cut-line 2 500 -1500 500 1000))))
    (press application :f :shift)
    (typed-point application "0")
    (click application 1000 -500)
    (click application 500 500)
    (is (= 2 (dom:drawing-entity-count (app:app-drawing application))) "no arc")
    (is (numbers-close-p '(500 -500 1500 -500) (line-ends (app:app-drawing application) 1)))))

(test the-fillet-tool-says-what-is-wrong
  (let ((application (reshaping-app (cut-line 1 0 0 1500 0)
                                    (cut-line 2 0 500 1500 500)
                                    (cut-circle 3 -1000 -1000 300))))
    (press application :f :shift)
    (click application -1000 -700)
    (is (search "Click a line" (message-text application)) "a circle is not a line")
    (click application 800 0)
    (click application 800 0)
    (is (search "two different lines" (message-text application)))
    (click application 800 500)
    (is (search "parallel" (message-text application)))
    (is (= 3 (dom:drawing-entity-count (app:app-drawing application))) "nothing changed")))

(test escape-leaves-the-fillet-tool-after-forgetting-the-first-line
  (let ((application (reshaping-app (cut-line 1 0 0 1500 0) (cut-line 2 0 0 0 1500))))
    (press application :f :shift)
    (click application 1000 0)
    (press application :escape)
    (is (not (app:tool-in-progress-p (app:app-tool application))))
    (is (eq :fillet (app:tool-kind (app:app-tool application))))
    (press application :escape)
    (is (eq :select (app:tool-kind (app:app-tool application))))))

(test an-edit-on-a-locked-layer-is-refused-by-the-tools
  (let ((application (reshaping-app (cut-line 1 -1500 0 1500 0) (cut-line 2 -500 -300 -500 300))))
    (dom:set-layer-locked (app:app-drawing application) 0 t)
    (press application :t :shift)
    (click application 0 0)
    (is (= 2 (dom:drawing-entity-count (app:app-drawing application))))
    (is (eq :error (cdr (app:app-message application))) "a locked layer cannot be picked at all")))

(test the-new-tools-and-their-messages-speak-every-language
  (dolist (language '(:pt-br :ru-ru))
    (i18n:with-language (language)
      (let ((application (reshaping-app (cut-line 1 -1000 0 1000 0)
                                        (dom:make-text :id 2 :layer-id 0 :x 1500d0 :y 1500d0 :string "x"
                                                       :height 100d0))))
        (click-widget application :modify)
        (is (null (untranslated-texts (remove nil (mapcar #'ui:widget-label (app:build-widgets application)))
                                      language))
            "the menu in ~a" language)
        (dolist (kind '(:trim :extend :offset :fillet))
          (app:operation-set-tool application kind)
          (is (null (untranslated-texts (list (app:tool-prompt (app:app-tool application) application)) language))
              "~a in ~a" kind language)
          (click application 1800 1800)
          (is (null (untranslated-texts (list (message-text application)) language)) "~a error in ~a" kind language))
        (app:operation-set-tool application :offset)
        (click application 1500 1500)
        (is (null (untranslated-texts (list (message-text application)) language)))
        (app:operation-set-tool application :trim)
        (click application 0 0)
        (is (null (untranslated-texts (list (message-text application)) language)) "a refusal in ~a" language)))))
