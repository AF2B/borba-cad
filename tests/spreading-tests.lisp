(in-package #:borba-cad/tests)

(def-suite spreading
  :description "Taking a shape apart and moving its pieces from each other: the geometry, the change set and the tool."
  :in all)

(in-suite spreading)

;;; Helpers

(defun spread-pieces (drawing
                      id
                      distance)
  "Apply the spreading of the entity with ID to DRAWING and return the entities that are then in it, in order.

DRAWING: a drawing.
ID: the id of the entity to spread.
DISTANCE: millimetres."
  (dom:apply-changes drawing (dom:spread-changes drawing id distance))
  (dom:drawing-entities drawing))

(defun piece-ends (entity)
  "Return the four coordinates of a line.

ENTITY: a line."
  (list (dom:line-x1 entity) (dom:line-y1 entity) (dom:line-x2 entity) (dom:line-y2 entity)))

(defun spread-rectangle (id
                    x
                    y
                    width
                    height)
  "Return a rectangle on layer 0.

ID: the id.
X: the left edge.
Y: the bottom edge.
WIDTH: its width.
HEIGHT: its height."
  (dom:make-rectangle :id id
                      :layer-id 0
                      :min-x (float x 1d0)
                      :min-y (float y 1d0)
                      :width (float width 1d0)
                      :height (float height 1d0)))

(defun spread-square (coordinates)
  "Return a closed polyline with id 1 on layer 0.

COORDINATES: a list of x and y, vertex after vertex."
  (dom:make-polyline :id 1
                     :layer-id 0
                     :closed t
                     :coordinates (coerce coordinates '(simple-array double-float (*)))))

;;; Geometry

(test a-rectangle-comes-apart-into-its-four-sides-each-moved-straight-out
  (let* ((drawing (reshaping-drawing (spread-rectangle 1 0 0 200 100)))
         (pieces (spread-pieces drawing 1 10d0)))
    (is (= 4 (length pieces)))
    (is (every (lambda (piece) (eq :line (dom:entity-kind piece))) pieces))
    (is (equal '(2 3 4 5) (mapcar #'dom:entity-id pieces)) "the rectangle is gone and the sides are new")
    (is (numbers-close-p '(0 -10 200 -10) (piece-ends (first pieces))) "the bottom side goes down")
    (is (numbers-close-p '(210 0 210 100) (piece-ends (second pieces))) "the right side goes right")
    (is (numbers-close-p '(200 110 0 110) (piece-ends (third pieces))) "the top side goes up")
    (is (numbers-close-p '(-10 100 -10 0) (piece-ends (fourth pieces))) "the left side goes left")))

(test the-pieces-stay-on-the-layer-of-the-shape
  (let ((drawing (reshaping-drawing (spread-rectangle 1 0 0 200 100))))
    (dom:add-layer drawing "walls" (dom:palette-color-at 1))
    (let* ((layer (dom:layer-id (dom:find-layer-by-name drawing "walls")))
           (moved (dom:entity-with-layer (dom:store-find (dom:drawing-store drawing) 1) layer)))
      (dom:apply-changes drawing (list (dom:make-change 1 (dom:store-find (dom:drawing-store drawing) 1) moved)))
      (is (every (lambda (piece) (= layer (dom:entity-layer-id piece)))
                 (spread-pieces drawing 1 10d0))))))

(test a-circle-comes-apart-into-four-arcs-moved-out-from-the-center
  (let* ((drawing (reshaping-drawing (cut-circle 1 0 0 100)))
         (pieces (spread-pieces drawing 1 10d0)))
    (is (= 4 (length pieces)))
    (is (every (lambda (piece) (eq :arc (dom:entity-kind piece))) pieces))
    (let ((first-arc (first pieces)))
      (is (numbers-close-p (list (* 10 (cos (/ pi 4))) (* 10 (sin (/ pi 4))))
                           (list (dom:arc-center-x first-arc) (dom:arc-center-y first-arc)))
          "the arc of the first quarter goes up and to the right")
      (is (= 100d0 (dom:arc-radius first-arc))))))

(test a-polygon-comes-apart-into-its-sides-each-moved-away-from-the-center
  (let* ((drawing (reshaping-drawing (dom:make-polygon :id 1 :layer-id 0 :center-x 0d0 :center-y 0d0
                                                       :radius 100d0 :sides 6 :rotation 0d0)))
         (pieces (spread-pieces drawing 1 10d0)))
    (is (= 6 (length pieces)))
    (dolist (piece pieces)
      (let* ((middle-x (/ (+ (dom:line-x1 piece) (dom:line-x2 piece)) 2))
             (middle-y (/ (+ (dom:line-y1 piece) (dom:line-y2 piece)) 2))
             (apothem (* 100 (cos (/ pi 6)))))
        (is (< (abs (- (sqrt (+ (* middle-x middle-x) (* middle-y middle-y))) (+ apothem 10))) 1d-6)
            "each side is ten millimetres further from the center")))))

(test a-closed-polyline-turns-out-whichever-way-it-runs
  (let ((clockwise (reshaping-drawing (spread-square '(0d0 0d0 0d0 100d0 100d0 100d0 100d0 0d0))))
        (counter-clockwise (reshaping-drawing (spread-square '(0d0 0d0 100d0 0d0 100d0 100d0 0d0 100d0)))))
    (dolist (drawing (list clockwise counter-clockwise))
      (let ((pieces (spread-pieces drawing 1 10d0)))
        (is (= 4 (length pieces)))
        (is (every (lambda (piece)
                     (let ((x (/ (+ (dom:line-x1 piece) (dom:line-x2 piece)) 2))
                           (y (/ (+ (dom:line-y1 piece) (dom:line-y2 piece)) 2)))
                       (> (max (abs (- x 50)) (abs (- y 50))) 55)))
                   pieces)
            "every side ends up outside the square it was part of")))))

(test an-open-polyline-comes-apart-into-segments-moved-away-from-its-center
  (let* ((drawing (reshaping-drawing (dom:make-polyline :id 1 :layer-id 0 :closed nil
                                                        :coordinates (coerce '(0d0 0d0 100d0 0d0 100d0 100d0)
                                                                             '(simple-array double-float (*))))))
         (pieces (spread-pieces drawing 1 10d0)))
    (is (= 2 (length pieces)))
    (is (< (dom:line-y1 (first pieces)) 0) "the segment below the center goes down")
    (is (> (dom:line-x1 (second pieces)) 100) "the segment on the right goes right")))

(test a-block-comes-apart-into-the-entities-it-stands-for
  (let ((drawing (dom:make-drawing)))
    (let ((context (script-env drawing)))
      (ran-ok context "(with-block \"post\" (line 0 0 0 100) (circle 0 100 10))")
      (ran-ok context "(insert \"post\" 500 500)"))
    (let ((pieces (spread-pieces drawing 1 10d0)))
      (is (= 2 (length pieces)))
      (is (equal '(:line :circle) (mapcar #'dom:entity-kind pieces)))
      (is (< (dom:line-y1 (first pieces)) 500) "the lower piece goes down")
      (is (> (dom:circle-center-y (second pieces)) 600) "the upper piece goes up"))))

(test the-distance-is-how-far-each-piece-moves
  (dolist (distance '(1d0 10d0 250d0))
    (let* ((drawing (reshaping-drawing (spread-rectangle 1 0 0 200 100)))
           (pieces (spread-pieces drawing 1 distance)))
      (is (numbers-close-p (list 0 (- distance) 200 (- distance)) (piece-ends (first pieces)))))))

;;; What cannot be spread

(test what-cannot-be-spread-is-refused-with-the-reason
  (let ((drawing (reshaping-drawing (cut-line 1 0 0 100 0)
                                    (cut-arc 2 0 0 100 0.2d0 1d0)
                                    (dom:make-text :id 3 :layer-id 0 :x 0d0 :y 0d0 :string "x" :height 10d0)
                                    (spread-rectangle 4 0 0 10 10))))
    (refused-with ("cannot be spread") (dom:spread-changes drawing 1 10d0))
    (refused-with ("cannot be spread") (dom:spread-changes drawing 2 10d0))
    (refused-with ("cannot be spread") (dom:spread-changes drawing 3 10d0))
    (refused-with ("no longer there") (dom:spread-changes drawing 99 10d0))
    (refused-with ("above zero") (dom:spread-changes drawing 4 0d0))
    (refused-with ("above zero") (dom:spread-changes drawing 4 -5d0))))

(test a-polyline-of-one-segment-is-a-single-piece
  (let ((drawing (reshaping-drawing (dom:make-polyline :id 1 :layer-id 0 :closed nil
                                                       :coordinates (coerce '(0d0 0d0 100d0 0d0)
                                                                            '(simple-array double-float (*)))))))
    (refused-with ("single piece") (dom:spread-changes drawing 1 10d0))))

(test a-shape-on-a-locked-layer-is-not-spread
  (let ((drawing (reshaping-drawing (spread-rectangle 1 0 0 100 100))))
    (dom:set-layer-locked drawing 0 t)
    (refused-with ("locked layer") (dom:spread-changes drawing 1 10d0))))

(test spreading-changes-nothing-until-the-change-set-is-applied-and-undoes-in-one-step
  (let* ((drawing (reshaping-drawing (spread-rectangle 1 0 0 200 100)))
         (history (dom:make-history)))
    (let ((changes (dom:spread-changes drawing 1 10d0)))
      (is (= 1 (dom:drawing-entity-count drawing)) "working it out changes nothing")
      (dom:history-commit history drawing changes)
      (is (= 4 (dom:drawing-entity-count drawing)))
      (dom:history-undo history drawing)
      (is (= 1 (dom:drawing-entity-count drawing)))
      (is (eq :rectangle (dom:entity-kind (dom:store-find (dom:drawing-store drawing) 1)))))))

;;; Picking what to spread

(test a-click-inside-a-shape-picks-the-smallest-shape-around-the-click
  (let ((drawing (reshaping-drawing (spread-rectangle 1 0 0 1000 1000)
                                    (spread-rectangle 2 100 100 300 300)
                                    (cut-circle 3 700 700 100))))
    (is (= 2 (dom:entity-id (dom:spread-target drawing 200d0 200d0 5d0))))
    (is (= 1 (dom:entity-id (dom:spread-target drawing 600d0 300d0 5d0))))
    (is (= 3 (dom:entity-id (dom:spread-target drawing 700d0 700d0 5d0))))
    (is (null (dom:spread-target drawing 5000d0 5000d0 5d0)))))

(test a-click-on-the-edge-of-a-shape-that-is-open-picks-it
  (let ((drawing (reshaping-drawing (dom:make-polyline :id 1 :layer-id 0 :closed nil
                                                       :coordinates (coerce '(0d0 0d0 100d0 0d0 100d0 100d0)
                                                                            '(simple-array double-float (*)))))))
    (is (= 1 (dom:entity-id (dom:spread-target drawing 50d0 1d0 5d0))))
    (is (null (dom:spread-target drawing 50d0 50d0 5d0)) "far from the polyline, and not inside a shape")))

;;; The tool

(test the-spread-tool-is-in-the-modify-menu-and-on-shift-p
  (let ((application (reshaping-app (spread-rectangle 1 -500 -500 1000 1000))))
    (press application :p :shift)
    (is (eq :spread (app:tool-kind (app:app-tool application))))
    (press application :escape)
    (choose-modify-tool-by-menu application :spread)
    (is (eq :spread (app:tool-kind (app:app-tool application))))
    (is (ui:widget-active (find-widget application :modify)))))

(test the-spread-tool-takes-the-shape-apart-where-it-is-clicked-and-undoes-in-one-step
  (let ((application (reshaping-app (spread-rectangle 1 -500 -500 1000 1000))))
    (app:operation-set-tool application :spread)
    (is (search "distance 10 mm" (app:tool-prompt (app:app-tool application) application)))
    (click application 0 0)
    (is (= 4 (dom:drawing-entity-count (app:app-drawing application))))
    (is (every (lambda (entity) (eq :line (dom:entity-kind entity))) (entities-of application)))
    (app:operation-undo application)
    (is (equal '(:rectangle) (mapcar #'dom:entity-kind (entities-of application))))))

(test a-number-typed-sets-the-distance-of-the-next-spread
  (let ((application (reshaping-app (spread-rectangle 1 -500 -500 1000 1000))))
    (app:operation-set-tool application :spread)
    (type-text application "25")
    (press application :enter)
    (is (= 25d0 (app:app-spread-distance application)))
    (is (search "25 mm" (message-text application)))
    (click application 0 0)
    (let ((bottom (first (entities-of application))))
      (is (numbers-close-p '(-500 -525 500 -525) (piece-ends bottom))))))

(test the-spread-tool-says-why-it-did-nothing
  (let ((application (reshaping-app (cut-line 1 -1000 0 1000 0))))
    (app:operation-set-tool application :spread)
    (click application 2000 2000)
    (is (search "Click inside" (message-text application)))
    (is (eq :error (cdr (app:app-message application))))
    (click application 0 0)
    (is (search "cannot be spread" (message-text application)))
    (is (= 1 (dom:drawing-entity-count (app:app-drawing application))) "nothing changed")))

(test the-spread-tool-shows-what-the-shape-will-become
  (let ((application (reshaping-app (spread-rectangle 1 -500 -500 1000 1000))))
    (app:operation-set-tool application :spread)
    (is (null (app:tool-preview-entities (app:app-tool application) application)) "nothing under the pointer")
    (send application (app:make-pointer-event :move nil
                                              (nth-value 0 (screen-position application 0 0))
                                              (nth-value 1 (screen-position application 0 0))))
    (is (= 4 (length (app:tool-preview-entities (app:app-tool application) application))))))

(test the-spread-tool-and-its-messages-speak-every-language
  (dolist (language '(:pt-br :ru-ru))
    (i18n:with-language (language)
      (let ((application (reshaping-app (cut-line 1 -1000 0 1000 0))))
        (app:operation-set-tool application :spread)
        (is (null (untranslated-texts (list (app:tool-prompt (app:app-tool application) application)) language)))
        (click application 2000 2000)
        (is (null (untranslated-texts (list (message-text application)) language)) "nothing there in ~a" language)
        (click application 0 0)
        (is (null (untranslated-texts (list (message-text application)) language)) "a refusal in ~a" language)
        (click-widget application :modify)
        (is (null (untranslated-texts (remove nil (mapcar #'ui:widget-label (app:build-widgets application)))
                                      language))
            "the menu in ~a" language)))))

;;; Exploding a shape

(test exploding-a-shape-leaves-its-pieces-where-they-were
  (let ((drawing (reshaping-drawing (spread-rectangle 1 0 0 200 100)
                                    (cut-circle 2 500 0 50)
                                    (cut-line 3 0 500 100 500)
                                    (dom:make-polyline :id 4
                                                       :layer-id 0
                                                       :closed nil
                                                       :coordinates (coerce '(0d0 900d0 100d0 900d0 100d0 1000d0)
                                                                            '(simple-array double-float (*)))))))
    (multiple-value-bind (changes exploded skipped) (dom:explode-changes drawing '(1 2 3 4))
      (is (= 2 exploded) "the rectangle and the polyline")
      (is (= 2 skipped) "the circle and the line have no pieces")
      (dom:apply-changes drawing changes)
      (let ((lines (remove-if-not (lambda (entity) (eq :line (dom:entity-kind entity)))
                                  (dom:drawing-entities drawing))))
        (is (= 7 (length lines)) "the line that was there, four sides and two segments")
        (is (some (lambda (line) (numbers-close-p '(0 0 200 0) (piece-ends line))) lines)
            "the bottom side is where the bottom of the rectangle was")
        (is (some (lambda (line) (numbers-close-p '(100 900 100 1000) (piece-ends line))) lines))))))

(test a-polygon-explodes-into-its-sides
  (let ((drawing (reshaping-drawing (dom:make-polygon :id 1 :layer-id 0 :center-x 0d0 :center-y 0d0
                                                      :radius 100d0 :sides 5 :rotation 0d0))))
    (dom:apply-changes drawing (dom:explode-changes drawing '(1)))
    (is (= 5 (dom:drawing-entity-count drawing)))
    (is (every (lambda (entity) (eq :line (dom:entity-kind entity))) (dom:drawing-entities drawing)))))

;;; Spreading what is selected

(test several-shapes-are-spread-together-with-ids-that-do-not-meet
  (let ((drawing (reshaping-drawing (spread-rectangle 1 0 0 200 100)
                                    (spread-rectangle 2 1000 0 200 100)
                                    (cut-line 3 0 500 100 500))))
    (multiple-value-bind (changes spread skipped) (dom:spread-selection-changes drawing '(1 2 3) 10d0)
      (is (= 2 spread))
      (is (= 1 skipped) "the line has no pieces")
      (let ((new-ids (loop for change in changes
                           when (null (dom:change-before change))
                             collect (dom:entity-id (dom:change-after change)))))
        (is (= 8 (length new-ids)))
        (is (= 8 (length (remove-duplicates new-ids))) "every piece has an id of its own"))
      (dom:apply-changes drawing changes)
      (is (= 9 (dom:drawing-entity-count drawing))))))

(test spreading-a-selection-needs-a-distance-above-zero
  (let ((drawing (reshaping-drawing (spread-rectangle 1 0 0 200 100))))
    (refused-with ("above zero") (dom:spread-selection-changes drawing '(1) 0d0))))

;;; The commands of the Modify menu, with and without a selection

(defun select-by-clicking (application
                           world-x
                           world-y)
  "Select the entity at a point with the Select tool.

APPLICATION: the application.
WORLD-X: world x.
WORLD-Y: world y."
  (app:operation-set-tool application :select)
  (click application world-x world-y))

(test explode-takes-apart-the-selected-shape-and-selects-the-pieces
  (let ((application (reshaping-app (spread-rectangle 1 -500 -500 1000 1000))))
    (select-by-clicking application -500 0)
    (choose-modify-tool-by-menu application :explode)
    (is (equal '(:line :line :line :line) (mapcar #'dom:entity-kind (entities-of application))))
    (is (= 4 (hash-table-count (app:app-selection application))) "the pieces are selected")
    (is (search "Exploded 1 entity" (message-text application)))
    (is (eq :select (app:tool-kind (app:app-tool application))))
    (app:operation-undo application)
    (is (equal '(:rectangle) (mapcar #'dom:entity-kind (entities-of application))))))

(test explode-says-what-to-do-when-nothing-is-selected-or-it-cannot-be-done
  (let ((application (reshaping-app (spread-rectangle 1 -500 -500 1000 1000)
                                    (cut-line 2 2000 0 3000 0))))
    (choose-modify-tool-by-menu application :explode)
    (is (search "Select what you want to explode first" (message-text application)))
    (select-by-clicking application 2500 0)
    (choose-modify-tool-by-menu application :explode)
    (is (search "That cannot be exploded" (message-text application)))
    (is (= 2 (dom:drawing-entity-count (app:app-drawing application))) "nothing changed")))

(test spread-acts-at-once-on-the-selection-and-leaves-the-select-tool-active
  (let ((application (reshaping-app (spread-rectangle 1 -500 -500 1000 1000))))
    (select-by-clicking application -500 0)
    (choose-modify-tool-by-menu application :spread)
    (is (= 4 (dom:drawing-entity-count (app:app-drawing application))))
    (is (every (lambda (entity) (eq :line (dom:entity-kind entity))) (entities-of application)))
    (is (= 4 (hash-table-count (app:app-selection application))) "the pieces are selected")
    (is (eq :select (app:tool-kind (app:app-tool application))) "no tool is left waiting for a click")
    (is (search "Spread 1 entity" (message-text application)))
    (app:operation-undo application)
    (is (equal '(:rectangle) (mapcar #'dom:entity-kind (entities-of application))))))

(test shift-p-spreads-the-selection-too
  (let ((application (reshaping-app (spread-rectangle 1 -500 -500 1000 1000))))
    (select-by-clicking application -500 0)
    (press application :p :shift)
    (is (= 4 (dom:drawing-entity-count (app:app-drawing application))))
    (is (eq :select (app:tool-kind (app:app-tool application))))))

(test spread-with-nothing-selected-waits-for-one-click-and-then-lets-go
  (let ((application (reshaping-app (spread-rectangle 1 -500 -500 1000 1000)
                                    (cut-circle 2 1500 0 200))))
    (choose-modify-tool-by-menu application :spread)
    (is (eq :spread (app:tool-kind (app:app-tool application))))
    (click application 0 0)
    (is (= 5 (dom:drawing-entity-count (app:app-drawing application))))
    (is (eq :select (app:tool-kind (app:app-tool application))) "after one spread the tool gives way to Select")
    (click application 1500 -200)
    (is (= 1 (hash-table-count (app:app-selection application))) "the next click selects, as it should")
    (is (eq :circle (dom:entity-kind (first (selected-entities application)))))))

(defun selected-entities (application)
  "Return the selected entities of an application.

APPLICATION: the application."
  (mapcar (lambda (id) (dom:store-find (dom:drawing-store (app:app-drawing application)) id))
          (app:selection-ids application)))

(test spread-says-what-cannot-be-spread-in-a-selection
  (let ((application (reshaping-app (cut-line 1 2000 0 3000 0))))
    (select-by-clicking application 2500 0)
    (choose-modify-tool-by-menu application :spread)
    (is (search "That cannot be spread" (message-text application)))
    (is (eq :error (cdr (app:app-message application))))
    (is (= 1 (dom:drawing-entity-count (app:app-drawing application))))))

(test the-console-spreads-and-explodes
  (let* ((drawing (dom:make-drawing))
         (context (script-env drawing)))
    (ran-ok context "(rect-size 0 0 200 100) (circle 500 0 50)")
    (is (equal '("(3 4 5 6)") (ran-ok context "(spread 1 20)")) "the ids of the pieces")
    (is (equal '("0") (ran-ok context "(explode 2)")) "a circle has nothing to explode")
    (is (equal '("(7 8 9 10)") (ran-ok context "(spread 2)")) "a circle comes apart into four arcs")
    (is (search "above zero" (failure-of context "(spread 3 0)")))
    (is (equal '("0") (ran-ok context "(explode 3)")) "a side is a line, which has no pieces")))
