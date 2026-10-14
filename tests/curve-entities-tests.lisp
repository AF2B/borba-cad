(in-package #:borba-cad/tests)

(def-suite curve-entities
  :description "Arcs, ellipses and regular polygons: the entities, their file format, their tools and drawing."
  :in all)

(in-suite curve-entities)

(defun an-arc (&key (id 1) (cx 0d0) (cy 0d0) (radius 1000d0) (start 0d0) (end (/ pi 2)))
  "Return an arc entity."
  (dom:make-arc :id id :layer-id 0 :center-x cx :center-y cy :radius radius :start-angle start :end-angle end))

(defun an-ellipse (&key (id 2) (cx 0d0) (cy 0d0) (rx 3000d0) (ry 1000d0) (rotation 0d0))
  "Return an ellipse entity."
  (dom:make-ellipse :id id :layer-id 0 :center-x cx :center-y cy :radius-x rx :radius-y ry :rotation rotation))

(defun a-polygon (&key (id 3) (cx 0d0) (cy 0d0) (radius 1000d0) (sides 6) (rotation 0d0))
  "Return a regular polygon entity."
  (dom:make-polygon :id id :layer-id 0 :center-x cx :center-y cy :radius radius :sides sides :rotation rotation))

(defun curve-drawing ()
  "Return a drawing that holds an arc, an ellipse and a polygon, apart from one another."
  (let ((drawing (dom:make-drawing)))
    (dom:apply-changes drawing
                       (dom:create-entities-changes
                        (list (an-arc :id 1 :cx 0d0 :cy 0d0 :start 0.3d0 :end 2.5d0)
                              (an-ellipse :id 2 :cx 6000d0 :cy 0d0 :rotation 0.4d0)
                              (a-polygon :id 3 :cx 0d0 :cy -5000d0 :sides 5 :rotation 0.2d0))))
    drawing))

;;; Arcs

(test an-arc-keeps-its-angles-within-one-turn-and-knows-its-sweep
  (let ((arc (an-arc :start (- (/ pi 2)) :end (/ pi 2))))
    (is (< (abs (- (* 1.5d0 pi) (dom:arc-start-angle arc))) 1d-12) "-90 is 270")
    (is (< (abs (- pi (dom:arc-sweep arc))) 1d-12) "from 270 to 90 counter-clockwise is a half turn"))
  (is (< (abs (- (/ pi 2) (dom:arc-sweep (an-arc :start 0d0 :end (/ pi 2))))) 1d-12))
  (is (< (abs (- (* 1.5d0 pi) (dom:arc-sweep (an-arc :start (/ pi 2) :end 0d0)))) 1d-12) "the long way round"))

(test an-arc-needs-a-radius-a-sweep-and-finite-angles
  (signals base:invalid-entity (an-arc :radius 0d0))
  (signals base:invalid-entity (an-arc :radius -5d0))
  (signals base:invalid-entity (an-arc :start 1d0 :end 1d0))
  (signals base:invalid-entity (an-arc :start 1d0 :end (+ 1d0 (* 2 pi))) "a whole turn is a circle")
  (signals base:invalid-entity (an-arc :start 1d7))
  (signals base:invalid-entity (an-arc :cx 2d7))
  (handler-case (an-arc :start 1d0 :end 1d0)
    (base:invalid-entity (condition)
      (is (search "Invalid arc" (princ-to-string condition)))
      (is (search "must differ" (princ-to-string condition))))))

(test the-bounds-and-the-picking-of-an-arc-are-those-of-its-arc-and-not-its-circle
  (let ((arc (an-arc :start 0d0 :end (/ pi 2))))
    (is (< (abs (- 1000d0 (geo:bounds-max-x (dom:entity-bounds arc)))) 1d-9))
    (is (< (abs (- 0d0 (geo:bounds-min-x (dom:entity-bounds arc)))) 1d-9)
        "the quarter does not reach left of the center")
    (is (< (dom:entity-pick-distance arc 707.1067811865476d0 707.1067811865476d0) 1d-6) "on the arc")
    (is (> (dom:entity-pick-distance arc -707d0 -707d0) 1000d0) "on the circle, away from the arc")
    (is (< (abs (- 1000d0 (dom:entity-pick-distance arc 0d0 0d0))) 1d-9) "from the center it is the radius")))

(test an-arc-offers-its-center-and-its-two-ends-for-snapping
  (let ((snaps (dom:entity-snap-points (an-arc :start 0d0 :end (/ pi 2)))))
    (is (= 3 (length snaps)))
    (is (equal '(:center :endpoint :endpoint) (mapcar #'third snaps)))
    (is (< (abs (- 1000d0 (first (second snaps)))) 1d-9) "the start")
    (is (< (abs (- 1000d0 (second (third snaps)))) 1d-9) "the end")))

(test an-arc-shows-its-properties-and-derives-its-length-and-sweep
  (let* ((arc (an-arc :radius 1000d0 :start 0d0 :end pi))
         (keys (mapcar #'dom:property-key (dom:entity-properties arc)))
         (derived (dom:entity-derived-values arc)))
    (is (equal '(:center-x :center-y :radius :start-angle :end-angle) keys))
    (is (equal '(:coordinate :coordinate :length :angle :angle)
               (mapcar #'dom:property-kind (dom:entity-properties arc))))
    (is (< (abs (- (* pi 1000d0) (dom:derived-value-value (first derived)))) 1d-9) "half a circumference")
    (is (< (abs (- pi (dom:derived-value-value (second derived)))) 1d-12))
    (dolist (key keys)
      (let ((value (dom:property-value (find key (dom:entity-properties arc) :key #'dom:property-key))))
        (is (equalp (entity-signature arc) (entity-signature (dom:entity-with-property arc key value)))
            "setting ~s to what it is changes nothing" key)))
    (is (= 2000d0 (dom:arc-radius (dom:entity-with-property arc :radius 2000d0))))
    (is (< (abs (- 1d0 (dom:arc-end-angle (dom:entity-with-property arc :end-angle 1d0)))) 1d-12))))

;;; Ellipses

(test an-ellipse-keeps-its-rotation-within-a-half-turn
  (is (< (abs (- 0.5d0 (dom:ellipse-rotation (an-ellipse :rotation 0.5d0)))) 1d-12))
  (is (< (abs (- 0.5d0 (dom:ellipse-rotation (an-ellipse :rotation (+ 0.5d0 pi))))) 1d-12)
      "half a turn is the same ellipse")
  (is (< (abs (- (- pi 0.5d0) (dom:ellipse-rotation (an-ellipse :rotation -0.5d0)))) 1d-12))
  (signals base:invalid-entity (an-ellipse :rx 0d0))
  (signals base:invalid-entity (an-ellipse :ry -1d0))
  (signals base:invalid-entity (an-ellipse :rotation 1d9)))

(test an-ellipse-is-picked-on-its-outline-and-reaches-as-far-as-its-bounds-say
  (let ((ellipse (an-ellipse :rx 3000d0 :ry 1000d0 :rotation 0d0)))
    (is (< (dom:entity-pick-distance ellipse 3000d0 0d0) 1d-9))
    (is (< (dom:entity-pick-distance ellipse 0d0 1000d0) 1d-9))
    (is (< (abs (- 1000d0 (dom:entity-pick-distance ellipse 0d0 0d0))) 1d-9) "the center: the short semi-axis")
    (is (< (abs (- 3000d0 (geo:bounds-max-x (dom:entity-bounds ellipse)))) 1d-9)))
  (let ((turned (an-ellipse :rx 3000d0 :ry 1000d0 :rotation (/ pi 2))))
    (is (< (abs (- 3000d0 (geo:bounds-max-y (dom:entity-bounds turned)))) 1d-6) "a quarter turn stands it up")
    (is (< (dom:entity-pick-distance turned 0d0 3000d0) 1d-6))))

(test an-ellipse-offers-its-center-and-the-ends-of-its-axes-for-snapping
  (let ((snaps (dom:entity-snap-points (an-ellipse :rx 3000d0 :ry 1000d0 :rotation 0d0))))
    (is (= 5 (length snaps)))
    (is (equal '(:center :endpoint :endpoint :endpoint :endpoint) (mapcar #'third snaps)))
    (is (member 3000d0 (mapcar #'first snaps) :test #'=))
    (is (member -1000d0 (mapcar #'second snaps) :test (lambda (a b) (< (abs (- a b)) 1d-9))))))

(test an-ellipse-shows-its-axes-in-the-inspector-and-derives-its-area
  (let ((ellipse (an-ellipse :rx 3000d0 :ry 1000d0)))
    (is (equal '(:center-x :center-y :radius-x :radius-y :rotation)
               (mapcar #'dom:property-key (dom:entity-properties ellipse))))
    (is (< (abs (- (* pi 3000d0 1000d0) (dom:derived-value-value (second (dom:entity-derived-values ellipse))))) 1d-3))
    (is (= 500d0 (dom:ellipse-radius-y (dom:entity-with-property ellipse :radius-y 500d0))))))

;;; Regular polygons

(test a-polygon-has-whole-sides-from-three-to-a-thousand
  (dolist (sides '(3 4 6 99 1000))
    (is (= sides (dom:polygon-sides (a-polygon :sides sides)))))
  (dolist (sides (list 2 0 -3 1001 3.5d0 "6" nil))
    (signals base:invalid-entity (a-polygon :sides sides)))
  (handler-case (a-polygon :sides 2)
    (base:invalid-entity (condition)
      (is (search "Invalid polygon" (princ-to-string condition))))))

(test a-polygon-has-vertices-on-its-circle-and-is-picked-on-its-edges
  (let* ((polygon (a-polygon :radius 1000d0 :sides 4 :rotation 0d0))
         (vertices (dom:polygon-vertices polygon)))
    (is (= 8 (length vertices)))
    (is (< (abs (- 1000d0 (aref vertices 0))) 1d-9))
    (is (< (dom:entity-pick-distance polygon 500d0 500d0) 1d-9) "the middle of the first edge")
    (is (< (abs (- (* 1000d0 (cos (/ pi 4))) (dom:entity-pick-distance polygon 0d0 0d0))) 1d-9)
        "the center is the apothem away")
    (is (< (abs (- 1000d0 (geo:bounds-max-x (dom:entity-bounds polygon)))) 1d-9))))

(test a-polygon-offers-its-center-and-its-vertices-for-snapping
  (let ((snaps (dom:entity-snap-points (a-polygon :sides 5))))
    (is (= 6 (length snaps)))
    (is (eq :center (third (first snaps))))
    (is (every (lambda (snap) (eq :endpoint (third snap))) (rest snaps)))))

(test changing-the-sides-of-a-polygon-changes-its-shape
  (let* ((polygon (a-polygon :sides 4))
         (octagon (dom:entity-with-property polygon :sides 8)))
    (is (= 16 (length (dom:polygon-vertices octagon))))
    (signals base:invalid-entity (dom:entity-with-property polygon :sides 2))
    (is (equal '(:center-x :center-y :radius :sides :rotation)
               (mapcar #'dom:property-key (dom:entity-properties polygon))))
    (let ((derived (dom:entity-derived-values (a-polygon :radius 1000d0 :sides 6))))
      (is (< (abs (- 1000d0 (dom:derived-value-value (first derived)))) 1d-9) "the side of a hexagon is its radius")
      (is (< (abs (- 6000d0 (dom:derived-value-value (second derived)))) 1d-9) "and the perimeter six of them"))))

;;; Moving, copying and the other things every entity does

(test the-new-entities-move-keep-their-shape-and-take-another-identity
  (dolist (entity (list (an-arc) (an-ellipse) (a-polygon)))
    (let ((moved (dom:entity-translated entity 100d0 -50d0))
          (copy (dom:entity-with-identity entity 77 0)))
      (is (= 77 (dom:entity-id copy)))
      (is (equal (entity-signature entity) (entity-signature copy)))
      (is (< (abs (- (+ (geo:bounds-min-x (dom:entity-bounds entity)) 100d0)
                     (geo:bounds-min-x (dom:entity-bounds moved))))
             1d-6))
      (is (equal (entity-signature entity) (entity-signature (dom:entity-with-layer entity 0)))))))

;;; The file

(test the-new-entities-are-written-and-read-back-exactly
  (let* ((drawing (curve-drawing))
         (text (persist:drawing-to-string drawing))
         (back (persist:parse-drawing-text text)))
    (is (search "(arc (id 1) (layer 0) (center-x 0.0) (center-y 0.0) (radius 1000.0) (start-angle 0.3) (end-angle 2.5))"
                text))
    (is (search "(ellipse (id 2) (layer 0) (center-x 6000.0)" text))
    (is (search "(polygon (id 3) (layer 0) (center-x 0.0) (center-y -5000.0) (radius 1000.0) (sides 5)" text))
    (is (equal (entity-values drawing) (entity-values back)))
    (is (string= text (persist:drawing-to-string back)))))

(test a-version-1-file-cannot-hold-the-new-entities
  (let* ((text (persist:drawing-to-string (curve-drawing)))
         (old (replace-once-text text "(format-version 2)" "(format-version 1)")))
    (handler-case (progn (persist:parse-drawing-text old)
                         (fail "a version 1 file with an arc was accepted"))
      (base:drawing-file-content-error (condition)
        (is (search "unknown entity type" (princ-to-string condition)))))))

(test the-reader-refuses-an-arc-or-a-polygon-that-is-not-valid
  (let ((text (persist:drawing-to-string (curve-drawing))))
    (dolist (broken (list (replace-once-text text
                                             "(start-angle 0.3) (end-angle 2.5)"
                                             "(start-angle 1.0) (end-angle 1.0)")
                          (replace-once-text text "(sides 5)" "(sides 2)")
                          (replace-once-text text "(sides 5)" "(sides 5.5)")
                          (replace-once-text text "(radius-x " "(radius-q ")))
      (signals base:drawing-file-error (persist:parse-drawing-text broken)))))

;;; Selecting by crossing

(test a-crossing-takes-arcs-ellipses-and-polygons-by-their-outlines
  (let ((drawing (curve-drawing)))
    (flet ((ids (x1 y1 x2 y2) (crossing-ids drawing x1 y1 x2 y2)))
      (is (equal '(1) (ids 900d0 100d0 1100d0 300d0)) "where the arc passes")
      (is (null (ids -100d0 -100d0 100d0 100d0)) "the center of the arc, which the arc does not reach")
      (is (null (ids 5500d0 -100d0 6500d0 100d0)) "inside the ellipse")
      (is (equal '(2) (ids 8700d0 1100d0 8830d0 1250d0)) "at the end of its long axis")
      (is (null (ids -100d0 -5100d0 100d0 -4900d0)) "inside the polygon")
      (is (equal '(3) (ids 900d0 -4850d0 1050d0 -4750d0)) "at a vertex of the polygon"))))

;;; Tools

(defun tool-clicks (application kind &rest points)
  "Choose the tool KIND from the menu and click each of POINTS, given as (x y) lists in world coordinates."
  (choose-tool application kind)
  (dolist (point points)
    (click application (first point) (second point))))

(test the-arc-tool-takes-a-center-a-start-and-a-direction
  (let ((application (make-test-app)))
    (tool-clicks application :arc '(0 0) '(1000 0))
    (is (= 0 (dom:drawing-entity-count (app:app-drawing application))) "two clicks are not enough")
    (is (search "counter-clockwise" (app:tool-prompt (app:app-tool application) application)))
    (send application (pointer application :move nil 0 500))
    (is (= 1 (length (app:tool-preview-entities (app:app-tool application) application)))
        "the arc is previewed once the start is placed")
    (click application 0 1000)
    (let ((arc (last-entity application)))
      (is (eq :arc (dom:entity-kind arc)))
      (is (= 1000d0 (dom:arc-radius arc)))
      (is (= 0d0 (dom:arc-start-angle arc)))
      (is (< (abs (- (/ pi 2) (dom:arc-end-angle arc))) 1d-12))
      (is (< (abs (- (/ pi 2) (dom:arc-sweep arc))) 1d-12)))
    (is (null (app:tool-preview-entities (app:app-tool application) application)))))

(test the-arc-tool-refuses-a-start-on-its-center-and-escape-abandons-it
  (let ((application (make-test-app)))
    (tool-clicks application :arc '(0 0) '(0 0))
    (is (search "must be apart" (message-text application)))
    (is (= 0 (dom:drawing-entity-count (app:app-drawing application))))
    (click application 1000 0)
    (press application :escape)
    (is (not (app:tool-in-progress-p (app:app-tool application))))
    (is (= 0 (dom:drawing-entity-count (app:app-drawing application))))))

(test the-ellipse-tool-takes-a-center-the-end-of-an-axis-and-the-other-half-axis
  (let ((application (make-test-app)))
    (tool-clicks application :ellipse '(0 0) '(2000 0) '(500 400))
    (let ((ellipse (last-entity application)))
      (is (eq :ellipse (dom:entity-kind ellipse)))
      (is (= 2000d0 (dom:ellipse-radius-x ellipse)))
      (is (< (abs (- 400d0 (dom:ellipse-radius-y ellipse))) 1d-9) "the distance from the axis, not from the center")
      (is (< (abs (dom:ellipse-rotation ellipse)) 1d-12)))
    (tool-clicks application :ellipse '(0 1000) '(0 2000) '(-300 1500))
    (let ((ellipse (last-entity application)))
      (is (= 1000d0 (dom:ellipse-radius-x ellipse)))
      (is (< (abs (- 300d0 (dom:ellipse-radius-y ellipse))) 1d-9))
      (is (< (abs (- (/ pi 2) (dom:ellipse-rotation ellipse))) 1d-12) "an axis that points up"))))

(test the-polygon-tool-takes-a-center-and-a-vertex-and-keeps-the-number-of-sides
  (let ((application (make-test-app)))
    (choose-tool application :polygon)
    (is (search "6 sides" (app:tool-prompt (app:app-tool application) application)))
    (press application :up)
    (press application :up)
    (is (= 8 (app:app-polygon-sides application)))
    (is (search "8 sides" (app:tool-prompt (app:app-tool application) application)))
    (click application 0 0)
    (click application 1000 0)
    (let ((polygon (last-entity application)))
      (is (eq :polygon (dom:entity-kind polygon)))
      (is (= 8 (dom:polygon-sides polygon)))
      (is (= 1000d0 (dom:polygon-radius polygon)))
      (is (< (abs (dom:polygon-rotation polygon)) 1d-12)))
    (click-widget application :select)
    (choose-tool application :polygon)
    (is (= 8 (app:app-polygon-sides application)) "the next polygon has the same number of sides")
    (dotimes (index 20) (press application :down))
    (is (= 3 (app:app-polygon-sides application)) "three sides is the fewest")
    (dotimes (index 2000) (press application :up))
    (is (= dom:+maximum-polygon-sides+ (app:app-polygon-sides application)))))

(test the-letters-of-the-new-tools-choose-them
  (let ((application (make-test-app)))
    (is (eq :arc (select-tool-with application :a)))
    (is (eq :ellipse (select-tool-with application :e)))
    (is (eq :polygon (select-tool-with application :n)))))

(test the-draw-menu-lists-the-new-tools-in-their-place
  (let ((application (make-test-app)))
    (click-widget application :draw)
    (is (equal '(:line :polyline :rectangle :polygon :circle :arc :ellipse :text :dimension)
               (mapcar (lambda (row) (second (ui:widget-id row))) (menu-rows application))))
    (is (equal '("L" "P" "R" "N" "C" "A" "E" "T" "D") (mapcar #'ui:widget-shortcut (menu-rows application))))
    (click-widget application '(:menu-item :arc))
    (is (eq :arc (app:tool-kind (app:app-tool application))))
    (is (eq :arc (ui:widget-icon (find-widget application :draw))))))

;;; In the window

(defun lines-drawn (drawing scale)
  "Return how many lines the scene of DRAWING takes at SCALE pixels per millimetre."
  (let ((application (make-test-app :drawing drawing)))
    (set-test-view application :scale scale)
    (app:app-render application)
    (scene:draw-list-line-count (app:app-draw-list application))))

(test the-new-entities-are-drawn-with-more-segments-the-larger-they-look
  (let ((drawing (dom:make-drawing))
        (empty (dom:make-drawing)))
    (dom:apply-changes drawing
                       (dom:create-entities-changes
                        (list (an-arc :id 1 :start 0.3d0 :end 2.5d0)
                              (an-ellipse :id 2 :rx 1500d0 :ry 800d0 :rotation 0.4d0)
                              (a-polygon :id 3 :radius 600d0 :sides 5))))
    (let ((small (- (lines-drawn drawing 0.02d0) (lines-drawn empty 0.02d0)))
          (large (- (lines-drawn drawing 0.2d0) (lines-drawn empty 0.2d0))))
      (is (plusp small))
      (is (> large small) "at a larger scale the curves are cut finer: ~d against ~d" large small)
      (is (< large 2000) "but not absurdly fine"))))

(test the-inspector-edits-the-new-entities
  (let ((application (make-test-app :drawing (curve-drawing))))
    (app:select-ids application '(3))
    (is (string= "5" (ui:widget-label (find-widget application '(:property :sides)))))
    (click-widget application '(:property :sides))
    (type-text application "7")
    (press application :enter)
    (is (= 7 (dom:polygon-sides (first (app:selected-entities application)))))
    (app:select-ids application '(1))
    (is (find-widget application '(:property :end-angle)))
    (click-widget application '(:property :end-angle))
    (type-text application "180")
    (press application :enter)
    (is (< (abs (- pi (dom:arc-end-angle (first (app:selected-entities application))))) 1d-12))
    (app:select-ids application '(2))
    (click-widget application '(:property :radius-y))
    (type-text application "500")
    (press application :enter)
    (is (= 500d0 (dom:ellipse-radius-y (first (app:selected-entities application)))))))

(test the-new-entities-survive-a-copy-and-a-paste-into-another-window
  (let ((source (make-test-app :drawing (curve-drawing)))
        (target (make-test-app)))
    (share-clipboard source target)
    (app:operation-select-all source)
    (press source :c :control)
    (press target :v :control :shift)
    (is (equal (entity-signatures source) (entity-signatures target)))
    (is (equal '(:arc :ellipse :polygon) (mapcar #'dom:entity-kind (entities-of target))))))

(test the-new-entities-and-their-tools-are-in-portuguese-in-portuguese
  (i18n:with-language (:pt-br)
    (let ((application (make-test-app :drawing (curve-drawing)))
          (leftovers '()))
      (dolist (id '(1 2 3))
        (app:select-ids application (list id))
        (let ((found (untranslated-texts (render-texts application))))
          (when found (push (list id found) leftovers))))
      (dolist (kind '(:arc :ellipse :polygon))
        (choose-tool application kind)
        (let ((found (untranslated-texts (render-texts application))))
          (when found (push (list kind found) leftovers))))
      (is (null leftovers) "English text on the screen: ~s" leftovers)
      (is (search "Arco" (app:tool-prompt (progn (choose-tool application :arc) (app:app-tool application))
                                          application))))))
