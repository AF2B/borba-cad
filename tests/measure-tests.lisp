(in-package #:borba-cad/tests)

(def-suite measuring
  :description "Measuring distances, angles and areas: the geometry, the tools, the menu and the languages."
  :in all)

(in-suite measuring)

;;; The geometry

(test a-contour-encloses-the-area-the-shoelace-formula-gives
  (is (= 1000000d0 (geo:contour-area (doubles 0d0 0d0 1000d0 0d0 1000d0 1000d0 0d0 1000d0))))
  (is (= 500000d0 (geo:contour-area (doubles 0d0 0d0 1000d0 0d0 0d0 1000d0))) "a right triangle")
  (is (= 1000000d0 (geo:contour-area (doubles 0d0 1000d0 1000d0 1000d0 1000d0 0d0 0d0 0d0)))
      "clockwise is the same area")
  (is (= 1000000d0 (geo:contour-area (doubles 5d6 5d6 5d6 5.001d6 5.001d6 5.001d6 5.001d6 5d6)))
      "far from the origin")
  (is (= 200d0 (geo:contour-area (doubles 0d0 0d0 30d0 0d0 30d0 10d0 20d0 10d0 20d0 5d0 0d0 5d0 0d0 0d0)))
      "a corner that comes back to the start adds nothing"))

(test a-contour-without-an-inside-has-no-area
  (is (= 0d0 (geo:contour-area (doubles))))
  (is (= 0d0 (geo:contour-area (doubles 1d0 2d0))))
  (is (= 0d0 (geo:contour-area (doubles 0d0 0d0 10d0 0d0))) "two corners")
  (is (= 0d0 (geo:contour-area (doubles 0d0 0d0 10d0 10d0 20d0 20d0))) "three on a line")
  (is (= 0d0 (geo:contour-area (doubles 0d0 0d0 10d0 10d0 10d0 0d0 0d0 10d0))) "a bow tie: its two loops cancel"))

(test the-angle-between-two-rays-is-the-smaller-one
  (is (< (abs (- (/ pi 2) (geo:angle-between-rays 0d0 0d0 1d0 0d0 0d0 1d0))) 1d-12))
  (is (< (abs (- (/ pi 2) (geo:angle-between-rays 0d0 0d0 0d0 1d0 1d0 0d0))) 1d-12) "the order does not matter")
  (is (< (abs (- pi (geo:angle-between-rays 0d0 0d0 1d0 0d0 -1d0 0d0))) 1d-12) "opposite rays")
  (is (< (abs (geo:angle-between-rays 3d0 3d0 4d0 4d0 40d0 40d0)) 1d-12) "the same ray")
  (is (< (abs (- (/ pi 4) (geo:angle-between-rays 10d0 10d0 20d0 10d0 20d0 20d0))) 1d-12) "from another vertex")
  (is (< (abs (- (* 3 (/ pi 4)) (geo:angle-between-rays 0d0 0d0 1d0 0d0 -1d0 1d0))) 1d-12) "an obtuse one"))

(test a-ray-with-no-length-has-no-angle
  (is (null (geo:angle-between-rays 1d0 1d0 1d0 1d0 5d0 5d0)))
  (is (null (geo:angle-between-rays 1d0 1d0 5d0 5d0 1d0 1d0)))
  (is (null (geo:angle-between-rays 1d0 1d0 1d0 1d0 1d0 1d0))))

;;; The tools

(defun measuring-app ()
  "Return an application, with the grid snap and the object snap off, holding a line, to be measured on."
  (let ((application (make-test-app)))
    (dom:set-snap-to-grid (app:app-drawing application) nil)
    (dom:set-object-snap (app:app-drawing application) nil)
    (app:commit-changes application
                        (dom:create-entity-changes (snap-line 1 -2000 -1500 2000 -1500))
                        "entity.create")
    application))

(defun preview-of (application)
  "Return the entities the active tool of APPLICATION draws now."
  (app:tool-preview-entities (app:app-tool application) application))

(defun preview-kinds (application)
  "Return the kinds of the entities the active tool draws now, sorted."
  (sort (mapcar #'dom:entity-kind (preview-of application)) #'string<))

(defun preview-label (application)
  "Return the text of the label the active tool draws, or NIL."
  (let ((text (find :text (preview-of application) :key #'dom:entity-kind)))
    (and text (dom:text-string text))))

(defun measured-drawing-count (application)
  "Return how many entities the drawing of APPLICATION holds."
  (dom:drawing-entity-count (app:app-drawing application)))

(test the-measure-button-opens-a-list-of-the-tools-that-measure
  (let ((application (measuring-app)))
    (click-widget application :measure)
    (is (equal '((:menu-item :measure-distance) (:menu-item :measure-angle) (:menu-item :measure-area))
               (mapcar #'ui:widget-id (menu-rows application))))
    (is (equal '("M" "Shift+A" "Shift+Q") (mapcar #'ui:widget-shortcut (menu-rows application))))
    (click-widget application '(:menu-item :measure-angle))
    (is (eq :measure-angle (app:tool-kind (app:app-tool application))))
    (is (ui:widget-active (find-widget application :measure)) "the Measure button lights up")
    (is (not (ui:widget-active (find-widget application :draw))))
    (is (not (ui:widget-active (find-widget application :modify))))))

(test the-keys-choose-the-measuring-tools-and-need-no-selection
  (let ((application (measuring-app)))
    (is (zerop (hash-table-count (app:app-selection application))))
    (press application :m)
    (is (eq :measure-distance (app:tool-kind (app:app-tool application))))
    (press application :escape)
    (press application :a :shift)
    (is (eq :measure-angle (app:tool-kind (app:app-tool application))))
    (press application :escape)
    (press application :q :shift)
    (is (eq :measure-area (app:tool-kind (app:app-tool application))))
    (press application :escape)
    (press application :a)
    (is (eq :arc (app:tool-kind (app:app-tool application))) "the plain letter is still the arc")))

(test distance-takes-two-points-and-says-how-far-apart-they-are
  (let ((application (measuring-app)))
    (press application :m)
    (click application 0 0)
    (move-pointer-to application 1200 900)
    (is (equal '(:line :text) (preview-kinds application)) "a line and a label while the second point is chosen")
    (is (search "1500" (preview-label application)) "live: the distance at the pointer")
    (click application 1200 900)
    (is (search "1500" (message-text application)))
    (is (search "dx 1200" (message-text application)))
    (is (search "dy 900" (message-text application)))
    (is (search "36.87" (message-text application)) "the direction, in degrees")
    (is (search "1500" (preview-label application)) "the result stays on the canvas")
    (is (not (app:tool-in-progress-p (app:app-tool application))) "Esc would leave, not cancel")
    (is (= 1 (measured-drawing-count application)) "nothing was drawn into the drawing")))

(test the-next-click-after-a-distance-starts-another
  (let ((application (measuring-app)))
    (press application :m)
    (click application 0 0)
    (click application 1000 0)
    (click application 500 500)
    (is (app:tool-in-progress-p (app:app-tool application)))
    (move-pointer-to application 500 1500)
    (is (search "1000" (preview-label application)) "the new measurement is from the new point")
    (click application 500 1500)
    (is (search "1000" (message-text application)))))

(test escape-leaves-a-measuring-tool-and-takes-its-drawing-away
  (let ((application (measuring-app)))
    (press application :m)
    (click application 0 0)
    (press application :escape)
    (is (eq :measure-distance (app:tool-kind (app:app-tool application))) "the first Esc cancels the measurement")
    (is (null (preview-of application)))
    (click application 0 0)
    (click application 1000 0)
    (is (preview-of application))
    (press application :escape)
    (is (eq :select (app:tool-kind (app:app-tool application))))
    (is (null (preview-of application)))))

(test two-points-in-one-place-are-not-a-distance
  (let ((application (measuring-app)))
    (press application :m)
    (click application 100 100)
    (click application 100 100)
    (is (eq :error (cdr (app:app-message application))))
    (is (search "apart" (message-text application)))
    (is (app:tool-in-progress-p (app:app-tool application)) "and it waits for another")))

(test a-distance-can-be-typed-and-held-to-an-axis
  (let ((application (measuring-app)))
    (press application :m)
    (click application 0 0)
    (press application :f8)
    (move-pointer-to application 800 100)
    (typed-point application "2000")
    (is (search "2000" (message-text application)))
    (is (search "dy 0" (message-text application)) "ortho held it horizontal")))

(test the-distance-is-in-the-display-unit-of-the-drawing
  (let ((application (measuring-app)))
    (dom:set-display-unit (app:app-drawing application) :meter)
    (press application :m)
    (click application 0 0)
    (click application 1500 0)
    (is (search "1.5" (preview-label application)))
    (is (search "m" (preview-label application)))))

(test angle-takes-the-vertex-and-a-point-on-each-arm
  (let ((application (measuring-app)))
    (press application :a :shift)
    (click application 0 0)
    (click application 1000 0)
    (is (member :line (preview-kinds application)) "the first arm is there, and the second follows the pointer")
    (move-pointer-to application 0 1000)
    (is (equal '(:arc :line :line :text) (preview-kinds application)))
    (is (search "90.00" (preview-label application)))
    (click application 0 1000)
    (is (search "90.00" (message-text application)))
    (is (search "270.00" (message-text application)) "and the other side")
    (is (search "90.00" (preview-label application)) "the result stays")
    (is (= 1 (measured-drawing-count application)))))

(test an-angle-is-the-smaller-whichever-way-the-arms-turn
  (loop for (first-x first-y second-x second-y expected) in '((1000 0 0 1000 "Angle: 90.00")
                                                              (0 1000 1000 0 "Angle: 90.00")
                                                              (1000 0 -1000 1000 "Angle: 135.00")
                                                              (-1000 1000 1000 0 "Angle: 135.00"))
        do (let ((application (measuring-app)))
             (press application :a :shift)
             (click application 0 0)
             (click application first-x first-y)
             (click application second-x second-y)
             (is (equal '(:arc :line :line :text) (preview-kinds application)) "~a" expected)
             (is (search expected (message-text application)) "~a" expected))))

(test straight-arms-measure-a-half-turn
  (let ((application (measuring-app)))
    (press application :a :shift)
    (click application 0 0)
    (click application 1000 0)
    (click application -1000 0)
    (is (search "180.00" (message-text application)))))

(test an-arm-that-ends-on-the-vertex-is-refused
  (let ((application (measuring-app)))
    (press application :a :shift)
    (click application 0 0)
    (click application 0 0)
    (is (eq :error (cdr (app:app-message application))))
    (is (null (preview-of application)))))

(test area-takes-the-corners-and-closes-with-enter
  (let ((application (measuring-app)))
    (press application :q :shift)
    (click application 0 0)
    (click application 1000 0)
    (click application 1000 1000)
    (move-pointer-to application 0 1000)
    (is (equal '(:polyline :text) (preview-kinds application)) "the outline and the area so far")
    (is (search "1000000" (preview-label application)))
    (click application 0 1000)
    (press application :enter)
    (is (search "Area:" (message-text application)))
    (is (search "1000000" (message-text application)))
    (is (search "4000" (message-text application)) "the perimeter")
    (is (search "1000000" (preview-label application)) "the result stays")
    (is (= 1 (measured-drawing-count application)))))

(test a-click-on-the-first-corner-closes-the-contour
  (let ((application (measuring-app)))
    (press application :q :shift)
    (click application 0 0)
    (click application 1000 0)
    (click application 1000 1000)
    (click application 0 0)
    (is (search "500000" (message-text application)) "a triangle")
    (is (not (app:tool-in-progress-p (app:app-tool application))))))

(test the-next-click-after-an-area-starts-another-contour
  (let ((application (measuring-app)))
    (press application :q :shift)
    (click application 0 0)
    (click application 1000 0)
    (click application 0 1000)
    (press application :enter)
    (click application 1000 1000)
    (is (app:tool-in-progress-p (app:app-tool application)))
    (is (null (preview-label application)) "the old label is gone")
    (click application 1500 1000)
    (click application 1500 1500)
    (press application :enter)
    (is (search "125000" (message-text application)))))

(test backspace-takes-the-last-corner-back
  (let ((application (measuring-app)))
    (press application :q :shift)
    (click application 0 0)
    (click application 1000 0)
    (click application 1000 1000)
    (click application 0 1000)
    (press application :backspace)
    (press application :enter)
    (is (search "500000" (message-text application)) "three corners are left")))

(test a-contour-needs-three-corners-and-an-inside
  (let ((application (measuring-app)))
    (press application :q :shift)
    (press application :enter)
    (is (null (message-text application)) "Enter means nothing before the first corner")
    (click application 0 0)
    (click application 1000 0)
    (press application :enter)
    (is (eq :error (cdr (app:app-message application))))
    (is (search "three corners" (message-text application)))
    (click application 2000 0)
    (press application :enter)
    (is (eq :error (cdr (app:app-message application))))
    (is (search "no area" (message-text application)))
    (is (app:tool-in-progress-p (app:app-tool application)) "it can still be corrected")))

(test a-corner-cannot-be-put-twice-in-a-row
  (let ((application (measuring-app)))
    (press application :q :shift)
    (click application 100 100)
    (click application 100 100)
    (is (eq :error (cdr (app:app-message application))))))

(test measuring-changes-neither-the-drawing-nor-the-history
  (let ((application (measuring-app)))
    (press application :escape)
    (let ((before (mapcar #'the-entity-signature (entities-of application))))
      (dolist (key '(:m :a :q))
        (press application key (if (eq key :m) nil :shift))
        (click application 0 0)
        (click application 1000 0)
        (click application 0 1000)
        (press application :enter)
        (press application :escape)
        (press application :escape))
      (is (equal before (mapcar #'the-entity-signature (entities-of application))))
      (app:operation-undo application)
      (is (= 0 (measured-drawing-count application)) "one undo takes back the line: measuring left nothing to undo"))))

;;; Languages

(test the-measure-menu-and-the-prompts-speak-portuguese-and-russian
  (dolist (language '(:pt-br :ru-ru))
    (i18n:with-language (language)
      (let ((application (measuring-app)))
        (click-widget application :measure)
        (is (null (untranslated-texts (remove nil (mapcar #'ui:widget-label (app:build-widgets application)))
                                      language))
            "the menu in ~a" language)
        (click-widget application '(:menu-item :measure-distance))
        (is (string/= "Distance: click the first point" (app:tool-prompt (app:app-tool application) application)))
        (click application 0 0)
        (click application 1000 0)
        (is (string/= "Distance: click a new first point; Esc leaves"
                      (app:tool-prompt (app:app-tool application) application)))
        (is (not (search "Distance:" (message-text application))) "the result is said in ~a" language)))))
