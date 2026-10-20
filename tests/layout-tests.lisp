(in-package #:borba-cad/tests)

(def-suite layouts
  :description "The layout: the sheets shown in the program, their viewports and what the pointer does to them."
  :in all)

(in-suite layouts)

;;; Helpers

(defun layout-app (&rest entities)
  "Return an application that holds ENTITIES (a plan 12 m by 8 m when none are given), with the layout closed.

ENTITIES: the entities."
  (apply #'reshaping-app (or entities
                             (list (spread-rectangle 1 0 0 12000 8000)
                                   (cut-line 2 6000 0 6000 8000)
                                   (cut-circle 3 9000 4000 1500)))))

(defun open-layout (application)
  "Open the layout of APPLICATION with Ctrl+L.

APPLICATION: the application."
  (press application :l :control)
  (is (app:layout-mode-p application)))

(defun sheet-of (application &optional (index 0))
  "Return a sheet of the drawing of APPLICATION.

APPLICATION: the application.
INDEX: its position."
  (dom:sheet-at (app:app-drawing application) index))

(defun viewport-of (application &optional (position 0) (index 0))
  "Return a viewport of a sheet of the drawing of APPLICATION.

APPLICATION: the application.
POSITION: the position of the viewport on the sheet.
INDEX: the position of the sheet."
  (nth position (dom:sheet-viewports (sheet-of application index))))

(defun sheet-point-of (viewport x y)
  "Return where a point of the drawing is on the sheet through a viewport, as two values.

VIEWPORT: the viewport.
X: x in the drawing.
Y: y in the drawing."
  (values (+ (dom:viewport-x viewport) (/ (dom:viewport-width viewport) 2)
             (* (dom:viewport-scale viewport) (- x (dom:viewport-center-x viewport))))
          (+ (dom:viewport-y viewport) (/ (dom:viewport-height viewport) 2)
             (* (dom:viewport-scale viewport) (- y (dom:viewport-center-y viewport))))))

(defun button-ids (application)
  "Return the ids of the buttons of the layout.

APPLICATION: the application."
  (loop for row in (app:layout-button-rows application)
        append (remove nil (mapcar #'first row))))

(defun button-enabled-p (application id)
  "Return true when a button of the layout is enabled.

APPLICATION: the application.
ID: the id of the button."
  (fourth (find id (apply #'append (app:layout-button-rows application)) :key #'first)))

;;; Opening and leaving

(test the-layout-opens-with-a-sheet-that-shows-all-of-the-drawing
  (let ((application (layout-app)))
    (is (not (app:layout-mode-p application)))
    (is (zerop (dom:sheet-count (app:app-drawing application))))
    (open-layout application)
    (is (= 1 (dom:sheet-count (app:app-drawing application))) "a first sheet was made")
    (let* ((sheet (sheet-of application))
           (viewport (first (dom:sheet-viewports sheet)))
           (window (dom:viewport-window viewport)))
      (is (equal "Sheet 1" (dom:sheet-name sheet)))
      (is (eq :a3 (dom:sheet-paper sheet)))
      (is (= 1 (length (dom:sheet-viewports sheet))))
      (is (<= (geo:bounds-min-x window) 0d0))
      (is (>= (geo:bounds-max-x window) 12000d0))
      (is (>= (geo:bounds-max-y window) 8000d0))
      (is (member (dom:viewport-scale viewport) '(0.02d0 0.01d0 0.04d0 0.025d0 0.0133d0) :test #'approx=)
          "one of the usual scales: ~a" (dom:viewport-scale viewport)))
    (is (eq :layout (app:tool-kind (app:app-tool application))))
    (is (search "Layout" (app:tool-prompt (app:app-tool application) application)))))

(test the-layout-is-left-with-the-drawing-as-it-was
  (let* ((application (layout-app))
         (before (app:app-viewport application)))
    (open-layout application)
    (is (not (eq before (app:app-viewport application))) "the view is the view of the sheet now")
    (send application (app:make-wheel-event 2d0 400d0 300d0))
    (press application :l :control)
    (is (not (app:layout-mode-p application)))
    (is (eq :select (app:tool-kind (app:app-tool application))))
    (is (approx= (view:viewport-scale before) (view:viewport-scale (app:app-viewport application))))
    (is (approx= (view:viewport-center-x before) (view:viewport-center-x (app:app-viewport application))))))

(test the-layout-can-be-opened-from-the-chip-the-more-menu-and-the-palette
  (let ((application (layout-app)))
    (click-widget application :layout)
    (is (app:layout-mode-p application))
    (is (ui:widget-active (find-widget application :layout)) "the chip is lit")
    (click-widget application :layout)
    (is (not (app:layout-mode-p application)))
    (click-widget application :more)
    (is (member '(:menu-item :layout) (mapcar #'ui:widget-id (menu-rows application)) :test #'equal))
    (click-widget application '(:menu-item :layout))
    (is (app:layout-mode-p application))
    (press application :k :control)
    (type-text application "show or leave")
    (is (member "Layout: Show or leave the sheets" (mapcar #'app:palette-entry-label
                                                          (app:palette-prompt-matches (app:app-prompt application)))
                :test #'string=))
    (press application :enter)
    (is (not (app:layout-mode-p application)))))

(test the-layout-keeps-the-sheets-it-already-has
  (let ((application (layout-app)))
    (ran-ok (script-env (app:app-drawing application)) "(sheet \"Mine\" :paper :a2) (sheet \"Other\")")
    (open-layout application)
    (is (equal '("Mine" "Other") (mapcar #'dom:sheet-name (dom:drawing-sheets (app:app-drawing application)))))
    (is (equal "Mine" (dom:sheet-name (app:layout-sheet application))) "the first is shown")))

(test an-empty-drawing-gets-a-sheet-with-a-window-all-the-same
  (let ((application (make-test-app)))
    (open-layout application)
    (is (= 1 (length (dom:sheet-viewports (sheet-of application)))))
    (is (approx= 0.02d0 (dom:viewport-scale (viewport-of application))))
    (app:app-render application)
    (is (plusp (scene:draw-list-line-count (app:app-draw-list application))))))

(test going-to-a-tool-of-the-drawing-leaves-the-layout
  (let ((application (layout-app)))
    (open-layout application)
    (press application :l)
    (is (not (app:layout-mode-p application)))
    (is (eq :line (app:tool-kind (app:app-tool application))))))

(test escape-deselects-and-then-leaves
  (let ((application (layout-app)))
    (open-layout application)
    (click application 200 150)
    (is (app:selected-viewport application))
    (press application :escape)
    (is (null (app:selected-viewport application)))
    (is (app:layout-mode-p application))
    (press application :escape)
    (is (not (app:layout-mode-p application)))))

;;; The buttons

(test the-buttons-are-there-and-the-ones-that-need-a-sheet-viewport-wait-for-one
  (let ((application (layout-app)))
    (open-layout application)
    (is (equal '(:layout-model :layout-previous :layout-next :layout-add-sheet :layout-sheet :layout-delete-sheet
                 :layout-new-viewport :layout-viewport :layout-delete-viewport :layout-export)
               (button-ids application)))
    (is (not (button-enabled-p application :layout-viewport)))
    (is (not (button-enabled-p application :layout-delete-viewport)))
    (is (not (button-enabled-p application :layout-previous)) "there is one sheet")
    (click application 200 150)
    (is (button-enabled-p application :layout-viewport))
    (is (button-enabled-p application :layout-delete-viewport))
    (is (find-widget application :layout-model))
    (is (search "1/1" (ui:widget-label (find-widget application :layout-sheet-name))))))

;;; The pointer on the viewports

(test a-click-selects-the-viewport-it-is-in-and-a-click-elsewhere-deselects
  (let ((application (layout-app)))
    (open-layout application)
    (is (null (app:selected-viewport application)))
    (click application 200 150)
    (is (eq (viewport-of application) (app:selected-viewport application)))
    (click application 200 20)
    (is (null (app:selected-viewport application)) "off every viewport, on the margin")))

(test a-drag-moves-the-viewport-and-what-it-shows-goes-with-it
  (let ((application (layout-app)))
    (open-layout application)
    (let* ((before (viewport-of application))
           (revision (dom:drawing-revision (app:app-drawing application))))
      (drag application 200 150 220 130)
      (let ((after (viewport-of application)))
        (is (approx= (+ (dom:viewport-x before) 20d0) (dom:viewport-x after)))
        (is (approx= (- (dom:viewport-y before) 20d0) (dom:viewport-y after)))
        (is (= (dom:viewport-width before) (dom:viewport-width after)))
        (is (= (dom:viewport-center-x before) (dom:viewport-center-x after)) "it still shows the same")
        (is (= (dom:viewport-scale before) (dom:viewport-scale after))))
      (is (< revision (dom:drawing-revision (app:app-drawing application))) "the drawing has changed")
      (is (app:app-dirty-p application)))))

(test the-viewport-follows-the-pointer-while-it-is-dragged-and-escape-gives-it-back
  (let ((application (layout-app)))
    (open-layout application)
    (let ((before (viewport-of application)))
      (send application
            (pointer application :move nil 200 150)
            (pointer application :down :left 200 150)
            (pointer application :move nil 230 150))
      (is (dom:viewport-p (app:layout-state-preview (app:app-sheet-view application))))
      (is (eq before (viewport-of application)) "not yet changed")
      (app:app-render application)
      (press application :escape)
      (is (null (app:layout-state-preview (app:app-sheet-view application))))
      (send application (pointer application :up :left 230 150))
      (is (eq before (viewport-of application)) "nothing changed"))))

(test a-corner-is-resized-and-what-the-viewport-shows-stays-where-it-is-on-the-sheet
  (let ((application (layout-app)))
    (open-layout application)
    (click application 200 150)
    (let* ((before (viewport-of application)))
      (multiple-value-bind (anchor-x anchor-y) (sheet-point-of before 6000d0 4000d0)
        (drag application 405 282 305 182)
        (let ((after (viewport-of application)))
          (is (approx= 15d0 (dom:viewport-x after)))
          (is (approx= 42d0 (dom:viewport-y after)))
          (is (approx= 290d0 (dom:viewport-width after)))
          (is (approx= 140d0 (dom:viewport-height after)))
          (is (= (dom:viewport-scale before) (dom:viewport-scale after)) "the scale does not change")
          (multiple-value-bind (x y) (sheet-point-of after 6000d0 4000d0)
            (is (approx= anchor-x x))
            (is (approx= anchor-y y))))))))

(test a-sheet-viewport-cannot-be-resized-to-nothing
  (let ((application (layout-app)))
    (open-layout application)
    (click application 200 150)
    (let ((before (viewport-of application)))
      (drag application 405 282 17 44)
      (is (eq before (viewport-of application)) "two millimetres is too small"))))

(test the-arrow-keys-nudge-the-selected-viewport
  (let ((application (layout-app)))
    (open-layout application)
    (let ((x (dom:viewport-x (viewport-of application)))
          (y (dom:viewport-y (viewport-of application))))
      (press application :right)
      (is (approx= x (dom:viewport-x (viewport-of application))) "nothing is selected, nothing moves")
      (click application 200 150)
      (press application :right)
      (is (approx= (+ x 1d0) (dom:viewport-x (viewport-of application))))
      (press application :up :shift)
      (is (approx= (+ y 10d0) (dom:viewport-y (viewport-of application))))
      (press application :left)
      (press application :down)
      (is (approx= x (dom:viewport-x (viewport-of application))))
      (is (approx= (+ y 9d0) (dom:viewport-y (viewport-of application)))))))

(test delete-takes-the-selected-viewport-away
  (let ((application (layout-app)))
    (open-layout application)
    (click application 200 150)
    (press application :delete)
    (is (null (dom:sheet-viewports (sheet-of application))))
    (is (null (app:selected-viewport application)))
    (is (app:layout-mode-p application) "the layout stays")
    (press application :delete)
    (is (app:layout-mode-p application) "and a second Delete has nothing to take")))

;;; New viewports

(test new-viewport-arms-the-next-drag-which-puts-a-sheet-viewport-on-the-sheet
  (let ((application (layout-app)))
    (open-layout application)
    (click application 200 150)
    (press application :delete)
    (click-widget application :layout-new-viewport)
    (is (app:layout-state-arming (app:app-sheet-view application)))
    (is (ui:widget-active (find-widget application :layout-new-viewport)))
    (is (search "drag a rectangle" (app:tool-prompt (app:app-tool application) application)))
    (drag application 30 60 230 200)
    (is (not (app:layout-state-arming (app:app-sheet-view application))) "one viewport for one click of the button")
    (is (= 1 (length (dom:sheet-viewports (sheet-of application)))))
    (let ((viewport (viewport-of application)))
      (is (approx= 30d0 (dom:viewport-x viewport)))
      (is (approx= 200d0 (dom:viewport-width viewport)))
      (is (approx= 140d0 (dom:viewport-height viewport)))
      (is (eq viewport (app:selected-viewport application)) "the new one is selected")
      (let ((window (dom:viewport-window viewport)))
        (is (<= (geo:bounds-min-x window) 0d0))
        (is (>= (geo:bounds-max-x window) 12000d0))))))

(test a-rectangle-dragged-from-any-corner-makes-the-same-viewport
  (let ((application (layout-app)))
    (open-layout application)
    (click-widget application :layout-new-viewport)
    (drag application 230 200 30 60)
    (let ((viewport (viewport-of application 1)))
      (is (approx= 30d0 (dom:viewport-x viewport)))
      (is (approx= 60d0 (dom:viewport-y viewport))))))

(test a-rectangle-that-is-too-small-says-so-and-stays-armed
  (let ((application (layout-app)))
    (open-layout application)
    (click-widget application :layout-new-viewport)
    (drag application 30 60 32 62)
    (is (search "too small" (message-text application)))
    (is (= 1 (length (dom:sheet-viewports (sheet-of application)))))
    (is (app:layout-state-arming (app:app-sheet-view application)) "another try")
    (press application :escape)
    (is (not (app:layout-state-arming (app:app-sheet-view application))))
    (is (app:layout-mode-p application) "Escape only took the tool back")))

(test the-button-takes-the-new-viewport-back-when-pressed-again
  (let ((application (layout-app)))
    (open-layout application)
    (click-widget application :layout-new-viewport)
    (click-widget application :layout-new-viewport)
    (is (not (app:layout-state-arming (app:app-sheet-view application))))))

;;; What a viewport shows

(defun viewport-form (application)
  "Open the form of the selected viewport with Enter and return its prompt.

APPLICATION: the application."
  (press application :enter)
  (let ((prompt (app:app-prompt application)))
    (is (eq :form (app:prompt-kind prompt)))
    prompt))

(test enter-asks-what-the-selected-viewport-shows
  (let ((application (layout-app)))
    (open-layout application)
    (click application 200 150)
    (let ((prompt (viewport-form application)))
      (is (equal '(:show :scale :center-x :center-y) (mapcar #'app:form-field-key (app:prompt-fields prompt))))
      (is (eq :keep (cdr (assoc :show (app:form-values prompt)))))
      (is (equal "1:50" (cdr (assoc :scale (app:form-values prompt)))) "12 m across 390 mm needs 1:50"))))

(test a-scale-and-a-center-can-be-typed
  (let ((application (layout-app)))
    (open-layout application)
    (click application 200 150)
    (viewport-form application)
    (press application :tab)
    (press application :a :control)
    (type-text application "1:20")
    (press application :tab)
    (press application :a :control)
    (type-text application "9000")
    (press application :tab)
    (press application :a :control)
    (type-text application "4000")
    (press application :enter)
    (is (null (app:app-prompt application)))
    (let ((viewport (viewport-of application)))
      (is (approx= 0.05d0 (dom:viewport-scale viewport)))
      (is (approx= 9000d0 (dom:viewport-center-x viewport)))
      (is (approx= 4000d0 (dom:viewport-center-y viewport))))))

(test the-form-can-ask-for-all-of-the-drawing-or-the-selection
  (let ((application (layout-app)))
    (open-layout application)
    (click application 200 150)
    (viewport-form application)
    (press application :space)
    (press application :enter)
    (is (null (app:app-prompt application)))
    (let ((window (dom:viewport-window (viewport-of application))))
      (is (<= (geo:bounds-min-x window) 0d0))
      (is (>= (geo:bounds-max-x window) 12000d0)))
    (app:operation-close-layout application)
    (open-layout application)
    (is (null (app:selected-viewport application)) "going in and out forgot the selection")))

(test the-form-can-ask-for-the-selection-of-the-drawing
  (let ((application (layout-app)))
    (open-layout application)
    (click application 200 150)
    (app:select-ids application '(3))
    (viewport-form application)
    (press application :space)
    (press application :space)
    (press application :enter)
    (let ((viewport (viewport-of application)))
      (is (approx= 9000d0 (dom:viewport-center-x viewport)) "the circle")
      (is (approx= 4000d0 (dom:viewport-center-y viewport)))
      (let ((window (dom:viewport-window viewport)))
        (is (<= (geo:bounds-min-x window) 7500d0))
        (is (>= (geo:bounds-max-x window) 10500d0))))))

(test a-form-that-cannot-be-done-stays-and-says-why
  (let ((application (layout-app)))
    (open-layout application)
    (click application 200 150)
    (viewport-form application)
    (press application :tab)
    (press application :a :control)
    (type-text application "big")
    (press application :enter)
    (is (eq :form (app:prompt-kind (app:app-prompt application))))
    (is (search "type a scale" (app:prompt-problem (app:app-prompt application))))
    (press application :a :control)
    (type-text application "0")
    (press application :enter)
    (is (search "above zero" (app:prompt-problem (app:app-prompt application))))
    (press application :escape)
    (is (null (app:app-prompt application)))))

(test the-viewport-button-opens-the-form-and-the-delete-button-takes-the-viewport-away
  (let ((application (layout-app)))
    (open-layout application)
    (click application 200 150)
    (click-widget application :layout-viewport)
    (is (eq :form (app:prompt-kind (app:app-prompt application))))
    (press application :escape)
    (click-widget application :layout-delete-viewport)
    (is (null (dom:sheet-viewports (sheet-of application))))))

;;; Standard scales and the corner arithmetic

(test the-usual-scales-are-found-below-what-fits
  (is (= 0.02d0 (app:standard-scale-within 0.0213d0)))
  (is (= 0.02d0 (app:standard-scale-within 0.02d0)) "exactly a standard scale")
  (is (= 0.04d0 (app:standard-scale-within 0.0499d0)))
  (is (= 1d0 (app:standard-scale-within 1.5d0)))
  (is (= 10d0 (app:standard-scale-within 50d0)) "nothing larger than 10:1")
  (is (= 0.0001d0 (app:standard-scale-within 0.0001d0)) "smaller than all of them: as it is"))

(test resizing-a-corner-moves-the-opposite-corner-never
  (let ((viewport (a-sheet-viewport :x 20d0
                                    :y 40d0
                                    :width 100d0
                                    :height 50d0
                                    :center-x 500d0
                                    :center-y 0d0
                                    :scale 0.1d0)))
    (dotimes (corner 4)
      (let* ((corners (app:viewport-corners viewport))
             (opposite (nth (mod (+ corner 2) 4) corners))
             (target (nth corner corners))
             (resized (app:resized-viewport viewport corner (+ (car target) 10d0) (+ (cdr target) 10d0)))
             (resized-corners (app:viewport-corners resized)))
        (is (not (null resized)))
        (is (equal opposite (nth (mod (+ corner 2) 4) resized-corners)) "corner ~d: the opposite one stays" corner)))))

(test dragging-a-corner-across-the-other-flips-the-viewport
  (let* ((viewport (a-sheet-viewport :x 20d0 :y 40d0 :width 100d0 :height 50d0))
         (flipped (app:resized-viewport viewport 2 0d0 0d0)))
    (is (approx= 0d0 (dom:viewport-x flipped)))
    (is (approx= 20d0 (dom:viewport-width flipped)))
    (is (approx= 40d0 (dom:viewport-height flipped)))
    (is (null (app:resized-viewport viewport 2 21d0 41d0)) "a corner a millimetre away leaves it too small")))

;;; Sheets

(test a-new-sheet-takes-the-paper-and-title-block-of-the-one-shown-but-not-its-title
  (let ((application (layout-app)))
    (open-layout application)
    (let ((sheet (sheet-of application)))
      (dom:replace-sheet (app:app-drawing application)
                         0
                         (dom:sheet-with sheet
                                         :paper :a2
                                         :landscape nil
                                         :fields '((:project . "House") (:title . "Plan") (:author . "Ana")))))
    (click-widget application :layout-add-sheet)
    (is (= 2 (dom:sheet-count (app:app-drawing application))))
    (is (= 1 (app:layout-state-sheet-index (app:app-sheet-view application))) "the new sheet is shown")
    (let ((sheet (sheet-of application 1)))
      (is (equal "Sheet 2" (dom:sheet-name sheet)))
      (is (eq :a2 (dom:sheet-paper sheet)))
      (is (not (dom:sheet-landscape sheet)))
      (is (equal "House" (dom:sheet-field sheet :project)))
      (is (equal "Ana" (dom:sheet-field sheet :author)))
      (is (equal "" (dom:sheet-field sheet :title)))
      (is (null (dom:sheet-viewports sheet))))
    (is (search "New viewport" (message-text application)))))

(test the-sheets-are-walked-through-with-the-arrows-and-the-buttons-going-round
  (let ((application (layout-app)))
    (open-layout application)
    (click-widget application :layout-add-sheet)
    (click-widget application :layout-add-sheet)
    (is (= 2 (app:layout-state-sheet-index (app:app-sheet-view application))))
    (click-widget application :layout-next)
    (is (= 0 (app:layout-state-sheet-index (app:app-sheet-view application))) "after the last, the first")
    (click-widget application :layout-previous)
    (is (= 2 (app:layout-state-sheet-index (app:app-sheet-view application))))
    (is (search "Sheet 3  3/3" (ui:widget-label (find-widget application :layout-sheet-name))))))

(test showing-another-sheet-forgets-the-selection-and-fits-the-view
  (let ((application (layout-app)))
    (open-layout application)
    (click application 200 150)
    (click-widget application :layout-add-sheet)
    (is (null (app:selected-viewport application)))
    (let ((view (app:app-viewport application)))
      (is (< 1d0 (view:viewport-scale view)) "an A3 sheet on a screen of 1440 pixels")
      (multiple-value-bind (width height) (plot:sheet-size (sheet-of application 1))
        (declare (ignore height))
        (is (< (* width (view:viewport-scale view)) (view:viewport-width view)))))))

(defun sheet-form (application)
  "Open the form of the properties of the sheet that is shown, with its button, and return its prompt.

APPLICATION: the application."
  (click-widget application :layout-sheet)
  (let ((prompt (app:app-prompt application)))
    (is (eq :form (app:prompt-kind prompt)))
    prompt))

(test the-sheet-form-asks-the-name-the-paper-and-the-title-block
  (let ((application (layout-app)))
    (open-layout application)
    (let ((prompt (sheet-form application)))
      (is (equal '(:name :paper :orientation :project :title :author :date :revision)
                 (mapcar #'app:form-field-key (app:prompt-fields prompt))))
      (is (eq :a3 (cdr (assoc :paper (app:form-values prompt)))))
      (is (eq :landscape (cdr (assoc :orientation (app:form-values prompt))))))
    (press application :a :control)
    (type-text application "Ground floor")
    (press application :tab)
    (press application :space)
    (press application :tab)
    (press application :space)
    (press application :tab)
    (type-text application "House")
    (press application :tab)
    (type-text application "Plan")
    (press application :enter)
    (is (null (app:app-prompt application)))
    (let ((sheet (sheet-of application)))
      (is (equal "Ground floor" (dom:sheet-name sheet)))
      (is (eq :a2 (dom:sheet-paper sheet)) "the next paper in the list, after A3")
      (is (not (dom:sheet-landscape sheet)))
      (is (equal "House" (dom:sheet-field sheet :project)))
      (is (equal "Plan" (dom:sheet-field sheet :title)))
      (is (= 1 (length (dom:sheet-viewports sheet))) "its viewports stay"))))

(test a-sheet-form-that-cannot-be-done-stays-and-says-why
  (let ((application (layout-app)))
    (open-layout application)
    (sheet-form application)
    (press application :a :control)
    (type-text application " ")
    (press application :enter)
    (is (eq :form (app:prompt-kind (app:app-prompt application))))
    (is (search "needs a name" (app:prompt-problem (app:app-prompt application))))
    (press application :escape)
    (is (equal "Sheet 1" (dom:sheet-name (sheet-of application))))))

(test deleting-a-sheet-asks-and-the-last-one-takes-the-layout-with-it
  (let ((application (layout-app)))
    (open-layout application)
    (click-widget application :layout-add-sheet)
    (click-widget application :layout-delete-sheet)
    (is (string= "Delete the sheet" (prompt-title-of application)))
    (is (search "Sheet 2" (app:prompt-message (app:app-prompt application))))
    (choose-in-prompt application 1)
    (is (= 2 (dom:sheet-count (app:app-drawing application))) "cancelled")
    (click-widget application :layout-delete-sheet)
    (choose-in-prompt application 0)
    (is (= 1 (dom:sheet-count (app:app-drawing application))))
    (is (= 0 (app:layout-state-sheet-index (app:app-sheet-view application))))
    (click-widget application :layout-delete-sheet)
    (choose-in-prompt application 0)
    (is (zerop (dom:sheet-count (app:app-drawing application))))
    (is (not (app:layout-mode-p application)) "no sheet to show")))

;;; What is drawn

(test the-sheet-is-drawn-with-its-paper-its-viewports-and-its-title-block
  (let ((application (layout-app)))
    (open-layout application)
    (app:app-render application)
    (let ((result (app:app-last-scene-result application)))
      (is (= 3 (scene:scene-result-drawn-entities result)) "the three entities, through the one viewport"))
    (is (plusp (scene:draw-list-line-count (app:app-draw-list application))))
    (is (plusp (scene:draw-list-triangle-count (app:app-draw-list application))) "the paper and the frame")))

(test what-a-sheet-viewport-has-worked-out-is-kept-until-the-drawing-changes
  (let ((application (layout-app)))
    (open-layout application)
    (let* ((viewport (viewport-of application))
           (first (app:layout-viewport-items application viewport)))
      (is (eq first (app:layout-viewport-items application viewport)) "the same, not worked out again")
      (app:operation-add-sheet application)
      (is (eq first (app:layout-viewport-items application viewport))
          "a change to the sheets alone does not make it work again")
      (app:commit-changes application
                          (dom:create-entities-changes (list (cut-line 9 0 0 100 100)))
                          "entity.create")
      (let ((again (app:layout-viewport-items application viewport)))
        (is (not (eq first again)) "a change to the drawing does")
        (is (= 4 (cdr again)))))))

(test a-hidden-layer-is-not-shown-through-a-sheet-viewport
  (let ((application (layout-app)))
    (open-layout application)
    (let ((shown (cdr (app:layout-viewport-items application (viewport-of application)))))
      (dom:set-layer-visible (app:app-drawing application) 0 nil)
      (is (= 0 (cdr (app:layout-viewport-items application (viewport-of application)))))
      (is (= 3 shown)))))

(test the-cursor-does-not-snap-to-the-drawing-on-the-sheet
  (let ((application (layout-app)))
    (dom:set-object-snap (app:app-drawing application) t)
    (open-layout application)
    (send application (pointer application :move nil 200 150))
    (is (null (app:cursor-snap-kind (app:app-cursor application))))
    (is (approx= 200d0 (app:cursor-x (app:app-cursor application))) 1d-6)))

(test zoom-to-fit-fits-the-sheet-in-the-layout
  (let ((application (layout-app)))
    (open-layout application)
    (send application (app:make-wheel-event 3d0 400d0 300d0))
    (app:operation-zoom-extents application)
    (multiple-value-bind (width height) (plot:sheet-size (sheet-of application))
      (let ((view (app:app-viewport application)))
        (is (approx= (/ width 2) (view:viewport-center-x view)))
        (is (approx= (/ height 2) (view:viewport-center-y view)))))))

;;; Saved with the drawing

(test the-sheets-are-saved-with-the-drawing-and-come-back
  (with-temporary-directory (directory)
    (let ((application (layout-app))
          (path (merge-pathnames "plan.bcad" directory)))
      (open-layout application)
      (drag application 200 150 220 130)
      (is (app:save-app-drawing application (uiop:native-namestring path)))
      (let ((again (make-test-app :drawing (persist:load-drawing path))))
        (is (= 1 (dom:sheet-count (app:app-drawing again))))
        (open-layout again)
        (is (= 1 (length (dom:sheet-viewports (sheet-of again)))))
        (is (approx= (dom:viewport-x (viewport-of application)) (dom:viewport-x (viewport-of again))))))))

;;; Printing

(test the-sheets-are-exported-as-a-pdf-with-a-page-for-each
  (with-temporary-directory (directory)
    (let ((application (layout-app)))
      (setf (app:app-file-path application) (merge-pathnames "plan.bcad" directory))
      (open-layout application)
      (click-widget application :layout-add-sheet)
      (click-widget application :layout-export)
      (is (string= "Export the sheets as PDF" (prompt-title-of application)))
      (is (equal '("Black" "Layer colors" "Cancel") (prompt-labels application)))
      (choose-in-prompt application 0)
      (is (string= "Export the sheets as a PDF file" (dialog-title-text application)))
      (is (equal "plan.pdf" (ui:edit-state-text (app:file-dialog-name-edit (dialog-of application)))))
      (press application :enter)
      (is (string= "PDF exported" (prompt-title-of application)))
      (is (search "with 2 sheets" (app:prompt-message (app:app-prompt application))))
      (let ((text (fixture-like-text (merge-pathnames "plan.pdf" directory))))
        (is (search "%PDF-1.4" text))
        (is (search "/Count 2" text))
        (is (search "(2/2) Tj" text) "the title block of the second sheet says so")))))

(test with-no-sheets-there-is-nothing-to-export
  (let ((application (layout-app)))
    (app:operation-export-sheets application)
    (is (null (app:app-prompt application)))
    (is (search "no sheets yet" (message-text application)))
    (is (eq :error (cdr (app:app-message application))))))

(test the-sheets-can-be-exported-in-the-colors-of-the-layers
  (with-temporary-directory (directory)
    (let ((application (layout-app)))
      (setf (app:app-file-path application) (merge-pathnames "plan.bcad" directory))
      (open-layout application)
      (app:operation-export-sheets application)
      (choose-in-prompt application 1)
      (press application :enter)
      (is (string= "PDF exported" (prompt-title-of application)))
      (is (probe-file (merge-pathnames "plan.pdf" directory))))))
