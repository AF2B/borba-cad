(in-package #:borba-cad/tests)

(def-suite app
  :description "Integration tests: scripted input events drive the whole headless application."
  :in all)

(in-suite app)

;;; Helpers: build an app and speak to it in world coordinates through real input events.

(defun make-test-app (&key telemetry drawing (width 1440d0) (height 900d0))
  "Create an application with a known view: 0.2 px/mm centered on the origin, which shows
about 2.7 m to the left and right of the origin and 2 m above and below it."
  (let ((application (app:make-app (quiet-config)
                                   :telemetry telemetry :drawing drawing :width width :height height)))
    (set-test-view application)
    application))

(defun canvas-of (application)
  "Return the canvas rectangle of APPLICATION's window."
  (ui:layout-canvas (app:app-layout application)))

(defun set-test-view (application &key (scale 0.2d0) (center-x 0d0) (center-y 0d0))
  "Give APPLICATION a view with the given scale and center."
  (let ((canvas (canvas-of application)))
    (setf (app:app-viewport application)
          (view:make-viewport :center-x center-x :center-y center-y :scale scale
                              :width (float (ui:rect-width canvas) 1d0)
                              :height (float (ui:rect-height canvas) 1d0)
                              :origin-x (float (ui:rect-x canvas) 1d0)
                              :origin-y (float (ui:rect-y canvas) 1d0)))))

(defun send (application &rest events)
  "Deliver EVENTS to APPLICATION."
  (dolist (event events)
    (app:app-handle-event application event)))

(defun screen-position (application world-x world-y)
  "Return the window position of a world point as two double values."
  (view:world->screen (app:app-viewport application) (float world-x 1d0) (float world-y 1d0)))

(defun pointer (application action button world-x world-y &optional modifiers)
  "Build a pointer event at a world position."
  (multiple-value-bind (x y) (screen-position application world-x world-y)
    (app:make-pointer-event action button x y modifiers)))

(defun click (application world-x world-y &key (button :left) modifiers)
  "Move to a world position, press and release a button."
  (send application
        (pointer application :move nil world-x world-y modifiers)
        (pointer application :down button world-x world-y modifiers)
        (pointer application :up button world-x world-y modifiers)))

(defun drag (application from-x from-y to-x to-y &key (button :left) modifiers (steps 6))
  "Press at FROM, move in steps and release at TO (world coordinates)."
  (send application
        (pointer application :move nil from-x from-y modifiers)
        (pointer application :down button from-x from-y modifiers))
  (loop for step from 1 to steps
        do (send application
                 (pointer application :move nil
                          (+ from-x (* (- to-x from-x) (/ step (float steps 1d0))))
                          (+ from-y (* (- to-y from-y) (/ step (float steps 1d0))))
                          modifiers)))
  (send application (pointer application :up button to-x to-y modifiers)))

(defun press (application key &rest modifiers)
  "Press and release a key."
  (send application (app:make-key-event key modifiers :down) (app:make-key-event key modifiers :up)))

(defun type-text (application string)
  "Type STRING through text events."
  (send application (app:make-text-event string)))

(defun entities-of (application)
  "Return the drawing's entities in stacking order."
  (drawing-entities (app:app-drawing application)))

(defun last-entity (application)
  "Return the most recently created entity."
  (car (last (entities-of application))))

(defun message-text (application)
  "Return the status message text, or NIL."
  (car (app:app-message application)))

(defun find-widget (application id)
  "Return the widget with ID in the current widget list, or NIL."
  (find id (app:build-widgets application) :key #'ui:widget-id :test #'equal))

(defun click-widget (application id)
  "Click the center of the widget with ID; fails the test when it does not exist."
  (let ((widget (find-widget application id)))
    (unless widget (error "No widget ~s" id))
    (let ((x (float (+ (ui:widget-x widget) (/ (ui:widget-width widget) 2)) 1d0))
          (y (float (+ (ui:widget-y widget) (/ (ui:widget-height widget) 2)) 1d0)))
      (send application
            (app:make-pointer-event :move nil x y)
            (app:make-pointer-event :down :left x y)
            (app:make-pointer-event :up :left x y)))))

(defun choose-tool (application kind)
  "Open the draw menu with the mouse and click the row of the tool KIND."
  (click-widget application :draw)
  (click-widget application (list :menu-item kind)))

(defun select-tool-with (application key)
  "Press the shortcut KEY and check that it activated a tool."
  (press application key)
  (app:tool-kind (app:app-tool application)))

;;; Basics

(test a-new-application-is-empty-clean-and-on-the-select-tool
  (let ((application (make-test-app)))
    (is (eq :select (app:tool-kind (app:app-tool application))))
    (is (= 0 (dom:drawing-entity-count (app:app-drawing application))))
    (is (not (app:app-dirty-p application)))
    (is (string= "Untitled - Borba CAD" (app:app-title application)))
    (is (not (app:app-wants-text-input-p application)))))

(test tool-shortcuts-switch-tools-and-escape-returns-to-select
  (let ((application (make-test-app)))
    (is (eq :line (select-tool-with application :l)))
    (is (eq :rectangle (select-tool-with application :r)))
    (is (eq :circle (select-tool-with application :c)))
    (is (eq :polyline (select-tool-with application :p)))
    (is (eq :text (select-tool-with application :t)))
    (is (eq :dimension (select-tool-with application :d)))
    (press application :escape)
    (is (eq :select (app:tool-kind (app:app-tool application))))))

;;; Creation

(test the-line-tool-creates-a-line-with-two-clicks-and-undo-redo-work
  (let ((application (make-test-app)))
    (press application :l)
    (click application 0 0)
    (is (= 0 (dom:drawing-entity-count (app:app-drawing application))))      ; one click is not enough
    (is (search "end point" (app:tool-prompt (app:app-tool application) application)))
    (click application 1000 0)
    (let ((line (last-entity application)))
      (is (typep line 'dom:line-entity))
      (is (= 0d0 (dom:line-x1 line)))
      (is (= 1000d0 (dom:line-x2 line)))
      (is (= dom:+default-layer-id+ (dom:entity-layer-id line))))
    (is (app:app-dirty-p application))
    (press application :z :control)
    (is (= 0 (dom:drawing-entity-count (app:app-drawing application))))
    (press application :y :control)
    (is (= 1 (dom:drawing-entity-count (app:app-drawing application))))
    ;; Ctrl+Shift+Z is redo: nothing is left to redo.
    (press application :z :control :shift)
    (is (= 1 (dom:drawing-entity-count (app:app-drawing application))))
    (is (search "Nothing to redo" (message-text application)))))

(test clicks-snap-to-the-grid
  (let ((application (make-test-app)))
    (press application :l)
    (click application 1049 1051)
    (click application 2551 -347)
    (let ((line (last-entity application)))
      (is (= 1000d0 (dom:line-x1 line)))
      (is (= 1100d0 (dom:line-y1 line)))
      (is (= 2600d0 (dom:line-x2 line)))
      (is (= -300d0 (dom:line-y2 line))))))

(test with-snapping-off-clicks-are-exact
  (let ((application (make-test-app)))
    (press application :s)                                                    ; toggle grid snap
    (press application :l)
    (click application 1049 1051)
    (click application 2551 -347)
    (let ((line (last-entity application)))
      (is (< (abs (- 1049d0 (dom:line-x1 line))) 1d-6))
      (is (< (abs (- -347d0 (dom:line-y2 line))) 1d-6)))))

(test object-snap-grabs-an-existing-endpoint-off-the-grid
  (let ((application (make-test-app)))
    (press application :s)                                                    ; grid snap off: endpoints are exact
    (press application :l)
    (click application 1037 53)
    (click application 2000 53)
    (click application 1040 56)                                               ; within 12 px (24 mm) of (1037, 53)
    (click application 1040 2000)
    (let ((second (last-entity application)))
      (is (< (abs (- 1037d0 (dom:line-x1 (first (entities-of application))))) 1d-6))
      (is (< (abs (- 1037d0 (dom:line-x1 second))) 1d-6))
      (is (< (abs (- 53d0 (dom:line-y1 second))) 1d-6)))))

(test the-rectangle-tool-accepts-corners-in-any-order
  (let ((application (make-test-app)))
    (press application :r)
    (click application 2000 1500)
    (click application 500 300)
    (let ((rectangle (last-entity application)))
      (is (typep rectangle 'dom:rectangle-entity))
      (is (= 500d0 (dom:rectangle-min-x rectangle)))
      (is (= 300d0 (dom:rectangle-min-y rectangle)))
      (is (= 1500d0 (dom:rectangle-width rectangle)))
      (is (= 1200d0 (dom:rectangle-height rectangle))))))

(test the-circle-tool-takes-the-radius-from-the-second-click
  (let ((application (make-test-app)))
    (press application :c)
    (click application 1000 1000)
    (click application 1300 1400)
    (let ((circle (last-entity application)))
      (is (= 500d0 (dom:circle-radius circle)))
      (is (= 1000d0 (dom:circle-center-x circle))))))

(test degenerate-creation-shows-an-error-and-keeps-the-tool-waiting
  (let ((application (make-test-app)))
    (press application :l)
    (click application 1000 1000)
    (click application 1000 1000)                                             ; zero-length line
    (is (= 0 (dom:drawing-entity-count (app:app-drawing application))))
    (is (eq :error (cdr (app:app-message application))))
    (is (search "Invalid line" (message-text application)))
    (click application 2000 1000)                                             ; the first point is still set
    (is (= 1 (dom:drawing-entity-count (app:app-drawing application))))))

(test the-polyline-tool-finishes-closes-removes-and-cancels
  (let ((application (make-test-app)))
    (press application :p)
    (click application 0 0)
    (click application 1000 0)
    (click application 1000 1000)
    (press application :enter)                                                ; finish open
    (let ((open (last-entity application)))
      (is (typep open 'dom:polyline-entity))
      (is (not (dom:polyline-closed open)))
      (is (= 3 (geo:polyline-vertex-count (dom:polyline-coordinates open)))))
    ;; Closed: click the first vertex after placing three.
    (click application -2500 0)
    (click application -1500 0)
    (click application -1500 1000)
    (click application -2500 0)
    (let ((closed (last-entity application)))
      (is (dom:polyline-closed closed))
      (is (= 3 (geo:polyline-vertex-count (dom:polyline-coordinates closed)))))
    ;; Backspace removes the last vertex; Escape abandons the polyline.
    (click application 1500 -500)
    (click application 2500 -500)
    (click application 2500 -1500)
    (press application :backspace)
    (press application :enter)
    (is (= 2 (geo:polyline-vertex-count (dom:polyline-coordinates (last-entity application)))))
    (click application -1000 -1500)
    (click application -2000 -1500)
    (press application :escape)
    (press application :enter)
    (is (= 3 (dom:drawing-entity-count (app:app-drawing application))))
    (is (eq :polyline (app:tool-kind (app:app-tool application))))            ; Escape only cancelled the polyline
    (press application :escape)
    (is (eq :select (app:tool-kind (app:app-tool application))))))

(test the-text-tool-asks-for-the-content
  (let ((application (make-test-app)))
    (press application :t)
    (click application 200 300)
    (is (app:app-prompt application))
    (is (eq :text (app:prompt-kind (app:app-prompt application))))
    (is (app:app-wants-text-input-p application))
    (type-text application "Hall")
    (press application :enter)
    (let ((text (last-entity application)))
      (is (typep text 'dom:text-entity))
      (is (string= "Hall" (dom:text-string text)))
      (is (= 200d0 (dom:text-x text)))
      (is (= 300d0 (dom:text-y text)))
      (is (= dom:+default-text-height+ (dom:text-height text))))
    (is (null (app:app-prompt application)))))

(test an-empty-text-is-rejected-with-a-message
  (let ((application (make-test-app)))
    (press application :t)
    (click application 200 300)
    (press application :enter)
    (is (= 0 (dom:drawing-entity-count (app:app-drawing application))))
    (is (search "Invalid text" (message-text application)))))

(test accented-text-can-be-typed
  (let ((application (make-test-app))
        (word (coerce (list #\C #\a #\f (code-char 233)) 'string)))
    (press application :t)
    (click application 0 0)
    (type-text application word)
    (press application :enter)
    (is (string= word (dom:text-string (last-entity application))))))

(test the-dimension-tool-places-the-line-on-the-clicked-side
  (let ((application (make-test-app)))
    (press application :d)
    (click application 0 0)
    (click application 1000 0)
    (click application 500 500)
    (let ((above (last-entity application)))
      (is (typep above 'dom:dimension-entity))
      (is (= 500d0 (dom:dimension-offset above)))
      (is (string= "1000" (dom:dimension-text above))))
    (click application 0 0)
    (click application 1000 0)
    (click application 500 -300)
    (is (= -300d0 (dom:dimension-offset (last-entity application))))))

(test creating-on-a-locked-or-hidden-layer-is-refused
  (let ((application (make-test-app)))
    (dom:set-layer-locked (app:app-drawing application) dom:+default-layer-id+ t)
    (press application :l)
    (click application 0 0)
    (click application 1000 0)
    (is (= 0 (dom:drawing-entity-count (app:app-drawing application))))
    (is (search "locked" (message-text application)))
    (dom:set-layer-locked (app:app-drawing application) dom:+default-layer-id+ nil)
    (dom:set-layer-visible (app:app-drawing application) dom:+default-layer-id+ nil)
    (click application 0 0)
    (click application 1000 0)
    (is (= 0 (dom:drawing-entity-count (app:app-drawing application))))
    (is (search "hidden" (message-text application)))))

;;; Selection and editing

(defun two-lines-app ()
  "Return an application holding two horizontal lines, at y = 0 and y = 300."
  (let ((application (make-test-app)))
    (press application :l)
    (click application 0 0)
    (click application 1000 0)
    (click application 0 300)
    (click application 1000 300)
    (press application :escape)
    application))

(test clicking-selects-shift-toggles-and-empty-space-clears
  (let ((application (two-lines-app)))
    (click application 500 5)
    (is (equal '(1) (app:selection-ids application)))
    (click application 500 305 :modifiers '(:shift))
    (is (equal '(1 2) (app:selection-ids application)))
    (click application 500 5 :modifiers '(:shift))
    (is (equal '(2) (app:selection-ids application)))
    (click application 500 150)                                               ; empty space
    (is (null (app:selection-ids application)))))

(test the-pick-radius-is-the-same-number-of-pixels-at-every-zoom
  (dolist (scale '(0.02d0 0.2d0 2d0 20d0))
    (let ((application (make-test-app)))
      ;; Snapping would move the pointer onto the line's endpoints and hide the radius.
      (app:operation-toggle-snap application)
      (app:operation-toggle-object-snap application)
      (press application :l)
      (click application -100 0)
      (click application 100 0)
      (press application :escape)
      (set-test-view application :scale scale)
      (flet ((offset (pixels)
               (view:pixels->world-length (app:app-viewport application) pixels)))
        (click application 0 (offset 5d0))
        (is (equal '(1) (app:selection-ids application)) "5 pixels away picks at ~a px/mm" scale)
        (click application 0 (offset 150d0))
        (is (null (app:selection-ids application)) "empty space clears at ~a px/mm" scale)
        (click application 0 (offset 8d0))
        (is (null (app:selection-ids application)) "8 pixels away misses at ~a px/mm" scale)))))

(test window-selection-takes-enclosed-entities-and-can-add
  (let ((application (two-lines-app)))
    (drag application -100 -100 1100 100)                                      ; encloses only the first line
    (is (equal '(1) (app:selection-ids application)))
    (drag application -100 200 1100 400 :modifiers '(:shift))
    (is (equal '(1 2) (app:selection-ids application)))
    (drag application -100 -100 500 400)                                       ; cuts both lines: selects nothing
    (is (null (app:selection-ids application)))))

(test select-all-and-delete-and-undo
  (let ((application (two-lines-app)))
    (press application :a :control)
    (is (equal '(1 2) (app:selection-ids application)))
    (press application :delete)
    (is (= 0 (dom:drawing-entity-count (app:app-drawing application))))
    (is (null (app:selection-ids application)))
    (press application :z :control)
    (is (= 2 (dom:drawing-entity-count (app:app-drawing application))))
    (is (null (app:selection-ids application)))                               ; undo does not reselect
    (press application :delete)
    (is (search "Nothing is selected" (message-text application)))))

(test deleting-skips-entities-on-locked-layers
  (let ((application (two-lines-app)))
    (press application :a :control)
    (dom:set-layer-locked (app:app-drawing application) dom:+default-layer-id+ t)
    (app:operation-delete-selection application)
    (is (= 2 (dom:drawing-entity-count (app:app-drawing application))))
    (is (eq :error (cdr (app:app-message application))))))

(test dragging-a-selected-entity-moves-it-in-one-undo-step
  (let ((application (two-lines-app)))
    (drag application 500 5 500 305)                                           ; grabs line 1, moves it by +300
    (let ((line (dom:store-find (dom:drawing-store (app:app-drawing application)) 1)))
      (is (= 300d0 (dom:line-y1 line)))
      (is (= 300d0 (dom:line-y2 line)))
      (is (= 0d0 (dom:line-x1 line))))
    (is (equal '(1) (app:selection-ids application)))
    (is (null (app:app-translation application)))
    (press application :z :control)
    (is (= 0d0 (dom:line-y1 (dom:store-find (dom:drawing-store (app:app-drawing application)) 1))))))

(test the-move-preview-is-visible-while-dragging
  (let ((application (two-lines-app)))
    (send application
          (pointer application :move nil 500 5)
          (pointer application :down :left 500 5)
          (pointer application :move nil 500 105)
          (pointer application :move nil 500 205))
    (is (consp (app:app-translation application)))
    (is (= 200d0 (cdr (app:app-translation application))))
    (send application (pointer application :up :left 500 205))
    (is (null (app:app-translation application)))))

(test a-tiny-drag-is-a-click-and-moves-nothing
  (let ((application (two-lines-app)))
    (send application
          (pointer application :move nil 500 5)
          (pointer application :down :left 500 5)
          (pointer application :move nil 502 5)                                ; 1 px: below the drag threshold
          (pointer application :up :left 502 5))
    (is (= 0d0 (dom:line-y1 (dom:store-find (dom:drawing-store (app:app-drawing application)) 1))))
    (is (equal '(1) (app:selection-ids application)))
    (is (= 2 (dom:history-undo-depth (app:app-history application))))))       ; only the two creations

(test locked-layer-entities-cannot-be-picked-or-moved
  (let ((application (two-lines-app)))
    (dom:set-layer-locked (app:app-drawing application) dom:+default-layer-id+ t)
    (click application 500 5)
    (is (null (app:selection-ids application)))
    (drag application 500 5 500 305)
    (is (= 0d0 (dom:line-y1 (dom:store-find (dom:drawing-store (app:app-drawing application)) 1))))))

;;; View

(test middle-drag-and-space-drag-pan-the-view
  (let ((application (make-test-app)))
    (multiple-value-bind (x y) (screen-position application 1000 500)
      (let ((world-before (multiple-value-list (view:screen->world (app:app-viewport application) x y))))
        (send application
              (app:make-pointer-event :move nil x y)
              (app:make-pointer-event :down :middle x y)
              (app:make-pointer-event :move nil (+ x 40d0) (+ y 25d0))
              (app:make-pointer-event :up :middle (+ x 40d0) (+ y 25d0)))
        (multiple-value-bind (wx wy) (view:screen->world (app:app-viewport application) (+ x 40d0) (+ y 25d0))
          (is (approx= (first world-before) wx))
          (is (approx= (second world-before) wy)))
        ;; Space + left drag does the same and creates nothing.
        (send application (app:make-key-event :space nil :down)
              (app:make-pointer-event :move nil x y)
              (app:make-pointer-event :down :left x y)
              (app:make-pointer-event :move nil (- x 30d0) y)
              (app:make-pointer-event :up :left (- x 30d0) y)
              (app:make-key-event :space nil :up))
        (is (= 0 (dom:drawing-entity-count (app:app-drawing application))))))))

(test the-wheel-zooms-around-the-pointer
  (let ((application (make-test-app)))
    (multiple-value-bind (x y) (screen-position application 1500 -1000)
      (let ((before (multiple-value-list (view:screen->world (app:app-viewport application) x y)))
            (scale (view:viewport-scale (app:app-viewport application))))
        (send application (app:make-wheel-event 2d0 x y))
        (is (approx= (* scale (expt app:+zoom-step-per-notch+ 2)) (view:viewport-scale (app:app-viewport application))))
        (multiple-value-bind (wx wy) (view:screen->world (app:app-viewport application) x y)
          (is (approx= (first before) wx))
          (is (approx= (second before) wy)))
        (send application (app:make-wheel-event -2d0 x y))
        (is (approx= scale (view:viewport-scale (app:app-viewport application))))))))

(test zoom-extents-fits-the-drawing-and-home-triggers-it
  (let ((application (two-lines-app)))
    (set-test-view application :scale 0.001d0 :center-x 90000d0 :center-y 90000d0)   ; far away and tiny
    (press application :home)
    (let ((bounds (view:viewport-visible-bounds (app:app-viewport application))))
      (is (geo:bounds-contains-bounds-p bounds (geo:make-bounds 0d0 0d0 1000d0 300d0))))
    (is (> (view:viewport-scale (app:app-viewport application)) 0.5d0))))

(test resizing-keeps-the-world-point-at-the-canvas-center
  (let ((application (make-test-app)))
    (send application (app:make-resize-event 1000d0 700d0))
    (let ((canvas (canvas-of application)))
      (is (= 700 (+ (ui:rect-height canvas) ui:+toolbar-height+ ui:+status-height+)))
      (is (= (- 1000 ui:+side-panel-width+ ui:+tool-palette-width+) (ui:rect-width canvas)))
      (is (= (float (ui:rect-width canvas) 1d0) (view:viewport-width (app:app-viewport application)))))
    (is (= 0d0 (view:viewport-center-x (app:app-viewport application))))))

;;; Inspector

(defun focus-property-field (application key)
  "Click the inspector field of property KEY."
  (click-widget application (list :property key)))

(test the-inspector-edits-a-numeric-property
  (let ((application (two-lines-app)))
    (click application 500 5)
    (focus-property-field application :x2)
    (is (equal '(:property :x2) (app:app-focus application)))
    (is (app:app-wants-text-input-p application))
    (type-text application "2500")                                             ; replaces the selected text
    (press application :enter)
    (is (= 2500d0 (dom:line-x2 (dom:store-find (dom:drawing-store (app:app-drawing application)) 1))))
    (is (null (app:app-focus application)))
    (press application :z :control)
    (is (= 1000d0 (dom:line-x2 (dom:store-find (dom:drawing-store (app:app-drawing application)) 1))))))

(test the-inspector-understands-units-in-typed-numbers
  (let ((application (two-lines-app)))
    (click application 500 5)
    (focus-property-field application :x2)
    (type-text application "1.5 m")
    (press application :enter)
    (is (= 1500d0 (dom:line-x2 (dom:store-find (dom:drawing-store (app:app-drawing application)) 1))))
    (app:operation-cycle-display-unit application)                              ; now shown in cm
    (focus-property-field application :y2)
    (type-text application "20")
    (press application :enter)
    (is (= 200d0 (dom:line-y2 (dom:store-find (dom:drawing-store (app:app-drawing application)) 1))))))

(test inspector-edits-that-are-invalid-or-unchanged-are-handled
  (let ((application (two-lines-app)))
    (click application 500 5)
    (let ((depth (dom:history-undo-depth (app:app-history application))))
      (focus-property-field application :x2)
      (press application :enter)                                                ; unchanged text: no history entry
      (is (= depth (dom:history-undo-depth (app:app-history application))))
      (focus-property-field application :x2)
      (type-text application "abc")
      (press application :enter)
      (is (eq :error (cdr (app:app-message application))))
      (is (search "not a number" (message-text application)))
      (focus-property-field application :x2)
      (type-text application "0")                                               ; makes the line degenerate
      (press application :enter)
      (is (search "Invalid line" (message-text application)))
      (is (= 1000d0 (dom:line-x2 (dom:store-find (dom:drawing-store (app:app-drawing application)) 1))))
      (is (= depth (dom:history-undo-depth (app:app-history application)))))))

(test escape-cancels-a-field-edit-and-clicking-elsewhere-commits-it
  (let ((application (two-lines-app)))
    (click application 500 5)
    (focus-property-field application :x2)
    (type-text application "1800")
    (press application :escape)
    (is (null (app:app-focus application)))
    (is (= 1000d0 (dom:line-x2 (dom:store-find (dom:drawing-store (app:app-drawing application)) 1))))
    (focus-property-field application :x2)
    (type-text application "1800")
    (focus-property-field application :y2)                                       ; blur commits the first field
    (is (= 1800d0 (dom:line-x2 (dom:store-find (dom:drawing-store (app:app-drawing application)) 1))))
    (is (equal '(:property :y2) (app:app-focus application)))))

(test boolean-and-unit-properties-toggle-on-click
  (let ((application (make-test-app)))
    (press application :p)
    (click application 0 0)
    (click application 1000 0)
    (click application 1000 1000)
    (press application :enter)
    (press application :escape)
    (click application 500 5)
    (click-widget application (list :property :closed))
    (is (dom:polyline-closed (last-entity application)))
    (press application :d)
    (click application 0 -1500)
    (click application 1000 -1500)
    (click application 500 -1900)
    (press application :escape)
    (click application 500 -1900)                                                ; select the dimension line
    (is (eq :millimeter (dom:dimension-unit (last-entity application))))
    (click-widget application (list :property :unit))
    (is (eq :centimeter (dom:dimension-unit (last-entity application))))
    (is (string= "100" (dom:dimension-text (last-entity application))))))

(test the-inspector-moves-the-selection-to-another-layer
  (let ((application (two-lines-app)))
    (app:operation-add-layer application "walls")
    (click application 500 5)
    (click-widget application :layer-cycle)
    (is (= 1 (dom:entity-layer-id (dom:store-find (dom:drawing-store (app:app-drawing application)) 1))))
    (press application :z :control)
    (is (= 0 (dom:entity-layer-id (dom:store-find (dom:drawing-store (app:app-drawing application)) 1))))))

(test settings-fields-change-the-grid-spacing-and-text-height
  (let ((application (make-test-app)))
    (click-widget application :setting-grid)
    (type-text application "250")
    (press application :enter)
    (is (= 250d0 (dom:drawing-grid-spacing (app:app-drawing application))))
    (click-widget application :setting-grid)
    (type-text application "0.1")                                                ; below the minimum spacing
    (press application :enter)
    (is (eq :error (cdr (app:app-message application))))
    (is (= 250d0 (dom:drawing-grid-spacing (app:app-drawing application))))
    (click-widget application :setting-text-height)
    (type-text application "100")
    (press application :enter)
    (is (= 100d0 (dom:drawing-default-text-height (app:app-drawing application))))))

;;; Layers

(defun layer-names (application)
  "Return the layer names of the drawing in order."
  (mapcar #'dom:layer-name (dom:layers-in-order (app:app-drawing application))))

(test adding-renaming-and-deleting-layers-through-the-panel
  (let ((application (make-test-app)))
    (click-widget application :layer-add)
    (is (eq :text (app:prompt-kind (app:app-prompt application))))
    (type-text application "walls")
    (press application :enter)
    (is (equal '("0" "walls") (layer-names application)))
    (is (= 1 (dom:drawing-current-layer-id (app:app-drawing application))))     ; the new layer is current
    (click-widget application :layer-rename)
    (type-text application "outer walls")
    (press application :enter)
    (is (equal '("0" "outer walls") (layer-names application)))
    (click-widget application :layer-add)
    (type-text application "outer walls")                                        ; duplicate name
    (press application :enter)
    (is (eq :error (cdr (app:app-message application))))
    (click-widget application :layer-delete)
    (is (equal '("0") (layer-names application)))
    (click-widget application :layer-delete)                                     ; the default layer
    (is (search "cannot be deleted" (message-text application)))))

(test deleting-a-layer-with-entities-is-refused-and-an-empty-one-clears-the-history
  (let ((application (make-test-app)))
    (app:operation-add-layer application "walls")
    (press application :l)
    (click application 0 0)
    (click application 1000 0)
    (press application :escape)
    (click-widget application :layer-delete)
    (is (search "still holds 1 entity" (message-text application)))
    (is (equal '("0" "walls") (layer-names application)))
    (app:operation-delete-selection application)                                  ; nothing selected: no-op
    (click application 500 5)
    (press application :delete)
    (is (plusp (dom:history-undo-depth (app:app-history application))))
    (click-widget application :layer-delete)
    (is (= 0 (dom:history-undo-depth (app:app-history application))))))

(test layer-visibility-lock-and-current-toggles
  (let ((application (two-lines-app)))
    (click application 500 5)
    (is (equal '(1) (app:selection-ids application)))
    (click-widget application '(:layer-visible 0))
    (is (not (dom:layer-visible (dom:find-layer (app:app-drawing application) 0))))
    (is (null (app:selection-ids application)))                                  ; hiding deselects
    (app:app-render application)
    (is (= 0 (scene:scene-result-drawn-entities (app:app-last-scene-result application))))
    (click-widget application '(:layer-visible 0))
    (click-widget application '(:layer-locked 0))
    (is (dom:layer-locked (dom:find-layer (app:app-drawing application) 0)))
    (click application 500 5)
    (is (null (app:selection-ids application)))
    (click-widget application '(:layer-locked 0))
    (app:operation-add-layer application "second")
    (click-widget application '(:layer 0))
    (is (= 0 (dom:drawing-current-layer-id (app:app-drawing application))))
    (let ((color (dom:layer-color (dom:find-layer (app:app-drawing application) 0))))
      (click-widget application :layer-color)
      (is (not (equalp color (dom:layer-color (dom:find-layer (app:app-drawing application) 0))))))))

;;; Files

(defun saved-lines-app ()
  "Return an application with two lines (dirty)."
  (two-lines-app))

(test saving-asks-for-a-name-then-saves-and-clears-the-dirty-flag
  (with-temporary-directory (directory)
    (let ((application (saved-lines-app))
          (path (namestring (merge-pathnames "plan.bcad" directory))))
      (is (app:app-dirty-p application))
      (press application :s :control)
      (is (app:app-dialog application))
      (type-text application path)
      (press application :enter)
      (is (null (app:app-dialog application)))
      (is (not (app:app-dirty-p application)))
      (is (probe-file path))
      (is (string= "plan.bcad - Borba CAD" (app:app-title application)))
      (is (search "Saved plan.bcad" (message-text application)))
      ;; A change makes it dirty again; Ctrl+S now saves silently to the same file.
      (press application :l)
      (click application 0 600)
      (click application 1000 600)
      (is (app:app-dirty-p application))
      (is (search "*" (app:app-title application)))
      (press application :s :control)
      (is (null (app:app-dialog application)))
      (is (null (app:app-prompt application)))
      (is (not (app:app-dirty-p application)))
      (is (= 3 (dom:drawing-entity-count (persist:load-drawing path)))))))

(test a-failed-save-reports-the-error-and-keeps-the-drawing-dirty
  (let ((application (saved-lines-app)))
    (press application :s :control)
    (type-text application "/proc/does-not-exist/plan.bcad")
    (press application :enter)
    (is (eq :error (cdr (app:app-message application))))
    (is (app:app-dirty-p application))
    (is (null (app:app-file-path application)))
    (is (app:app-dialog application) "the dialog stays open")
    (is (eq :error (app:file-dialog-message-severity (app:app-dialog application))))
    (is (plusp (length (app:file-dialog-message (app:app-dialog application)))) "and says what went wrong")))

(test opening-replaces-the-drawing-and-bad-files-leave-it-intact
  (with-temporary-directory (directory)
    (let* ((source (saved-lines-app))
           (path (namestring (merge-pathnames "plan.bcad" directory)))
           (broken (namestring (merge-pathnames "broken.bcad" directory))))
      (app:save-app-drawing source path)
      (with-open-file (out broken :direction :output) (write-string "(drawing (format-version 9))" out))
      (let ((application (make-test-app)))
        (press application :o :control)
        (is (eq :open (app:file-dialog-mode (app:app-dialog application))))
        (type-text application path)
        (press application :enter)
        (is (null (app:app-dialog application)))
        (is (= 2 (dom:drawing-entity-count (app:app-drawing application))))
        (is (not (app:app-dirty-p application)))
        (is (= 0 (dom:history-undo-depth (app:app-history application))))
        (is (null (app:selection-ids application)))
        ;; A broken file: error shown in the dialog, drawing intact.
        (press application :o :control)
        (type-text application broken)
        (press application :enter)
        (is (eq :error (cdr (app:app-message application))))
        (is (search "newer" (message-text application)))
        (is (search "newer" (app:file-dialog-message (app:app-dialog application))))
        (is (= 2 (dom:drawing-entity-count (app:app-drawing application))))
        ;; A missing file.
        (press application :escape)
        (is (null (app:app-dialog application)))
        (press application :o :control)
        (type-text application (namestring (merge-pathnames "missing.bcad" directory)))
        (press application :enter)
        (is (search "does not exist" (app:file-dialog-message (app:app-dialog application))))
        (is (= 2 (dom:drawing-entity-count (app:app-drawing application))))))))

(test new-and-open-offer-to-save-unsaved-changes
  (with-temporary-directory (directory)
    (let ((application (saved-lines-app))
          (path (namestring (merge-pathnames "plan.bcad" directory))))
      ;; Cancel keeps everything.
      (press application :n :control)
      (is (eq :choice (app:prompt-kind (app:app-prompt application))))
      (is (equal '("Save" "Discard" "Cancel") (mapcar #'car (app:prompt-choices (app:app-prompt application)))))
      (press application :escape)
      (is (= 2 (dom:drawing-entity-count (app:app-drawing application))))
      ;; Discard resets.
      (press application :n :control)
      (click-widget application '(:prompt-button 1))
      (is (= 0 (dom:drawing-entity-count (app:app-drawing application))))
      (is (not (app:app-dirty-p application)))
      ;; Save, then continue: with no file name the save prompt comes first.
      (press application :l)
      (click application 0 0)
      (click application 1000 0)
      (press application :n :control)
      (click-widget application '(:prompt-button 0))
      (is (eq :save (app:file-dialog-mode (app:app-dialog application))))
      (type-text application path)
      (press application :enter)
      (is (probe-file path))
      (is (= 0 (dom:drawing-entity-count (app:app-drawing application)))))))

(test quitting-with-unsaved-changes-asks-first
  (let ((application (saved-lines-app)))
    (send application (app:make-quit-event))
    (is (not (app:app-quit-requested application)))
    (is (eq :choice (app:prompt-kind (app:app-prompt application))))
    (click-widget application '(:prompt-button 1))                               ; Discard
    (is (app:app-quit-requested application)))
  (let ((clean (make-test-app)))
    (send clean (app:make-quit-event))
    (is (app:app-quit-requested clean))))

;;; Prompts

(test the-prompt-is-modal-and-supports-editing
  (let ((application (make-test-app)))
    (press application :t)
    (click application 0 0)
    (is (app:app-prompt application))
    (press application :l)                                                       ; shortcuts do nothing under a prompt
    (is (eq :text (app:tool-kind (app:app-tool application))))
    (click application 1000 1000)                                                ; the canvas is blocked
    (is (= 0 (dom:drawing-entity-count (app:app-drawing application))))
    (type-text application "abc")
    (press application :left)
    (press application :backspace)
    (type-text application "X")
    (is (string= "aXc" (ui:edit-state-text (app:prompt-edit (app:app-prompt application)))))
    (press application :escape)
    (is (null (app:app-prompt application)))
    (is (app:app-wants-text-input-p application) "the text tool takes a typed point, so it listens")
    (press application :escape)
    (is (not (app:app-wants-text-input-p application)) "and the select tool does not")
    (is (= 0 (dom:drawing-entity-count (app:app-drawing application))))))

(test prompt-buttons-can-be-clicked
  (let ((application (make-test-app)))
    (press application :t)
    (click application 0 0)
    (type-text application "Via button")
    (click-widget application '(:prompt-button 0))
    (is (string= "Via button" (dom:text-string (last-entity application))))
    (press application :t)
    (click application 100 100)
    (click-widget application '(:prompt-button 1))                               ; Cancel
    (is (null (app:app-prompt application)))
    (is (= 1 (dom:drawing-entity-count (app:app-drawing application))))))

;;; Rendering

(defun all-text (application)
  "Return every string drawn in the application's current draw list."
  (map 'list #'scene:text-run-string (scene:draw-list-text-runs (app:app-draw-list application))))

(test rendering-draws-the-canvas-panels-and-status
  (let ((application (two-lines-app)))
    (app:app-render application)
    (is (not (app:app-needs-redraw application)))
    (is (= 2 (scene:scene-result-drawn-entities (app:app-last-scene-result application))))
    (let ((texts (all-text application)))
      (dolist (expected '("Borba CAD" "Untitled" "Layers" "Properties" "Grid" "Snap" "OSnap" "Unit: mm" "Fit"))
        (is (member expected texts :test #'string=) "~s is drawn" expected))
      (is (some (lambda (text) (search "Select:" text)) texts)))
    (app:app-request-redraw application)
    (is (app:app-needs-redraw application))))

(test the-status-bar-follows-the-tool-and-shows-messages-and-hints
  (let ((application (make-test-app)))
    (multiple-value-bind (left severity) (app:app-status-texts application nil)
      (is (search "Select:" left))
      (is (eq :info severity)))
    (press application :r)
    (app:app-render application)
    (is (some (lambda (text) (search "Rectangle: click the first corner" text)) (all-text application)))
    (app:report-error application (make-condition 'base:invalid-input :text "x" :reason "bad"))
    (multiple-value-bind (left severity) (app:app-status-texts application nil)
      (is (search "bad" left))
      (is (eq :error severity)))
    ;; Hovering a toolbar button shows its hint (once the message is gone).
    (let ((fresh (make-test-app)))
      (let ((widget (find-widget fresh :select)))
        (send fresh (app:make-pointer-event :move nil
                                            (float (+ (ui:widget-x widget) 5) 1d0)
                                            (float (+ (ui:widget-y widget) 5) 1d0)))
        (app:app-render fresh)
        (is (some (lambda (text) (search "Select and move (Esc)" text)) (all-text fresh)))))))

(test a-message-leaves-the-status-bar-after-a-while-and-an-error-after-longer
  (let ((application (make-test-app))
        (start (sbcl:monotonic-nanoseconds)))
    (app:set-message application "Saved plan.bcad.")
    (is (not (app:expire-message application start)) "a fresh message stays")
    (is (app:app-message application))
    (app:app-render application)
    (is (app:app-message application) "drawing a frame does not remove it either")
    (is (app:expire-message application (+ start (* 7 sbcl:+nanoseconds-per-second+))))
    (is (null (app:app-message application)))
    (is (not (app:expire-message application (+ start (* 99 sbcl:+nanoseconds-per-second+)))) "nothing to remove")
    (app:set-message application "The file is damaged." :error)
    (let ((set-at (sbcl:monotonic-nanoseconds)))
      (is (not (app:expire-message application (+ set-at (* 7 sbcl:+nanoseconds-per-second+)))) "an error stays longer")
      (is (app:expire-message application (+ set-at (* 13 sbcl:+nanoseconds-per-second+)))))))

(test an-internal-error-is-told-to-the-user-without-touching-the-drawing
  (let ((application (two-lines-app)))
    (app:report-internal-error application
                               (make-condition 'type-error :datum 1 :expected-type 'string)
                               "handling input"
                               nil)
    (is (= 2 (dom:drawing-entity-count (app:app-drawing application))))
    (is (eq :error (cdr (app:app-message application))))
    (is (search "Internal error while handling input" (message-text application)))
    (is (typep (app:app-last-error application) 'type-error))
    (app:app-render application)
    (is (some (lambda (text) (search "Internal error" text)) (all-text application)) "and it is drawn")))

(test the-next-key-or-click-clears-the-last-message-and-what-it-produces-stays
  (let ((application (make-test-app)))
    (app:set-message application "Old news.")
    (press application :g)                                      ; any key, here the grid toggle
    (is (null (app:app-message application)))
    (app:set-message application "More old news.")
    (click application 0 0)
    (is (null (app:app-message application)) "a click clears it too")
    (press application :a :control)                             ; select all says how many it selected
    (is (search "selected" (message-text application)) "the message the key itself makes stays")
    (app:report-error application (make-condition 'base:invalid-input :text "x" :reason "bad"))
    (send application (app:make-key-event :g nil :up))          ; a key release is not a new beginning
    (is (eq :error (cdr (app:app-message application))))))

(test once-the-message-is-gone-the-tool-prompt-and-the-hints-show-again
  (let ((application (make-test-app)))
    (app:set-message application "Layer created.")
    (multiple-value-bind (left) (app:app-status-texts application nil)
      (is (string= "Layer created." left)))
    (app:expire-message application (+ (sbcl:monotonic-nanoseconds) (* 60 sbcl:+nanoseconds-per-second+)))
    (multiple-value-bind (left) (app:app-status-texts application nil)
      (is (search "Select:" left)))
    (multiple-value-bind (left) (app:app-status-texts application "Line (L)")
      (is (string= "Line (L)" left)))))

(test losing-the-keyboard-lets-go-of-space-and-ends-a-drag-whose-release-will-never-come
  (let ((application (two-lines-app)))
    ;; Space held, window switched away: the release never arrives. Afterwards a drag must select, not pan.
    (send application (app:make-key-event :space nil :down))
    (send application (app:make-window-state-event :focus-lost))
    (let ((before (view:viewport-center-x (app:app-viewport application))))
      (drag application 2000 1000 2200 1100)
      (is (= before (view:viewport-center-x (app:app-viewport application))) "space is not held any more"))
    ;; A move in progress is abandoned: nothing moves when the button comes back up elsewhere.
    (click application 500 0)
    (is (equal '(1) (app:selection-ids application)))
    (let ((line-before (dom:line-y1 (first (entities-of application)))))
      (send application
            (pointer application :down :left 300 0)         ; away from the grips, which sit at 0, 500 and 1000
            (pointer application :move nil 300 100))
      (is (app:app-translation application) "the move is previewed")
      (send application (app:make-window-state-event :focus-lost))
      (is (null (app:app-translation application)) "and abandoned")
      (send application (pointer application :up :left 500 200))
      (is (= line-before (dom:line-y1 (first (entities-of application)))) "nothing moved"))
    ;; A pan is ended too.
    (send application
          (pointer application :down :middle 0 0)
          (app:make-window-state-event :focus-lost)
          (pointer application :move nil 100 100))
    (let ((center (view:viewport-center-x (app:app-viewport application))))
      (send application (pointer application :move nil 300 300))
      (is (= center (view:viewport-center-x (app:app-viewport application))) "moving no longer pans"))))

(test a-line-half-drawn-survives-losing-the-keyboard
  (let ((application (make-test-app)))
    (press application :l)
    (click application 0 0)
    (send application (app:make-window-state-event :focus-lost))
    (click application 1000 0)
    (is (= 1 (dom:drawing-entity-count (app:app-drawing application))) "the line was finished after coming back")))

(test the-pointer-leaving-the-window-clears-its-position-unless-a-drag-holds-it
  (let ((application (make-test-app)))
    (send application (pointer application :move nil 100 100))
    (is (app:app-cursor application))
    (send application (app:make-window-state-event :pointer-left))
    (is (null (app:app-cursor application)) "no coordinates in the status bar")
    ;; While a pan holds the pointer, leaving the window changes nothing.
    (send application (pointer application :down :middle 0 0))
    (send application (pointer application :move nil 50 50))
    (send application (app:make-window-state-event :pointer-left))
    (let ((center (view:viewport-center-x (app:app-viewport application))))
      (send application (pointer application :move nil 120 120))
      (is (/= center (view:viewport-center-x (app:app-viewport application))) "the pan goes on"))))

(test areas-and-lengths-read-the-same-way-without-trailing-zeros
  (let ((application (make-test-app)))
    (press application :r)
    (click application 0 0)
    (click application 1000 500)
    (press application :escape)
    (click application 500 0)
    (let ((texts (mapcar #'ui:widget-label (app:build-widgets application))))
      (is (some (lambda (text) (and text (search "Area: 500000 mm" text))) texts)
          "mm: 1000 by 500 is 500000, with no .0000 after it")
      (is (notany (lambda (text) (and text (search ".0000" text))) texts)))
    (app:operation-cycle-display-unit application)             ; cm
    (app:operation-cycle-display-unit application)             ; m
    (let ((texts (mapcar #'ui:widget-label (app:build-widgets application))))
      (is (some (lambda (text) (and text (search "Area: 0.5 m" text))) texts) "m: half a square metre"))))

(defun trim-zeros-cases ()
  "Return (input expected) pairs for TRIM-DECIMAL-ZEROS."
  '(("12.500" "12.5") ("70.0000" "70") ("0.0" "0") ("100" "100")
    ("100.5" "100.5") ("-3.250" "-3.25") ("0.001" "0.001")))

(test trailing-zeros-and-a-lonely-point-are-trimmed
  (dolist (case (trim-zeros-cases))
    (is (string= (second case) (app::trim-decimal-zeros (first case))) "~s" (first case))))

(test the-creation-preview-follows-the-pointer
  (let ((application (make-test-app)))
    (press application :l)
    (click application 0 0)
    (send application (pointer application :move nil 1000 500))
    (let ((preview (app:tool-preview-entities (app:app-tool application) application)))
      (is (= 1 (length preview)))
      (is (= 1000d0 (dom:line-x2 (first preview)))))
    (app:app-render application)
    (is (plusp (scene:draw-list-line-count (app:app-draw-list application))))
    (press application :escape)
    (is (null (app:tool-preview-entities (app:app-tool application) application)))))

(test toolbar-buttons-reflect-state-and-drive-operations
  (let ((application (make-test-app)))
    (is (not (ui:widget-enabled (find-widget application :undo))))
    (choose-tool application :line)
    (is (eq :line (app:tool-kind (app:app-tool application))))
    (is (ui:widget-active (find-widget application :draw)))
    (is (eq :line (ui:widget-icon (find-widget application :draw))) "the button shows the tool in use")
    (click application 0 0)
    (click application 1000 0)
    (is (ui:widget-enabled (find-widget application :undo)))
    (click-widget application :undo)
    (is (= 0 (dom:drawing-entity-count (app:app-drawing application))))
    (click-widget application :redo)
    (is (= 1 (dom:drawing-entity-count (app:app-drawing application))))
    (click-widget application :grid)
    (is (not (dom:drawing-grid-visible (app:app-drawing application))))
    (click-widget application :snap)
    (is (not (dom:drawing-snap-to-grid (app:app-drawing application))))
    (click-widget application :object-snap)
    (is (not (dom:drawing-object-snap (app:app-drawing application))))
    (click-widget application :unit)
    (is (eq :centimeter (dom:drawing-display-unit (app:app-drawing application))))
    (is (string= "Unit: cm" (ui:widget-label (find-widget application :unit))))
    (click-widget application :perf)
    (is (app:app-overlay-visible application))))

;;; Telemetry integration

(test operations-are-timed-and-logged-with-their-workload
  (with-temporary-directory (directory)
    (let* ((telemetry (obs:make-telemetry (test-config directory :level :debug) :now 0))
           (application (make-test-app :telemetry telemetry))
           (path (namestring (merge-pathnames "plan.bcad" directory))))
      (press application :l)
      (click application 0 0)
      (click application 1000 0)
      (press application :escape)
      (click application 500 5)
      (press application :delete)
      (press application :z :control)
      (app:save-app-drawing application path)
      (app:load-drawing-into-app application path)
      (press application :f3)
      (app:app-render application)
      (obs:telemetry-tick telemetry (* 2 +test-second+))
      (obs:close-telemetry telemetry)
      (let ((entries (entries-in directory)))
        (flet ((entry (event) (find event entries :key #'obs:log-entry-event :test #'string=)))
          (is (string= "success" (obs:log-entry-field (entry "entity.create") "result")))
          (is (string= "1" (obs:log-entry-field (entry "entity.create") "changed")))
          (is (entry "entity.delete"))
          (is (entry "history.undo"))
          (is (entry "geometry.pick"))                                            ; debug level was enabled
          (is (string= "1" (obs:log-entry-field (entry "file.save") "entity_count")))
          (is (plusp (parse-integer (obs:log-entry-field (entry "file.save") "bytes"))))
          (is (string= "1" (obs:log-entry-field (entry "file.load") "entity_count")))
          (is (obs:log-entry-field (entry "file.load") "parse_ms"))
          (is (entry "render.full-redraw"))
          (let ((input (find-if (lambda (e) (and (string= "input.event" (obs:log-entry-event e))
                                                 (equal "pointer-move" (obs:log-entry-field e "type"))))
                                entries)))
            (is (not (null input)))
            (is (plusp (parse-integer (obs:log-entry-field input "count"))))))))))

(test the-performance-overlay-draws-measured-figures
  (with-temporary-directory (directory)
    (let* ((telemetry (obs:make-telemetry (test-config directory) :now 0))
           (application (make-test-app :telemetry telemetry)))
      (press application :f3)
      (app:app-render application)
      (is (some (lambda (text) (search "Frames:" text)) (all-text application)))
      (is (some (lambda (text) (search "Heap:" text)) (all-text application)))
      (press application :f3)
      (app:app-render application)
      (is (notany (lambda (text) (search "Frames:" text)) (all-text application)))
      (obs:close-telemetry telemetry))))

;;; Robustness

(defun random-event (application prng)
  "Return a pseudo-random input event aimed at APPLICATION's window."
  (let* ((width (app:app-window-width application))
         (height (app:app-window-height application))
         (x (rng:prng-next-double prng 0d0 width))
         (y (rng:prng-next-double prng 0d0 height))
         (modifiers (rng:prng-pick prng '(() () (:shift) (:control) (:shift :control)))))
    (case (rng:prng-next-below prng 10)
      (0 (app:make-pointer-event :move nil x y modifiers))
      ((1 2) (app:make-pointer-event :down (rng:prng-pick prng '(:left :left :left :middle :right)) x y modifiers))
      ((3 4) (app:make-pointer-event :up (rng:prng-pick prng '(:left :left :middle :right)) x y modifiers))
      (5 (app:make-wheel-event (rng:prng-pick prng '(-2d0 -1d0 1d0 2d0)) x y modifiers))
      (6 (app:make-key-event (rng:prng-pick prng '(:l :r :c :p :t :d :escape :enter :backspace :delete :a :z :y :s :o :n
                                                    :g :u :home :f3 :space :left :right :up :down :page-up
                                                    :page-down :tab :end :v :c :x :l))
                             modifiers
                             (rng:prng-pick prng '(:down :down :up))))
      (7 (app:make-file-drop-event (rng:prng-pick prng '("/nonexistent/x.bcad" "" "/proc/version" "relative.bcad"))))
      (8 (progn (app:set-clipboard-text application
                                        (rng:prng-pick prng (list nil "" "pasted" "12,5" (format nil "two~%lines")
                                                                  (make-string 600 :initial-element #\x))))
                (app:make-key-event :v '(:control))))
      (t (app:make-text-event (rng:prng-pick prng (list "a" "12" "-5.5" "m" "x y" "" "a/b" "ok" "name with spaces"
                                                        "\"quoted\"" (format nil "caf~c" (code-char 233)))))))))

(defun check-dialog-invariants (application)
  "Check what must hold of an open file dialog after any event: the keyboard is on a field that
exists, no caret lies outside its text, the scroll is not negative, the selection names an entry
of the folder, and the folder shown exists."
  (let ((dialog (app:app-dialog application)))
    (when dialog
      (is (member (app:file-dialog-focus dialog) '(:name :filter :location)))
      (let ((edit (app::focused-edit dialog)))
        (is-true edit "the focused field has an edit state")
        (when edit
          (is (<= 0 (ui:edit-state-caret edit) (length (ui:edit-state-text edit))))))
      (dolist (edit (list (app:file-dialog-name-edit dialog) (app:file-dialog-filter-edit dialog)))
        (is (<= 0 (ui:edit-state-caret edit) (length (ui:edit-state-text edit)))))
      (is (>= (app:file-dialog-scroll dialog) 0))
      (is (or (null (app:file-dialog-selected dialog)) (app::find-entry dialog (app:file-dialog-selected dialog))))
      (is (eq :directory (sbcl:file-kind (app:file-dialog-directory dialog)))))))

(test random-event-sequences-never-escape-as-errors-and-keep-the-model-consistent
  ;; Monkey test: thousands of pseudo-random events (clicks, drags, keys, text, wheel). The
  ;; application must report problems to the user, never signal, and keep its invariants.
  (with-temporary-directory (directory)
    (dolist (seed '(1 2 3))
      (let ((application (make-test-app))
            (prng (rng:make-prng (* 1000 seed)))
            (events-with-a-dialog 0))
        (dotimes (i 1500)
          (let ((event (random-event application prng)))
            ;; Now and then open a file dialog, so that the random events spend time inside it.
            (when (and (zerop (mod i 40)) (null (app:app-dialog application)) (null (app:app-prompt application)))
              (if (evenp (floor i 40))
                  (app:operation-open application)
                  (app:operation-save-as application)))
            (handler-case (app:app-handle-event application event)
              (error (condition)
                (fail "event ~d (~a) escaped as an error: ~a" i (type-of event) condition)))
            (check-dialog-invariants application)
            (when (app:app-dialog application)
              (incf events-with-a-dialog))
            ;; Never let a random "Save" write outside the sandbox: close file prompts, and close a
            ;; file dialog the moment a random click on a place or on the folder trail takes it
            ;; anywhere but the folder made for tests.
            (when (and (app:app-prompt application) (zerop (mod i 7)))
              (press application :escape))
            (let ((dialog (app:app-dialog application)))
              (when (and dialog
                         (not (equal (truename (scratch-drawings-directory))
                                     (truename (app:file-dialog-directory dialog)))))
                (app:close-file-dialog application)))
            (when (zerop (mod i 50))
              (app:app-render application))))
        (is (> events-with-a-dialog 30) "the monkey really spent time in the file dialog")
        ;; Invariants.
        (let ((ids (entity-ids (app:app-drawing application))))
          (is (equal ids (sort (copy-list ids) #'<)))
          (is (= (length ids) (length (remove-duplicates ids)))))
        (dolist (id (app:selection-ids application))
          (is (dom:store-find (dom:drawing-store (app:app-drawing application)) id)))
        (loop while (dom:history-undo (app:app-history application) (app:app-drawing application)))
        (is (= 0 (dom:drawing-entity-count (app:app-drawing application))))))))

;;; The demonstration scenario

(test the-demo-scenario-draws-a-complete-plan-without-errors
  (with-temporary-directory (directory)
    (let* ((config (test-config directory))
           (telemetry (obs:make-telemetry config :now 0))
           (application (app:make-app config :telemetry telemetry)))
      (dolist (step (app:demo-steps application))
        (funcall step)
        (is (null (app:app-last-error application)))
        (app:app-render application))
      (let ((kinds (mapcar #'dom:entity-kind (entities-of application))))
        (dolist (kind '(:rectangle :line :circle :polyline :text :dimension))
          (is (member kind kinds)))
        (is (= 1 (count :rectangle kinds)))
        (is (= 2 (count :line kinds)))
        (is (= 2 (count :circle kinds)))
        (is (= 2 (count :polyline kinds)))
        (is (= 2 (count :text kinds)))
        (is (= 1 (count :dimension kinds))))
      (is (equal '("0" "walls" "furniture" "annotations") (layer-names application)))
      (is (app:app-overlay-visible application))
      ;; The script saved the plan and opened it again, so the application now edits that file.
      (is (equal (app:demo-save-path application)
                 (uiop:native-namestring (app:app-file-path application))))
      (is (= 10 (dom:drawing-entity-count (persist:load-drawing (app:demo-save-path application)))))
      (let ((log-path (obs:logger-path (obs:telemetry-logger telemetry))))
        (obs:close-telemetry telemetry)
        (let ((log-text (uiop:read-file-string log-path)))
          ;; At the default verbosity the hundreds of pointer moves of the scenario leave at most
          ;; one aggregated entry, and neither redraws nor picks are logged one by one.
          (is (<= (occurrences-of "type=pointer-move" log-text) 1))
          (is (zerop (occurrences-of "render.full-redraw" log-text)))
          (is (zerop (occurrences-of "geometry.pick" log-text)))
          (is (plusp (occurrences-of "entity.create" log-text))))))))
