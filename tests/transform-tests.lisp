(in-package #:borba-cad/tests)

(def-suite transforms
  :description "Rotating, mirroring and scaling: the entities, the change sets, the tools and the console commands."
  :in all)

(in-suite transforms)

;;; Helpers

(defun close-to-p (expected
                   actual
                   &optional (tolerance 1d-9))
  "Return true when two numbers agree to TOLERANCE."
  (< (abs (- expected actual)) tolerance))

(defun angle-close-p (expected
                      actual)
  "Return true when two angles are the same direction, whatever whole turns they differ by."
  (or (close-to-p 0d0 (geo:normalize-angle (- expected actual)))
      (close-to-p (* 2 pi) (geo:normalize-angle (- expected actual)))))

(defun the-entity-signature (entity)
  "Return the kind of ENTITY and its property values, with every angle reduced to one turn."
  (list (dom:entity-kind entity)
        (mapcar (lambda (property)
                  (let ((value (dom:property-value property)))
                    (if (eq :angle (dom:property-kind property))
                        (geo:normalize-angle value)
                        value)))
                (dom:entity-properties entity))))

(defun signatures-close-p (first
                           second)
  "Return true when two entity signatures are of the same kind and have the same numbers to a nanometre."
  (and (eq (first first) (first second))
       (= (length (second first)) (length (second second)))
       (every (lambda (a b)
                (if (and (realp a) (realp b))
                    (or (close-to-p a b 1d-6) (close-to-p (* 2 pi) (abs (- a b)) 1d-6))
                    (equal a b)))
              (second first)
              (second second))))

(defun one-of-each ()
  "Return an entity of every kind that can be transformed, with distinct ids from 1."
  (list (dom:make-line :id 1 :layer-id 0 :x1 100d0 :y1 50d0 :x2 400d0 :y2 250d0)
        (dom:make-rectangle :id 2 :layer-id 0 :min-x 10d0 :min-y 20d0 :width 30d0 :height 40d0)
        (dom:make-circle :id 3 :layer-id 0 :center-x 100d0 :center-y 0d0 :radius 10d0)
        (dom:make-polyline :id 4 :layer-id 0 :closed nil :coordinates (doubles 0d0 0d0 100d0 50d0 120d0 200d0))
        (dom:make-text :id 5 :layer-id 0 :x 30d0 :y 40d0 :height 20d0 :string "Sala" :angle 0.3d0)
        (dom:make-dimension :id 6 :layer-id 0 :x1 0d0 :y1 0d0 :x2 1000d0 :y2 0d0 :offset 300d0
                            :text-height 100d0 :unit :millimeter :precision 0)
        (dom:make-arc :id 7 :layer-id 0 :center-x 10d0 :center-y 20d0 :radius 100d0
                      :start-angle 0.5d0 :end-angle 2d0)
        (dom:make-ellipse :id 8 :layer-id 0 :center-x 50d0 :center-y -30d0 :radius-x 80d0 :radius-y 30d0
                          :rotation 0.4d0)
        (dom:make-polygon :id 9 :layer-id 0 :center-x -20d0 :center-y 60d0 :radius 50d0 :sides 5
                          :rotation 0.2d0)))

(defun transformed-drawing (&optional (entities (one-of-each)))
  "Return a drawing holding ENTITIES."
  (let ((drawing (dom:make-drawing)))
    (dom:apply-changes drawing (dom:create-entities-changes entities))
    drawing))

;;; The points

(test a-point-is-turned-reflected-and-scaled
  (multiple-value-bind (x y) (dom:rotate-xy 10d0 0d0 0d0 0d0 (/ pi 2))
    (is (and (close-to-p 0d0 x) (close-to-p 10d0 y))))
  (multiple-value-bind (x y) (dom:rotate-xy 20d0 10d0 10d0 10d0 pi)
    (is (and (close-to-p 0d0 x) (close-to-p 10d0 y)) "about another center"))
  (multiple-value-bind (x y) (dom:mirror-xy 3d0 4d0 0d0 0d0 1d0 0d0)
    (is (and (close-to-p 3d0 x) (close-to-p -4d0 y)) "in the x axis"))
  (multiple-value-bind (x y) (dom:mirror-xy 3d0 4d0 0d0 0d0 5d0 5d0)
    (is (and (close-to-p 4d0 x) (close-to-p 3d0 y)) "in the diagonal swaps x and y"))
  (multiple-value-bind (x y) (dom:scale-xy 12d0 14d0 10d0 10d0 3d0)
    (is (and (close-to-p 16d0 x) (close-to-p 22d0 y)))))

;;; Rotating

(test a-quarter-turn-puts-each-kind-where-it-should-be
  (let ((entities (one-of-each)))
    (flet ((turned (index)
             (dom:entity-rotated (nth index entities) 0d0 0d0 (/ pi 2))))
      (let ((line (turned 0)))
        (is (equal '(-50d0 100d0 -250d0 400d0)
                   (mapcar (lambda (value) (round-to-ten-thousandth value))
                           (list (dom:line-x1 line) (dom:line-y1 line) (dom:line-x2 line) (dom:line-y2 line))))))
      (let ((rectangle (turned 1)))
        (is (eq :rectangle (dom:entity-kind rectangle)) "a quarter turn keeps a rectangle one")
        (is (close-to-p -60d0 (dom:rectangle-min-x rectangle)))
        (is (close-to-p 10d0 (dom:rectangle-min-y rectangle)))
        (is (close-to-p 40d0 (dom:rectangle-width rectangle)))
        (is (close-to-p 30d0 (dom:rectangle-height rectangle))))
      (let ((circle (turned 2)))
        (is (and (close-to-p 0d0 (dom:circle-center-x circle)) (close-to-p 100d0 (dom:circle-center-y circle))))
        (is (= 10d0 (dom:circle-radius circle))))
      (let ((text (turned 4)))
        (is (close-to-p (+ 0.3d0 (/ pi 2)) (dom:text-angle text)))
        (is (and (close-to-p -40d0 (dom:text-x text)) (close-to-p 30d0 (dom:text-y text)))))
      (let ((dimension (turned 5)))
        (is (close-to-p 300d0 (dom:dimension-offset dimension)) "the offset stays on the same side")
        (is (and (close-to-p 0d0 (dom:dimension-x2 dimension)) (close-to-p 1000d0 (dom:dimension-y2 dimension)))))
      (let ((arc (turned 6)))
        (is (close-to-p (+ 0.5d0 (/ pi 2)) (dom:arc-start-angle arc)))
        (is (close-to-p (+ 2d0 (/ pi 2)) (geo:normalize-angle (dom:arc-end-angle arc)))))
      (is (angle-close-p (+ 0.4d0 (/ pi 2)) (dom:ellipse-rotation (turned 7))))
      (is (angle-close-p (+ 0.2d0 (/ pi 2)) (dom:polygon-rotation (turned 8)))))))

(defun round-to-ten-thousandth (value)
  "Return VALUE rounded to four decimals, as a double-float."
  (/ (fround (* value 10000d0)) 10000d0))

(test a-rectangle-turned-to-a-slant-becomes-a-closed-polyline
  (let* ((rectangle (first (list (dom:make-rectangle :id 2 :layer-id 0 :min-x 0d0 :min-y 0d0 :width 100d0
                                                     :height 50d0))))
         (slanted (dom:entity-rotated rectangle 0d0 0d0 (/ pi 4)))
         (half-turn (dom:entity-rotated rectangle 0d0 0d0 pi)))
    (is (eq :polyline (dom:entity-kind slanted)))
    (is-true (dom:polyline-closed slanted))
    (is (= 2 (dom:entity-id slanted)) "with the id and the layer it had")
    (is (= 8 (length (dom:polyline-coordinates slanted))))
    (is (eq :rectangle (dom:entity-kind half-turn)) "a half turn keeps it a rectangle")
    (is (close-to-p -100d0 (dom:rectangle-min-x half-turn)))))

;;; Mirroring

(test a-mirror-reflects-each-kind
  (let ((entities (one-of-each)))
    (flet ((mirrored (index)
             (dom:entity-mirrored (nth index entities) 0d0 0d0 1d0 0d0)))
      (let ((line (mirrored 0)))
        (is (and (close-to-p -50d0 (dom:line-y1 line)) (close-to-p -250d0 (dom:line-y2 line)))))
      (let ((arc (mirrored 6)))
        (is (angle-close-p (- 2d0) (dom:arc-start-angle arc)) "start and end swap, so it still turns counter-clockwise")
        (is (angle-close-p (- 0.5d0) (dom:arc-end-angle arc)))
        (is (close-to-p -20d0 (dom:arc-center-y arc))))
      (is (angle-close-p 0d0 (* 2 (- -0.4d0 (dom:ellipse-rotation (mirrored 7)))))
          "an ellipse looks the same turned half a turn")
      (is (angle-close-p -0.2d0 (dom:polygon-rotation (mirrored 8))))
      (let ((text (mirrored 4)))
        (is (close-to-p 0.3d0 (dom:text-angle text)) "a text is not turned back to front")
        (is (close-to-p -40d0 (dom:text-y text))))
      (is (close-to-p -300d0 (dom:dimension-offset (mirrored 5))) "a dimension goes to the other side of its line")
      (let ((rectangle (mirrored 1)))
        (is (eq :rectangle (dom:entity-kind rectangle)))
        (is (close-to-p -60d0 (dom:rectangle-min-y rectangle)))))
    (is (eq :polyline (dom:entity-kind (dom:entity-mirrored (nth 1 entities) 0d0 0d0 1d0 0.5d0)))
        "across a slanted line a rectangle is no longer one")))

;;; Scaling

(test scaling-multiplies-every-length-about-the-center
  (let ((entities (one-of-each)))
    (flet ((scaled (index)
             (dom:entity-scaled (nth index entities) 10d0 10d0 2d0)))
      (is (= 20d0 (dom:circle-radius (scaled 2))))
      (is (close-to-p 190d0 (dom:circle-center-x (scaled 2))))
      (is (= 40d0 (dom:text-height (scaled 4))))
      (let ((dimension (scaled 5)))
        (is (= 600d0 (dom:dimension-offset dimension)))
        (is (= 200d0 (dom:dimension-text-height dimension)))
        (is (close-to-p 1990d0 (dom:dimension-x2 dimension))))
      (is (= 200d0 (dom:arc-radius (scaled 6))))
      (is (= 160d0 (dom:ellipse-radius-x (scaled 7))))
      (is (= 100d0 (dom:polygon-radius (scaled 8))))
      (let ((rectangle (scaled 1)))
        (is (close-to-p 60d0 (dom:rectangle-width rectangle)))
        (is (close-to-p 80d0 (dom:rectangle-height rectangle)))
        (is (close-to-p 10d0 (dom:rectangle-min-x rectangle)) "10 is the center, so it stays")))))

(test turning-and-turning-back-or-mirroring-twice-or-scaling-and-back-restores-everything
  (dolist (entity (one-of-each))
    (let ((original (the-entity-signature entity)))
      ;; A rectangle that was slanted is a polyline and stays one, so it is left out of these two.
      (unless (eq :rectangle (dom:entity-kind entity))
        (let ((turned-back (dom:entity-rotated (dom:entity-rotated entity 30d0 -20d0 1.1d0) 30d0 -20d0 -1.1d0))
              (mirrored-back (dom:entity-mirrored (dom:entity-mirrored entity 0d0 5d0 10d0 12d0) 0d0 5d0 10d0 12d0)))
          (is (signatures-close-p original (the-entity-signature turned-back)) "rotate ~a" (dom:entity-kind entity))
          (is (signatures-close-p original (the-entity-signature mirrored-back)) "mirror ~a" (dom:entity-kind entity))))
      (is (signatures-close-p original
                              (the-entity-signature (dom:entity-scaled (dom:entity-scaled entity 7d0 9d0 2.5d0)
                                                                       7d0 9d0 0.4d0)))
          "scale ~a" (dom:entity-kind entity)))))

(test the-id-and-the-layer-are-kept
  (dolist (entity (one-of-each))
    (dolist (result (list (dom:entity-rotated entity 0d0 0d0 0.7d0)
                          (dom:entity-mirrored entity 0d0 0d0 3d0 1d0)
                          (dom:entity-scaled entity 0d0 0d0 1.5d0)))
      (is (= (dom:entity-id entity) (dom:entity-id result)))
      (is (= (dom:entity-layer-id entity) (dom:entity-layer-id result))))))

(test a-result-that-would-not-be-valid-is-refused
  (let ((circle (dom:make-circle :id 1 :layer-id 0 :center-x 0d0 :center-y 0d0 :radius 1000d0)))
    (signals base:invalid-entity (dom:entity-scaled circle 0d0 0d0 1d9))
    (signals base:invalid-entity (dom:entity-scaled circle 0d0 0d0 1d-12))
    (signals base:invalid-entity
      (dom:entity-rotated (dom:make-line :id 2 :layer-id 0 :x1 9d6 :y1 9d6 :x2 9.5d6 :y2 9d6)
                          0d0
                          0d0
                          (/ pi 4)))))

;;; The change sets

(test a-change-set-replaces-the-originals-or-adds-copies
  (let ((drawing (transformed-drawing)))
    (multiple-value-bind (changes skipped) (dom:rotate-entities-changes drawing '(1 3) 0d0 0d0 (/ pi 2))
      (is (= 2 (length changes)))
      (is (= 0 skipped))
      (is (equal '(1 3) (mapcar #'dom:change-entity-id changes)))
      (is (every #'dom:change-before changes) "each replaces what was there"))
    (multiple-value-bind (changes) (dom:mirror-entities-changes drawing '(1 3) 0d0 0d0 1d0 0d0 :copy t)
      (is (= 2 (length changes)))
      (is (equal '(10 11) (mapcar #'dom:change-entity-id changes)) "the next ids")
      (is (every (lambda (change) (null (dom:change-before change))) changes) "each is new")
      (dom:apply-changes drawing changes)
      (is (= 11 (dom:drawing-entity-count drawing)))
      (is (= 100d0 (dom:line-x1 (dom:store-find (dom:drawing-store drawing) 1))) "the originals did not move"))))

(test entities-on-a-locked-layer-are-skipped
  (let* ((drawing (transformed-drawing))
         (layer (dom:add-layer drawing "fixed" (dom:make-rgb 1 2 3))))
    (dom:apply-changes drawing
                       (list (dom:make-change 3
                                              (dom:store-find (dom:drawing-store drawing) 3)
                                              (dom:entity-with-layer (dom:store-find (dom:drawing-store drawing) 3)
                                                                     (dom:layer-id layer)))))
    (dom:set-layer-locked drawing (dom:layer-id layer) t)
    (multiple-value-bind (changes skipped) (dom:scale-entities-changes drawing '(1 3 99) 0d0 0d0 2d0)
      (is (equal '(1) (mapcar #'dom:change-entity-id changes)))
      (is (= 2 skipped) "the locked one and the one that is not there"))))

(test a-transformation-is-one-step-of-undo
  (let* ((drawing (transformed-drawing))
         (history (dom:make-history))
         (before (mapcar #'the-entity-signature (dom:drawing-entities drawing))))
    (dom:history-commit history
                        drawing
                        (dom:rotate-entities-changes drawing '(1 2 3 4 5 6 7 8 9) 0d0 0d0 1d0))
    (is (not (equal before (mapcar #'the-entity-signature (dom:drawing-entities drawing)))))
    (dom:history-undo history drawing)
    (is (equal before (mapcar #'the-entity-signature (dom:drawing-entities drawing))))))

;;; The tools

(defun app-with-selection (&rest entities)
  "Return an application, with the grid and the object snaps off, whose drawing holds ENTITIES, all selected."
  (let ((application (make-test-app)))
    (dom:set-snap-to-grid (app:app-drawing application) nil)
    (dom:set-object-snap (app:app-drawing application) nil)
    (app:commit-changes application (dom:create-entities-changes entities) "entity.create")
    (app::select-ids application (mapcar #'dom:entity-id entities))
    application))

(defun choose-modify-tool (application
                           kind)
  "Open the modify menu with the mouse and click the row of the tool KIND."
  (click-widget application :modify)
  (click-widget application (list :menu-item kind)))

(test the-modify-button-opens-a-list-of-the-tools-that-change-the-selection
  (let ((application (app-with-selection (snap-line 1 0 0 1000 0))))
    (click-widget application :modify)
    (is (equal '((:menu-item :rotate) (:menu-item :mirror) (:menu-item :scale))
               (mapcar #'ui:widget-id (menu-rows application))))
    (is (equal '("Shift+R" "Shift+M" "Shift+S") (mapcar #'ui:widget-shortcut (menu-rows application))))
    (click-widget application '(:menu-item :mirror))
    (is (eq :mirror (app:tool-kind (app:app-tool application))))
    (is (ui:widget-active (find-widget application :modify)) "the Modify button lights up")
    (is (not (ui:widget-active (find-widget application :draw))))))

(test shift-and-a-letter-choose-the-modify-tools
  (let ((application (app-with-selection (snap-line 1 0 0 1000 0))))
    (press application :r :shift)
    (is (eq :rotate (app:tool-kind (app:app-tool application))))
    (press application :escape)
    (press application :m :shift)
    (is (eq :mirror (app:tool-kind (app:app-tool application))))
    (press application :escape)
    (press application :s :shift)
    (is (eq :scale (app:tool-kind (app:app-tool application))))
    (press application :escape)
    (press application :r)
    (is (eq :rectangle (app:tool-kind (app:app-tool application))) "the plain letters are as they were")
    (press application :s)
    (is (eq :rectangle (app:tool-kind (app:app-tool application))) "S toggles the snap")))

(test with-nothing-selected-a-modify-tool-says-so-and-stays-out
  (let ((application (make-test-app)))
    (press application :r :shift)
    (is (eq :select (app:tool-kind (app:app-tool application))))
    (is (eq :error (cdr (app:app-message application))))
    (is (search "Select what you want to change" (message-text application)))))

(test rotate-turns-the-selection-by-the-direction-of-the-second-click
  (let ((application (app-with-selection (snap-line 1 1000 0 2000 0))))
    (choose-modify-tool application :rotate)
    (click application 0 0)
    (is (app:tool-in-progress-p (app:app-tool application)))
    (move-pointer-to application 0 1500)
    (is (= 1 (length (app:tool-preview-entities (app:app-tool application) application))) "the preview")
    (click application 0 1500)
    (let ((line (dom:store-find (dom:drawing-store (app:app-drawing application)) 1)))
      (is (close-to-p 0d0 (dom:line-x1 line) 1d-6))
      (is (close-to-p 1000d0 (dom:line-y1 line) 1d-6))
      (is (close-to-p 2000d0 (dom:line-y2 line) 1d-6) "turned a quarter, to point straight up"))
    (is (eq :select (app:tool-kind (app:app-tool application))) "and the tool is done")
    (is (equal '(1) (app:selection-ids application)) "the selection stays")
    (is (search "Rotated 1" (message-text application)))
    (app:operation-undo application)
    (is (close-to-p 1000d0 (dom:line-x1 (dom:store-find (dom:drawing-store (app:app-drawing application)) 1)) 1d-6))))

(test an-angle-typed-in-degrees-turns-the-selection
  (let ((application (app-with-selection (snap-line 1 1000 0 2000 0))))
    (press application :r :shift)
    (click application 0 0)
    (typed-point application "90")
    (let ((line (dom:store-find (dom:drawing-store (app:app-drawing application)) 1)))
      (is (close-to-p 1000d0 (dom:line-y1 line) 1d-6)))
    (is (eq :select (app:tool-kind (app:app-tool application))))))

(test the-center-can-be-typed-and-an-angle-before-the-center-is-not-an-angle
  (let ((application (app-with-selection (snap-line 1 1000 0 2000 0))))
    (press application :r :shift)
    (typed-point application "90")
    (is (eq :error (cdr (app:app-message application))) "no center yet, so 90 is not a point either")
    (press application :escape)
    (typed-point application "0;0")
    (is (app:tool-in-progress-p (app:app-tool application)))))

(test control-at-the-last-click-keeps-the-originals-and-selects-the-copies
  (let ((application (app-with-selection (snap-circle 1 1000 0 100))))
    (press application :r :shift)
    (click application 0 0)
    (send application
          (pointer application :move nil 0 1500 '(:control))
          (pointer application :down :left 0 1500 '(:control))
          (pointer application :up :left 0 1500 '(:control)))
    (is (= 2 (length (entities-of application))))
    (is (equal '(2) (app:selection-ids application)) "the copy is what is selected")
    (is (close-to-p 1000d0 (dom:circle-center-x (first (entities-of application))) 1d-6) "the original stayed")
    (is (close-to-p 1000d0 (dom:circle-center-y (second (entities-of application))) 1d-6))))

(test mirror-asks-whether-to-keep-the-originals
  (let ((application (app-with-selection (snap-line 1 1000 500 2000 800))))
    (press application :m :shift)
    (click application 0 0)
    (move-pointer-to application 1000 0)
    (is (= 1 (length (app:tool-preview-entities (app:app-tool application) application))))
    (click application 1000 0)
    (is (string= "Mirror" (prompt-title-of application)))
    (is (equal '("Keep" "Delete" "Cancel") (prompt-labels application)))
    (choose-in-prompt application 0)
    (is (= 2 (length (entities-of application))) "the originals are kept")
    (let ((copy (second (entities-of application))))
      (is (close-to-p -500d0 (dom:line-y1 copy) 1d-6))
      (is (close-to-p -800d0 (dom:line-y2 copy) 1d-6)))
    (is (eq :select (app:tool-kind (app:app-tool application))))))

(test mirror-can-delete-the-originals-or-be-cancelled
  (let ((application (app-with-selection (snap-line 1 1000 500 2000 800))))
    (press application :m :shift)
    (click application 0 0)
    (click application 1000 0)
    (choose-in-prompt application 1)
    (is (= 1 (length (entities-of application))))
    (is (close-to-p -500d0 (dom:line-y1 (first (entities-of application))) 1d-6) "the original became its reflection"))
  (let ((application (app-with-selection (snap-line 1 1000 500 2000 800))))
    (press application :m :shift)
    (click application 0 0)
    (click application 1000 0)
    (choose-in-prompt application 2)
    (is (= 1 (length (entities-of application))))
    (is (close-to-p 500d0 (dom:line-y1 (first (entities-of application))) 1d-6) "nothing happened")))

(test scale-takes-the-reference-and-the-target
  (let ((application (app-with-selection (snap-circle 1 1000 0 100))))
    (press application :s :shift)
    (click application 0 0)
    (click application 500 0)
    (move-pointer-to application 1500 0)
    (is (= 1 (length (app:tool-preview-entities (app:app-tool application) application))))
    (click application 1500 0)
    (let ((circle (first (entities-of application))))
      (is (close-to-p 300d0 (dom:circle-radius circle) 1d-6) "from 500 to 1500 is three times")
      (is (close-to-p 3000d0 (dom:circle-center-x circle) 1d-6)))
    (is (search "Scaled 1" (message-text application)))))

(test a-scale-factor-can-be-typed-and-must-be-above-zero
  (let ((application (app-with-selection (snap-circle 1 1000 0 100))))
    (press application :s :shift)
    (click application 0 0)
    (typed-point application "0")
    (is (eq :error (cdr (app:app-message application))))
    (is (= 100d0 (dom:circle-radius (first (entities-of application)))) "nothing changed")
    (typed-point application "2.5")
    (is (close-to-p 250d0 (dom:circle-radius (first (entities-of application))) 1d-6))
    (is (close-to-p 2500d0 (dom:circle-center-x (first (entities-of application))) 1d-6))))

(test escape-cancels-a-modify-tool-and-then-leaves-it
  (let ((application (app-with-selection (snap-line 1 0 0 1000 0))))
    (press application :r :shift)
    (click application 0 0)
    (press application :escape)
    (is (not (app:tool-in-progress-p (app:app-tool application))))
    (is (eq :rotate (app:tool-kind (app:app-tool application))))
    (press application :escape)
    (is (eq :select (app:tool-kind (app:app-tool application))))
    (is (equal '(1) (app:selection-ids application)) "and the selection was kept")))

(test a-transformation-that-would-be-invalid-is-said-and-changes-nothing
  (let ((application (app-with-selection (snap-circle 1 1000 0 100))))
    (press application :s :shift)
    (click application 0 0)
    (typed-point application "1e9")
    (is (eq :error (cdr (app:app-message application))))
    (is (= 100d0 (dom:circle-radius (first (entities-of application)))))))

(test entities-on-a-locked-layer-stay-and-it-is-said
  (let ((application (app-with-selection (snap-line 1 0 0 1000 0))))
    (let ((layer (dom:add-layer (app:app-drawing application) "fixed" (dom:make-rgb 1 2 3))))
      (app:commit-changes application
                          (list (dom:make-change 1
                                                 (first (entities-of application))
                                                 (dom:entity-with-layer (first (entities-of application))
                                                                        (dom:layer-id layer))))
                          "entity.edit")
      (dom:set-layer-locked (app:app-drawing application) (dom:layer-id layer) t))
    (app::select-ids application '(1))
    (press application :s :shift)
    (click application 0 0)
    (typed-point application "2")
    (is (eq :error (cdr (app:app-message application))))
    (is (search "locked" (message-text application)))
    (is (= 1000d0 (dom:line-x2 (first (entities-of application)))))))

(test every-kind-is-transformed-together-by-the-tools
  (let ((application (apply #'app-with-selection (one-of-each))))
    (press application :r :shift)
    (click application 0 0)
    (typed-point application "30")
    (is (= 9 (length (entities-of application))))
    (is (search "Rotated 9" (message-text application)))
    (app:operation-undo application)
    (is (equal (mapcar #'the-entity-signature (one-of-each))
               (mapcar #'the-entity-signature (entities-of application))))))

;;; The console commands

(test the-console-rotates-mirrors-and-scales
  (let* ((drawing (dom:make-drawing))
         (context (script-env drawing)))
    (ran-ok context "(line 1000 0 2000 0) (circle 500 0 100)")
    (is (equal '("2") (ran-ok context "(rotate '(1 2) 90)")))
    (is (close-to-p 1000d0 (dom:line-y1 (stored context 1)) 1d-6))
    (is (close-to-p 500d0 (dom:circle-center-y (stored context 2)) 1d-6))
    (is (equal '("1") (ran-ok context "(scale 2 3 :about-x 0 :about-y 500)")))
    (is (close-to-p 300d0 (dom:circle-radius (stored context 2)) 1d-6))
    (is (equal '("1") (ran-ok context "(mirror 1 0 0 1 0)")))
    (is (close-to-p -1000d0 (dom:line-y1 (stored context 1)) 1d-6))
    (is (close-to-p 0d0 (dom:line-x1 (stored context 1)) 1d-6))))

(test with-copy-the-console-adds-copies-and-returns-their-ids
  (let* ((drawing (dom:make-drawing))
         (context (script-env drawing)))
    (ran-ok context "(line 0 0 1000 0)")
    (is (equal '("(2)") (ran-ok context "(rotate 1 90 :copy t)")))
    (is (equal '("(3)") (ran-ok context "(mirror 1 0 100 1 100 :copy t)")))
    (is (equal '("(4)") (ran-ok context "(scale 1 2 :copy t)")))
    (is (= 4 (dom:drawing-entity-count drawing)))
    (is (close-to-p 1000d0 (dom:line-x2 (stored context 1)) 1d-9) "the original is as it was")
    (is (close-to-p 2000d0 (dom:line-x2 (stored context 4)) 1d-9))
    (is (close-to-p 200d0 (dom:line-y2 (stored context 3)) 1d-9))))

(test an-angle-is-in-degrees-or-radians-and-a-bad-command-leaves-nothing
  (let* ((drawing (dom:make-drawing))
         (context (script-env drawing)))
    (ran-ok context "(line 1000 0 2000 0)")
    (ran-ok context "(rotate 1 (rad (/ pi 2)))")
    (is (close-to-p 1000d0 (dom:line-y1 (stored context 1)) 1d-6))
    (is (search "above zero" (failure-of context "(scale 1 0)")))
    (is (search "apart" (failure-of context "(mirror 1 5 5 5 5)")))
    (is (search "no entity" (string-downcase (failure-of context "(rotate 99 10)"))))
    (is (search "Invalid" (failure-of context "(rotate 1 10) (scale 1 1d9)")))
    (is (close-to-p 1000d0 (dom:line-y1 (stored context 1)) 1d-6) "the rotation before the failure was taken back")))

(test the-console-help-lists-the-new-commands
  (let ((overview (script:script-result-output (run-script (script-env) "(help)"))))
    (is (search "rotate mirror scale" overview))))

;;; Languages

(test the-modify-menu-and-the-prompts-speak-portuguese-and-russian
  (dolist (language '(:pt-br :ru-ru))
    (i18n:with-language (language)
      (let ((application (app-with-selection (snap-line 1 0 0 1000 0))))
        (click-widget application :modify)
        (is (null (untranslated-texts (remove nil (mapcar #'ui:widget-label (app:build-widgets application)))
                                      language))
            "the menu in ~a" language)
        (click-widget application '(:menu-item :mirror))
        (click application 0 0)
        (click application 1000 0)
        (is (string/= "Mirror" (prompt-title-of application)) "~a" language)
        (is (null (untranslated-texts (prompt-labels application) language)))))))
