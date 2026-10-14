(in-package #:borba-cad/tests)

(def-suite ui
  :description "Window layout, text fitting, text editing, widgets and the overlay."
  :in all)

(in-suite ui)

;;; Layout

(test the-layout-tiles-the-window-without-gaps
  (let* ((layout (ui:compute-layout 1440 900))
         (toolbar (ui:layout-toolbar layout))
         (canvas (ui:layout-canvas layout))
         (side (ui:layout-side-panel layout))
         (status (ui:layout-status-bar layout)))
    (is (= ui:+toolbar-height+ (ui:rect-height toolbar)))
    (is (= ui:+status-height+ (ui:rect-height status)))
    (is (= (- 1440 ui:+side-panel-width+) (ui:rect-width canvas)))
    (is (= (ui:rect-x side) (+ (ui:rect-x canvas) (ui:rect-width canvas))))
    (is (= 900 (+ (ui:rect-height toolbar) (ui:rect-height canvas) (ui:rect-height status))))
    (is (= (ui:rect-y status) (+ (ui:rect-y canvas) (ui:rect-height canvas))))
    (let ((layers (ui:layout-layers-panel layout))
          (inspector (ui:layout-inspector-panel layout)))
      (is (= (ui:rect-height side) (+ (ui:rect-height layers) (ui:rect-height inspector))))
      (is (< (abs (- (ui:rect-y inspector) (+ (ui:rect-y layers) (ui:rect-height layers)))) 1e-3)))))

(test the-layout-survives-tiny-windows
  (let ((layout (ui:compute-layout 200 50)))
    (is (>= (ui:rect-width (ui:layout-canvas layout)) 0f0))
    (is (>= (ui:rect-height (ui:layout-canvas layout)) 0f0))))

(test rect-containment-includes-the-edges
  (let ((rect (ui:make-rect 10f0 20f0 30f0 40f0)))
    (is (ui:rect-contains-p rect 10 20))
    (is (ui:rect-contains-p rect 40 60))
    (is (not (ui:rect-contains-p rect 40.1 60)))
    (is (not (ui:rect-contains-p rect 9 30)))))

;;; Text fitting

(test text-width-uses-the-fixed-advance
  (is (< (abs (- 84f0 (ui:text-width-pixels "0123456789" 14f0))) 1e-4))
  (is (= 0f0 (ui:text-width-pixels "" 14f0))))

(test fit-text-truncates-with-an-ellipsis-only-when-needed
  (is (string= "short" (ui:fit-text "short" 200f0)))
  (let ((fitted (ui:fit-text "a very long layer name indeed" 100f0)))   ; capacity floor(100 / 8.4) = 11
    (is (= 11 (length fitted)))
    (is (string= "..." (subseq fitted 8))))
  (is (string= "ab" (ui:fit-text "abcdef" 17f0)))                       ; capacity 2: no room for an ellipsis
  (is (string= "" (ui:fit-text "abc" 1f0)))
  (is (string= "" (ui:fit-text "" 10f0))))

;;; Text editing

(defun edited (&rest steps)
  "Apply STEPS to an empty edit state. A string step inserts, a keyword step is a key."
  (let ((state (ui:make-edit-state)))
    (dolist (step steps state)
      (setf state (if (stringp step)
                      (ui:edit-insert state step)
                      (ui:edit-handle-key state step))))))

(test editing-inserts-at-the-caret
  (let ((state (edited "abc" :left :left "X")))
    (is (string= "aXbc" (ui:edit-state-text state)))
    (is (= 2 (ui:edit-state-caret state)))))

(test editing-backspace-and-delete-respect-the-boundaries
  (is (string= "ab" (ui:edit-state-text (edited "abc" :backspace))))
  (is (string= "" (ui:edit-state-text (edited :backspace))))                 ; nothing to delete
  (is (string= "bc" (ui:edit-state-text (edited "abc" :home :delete))))
  (is (string= "abc" (ui:edit-state-text (edited "abc" :delete))))           ; caret at the end
  (is (string= "ac" (ui:edit-state-text (edited "abc" :left :backspace)))))

(test caret-movement-is-clamped
  (is (= 0 (ui:edit-state-caret (edited "abc" :left :left :left :left :left))))
  (is (= 3 (ui:edit-state-caret (edited "abc" :home :right :right :right :right :right))))
  (is (= 0 (ui:edit-state-caret (edited "abc" :home))))
  (is (= 3 (ui:edit-state-caret (edited "abc" :home :end)))))

(test editing-drops-control-characters-and-honors-the-maximum-length
  (is (string= "ab" (ui:edit-state-text (edited (format nil "a~%b")))))
  (is (string= "ab" (ui:edit-state-text (edited (coerce (list #\a (code-char 9) #\b (code-char 127)) 'string)))))
  (let ((state (ui:edit-insert (ui:make-edit-state :maximum-length 5) "abcdefgh")))
    (is (string= "abcde" (ui:edit-state-text state)))
    (is (string= "abcde" (ui:edit-state-text (ui:edit-insert state "more"))))))

(test editing-works-on-code-points
  (let ((state (edited (coerce (list (code-char 233) (code-char 231) (code-char 128512)) 'string) :backspace)))
    (is (= 2 (length (ui:edit-state-text state))))
    (is (= 2 (ui:edit-state-caret state)))))

(test selecting-all-makes-the-next-input-replace-the-text
  (let ((selected (ui:edit-select-all-replace (ui:make-edit-state :text "old value"))))
    (is (string= "new" (ui:edit-state-text (ui:edit-insert selected "new"))))
    (is (string= "" (ui:edit-state-text (ui:edit-backspace selected))))
    (is (string= "" (ui:edit-state-text (ui:edit-delete selected))))
    (is (string= "old value" (ui:edit-state-text (ui:edit-move selected :end))))))   ; moving clears the selection

(test only-editing-keys-are-handled
  (is (null (ui:edit-handle-key (ui:make-edit-state) :enter)))
  (is (null (ui:edit-handle-key (ui:make-edit-state) :escape)))
  (is (ui:edit-handle-key (ui:make-edit-state :text "a") :backspace)))

;;; Widgets

(defun a-widget (kind id x y &key (width 100f0) (height 20f0) label active (enabled t) blocked swatch edit)
  "Build a widget with convenient defaults."
  (ui:make-widget kind id x y width height :label label :active active :enabled enabled :blocked blocked
                                           :swatch swatch :edit edit))

(test hit-testing-finds-the-topmost-interactive-widget
  (let* ((panel (a-widget :panel :panel 0f0 0f0 :width 400f0 :height 400f0))
         (bottom (a-widget :button :bottom 10f0 10f0))
         (top (a-widget :button :top 50f0 10f0))
         (widgets (list panel bottom top)))
    (is (eq bottom (ui:widget-at widgets 20 15)))
    (is (eq top (ui:widget-at widgets 60 15)))
    (is (eq top (ui:widget-at widgets 110 30)))                           ; the shared edge belongs to the later widget
    (is (null (ui:widget-at widgets 300 300)))                            ; only the panel is there
    (is (null (ui:widget-at widgets 5 5)))))

(test hit-testing-ignores-disabled-blocked-and-non-interactive-widgets
  (let ((widgets (list (a-widget :button :disabled 0f0 0f0 :enabled nil)
                       (a-widget :row :blocked 200f0 0f0 :blocked t)
                       (a-widget :label :label 400f0 0f0)
                       (a-widget :swatch :swatch 600f0 0f0)
                       (a-widget :field :field 800f0 0f0))))
    (is (null (ui:widget-at widgets 10 10)))
    (is (null (ui:widget-at widgets 210 10)))
    (is (null (ui:widget-at widgets 410 10)))
    (is (null (ui:widget-at widgets 610 10)))
    (is (eq :field (ui:widget-id (ui:widget-at widgets 810 10))))))

(defun draw-ui (widgets &key (pointer-x -1f0) (pointer-y -1f0))
  "Draw WIDGETS into a fresh draw list and return it."
  (let ((list (scene:make-draw-list)))
    (ui:draw-widgets list (scene:default-theme) widgets pointer-x pointer-y)
    list))

(test a-button-draws-a-fill-an-outline-and-a-centered-label
  (let* ((list (draw-ui (list (a-widget :button :ok 100f0 50f0 :label "OK"))))
         (run (aref (scene:draw-list-text-runs list) 0)))
    (is (= 2 (scene:draw-list-triangle-count list)))
    (is (= 4 (scene:draw-list-line-count list)))
    (is (string= "OK" (scene:text-run-string run)))
    ;; Centered: the text box (2 x 8.4 px) is centered in the 100 px wide button.
    (is (< (abs (- (+ 100f0 (/ (- 100f0 16.8f0) 2)) (scene:text-run-x run))) 1e-3))))

(defun fill-color (list)
  "Return the color of the first vertex of LIST as (r g b a)."
  (let ((array (scene:draw-list-vertices list)))
    (list (aref array 4) (aref array 5) (aref array 6) (aref array 7))))

(defun rgba-list (rgba)
  "Return RGBA as (r g b a)."
  (list (scene:rgba-r rgba) (scene:rgba-g rgba) (scene:rgba-b rgba) (scene:rgba-a rgba)))

(test buttons-change-color-when-hovered-or-active
  (let ((theme (scene:default-theme))
        (button (a-widget :button :b 100f0 50f0 :label "x")))
    (is (equal (rgba-list (scene:theme-button theme)) (fill-color (draw-ui (list button)))))
    (is (equal (rgba-list (scene:theme-button-hover theme))
               (fill-color (draw-ui (list button) :pointer-x 120f0 :pointer-y 60f0))))
    (is (equal (rgba-list (scene:theme-button-active theme))
               (fill-color (draw-ui (list (a-widget :button :b 100f0 50f0 :label "x" :active t))))))
    ;; A blocked button never shows hover.
    (is (equal (rgba-list (scene:theme-button theme))
               (fill-color (draw-ui (list (a-widget :button :b 100f0 50f0 :label "x" :blocked t))
                                    :pointer-x 120f0 :pointer-y 60f0))))))

(test long-labels-are-truncated-to-fit
  (let* ((list (draw-ui (list (a-widget :button :b 0f0 0f0 :width 100f0 :label "an extremely long button label"))))
         (text (scene:text-run-string (aref (scene:draw-list-text-runs list) 0))))
    (is (< (length text) (length "an extremely long button label")))
    (is (string= "..." (subseq text (- (length text) 3))))))

(test rows-show-a-swatch-and-a-left-aligned-label
  (let* ((swatch (scene:make-rgba 1f0 0f0 0f0 1f0))
         (widget (ui:make-widget :row :layer 0f0 100f0 300f0 24f0 :label "walls" :swatch swatch :align :left))
         (list (draw-ui (list widget)))
         (run (aref (scene:draw-list-text-runs list) 0)))
    (is (= 2 (scene:draw-list-triangle-count list)))                  ; the swatch fill (rows have no fill idle)
    (is (> (scene:text-run-x run) (+ 0f0 ui:+padding+ 14f0)))))

(test a-focused-field-draws-its-text-and-a-caret-and-an-idle-one-its-label
  (let* ((edit (ui:make-edit-state :text "1250" :caret 2))
         (focused (draw-ui (list (a-widget :field :f 10f0 10f0 :width 200f0 :height 24f0 :label "1250" :edit edit))))
         (idle (draw-ui (list (a-widget :field :f 10f0 10f0 :width 200f0 :height 24f0 :label "1250")))))
    (is (string= "1250" (scene:text-run-string (aref (scene:draw-list-text-runs focused) 0))))
    ;; Outline (4) plus the caret (1).
    (is (= 5 (scene:draw-list-line-count focused)))
    (is (= 4 (scene:draw-list-line-count idle)))
    (let* ((lines (draw-lines focused))
           (caret (car (last lines))))
      ;; The caret sits after two characters: 10 + 8 + 2 * 8.4 px.
      (is (< (abs (- (+ 18f0 16.8f0) (first caret))) 1e-3)))
    (is (= 1 (length (scene:draw-list-text-runs idle))))))

(test a-selected-field-text-is-highlighted
  (let ((list (draw-ui (list (a-widget :field :f 10f0 10f0 :width 200f0 :height 24f0
                                       :edit (ui:edit-select-all-replace (ui:make-edit-state :text "abc")))))))
    (is (= 4 (scene:draw-list-triangle-count list)))))                    ; the field fill and the selection fill

(test the-overlay-sizes-itself-to-its-longest-line
  (let ((list (scene:make-draw-list)))
    (ui:draw-overlay list (scene:default-theme) '("short" "a considerably longer overlay line") 10f0 20f0)
    (is (= 2 (length (scene:draw-list-text-runs list))))
    (is (= 2 (scene:draw-list-triangle-count list)))
    (let* ((array (scene:draw-list-vertices list))
           (x-values (loop for index below 6 collect (aref array (* index scene:+floats-per-vertex+))))
           (width (- (reduce #'max x-values) (reduce #'min x-values))))
      (is (>= width (ui:text-width-pixels "a considerably longer overlay line" ui:+small-font-pixels+)))))
  (let ((list (scene:make-draw-list)))
    (ui:draw-overlay list (scene:default-theme) '() 0f0 0f0)
    (is (= 0 (scene:draw-list-vertex-count list)))))
