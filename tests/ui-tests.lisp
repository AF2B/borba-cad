(in-package #:borba-cad/tests)

(def-suite ui
  :description "Window layout, text fitting, text editing, widgets and the overlay."
  :in all)

(in-suite ui)

;;; Layout

(test the-layout-tiles-the-window-without-gaps
  (let* ((layout (ui:compute-layout 1440 900))
         (toolbar (ui:layout-toolbar layout))
         (palette (ui:layout-tool-palette layout))
         (canvas (ui:layout-canvas layout))
         (side (ui:layout-side-panel layout))
         (status (ui:layout-status-bar layout)))
    (is (= ui:+toolbar-height+ (ui:rect-height toolbar)))
    (is (= ui:+status-height+ (ui:rect-height status)))
    (is (= ui:+tool-palette-width+ (ui:rect-width palette)))
    (is (= (ui:rect-height canvas) (ui:rect-height palette)))
    (is (= (ui:rect-height canvas) (ui:rect-height side)))
    (is (= (ui:rect-x canvas) (+ (ui:rect-x palette) (ui:rect-width palette))))
    (is (= (- 1440 ui:+side-panel-width+ ui:+tool-palette-width+) (ui:rect-width canvas)))
    (is (= (ui:rect-x side) (+ (ui:rect-x canvas) (ui:rect-width canvas))))
    (is (= 1440 (+ (ui:rect-width palette) (ui:rect-width canvas) (ui:rect-width side))))
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
  (let ((fitted (ui:fit-text "a very long layer name indeed" 100f0 14f0)))   ; capacity floor(100 / 8.4) = 11
    (is (= 11 (length fitted)))
    (is (string= "..." (subseq fitted 8))))
  (is (string= "ab" (ui:fit-text "abcdef" 17f0 14f0)))                        ; capacity 2: no room for an ellipsis
  (is (string= "" (ui:fit-text "abc" 1f0)))
  (is (string= "" (ui:fit-text "" 10f0))))

(defun proportional-measurer (font string size)
  "A text measurer for tests in which an i is narrow, an m is wide, and the strong font is wider."
  (* size (if (eq font :ui-strong) 1.2d0 1d0)
     (loop for character across string
           sum (case character
                 (#\i 0.25d0)
                 (#\m 0.9d0)
                 (t 0.5d0)))))

(test layout-measures-text-with-the-installed-measurer-and-the-font-asked-for
  (let ((ui:*text-measurer* #'proportional-measurer))
    (is (< (abs (- 11.5f0 (ui:text-width-pixels "mi" 10f0 :ui))) 1e-4))
    (is (< (abs (- (* 1.2f0 11.5f0) (ui:text-width-pixels "mi" 10f0 :ui-strong))) 1e-3))
    (is (> (ui:text-width-pixels "mmmm" 10f0) (ui:text-width-pixels "iiii" 10f0)))
    ;; Truncation works on the measured width, not on the character count.
    (is (string= "iiiiiiiiii" (ui:fit-text "iiiiiiiiii" 30f0 10f0)))
    (is (< (length (ui:fit-text "mmmmmmmmmm" 30f0 10f0)) 10)))
  (is (= (ui:text-width-pixels "mi" 10f0 :ui) (ui:text-width-pixels "mi" 10f0 :mono))
      "the default measurer is a fixed advance whatever the font"))

(test wrapping-breaks-at-spaces-and-cuts-long-words
  (flet ((wrap (text width lines) (ui:wrap-text text width lines 10f0)))     ; 6 px per character
    (is (equal '("one two") (wrap "one two" 100f0 3)))
    (is (equal '("one two" "three") (wrap "one two three" 50f0 3)))          ; 8 characters fit per line
    (is (equal '("abcdefgh" "ijkl") (wrap "abcdefghijkl" 50f0 3)))          ; a word wider than a line is cut
    (is (equal '("") (wrap "" 50f0 3)))
    (let ((lines (wrap "one two three four five six seven eight nine ten" 50f0 2)))
      (is (= 2 (length lines)))
      (is (string= "..." (subseq (second lines) (- (length (second lines)) 3))))
      (is (every (lambda (line) (<= (ui:text-width-pixels line 10f0) 50.01f0)) lines)))
    (is (every (lambda (line) (<= (ui:text-width-pixels line 10f0) 50.01f0))
               (wrap "a_very_long_path_without_any_space_at_all/file.bcad" 50f0 4)))))

(test the-visible-window-of-a-long-field-always-contains-the-caret
  (let* ((text "0123456789abcdefghij")                                      ; 20 characters, 6 px each at size 10
         (width 60f0))                                                      ; 10 characters fit
    (multiple-value-bind (start end) (ui:visible-text-window text 3 width 10f0 :ui)
      (is (= 0 start))
      (is (= 10 end)))
    (multiple-value-bind (start end) (ui:visible-text-window text 20 width 10f0 :ui)
      (is (= 10 start))
      (is (= 20 end)))
    (multiple-value-bind (start end) (ui:visible-text-window text 15 width 10f0 :ui)
      (is (<= start 15 end))
      (is (<= (ui:text-width-pixels (subseq text start end) 10f0) 60.01f0)))
    (multiple-value-bind (start end) (ui:visible-text-window "" 0 width 10f0 :ui)
      (is (and (= 0 start) (= 0 end))))))

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

(defun vertex-count-of (list)
  "Return the number of vertices in LIST."
  (scene:draw-list-vertex-count list))

(defun all-vertices (list)
  "Return the vertices of LIST as (x y r g b a) lists."
  (loop for index below (vertex-count-of list) collect (vertex-at list index)))

(defun rgba-list (rgba)
  "Return RGBA as (r g b a)."
  (list (scene:rgba-r rgba) (scene:rgba-g rgba) (scene:rgba-b rgba) (scene:rgba-a rgba)))

(defun fill-color (list)
  "Return the color of the first vertex of LIST as (r g b a)."
  (rest (rest (vertex-at list 0))))

(defun drawn-with-color-p (list rgba)
  "Return true when some vertex of LIST has exactly the color RGBA."
  (let ((wanted (rgba-list rgba)))
    (some (lambda (vertex) (equal wanted (rest (rest vertex)))) (all-vertices list))))

(defun horizontal-extent (list)
  "Return the smallest and largest x among the vertices of LIST."
  (let ((xs (mapcar #'first (all-vertices list))))
    (values (reduce #'min xs) (reduce #'max xs))))

(test a-button-draws-a-body-and-a-centered-label-in-the-interface-font
  (let* ((list (draw-ui (list (a-widget :button :ok 100f0 50f0 :label "OK"))))
         (run (aref (scene:draw-list-text-runs list) 0))
         (text-width (ui:text-width-pixels "OK")))
    (is (plusp (scene:draw-list-triangle-count list)))
    (is (string= "OK" (scene:text-run-string run)))
    (is (eq :ui (scene:text-run-font run)))
    ;; Centered: the text box is centered in the 100 px wide button.
    (is (< (abs (- (+ 100f0 (/ (- 100f0 text-width) 2)) (scene:text-run-x run))) 1e-3))
    ;; And its baseline sits a little below the middle of the 20 px high button.
    (is (< 60f0 (scene:text-run-y run) 70f0))))

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

(test a-primary-button-is-solid-accent-and-a-disabled-one-is-dimmed
  (let* ((theme (scene:default-theme))
         (primary (ui:make-widget :button :ok 0f0 0f0 96f0 32f0 :label "OK" :variant :primary))
         (disabled (ui:make-widget :button :ok 0f0 0f0 96f0 32f0 :label "OK" :enabled nil))
         (primary-list (draw-ui (list primary)))
         (disabled-list (draw-ui (list disabled))))
    (is (equal (rgba-list (scene:theme-accent theme)) (fill-color primary-list)))
    (is (equal (rgba-list (scene:theme-accent-contrast theme))
               (rgba-list (scene:text-run-color (aref (scene:draw-list-text-runs primary-list) 0)))))
    (is (equal (rgba-list (scene:theme-text-faint theme))
               (rgba-list (scene:text-run-color (aref (scene:draw-list-text-runs disabled-list) 0)))))))

(test long-labels-are-truncated-to-fit
  (let* ((list (draw-ui (list (a-widget :button :b 0f0 0f0 :width 100f0 :label "an extremely long button label"))))
         (text (scene:text-run-string (aref (scene:draw-list-text-runs list) 0))))
    (is (< (length text) (length "an extremely long button label")))
    (is (string= "..." (subseq text (- (length text) 3))))))

(test an-icon-button-draws-its-icon-and-no-text
  (let* ((widget (ui:make-widget :button :save 10f0 10f0 36f0 36f0 :icon :save :variant :icon))
         (list (draw-ui (list widget))))
    (is (zerop (length (scene:draw-list-text-runs list))))
    (is (plusp (scene:draw-list-triangle-count list)))
    (multiple-value-bind (left right) (horizontal-extent list)
      (is (>= left 10f0))
      (is (<= right 46f0)))))

(test a-button-with-an-icon-and-a-label-draws-both
  (let ((list (draw-ui (list (ui:make-widget :button :add 0f0 0f0 90f0 28f0 :label "Add" :icon :plus
                                             :variant :ghost :size ui:+small-font-pixels+)
                             ))))
    (is (= 1 (length (scene:draw-list-text-runs list))))
    (is (plusp (scene:draw-list-triangle-count list)))
    (let ((run (aref (scene:draw-list-text-runs list) 0)))
      (is (> (scene:text-run-x run) 20f0) "the label sits after the icon"))))

(test a-toggled-chip-lights-its-dot
  (let* ((theme (scene:default-theme))
         (on (draw-ui (list (ui:make-widget :button :grid 0f0 0f0 62f0 22f0 :label "Grid" :active t
                                            :variant :chip :size ui:+small-font-pixels+))))
         (off (draw-ui (list (ui:make-widget :button :grid 0f0 0f0 62f0 22f0 :label "Grid"
                                             :variant :chip :size ui:+small-font-pixels+)))))
    (is (drawn-with-color-p on (scene:theme-accent theme)))
    (is (not (drawn-with-color-p off (scene:theme-accent theme))))))

(test rows-show-a-swatch-and-a-left-aligned-label
  (let* ((swatch (scene:make-rgba 1f0 0f0 0f0 1f0))
         (widget (ui:make-widget :row :layer 0f0 100f0 300f0 24f0 :label "walls" :swatch swatch :align :left))
         (list (draw-ui (list widget)))
         (run (aref (scene:draw-list-text-runs list) 0)))
    (is (drawn-with-color-p list swatch))
    (is (equal (rgba-list swatch) (fill-color list)) "an idle row draws nothing before the swatch")
    (is (> (scene:text-run-x run) (+ 0f0 ui:+padding+ 14f0)))))

(test the-current-row-is-highlighted-and-reserves-room-for-the-buttons-over-it
  (let* ((theme (scene:default-theme))
         (plain (ui:make-widget :row :layer 0f0 0f0 200f0 28f0 :label "a very long layer name for this row"
                                :align :left))
         (current (ui:make-widget :row :layer 0f0 0f0 200f0 28f0 :label "a very long layer name for this row"
                                  :align :left :active t :trailing 60f0))
         (current-list (draw-ui (list current)))
         (plain-list (draw-ui (list plain))))
    (is (drawn-with-color-p current-list (scene:theme-accent-soft theme)))
    (is (not (drawn-with-color-p plain-list (scene:theme-accent-soft theme))))
    (let ((run (aref (scene:draw-list-text-runs current-list) 0))
          (plain-run (aref (scene:draw-list-text-runs plain-list) 0)))
      (is (< (ui:text-width-pixels (scene:text-run-string run))
             (ui:text-width-pixels (scene:text-run-string plain-run)))
          "the trailing space shortens the label"))))

(test a-focused-field-draws-its-text-a-caret-and-a-focus-ring-and-an-idle-one-its-label
  (let* ((theme (scene:default-theme))
         (edit (ui:make-edit-state :text "1250" :caret 2))
         (focused (draw-ui (list (a-widget :field :f 10f0 10f0 :width 200f0 :height 24f0 :label "1250" :edit edit))))
         (idle (draw-ui (list (a-widget :field :f 10f0 10f0 :width 200f0 :height 24f0 :label "1250")))))
    (is (string= "1250" (scene:text-run-string (aref (scene:draw-list-text-runs focused) 0))))
    (is (= 1 (length (scene:draw-list-text-runs idle))))
    (is (drawn-with-color-p focused (scene:theme-accent theme)))
    (is (not (drawn-with-color-p idle (scene:theme-accent theme))))
    ;; The caret sits after two characters: the padding plus the width of "12".
    (let* ((caret-x (+ 10f0 ui:+padding+ (ui:text-width-pixels "12")))
           (caret-vertices (remove-if-not (lambda (vertex)
                                            (and (< (abs (- (first vertex) caret-x)) 1f0)
                                                 (<= 15.9f0 (second vertex) 28.1f0)
                                                 (equal (rgba-list (scene:theme-accent theme)) (cddr vertex))))
                                          (all-vertices focused))))
      (is (plusp (length caret-vertices))))))

(test a-long-text-being-edited-shows-only-the-part-around-the-caret
  (let* ((long (make-string 80 :initial-element #\x))
         (list (draw-ui (list (a-widget :field :f 0f0 0f0 :width 200f0 :height 24f0 :label long
                                        :edit (ui:make-edit-state :text long :caret 80)))))
         (shown (scene:text-run-string (aref (scene:draw-list-text-runs list) 0))))
    (is (< (length shown) 80))
    (is (<= (ui:text-width-pixels shown) (- 200f0 (* 2 ui:+padding+) -0.01f0)))))

(test a-selected-field-text-is-highlighted
  (let* ((theme (scene:default-theme))
         (list (draw-ui (list (a-widget :field :f 10f0 10f0 :width 200f0 :height 24f0
                                        :edit (ui:edit-select-all-replace (ui:make-edit-state :text "abc")))))))
    (is (drawn-with-color-p list (scene:theme-accent-soft theme)))))

(test cards-labels-icons-separators-and-scrims-draw-what-they-are
  (let* ((theme (scene:default-theme))
         (card (draw-ui (list (ui:make-widget :card :card 20f0 20f0 200f0 100f0))))
         (scrim (draw-ui (list (ui:make-widget :scrim :scrim 0f0 0f0 300f0 200f0))))
         (separator (draw-ui (list (ui:make-widget :separator :line 5f0 5f0 100f0 1f0))))
         (label (draw-ui (list (ui:make-widget :label :title 0f0 0f0 120f0 20f0 :label "Layers" :icon :layers
                                               :font :ui-strong :align :left)))))
    (is (drawn-with-color-p card (scene:theme-title-bar-top theme)) "the card body")
    (is (plusp (scene:draw-list-triangle-count card)))
    (is (equal (rgba-list (scene:theme-scrim theme)) (fill-color scrim)))
    (is (equal (rgba-list (scene:theme-panel-border theme)) (fill-color separator)))
    (let ((run (aref (scene:draw-list-text-runs label) 0)))
      (is (eq :ui-strong (scene:text-run-font run)))
      (is (>= (scene:text-run-x run) 22f0) "the text starts after the icon"))))

(test panel-variants-draw-their-borders
  (let* ((theme (scene:default-theme))
         (title (draw-ui (list (ui:make-widget :panel :toolbar 0f0 0f0 400f0 48f0 :variant :title-bar))))
         (status (draw-ui (list (ui:make-widget :panel :status 0f0 100f0 400f0 30f0 :variant :status-bar))))
         (plain (draw-ui (list (ui:make-widget :panel :plain 0f0 0f0 400f0 48f0)))))
    (is (drawn-with-color-p title (scene:theme-title-bar-top theme)) "the title bar is shaded")
    (is (drawn-with-color-p title (scene:theme-panel-border theme)))
    (is (drawn-with-color-p status (scene:theme-panel-border theme)))
    (is (not (drawn-with-color-p plain (scene:theme-panel-border theme))))
    (is (drawn-with-color-p plain (scene:theme-panel-background theme)))))

(test the-logo-and-the-tooltip-draw-and-the-tooltip-measures-itself
  (let* ((theme (scene:default-theme))
         (logo (draw-ui (list (ui:make-widget :logo :logo 10f0 10f0 28f0 28f0))))
         (width (ui:tooltip-width "Line" "L"))
         (tooltip (draw-ui (list (ui:make-widget :tooltip :tooltip 0f0 0f0 width ui:+tooltip-height+
                                                 :label "Line" :shortcut "L")))))
    (is (drawn-with-color-p logo (scene:theme-accent theme)))
    (is (drawn-with-color-p logo (scene:theme-accent-contrast theme)))
    (is (= 2 (length (scene:draw-list-text-runs tooltip))) "the hint and the key cap")
    (is (> (ui:tooltip-width "Line" "L") (ui:tooltip-width "Line" nil)))
    (is (> (ui:tooltip-width "Select and move" nil) (ui:tooltip-width "Line" nil)))
    (multiple-value-bind (left right) (horizontal-extent tooltip)
      (declare (ignore left))
      (is (<= right (+ width 10f0)) "everything drawn stays within the width plus the shadow"))))

(test the-overlay-sizes-itself-to-its-longest-line
  (let ((list (scene:make-draw-list)))
    (ui:draw-overlay list (scene:default-theme) '("short" "a considerably longer overlay line") 10f0 20f0)
    (is (= 2 (length (scene:draw-list-text-runs list))))
    (is (every (lambda (run) (eq :mono (scene:text-run-font run))) (scene:draw-list-text-runs list)))
    (multiple-value-bind (left right) (horizontal-extent list)
      (is (>= (- right left)
              (ui:text-width-pixels "a considerably longer overlay line" ui:+small-font-pixels+ :mono)))))
  (let ((list (scene:make-draw-list)))
    (ui:draw-overlay list (scene:default-theme) '() 0f0 0f0)
    (is (= 0 (scene:draw-list-vertex-count list)))))

;;; Icons

(test every-icon-has-shapes-and-draws-them
  (let ((names (ui:icon-names)))
    (is (> (length names) 40))
    (is (= (length names) (length (remove-duplicates names))))
    (dolist (name names)
      (is (plusp (length (ui:icon-shapes name))) "~s has shapes" name)
      (let ((list (scene:make-draw-list)))
        (ui:draw-icon list name 0f0 0f0 24f0 (scene:make-rgba 1f0 1f0 1f0 1f0))
        (is (plusp (scene:draw-list-vertex-count list)) "~s draws something" name)
        (multiple-value-bind (left right) (horizontal-extent list)
          (is (>= left -1f0) "~s stays inside its square on the left" name)
          (is (<= right 25f0) "~s stays inside its square on the right" name))))))

(test an-unknown-icon-draws-nothing
  (let ((list (scene:make-draw-list)))
    (ui:draw-icon list :no-such-icon 0f0 0f0 24f0 (scene:make-rgba 1f0 1f0 1f0 1f0))
    (is (= 0 (scene:draw-list-vertex-count list)))))

(test icons-scale-with-their-size-and-keep-a-visible-stroke
  (flet ((extent (size)
           (let ((list (scene:make-draw-list)))
             (ui:draw-icon list :line 0f0 0f0 size (scene:make-rgba 1f0 1f0 1f0 1f0))
             (multiple-value-bind (left right) (horizontal-extent list) (- right left)))))
    (is (> (extent 32f0) (* 1.8f0 (extent 16f0))) "twice the size, about twice the extent")
    (is (plusp (extent 8f0)))))
