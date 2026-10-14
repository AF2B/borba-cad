(in-package #:borba-cad/tests)

(def-suite panels
  :description "The title bar, tool palette, side panel cards, status bar, tooltips and prompts."
  :in all)

(in-suite panels)

(defconstant +minimum-window-width+ 1200
  "Width of the smallest window the program opens.")

(defconstant +minimum-window-height+ 700
  "Height of the smallest window the program opens.")

(defconstant +edge-slack+ 0.001f0
  "Tolerance when checking that a widget lies inside the window.")

(defun hover (application id)
  "Move the pointer over the center of the widget with ID."
  (let ((widget (find-widget application id)))
    (unless widget (error "No widget ~s" id))
    (send application
          (app:make-pointer-event :move
                                  nil
                                  (float (+ (ui:widget-x widget) (/ (ui:widget-width widget) 2)) 1d0)
                                  (float (+ (ui:widget-y widget) (/ (ui:widget-height widget) 2)) 1d0)))))

(defun select-only (application entity)
  "Make ENTITY the whole selection of APPLICATION."
  (clrhash (app:app-selection application))
  (setf (gethash (dom:entity-id entity) (app:app-selection application)) t))

(defun overlapping-p (first second)
  "Return true when two widgets share more than their edge."
  (and (< (ui:widget-x first) (- (+ (ui:widget-x second) (ui:widget-width second)) +edge-slack+))
       (< (ui:widget-x second) (- (+ (ui:widget-x first) (ui:widget-width first)) +edge-slack+))
       (< (ui:widget-y first) (- (+ (ui:widget-y second) (ui:widget-height second)) +edge-slack+))
       (< (ui:widget-y second) (- (+ (ui:widget-y first) (ui:widget-height first)) +edge-slack+))))

(defun outside-window-p (widget width height)
  "Return true when WIDGET reaches beyond a window of WIDTH by HEIGHT."
  (or (< (ui:widget-x widget) (- +edge-slack+))
      (< (ui:widget-y widget) (- +edge-slack+))
      (> (+ (ui:widget-x widget) (ui:widget-width widget)) (+ width +edge-slack+))
      (> (+ (ui:widget-y widget) (ui:widget-height widget)) (+ height +edge-slack+))))

(defun widgets-with-kind (application &rest kinds)
  "Return the widgets of APPLICATION whose kind is one of KINDS."
  (remove-if-not (lambda (widget) (member (ui:widget-kind widget) kinds))
                 (app:build-widgets application)))

;;; Consistency

(test the-widgets-of-every-selection-use-icons-that-exist
  (let* ((application (make-test-app :drawing (rng:generate-drawing :seed 5 :entity-count 60)))
         (known (ui:icon-names)))
    (flet ((check ()
             (dolist (widget (app:build-widgets application))
               (when (ui:widget-icon widget)
                 (is (member (ui:widget-icon widget) known)
                     "~s names the icon ~s, which does not exist" (ui:widget-id widget) (ui:widget-icon widget))))))
      (check)
      (dolist (entity (entities-of application))
        (select-only application entity)
        (check))
      (app:operation-select-all application)
      (check)
      (app:open-text-prompt application "Title" "Message" "text" #'identity)
      (check))))

(test every-widget-stays-inside-the-window-whatever-its-size
  (dolist (size (list (list +minimum-window-width+ +minimum-window-height+)
                      (list 1440 900)
                      (list 1920 1080)))
    (destructuring-bind (width height) size
      (let ((application (make-test-app :width (float width 1d0)
                                        :height (float height 1d0)
                                        :drawing (rng:generate-drawing :seed 7 :entity-count 40))))
        (app:operation-select-all application)
        (dolist (widget (app:build-widgets application))
          (is (not (outside-window-p widget width height))
              "~s is outside a ~dx~d window" (ui:widget-id widget) width height))
        (app:open-text-prompt application "Title" "Message" "text" #'identity)
        (dolist (widget (app:build-widgets application))
          (is (not (outside-window-p widget width height))
              "~s is outside a ~dx~d window with a prompt open" (ui:widget-id widget) width height))))))

(test buttons-and-fields-never-overlap-each-other
  (dolist (size (list (list +minimum-window-width+ +minimum-window-height+) (list 1440 900)))
    (destructuring-bind (width height) size
      (let ((application (make-test-app :width (float width 1d0)
                                        :height (float height 1d0)
                                        :drawing (rng:generate-drawing :seed 3 :entity-count 40))))
        (dolist (entity (cons nil (subseq (entities-of application) 0 6)))
          (if entity
              (select-only application entity)
              (clrhash (app:app-selection application)))
          (let ((controls (widgets-with-kind application :button :field)))
            (loop for (first . others) on controls
                  do (dolist (second others)
                       (is (not (overlapping-p first second))
                           "~s overlaps ~s" (ui:widget-id first) (ui:widget-id second))))))))))

(test the-status-bar-never-lets-its-parts_overlap
  (let* ((application (make-test-app :width (float +minimum-window-width+ 1d0)
                                     :height (float +minimum-window-height+ 1d0)))
         (left (find-widget application :status-left))
         (right (find-widget application :status-right))
         (chips (mapcar (lambda (id) (find-widget application id)) '(:grid :snap :object-snap :unit :extents))))
    (is (not (overlapping-p left right)))
    (dolist (chip chips)
      (is (not (overlapping-p right chip)))
      (is (not (overlapping-p left chip))))
    (is (< (+ (ui:widget-x left) (ui:widget-width left)) (ui:widget-x right)))))

;;; Title bar

(test the-title-bar-shows-the-document-and-whether-it-has-unsaved-changes
  (let ((application (make-test-app)))
    (let ((name (find-widget application :document-name)))
      (is (string= "Untitled" (ui:widget-label name)))
      (is (null (ui:widget-swatch name)) "a clean drawing has no marker"))
    (press application :l)
    (click application 0 0)
    (click application 100 0)
    (is (ui:widget-swatch (find-widget application :document-name)) "a changed drawing shows the dot")
    (setf (app:app-file-path application) #p"/tmp/plan-a.bcad")
    (is (string= "plan-a.bcad" (ui:widget-label (find-widget application :document-name))))))

(test the-document-name-is-centered-between-the-buttons-and-the-performance-toggle
  (let* ((application (make-test-app))
         (name (find-widget application :document-name))
         (redo (find-widget application :redo))
         (perf (find-widget application :perf))
         (middle (+ (ui:widget-x name) (/ (ui:widget-width name) 2)))
         (free-left (+ (ui:widget-x redo) (ui:widget-width redo)))
         (free-right (ui:widget-x perf)))
    (is (< (abs (- middle (/ (+ free-left free-right) 2))) 1f0))))

(test file-and-history-buttons-are-icons-with-hints-and-shortcuts
  (let ((application (make-test-app)))
    (dolist (id '(:new :open :save :save-as :undo :redo :perf))
      (let ((widget (find-widget application id)))
        (is-true widget)
        (is (ui:widget-icon widget))
        (is (null (ui:widget-label widget)))
        (is (ui:widget-hint widget))
        (is (ui:widget-shortcut widget))))
    (is (string= "Ctrl+Shift+S" (ui:widget-shortcut (find-widget application :save-as))))))

;;; Tool palette

(test the-palette-lists-the-tools-top-to-bottom-inside-its-column
  (let* ((application (make-test-app))
         (layout (ui:compute-layout 1440 900))
         (palette (ui:layout-tool-palette layout))
         (tools (mapcar (lambda (id) (find-widget application id))
                        '(:select :line :rectangle :circle :polyline :text :dimension))))
    (is (every (lambda (widget) (eq :tool (ui:widget-variant widget))) tools))
    (is (apply #'< (mapcar #'ui:widget-y tools)))
    (dolist (widget tools)
      (is (ui:rect-contains-p palette (ui:widget-x widget) (ui:widget-y widget)))
      (is (ui:rect-contains-p palette
                              (+ (ui:widget-x widget) (ui:widget-width widget))
                              (+ (ui:widget-y widget) (ui:widget-height widget)))))
    (is (ui:widget-active (find-widget application :select)))
    (click-widget application :circle)
    (is (ui:widget-active (find-widget application :circle)))
    (is (not (ui:widget-active (find-widget application :select))))))

;;; Tooltips

(test hovering-an-icon-button-shows-a-tooltip-with-its-hint-and-shortcut
  (let ((application (make-test-app)))
    (is (null (find-widget application :tooltip)) "no tooltip without hover")
    (hover application :line)
    (let ((tooltip (find-widget application :tooltip))
          (button (find-widget application :line)))
      (is-true tooltip)
      (is (string= "Line" (ui:widget-label tooltip)))
      (is (string= "L" (ui:widget-shortcut tooltip)))
      (is (>= (ui:widget-x tooltip) (+ (ui:widget-x button) (ui:widget-width button))) "to the right of a tool")
      (is (< (abs (- (+ (ui:widget-y tooltip) (/ (ui:widget-height tooltip) 2))
                     (+ (ui:widget-y button) (/ (ui:widget-height button) 2))))
             1f0)))
    (hover application :save)
    (let ((tooltip (find-widget application :tooltip))
          (button (find-widget application :save)))
      (is (string= "Save" (ui:widget-label tooltip)))
      (is (>= (ui:widget-y tooltip) (+ (ui:widget-y button) (ui:widget-height button))) "under a title bar button"))))

(test a-tooltip-never-leaves-the-window
  (let ((application (make-test-app :width (float +minimum-window-width+ 1d0)
                                    :height (float +minimum-window-height+ 1d0))))
    (dolist (id '(:perf :new :select :dimension))
      (hover application id)
      (let ((tooltip (find-widget application :tooltip)))
        (is-true tooltip)
        (is (not (outside-window-p tooltip +minimum-window-width+ +minimum-window-height+)))))))

(test chips-and-text-buttons-show-no-tooltip-but-their-hint-reaches-the-status-bar
  (let ((application (make-test-app)))
    (hover application :grid)
    (is (null (find-widget application :tooltip)))
    (app:app-render application)
    (is (some (lambda (text) (search "Show the grid (G)" text)) (all-text application)))
    (hover application :layer-rename)
    (is (null (find-widget application :tooltip)))
    (app:app-render application)
    (is (some (lambda (text) (search "Rename the current layer" text)) (all-text application)))))

(test no-tooltip-appears-while-a-prompt-is-open
  (let ((application (make-test-app)))
    (hover application :line)
    (is (find-widget application :tooltip))
    (app:open-text-prompt application "Title" "Message" "" #'identity)
    (is (null (find-widget application :tooltip)))))

;;; Layers

(defun layer-row-widgets (application)
  "Return the layer rows of the current widget list."
  (remove-if-not (lambda (widget) (and (consp (ui:widget-id widget)) (eq :layer (first (ui:widget-id widget)))))
                 (app:build-widgets application)))

(test the-layers-card-lists-every-layer-with-its-buttons-and-a-count
  (let ((application (make-test-app)))
    (app:operation-add-layer application "walls")
    (app:operation-add-layer application "doors")
    (is (= 3 (length (layer-row-widgets application))))
    (is (string= "3" (ui:widget-label (find-widget application '(:layers-card :detail)))))
    (is (= 1 (count-if #'ui:widget-active (layer-row-widgets application))) "only the current layer is highlighted")
    (dolist (row (layer-row-widgets application))
      (let ((id (second (ui:widget-id row))))
        (is (find-widget application (list :layer-visible id)))
        (is (find-widget application (list :layer-locked id)))
        (is (plusp (ui:widget-trailing row)) "the row keeps clear of the buttons over it")))
    (is (null (find-widget application :layers-scrollbar)) "everything fits")))

(test the-visibility-and-lock-buttons-show-their-state-in-the-icon
  (let ((application (make-test-app)))
    (is (eq :eye (ui:widget-icon (find-widget application '(:layer-visible 0)))))
    (is (eq :unlock (ui:widget-icon (find-widget application '(:layer-locked 0)))))
    (click-widget application '(:layer-visible 0))
    (click-widget application '(:layer-locked 0))
    (is (eq :eye-off (ui:widget-icon (find-widget application '(:layer-visible 0)))))
    (is (eq :lock (ui:widget-icon (find-widget application '(:layer-locked 0)))))))

(test many-layers-scroll-inside-the-card-and-show-a-scrollbar
  (let ((application (make-test-app :height 700d0)))
    (dotimes (index 30)
      (app:operation-add-layer application (format nil "layer ~d" index)))
    (let* ((layout (ui:compute-layout 1440 700))
           (panel (ui:layout-layers-panel layout))
           (rows (layer-row-widgets application)))
      (is (< (length rows) 31))
      (is (plusp (length rows)))
      (is (find-widget application :layers-scrollbar))
      (dolist (row rows)
        (is (<= (+ (ui:widget-y row) (ui:widget-height row))
                (+ (ui:rect-y panel) (ui:rect-height panel)))))
      (let ((first-before (ui:widget-id (first rows)))
            (scroll-before (app:app-layer-scroll application)))
        ;; The new layer, the last, is in view, so the list starts at the bottom; the wheel goes up.
        (send application (app:make-wheel-event 3d0
                                                (float (+ (ui:rect-x panel) 100) 1d0)
                                                (float (+ (ui:rect-y panel) 100) 1d0)))
        (is (= (- scroll-before 3) (app:app-layer-scroll application)))
        (is (not (equal first-before (ui:widget-id (first (layer-row-widgets application))))))
        (send application (app:make-wheel-event -10d0
                                                (float (+ (ui:rect-x panel) 100) 1d0)
                                                (float (+ (ui:rect-y panel) 100) 1d0)))
        (layer-row-widgets application)                         ; building the card clamps the scroll
        (is (= scroll-before (app:app-layer-scroll application)) "and stops at the bottom")))))

(test a-layer-added-or-chosen-out-of-sight-is-scrolled-into-view-and-hand-scrolling-is-left-alone
  (let ((application (make-test-app :height 700d0)))
    (dotimes (index 30)
      (app:operation-add-layer application (format nil "layer ~d" index)))
    (let ((rows (layer-row-widgets application)))
      (is (find-if #'ui:widget-active rows) "the layer just added, the last one, is in view and current")
      (is (plusp (app:app-layer-scroll application))))
    ;; Scrolling to the top by hand is respected as long as the current layer does not change.
    (send application (app:make-wheel-event 40d0 1300d0 150d0))
    (is (= 0 (app:app-layer-scroll application)))
    (is (null (find-if #'ui:widget-active (layer-row-widgets application))) "the current layer is out of view")
    (app:app-render application)
    (is (= 0 (app:app-layer-scroll application)) "rebuilding does not undo the scroll")
    ;; Choosing another layer out of sight brings it into view.
    (app:operation-set-current-layer application 29)
    (is (find-if #'ui:widget-active (layer-row-widgets application)))
    (app:operation-set-current-layer application 0)
    (is (find-if #'ui:widget-active (layer-row-widgets application)))
    (is (= 0 (app:app-layer-scroll application)))))

(test an-empty-drawing-tells-a-new-user-how-to-begin
  (let ((application (make-test-app)))
    (let ((title (find-widget application :empty-hint-title)))
      (is-true title)
      (is (string= "Start drawing" (ui:widget-label title)))
      (let* ((canvas (canvas-of application))
             (middle (+ (ui:widget-x title) (/ (ui:widget-width title) 2))))
        (is (< (abs (- middle (+ (ui:rect-x canvas) (/ (ui:rect-width canvas) 2)))) 1f0) "centered on the canvas")
        (is (< (abs (- (+ (ui:widget-y title) (ui:widget-height title))
                       (+ (ui:rect-y canvas) (/ (ui:rect-height canvas) 2))))
               (ui:rect-height canvas)))))
    (is-true (find-widget application '(:empty-hint-line 0)))
    (app:app-render application)
    (is (some (lambda (text) (search "press L for a line" text)) (all-text application)))))

(test the-hint-goes-away-with-the-first-entity-while-a-tool-works-and-under-a-dialog
  (let ((application (make-test-app)))
    (is (find-widget application :empty-hint-title))
    (press application :l)
    (click application 0 0)                                       ; a line has begun, nothing is drawn yet
    (is (null (find-widget application :empty-hint-title)))
    (click application 1000 0)
    (press application :escape)
    (is (null (find-widget application :empty-hint-title)) "there is a line now")
    (app:operation-undo application)
    (is (find-widget application :empty-hint-title) "undone, the drawing is empty again")
    (app:operation-open application)
    (is (null (find-widget application :empty-hint-title)) "a dialog covers the canvas")
    (app:close-file-dialog application)
    (app:open-text-prompt application "Title" "Message" "" #'identity)
    (is (null (find-widget application :empty-hint-title)))))

(defun widget-kind-name (widget)
  "Return the first part of the id of WIDGET: the id itself, or the head of a list id."
  (let ((id (ui:widget-id widget)))
    (if (consp id) (first id) id)))

(test the-hint-never-leaves-the-canvas
  (let* ((application (make-test-app :width (float +minimum-window-width+ 1d0)
                                     :height (float +minimum-window-height+ 1d0)))
         (canvas (canvas-of application))
         (left (ui:rect-x canvas))
         (top (ui:rect-y canvas))
         (right (+ left (ui:rect-width canvas) 0.01f0))
         (bottom (+ top (ui:rect-height canvas) 0.01f0)))
    (dolist (widget (remove-if-not (lambda (widget)
                                     (member (widget-kind-name widget)
                                             '(:empty-hint-card :empty-hint-title :empty-hint-line)))
                                   (app:build-widgets application)))
      (is (>= (ui:widget-x widget) left))
      (is (<= (+ (ui:widget-x widget) (ui:widget-width widget)) right))
      (is (>= (ui:widget-y widget) top))
      (is (<= (+ (ui:widget-y widget) (ui:widget-height widget)) bottom)))))

(test a-very-long-layer-name-is-cut-to-the-row
  (let ((application (make-test-app)))
    (app:operation-add-layer application (make-string 60 :initial-element #\w))
    (app:app-render application)
    (let ((row (find-widget application (list :layer 1))))
      (is (<= (ui:text-width-pixels (ui:widget-label row))
              (+ (ui:widget-width row) 200f0))
          "the label is cut when drawn, not in the widget")
      (is (every (lambda (run)
                   (<= (+ (scene:text-run-x run) (ui:text-width-pixels (scene:text-run-string run)))
                       1441f0))
                 (scene:draw-list-text-runs (app:app-draw-list application)))))))

;;; Inspector

(test the-properties-card-stays-inside-its-bounds-and-scrolls-when-it-must
  (let* ((application (make-test-app :height (float +minimum-window-height+ 1d0)
                                     :width (float +minimum-window-width+ 1d0)
                                     :drawing (rng:generate-drawing :seed 11 :entity-count 20)))
         (layout (ui:compute-layout +minimum-window-width+ +minimum-window-height+))
         (panel (ui:layout-inspector-panel layout))
         (bottom (+ (ui:rect-y panel) (ui:rect-height panel))))
    (dolist (entity (entities-of application))
      (select-only application entity)
      (dolist (widget (app:build-widgets application))
        (when (and (consp (ui:widget-id widget)) (eq :property (first (ui:widget-id widget))))
          (is (<= (+ (ui:widget-y widget) (ui:widget-height widget)) bottom)
              "~s ends below the properties card" (ui:widget-id widget)))))))

(test the-properties-card-names-what-it-shows
  (let ((application (two-lines-app)))
    (is (string= "Drawing" (ui:widget-label (find-widget application '(:inspector-card :detail)))))
    (app:operation-select-all application)
    (is (string= "Selection" (ui:widget-label (find-widget application '(:inspector-card :detail)))))
    (select-only application (first (entities-of application)))
    (is (string= "Entity" (ui:widget-label (find-widget application '(:inspector-card :detail)))))
    (is (eq :line (ui:widget-icon (find-widget application :entity-title))))))

;;; Prompts

(test the-prompt-card-grows-with-a-long-message-and-keeps-everything-inside
  (let* ((application (make-test-app :width (float +minimum-window-width+ 1d0)
                                     :height (float +minimum-window-height+ 1d0)))
         (message (format nil "~{~a~^ ~}" (loop repeat 40 collect "overwrite"))))
    (app:open-text-prompt application "Title" "Short" "" #'identity)
    (let ((short-height (ui:widget-height (find-widget application :prompt))))
      (app:close-prompt application)
      (app:open-text-prompt application "Title" message "" #'identity)
      (let ((card (find-widget application :prompt)))
        (is (> (ui:widget-height card) short-height))
        (is (find-widget application '(:prompt-message 1)) "the message wraps")
        (is (find-widget application '(:prompt-message 2)))
        (is (null (find-widget application '(:prompt-message 3))) "and stops at three lines")
        (dolist (widget (app:build-widgets application))
          (when (member (ui:widget-id widget) '(:prompt-title :prompt-message :prompt-field))
            (is (>= (ui:widget-x widget) (ui:widget-x card)))
            (is (<= (+ (ui:widget-x widget) (ui:widget-width widget))
                    (+ (ui:widget-x card) (ui:widget-width card) +edge-slack+)))
            (is (<= (+ (ui:widget-y widget) (ui:widget-height widget))
                    (+ (ui:widget-y card) (ui:widget-height card)))))
          (when (and (consp (ui:widget-id widget)) (eq :prompt-button (first (ui:widget-id widget))))
            (is (<= (+ (ui:widget-y widget) (ui:widget-height widget))
                    (+ (ui:widget-y card) (ui:widget-height card)))
                "the buttons stay on the card")))))))

(test the-first-prompt-button-is-the-primary-action
  (let ((application (make-test-app)))
    (app:open-choice-prompt application "Discard?" "Unsaved changes."
                            (list (cons "Save" nil) (cons "Discard" nil) (cons "Cancel" nil)))
    (is (eq :primary (ui:widget-variant (find-widget application '(:prompt-button 0)))))
    (is (eq :default (ui:widget-variant (find-widget application '(:prompt-button 1)))))
    (is (eq :default (ui:widget-variant (find-widget application '(:prompt-button 2)))))
    ;; The buttons are right-aligned in order.
    (is (< (ui:widget-x (find-widget application '(:prompt-button 0)))
           (ui:widget-x (find-widget application '(:prompt-button 1)))
           (ui:widget-x (find-widget application '(:prompt-button 2)))))
    (is (find-widget application :scrim))))

(test a-prompt-veils-the-whole-window-and-blocks-the-widgets-behind-it
  (let ((application (make-test-app)))
    (app:open-text-prompt application "Title" "Message" "" #'identity)
    (let ((scrim (find-widget application :scrim)))
      (is (= 0f0 (ui:widget-x scrim)))
      (is (= 1440f0 (ui:widget-width scrim)))
      (is (= 900f0 (ui:widget-height scrim))))
    (dolist (id '(:line :save :grid :layer-add))
      (is (ui:widget-blocked (find-widget application id)) "~s is blocked" id))))
