(in-package #:borba-cad/app)

;;;; Panels: the widget lists of the title bar, the tool palette, the status bar and the tooltips,
;;;; derived from the application state. Because the list is rebuilt from state, drawing and hit
;;;; testing always agree.

(defconstant +title-margin+ 14f0
  "Space between the window edge and the first element of the title bar, in pixels.")

(defconstant +logo-size+ 28f0
  "Side of the brand mark in the title bar, in pixels.")

(defconstant +title-button-gap+ 4f0
  "Gap between the icon buttons of the title bar, in pixels.")

(defconstant +title-group-gap+ 14f0
  "Space around a separator of the title bar, in pixels.")

(defconstant +palette-button-size+ 44f0
  "Side of a tool button in the palette, in pixels.")

(defconstant +palette-button-gap+ 4f0
  "Gap between tool buttons in the palette, in pixels.")

(defconstant +tooltip-distance+ 10f0
  "Gap between a button and its tooltip, in pixels.")

(defconstant +tooltip-edge-margin+ 6f0
  "Closest a tooltip comes to the window edge, in pixels.")

(defconstant +status-padding+ 14f0
  "Space between the window edge and the status bar's content, in pixels.")

(defconstant +chip-gap+ 6f0
  "Gap between the chips of the status bar, in pixels.")

(defconstant +dirty-marker-width+ 14f0
  "Room the unsaved-changes dot takes before the document name, in pixels.")

(defconstant +logo-gap+ 10f0
  "Space between the brand mark and the program's name, in pixels.")

(defconstant +wordmark-slack+ 4f0
  "Room left after the program's name in the title bar so that it is never cut, in pixels.")

(defconstant +title-separator-inset+ 6f0
  "Space above and below a separator of the title bar, within the height of its buttons.")

(defconstant +separator-width+ 1f0
  "Width of a vertical separator, in pixels.")

(defconstant +palette-separator-inset+ 8f0
  "Space between the edge of a palette button and the end of the separator between groups.")

(defconstant +palette-separator-drop+ 2f0
  "How far below the last button of a group the separator that ends it sits, in pixels.")

(defconstant +palette-group-gap+ 10f0
  "Extra space between two groups of tools in the palette, in pixels.")

(defconstant +chip-icon-room+ 22f0
  "Room a chip needs beside its label for an icon, in pixels.")

(defconstant +chip-dot-room+ 14f0
  "Room a chip needs beside its label for its on/off dot, in pixels.")

(defconstant +tooltip-lift+ 4f0
  "How far a tooltip under a title bar button tucks up toward the button, in pixels.")

(defconstant +empty-hint-title-size+ 22f0
  "Em size of the heading of the hint shown over an empty drawing, in pixels.")

(defconstant +empty-hint-line-height+ 24f0
  "Height of a line of the hint shown over an empty drawing, in pixels.")

(defconstant +empty-hint-gap+ 10f0
  "Space between the heading of the empty-drawing hint and its lines, in pixels.")

(defconstant +empty-hint-padding-x+ 36f0
  "Space between the edge of the empty-drawing hint's card and its text, left and right, in pixels.")

(defconstant +empty-hint-padding-y+ 26f0
  "Space between the edge of the empty-drawing hint's card and its text, above and below, in pixels.")

(defparameter *empty-hint-lines*
  (list (i18n:mark "Pick a tool on the left, or press L for a line, R for a rectangle, C for a circle.")
        (i18n:mark "Ctrl+O opens a drawing, and a .bcad file dropped on the window opens too."))
  "The lines of the hint shown over an empty drawing, under its heading.")

(defconstant +caret-size+ 10f0
  "Side of the small arrow that tells a button it opens a menu, in pixels.")

(defconstant +caret-margin+ 3f0
  "Space between the arrow of the draw button and the edge of the button, in pixels.")

(defun focused-p (app
                  id)
  "Return true when the field ID has the keyboard focus.

APP: the application.
ID: field id."
  (and (app-focus app) (equal (app-focus app) id)))

(defun start-field-edit (app
                         id
                         text
                         commit)
  "Give the keyboard focus to a field, with its text selected.

APP: the application.
ID: field id.
TEXT: the text the field shows.
COMMIT: function of the typed string, called when the edit is committed."
  (setf (app-focus app) id
        (app-edit app) (ui:edit-select-all-replace (ui:make-edit-state :text text))
        (app-field-commit app) commit
        (app-field-original app) text)
  (app-request-redraw app))

(defun stop-field-edit (app)
  "Remove the keyboard focus from the focused field without committing.

APP: the application."
  (setf (app-focus app) nil
        (app-edit app) nil
        (app-field-commit app) nil)
  (app-request-redraw app))

(defun commit-field-edit (app)
  "Commit the focused field: the commit function runs unless the text is unchanged.

APP: the application."
  (let ((commit (app-field-commit app))
        (text (and (app-edit app) (ui:edit-state-text (app-edit app))))
        (original (app-field-original app)))
    (stop-field-edit app)
    (when (and commit text (string/= text original))
      (funcall commit text))))

;;; Shared widget constructors

(defun make-label (id
                   text
                   x
                   y
                   width
                   height
                   &key (align :left)
                        color
                        (font :ui)
                        (size ui:+font-pixels+)
                        icon
                        swatch
                        (trailing 0f0))
  "Create a text label.

ID: widget id.
TEXT: what it says.
X: left edge.
Y: top edge.
WIDTH: width.
HEIGHT: height.
ALIGN: :left, :center or :right.
COLOR: text color override, an RGBA.
FONT: :ui, :ui-strong or :mono.
SIZE: em size in pixels.
ICON: name of an icon drawn before the text.
SWATCH: an RGBA drawn as a dot before the text.
TRAILING: pixels at the right the text must stay clear of."
  (ui:make-widget :label
                  id
                  x
                  y
                  width
                  height
                  :label text
                  :align align
                  :color color
                  :font font
                  :size size
                  :icon icon
                  :swatch swatch
                  :trailing trailing))

(defun make-icon-button (id
                         icon
                         x
                         y
                         &key action
                              active
                              (enabled t)
                              blocked
                              hint
                              shortcut
                              (variant :icon)
                              (size ui:+icon-button-size+)
                              color)
  "Create a square button that shows only an icon.

ID: widget id.
ICON: icon name.
X: left edge.
Y: top edge.
ACTION: function called on click.
ACTIVE: toggled-on state.
ENABLED: false to dim and disable.
BLOCKED: true while a modal covers it.
HINT: what it does, shown as a tooltip and in the status bar.
SHORTCUT: the keys that do the same.
VARIANT: :icon, :tool or :ghost.
SIZE: side in pixels.
COLOR: icon color override, an RGBA."
  (ui:make-widget :button
                  id
                  x
                  y
                  size
                  size
                  :icon icon
                  :action action
                  :active active
                  :enabled enabled
                  :blocked blocked
                  :hint hint
                  :shortcut shortcut
                  :variant variant
                  :color color))

(defun make-separator (id
                       x
                       y
                       width
                       height)
  "Create a hairline.

ID: widget id.
X: left edge.
Y: top edge.
WIDTH: width; 1 for a vertical line.
HEIGHT: height; 1 for a horizontal line."
  (ui:make-widget :separator
                  id
                  x
                  y
                  width
                  height))

;;; Title bar

(defun title-button-specs (app)
  "Return the icon buttons of the title bar as (id icon action hint shortcut enabled), with NIL
marking a separator.

APP: the application."
  (list (list :new :new (lambda () (operation-new app)) (i18n:tr "New drawing") "Ctrl+N" t)
        (list :open :open (lambda () (operation-open app)) (i18n:tr "Open a drawing") "Ctrl+O" t)
        (list :save :save (lambda () (operation-save app)) (i18n:tr "Save") "Ctrl+S" t)
        (list :save-as :save-as (lambda () (operation-save-as app)) (i18n:tr "Save as...") "Ctrl+Shift+S" t)
        nil
        (list :undo :undo (lambda () (operation-undo app)) (i18n:tr "Undo") "Ctrl+Z"
              (dom:history-can-undo-p (app-history app)))
        (list :redo :redo (lambda () (operation-redo app)) (i18n:tr "Redo") "Ctrl+Y"
              (dom:history-can-redo-p (app-history app)))))

(defun document-name (app)
  "Return the name shown for the drawing in the title bar.

APP: the application."
  (let ((path (app-file-path app)))
    (if path
        (file-namestring path)
        (i18n:tr "Untitled"))))

(defun document-widget (app
                        left
                        right)
  "Build the document name, centered between LEFT and RIGHT, with a dot when there are unsaved
changes.

APP: the application.
LEFT: x where the free space of the title bar begins.
RIGHT: x where it ends."
  (let* ((theme (app-theme app))
         (dirty (app-dirty-p app))
         (available (max 0f0 (- right left)))
         (name (document-name app))
         (inset (if dirty +dirty-marker-width+ 0f0))
         (shown (ui:fit-text name (max 0f0 (- available inset))))
         (width (+ inset (ui:text-width-pixels shown))))
    (make-label :document-name
                shown
                (+ left (/ (- available width) 2))
                0f0
                width
                ui:+toolbar-height+
                :color (scene:theme-text theme)
                :swatch (and dirty (scene:theme-accent theme)))))

(defun toolbar-widgets (app
                        blocked)
  "Build the title bar: brand mark, file and history buttons, the document name, and the
buttons for the other file formats, the language and the performance overlay.

APP: the application.
BLOCKED: true while a modal covers the title bar."
  (let* ((theme (app-theme app))
         (width (float (app-window-width app) 1f0))
         (button-y (/ (- ui:+toolbar-height+ ui:+icon-button-size+) 2))
         (wordmark-x (+ +title-margin+ +logo-size+ +logo-gap+))
         (wordmark-width (+ (ui:text-width-pixels "Borba CAD"
                                                  ui:+heading-font-pixels+
                                                  :ui-strong)
                            +wordmark-slack+))
         (x (+ wordmark-x wordmark-width +title-group-gap+))
         ;; Widgets are pushed in reverse and reversed once at the end, so the initial list is
         ;; written back to front.
         (widgets (reverse (list (ui:make-widget :panel
                                                 :toolbar
                                                 0f0
                                                 0f0
                                                 width
                                                 ui:+toolbar-height+
                                                 :variant :title-bar)
                                 (ui:make-widget :logo
                                                 :logo
                                                 +title-margin+
                                                 (/ (- ui:+toolbar-height+ +logo-size+) 2)
                                                 +logo-size+
                                                 +logo-size+)
                                 (make-label :wordmark
                                             "Borba CAD"
                                             wordmark-x
                                             0f0
                                             wordmark-width
                                             ui:+toolbar-height+
                                             :font :ui-strong
                                             :size ui:+heading-font-pixels+
                                             :color (scene:theme-text theme))
                                 (make-separator :separator-brand
                                                 x
                                                 (+ button-y +title-separator-inset+)
                                                 +separator-width+
                                                 (- ui:+icon-button-size+ (* 2 +title-separator-inset+)))))))
    (incf x (+ +title-group-gap+ +separator-width+))
    (dolist (spec (title-button-specs app))
      (if (null spec)
          (progn
            (incf x (- +title-group-gap+ +title-button-gap+))
            (push (make-separator :separator-files
                                  x
                                  (+ button-y +title-separator-inset+)
                                  +separator-width+
                                  (- ui:+icon-button-size+ (* 2 +title-separator-inset+)))
                  widgets)
            (incf x (+ +title-group-gap+ +separator-width+)))
          (destructuring-bind (id icon action hint shortcut enabled) spec
            (push (make-icon-button id
                                    icon
                                    x
                                    button-y
                                    :action action
                                    :enabled enabled
                                    :blocked blocked
                                    :hint hint
                                    :shortcut shortcut)
                  widgets)
            (incf x (+ ui:+icon-button-size+ +title-button-gap+)))))
    (let* ((perf-x (- width +title-margin+ ui:+icon-button-size+))
           (language-x (- perf-x +title-button-gap+ ui:+icon-button-size+))
           (more-x (- language-x +title-button-gap+ ui:+icon-button-size+)))
      (push (make-icon-button :more
                              :more
                              more-x
                              button-y
                              :action (lambda ()
                                        (toggle-menu app
                                                     :more
                                                     :more))
                              :active (and (app-menu app) (eq :more (menu-kind (app-menu app))))
                              :blocked blocked
                              :hint (i18n:tr "More"))
            widgets)
      (push (make-icon-button :language
                              :globe
                              language-x
                              button-y
                              :action (lambda ()
                                        (toggle-menu app
                                                     :language
                                                     :language))
                              :active (and (app-menu app) (eq :language (menu-kind (app-menu app))))
                              :blocked blocked
                              :hint (i18n:tr "Language"))
            widgets)
      (push (make-icon-button :perf
                              :perf
                              perf-x
                              button-y
                              :action (lambda () (operation-toggle-overlay app))
                              :active (app-overlay-visible app)
                              :blocked blocked
                              :hint (i18n:tr "Performance overlay")
                              :shortcut "F3")
            widgets)
      (push (document-widget app
                             (+ (- x +title-button-gap+) +title-group-gap+)
                             (- more-x +title-group-gap+))
            widgets))
    (nreverse widgets)))

;;; Tool palette

(defun empty-hint-widgets (app
                           canvas)
  "Build the hint that tells a new user how to begin, centered over an empty drawing. It is
gone as soon as there is something drawn, a tool is at work, or a dialog or a question is open.

APP: the application.
CANVAS: the canvas rectangle."
  (when (and (zerop (dom:drawing-entity-count (app-drawing app)))
             (not (tool-in-progress-p (app-tool app)))
             (null (app-prompt app))
             (null (app-dialog app)))
    (let* ((theme (app-theme app))
           (x (ui:rect-x canvas))
           (width (ui:rect-width canvas))
           (lines (mapcar #'i18n:tr *empty-hint-lines*))
           (block-height (+ +empty-hint-title-size+
                            +empty-hint-gap+
                            (* (length lines) +empty-hint-line-height+)))
           (block-width (reduce #'max
                                (cons (ui:text-width-pixels (i18n:tr "Start drawing")
                                                            +empty-hint-title-size+
                                                            :ui-strong)
                                      (mapcar #'ui:text-width-pixels lines))))
           (card-width (min (ui:rect-width canvas) (+ block-width (* 2 +empty-hint-padding-x+))))
           (top (+ (ui:rect-y canvas) (/ (- (ui:rect-height canvas) block-height) 2))))
      (list* (ui:make-widget :card
                             :empty-hint-card
                             (+ x (/ (- width card-width) 2))
                             (- top +empty-hint-padding-y+)
                             card-width
                             (+ block-height (* 2 +empty-hint-padding-y+)))
             (make-label :empty-hint-title
                        (i18n:tr "Start drawing")
                        x
                        top
                        width
                        +empty-hint-title-size+
                        :align :center
                        :font :ui-strong
                        :size +empty-hint-title-size+
                        :color (scene:theme-text-dim theme))
            (loop for line in lines
                  for index from 0
                  collect (make-label (list :empty-hint-line index)
                                      line
                                      x
                                      (+ top +empty-hint-title-size+ +empty-hint-gap+
                                         (* index +empty-hint-line-height+))
                                      width
                                      +empty-hint-line-height+
                                      :align :center
                                      :color (scene:theme-text-faint theme)))))))

(defun draw-button-icon (app)
  "Return the icon of the draw button: the icon of the drawing tool in use, or a pencil while the
select tool is.

APP: the application."
  (let ((kind (tool-kind (app-tool app))))
    (if (or (eq kind :select) (modify-tool-kind-p kind) (measure-tool-kind-p kind))
        :pencil
        kind)))

(defun palette-widgets (app
                        rect
                        blocked)
  "Build the column of tools along the left edge: the select tool, and the button that opens the
list of drawing tools.

APP: the application.
RECT: the palette rectangle.
BLOCKED: true while a modal covers the palette."
  (let* ((theme (app-theme app))
         (selecting (eq (tool-kind (app-tool app)) :select))
         (button-x (/ (- (ui:rect-width rect) +palette-button-size+) 2))
         (select-y (+ (ui:rect-y rect) ui:+padding+))
         (separator-y (+ select-y +palette-button-size+ +palette-button-gap+ +palette-separator-drop+))
         (draw-y (+ select-y +palette-button-size+ +palette-button-gap+ +palette-group-gap+))
         (modify-y (+ draw-y +palette-button-size+ +palette-button-gap+))
         (modifying (modify-tool-kind-p (tool-kind (app-tool app))))
         (measure-y (+ modify-y +palette-button-size+ +palette-button-gap+))
         (measuring (measure-tool-kind-p (tool-kind (app-tool app)))))
    (list (ui:make-widget :panel
                          :palette
                          (ui:rect-x rect)
                          (ui:rect-y rect)
                          (ui:rect-width rect)
                          (ui:rect-height rect)
                          :variant :palette)
          (make-icon-button :select
                            :select
                            button-x
                            select-y
                            :action (lambda () (operation-set-tool app :select))
                            :active selecting
                            :blocked blocked
                            :hint (i18n:tr "Select and move")
                            :shortcut "Esc"
                            :variant :tool
                            :size +palette-button-size+)
          (make-separator :palette-separator
                          (+ button-x +palette-separator-inset+)
                          separator-y
                          (- +palette-button-size+ (* 2 +palette-separator-inset+))
                          +separator-width+)
          (make-icon-button :draw
                            (draw-button-icon app)
                            button-x
                            draw-y
                            :action (lambda ()
                                      (toggle-menu app
                                                   :draw
                                                   :draw))
                            :active (not (or selecting modifying measuring))
                            :blocked blocked
                            :hint (i18n:tr "Drawing tools")
                            :variant :tool
                            :size +palette-button-size+)
          (ui:make-widget :icon
                          :draw-caret
                          (- (+ button-x +palette-button-size+) +caret-size+ +caret-margin+)
                          (+ draw-y (/ (- +palette-button-size+ +caret-size+) 2))
                          +caret-size+
                          +caret-size+
                          :icon :caret
                          :color (scene:theme-text-faint theme))
          (make-icon-button :modify
                            (if modifying (tool-kind (app-tool app)) :rotate)
                            button-x
                            modify-y
                            :action (lambda ()
                                      (toggle-menu app
                                                   :modify
                                                   :modify))
                            :active modifying
                            :blocked blocked
                            :hint (i18n:tr "Modify: rotate, mirror, scale, trim, extend, offset, fillet")
                            :variant :tool
                            :size +palette-button-size+)
          (ui:make-widget :icon
                          :modify-caret
                          (- (+ button-x +palette-button-size+) +caret-size+ +caret-margin+)
                          (+ modify-y (/ (- +palette-button-size+ +caret-size+) 2))
                          +caret-size+
                          +caret-size+
                          :icon :caret
                          :color (scene:theme-text-faint theme))
          (make-icon-button :measure
                            (if measuring
                                (cdr (assoc (tool-kind (app-tool app)) *measure-button-icons*))
                                :ruler)
                            button-x
                            measure-y
                            :action (lambda ()
                                      (toggle-menu app
                                                   :measure
                                                   :measure))
                            :active measuring
                            :blocked blocked
                            :hint (i18n:tr "Measure: distance, angle, area")
                            :variant :tool
                            :size +palette-button-size+)
          (ui:make-widget :icon
                          :measure-caret
                          (- (+ button-x +palette-button-size+) +caret-size+ +caret-margin+)
                          (+ measure-y (/ (- +palette-button-size+ +caret-size+) 2))
                          +caret-size+
                          +caret-size+
                          :icon :caret
                          :color (scene:theme-text-faint theme)))))

;;; Status bar

(defparameter *snap-kind-labels*
  (list (cons :endpoint (i18n:mark "endpoint"))
        (cons :center (i18n:mark "center"))
        (cons :midpoint (i18n:mark "midpoint"))
        (cons :intersection (i18n:mark "intersection"))
        (cons :quadrant (i18n:mark "quadrant"))
        (cons :perpendicular (i18n:mark "perpendicular"))
        (cons :tangent (i18n:mark "tangent"))
        (cons :grid (i18n:mark "grid"))
        (cons :ortho (i18n:mark "ortho"))
        (cons :polar (i18n:mark "polar")))
  "How the status bar names what the pointer snapped to, in English; it is translated where shown.")

(defun snap-kind-label (kind)
  "Return the name of a kind of snap in the current language, in lower case.

KIND: :endpoint, :center or :grid."
  (let ((label (cdr (assoc kind *snap-kind-labels*))))
    (if label
        (i18n:tr label)
        (string-downcase (symbol-name kind)))))

(defun cursor-text (app)
  "Return the pointer position and zoom as status text.

APP: the application."
  (let* ((cursor (app-cursor app))
         (unit (dom:drawing-display-unit (app-drawing app)))
         (zoom (format nil "~a px/~a"
                       (base:format-decimal (* (view:viewport-scale (app-viewport app))
                                               (base:unit->millimeters 1d0 unit))
                                            2)
                       (base:unit-abbreviation unit))))
    (if cursor
        (i18n:trf "X ~a  Y ~a ~a | ~a~@[ | snap: ~a~]"
                (base:format-length (cursor-x cursor)
                                    unit
                                    :precision 3)
                (base:format-length (cursor-y cursor)
                                    unit
                                    :precision 3)
                (base:unit-abbreviation unit)
                zoom
                (and (cursor-snap-kind cursor) (snap-kind-label (cursor-snap-kind cursor))))
        (format nil "~a" zoom))))

(defun app-status-texts (app
                         hover-hint)
  "Return the status bar content as three values: the left text, its severity (:info or :error)
and the right text.

APP: the application.
HOVER-HINT: hint of the widget under the pointer, or NIL."
  (let ((message (app-message app)))
    (values (cond (message (car message))
                  (hover-hint hover-hint)
                  (t (tool-prompt (app-tool app) app)))
            (if message (cdr message) :info)
            (cursor-text app))))

(defun status-chip-specs (app)
  "Return the chips of the status bar as (id label icon action hint shortcut active), from left
to right. A chip without an icon shows a dot that lights up when it is on.

APP: the application."
  (let ((drawing (app-drawing app)))
    (list (list :grid (i18n:tr "Grid") nil
                (lambda () (operation-toggle-grid app))
                (i18n:tr "Show the grid") "G" (dom:drawing-grid-visible drawing))
          (list :snap (i18n:tr "Snap") nil
                (lambda () (operation-toggle-snap app))
                (i18n:tr "Snap to the grid") "S" (dom:drawing-snap-to-grid drawing))
          (list :object-snap (i18n:tr "OSnap") nil
                (lambda () (operation-toggle-object-snap app))
                (i18n:tr "Snap to the points of the entities") "O" (dom:drawing-object-snap drawing))
          (list :ortho (i18n:tr "Ortho") nil
                (lambda () (operation-toggle-ortho app))
                (i18n:tr "Hold the next point to a horizontal or vertical line") "F8"
                (eq :ortho (app-constraint app)))
          (list :polar (i18n:tr "Polar") nil
                (lambda () (operation-toggle-polar app))
                (i18n:tr "Hold the next point to multiples of 15 degrees") "F10"
                (eq :polar (app-constraint app)))
          (list :unit
                (i18n:trf "Unit: ~a" (base:unit-abbreviation (dom:drawing-display-unit drawing)))
                :unit
                (lambda () (operation-cycle-display-unit app))
                (i18n:tr "Change the display unit") "U" nil)
          (list :extents (i18n:tr "Fit") :extents
                (lambda () (operation-zoom-extents app))
                (i18n:tr "Zoom to fit everything") "Home" nil))))

(defun chip-width (label
                   icon)
  "Return the width of a status chip.

LABEL: its text.
ICON: its icon name, or NIL for the dot."
  (+ (ui:text-width-pixels label ui:+small-font-pixels+)
     (* 2 ui:+chip-padding+)
     (if icon +chip-icon-room+ +chip-dot-room+)))

(defun status-chip-widgets (app
                            right-edge
                            top
                            blocked)
  "Build the chips of the status bar, ending at RIGHT-EDGE. Returns the widgets and the x where
the leftmost chip begins.

APP: the application.
RIGHT-EDGE: x where the last chip ends.
TOP: y of the status bar.
BLOCKED: true while a modal covers the status bar."
  (let ((x right-edge)
        (widgets '()))
    (dolist (spec (reverse (status-chip-specs app)))
      (destructuring-bind (id label icon action hint shortcut active) spec
        (let ((width (chip-width label icon)))
          (decf x width)
          (push (ui:make-widget :button
                                id
                                x
                                (+ top (/ (- ui:+status-height+ ui:+chip-height+) 2))
                                width
                                ui:+chip-height+
                                :label label
                                :icon icon
                                :action action
                                :active active
                                :blocked blocked
                                :hint hint
                                :shortcut shortcut
                                :variant :chip
                                :size ui:+small-font-pixels+)
                widgets)
          (decf x +chip-gap+))))
    (values widgets (+ x +chip-gap+))))

(defun status-widgets (app
                       hover-hint
                       blocked)
  "Build the status bar: what to do or what went wrong on the left, the pointer position, and
the chips that switch the drawing aids.

APP: the application.
HOVER-HINT: hint of the widget under the pointer, or NIL.
BLOCKED: true while a modal covers the status bar."
  (multiple-value-bind (left severity right)
      (app-status-texts app hover-hint)
    (let* ((theme (app-theme app))
           (width (float (app-window-width app) 1f0))
           (top (float (- (app-window-height app) ui:+status-height+) 1f0))
           (right-width (ui:text-width-pixels right ui:+small-font-pixels+)))
      (multiple-value-bind (chips chips-left)
          (status-chip-widgets app
                               (- width +status-padding+)
                               top
                               blocked)
        (let* ((right-x (- chips-left +status-padding+ right-width))
               (error-p (eq severity :error)))
          (append (list (ui:make-widget :panel
                                        :status
                                        0f0
                                        top
                                        width
                                        ui:+status-height+
                                        :variant :status-bar)
                        (make-label :status-left
                                    left
                                    +status-padding+
                                    top
                                    (max 0f0 (- right-x +status-padding+ +status-padding+))
                                    ui:+status-height+
                                    :size ui:+small-font-pixels+
                                    :icon (if error-p :warning :info)
                                    :color (if error-p
                                               (scene:theme-danger theme)
                                               (scene:theme-text-dim theme)))
                        (make-label :status-right
                                    right
                                    right-x
                                    top
                                    right-width
                                    ui:+status-height+
                                    :size ui:+small-font-pixels+
                                    :color (scene:theme-text-dim theme)))
                  chips))))))

;;; Tooltips

(defun hint-with-shortcut (widget)
  "Return the hint of WIDGET for the status bar, with its shortcut in parentheses, or NIL.

WIDGET: a widget."
  (let ((hint (ui:widget-hint widget))
        (shortcut (ui:widget-shortcut widget)))
    (and hint
         (if shortcut
             (format nil "~a (~a)" hint shortcut)
             hint))))

(defun tooltip-widget (app
                       hovered)
  "Build the tooltip of the hovered widget when it shows only an icon, or return NIL. Tool
buttons show it to their right, the title bar's buttons under themselves.

APP: the application.
HOVERED: the interactive widget under the pointer, or NIL."
  (when (and hovered
             (ui:widget-hint hovered)
             (ui:widget-icon hovered)
             (null (ui:widget-label hovered)))
    (let* ((hint (ui:widget-hint hovered))
           (shortcut (ui:widget-shortcut hovered))
           (width (ui:tooltip-width hint shortcut))
           (height ui:+tooltip-height+)
           (window-width (float (app-window-width app) 1f0))
           (button-x (ui:widget-x hovered))
           (button-y (ui:widget-y hovered)))
      (multiple-value-bind (x y)
          (if (eq (ui:widget-variant hovered) :tool)
              (values (+ button-x (ui:widget-width hovered) +tooltip-distance+)
                      (+ button-y (/ (- (ui:widget-height hovered) height) 2)))
              (values (- (+ button-x (/ (ui:widget-width hovered) 2)) (/ width 2))
                      (- (+ button-y (ui:widget-height hovered) +tooltip-distance+) +tooltip-lift+)))
        (ui:make-widget :tooltip
                        :tooltip
                        (max +tooltip-edge-margin+
                             (min x (- window-width width +tooltip-edge-margin+)))
                        y
                        width
                        height
                        :label hint
                        :shortcut shortcut)))))
