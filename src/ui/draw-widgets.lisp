(in-package #:borba-cad/ui)

;;;; Drawing of widgets into the draw list: surfaces, buttons, rows, labels, fields, tooltips and
;;;; the performance overlay. What each kind looks like is decided here and in the theme; what it
;;;; does is decided by the application that builds the widgets.

;;; Helpers

(defun fill-widget (list
                    widget
                    radius
                    color)
  "Fill the bounds of WIDGET with a rounded rectangle.

LIST: the draw list.
WIDGET: a widget.
RADIUS: corner radius in pixels.
COLOR: an RGBA."
  (scene:add-rounded-rectangle list
                               (widget-x widget)
                               (widget-y widget)
                               (widget-width widget)
                               (widget-height widget)
                               radius
                               color))

(defun outline-widget (list
                       widget
                       radius
                       thickness
                       color)
  "Draw a rounded outline inside the bounds of WIDGET.

LIST: the draw list.
WIDGET: a widget.
RADIUS: corner radius in pixels.
THICKNESS: stroke width in pixels.
COLOR: an RGBA."
  (scene:add-rounded-rectangle-outline list
                                       (widget-x widget)
                                       (widget-y widget)
                                       (widget-width widget)
                                       (widget-height widget)
                                       radius
                                       thickness
                                       color))

(defun add-indicator (list
                      x
                      y
                      height
                      theme)
  "Draw the accent bar that marks the active tool or the current row.

LIST: the draw list.
X: left edge of the bar.
Y: top edge of the bar.
HEIGHT: height of the bar.
THEME: the theme."
  (scene:add-rounded-rectangle list
                               x
                               y
                               +indicator-width+
                               height
                               +indicator-radius+
                               (scene:theme-accent theme)))

(defun baseline-for (widget
                     size)
  "Return the y of the text baseline that centers one line of SIZE vertically in WIDGET.

WIDGET: a widget.
SIZE: em size in pixels."
  (+ (widget-y widget)
     (/ (widget-height widget) 2)
     (* +baseline-offset+ size)))

(defun hovered-p (widget
                  pointer-x
                  pointer-y)
  "Return true when the pointer is over an interactive WIDGET.

WIDGET: a widget.
POINTER-X: pointer x in pixels.
POINTER-Y: pointer y in pixels."
  (and (interactive-widget-p widget)
       (widget-contains-p widget
                          pointer-x
                          pointer-y)))

(defun widget-text-color (widget
                          theme)
  "Return the color that WIDGET's text and icon are drawn in.

WIDGET: a widget.
THEME: the theme."
  (let ((button-p (eq (widget-kind widget) :button)))
    (cond ((not (widget-enabled widget))
           (scene:theme-text-faint theme))
          ((widget-color widget))
          ((and button-p (eq (widget-variant widget) :primary))
           (scene:theme-accent-contrast theme))
          ((and button-p (widget-active widget))
           (scene:theme-accent theme))
          (t (scene:theme-text theme)))))

(defun widget-padding (widget)
  "Return the space between the edge of WIDGET and its text. Labels are placed exactly where
they are wanted, chips and ghost buttons are snug, and a button too narrow for two paddings keeps a
small one.

WIDGET: a widget."
  (case (widget-kind widget)
    (:label 0f0)
    (:button (cond ((eq (widget-variant widget) :chip) +chip-padding+)
                   ((eq (widget-variant widget) :ghost) +gap+)
                   ((< (widget-width widget) (* 2 +icon-button-size+)) +gap+)
                   (t +padding+)))
    (t +padding+)))

(defun draw-widget-text (list
                         widget
                         text
                         color
                         &key (left-inset 0f0))
  "Draw TEXT inside WIDGET, truncated to fit and aligned as the widget asks.

LIST: the draw list.
WIDGET: a widget.
TEXT: the string.
COLOR: an RGBA.
LEFT-INSET: pixels at the left that the text must stay clear of, besides the padding."
  (when (and text (plusp (length text)))
    (let* ((size (widget-size widget))
           (font (widget-font widget))
           (padding (widget-padding widget))
           (left (+ (widget-x widget) padding left-inset))
           (available (max 0f0 (- (widget-width widget)
                                  (* 2 padding)
                                  left-inset
                                  (widget-trailing widget))))
           (fitted (fit-text text
                             available
                             size
                             font))
           (width (text-width-pixels fitted
                                     size
                                     font))
           (text-x (ecase (widget-align widget)
                     (:left left)
                     (:center (+ left (/ (- available width) 2)))
                     (:right (+ left (- available width))))))
      (scene:add-text list
                      fitted
                      text-x
                      (baseline-for widget size)
                      size
                      color
                      :font font))))

;;; Surfaces

(defun draw-panel-widget (list
                          widget
                          theme)
  "Draw a flat region. The title bar is shaded, and each region that borders the canvas has a
hairline on that side.

LIST: the draw list.
WIDGET: the widget.
THEME: the theme."
  (let* ((x (widget-x widget))
         (y (widget-y widget))
         (width (widget-width widget))
         (height (widget-height widget))
         (border (scene:theme-panel-border theme))
         (variant (widget-variant widget)))
    (if (eq variant :title-bar)
        (scene:add-gradient-rectangle list
                                      x
                                      y
                                      width
                                      height
                                      (scene:theme-title-bar-top theme)
                                      (scene:theme-title-bar-bottom theme))
        (scene:add-filled-rectangle list
                                    x
                                    y
                                    width
                                    height
                                    (scene:theme-panel-background theme)))
    (multiple-value-bind (line-x line-y line-width line-height)
        (case variant
          (:title-bar (values x (+ y (- height +hairline+)) width +hairline+))
          (:status-bar (values x y width +hairline+))
          (:palette (values (+ x (- width +hairline+)) y +hairline+ height))
          (:side (values x y +hairline+ height))
          (:console (values x y width +hairline+)))
      (when line-x
        (scene:add-filled-rectangle list
                                    line-x
                                    line-y
                                    line-width
                                    line-height
                                    border)))))

(defun draw-card-widget (list
                         widget
                         theme)
  "Draw a raised, rounded surface with a soft shadow and a hairline border.

LIST: the draw list.
WIDGET: the widget.
THEME: the theme."
  (scene:add-shadow list
                    (widget-x widget)
                    (widget-y widget)
                    (widget-width widget)
                    (widget-height widget)
                    +card-radius+
                    +card-shadow-spread+
                    (scene:with-alpha (scene:theme-shadow theme) +card-shadow-opacity+))
  (fill-widget list
               widget
               +card-radius+
               (scene:theme-title-bar-top theme))
  (outline-widget list
                  widget
                  +card-radius+
                  +hairline+
                  (scene:theme-panel-border theme)))

;;; Buttons

(defun button-radius (widget)
  "Return the corner radius of a button of its variant.

WIDGET: a button."
  (case (widget-variant widget)
    (:chip (/ (widget-height widget) 2))
    (:tool +tool-radius+)
    (t +radius+)))

(defun button-icon-size (widget)
  "Return the size of the icon of a button of its variant.

WIDGET: a button."
  (case (widget-variant widget)
    (:tool +tool-icon-size+)
    ((:icon :floating) +icon-size+)
    (t +small-icon-size+)))

(defun toggle-fill (widget
                    theme
                    hover)
  "Return the body color of a button that is idle, under the pointer or toggled on.

WIDGET: a button.
THEME: the theme.
HOVER: true when the pointer is over it."
  (cond ((widget-active widget) (scene:theme-button-active theme))
        ((and hover (widget-enabled widget)) (scene:theme-button-hover theme))
        (t (scene:theme-button theme))))

(defun draw-icon-button-body (list
                              widget
                              theme
                              hover)
  "Draw the body of a button that shows only an icon: nothing when idle, a wash under the
pointer, a tint of the accent when active; a tool of the palette also gets the bar beside it.

LIST: the draw list.
WIDGET: a :tool or :icon button.
THEME: the theme.
HOVER: true when the pointer is over it."
  (let ((radius (button-radius widget)))
    (cond ((widget-active widget)
           (fill-widget list
                        widget
                        radius
                        (scene:theme-accent-soft theme)))
          ((and hover (widget-enabled widget))
           (fill-widget list
                        widget
                        radius
                        (scene:theme-button-hover theme))))
    (when (and (widget-active widget) (eq (widget-variant widget) :tool))
      (add-indicator list
                     (- (widget-x widget) +tool-indicator-offset+)
                     (+ (widget-y widget) (/ (- (widget-height widget) +tool-indicator-height+) 2))
                     +tool-indicator-height+
                     theme))))

(defun draw-outlined-button-body (list
                                  widget
                                  theme
                                  hover
                                  active-outline-opacity)
  "Draw the body of a default button or a chip: a fill and an outline that turns to the accent
when the button is on.

LIST: the draw list.
WIDGET: a :default or :chip button.
THEME: the theme.
HOVER: true when the pointer is over it.
ACTIVE-OUTLINE-OPACITY: opacity of the accent outline of a button that is on."
  (let ((radius (button-radius widget)))
    (fill-widget list
                 widget
                 radius
                 (toggle-fill widget
                              theme
                              hover))
    (outline-widget list
                    widget
                    radius
                    +hairline+
                    (if (widget-active widget)
                        (scene:with-alpha (scene:theme-accent theme) active-outline-opacity)
                        (scene:theme-panel-border theme)))))

(defun draw-button-body (list
                         widget
                         theme
                         hover)
  "Draw the body of a button in the style its variant asks for. Returns how far the label must
keep from the left edge besides its padding, which is the room a chip's dot takes.

LIST: the draw list.
WIDGET: a button.
THEME: the theme.
HOVER: true when the pointer is over it."
  (let ((variant (widget-variant widget))
        (inset 0f0))
    (case variant
      (:primary
       (fill-widget list
                    widget
                    (button-radius widget)
                    (scene:with-alpha (scene:theme-accent theme)
                                      (cond ((not (widget-enabled widget)) +primary-disabled-opacity+)
                                            (hover +primary-hover-opacity+)
                                            (t 1f0)))))
      (:ghost
       (when (and hover (widget-enabled widget))
         (fill-widget list
                      widget
                      (button-radius widget)
                      (scene:theme-button-hover theme))))
      ((:tool :icon)
       (draw-icon-button-body list
                              widget
                              theme
                              hover))
      (:chip
       (draw-outlined-button-body list
                                  widget
                                  theme
                                  hover
                                  +chip-active-outline-opacity+)
       (unless (widget-icon widget)
         (scene:add-disc list
                         (+ (widget-x widget) +chip-dot-center+)
                         (+ (widget-y widget) (/ (widget-height widget) 2))
                         +chip-dot-radius+
                         (if (widget-active widget)
                             (scene:theme-accent theme)
                             (scene:theme-text-faint theme)))
         (setf inset +chip-dot-inset+)))
      (t
       (draw-outlined-button-body list
                                  widget
                                  theme
                                  hover
                                  +active-outline-opacity+)
       (when (widget-swatch widget)
         (scene:add-disc list
                         (+ (widget-x widget) +padding+ +swatch-radius+)
                         (+ (widget-y widget) (/ (widget-height widget) 2))
                         +swatch-radius+
                         (widget-swatch widget))
         (setf inset +swatch-inset+))))
    inset))

(defun draw-icon-with-label (list
                             widget
                             color)
  "Draw an icon and a label side by side, centered together in the button.

LIST: the draw list.
WIDGET: a button with an icon and a label.
COLOR: an RGBA."
  (let* ((size (widget-size widget))
         (font (widget-font widget))
         (icon-size (button-icon-size widget))
         (label-width (text-width-pixels (widget-label widget)
                                         size
                                         font))
         (group-width (+ icon-size +gap+ label-width))
         (start (+ (widget-x widget)
                   (max +gap+ (/ (- (widget-width widget) group-width) 2))))
         (room (max 0f0 (- (widget-width widget) (* 2 +gap+) icon-size +gap+))))
    (scene:add-text list
                    (fit-text (widget-label widget)
                              room
                              size
                              font)
                    (+ start icon-size +gap+)
                    (baseline-for widget size)
                    size
                    color
                    :font font)
    (draw-icon list
               (widget-icon widget)
               start
               (+ (widget-y widget) (/ (- (widget-height widget) icon-size) 2))
               icon-size
               color)))

(defun draw-button-widget (list
                           widget
                           theme
                           hover)
  "Draw a button: its body, then its icon and its label.

LIST: the draw list.
WIDGET: the widget.
THEME: the theme.
HOVER: true when the pointer is over it."
  (let ((inset (draw-button-body list
                                 widget
                                 theme
                                 hover))
        (color (widget-text-color widget theme))
        (icon-size (button-icon-size widget)))
    (cond ((and (widget-icon widget) (widget-label widget))
           (draw-icon-with-label list
                                 widget
                                 color))
          ((widget-icon widget)
           (draw-icon list
                      (widget-icon widget)
                      (+ (widget-x widget) (/ (- (widget-width widget) icon-size) 2))
                      (+ (widget-y widget) (/ (- (widget-height widget) icon-size) 2))
                      icon-size
                      color))
          (t
           (draw-widget-text list
                             widget
                             (widget-label widget)
                             color
                             :left-inset inset)))))

;;; Rows and labels

(defun draw-row-widget (list
                        widget
                        theme
                        hover)
  "Draw a list row: highlight, color dot, icon and label.

LIST: the draw list.
WIDGET: the widget.
THEME: the theme.
HOVER: true when the pointer is over it."
  (let ((x (widget-x widget))
        (y (widget-y widget))
        (height (widget-height widget))
        (color (widget-text-color widget theme))
        (inset 0f0))
    (cond ((widget-active widget)
           (fill-widget list
                        widget
                        +small-radius+
                        (scene:theme-accent-soft theme))
           (add-indicator list
                          x
                          (+ y +row-indicator-margin+)
                          (- height (* 2 +row-indicator-margin+))
                          theme))
          (hover
           (fill-widget list
                        widget
                        +small-radius+
                        (scene:theme-button-hover theme))))
    (when (widget-swatch widget)
      (let ((center-x (+ x +padding+ +swatch-radius+))
            (center-y (+ y (/ height 2))))
        (scene:add-disc list
                        center-x
                        center-y
                        +swatch-radius+
                        (widget-swatch widget))
        (scene:add-ring list
                        center-x
                        center-y
                        +swatch-ring-radius+
                        +hairline+
                        (scene:with-alpha (scene:theme-text theme) +swatch-ring-opacity+)))
      (setf inset +swatch-inset+))
    (when (widget-icon widget)
      (draw-icon list
                 (widget-icon widget)
                 (+ x +padding+)
                 (+ y (/ (- height +small-icon-size+) 2))
                 +small-icon-size+
                 color)
      (setf inset +row-icon-inset+))
    (draw-widget-text list
                      widget
                      (widget-label widget)
                      color
                      :left-inset inset)
    (when (widget-shortcut widget)
      (let ((shortcut (widget-shortcut widget)))
        (scene:add-text list
                        shortcut
                        (- (+ x (widget-width widget)) +padding+ (text-width-pixels shortcut +small-font-pixels+))
                        (baseline-for widget +small-font-pixels+)
                        +small-font-pixels+
                        (scene:theme-text-faint theme)
                        :font :ui)))))

(defun draw-label-widget (list
                          widget
                          theme)
  "Draw a label: an optional dot or icon, then the text.

LIST: the draw list.
WIDGET: the widget.
THEME: the theme."
  (let ((x (widget-x widget))
        (middle (+ (widget-y widget) (/ (widget-height widget) 2)))
        (color (widget-text-color widget theme)))
    (when (widget-swatch widget)
      (scene:add-disc list
                      (+ x +label-dot-center+)
                      middle
                      +label-dot-radius+
                      (widget-swatch widget)))
    (when (widget-icon widget)
      (draw-icon list
                 (widget-icon widget)
                 x
                 (- middle (/ +small-icon-size+ 2))
                 +small-icon-size+
                 color))
    (draw-widget-text list
                      widget
                      (widget-label widget)
                      color
                      :left-inset (cond ((widget-icon widget) +label-icon-inset+)
                                        ((widget-swatch widget) +label-dot-inset+)
                                        (t 0f0)))))

;;; Fields

(defun field-inset (widget)
  "Return how far the text of a field keeps from its left padding: the room of the icon that
stands before it, if it has one.

WIDGET: a field."
  (if (widget-icon widget)
      +row-icon-inset+
      0f0))

(defun draw-edited-text (list
                         widget
                         theme)
  "Draw the text of a field that has the keyboard focus: the part around the caret, the
highlight behind selected text, and the caret.

LIST: the draw list.
WIDGET: a field with an edit state.
THEME: the theme."
  (let* ((edit (widget-edit widget))
         (text (edit-state-text edit))
         (size (widget-size widget))
         (font (widget-font widget))
         (text-x (+ (widget-x widget) +padding+ (field-inset widget)))
         (available (max 0f0 (- (widget-width widget) (* 2 +padding+) (field-inset widget)))))
    (multiple-value-bind (start end)
        (visible-text-window text
                             (edit-state-caret edit)
                             available
                             size
                             font)
      (let* ((shown (subseq text start end))
             (shown-width (text-width-pixels shown
                                             size
                                             font))
             (caret-x (+ text-x (text-width-pixels (subseq text start (edit-state-caret edit))
                                                   size
                                                   font)))
             (top (widget-y widget)))
        (when (edit-state-replace-on-type edit)
          (scene:add-rounded-rectangle list
                                       (- text-x +selection-margin+)
                                       (+ top +selection-inset+)
                                       (+ shown-width (* 2 +selection-margin+))
                                       (- (widget-height widget) (* 2 +selection-inset+))
                                       +selection-radius+
                                       (scene:theme-accent-soft theme)))
        (scene:add-text list
                        shown
                        text-x
                        (baseline-for widget size)
                        size
                        (scene:theme-text theme)
                        :font font)
        (scene:add-thick-line list
                              caret-x
                              (+ top +caret-inset+)
                              caret-x
                              (- (+ top (widget-height widget)) +caret-inset+)
                              +caret-width+
                              (scene:theme-accent theme))))))

(defun draw-field-widget (list
                          widget
                          theme
                          hover)
  "Draw a text field: well, border, and either its label or the text being edited with caret.

LIST: the draw list.
WIDGET: the widget.
THEME: the theme.
HOVER: true when the pointer is over it."
  (let ((editing (widget-edit widget)))
    (fill-widget list
                 widget
                 +small-radius+
                 (scene:theme-field-background theme))
    (outline-widget list
                    widget
                    +small-radius+
                    (if editing +focus-ring-width+ +hairline+)
                    (cond (editing (scene:theme-accent theme))
                          (hover (scene:theme-text-faint theme))
                          (t (scene:theme-panel-border theme))))
    (when (widget-icon widget)
      (draw-icon list
                 (widget-icon widget)
                 (+ (widget-x widget) +padding+)
                 (+ (widget-y widget) (/ (- (widget-height widget) +small-icon-size+) 2))
                 +small-icon-size+
                 (scene:theme-text-faint theme)))
    (if editing
        (draw-edited-text list
                          widget
                          theme)
        (draw-widget-text list
                          widget
                          (widget-label widget)
                          (if (and hover (widget-enabled widget))
                              (scene:theme-text theme)
                              (widget-text-color widget theme))
                          :left-inset (field-inset widget)))))

;;; Tooltips

(defun tooltip-width (label
                      shortcut)
  "Return the width of a tooltip with the given text and key cap.

LABEL: what the tooltip says.
SHORTCUT: the keys that do the same, or NIL."
  (+ (* 2 +tooltip-padding+)
     (text-width-pixels label
                        +small-font-pixels+
                        :ui)
     (if shortcut
         (+ +gap+
            (* 2 +key-cap-padding+)
            (text-width-pixels shortcut
                               +small-font-pixels+
                               :ui-strong))
         0f0)))

(defun draw-tooltip-widget (list
                            widget
                            theme)
  "Draw a tooltip: a dark rounded label with the shortcut as a small key cap.

LIST: the draw list.
WIDGET: the widget.
THEME: the theme."
  (let* ((x (widget-x widget))
         (y (widget-y widget))
         (width (widget-width widget))
         (height (widget-height widget))
         (size +small-font-pixels+)
         (baseline (+ y (/ height 2) (* +baseline-offset+ size)))
         (shortcut (widget-shortcut widget)))
    (scene:add-shadow list
                      x
                      y
                      width
                      height
                      +radius+
                      +tooltip-shadow-spread+
                      (scene:with-alpha (scene:theme-shadow theme) +tooltip-shadow-opacity+))
    (fill-widget list
                 widget
                 +radius+
                 (scene:theme-overlay-background theme))
    (outline-widget list
                    widget
                    +radius+
                    +hairline+
                    (scene:theme-panel-border theme))
    (scene:add-text list
                    (widget-label widget)
                    (+ x +tooltip-padding+)
                    baseline
                    size
                    (scene:theme-text theme)
                    :font :ui)
    (when shortcut
      (let* ((cap-width (+ (* 2 +key-cap-padding+)
                           (text-width-pixels shortcut
                                              size
                                              :ui-strong)))
             (cap-x (- (+ x width) cap-width +tooltip-padding+)))
        (scene:add-rounded-rectangle list
                                     cap-x
                                     (+ y +key-cap-inset+)
                                     cap-width
                                     (- height (* 2 +key-cap-inset+))
                                     +key-cap-radius+
                                     (scene:theme-button-hover theme))
        (scene:add-text list
                        shortcut
                        (+ cap-x +key-cap-padding+)
                        baseline
                        size
                        (scene:theme-text-dim theme)
                        :font :ui-strong)))))

;;; Dispatch

(defun draw-widget (list
                    widget
                    theme
                    pointer-x
                    pointer-y)
  "Draw one widget.

LIST: the draw list.
WIDGET: the widget.
THEME: the theme.
POINTER-X: pointer x in pixels, for hover highlighting.
POINTER-Y: pointer y in pixels."
  (let ((hover (hovered-p widget
                          pointer-x
                          pointer-y)))
    (ecase (widget-kind widget)
      (:panel
       (draw-panel-widget list
                          widget
                          theme))
      (:card
       (draw-card-widget list
                         widget
                         theme))
      (:button
       (draw-button-widget list
                           widget
                           theme
                           hover))
      (:row
       (draw-row-widget list
                        widget
                        theme
                        hover))
      (:label
       (draw-label-widget list
                          widget
                          theme))
      (:swatch
       (fill-widget list
                    widget
                    +small-radius+
                    (widget-swatch widget)))
      (:scrim
       (scene:add-filled-rectangle list
                                   (widget-x widget)
                                   (widget-y widget)
                                   (widget-width widget)
                                   (widget-height widget)
                                   (scene:theme-scrim theme)))
      (:separator
       (scene:add-filled-rectangle list
                                   (widget-x widget)
                                   (widget-y widget)
                                   (max +hairline+ (widget-width widget))
                                   (max +hairline+ (widget-height widget))
                                   (scene:theme-panel-border theme)))
      (:icon
       (draw-icon list
                  (widget-icon widget)
                  (widget-x widget)
                  (widget-y widget)
                  (widget-width widget)
                  (widget-text-color widget theme)))
      (:logo
       (draw-logo list
                  (widget-x widget)
                  (widget-y widget)
                  (widget-width widget)
                  theme))
      (:field
       (draw-field-widget list
                          widget
                          theme
                          hover))
      (:tooltip
       (draw-tooltip-widget list
                            widget
                            theme)))))

(defun draw-widgets (list
                     theme
                     widgets
                     pointer-x
                     pointer-y)
  "Draw every widget in order.

LIST: the draw list.
THEME: the theme.
WIDGETS: list of widgets in drawing order.
POINTER-X: pointer x in pixels, for hover highlighting.
POINTER-Y: pointer y in pixels."
  (dolist (widget widgets)
    (draw-widget list
                 widget
                 theme
                 pointer-x
                 pointer-y)))

(defun draw-overlay (list
                     theme
                     lines
                     x
                     y)
  "Draw the performance overlay: a translucent card with one monospaced text line per entry.

LIST: the draw list.
THEME: the theme.
LINES: list of strings.
X: left edge in pixels.
Y: top edge in pixels."
  (when lines
    (let* ((size +small-font-pixels+)
           (line-height (* +overlay-line-spacing+ size))
           (widest (reduce #'max lines
                           :key (lambda (line)
                                  (text-width-pixels line
                                                     size
                                                     :mono))))
           (width (+ (* 2 +padding+) widest))
           (height (+ (* 2 +padding+) (* line-height (length lines)))))
      (scene:add-rounded-rectangle list
                                   x
                                   y
                                   width
                                   height
                                   +radius+
                                   (scene:theme-overlay-background theme))
      (scene:add-rounded-rectangle-outline list
                                           x
                                           y
                                           width
                                           height
                                           +radius+
                                           +hairline+
                                           (scene:theme-panel-border theme))
      (loop for line in lines
            for row from 1
            do (scene:add-text list
                               line
                               (+ x +padding+)
                               (+ y +padding+ (* line-height row) (* +overlay-baseline-lift+ size))
                               size
                               (scene:theme-text theme)
                               :font :mono)))))
